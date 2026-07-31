##############################################################################
# Grass-fed vs. Grain-fed Beef: Variance Comparison (Brown-Forsythe Test)
# Across All Yield Scenario (S) x Fatty Acid (F) Combinations
#
# WHAT THIS SCRIPT DOES (plain-language summary):
#   This script is a companion to the Mann-Whitney U script used earlier.
#   While the Mann-Whitney U test asks "do grass-fed and grain-fed values
#   tend to be different in MAGNITUDE (median)?", this script asks a
#   different question: "do grass-fed and grain-fed values differ in how
#   SPREAD OUT (variable) they are?"
#
#   It opens every sheet in your Excel workbook (same 61-sheet structure as
#   before), and for each one runs a Brown-Forsythe test to check whether
#   the variance (spread) of the Grass-fed group is significantly different
#   from the variance of the Grain-fed group. All 61 results are then
#   collected into one summary table, with p-values adjusted for running
#   many tests at once (Benjamini-Hochberg / FDR correction).
#
# WHY THE BROWN-FORSYTHE TEST (INSTEAD OF THE ORIGINAL LEVENE'S TEST):
#   The classic Levene's test centers each group's data on its MEAN before
#   comparing spread. The Brown-Forsythe test is a small modification that
#   centers each group's data on its MEDIAN instead. Centering on the
#   median makes the test more robust (reliable) when data are skewed or
#   contain outliers -- which, as established earlier in this analysis, is
#   the case for this dataset. This keeps the variance-comparison approach
#   consistent with the nonparametric, distribution-agnostic philosophy
#   used for the Mann-Whitney U tests elsewhere in this analysis.
#
# WHY THIS MATTERS FOR YOUR RESULTS:
#   Box-and-whisker plots may visually suggest that one group (e.g.,
#   grass-fed) is more variable than the other. This script turns that
#   visual impression into a formal statistical test, so the claim can be
#   backed up with a p-value rather than resting on visual inspection
#   alone. It also protects against the possibility that a visually wider
#   spread is simply a side effect of a larger sample size, rather than a
#   true difference in underlying variability.
#
# WHAT YOU NEED BEFORE RUNNING THIS:
#   1. R and RStudio installed on your computer (free to download).
#   2. The SAME Excel workbook used for the Mann-Whitney U analysis: one
#      sheet per S x F comparison, each with exactly two columns:
#        - diet_group          : must say exactly "Grass" or "Grain"
#        - emissions_per_g_fa  : the numeric emissions value
#   3. Update the "input_file" line below (Step 2) to point to your file.
#
# WHAT THIS SCRIPT PRODUCES:
#   A CSV file (opens fine in Excel) with one row per sheet/comparison,
#   showing the variance of each group, the IQR of each group, which group
#   had the higher variance, the Brown-Forsythe test statistic, the raw
#   p-value, and the BH-adjusted p-value.
##############################################################################


# ============================================================
# STEP 1: Install and load the packages this script needs
# ============================================================
# "car" provides the leveneTest() function, which supports the
# median-centered (Brown-Forsythe) version directly.
# You only need to install each package once ever on your computer.

# install.packages("readxl")   # lets R read Excel files
# install.packages("dplyr")    # makes data-handling code easier to read
# install.packages("car")      # provides leveneTest() for variance testing

library(readxl)
library(dplyr)
library(car)


# ============================================================
# STEP 2: Tell R where your Excel file is, and what to name the output
# ============================================================
# IMPORTANT: Change the text below to match your actual file name and
# location -- this should be the SAME workbook used for the Mann-Whitney
# U analysis.

input_file  <- "grass_grain_emissions_data.xlsx"   # <-- EDIT THIS
output_file <- "brown_forsythe_results.csv"        # name of the results file this script will create


# ============================================================
# STEP 3: Read all sheet names
# ============================================================

sheet_names <- excel_sheets(input_file)
cat("Found", length(sheet_names), "sheets to process.\n")


# ============================================================
# STEP 4: Function to run the Brown-Forsythe test on a single sheet
# ============================================================

run_bf_test <- function(sheet_name, file) {

  d <- read_excel(file, sheet = sheet_name)

  # Basic column check
  required_cols <- c("diet_group", "emissions_per_g_fa")
  if (!all(required_cols %in% names(d))) {
    warning(paste0("Sheet '", sheet_name,
                    "' is missing required columns. Skipped."))
    return(NULL)
  }

  # Standardize group labels (trim whitespace)
  d$diet_group <- trimws(d$diet_group)
  d$diet_group <- factor(d$diet_group)  # leveneTest() needs a factor column

  grass_vals <- d$emissions_per_g_fa[d$diet_group == "Grass"]
  grain_vals <- d$emissions_per_g_fa[d$diet_group == "Grain"]

  # Flag sheets with missing or malformed group labels
  if (length(grass_vals) == 0 | length(grain_vals) == 0) {
    warning(paste0("Sheet '", sheet_name,
                    "' has zero values in one group. Check diet_group labels. Skipped."))
    return(NULL)
  }

  # center = "median" is what makes this the Brown-Forsythe version
  # of Levene's test, rather than the classic mean-centered version.
  bf_test <- leveneTest(emissions_per_g_fa ~ diet_group, data = d, center = "median")

  var_grass <- var(grass_vals, na.rm = TRUE)
  var_grain <- var(grain_vals, na.rm = TRUE)
  higher_variance_group <- ifelse(var_grass > var_grain, "Grass", "Grain")

  data.frame(
    comparison             = sheet_name,
    var_grass              = var_grass,
    var_grain              = var_grain,
    IQR_grass              = IQR(grass_vals, na.rm = TRUE),
    IQR_grain              = IQR(grain_vals, na.rm = TRUE),
    higher_variance_group  = higher_variance_group,
    BF_statistic           = bf_test$"F value"[1],
    p_value                = bf_test$"Pr(>F)"[1],
    stringsAsFactors = FALSE
  )
}


# ============================================================
# STEP 5: Run the test across all sheets
# ============================================================

results_list <- lapply(sheet_names, run_bf_test, file = input_file)
results <- bind_rows(results_list)

cat("Successfully ran tests on", nrow(results), "of", length(sheet_names), "sheets.\n")


# ============================================================
# STEP 6: Multiple comparison correction (Benjamini-Hochberg / FDR)
# ============================================================

results <- results %>%
  mutate(p_adjusted_BH = p.adjust(p_value, method = "BH")) %>%
  mutate(significant_BH_0.05 = p_adjusted_BH < 0.05) %>%
  arrange(p_adjusted_BH)


# ============================================================
# STEP 7: Save results
# ============================================================

write.csv(results, output_file, row.names = FALSE)
cat("Results saved to", output_file, "\n")


# ============================================================
# STEP 8: Quick summary printed to console
# ============================================================

cat("\n--- Summary ---\n")
cat("Total comparisons run:", nrow(results), "\n")
cat("Significant variance difference after BH correction (p_adj < 0.05):",
    sum(results$significant_BH_0.05, na.rm = TRUE), "\n")
cat("Of those, higher-variance group breakdown:\n")
print(table(results$higher_variance_group[results$significant_BH_0.05]))

print(results)
