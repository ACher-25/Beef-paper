##############################################################################
# Grass-fed vs. Grain-fed Beef: Emissions per Gram Fatty Acid
# Mann-Whitney U Test Across All Yield Scenario (S) x Fatty Acid (F) Combinations
#
# WHAT THIS SCRIPT DOES (plain-language summary):
#   Your Excel workbook has many sheets (e.g., "S1F1_totalFA", "S2F2_SFA_rib",
#   etc.). Each sheet holds emissions-per-gram-of-fatty-acid values for a
#   Grass-fed group and a Grain-fed group. This script opens EVERY sheet in
#   the workbook, one at a time, and asks the same question for each one:
#   "Is there a statistically significant difference between the Grass-fed
#   values and the Grain-fed values in this sheet?"
#
#   It answers that question using the Mann-Whitney U test (also called the
#   Wilcoxon rank-sum test), which is used instead of a t-test because your
#   data are not normally distributed (confirmed earlier by histogram
#   inspection). At the end, it collects all the individual test results
#   into one single summary table and adjusts the p-values to account for
#   running many tests at once (this is called a "multiple comparisons
#   correction" -- specifically the Benjamini-Hochberg / FDR method).
#
# WHY MANN-WHITNEY U:
#   A regular t-test assumes your data follow a bell-curve (normal)
#   distribution. Your emissions data are skewed / multimodal, so a t-test
#   would not be reliable. Mann-Whitney U makes no assumption about the
#   shape of the distribution -- it simply asks whether one group's values
#   tend to be ranked higher or lower than the other's.
#
# WHY BENJAMINI-HOCHBERG (BH) INSTEAD OF BONFERRONI:
#   When you run many statistical tests at once, some "significant" results
#   will appear by chance alone -- this is the multiple comparisons problem.
#   Bonferroni is a stricter, more conservative correction that assumes all
#   tests are independent of one another. In your case, many of the 61
#   comparisons are calculated from the same underlying emissions data
#   points (just divided by different fatty acid concentration numbers), so
#   the tests are NOT fully independent. Benjamini-Hochberg is a more
#   appropriate, less overly conservative correction for this situation.
#
# WHAT YOU NEED BEFORE RUNNING THIS:
#   1. R and RStudio installed on your computer (free to download).
#   2. Your Excel workbook saved somewhere on your computer, with one sheet
#      per S x F comparison. Each sheet must have exactly two columns:
#        - diet_group          : must say exactly "Grass" or "Grain"
#        - emissions_per_g_fa  : the numeric emissions value
#   3. Update the "input_file" line below (Step 1) to point to your file.
#
# WHAT THIS SCRIPT PRODUCES:
#   A CSV file (opens fine in Excel) with one row per sheet/comparison,
#   showing sample sizes, medians, IQRs (spread of the data), the test
#   statistic, the raw p-value, and the corrected (BH-adjusted) p-value.
##############################################################################


# ============================================================
# STEP 1: Install and load the packages this script needs
# ============================================================
# A "package" is a free add-on toolkit for R. You only need to INSTALL
# each package once ever on your computer (the install.packages lines
# below). After that, you just LOAD it with library() every time you run
# the script. If you already have these installed, you can skip the
# install.packages lines (or leave them -- re-installing does no harm,
# it just takes a little time).

# install.packages("readxl")   # lets R read Excel files
# install.packages("dplyr")    # makes data-handling code easier to read

library(readxl)   # for reading the .xlsx workbook
library(dplyr)    # for tidying and combining results


# ============================================================
# STEP 2: Tell R where your Excel file is, and what to name the output
# ============================================================
# IMPORTANT: Change the text below to match your actual file name and
# location. If your file is in the same folder as this R script, you
# only need the file name. Otherwise, use the full file path.
#
# Example (Windows):  "C:/Users/YourName/Documents/grass_grain_data.xlsx"
# Example (Mac):      "/Users/YourName/Documents/grass_grain_data.xlsx"

input_file  <- "grass_grain_emissions_data.xlsx"   # <-- EDIT THIS
output_file <- "mann_whitney_results.csv"          # name of the results file this script will create

# ---- 1. Read all sheet names -------------------------------------------

sheet_names <- excel_sheets(input_file)
cat("Found", length(sheet_names), "sheets to process.\n")

# ---- 2. Function to run Mann-Whitney U on a single sheet ---------------

run_mw_test <- function(sheet_name, file) {

  d <- read_excel(file, sheet = sheet_name)

  # Basic column check
  required_cols <- c("diet_group", "emissions_per_g_fa")
  if (!all(required_cols %in% names(d))) {
    warning(paste0("Sheet '", sheet_name,
                    "' is missing required columns. Skipped."))
    return(NULL)
  }

  # Standardize group labels (trim whitespace, fix case)
  d$diet_group <- trimws(d$diet_group)

  grass_vals <- d$emissions_per_g_fa[d$diet_group == "Grass"]
  grain_vals <- d$emissions_per_g_fa[d$diet_group == "Grain"]

  # Flag sheets with missing or malformed group labels
  if (length(grass_vals) == 0 | length(grain_vals) == 0) {
    warning(paste0("Sheet '", sheet_name,
                    "' has zero values in one group. Check diet_group labels. Skipped."))
    return(NULL)
  }

  test <- wilcox.test(grass_vals, grain_vals, exact = FALSE)

  median_grass <- median(grass_vals, na.rm = TRUE)
  median_grain <- median(grain_vals, na.rm = TRUE)

  # higher_group simply records which diet group had the larger median value
  # in this comparison -- useful for quickly scanning the results table
  # without having to compare the median columns by eye each time.
  higher_group <- ifelse(median_grass > median_grain, "Grass", "Grain")

  data.frame(
    comparison    = sheet_name,
    n_grass       = length(grass_vals),
    n_grain       = length(grain_vals),
    median_grass  = median_grass,
    median_grain  = median_grain,
    IQR_grass     = IQR(grass_vals, na.rm = TRUE),
    IQR_grain     = IQR(grain_vals, na.rm = TRUE),
    higher_group  = higher_group,
    W_statistic   = unname(test$statistic),
    p_value       = test$p.value,
    stringsAsFactors = FALSE
  )
}

# ---- 3. Run across all sheets -------------------------------------------

results_list <- lapply(sheet_names, run_mw_test, file = input_file)
results <- bind_rows(results_list)

cat("Successfully ran tests on", nrow(results), "of", length(sheet_names), "sheets.\n")

# ---- 4. Multiple comparison correction (Benjamini-Hochberg / FDR) -------

results <- results %>%
  mutate(p_adjusted_BH = p.adjust(p_value, method = "BH")) %>%
  mutate(significant_BH_0.05 = p_adjusted_BH < 0.05) %>%
  arrange(p_adjusted_BH)

# ---- 5. Save results ------------------------------------------------------

write.csv(results, output_file, row.names = FALSE)
cat("Results saved to", output_file, "\n")

# ---- 6. Quick summary printed to console -----------------------------

cat("\n--- Summary ---\n")
cat("Total comparisons run:", nrow(results), "\n")
cat("Significant after BH correction (p_adj < 0.05):",
    sum(results$significant_BH_0.05, na.rm = TRUE), "\n")

print(results)
