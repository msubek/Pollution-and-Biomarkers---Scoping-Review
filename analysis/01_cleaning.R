# =============================================================================
# Data Completeness Audit
# =============================================================================

library(readxl)
library(tidyverse)
library(janitor)
library(dplyr)

fulldata <- read_excel("results-survey551932 (2).xlsx")
df <- fulldata %>% select(-`Date submitted`, -`Last page`, -`Start language`, -Seed, -`Extractor Name`)


cat("First few column names:\n")
print(head(names(df), 20))

# Clean column names
df <- df %>%
  clean_names()

cat("\nFirst few cleaned column names:\n")
print(head(names(df), 20))

# Find ID column
id_col <- names(df)[grepl("response|id", names(df), ignore.case = TRUE)][1]
cat("\nUsing ID column:", id_col, "\n")

# Remove empty rows using the identified ID column
if (!is.null(id_col) && !is.na(id_col)) {
  df <- df %>%
    filter(!is.na(!!sym(id_col)) & !!sym(id_col) != "")
} else {
  # If no ID column found
  df <- df %>%
    filter(!is.na(.[[1]]) & .[[1]] != "")
}

# Total papers
total_papers <- nrow(df)
cat("\nTotal papers in dataset:", total_papers, "\n\n")




# Ccheck completeness for each field
check_completeness <- function(field) {
  
  if (field == "publication_year") {
    # Find publication year column
    year_col <- names(df)[grepl("publication|year", names(df), ignore.case = TRUE)][1]
    if (is.null(year_col) || is.na(year_col)) return(rep(FALSE, nrow(df)))
    complete <- !is.na(df[[year_col]]) & df[[year_col]] != ""
    
  } else if (field == "country") {
    country_col <- names(df)[grepl("country", names(df), ignore.case = TRUE)][1]
    if (is.null(country_col) || is.na(country_col)) return(rep(FALSE, nrow(df)))
    complete <- !is.na(df[[country_col]]) & df[[country_col]] != ""
    
  } else if (field == "study_design") {
    design_col <- names(df)[grepl("study_design", names(df), ignore.case = TRUE)][1]
    if (is.null(design_col) || is.na(design_col)) return(rep(FALSE, nrow(df)))
    complete <- !is.na(df[[design_col]]) & df[[design_col]] != ""
    
  } else if (field == "participant_age_groups") {
    # Find all age group columns
    age_cols <- grep("age|group", names(df), value = TRUE, ignore.case = TRUE)
    if (length(age_cols) == 0) return(rep(FALSE, nrow(df)))
    complete <- rowSums(!is.na(df[age_cols]) & df[age_cols] != "", na.rm = TRUE) > 0
    
  } else if (field == "total_sample_size") {
    size_col <- names(df)[grepl("sample_size|total_sample", names(df), ignore.case = TRUE)][1]
    if (is.null(size_col) || is.na(size_col)) return(rep(FALSE, nrow(df)))
    complete <- !is.na(df[[size_col]]) & df[[size_col]] != ""
    
  } else if (field == "pollutant_type") {
    # Find all pollutant type columns
    pollutant_cols <- grep("exposure|pollutant", names(df), value = TRUE, ignore.case = TRUE)
    if (length(pollutant_cols) == 0) return(rep(FALSE, nrow(df)))
    complete <- rowSums(!is.na(df[pollutant_cols]) & df[pollutant_cols] != "", na.rm = TRUE) > 0
    
  } else if (field == "exposure_measurement_method") {
    method_cols <- grep("measurement|method", names(df), value = TRUE, ignore.case = TRUE)
    if (length(method_cols) == 0) return(rep(FALSE, nrow(df)))
    complete <- rowSums(!is.na(df[method_cols]) & df[method_cols] != "", na.rm = TRUE) > 0
    
  } else if (field == "outcome_names") {
    outcome_cols <- grep("outcome", names(df), value = TRUE, ignore.case = TRUE)
    if (length(outcome_cols) == 0) return(rep(FALSE, nrow(df)))
    complete <- rowSums(!is.na(df[outcome_cols]) & df[outcome_cols] != "", na.rm = TRUE) > 0
    
  } else if (field == "outcome_sample_type") {
    sample_cols <- grep("sample|type", names(df), value = TRUE, ignore.case = TRUE)
    if (length(sample_cols) == 0) return(rep(FALSE, nrow(df)))
    complete <- rowSums(!is.na(df[sample_cols]) & df[sample_cols] != "", na.rm = TRUE) > 0
    
  } else if (field == "effect_direction") {
    effect_cols <- grep("effect|direction", names(df), value = TRUE, ignore.case = TRUE)
    if (length(effect_cols) == 0) return(rep(FALSE, nrow(df)))
    complete <- rowSums(!is.na(df[effect_cols]) & df[effect_cols] != "", na.rm = TRUE) > 0
  }
  
  return(complete)
}




# Define fields in order
fields <- c(
  "publication_year",
  "country",
  "study_design",
  "participant_age_groups",
  "total_sample_size",
  "pollutant_type",
  "exposure_measurement_method",
  "outcome_names",
  "outcome_sample_type",
  "effect_direction"
)

# Results table
results_table <- data.frame(
  Field = character(),
  Expected_N = integer(),
  N_Complete = integer(),
  N_Missing = integer(),
  Percent_Complete = character(),
  stringsAsFactors = FALSE
)

# Calculate for each field
for (field in fields) {
  complete <- check_completeness(field)
  n_complete <- sum(complete, na.rm = TRUE)
  n_missing <- total_papers - n_complete
  pct_complete <- sprintf("%.1f%%", (n_complete / total_papers) * 100)
  
  # Format field name for display
  display_name <- case_when(
    field == "publication_year" ~ "Publication Year",
    field == "country" ~ "Country",
    field == "study_design" ~ "Study Design",
    field == "participant_age_groups" ~ "Participant Age Groups",
    field == "total_sample_size" ~ "Total Sample Size",
    field == "pollutant_type" ~ "Pollutant Type",
    field == "exposure_measurement_method" ~ "Exposure Measurement Method",
    field == "outcome_names" ~ "Outcome Names",
    field == "outcome_sample_type" ~ "Outcome Sample Type",
    field == "effect_direction" ~ "Effect Direction",
    TRUE ~ field
  )
  
  results_table <- rbind(results_table, data.frame(
    Field = display_name,
    Expected_N = total_papers,
    N_Complete = n_complete,
    N_Missing = n_missing,
    Percent_Complete = pct_complete,
    stringsAsFactors = FALSE
  ))
}

# Print table
cat("\nCOMPLETENESS AUDIT RESULTS\n")
cat("==========================\n\n")

# Print as formatted table
cat(sprintf("%-30s %12s %12s %12s %15s\n", "Field", "Expected N", "N Complete", "N Missing", "% Complete"))
cat(strrep("-", 85), "\n")

for (i in 1:nrow(results_table)) {
  cat(sprintf("%-30s %12d %12d %12d %15s\n", 
              results_table$Field[i],
              results_table$Expected_N[i],
              results_table$N_Complete[i],
              results_table$N_Missing[i],
              results_table$Percent_Complete[i]))
}

# Save to CSV
write.csv(results_table, "completeness_audit_table.csv", row.names = FALSE)

# Text version
sink("completeness_audit_table.txt")
cat("COMPLETENESS AUDIT RESULTS\n")
cat("==========================\n\n")
cat(sprintf("%-30s %12s %12s %12s %15s\n", "Field", "Expected N", "N Complete", "N Missing", "% Complete"))
cat(strrep("-", 85), "\n")

for (i in 1:nrow(results_table)) {
  cat(sprintf("%-30s %12d %12d %12d %15s\n", 
              results_table$Field[i],
              results_table$Expected_N[i],
              results_table$N_Complete[i],
              results_table$N_Missing[i],
              results_table$Percent_Complete[i]))
}
sink()

cat("\n\nSUMMARY STATISTICS\n")
cat("------------------\n")

# Convert percentage string to numeric for comparison
results_table$pct_numeric <- as.numeric(gsub("%", "", results_table$Percent_Complete))

cat("Variables with excellent completeness (≥90%):\n")
excellent <- results_table %>% filter(pct_numeric >= 90)
if (nrow(excellent) > 0) {
  for (i in 1:nrow(excellent)) {
    cat(sprintf("  ✓ %s: %s\n", excellent$Field[i], excellent$Percent_Complete[i]))
  }
} else {
  cat("  None\n")
}

cat("\nVariables requiring attention (<75%):\n")
attention <- results_table %>% filter(pct_numeric < 75)
if (nrow(attention) > 0) {
  for (i in 1:nrow(attention)) {
    cat(sprintf("  ⚠ %s: %s\n", attention$Field[i], attention$Percent_Complete[i]))
  }
} else {
  cat("  None\n")
}

cat("\n\nFiles saved:\n")
cat("1. completeness_audit_table.csv\n")
cat("2. completeness_audit_table.txt\n")

