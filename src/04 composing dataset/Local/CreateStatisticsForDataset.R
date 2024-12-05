# Load necessary libraries
library(dplyr)
library(yaml)
library(knitr)
library(tidyr)

# Connect to YAML configuration file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))
config_path <- file.path(config_dir, "config_04.yml")
config <- yaml::yaml.load_file(config_path)

# Define output path
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")
localdataset_dir <- normalizePath(file.path(parent_directory, config$input_data$localdataset), winslash = "/")
localdataset <- read.csv(localdataset_dir, sep = ',')

colnames(localdataset) <- gsub("_", " ", colnames(localdataset))  # Replace underscores with spaces
colnames(localdataset) <- gsub("[^[:alnum:][:space:]]", "", colnames(localdataset))  # Remove other special characters

# List column names to be analyzed
# List column names to be analyzed
columns_to_analyze <- c("nightlight 450",
  "nightlight 4950",  "population 3000", "road class 1 5000",
  "road class 2 1000", "road class 2 5000", "road class 3 100",
  "road class 3 300", "trafBuf50"
)

# Function to compute statistics for specified columns
compute_statistics <- function(data, columns) {
  stats <- data %>% 
    select(all_of(columns)) %>% 
    summarise(across(
      everything(),
      list(
        `25th` = ~ quantile(., 0.25, na.rm = TRUE),
        `50th` = ~ quantile(., 0.50, na.rm = TRUE),
        `75th` = ~ quantile(., 0.75, na.rm = TRUE),
        mean = ~ mean(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE),
        min = ~ min(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    pivot_longer(
      everything(),
      names_to = c("Column", "Statistic"),
      names_sep = "_",
      values_to = "Value"
    ) %>%
    arrange(Column, Statistic) %>%
    group_by(Column, Statistic) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")  # Handle duplicates and summarize

  return(stats)
}

# Compute statistics for selected columns
stats_df <- compute_statistics(localdataset, columns_to_analyze)

# Create a LaTeX table
# We need to pivot the data to have variables as rows and statistics as columns
latex_table <- stats_df %>%
  pivot_wider(
    names_from = Statistic,
    values_from = Value
  ) %>%
  arrange(Column)  # Ensure rows are ordered by the variables

# Now format the LaTeX table string
latex_output <- "\\begin{table}[ht]\n\\centering\n\\begin{tabular}{|l|c|c|c|c|c|c|c|}\n\\hline\n"
latex_output <- paste0(latex_output, "Variable & 25th & 50th & 75th & Mean & Median & Max & Min \\\\ \\hline\n")

# Add each row of the data
for (i in 1:nrow(latex_table)) {
  row <- latex_table[i, ]
  
  # Handle possible NA values in the data
  row_values <- c(
    row$Column,
    ifelse(is.na(row$`25th`), "NA", round(row$`25th`, 2)),
    ifelse(is.na(row$`50th`), "NA", round(row$`50th`, 2)),
    ifelse(is.na(row$`75th`), "NA", round(row$`75th`, 2)),
    ifelse(is.na(row$mean), "NA", round(row$mean, 2)),
    ifelse(is.na(row$median), "NA", round(row$median, 2)),
    ifelse(is.na(row$max), "NA", round(row$max, 2)),
    ifelse(is.na(row$min), "NA", round(row$min, 2))
  )
  
  latex_output <- paste0(latex_output, 
                         paste(row_values, collapse = " & "), 
                         " \\\\ \\hline\n")
}

# Finish the LaTeX table
latex_output <- paste0(latex_output, "\\end{tabular}\n\\caption{Summary statistics for local predictors.}\n\\end{table}")

# Write the LaTeX table to the text file
output_txt_dir <- file.path(out_location_dir, "statistics_local_predictors")
if (!dir.exists(output_txt_dir)) {
  dir.create(output_txt_dir)
}

output_file_path <- file.path(output_txt_dir, "statistics_predictors_local_latex.txt")
writeLines(latex_output, output_file_path)

# Confirm completion
cat("LaTeX table written to", output_file_path)