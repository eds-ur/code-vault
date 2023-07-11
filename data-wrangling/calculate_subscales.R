# Loading of the required libraries
library(tidyverse)

# Creating a sample dataset with generic variable names
data <- data.frame(
  matrix(sample(1:5, 60, replace = TRUE), nrow = 5)
)

# This is where the subscales are defined: simply define any number of subscales and pass the column names as vectors
score_variables <- list(
  "S1_sum" = c("X1", "X2", "X3", "X4"),
  "S2_sum" = c("X5", "X6", "X7", "X8", "X9", "X10"),
  "S3_sum" = c("X11", "X11", "X12")
)

# Iterate over the subscales and calculate the total score for each subscale
for (score_variable in names(score_variables)) {
  data <- data %>%
    # Instead of rowSums, rowMeans can also be used
    mutate({{score_variable}} := rowSums(across(all_of(score_variables[[score_variable]])), na.rm = TRUE))
}

# Reorder the score variables
score_variables_order <- names(score_variables)
data <- data %>%
  relocate(all_of(score_variables_order), .after = X12)
