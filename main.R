#install.packages("dplyr")
#install.packages("ggplot2")
library(dplyr)

### Assign imported data
data <- exported_data

### Select necessary columns and remove NAs
data_selected <- data %>%
  select(text, linkId, answer) %>%
  na.omit()

### Check if NA have been removed
nrow(data_selected) / 21

### Distribute Questions for easier calculation
avg_questions <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13)
avg_negative_questions <- c(16, 17, 18)
other_question <- 10

### Filter and assign Data to DataFrames
normal_questions <- data_selected %>% arrange(linkId) %>% filter(linkId %in% avg_questions)
negative_questions <- data_selected %>% arrange(linkId) %>% filter(linkId %in% avg_negative_questions)
