### Install necessary packages
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("tidyverse")

### Load necessary packages
library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)


### Import and assign data
exported_data <- readr::read_delim("./data/exported_data.csv", delim = ";", na = "NA")
lookup_data <- readxl::read_excel("./data/lookup_table_strict.xlsx")
data <- base::data.frame(exported_data)

### Select necessary columns and remove NAs
data_selected <- data %>%
  dplyr::select(text, linkId, answer) %>%
  dplyr::mutate(linkId = as.integer(linkId)) %>%
  dplyr::mutate(answer = as.integer(answer)) %>%
  stats::na.omit()

### Check if NA have been removed
nrow(data_selected) / 21

### Distribute Questions for easier calculation
avg_questions <- c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 13)
avg_negative_questions <- c(16, 17, 18)
other_question <- 10

### Filter and assign Data to DataFrames
normal_questions <- data_selected %>%
  arrange(linkId) %>%
  filter(linkId %in% avg_questions)

negative_questions <- data_selected %>%
  arrange(linkId) %>%
  filter(linkId %in% avg_negative_questions)


### Create empty dataframe for processing
mutated_questions <- data.frame()

### Looping over avg_questions to assign the correct codings
### To calculate the positive questions
for (i in avg_questions) {
  look <- lookup_data[i,]
  index_one <- look$NumAnswers - look$zero_index
  index_one <- look$NumAnswers - look$zero_index

  temp_questions <- normal_questions %>%
    filter(linkId == i) %>%
    mutate(coding = case_when(
      answer <= look$one_index ~ as.integer(1),
      answer >= look$zero_index & answer < look$na_index ~ as.integer(0),
      answer >= look$na_index ~ NA
    ))
  mutated_questions <- rbind(mutated_questions, temp_questions)
}


### Looping over avg_negative_questions to assign the correct codings
### To calculate the negative questions
for (i in avg_negative_questions) {
  look <- lookup_data[i,]
  temp_questions <- negative_questions %>%
    filter(linkId == i) %>%
    mutate(coding = case_when(
      answer <= look$zero_index ~ as.integer(0),
      answer > look$zero_index & answer < look$na_index ~ as.integer(1),
      answer >= look$na_index ~ NA
    ))
  mutated_questions <- rbind(mutated_questions, temp_questions)
}

### Filter rows containing NAs
mutated_questions <- mutated_questions %>%
  filter(!is.na(coding))

### Summarise by the codings
summarised_coding <- mutated_questions %>%
  group_by(linkId) %>%
  summarise(
    coding_1 = sum(coding == 1),
    coding_0 = sum(coding == 0)
  )

### Calculate percentages
scored_result <- summarised_coding %>%
  mutate(score = (coding_1 / (coding_0 + coding_1)) * 100) %>%
  mutate(score = round(score, digits = 2))


# Initialize an empty vector to store the matched text
matched_texts <- c()

# Loop through each row of scored_result
for (i in seq_len(nrow(scored_result))) {
  # Get the index value from the current row
  index <- scored_result$linkId[i]

  # Filter the row from lookup_data based on the index value
  matching_row <- lookup_data %>% filter(LinkId == index)

  if (nrow(matching_row) > 0) {
    matched_texts <- c(matched_texts, matching_row$text)
  } else {
    # If there's no match, append NA
    matched_texts <- c(matched_texts, NA)
  }
}


# Add the matched text as a new column to scored_result
scored_result$text <- matched_texts

write.csv(scored_result, "./data/result_strict.csv", row.names = FALSE, fileEncoding = "UTF-8")


