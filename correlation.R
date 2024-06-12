# install.packages("readr")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("ggcorrplot")
# install.packages("MASS")
# install.packages("corrplot")
# install.packages("PerformanceAnalytics")
# install.packages("lares")


# Load packages
library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(MASS)
library(corrplot)
library(PerformanceAnalytics)
library(lares)


### Import and assign data
exported_data <- readr::read_delim("./data/exported_data.csv", delim = ";", na = "NA") %>% na.omit()
lookup_data <- readxl::read_excel("./data/lookup_table_strict.xlsx")
data <- base::data.frame(exported_data)

### Select necessary columns and remove NAs
data_selected <- data %>%
  dplyr::select(subject, linkId, answer) %>%
  dplyr::mutate(linkId = as.integer(linkId)) %>%
  dplyr::mutate(answer = as.integer(answer)) %>%
  stats::na.omit()

### Check if NA have been removed, should return an whole positive number.
nrow(data_selected) / 21

### Select questions 
#### avg_questions => Questions which are positively annotated, answer index 0 = best answer for example.
#### avg_negative_questions => Questions whicha re negatively annotaded, answer index with the highes number = best answer
#### other_question => question Which can not be calcluated like the others.
avg_questions <- c(1, 2, 3, 4, 5, 6, 7, 8,9 ,11, 12, 13,14,15)
avg_negative_questions <- c(16, 17, 18,20)
other_question <- 10

### Filter and assign Data to DataFrames
normal_questions <- data_selected %>%
  arrange(linkId) %>%
  filter(linkId %in% avg_questions)

negative_questions <- data_selected %>%
  arrange(linkId) %>%
  filter(linkId %in% avg_negative_questions)

other_questions <- data_selected %>%
  arrange(linkId) %>%
  filter(linkId == 10) %>% 
  dplyr::select(subject, answer)

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
      #answer >= look$zero_index ~ as.integer(0),
      answer >= look$zero_index & answer < look$na_index ~ as.integer(0),
      #answer >= look$na_index ~ NA
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
      #answer > look$zero_index ~ as.integer(1),
      answer > look$zero_index & answer < look$na_index ~ as.integer(1),
      #answer >= look$na_index ~ NA
    ))
  mutated_questions <- rbind(mutated_questions, temp_questions)
}

### Prepare the ranged_answer dataframe.
range_answer <- data_selected %>%
  filter(linkId == 19) %>%
  mutate(coding = answer)

### Prepare the dataframe for the regression, by combining the range answer (0-10) with the other data.
mutated_questions_regression <- rbind(mutated_questions, range_answer)


### Filter rows containing NAs
mutated_questions <- mutated_questions %>%
  filter(!is.na(coding))%>%
  select(subject, linkId, coding, answer)


### Function to create a matrix which can be used for further statistical calculations.
### Q19 is a boolean, if enabled the Question 19 is not being excluded.
### df is the dataframe on which the calculation is performed
prep_matrix <- function(df, Q19){
  wide_data <- df %>%
    group_by(subject, linkId) %>%
    summarise(coding = max(coding),.groups = 'drop') %>%
    pivot_wider(names_from = linkId, values_from = coding,names_prefix = "Q")
    #dplyr::select(-subject)
  
  if(Q19 == T){
  temp_data <- wide_data %>% dplyr::select(Q19)
  temp_subject <- wide_data %>% dplyr::select(subject)
  wide_data <- wide_data %>% dplyr::select(-Q19)
  }
  matrix <- wide_data %>%
    mutate(across(everything(), ~ {
      # Replace NA with column mean
      filled <- ifelse(is.na(.), mean(., na.rm = TRUE), .)
      # Round values to 0 or 1
      rounded <- ifelse(filled < 0.5, 0, 1)
      return(rounded)
    }))
  matrix <- matrix %>% dplyr::select(-subject)
  
  if(Q19==T){
  matrix <- cbind(matrix, temp_data)
  matrix <- cbind(matrix, temp_subject)
  
  }
  
  return(matrix) 
}


### Reduce the selection of questions to create a more readable correlation matrix
selected_short <- c(1, 2, 4, 5, 12, 13, 17,19)
selected_short_q <- paste0("Q", selected_short)
mutated_questions_regression <- mutated_questions_regression %>% filter(linkId %in% selected_short)

mat2 <- prep_matrix(mutated_questions_regression,T)

### Transform the subject of Q10 into a factor so it can be used in the regression.
df_merge <- merge(mat2, other_questions, by="subject") %>%
  rename("Q10" = answer) %>%
  mutate(Q10 = factor(Q10))%>%
  pivot_wider(names_from = Q10, values_from = Q10, names_prefix = "P",
              values_fn = length, values_fill = 0) %>%
  dplyr::select(-subject)

### create a linear model.
mat2_model <- lm(df_merge$Q19 ~ ., data = df_merge)
summary(mat2_model)

### calculate the best model possible using the stepAIC() function.
stepwise_model <- stepAIC(mat2_model, direction = "both")
summary(stepwise_model)

### create some correlation matrices
mat <- prep_matrix(mutated_questions, F)
corr <- round(cor(mat,method = "pearson"), 2)
p.mat <- cor_pmat(mat)
ggcorrplot(corr, hc.order = T, outline.color = "black", lab=T, type = "lower", p.mat = p.mat,  insig = "blank",title = "Korrelationsmatrix SCAPE-Fragen")

corr2 <- round(cor(mat2,method = "pearson"), 2)
p.mat2 <- cor_pmat(mat2)
ggcorrplot(corr2, hc.order = T, outline.color = "black", lab=T, type = "lower", p.mat = p.mat2,  insig = "blank",title = "Korrelationsmatrix SCAPE-Fragen")





