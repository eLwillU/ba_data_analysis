library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
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

### Check if NA have been removed
nrow(data_selected) / 21


avg_questions <- c(1, 2, 3, 4, 5, 6, 7, 8,9 ,11, 12, 13)
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
range_answer <- data_selected %>%
  filter(linkId == 19) %>%
  mutate(coding = answer)
mutated_questions_regression <- rbind(mutated_questions, range_answer)


selected_questions <- c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,20,21)

### Filter rows containing NAs
mutated_questions <- mutated_questions %>%
  filter(!is.na(coding))%>%
  select(subject, linkId, coding, answer)


# Create the sample dataset
df <- mutated_questions

prep_matrix <- function(df, Q19){
  
  wide_data <- df %>%
    group_by(subject, linkId) %>%
    summarise(coding = max(coding),.groups = 'drop') %>%
    pivot_wider(names_from = linkId, values_from = coding,names_prefix = "Q")
    #dplyr::select(-subject)
  
  if(Q19 == T){
  temp_data <- wide_data %>% dplyr::select(Q19)
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
  
  if(Q19==T){
  matrix <- cbind(matrix, temp_data)
  }
  matrix <- matrix %>% dplyr::select(-subject)
  return(matrix) 
}
library(MASS)

mat2 <- prep_matrix(mutated_questions_regression,T)
model <- lm(mat2$Q19 ~ ., data = mat2)
summary(model <- lm(mat2$Q19 ~ ., data = mat2))

stepwise_model <- stepAIC(model, direction = "both")
summary(stepwise_model)

mat <- prep_matrix(mutated_questions, F)

corr <- round(cor(mat,method = "pearson"), 2)
p.mat <- cor_pmat(mat)

ggcorrplot(corr, hc.order = T, outline.color = "black", lab=T, type = "lower", p.mat = p.mat,  insig = "blank",title = "Korrelationsmatrix SCAPE-Fragen")


corr2 <- round(cor(mat2,method = "pearson"), 2)
p.mat2 <- cor_pmat(mat2)

ggcorrplot(corr2, hc.order = T, outline.color = "black", lab=T, type = "lower", p.mat = p.mat2,  insig = "blank",title = "Korrelationsmatrix SCAPE-Fragen")



#ggcorrplot(corr, hc.order = TRUE, outline.color = "white", lab=T, p.mat = p.mat,  insig = "blank")
positive_proportion <- colMeans(wide_data_filled_rounded, na.rm = TRUE)
print(positive_proportion)

library(psych)
efa_results <- fa(wide_data_filled_rounded, nfactors = 2, rotate = "varimax")
print(efa_results)

cronbach_alpha <- psych::alpha(wide_data_filled_rounded)
print(cronbach_alpha)
########################



# Create the sample dataset
df <- mutated_questions %>% select(linkId, coding) %>%
  group_by(row_id = row_number() %/% max(linkId))  %>%
  pivot_wider(names_from = linkId, values_from = coding, names_prefix = "Q") %>%
  ungroup() %>%
  select(-row_id)


df$id <- ave(df$linkId, df$linkId, FUN = seq_along)
reshaped_df <- pivot_wider(df, names_from = id, values_from = coding)
# Add respondent_id
df <- df %>%
  mutate(respondent_id = (row_number() - 1) %/% 21 + 1)

# Transform the data to wide format
df_wide <- df %>%
  pivot_wider(names_from = linkId, values_from = coding, names_prefix = "Q") %>%
  select(-c("respondent_id"))
# View the transformed dataset
print(df_wide)
corr <- round(cor(df_wide,method = "pearson"), 1)
p.mat <- cor_pmat(df_wide)
p.mat
ggcorrplot(corr, hc.order = T, outline.color = "black", lab=T, type = "lower", p.mat = p.mat,  insig = "blank",title = "Korrelationsmatrix SCAPE-Fragen")
#ggcorrplot(corr, hc.order = TRUE, outline.color = "white", lab=T, p.mat = p.mat,  insig = "blank")
