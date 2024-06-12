### Import necessary libraries
# Load the required libraries
library(readr)       
library(dplyr)       
library(readxl)      
library(tidyr)       
library(ggplot2)     
library(ggcorrplot)  
library(MASS)        
library(psych)       
library(jtools)   
library(corrplot)              
library(PerformanceAnalytics)  
library(lares)         

### Import and assign data
# Read the CSV file and remove rows with NA values
exported_data <- readr::read_delim("./data/exported_data.csv", delim = ";", na = "NA") %>% na.omit()
# Read the Excel file
lookup_data <- readxl::read_excel("./data/lookup_table_strict.xlsx")
# Convert the data to a data frame
data <- base::data.frame(exported_data)

### Select necessary columns and remove NAs
# Select specific columns and convert data types
data_selected <- data %>%
  dplyr::select(subject, linkId, answer) %>%
  dplyr::mutate(linkId = as.integer(linkId)) %>%
  dplyr::mutate(answer = as.integer(answer)) %>%
  stats::na.omit()  # Remove rows with NA values

# Select necessary columns from lookup data
lookup_data <- lookup_data %>% dplyr::select(linkId, NumAnswers)

# Merge the data sets by 'linkId'
merged_df <- merge(data_selected, lookup_data, by="linkId")
# Adjust the 'answer' based on 'NumAnswers'
test <- merged_df %>%
  mutate(answer = ifelse(!is.na(NumAnswers), NumAnswers - answer, answer))

# Transform the data into a wide format
data_wide <- test %>%
  group_by(subject, linkId) %>%
  summarise(answer = min(answer), .groups = 'drop') %>%
  pivot_wider(names_from = linkId, values_from = answer, names_prefix = "Q") %>%
  dplyr::select(-c(subject)) %>%
  mutate(Q10 = factor(Q10)) %>%
  pivot_wider(names_from = Q10, values_from = Q10, names_prefix = "P",
              values_fn = length, values_fill = 0)

# Calculate correlation matrix and p-values
corr <- round(cor(data_wide, method = "pearson"), 2)
p.mat <- cor_pmat(data_wide)
# Plot the correlation matrix
ggcorrplot(corr, hc.order = T, outline.color = "black", lab = T, type = "lower", p.mat = p.mat, insig = "blank",
           title = "Korrelationsmatrix Fragebogen", ggtheme = ggplot2::theme_bw)

### Linear models and stepwise regression
# Fit a linear model
model <- lm(data_wide$Q19 ~ ., data = data_wide)
# Summary of the model
summary(model)
# Enhanced summary of the model
summ(model)
# Plot the regression summaries
plot_summs(model)
# Stepwise model selection
stepwise_model <- stepAIC(model, direction = "both")
# Summary of the stepwise model
summary(stepwise_model)
summ(stepwise_model)
# Boxplot of Q19
boxplot(data_wide$Q19)

# Subset of data for AIC
data_aic <- data_wide %>%
  dplyr::select(Q19, Q1, Q2, Q3, Q6, Q7, Q13, Q14, Q15, Q16, Q17)

# Calculate correlation matrix and p-values for subset
corr2 <- round(cor(data_aic, method = "pearson"), 2)
p.mat2 <- cor_pmat(data_aic)
# Plot the correlation matrix
ggcorrplot(corr2, hc.order = T, outline.color = "black", lab = T, type = "lower", p.mat = p.mat2, insig = "blank")

# Cross correlation plot
corr_cross(data_aic, rm.na = T, max_pvalue = 0.05, top = 15, grid = T) + geom_label(fontface = "bold")

# Frequency distribution
dägg <- freqs(data_aic)

# Distribution plot
distr(data_aic, plot = T)
# Correlation chart
chart.Correlation(corr2)

# Fit another linear model
model2 <- lm(data_aic$Q19 ~ ., data = data_aic)
# Summary of the model
summary(model2)

# Fit a linear model on the full data set
model1 <- lm(data_wide$Q19 ~ ., data = data_wide)
# Stepwise model selection
stepwise_Modell <- stepAIC(model1, direction = "both")
# Summaries of the models
summary(model1)
summary(stepwise_Modell)

# Fit a generalized linear model
model1 <- glm(data_wide$P1 ~ ., data = data_wide)
# Stepwise model selection
stepwise_Modell <- stepAIC(model1, direction = "both")
# Summaries of the models
summary(model1)
summary(stepwise_Modell)

# Stepwise model selection for model2
stepwise_model <- stepAIC(model2, direction = "both")
# Summary of the stepwise model
summary(stepwise_model)

### AIC selected data and correlation plots
# Select specific columns for AIC analysis
aic_selected <- data_aic %>%
  dplyr::select(Q1, Q2, Q3, Q7, Q14, Q16, Q17, Q19)

# Rename columns for better readability
aic_renamed <- data_aic %>%
  dplyr::select(Q1, Q2, Q3, Q7, Q14, Q16, Q17, Q19) %>%
  rename("Nebenwirkungen" = Q1) %>%
  rename("Langzeitnebenwirkungen" = Q2) %>%
  rename("Einbezug in Behandlungsentscheide" = Q3) %>%
  rename("Ratschläge für Langzeitfolgen" = Q7) %>%
  rename("Zusammenarbeit der Leistungserbringer" = Q14) %>%
  rename("Verfügbarkeit Krankenakte/Ergebnisse" = Q16) %>%
  rename("Wiedersprüchliche Informationen" = Q17) %>%
  rename("Gesamtzufriedenheit" = Q19)

# Cross correlation plot for renamed data
corr_cross(aic_renamed, rm.na = T, max_pvalue = 0.05, top = 15, grid = T)

# Correlation plot for selected AIC data
corr2 <- round(cor(aic_selected, method = "pearson"), 2)
p.mat2 <- cor_pmat(aic_selected)
ggcorrplot(corr2, hc.order = T, outline.color = "black", lab = T, type = "lower", p.mat = p.mat2, insig = "blank")
