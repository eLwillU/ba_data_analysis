library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(MASS)
library(psych)
library(jtools)
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

lookup_data <- lookup_data %>% dplyr::select(linkId, NumAnswers)

merged_df <- merge(data_selected, lookup_data, by="linkId")
test <- merged_df %>%
  mutate(answer = ifelse(!is.na(NumAnswers), NumAnswers - answer, answer))

data_wide <- test %>%
  group_by(subject, linkId) %>%
  summarise(answer = min(answer),.groups = 'drop') %>%
  pivot_wider(names_from = linkId, values_from = answer,names_prefix = "Q") %>%
  dplyr::select(-c(subject, Q9)) %>%
  mutate(Q10 = factor(Q10))%>%
  pivot_wider(names_from = Q10, values_from = Q10, names_prefix = "P",
              values_fn = length, values_fill = 0)


corr <- round(cor(data_wide,method = "pearson"), 2)
p.mat <- cor_pmat(data_wide)
ggcorrplot(corr, hc.order = T, outline.color = "black", lab=T, type = "lower", p.mat = p.mat,  insig = "blank",title = "Korrelationsmatrix Fragebogen",
                   ggtheme = ggplot2::theme_bw,
)


model <- lm(data_wide$Q19 ~ ., data = data_wide)
summary(model)
summ(model)
plot_summs(model)
stepwise_model <- stepAIC(model, direction = "both")
summary(stepwise_model)
boxplot(data_wide$Q19)

data_aic <- data_wide %>%
  dplyr::select(Q19,Q1,Q2,Q3,Q6,Q7,Q13,Q14,Q15,Q16,Q17)

corr2 <- round(cor(data_aic,method = "pearson"), 2)
p.mat2 <- cor_pmat(data_aic)
ggcorrplot(corr2, hc.order = T, outline.color = "black", lab=T, type = "lower", p.mat = p.mat2,  insig = "blank",title = "Korrelationsmatrix SCAPE-Fragen")

library(corrplot)
library(PerformanceAnalytics)
library(lares)
corr_cross(data_aic, rm.na = T, max_pvalue = 0.05, top = 15, grid = T) + geom_label(fontface = "bold")

dägg <- freqs(data_aic)

?corr_cross
distr(data_aic, plot=T)
chart.Correlation(corr2)

model2 <- lm(data_aic$Q19 ~ ., data = data_aic)
summary(model2)


model1 <- lm(data_wide$Q19 ~ ., data = data_wide)
stepwise_Modell <- stepAIC(model1, direction = "both")
summary(model1)
summary(stepwise_Modell)

model1 <- glm(data_wide$P1 ~ ., data = data_wide)
stepwise_Modell <- stepAIC(model1, direction = "both")
summary(model1)
summary(stepwise_Modell)


stepwise_model <- stepAIC(model2, direction = "both")
summary(stepwise_model)

aic_selected <- data_aic %>%
  dplyr::select(Q1,Q2,Q3,Q7,Q14,Q16,Q17, Q19)

aic_renamed <- data_aic %>%
  dplyr::select(Q1,Q2,Q3,Q7,Q14,Q16,Q17, Q19) %>%
  rename("Nebenwirkungen" = Q1) %>%
  rename("Langzeitnebenwirkungen" = Q2) %>%
  rename("Einbezug in Behandlungsentscheide" = Q3) %>%
  rename("Ratschläge für Langzeitfolgen" = Q7) %>%
  rename("Zusammenarbeit der Leistungserbringer" = Q14) %>%
  rename("Verfügbarkeit Krankenakte/Ergebnisse" = Q16)%>%
  rename("Wiedersprüchliche Informationen" = Q17)%>%
  rename("Gesamtzufriedenheit" = Q19)

corr_cross(aic_renamed, rm.na = T, max_pvalue = 0.05, top = 15, grid = T) 

corr2 <- round(cor(aic_selected,method = "pearson"), 2)
p.mat2 <- cor_pmat(aic_selected)
ggcorrplot(corr2, hc.order = T, 
           outline.color = "black", 
           lab=T, 
           type = "lower", 
           p.mat = p.mat2,  
           insig = "blank",
           title = "Abbildung 1: Korrelationsmatrix") + theme(plot.title = element_text(size=18)) 


