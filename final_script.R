## THIS IS THE CODE FOR MY MASTER'S DISSERTATION
## STUDENT: MARIA LUIZA DIAS CAMPOS
## COURSE: BEHAVIOURAL SCIENCES MSc
## TITLE: Fertility Decision-Making in Brazil: Impact of Full-Time Education 
## on the Prevention of Early Pregnancy among Young Women in Amazon. 

## Null hypothesis: municipalities with full-time high school enrolments are not more 
## likely to have lower odds of early pregnancy rate compared to 
## municipalities without full-time high school enrolments. 

## Methodology: GLMM with Beta family estimators. The dependent variable is a ratio
## within the open interval (0,1) and the main independent variable is a dummy
## which equals to 1 if the municipality has enrolments in high school full time, 
## or 0 otherwise. 

## Conclusion: overall, municipalities with enrolments in high school full time 
## are indeed correlated with lower chances of early childbearing, compared to  
## municipalities without enrolments – and this relationship is especially 
## significant for municipalities in the lower-to-medium income level group.
## Hence, this study suggests that full time education in teenage years might
## indeed play a better role in changing fertility intentions and decisions 
## compared to traditional education, rejecting the null hypothesis firstly stated.
## However, the challenges with modelling and the extremely limited dataset 
## do not allow us to assert that this will be the general case for the Amazon region,
## throughout space and time. More detailed data and causal estimation methodologies 
## are necessary to provide greater robustness to the relationship in question.

# Loading packages -------------------------------------------------------

library(car) ## VIF
library(betareg) ## Beta regression
library(psych)
library(tidyverse)
library(readxl)
library(dplyr)
library(broom.mixed)
library(glmmTMB) ## GLMM model
library(writexl)
library(DHARMa)
library(performance)
library(cvTools)
library(caret)
library(ggplot2)
library(sjPlot) ## Tables
library(brms)
library(loo)

install.packages("extrafont")
library(extrafont)
font_import(pattern = "Times", prompt = FALSE)
loadfonts(device = "pdf")
fonts()


# loading dataset ---------------------------------------------------------

## I tested the following models with two different samples. 
## For the 2010-2015 dataset, I have a smaller sample but more controls.
## For the 2010-2017 dataset, I have a bigger sample but less controls. 
## The reported models are from the 2010-2015 dataset since the models were
## better adjusted for that sample. 

municipalities_2015 <- read_excel("/Users/malucampos/Documents/dissertation/Codes/FINAL_MATERIAL/municipalities_2015.xlsx")
municipalities <- read_excel("/Users/malucampos/Documents/dissertation/Codes/FINAL_MATERIAL/municipalities.xlsx")

# cleaning data ---------------------------------------------------

municipalities_2015 <- 
  municipalities_2015[!is.na(municipalities_2015$HDI_index_FIRJAN), ]

municipalities <- 
  municipalities[!is.na(municipalities$early_pregnancy_rt_15_19), ]

# Checking multicollinearity  ----------------------------------------------

lm_model_adjusted <- lm(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male + primary_health_care_coverage,
  data = municipalities_2015
)
vif_values_adjusted <- vif(lm_model_adjusted)
print(vif_values_adjusted)

## No problems with multicollinearity detected. 

# Preliminary analysis  -------------------------------------------------------

## checking dependent variable

psych::describe(municipalities_2015$early_pregnancy_rt_15_19, quant = c(.25, .75))

## doing histogram

ggplot(municipalities_2015, aes(x = early_pregnancy_rt_15_19)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "gray") +
  labs(title = "Histogram Early Pregnancy Rate (15 to 19 years old)",
       x = "Early Pregnancy Rate (15 to 19 years old)",
       y = "Frequency") +
  theme_minimal(base_family = "Times New Roman")

## normality test

shapiro.test(municipalities_2015$early_pregnancy_rt_15_19)

## boxplot

ggplot(municipalities_2015, aes(x = "", y = early_pregnancy_rt_15_19)) +
  geom_boxplot() +
  labs(title = "Boxplot for Early Pregnancy (15 to 19 years old)",
       y = "Early Pregnancy Rate (15 to 19 years old)",
       x = "") +
  theme_minimal(base_family = "Times New Roman")


## doing scatter plot

ggplot(municipalities_2015, aes(x = d_enrollments_high_school_full, y = early_pregnancy_rt_15_19)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot: Dummy of Enrollments (High School Full Time)",
       x = "Dummy of Enrollments (High School)",
       y = "Early Pregnancy Rate (15 to 19 years old)") +
  theme_minimal(base_family = "Times New Roman")

## pearson's coefficient 

p_correlation <- cor(municipalities_2015$d_enrollments_high_school_full, municipalities_2015$early_pregnancy_rt_15_19, method = "pearson")
print(p_correlation)

test_correlation <- cor.test(municipalities_2015$d_enrollments_high_school_full, municipalities_2015$early_pregnancy_rt_15_19, method = "pearson")
print(test_correlation)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 
## We rejected the null, p-value is very low. There is evidence for a linear relationship. 
## However, Pearson correlation test assumes normality and it is sensitive towards outliers. 
## So I am doing Spearman correlation test.

## Doing the Spearman correlation test

result_spearman <- cor.test(municipalities_2015$d_enrollments_high_school_full, 
                            municipalities_2015$early_pregnancy_rt_15_19, method = "spearman")
print(result_spearman)

# Extract the correlation coefficient and p-value

rho <- result_spearman$estimate
p_value <- result_spearman$p.value

# Create the scatter plot with the correlation result annotation

ggplot(municipalities_2015, aes(x = d_enrollments_high_school_full, y = early_pregnancy_rt_15_19)) +
  geom_point(color = "black", alpha = 0.6) + # Add scatter points with blue color and some transparency
  geom_smooth(method = "loess", color = "red", se = FALSE) + # Add a smooth trend line
  labs(title = "Spearman Correlation between High School Full-Time Enrollments and Early Pregnancy",
       x = "High School Full-Time Enrollments (dummy)",
       y = "Early Pregnancy Rate (15-19 years)") +
  theme_minimal(base_family = "Times New Roman") + # Apply a minimalistic theme with Times New Roman font
  annotate("text", x = 0.5, y = max(municipalities_2015$early_pregnancy_rt_15_19), 
           label = paste("Spearman's rho =", round(rho, 3), "\n", "p-value =", format(p_value, digits = 3, scientific = FALSE)), 
           hjust = 0, vjust = 1, family = "Times New Roman")

## P-value is too low, indicating that there is a real correlation between those variables. 
## However, the correlation is negative and weak. 
## The Spearman correlation test does not consider the dependence within the sample.
## That's why we are doing the GLMM. 

# Generalized linear Mixed Model -------------------------------------------------------

# Filtering the dataset 

# I am filtering the dataset because municipalities are highly unequal between each other.
# This was an attempt of grouping municipalities that might be similar. 
# However, this was also a limitation, since the groups below are still very heterogeneous.
# Creating more sub-samples would make the samples even smaller. 

## 2010-2015 sample

low_pib_data_2015 <- subset(municipalities_2015, pib_tercil == "low")
medium_pib_data_2015 <- subset(municipalities_2015, pib_tercil == "medium")
high_pib_data_2015 <- subset(municipalities_2015, pib_tercil == "high")

## 2010-2017 sample

municipalities$pib_tercil <- cut(municipalities$gdp_pc,
                                      breaks = quantile(municipalities$gdp_pc, probs = seq(0, 1, by = 1/3), na.rm = TRUE),
                                      include.lowest = TRUE,
                                      labels = c("low", "medium", "high"))

low_pib_data <- subset(municipalities, pib_tercil == "low")
medium_pib_data <- subset(municipalities, pib_tercil == "medium")
high_pib_data <- subset(municipalities, pib_tercil == "high")

# running the model

## full model

## dataset until 2015

glmm_model_global <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc +
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = municipalities_2015
)
summary(glmm_model_global)

## dataset until 2017

glmm_model_global_2 <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school +
    proportion_male + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = municipalities
)
summary(glmm_model_global_2)


# Making tables -----------------------------------------------------------

custom_labels <- c("(Intercept)" = "Intercept",
                   "d_enrollments_high_school_full" = "High School Enrollments (Dummy)",
                   "log_gdp_pc" = "Log GDP per Capita",
                   "failures_high_school" = "High School Failures",
                   "dropouts_high_school" = "High School Dropouts",
                   "HDI_index_FIRJAN" = "HDI Index",
                   "proportion_male" = "Proportion of Males",
                   "primary_health_care_coverage" = "Primary Health Care Coverage")


tab_model(glmm_model_global,glmm_model_global_2,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("2010-2015", "2010-2017"),
          title = "GLMM Model Summaries")

## reported table

tab_model(glmm_model_global,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("2010-2015"),
          title = "GLMM Model Summaries",
          file = "GLMM_Model_Summaries.tex")


# Making the graph for the global model -----------------------------------

# calculating the odds of the predicted values and adding them to the main dataset

municipalities_2015 <- municipalities_2015 %>%
  mutate(predicted_logit = predict(glmm_model_global, type = "link"),
         predicted_odds = exp(predicted_logit))

# Aggregate the predictions by the dummy variable

pred_data <- municipalities_2015 %>%
  group_by(d_enrollments_high_school_full) %>%
  summarise(mean_odds = mean(predicted_odds))

# Plot the results

ggplot(pred_data, aes(x = factor(d_enrollments_high_school_full), y = mean_odds)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Impact of High School Enrollments on Odds Ratio of Early Pregnancy",
    x = "High School Enrollments (0 = No, 1 = Yes)",
    y = "Mean Odds Ratio"
  ) +
  theme_minimal()

ggplot(municipalities_2015, aes(x = d_enrollments_high_school_full, y = predicted_odds)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Impact of High School Full Time Enrollments on Odds Ratio of Early Pregnancy",
    x = "High School Enrollments (0 = No, 1 = Yes)",
    y = "Predicted Odds Ratio of Early Pregnancy (15 to 19 years old)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman")
  )

# income ranges -----------------------------------------------------------

## low income municipalities (2015)

glmm_model_low_2015 <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = low_pib_data_2015
)
summary(glmm_model_low_2015)

## low income municipalities 

glmm_model_low <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school +
    proportion_male + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = low_pib_data
)
summary(glmm_model_low)


# Making tables -----------------------------------------------------------

tab_model(glmm_model_low_2015, glmm_model_low,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("(2010-2015)", "(2010-2017)"),
          title = "GLMM Model Summaries for Low Income Municipalities")

# -------------------------------------------------------------------------

# medium income municipalities (2015)

glmm_model_medium_2015 <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = medium_pib_data_2015
)
summary(glmm_model_medium_2015)

# medium income municipalities

glmm_model_medium <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school +
    proportion_male + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = medium_pib_data
)
summary(glmm_model_medium)

# Making tables -----------------------------------------------------------

tab_model(glmm_model_medium_2015, glmm_model_medium,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("(2010-2015)", "(2010-2017)"),
          title = "GLMM Model Summaries for Medium Income Municipalities")

## reported table

tab_model(glmm_model_low_2015, glmm_model_medium_2015,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("Low Income", "Medium Income"),
          title = "GLMM Model Summaries for Low to Medium Income Municipalities",
          file = "glmm_model_summaries.tex") 

# -------------------------------------------------------------------------

# higher income municipalities (2015) 

glmm_model_high_2015 <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = high_pib_data_2015
)
summary(glmm_model_high_2015)

# higher income municipalities 

glmm_model_high <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school +
    proportion_male + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = high_pib_data
)
summary(glmm_model_high)

# Making tables -----------------------------------------------------------

tab_model(glmm_model_high_2015, glmm_model_high,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("(2010-2015)", "(2010-2017)"),
          title = "GLMM Model Summaries for High Income Municipalities")

## reported table

tab_model(glmm_model_high_2015,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("High Income"),
          title = "GLMM Model Summaries for High Income Municipalities",
          file = "glmm_model_summaries_high.tex")

# checking model fit ------------------------------------------------------

## LOW-INCOME

simulate_residuals_low <- simulateResiduals(glmm_model_low)
simulate_residuals_low_2015 <- simulateResiduals(glmm_model_low_2015)
plot(simulate_residuals_low)
plot(simulate_residuals_low_2015)

## the larger sample, removing controls, make the model fit worse. That's why I am reporting only
## the 2015 dataset results, where results for adjustment are better. 

## MEDIUM-INCOME

simulate_residuals_medium <- simulateResiduals(glmm_model_medium)
simulate_residuals_medium_2015 <- simulateResiduals(glmm_model_medium_2015)
plot(simulate_residuals_medium)
plot(simulate_residuals_medium_2015)

## HIGHER INCOME

simulate_residuals_high <- simulateResiduals(glmm_model_high)
plot(simulate_residuals_high)

### the model is generally well-fitted with some minor areas of deviation which are not significant.

## GLOBAL MODEL

simulate_residuals_global <- simulateResiduals(fittedModel = glmm_model_global)
plot(simulate_residuals_global)

## even though the residual plot is saying that the model is well-adjusted,
## maybe the lack of variation between years and municipalities is
## making GLMM very complex to this data.

## Limitations: over fitting, definitely. Model complexity and small sample size. 

# Results in terms of odds ratio ------------------------------------------

## I am calculating the odds ratio for every significant variable. 
## However, I am calculating only the percentage change for the main independent variable
## which is enrolments in high school full time. 

## LOW INCOME

psych::describe(low_pib_data$gdp_pc, quant = c(.25, .75))

## The PIB per capita here is between 3.137 to 5.610. 
## Number of observations: 119. 
## Range: 2.472,91

## extracting coefficients

fixed_effects_low_2015 <- fixef(glmm_model_low_2015)$cond
fixed_effects_low <- fixef(glmm_model_low)$cond

## isolating coefficients 

coef_d_enrollments_2015 <- fixed_effects_low_2015["d_enrollments_high_school_full"]
coef_d_enrollments <- fixed_effects_low["d_enrollments_high_school_full"]
coef_proportion_male_2015 <- fixed_effects_low_2015["proportion_male"]
coef_proportion_male <- fixed_effects_low["proportion_male"]
coef_log_gdp <- fixed_effects_low["log_gdp_pc"]

## calculating odds ratio

or_d_enrollments_2015 <- exp(coef_d_enrollments_2015)
or_d_enrollments <- exp(coef_d_enrollments)
or_proportion_male_2015 <- exp(coef_proportion_male_2015)
or_proportion_male <- exp(coef_proportion_male)
or_gdp_pc <- exp(coef_log_gdp)

## the percentage change in odds

percent_change_d_enrollments_2015 <- (1 - or_d_enrollments_2015) * 100
percent_change_d_enrollments <- (1 - or_d_enrollments) * 100

## print results

cat("Odds Ratio and Percentage Change in the Odds (2010-2015):\n")
cat("d_enrollments_2015: OR =", or_d_enrollments_2015, ", Mudança Percentual =", 
    percent_change_d_enrollments_2015, "%\n")

cat("Odds Ratio and Percentage Change in the Odds (2010-2019):\n")
cat("d_enrollments: OR =", or_d_enrollments, ", Mudança Percentual =", 
    percent_change_d_enrollments, "%\n")

## MEDIUM INCOME

psych::describe(medium_pib_data$gdp_pc, quant = c(.25, .75))

## The PIB per capita here is between 5.633 to 7.749. 
## Number of observations: 119. 
## Range: 2.115,5

# extracting coefficients

fixed_effects_medium_2015 <- fixef(glmm_model_medium_2015)$cond
fixed_effects_medium <- fixef(glmm_model_medium)$cond

## isolating coefficients 

coefm_d_enrollments_2015 <- fixed_effects_medium_2015["d_enrollments_high_school_full"]
coefm_dropouts_high_school_2015 <- fixed_effects_medium_2015["dropouts_high_school"]
coefm_proportion_male_2015 <- fixed_effects_medium_2015["proportion_male"]
coefm_proportion_male <- fixed_effects_medium["proportion_male"]

## calculating odds ratio

orm_d_enrollments_high_school_full <- exp(coefm_d_enrollments_2015)
orm_dropouts_high_school <- exp(coefm_dropouts_high_school_2015)
orm_proportion_male_2015 <- exp(coefm_proportion_male_2015)
orm_proportion_male <- exp(coefm_proportion_male)

## the percentage change in odds

percentm_change_d_enrollments <- (1 - orm_d_enrollments_high_school_full) * 100

## print results

cat("Odds Ratio and Percentage Change in the Odds (2010-2015):\n")
cat("d_enrollments_high_school_full: OR =", orm_d_enrollments_high_school_full, ", Mudança Percentual =", 
    percentm_change_d_enrollments, "%\n")

## HIGH INCOME

psych::describe(high_pib_data$gdp_pc, quant = c(.25, .75))

## The PIB per capita here is between 7.604 to 56.055. 
## Number of observations: 124. 
## Range: 48451.42

fixed_effects_high <- fixef(glmm_model_high)$cond

## isolating coefficients 

coefh_proportion_male <- fixed_effects_high["proportion_male"]
coefh_dropouts <- fixed_effects_high["dropouts_high_school"]

## calculating odds ratio

orh_proportion_male <- exp(coefh_proportion_male)
orh_dropouts <- exp(coefh_dropouts)

## GLOBAL MODEL

## extracting coefficients

coef_global_2015 <- fixef(glmm_model_global)$cond
coef_global <- fixef(glmm_model_global_2)$cond

## isolating coefficients 

coef_d_enrollments_2015 <- coef_global_2015["d_enrollments_high_school_full"]
coef_d_enrollments <- coef_global["d_enrollments_high_school_full"]
coef_proportion_male_2015 <- coef_global_2015["proportion_male"]
coef_proportion_male <- coef_global["proportion_male"]

## calculating odds ratio

or_g_enrollments_2015 <- exp(coef_d_enrollments_2015)
or_g_enrollments <- exp(coef_d_enrollments)
or_g_proportion_male_2015 <- exp(coef_proportion_male_2015)
or_g_proportion_male <- exp(coef_proportion_male)

## the percentage change in odds

percent_change_d_enrollments_2015 <- (1 - or_g_enrollments_2015) * 100
percent_change_d_enrollments <- (1 - or_g_enrollments) * 100

## print results

cat("Odds Ratio and Percentage Change in the Odds (2010-2015):\n")
cat("d_enrollments_high_school_full: OR =", or_g_enrollments_2015, ", Mudança Percentual =", 
    percent_change_d_enrollments_2015, "%\n")

cat("Odds Ratio and Percentage Change in the Odds (2010-2019):\n")
cat("d_enrollments_high_school_full: OR =", or_g_enrollments, ", Mudança Percentual =", 
    percent_change_d_enrollments, "%\n")


# PROPORTION OF FEMALES ---------------------------------------------------

## full model

glmm_model_global_f <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc +
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_female + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = municipalities_2015
)
summary(glmm_model_global_f)

## low income municipalities

glmm_model_low_f <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_female + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = low_pib_data_2015
)
summary(glmm_model_low_f)

# medium income municipalities

glmm_model_medium_f <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_female + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = medium_pib_data_2015
)
summary(glmm_model_medium_f)

# higher income municipalities 

glmm_model_high_f <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_female + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = high_pib_data_2015
)
summary(glmm_model_high_f)

# GENERATING TABLES -------------------------------------------------------

tab_model(glmm_model_global_f,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,
          dv.labels = c("Amazonian Municipalities (2010-2015)"),
          title = "GLMM Global Model Summaries (Females)",
          file = "model_females_global.tex"
)

tab_model(
  glmm_model_low_f, glmm_model_medium_f,
  show.ci = NULL, 
  show.se = TRUE, 
  show.est = TRUE, 
  show.std = NULL,
  transform = NULL,
  pred.labels = custom_labels,
  dv.labels = c("Low Income", "Medium Income"),
  title = "GLMM Model Summaries for Low to Medium Income Municipalities (Females)",
  file = "model_females_low_medium.tex"
)

tab_model(glmm_model_high_f,  
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,
          dv.labels = c("High Income"),
          title = "GLMM Model Summaries for High Income Municipalities (Females)",
          file = "model_females_high.tex"
)


# BETA REGRESSION VS GLMM MODEL -------------------------------------------

# Checking another methodology, more simple. 
# I did not report the Beta Regression due to the AIC test - explained below. 
# Also, the simple beta regression  retrieved very strange and hard results to explain. 

beta_model_global <- betareg(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc +
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage | municipality + year,
  data = municipalities_2015,
  link = "logit"
)
summary(beta_model_global)

tab_model(beta_model_global, glmm_model_global,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("Beta Regression", "GLMM Model"),
          title = "Beta Model vs GLMM model Summaries (2010-2015)",
          file = "beta_vs_GLMM.tex")

## when modelling for the dispersion variables, year and municipality, the model
## changes drastically, indicating that accounting for random effects and variability
## is necessary. 
## Conclusion: even though the variability between years and municipalities are low,
## it is significant to explain the early pregnancy rate in Amazon.

# Akaike Information Criterion (AIC) > choosing the model

## Burnham, K. P., & Anderson, D. R. (2002). Model Selection and Multimodel Inference: 
## A Practical Information-Theoretic Approach. Springer Science & Business Media.

aic_model1 <- AIC(beta_model_global)
aic_model2 <- AIC(glmm_model_global)

print(aic_model1)
print(aic_model2)

# comparing

if (aic_model1 < aic_model2) {
  print("model 1 is better.")
} else {
  print("modelo 2 is better.")
}

## strong evidence that GLMM is better adjusted than Beta regression.

#  BAYESIAN APPROACH: robustness check -----------------------------------

# To run a Bayesian approach was an attempt to provide a robustness check. 
## The Bayesian approach fits better small samples. 

## defining distributions a priori

prior <- c(
  set_prior("normal(0, 10)", class = "b"), 
  set_prior("student_t(3, 0, 10)", class = "sd"), 
  set_prior("gamma(2, 0.1)", class = "phi") 
)

brms_model <- brm(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc +
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = Beta(link = "logit"),
  data = municipalities_2015,
  prior = prior,
  iter = 4000, warmup = 2000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.99)
)
summary(brms_model)

tab_model(brms_model, glmm_model_global, 
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("Bayesian Approach", "GLMM Estimates"),
          title = "Bayesian Approach vs GLMM Estimates (2010-2015)",
          file = "model_bayesian_glmm.tex")

## this results show that the enrolments are still significant, but the sigma squared could not
## be calculated. Small sample size and bad data quality could explain this situation. 

