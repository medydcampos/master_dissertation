## THIS IS THE CODE FOR MY MASTER'S DISSERTATION
## STUDENT'S ID: 32548
## TITLE: Fertility Decision-Making in Brazil: Impact of Full-Time Education 
## on the Prevention of Early Pregnancy among Young Women in Amazon

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


# loading dataset ---------------------------------------------------------

municipalities_2015 <- read_excel("/Users/malucampos/Documents/dissertation/Codes/FINAL_MATERIAL/municipalities_2015.xlsx")


# Transforming variables ---------------------------------------------------


municipalities_2015$primary_health_care_coverage <- ifelse(
  municipalities_2015$primary_health_care_coverage == 0, 
  0.0001, 
  ifelse(municipalities_2015$primary_health_care_coverage == 1, 
         0.9999, 
         municipalities_2015$primary_health_care_coverage)
)

# Checking multicollinearity  ----------------------------------------------

lm_model_adjusted <- lm(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + failures_high_school + dropouts_high_school + HDI_index_FIRJAN
  + proportion_male + primary_health_care_coverage,
  data = municipalities_2015
)
vif_values_adjusted <- vif(lm_model_adjusted)
print(vif_values_adjusted)

# running the model: Generalized linear Mixed Model -------------------------------------------------------

## checking dependent variable

psych::describe(municipalities_2015$early_pregnancy_rt_15_19, quant = c(.25, .75))
hist(municipalities_2015$early_pregnancy_rt_15_19,
     main = "Histogram Early Pregnancy Rate (15 to 19 years old)", xlab = "Early Pregnancy Rate (15 to 19 years old)")
shapiro.test(municipalities_2015$early_pregnancy_rt_15_19)
boxplot(municipalities_2015$early_pregnancy_rt_15_19, main = "Boxplot for early pregnancy (15 to 19 yearsold)")


## preliminary analysis

plot(municipalities_2015$d_enrollments_high_school_full, municipalities_2015$early_pregnancy_rt_15_19, 
     main = "Scatter Plot: dummy of Enrollments (high school full time)",
     xlab = "Dummy of Enrollments (High School)",
     ylab = "Early Pregnancy Rate (15 to 19 years old)")

linear_enrollments_high_school <- lm(early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full, data = municipalities_2015)
summary(linear_enrollments_high_school)
abline(linear_enrollments_high_school, col = "red")
check_model(linear_enrollments_high_school, check = c("all"))

## pearson's coefficient 

p_correlation <- cor(municipalities_2015$d_enrollments_high_school_full, municipalities_2015$early_pregnancy_rt_15_19, method = "pearson")
print(p_correlation)

test_correlation <- cor.test(municipalities_2015$d_enrollments_high_school_full, municipalities_2015$early_pregnancy_rt_15_19, method = "pearson")
print(test_correlation)

## Pearson test 
## Null Hypothesis: there is no linear correlation between variables. 
## low p-values mean REJECTION of the null hypothesis. 

# filtering dataset -------------------------------------------------------

low_pib_data <- subset(municipalities_2015, pib_tercil == "low")
medium_pib_data <- subset(municipalities_2015, pib_tercil == "medium")
high_pib_data <- subset(municipalities_2015, pib_tercil == "high")

## running the model

# running the model -------------------------------------------------------

## full model

glmm_model_global <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc +
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = municipalities_2015
)
summary(glmm_model_global)


# income ranges -----------------------------------------------------------

## low income municipalities

glmm_model_low <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = low_pib_data
)
summary(glmm_model_low)

# medium income municipalities

glmm_model_medium <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = medium_pib_data
)
summary(glmm_model_medium)

# higher income municipalities 

glmm_model_high <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_male + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = high_pib_data
)
summary(glmm_model_high)

# checking model fit ------------------------------------------------------

## LOW-INCOME

simulate_residuals_low <- simulateResiduals(glmm_model_low)
plot(simulate_residuals_low)

## the residual analysis suggests that the model is well adjusted. Residuals don't follow
## any specific trend or pattern and are well distributed along the reference line.

## MEDIUM-INCOME

simulate_residuals_medium <- simulateResiduals(glmm_model_medium)
plot(simulate_residuals_medium)

## the residual analysis suggests that the model is well adjusted. Residuals don't follow
## any specific trend or pattern and are well distributed along the reference line. 

## HIGHER INCOME

simulate_residuals_high <- simulateResiduals(glmm_model_high)
plot(simulate_residuals_high)

### the model is generally well-fitted with some minor areas of deviation.

## GLOBAL MODEL

simulate_residuals_global <- simulateResiduals(fittedModel = glmm_model_global)
plot(simulate_residuals_global)

## even though the residual plot is saying that the model is well-adjusted,
## maybe the lack of variation between years and municipalities is
## making GLMM very complex to this data. 


# Results in terms of odds ratio ------------------------------------------

## LOW INCOME

psych::describe(low_pib_data$gdp_pc, quant = c(.25, .75))

## The PIB per capita here is between 3.137 to 5.610. 
## Number of observations: 119. 
## Range: 2.472,91

## extracting coefficients

fixed_effects_low <- fixef(glmm_model_low)$cond

## isolating coefficients 

coef_d_enrollments_high_school_full <- fixed_effects_low["d_enrollments_high_school_full"]
coef_proportion_male <- fixed_effects_low["proportion_male"]

## calculating odds ratio

or_d_enrollments_high_school_full <- exp(coef_d_enrollments_high_school_full)
or_proportion_male <- exp(coef_proportion_male)

## the percentage change in odds

percent_change_d_enrollments_high_school_full <- (1 - or_d_enrollments_high_school_full) * 100

## print results

cat("Odds Ratio and Percentage Change in the Odds:\n")
cat("d_enrollments_high_school_full: OR =", or_d_enrollments_high_school_full, ", Mudança Percentual =", 
    percent_change_d_enrollments_high_school_full, "%\n")

## MEDIUM INCOME

psych::describe(medium_pib_data$gdp_pc, quant = c(.25, .75))

## The PIB per capita here is between 5.633 to 7.749. 
## Number of observations: 119. 
## Range: 2.115,5

# extracting coefficients

fixed_effects_medium <- fixef(glmm_model_medium)$cond

## isolating coefficients 

coefm_d_enrollments_high_school_full <- fixed_effects_medium["d_enrollments_high_school_full"]
coefm_dropouts_high_school <- fixed_effects_medium["dropouts_high_school"]
coefm_proportion_male <- fixed_effects_medium["proportion_male"]

## calculating odds ratio

orm_d_enrollments_high_school_full <- exp(coefm_d_enrollments_high_school_full)
orm_dropouts_high_school <- exp(coefm_dropouts_high_school)
orm_proportion_male <- exp(coefm_proportion_male)


## the percentage change in odds

percentm_change_d_enrollments_high_school_full <- (1 - orm_d_enrollments_high_school_full) * 100

## print results

cat("Odds Ratio and Percentage Change in the Odds:\n")
cat("d_enrollments_high_school_full: OR =", orm_d_enrollments_high_school_full, ", Mudança Percentual =", 
    percentm_change_d_enrollments_high_school_full, "%\n")

## HIGH INCOME

psych::describe(high_pib_data$gdp_pc, quant = c(.25, .75))

## The PIB per capita here is between 7.604 to 56.055. 
## Number of observations: 124. 
## Range: 48451.42

## isolating coefficients 

coefh_proportion_male <- fixed_effects_high["proportion_male"]

## calculating odds ratio

orh_proportion_male <- exp(coefh_proportion_male)

## GLOBAL MODEL

## extracting coefficients

coef_global <- fixef(glmm_model_global)$cond

## isolating coefficients 

coef_d_enrollments_high_school_full <- coef_global["d_enrollments_high_school_full"]
coef_proportion_male <- coef_global["proportion_male"]

## calculating odds ratio

or_g_enrollments_high_school_full <- exp(coef_d_enrollments_high_school_full)
or_g_proportion_male <- exp(coef_proportion_male)

## the percentage change in odds

percent_change_d_enrollments_high_school_full <- (1 - or_g_enrollments_high_school_full) * 100

## print results

cat("Odds Ratio and Percentage Change in the Odds:\n")
cat("d_enrollments_high_school_full: OR =", or_g_enrollments_high_school_full, ", Mudança Percentual =", 
    percent_change_d_enrollments_high_school_full, "%\n")


# MAKING TABLES -----------------------------------------------------------

custom_labels <- c("(Intercept)" = "Intercept",
                   "d_enrollments_high_school_full" = "High School Enrollments (Dummy)",
                   "log_gdp_pc" = "Log GDP per Capita",
                   "failures_high_school" = "High School Failures",
                   "dropouts_high_school" = "High School Dropouts",
                   "HDI_index_FIRJAN" = "HDI Index",
                   "proportion_male" = "Proportion of Males",
                   "primary_health_care_coverage" = "Primary Health Care Coverage")

## LOW TO MEDIUM

tab_model(glmm_model_low, glmm_model_medium,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("Low Income", "Medium Income"),
          title = "GLMM Model Summaries for Low to Medium Income Municipalities")

# HIGH

tab_model(glmm_model_high,  
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("High Income"),
          title = "GLMM Model Summaries for High Income Municipalities")

# GLOBAL

tab_model(glmm_model_global,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("Amazonian Municipalities (2010-2015)"),
          title = "GLMM Model Summaries")


# PROPORTION OF FEMALES ---------------------------------------------------

## full model

glmm_model_global_f <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc +
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_female + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = municipalities_2015
)
summary(glmm_model_global)

## low income municipalities

glmm_model_low_f <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_female + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = low_pib_data
)
summary(glmm_model_low_f)

# medium income municipalities

glmm_model_medium_f <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_female + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = medium_pib_data
)
summary(glmm_model_medium_f)

# higher income municipalities 

glmm_model_high_f <- glmmTMB(
  early_pregnancy_rt_15_19 ~ d_enrollments_high_school_full + log_gdp_pc + 
    failures_high_school + dropouts_high_school + HDI_index_FIRJAN +
    proportion_female + primary_health_care_coverage + (1 | year) + (1 | municipality),
  family = beta_family(link = "logit"),
  data = high_pib_data
)
summary(glmm_model_high_f)


# GENERATING TABLES -------------------------------------------------------

tab_model(glmm_model_global_f,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels_m,  # Apply custom labels
          dv.labels = c("Amazonian Municipalities (2010-2015)"),
          title = "GLMM Model Summaries")

tab_model(glmm_model_low_f, glmm_model_medium_f,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels_m,  # Apply custom labels
          dv.labels = c("Low Income", "Medium Income"),
          title = "GLMM Model Summaries for Low to Medium Income Municipalities")

tab_model(glmm_model_high_f,  
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels_m,  # Apply custom labels
          dv.labels = c("High Income"),
          title = "GLMM Model Summaries for High Income Municipalities")


# BETA REGRESSION VS GLMM MODEL -------------------------------------------

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
          title = "Beta Model vs GLMM model Summaries")

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

#  BAYESIAN APPROACH -----------------------------------

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

tab_model(brms_model,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels_m,  # Apply custom labels
          dv.labels = c("Amazonian Municipalities (2010-2015)"),
          title = "GLMM Model Summaries")

