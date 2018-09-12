# README: This script carries out the regression models to identify the moral hazard
#----------------------------------#

# Settings

remove(list = ls())
PathBase <- "C:/Users/tuananhle/Documents/NA/Thesis"
PathRaw <- paste(PathBase, "/Raw Data", sep = "")
PathProcessed <- paste(PathBase, "/Processed Data", sep = "")
PathOut <- paste(PathBase, "/Output", sep = "")
PathCode <- paste(PathBase, "/thesis-code", sep = "")
#----------------------------------#

# CONSTRUCT VARIABLES FROM ANOTHER R SCRIPT

setwd(PathCode)
source("variables.R")
#----------------------------------#

# ADDITIONAL SETTINGS #

# Required package to export regression results
if (!require(broom)) {
  install.packages("broom")
  library(broom)
}

# Required package to compute marginal effects
if (!require(mfx)) {
  install.packages("mfx")
  library(mfx)
}

# Install package to export R results in LATEX format
if (!require(stargazer)) {
  install.packages("stargazer")
  library(stargazer)
}

# Set directory for output
setwd(PathOut)

# Select the relevant variables for the regression
moral <- subset(moral, select = - c(hhnetinc_pc, status, idind, uebmi, urbmi, status, rncms))
#----------------------------------#

# GENERATE ADDITIONAL VARIABLES FOR THE REGRESSION #

# Create dummy variales for 2015
moral$wave_2015 <- ifelse(moral$wave == 2015, 1, 0)
moral$wave_2015 <- factor(moral$wave_2015,
                        labels = c("Wave 2011", "Wave 2015"))

# Create a list of variables on RHS
continuous <- c("loginc", "age", "bmi") ## Continuous variables
discrete <- setdiff(names(moral), union(continuous, c("preventive", "wave", "wave_2015"))) ## Discrete variables

# Create interaction variables to account for observations in 2015 only (in unrestricted model)

for (i in continuous){
  moral[[paste(i, "2015", sep = "_")]] <- ifelse(moral$wave == 2011, 0, moral[[i]])
}

for (i in discrete){
  moral[[paste(i, "2015", sep = "_")]] <- interaction(moral[[i]], moral$wave_2015)
}

# Relabel factors
levels(moral$gender_2015) <- c(rep("Male.Wave 2015",3), "Female.Wave 2015")
levels(moral$ethnicity_2015) <- c(rep("Minority.Wave 2015", 3), "Han.Wave 2015")
levels(moral$educ_2015) <- c(rep("No qualification.Wave 2015", 6), "Primary school.Wave 2015", "Middle school.Wave 2015",
                             "Vocational school.Wave 2015", "University degree and higher.Wave 2015")
levels(moral$employment_2015) <- c(rep("Unemployed.Wave 2015", 3), "Employed.Wave 2015")
levels(moral$diet_2015) <- c(rep("Healthy.Wave 2015", 3), "Unhealthy.Wave 2015")
levels(moral$exercise_2015) <- c(rep("Active.Wave 2015", 3), "Inactive.Wave 2015")
levels(moral$urban_2015) <- c(rep("Rural.Wave 2015", 3), "Urban.Wave 2015")
levels(moral$province_2015) <- c(rep("Chongqing.Wave 2015", 13), "Beijing.Wave 2015", "Liaoning.Wave 2015",
                                 "Heilongjiang.Wave 2015", "Shanghai.Wave 2015", "Jiangsu.Wave 2015", "Shandong.Wave 2015",
                                 "Henan.Wave 2015", "Hubei.Wave 2015", "Hunan.Wave 2015", "Guangxi.Wave 2015",
                                 "Guizhou.Wave 2015")
levels(moral$diabetes_2015) <- c(rep("No diabetes.Wave 2015", 3), "Has diabetes.Wave 2015")
levels(moral$pressure_2015) <- c(rep("Normal blood pressure.Wave 2015", 3), "High blood pressure.Wave 2015")

# Data for unrestricted model
unrestricted <- setdiff(names(moral), "wave")            ## RHS variables in the unrestricted model
unrestricted <- moral[, names(moral) %in% unrestricted]

# Data for restricted model
restricted <- union(setdiff(setdiff(names(moral), "wave"), 
                            names(moral)[grep("_2015",names(moral))]), "wave_2015") ## RHS variables in restricted model
restricted <- moral[, names(moral) %in% restricted]

#----------------------------------#

# LOGISTC REGRESSION RESTRICTED MODEL #

# Execute the model
logregr <- glm(preventive ~ ., family = "binomial", data = restricted)
summary(logregr)
write.csv(tidy(logregr), "Logistic regression restricted model.csv")

# Odds ratio
or <- round(exp(coef(logregr)), digits = 3)
write.csv(or, "Odds ratio of restricted model.csv")

# Average marginal effect
sink("Average marginal effect.txt")
amer <- logitmfx(preventive ~., atmean = FALSE, data = restricted)
amer
sink()

# Marginal effect at means
sink("Marginal effect at means.txt")
memr <- logitmfx(preventive ~., atmean = TRUE, data = restricted)
memr
sink()

# Prediction
prop <- NROW(restricted[restricted$preventive == 1, ]) / NROW(restricted) ## Set the threshold
predict <- predict.glm(logregr, type = "response")
restricted[ , "predict"] <- predict
restricted$predict <- ifelse(restricted$predict >= prop, 1, 0)
contingency <- with(restricted, table(predict, preventive, dnn = c("Predicted", "Actual")))
write.csv(contingency, "Contingency predicted vs actual of restricted model.csv")
#----------------------------------#

# LOGISTC REGRESSION UNRESTRICTED MODEL #

# Execute the model
logregu <- glm(preventive ~ ., family = "binomial", data = unrestricted)
summary(logregu)
write.csv(tidy(logregu), "Logistic regression unrestricted model.csv")
#----------------------------------#

# TESTS FOR MODEL SELECTION #

# Likelihood ratio test
lrtest(logregr, logregu)

# Alkaike Information Criterion
AICu <- AIC(logregu)
AICr <- AIC(logregr)

# Schawrz's Bayesian Information Criterion
BICu <- BIC(logregu)
BICr <- BIC(logregr)

# Export test results
selection <- data.frame(Model = c("Unrestricted", "Restricted"), AIC = c(AICu, AICr), BIC = c(BICu, BICr))
write.csv(selection, "Model selection.csv")
#----------------------------------#

# LINEAR REGRESSION MODEL #

# Restricted model
restricted <- restricted[, setdiff(names(restricted), "predict")]
linregr <- lm(preventive ~ ., data = restricted)
summary(linregr)
write.csv(tidy(linregr), "Linear regression restricted model.csv")
#----------------------------------#

# EXPORT REGRESSION RESULTS #

# Latex table of the regression results
latex <- stargazer(logregr, logregr, logregr, logregr, linregr, logregu, title = "Full regression results",
                   align = TRUE, dep.var.labels = "Use of preventive care",
                   no.space = TRUE)
write.csv(latex, "latex.csv")
#----------------------------------#

# REFERENCE (required by the author) #

# Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.1. https://CRAN.R-project.org/package=stargazer