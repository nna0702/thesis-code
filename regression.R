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
#----------------------------------#

# Set directory for output
setwd(PathOut)

# Select the relevant variables for the regression
moral <- subset(moral, select = - c(hhnetinc_pc, status, idind, uebmi, urbmi, status, rncms))
#----------------------------------#

# GENERATE ADDITIONAL VARIABLES FOR THE REGRESSION

# Create dummy variales for 2015
moral$wave_2015 <- ifelse(moral$wave == 2015, 1, 0)
moral$wave_2015 <- factor(moral$wave_2015,
                          labels = c("Wave 2011", "Wave 2015"))

# Create a list of variables on RHS
rhs <- setdiff(names(moral), c("preventive", "wave", "wave_2015"))

# Create interaction variables to account for observations in 2015 only (in unrestricted model)
for (i in rhs){
  moral[[paste(i, "2015", sep = "_")]] <- ifelse(moral$wave == 2011, 0, moral[[i]])
}

# Data for unrestricted model
unrestricted <- setdiff(names(moral), "wave")            ## RHS variables in the unrestricted model
unrestricted <- moral[, names(moral) %in% unrestricted]

# Data for restricted model
restricted <- union(setdiff(setdiff(names(moral), "wave"), 
                            names(moral)[grep("_2015",names(moral))]), "wave_2015") ## RHS variables in restricted model
restricted <- moral[, names(moral) %in% restricted]
#----------------------------------#

# LINEAR REGRESSION MODEL

# Restricted model
linregr <- lm(preventive ~ ., data = restricted)
summary(linregr)
write.csv(tidy(linregr), "Linear regression restricted model.csv")

# Unrestricted model
linregu <- lm(preventive ~ ., data = unrestricted)
summary(linregu)
write.csv(tidy(linregu), "Linear regression unrestricted model.csv")
#----------------------------------#

# LOGISTC REGRESSION RESTRICTED MODEL

# Execute the model
logregr <- glm(preventive ~ ., family = "binomial", data = restricted)
summary(logregr)
write.csv(tidy(logregr), "Logistic regression restricted model.csv")

# Odds ratio
or <- round(exp(coef(logregr)), digits = 3)
write.csv(or, "Odds ratio of restricted model.csv")

# Average marginal effect
ame <- logitmfx(preventive ~., atmean = FALSE, data = restricted)
ame

# Marginal effect at means
mem <- logitmfx(preventive ~., atmean = TRUE, data = restricted)
mem

# Prediction
prop <- NROW(restricted[restricted$preventive == 1, ]) / NROW(restricted) ## Set the threshold
predict <- predict.glm(logregr, type = "response")
restricted[ , "predict"] <- predict
restricted$predict <- ifelse(restricted$predict >= prop, 1, 0)
contingency <- with(restricted, table(predict, preventive, dnn = c("Predicted", "Actual")))
write.csv(contingency, "Contingency predicted vs actual of restricted model.csv")
#----------------------------------#

# LOGISTC REGRESSION UNRESTRICTED MODEL

# Execute the model
logregu <- glm(preventive ~ ., family = "binomial", data = unrestricted)
summary(logregu)
write.csv(tidy(logregu), "Logistic regression unrestricted model.csv")

# Odds ratio
or <- round(exp(coef(logregu)), digits = 3)
write.csv(or, "Odds ratio of unrestricted model.csv")

# Average marginal effect
ame <- logitmfx(preventive ~., atmean = FALSE, data = unrestricted)
ame

# Marginal effect at means
mem <- logitmfx(preventive ~., atmean = TRUE, data = unrestricted)
mem

# Prediction
prop <- NROW(unrestricted[unrestricted$preventive == 1, ]) / NROW(unrestricted) ## Set the threshold
predict <- predict.glm(logregu, type = "response")
unrestricted[ , "predict"] <- predict
unrestricted$predict <- ifelse(unrestricted$predict >= prop, 1, 0)
contingency <- with(unrestricted, table(predict, preventive, dnn = c("Predicted", "Actual")))
write.csv(contingency, "Contingency predicted vs actual of unrestricted model.csv")


