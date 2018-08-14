# README: This script carries out the regression models to identify the moral hazard
#----------------------------------#

# Settings

remove(list = ls())
PathBase <- "C:/Users/tuananhle/Documents/NA/Thesis"
PathRaw <- paste(PathBase, "/Raw Data", sep = "")
PathProcessed <- paste(PathBase, "/Processed Data", sep = "")
PathOut <- paste(PathBase, "/Output", sep = "")
PathCode <- paste(PathBase, "/code", sep = "")
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
  moral[[paste(i, "2015", sep = "_")]] <- with(moral, interaction(gender,wave_2015))
}

# List of RHS variables used in the unrestricted model
unrestricted <- setdiff(names(moral), "wave")

# List of RHS variables used in the restricted model
restricted <- union(setdiff(setdiff(names(moral), "wave"), 
                            names(moral)[grep("_2015",names(moral))]), "wave_2015")
#----------------------------------#

# LINEAR REGRESSION MODEL

# Restricted model
linregr <- lm(preventive ~ ., data = moral[, names(moral) %in% restricted])
summary(linregr)
linregr <- tidy(linregr)
write.csv(linregr, "Linear regression restricted model.csv")

# Unrestricted model
linregu <- lm(preventive ~ ., data = moral[, names(moral) %in% unrestricted])
summary(linregu)
linregu <- tidy(linregu)
write.csv(linregu, "Linear regression unrestricted model.csv")
#----------------------------------#

# LOGISTC REGRESSION MODEL

# Restricted model
logregr <- glm(preventive ~ ., data = moral[, names(moral) %in% restricted])
summary(logregr)
logregr <- tidy(logregr)
write.csv(linregr, "Logistic regression restricted model.csv")

# Marginal effects of restricted model


