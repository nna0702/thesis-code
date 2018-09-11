# README: This script merges raw data files into a master and select matched observations for waves 2011 & 2015

# Settings

remove(list = ls())
PathBase <- "C:/Users/tuananhle/Documents/NA/Thesis"
PathRaw <- paste(PathBase, "/Raw Data", sep = "")
PathProcessed <- paste(PathBase, "/Processed Data", sep = "")
PathOut <- paste(PathBase, "/Output", sep = "")
PathCode <- paste(PathBase, "/thesis-code", sep = "")
setwd(PathProcessed)
#----------------------------------#

# Load the relevant datasets

df <- read.csv("data.csv")
#----------------------------------#

# PRELIMINARY SELECTION #

# Remove people with missing insurance status
df <- subset(df, !is.na(df$m1))

# Only pick those aged above 18
df <- subset(df, df$age >= 18)

# Turn wave variable into a factor variable
df$wave <- factor(df$wave, levels = c("2011", "2015"))
#----------------------------------#

# SELECT SAMPLE BY INSURANCE PLANS #

# Recode uninsured individuals
df$m3a_0[df$m1 == 0 ] <- 0    ## Private health insurance
df$m3a_1[df$m1 == 0 ] <- 0    ## Free health insurance
df$m3a_12[df$m1 == 0 ] <- 0   ## Urban employee-based health insurance
df$m3a_13[df$m1 == 0 ] <- 0   ## Urban rural resident health insurance
df$m3a_4[df$m1 == 0 ] <- 0    ## New rural cooperative scheme
df$m3a_8[df$m1 == 0 ] <- 0    ## Other health insurance

# Select the appropriate insurance types
df <- subset(df, (m3a_0 == 0 & m3a_1 == 0 & m3a_8  == 0))   ## Remove individuals with plans other than the 3 main plans
df <- subset(df, !(m3a_12 == 1 & m3a_13 == 1))              ## Remove individuals with 2 basic plans
df <- subset(df, !(m3a_12 == 1 & m3a_4 == 1))               ## Remove individuals with 2 basic plans
df <- subset(df, !(m3a_13 == 1 & m3a_4 == 1))               ## Remove individuals with 2 basic plans
df <- subset(df, !(m3a_12 == 9 | m3a_13 == 9 | m3a_4 == 9)) ## Remove missing values
#----------------------------------#

# CONSTRUCT INURANCE STATUS VARIABLES #

# Rename variables which describe insurance status
names(df)[names(df) == "m3a_12"] <- "uebmi"
names(df)[names(df) == "m3a_13"] <- "urbmi"
names(df)[names(df) == "m3a_4"] <- "rncms"

# Generate a variable that describes insurance status
df$status <- 0                        ## Uninsured
df$status[df$rncms == 1] <- 1         ## Rural New Cooperative Medical Scheme (RNCMS)
df$status[df$uebmi == 1] <- 2         ## Urban Employee Basic Medical Insurance (UEBMI)
df$status[df$urbmi == 1] <- 3         ## Urban Resident Basic Medical Insurance (URBMI)

# Label the values of the variable describing insurance status
df$status <- factor(df$status,
                    labels = c("Uninsured", "RNCMS", "UEBMI", "URBMI"))
#----------------------------------#

# CONSTRUCT HEALTH UTILISATION MEASURES - PREVENTIVE CARE #

# Rename variable
names(df)[names(df) == "m47"] <- "preventive"

# Remove unknown values
df <- df[df$preventive != 9, ]
#----------------------------------#

# CONSTRUCT OTHER EXPLANATORY VARIABLES - REGIONAL VARIABLES #

# Province: Chongqing as reference group
names(df)[names(df) == "t1"] <- "province"
df$province <- factor(df$province, 
                      labels = c("Beijing", "Liaoning", "Heilongjiang", "Shanghai", "Jiangsu",
                                 "Shandong", "Henan", "Hubei", "Hunan", "Guangxi", "Guizhou", "Chongqing"))
df$province <- relevel(df$province, "Chongqing")

# Rural/ urban status: Rural as the reference group 
df$urban <- factor(df$urban,
                   labels = c("Rural", "Urban"))
#----------------------------------#

# CONSTRUCT OTHER EXPLANATORY VARIABLES - SOCIO-ECONOMIC & DEMOGRAPHIC VARIABLES #

# Gender: Male as the reference group
df$gender <- factor(df$gender, labels = c("Male", "Female"))

# Nationality: Minority as the reference group
names(df)[names(df) == "nationality"] <- "ethnicity"
df$ethnicity[df$ethnicity != 1] <- 0                  ## Treat unknown values as missing 
df$ethnicity <- factor(df$ethnicity,
                       labels = c("Minority", "Han"))

# Education level: Below primary school education as reference group
names(df)[names(df) == "a12"] <- "educ"
df$educ[df$a11 >=0 & df$a11 <= 14 & !is.na(df$a11)] <- 0  ## Classify "No school" as "No qualification attained"
df$educ[df$educ == 2 | df$educ == 3] <- 3                ## Categories of middle school
df$educ[df$educ == 6] <- 5                               ## Master and bachelor as degree category
df$educ[df$educ == 9] <- NA              ## Treat unknown values as missing
df$educ <- factor(df$educ,
                  labels = c("No qualification", "Primary school", "Middle school", 
                             "Vocational school", "University degree and higher"))

# Employment status: Binary variable
names(df)[names(df) == "b2"] <- "employment"
df$employment <- factor(df$employment,
                        labels = c("Unemployed", "Employed"))

# Household income per capita
df$hhinc[df$hhinc <= 0] <- NA               ## Treat non-positive values as missing
df$hhnetinc_pc <- df$hhinc / df$hhsize
df$loginc <- log(df$hhnetinc_pc)
#----------------------------------#

# CONSTRUCT OTHER EXPLANATORY VARIABLES - HEALTH VARIABLES #

# Health attitude

## Physical activity
names(df)[names(df) == "u406"] <- "exercise"
df$exercise[df$exercise == 9] <- NA
df$exercise <- ifelse(df$exercise < 3, 1, 0)                 ## Exercise not very important or not important at all
df$exercise <- factor(df$exercise,
                      labels = c("Active", "Inactive"))

## Diet
names(df)[names(df) == "u407"] <- "diet"
df$diet[df$diet == 9] <- NA
df$diet <- ifelse(df$diet < 3, 1, 0)                         ## Healthy diet not very important or not important at all
df$diet <- factor(df$diet, 
                  labels = c("Healthy", "Unhealthy"))

# BMI
df$bmi <- df$weight / (df$height/100)^2

# Chronic diseases

## Diabetes
names(df)[names(df) == "u24a"] <- "diabetes"
df$diabetes[df$diabetes == 9] <- NA
df$diabetes <- factor(df$diabetes,
                      labels = c("No diabetes", "Has diabetes"))

## High blood pressure
names(df)[names(df) == "u22"] <- "pressure"
df$pressure[df$pressure == 9] <- NA
df$pressure <- factor(df$pressure,
                      labels = c("Normal blood pressure", "High blood pressure"))
#----------------------------------#

# DEALING WITH MISSING VALUES #

# Create a list of relevant variables
variables <- c("idind","uebmi", "urbmi", "rncms", "preventive", 
               "gender", "age", "ethnicity","educ", "employment", "hhnetinc_pc", "loginc",
               "exercise", "diet", "status", "urban", "province", "wave", "bmi", "diabetes", "pressure")

# Remove observations with at least one missing values in the chosen variables
na_tab <- sapply(df[, variables], function (x) sum(is.na(x)))
na_tab <- data.frame(na_tab)
colnames(na_tab)[1] <- "No. of missing values"
setwd(PathOut)
write.csv(na_tab, "Missing values.csv")

df$na <- rowSums(is.na(df[, variables]))
sample <- subset(df[, variables], df$na == 0) 
#----------------------------------#

# Select the sample for descriptive analysis

sample <- sample[duplicated(sample$idind) | duplicated(sample$idind, fromLast = TRUE),]
stopifnot(NROW(sample[sample$wave == 2011, ]) == NROW(sample[sample$wave == 2015, ]))  ## Cross check
#----------------------------------#

# Select the sample for descriptive analysis and regression analysis to identify moral hazard

## Retain those with RNCMS
moral <- sample[sample$status == "RNCMS",]
#----------------------------------#