# README: This script performs descriptive statistics
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

# SETTINGS #

# Set the directory to save the outputs
setwd(PathOut)  

# Install the package to plot graphs
if (!require(ggplot2)) {        
  install.packages("ggplot2")
  library(ggplot2)
}

# Load the libraries to create frequency tables and scales for plots
library(plyr)
library(scales)
#----------------------------------#

# USE OF PREVENTIVE CARE BY INSURANCE STATUS

# Plot the graph (absolute numbers)
sample$status <- relevel(sample$status, "Uninsured")    ## Reorder the levels of the factor variable
status_plot <- ggplot(sample, aes(x = status, y = preventive)) + 
  geom_bar(stat="identity", fill= "#56B4E9", width = 0.5) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="lightgrey"),
        axis.ticks = element_blank()) +
  labs(x = "", y = "")
ggsave("status.png")

# Plot the graph (in %)
preventive_tab <- with(sample, table(status, preventive, useNA = "ifany"))         ## Construct frequency table
preventive_tab <- as.data.frame.matrix(prop.table(preventive_tab, margin = 1))
preventive_tab$status <- c("Uninsured", "RNCMS", "UEBMI", "URBMI")
preventive_tab$status <- factor(preventive_tab$status,                             ## Reorder insurance status for the plot
                                levels = c("Uninsured", "RNCMS", "UEBMI", "URBMI"))
names(preventive_tab)[names(preventive_tab) == "1"] <- "preventive"
status_pct_plot <- ggplot(preventive_tab,aes(x = status, y = preventive)) + 
  geom_bar(stat="identity", fill= "#56B4E9", width = 0.5) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="lightgrey"),
        axis.ticks = element_blank()) +
  labs(x = "", y = "") + scale_y_continuous(labels = percent)
ggsave("status by pct.png")
write.csv(preventive_tab, "Use of preventive care.csv")

# Turn preventive into a factor
sample$preventive <- factor(sample$preventive,
                            labels = c("Not used preventive care", "Used preventive care"))

# Frequency table of insurance status                                      
status_tab <- table(sample$status, dnn = c("Insurance plan status"), useNA = "ifany")
status_tab_prop <- round(prop.table(status_tab), digits = 2)    ## Turn into a proportion table
write.csv(status_tab, "Insurance status.csv")                   ## Save the table for output in the paper
#----------------------------------#

# DESCRIPTIVE STATISTICS OF THE SAMPLE AND SUB-SAMPLES #

# Create a list of samples
dflist <- list(sample, sample[sample$status == "Uninsured", ], sample[sample$status == "RNCMS", ],
               sample[sample$status == "UEBMI", ], sample[sample$status == "URBMI", ])
# 
# Remove variable "status"
sample <- subset(sample, select = - status)

# Create a list of factor variables
factors <- setdiff(variables, c("hhnetinc_pc", "loginc", "rncms", "uebmi", "urbmi", "preventive", "status", "age", "bmi"))  

# Summary statistics of factor variables for whole sample
mean_all <- list()
for (i in 1:length(factors)) {
  mean_all[[i]] <- as.data.frame(count(dflist[[1]][[factors[[i]]]]))
}
mean_all <- do.call(rbind, mean_all)
mean_all <- as.data.frame(mean_all)
mean_all[[2]] <- round(mean_all[[2]]/NROW(dflist[[1]]), digits = 2)
names(mean_all) <- c("Variable", "All")

# Summary statistics of factor variables for sample of uninsured
mean_uninsured <- list()
for (i in 1:length(factors)) {
  mean_uninsured[[i]] <- as.data.frame(count(dflist[[2]][[factors[[i]]]]))
}
mean_uninsured <- do.call(rbind, mean_uninsured)
mean_uninsured <- as.data.frame(mean_uninsured)
mean_uninsured[[2]] <- round(mean_uninsured[[2]]/NROW(dflist[[2]]), digits = 2)
names(mean_uninsured) <- c("Variable", "Uninsured")

# Summary statistics of factor variables for sample of RNCMS
mean_RNCMS <- list()
for (i in 1:length(factors)) {
  mean_RNCMS[[i]] <- as.data.frame(count(dflist[[3]][[factors[[i]]]]))
}
mean_RNCMS <- do.call(rbind, mean_RNCMS)
mean_RNCMS <- as.data.frame(mean_RNCMS)
mean_RNCMS[[2]] <- round(mean_RNCMS[[2]]/NROW(dflist[[3]]), digits = 2)
names(mean_RNCMS) <- c("Variable", "RNCMS")

# Summary statistics of factor variables for sample of UEBMI
mean_UEBMI <- list()
for (i in 1:length(factors)) {
  mean_UEBMI[[i]] <- as.data.frame(count(dflist[[4]][[factors[[i]]]]))
}
mean_UEBMI <- do.call(rbind, mean_UEBMI)
mean_UEBMI <- as.data.frame(mean_UEBMI)
mean_UEBMI[[2]] <- round(mean_UEBMI[[2]]/NROW(dflist[[4]]), digits = 2)
names(mean_UEBMI) <- c("Variable", "UEBMI")

# Summary statistics of factor variables for sample of URBMI
mean_URBMI <- list()
for (i in 1:length(factors)) {
  mean_URBMI[[i]] <- as.data.frame(count(dflist[[5]][[factors[[i]]]]))
}
mean_URBMI <- do.call(rbind, mean_URBMI)
mean_URBMI <- as.data.frame(mean_URBMI)
mean_URBMI[[2]] <- round(mean_URBMI[[2]]/NROW(dflist[[5]]), digits = 2)
names(mean_URBMI) <- c("Variable", "URBMI")

# Merge lists
mean_list <- list(mean_all, mean_uninsured, mean_RNCMS, mean_UEBMI, mean_URBMI)
summary <- Reduce(function(x,y){
  join(x, y)
}, mean_list)

# Summary statistics of non-factor variable

## Age
age <- list()
for (i in 1:length(dflist)) {
  age[[i]] <- data.frame(Variable = "Age (in years)", 
                         All = round(mean(dflist[[i]]$age), digits = 0))
}
names(age[[2]])[[2]] <- "Uninsured"    ## Change the column headings
names(age[[3]])[[2]] <- "RNCMS"
names(age[[4]])[[2]] <- "UEBMI"
names(age[[5]])[[2]] <- "URBMI"
age <- Reduce(function(x,y){           ## Compile the summary statistics across samples
  join(x, y)
}, age)

## BMI
bmi <- list()
for (i in 1:length(dflist)) {
  bmi[[i]] <- data.frame(Variable = "BMI", 
                         All = round(mean(dflist[[i]]$bmi), digits = 1))
}
names(bmi[[2]])[[2]] <- "Uninsured"    ## Change the column headings
names(bmi[[3]])[[2]] <- "RNCMS"
names(bmi[[4]])[[2]] <- "UEBMI"
names(bmi[[5]])[[2]] <- "URBMI"
bmi <- Reduce(function(x,y){           ## Compile the summary statistics across samples
  join(x, y)
}, bmi)

## Household net income per capita
income <- list()
for (i in 1:length(dflist)) {
  income[[i]] <- data.frame(Variable = "Annual household income per capita (in RMB)", 
                            All = round(mean(dflist[[i]]$hhnetinc_pc), digits = 0))
}
names(income[[2]])[[2]] <- "Uninsured"    ## Change the column headings
names(income[[3]])[[2]] <- "RNCMS"
names(income[[4]])[[2]] <- "UEBMI"
names(income[[5]])[[2]] <- "URBMI"
income <- Reduce(function(x,y){           ## Compile the summary statistics across samples
  join(x, y)
}, income)

## Combine summary statistics of factor and non-factor variables
summary <- rbind(summary, age, bmi, income)

## Format the sizes of the sample from the frequency table created earlier
status_tab <- as.data.frame(t(as.matrix(status_tab)))  ## Turn into a dataframe
status_tab$All <- rowSums(status_tab)                  ## Create a column for whole sample size
status_tab$Variable <- "Sample size"                   ## Create the label for the sample size
status_tab <- status_tab[, c(6, 5, 1, 2, 3, 4)]        ## Reorder columns to be the same as summary table
summary <- rbind(status_tab, summary)                  ## Append sample size into the summary table

## Save the summary table
write.csv(summary, "Summary statistics.csv")

#----------------------------------#

# EXPLORE THE DISTRIBUTION OF HOUSEHOLD NET INCOME PER CAPITA AND AGE

# Install pacakges to describe distribution
if (!require(moments)) {        
  install.packages("moments")
  library(moments)
}

# Plot the distirbution of hhnetinc_pc
dist_inc <- ggplot(sample, aes(x = hhnetinc_pc)) + geom_histogram(fill = "#56B4E9", binwidth = 5000) 
dist_inc <- dist_inc + theme(panel.background = element_blank(),
                             panel.grid = element_blank(),
                             panel.grid.major.y = element_line(size=.1, color="grey"),
                             axis.ticks = element_blank()) + 
  labs(x = "", y = "")               
ggsave("Distribution of income.png") 

# Plot the distribution of log income
dist_loginc <- ggplot(sample, aes(x = loginc)) + geom_histogram(fill = "#56B4E9", binwidth = 0.2) 
dist_loginc <- dist_loginc + theme(panel.background = element_blank(),
                                   panel.grid = element_blank(),
                                   panel.grid.major.y = element_line(size=.1, color="grey"),
                                   axis.ticks = element_blank()) +  
  labs(x = "", y = "")               

ggsave("Distribution of log income.png") 

# Plot distribution of age
dist_age <- ggplot(sample, aes(x = age)) + geom_histogram(fill = "#56B4E9", binwidth = 2) 
dist_age <- dist_age + theme(panel.background = element_blank(),
                             panel.grid = element_blank(),
                             panel.grid.major.y = element_line(size=.1, color="lightgrey"),
                             axis.ticks = element_blank()) +  
  labs(x = "", y = "") 
ggsave("Distribution of age.png") 

# Calculate skewness, kurtosis, mean and median of the distribution of the two income-related variables and age
skewness <- sapply(sample[, c("hhnetinc_pc", "loginc", "age")], function(x) round(skewness(x), digits = 4))
kurtosis <- sapply(sample[, c("hhnetinc_pc", "loginc", "age")], function(x) round(kurtosis(x), digits = 4))
mean <- sapply(sample[, c("hhnetinc_pc", "loginc", "age")], function(x) round(mean(x), digits = 4))
median <- sapply(sample[, c("hhnetinc_pc", "loginc", "age")], function(x) round(median(x), digits = 4))
statistics <- rbind(mean, median, skewness, kurtosis)
statistics <- as.data.frame.matrix(statistics)
colnames(statistics) <- c("Household net income per capita", "Log household net income per capita", "Age (in years)")
write.csv(statistics, "Statistics of income variables and age.csv")
#----------------------------------#