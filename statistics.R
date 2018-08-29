# README: This script performs descriptive statistics
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

# Install pacakges to describe distribution
if (!require(moments)) {        
  install.packages("moments")
  library(moments)
}
#----------------------------------#

# PLOT GRAPHS #

# Population breakdown by insurance status

sample$status <- relevel(sample$status, "Uninsured")           ## Reorder the levels of the factor variable
population <- ggplot(sample, aes(x = status, fill = wave)) + 
  geom_bar(width = 0.5, position = "dodge") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="lightgrey"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  labs(x = "Insurance group", y = "Number of respondents", fill = "Wave")
ggsave("population.png")

# Use of preventive care by insurance status (2011-2015)

## Construct frequency tables

preventive_tab_2011 <- with(sample[sample$wave == 2011, ], table(status, preventive, useNA = "ifany")) 
preventive_tab_2011 <- as.data.frame.matrix(prop.table(preventive_tab_2011, margin = 1))
preventive_tab_2015 <- with(sample[sample$wave == 2015, ], table(status, preventive, useNA = "ifany")) 
preventive_tab_2015 <- as.data.frame.matrix(prop.table(preventive_tab_2015, margin = 1))

## Clean the tables
preventive_tab_2011$wave <- 2011
preventive_tab_2011 <- preventive_tab_2011[, names(preventive_tab_2011) != "0"]
preventive_tab_2015$wave <- 2015
preventive_tab_2015 <- preventive_tab_2015[, names(preventive_tab_2015) != "0"]

## Combine two tables and prepare for the plot
preventive_tab <- rbind(preventive_tab_2015, preventive_tab_2011)
names(preventive_tab)[names(preventive_tab) == "1"] <- "preventive"
preventive_tab$status <- rep(c("Uninsured", "RNCMS", "UEBMI", "URBMI"),2)
preventive_tab$status <- factor(preventive_tab$status,                      ## Reorder insurance status for the plot
                                levels = c("Uninsured", "RNCMS", "UEBMI", "URBMI"))
names(preventive_tab)[names(preventive_tab) == "1"] <- "preventive"
preventive_tab$wave <- factor(preventive_tab$wave,                          ## Factorise wave for the plot
                              levels = c("2011", "2015"))
write.csv(preventive_tab, "Use of preventive care.csv")

## Plot the graph
status_pct_plot <- ggplot() + 
  geom_bar(aes(x = status, y = preventive, fill = wave), data = preventive_tab, stat = "identity", width = 0.5, 
           position_dodge()) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size=.1, color="lightgrey"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size= 10)) +
  labs(x = "Insurance group", y = "Proportion of respondents", fill = "Wave") + scale_y_continuous(labels = percent)
ggsave("status by pct.png")
#----------------------------------#

# DESCRIPTIVE STATISTICS OF THE SAMPLE AND SUB-SAMPLES #

# Create a list of samples
dflist <- list(moral, moral[moral$wave == "2011", ], moral[moral$wave == "2015", ])
# 
# Remove variable "status"
sample <- subset(sample, select = - status)

# Create a list of factor variables
factors <- setdiff(variables, c("hhnetinc_pc", "loginc", "rncms", "uebmi", "urbmi", "preventive", "status", 
                                "idind", "age", "bmi", "wave"))  

# Summary statistics of factor variables for whole sample
mean_all <- list()
for (i in 1:length(factors)) {
  mean_all[[i]] <- as.data.frame(count(dflist[[1]][[factors[[i]]]]))
}
mean_all <- do.call(rbind, mean_all)
mean_all <- as.data.frame(mean_all)
mean_all[[2]] <- round(mean_all[[2]]/NROW(dflist[[1]]), digits = 2)
names(mean_all) <- c("Variable", "All")

# Summary statistics of factor variables for 2011 sample
mean_2011 <- list()
for (i in 1:length(factors)) {
  mean_2011[[i]] <- as.data.frame(count(dflist[[2]][[factors[[i]]]]))
}
mean_2011 <- do.call(rbind, mean_2011)
mean_2011 <- as.data.frame(mean_2011)
mean_2011[[2]] <- round(mean_2011[[2]]/NROW(dflist[[2]]), digits = 2)
names(mean_2011) <- c("Variable", "2011")

# Summary statistics of factor variables for 2015 sample
mean_2015 <- list()
for (i in 1:length(factors)) {
  mean_2015[[i]] <- as.data.frame(count(dflist[[3]][[factors[[i]]]]))
}
mean_2015 <- do.call(rbind, mean_2015)
mean_2015 <- as.data.frame(mean_2015)
mean_2015[[2]] <- round(mean_2015[[2]]/NROW(dflist[[3]]), digits = 2)
names(mean_2015) <- c("Variable", "2015")

# Merge lists
mean_list <- list(mean_all, mean_2011, mean_2015)
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
names(age[[2]])[[2]] <- "2011"    ## Change the column headings
names(age[[3]])[[2]] <- "2015"
age <- Reduce(function(x,y){      ## Compile the summary statistics across samples
  join(x, y)
}, age)

## BMI
bmi <- list()
for (i in 1:length(dflist)) {
  bmi[[i]] <- data.frame(Variable = "BMI", 
                         All = round(mean(dflist[[i]]$bmi), digits = 1))
}
names(bmi[[2]])[[2]] <- "2011"    ## Change the column headings
names(bmi[[3]])[[2]] <- "2015"
bmi <- Reduce(function(x,y){      ## Compile the summary statistics across samples
  join(x, y)
}, bmi)

## Household net income per capita
income <- list()
for (i in 1:length(dflist)) {
  income[[i]] <- data.frame(Variable = "Annual household income per capita (in RMB)", 
                            All = round(mean(dflist[[i]]$hhnetinc_pc), digits = 0))
}
names(income[[2]])[[2]] <- "2011"    ## Change the column headings
names(income[[3]])[[2]] <- "2015"
income <- Reduce(function(x,y){      ## Compile the summary statistics across samples
  join(x, y)
}, income)

## Combine summary statistics of factor and non-factor variables
summary <- rbind(summary, age, bmi, income)

## Save the summary table
write.csv(summary, "Summary statistics.csv")
#----------------------------------#

# EXPLORE THE DISTRIBUTION OF HOUSEHOLD NET INCOME PER CAPITA AND AGE #

# Plot the distirbution of hhnetinc_pc
dist_inc <- ggplot(moral, aes(x = hhnetinc_pc, fill = wave)) + 
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) + xlim(0, 150000) + ## 21 extreme observations are removed
  labs(x = "Income level", y = "Number of respondents") 
dist_inc <- dist_inc + theme(panel.background = element_blank(),
                             panel.grid = element_blank(),
                             panel.grid.major.y = element_line(size=.1, color="lightgrey"),
                             axis.ticks = element_blank(),
                             axis.text = element_text(size=10),
                             axis.title = element_text(size=10, color = "black"),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=10))
ggsave("Distribution of income 2011 & 2015.png") 


# Plot the distribution of log income
dist_loginc <- ggplot(moral, aes(x = loginc, fill = wave)) + 
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) +
  labs(x = "Log of income level", y = "Number of respondents") 
dist_loginc <- dist_loginc + theme(panel.background = element_blank(),
                             panel.grid = element_blank(),
                             panel.grid.major.y = element_line(size=.1, color="lightgrey"),
                             axis.ticks = element_blank(),
                             axis.text = element_text(size=10),
                             axis.title = element_text(size=10, color = "black"),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=10))              
ggsave("Distribution of log income 2011 & 2015.png") 

# Calculate skewness, kurtosis, mean and median of the distribution of the two income-related variables and age

## Divide datatsets into two subsets by wave
moral11 <- moral[moral$wave == 2011, ]
moral15 <- moral[moral$wave == 2015, ]

## Statistics in 2011
skewness <- sapply(moral11[, c("hhnetinc_pc", "loginc")], function(x) round(skewness(x), digits = 4))
kurtosis <- sapply(moral11[, c("hhnetinc_pc", "loginc")], function(x) round(kurtosis(x), digits = 4))
mean <- sapply(moral11[, c("hhnetinc_pc", "loginc")], function(x) round(mean(x), digits = 4))
median <- sapply(moral11[, c("hhnetinc_pc", "loginc")], function(x) round(median(x), digits = 4))
statistics <- rbind(mean, median, skewness, kurtosis)
statistics <- as.data.frame.matrix(statistics)
colnames(statistics) <- c("Household net income per capita", "Log household net income per capita")
write.csv(statistics, "Statistics of income variables 2011.csv")

## Statistics in 2015
skewness <- sapply(moral15[, c("hhnetinc_pc", "loginc")], function(x) round(skewness(x), digits = 4))
kurtosis <- sapply(moral15[, c("hhnetinc_pc", "loginc")], function(x) round(kurtosis(x), digits = 4))
mean <- sapply(moral15[, c("hhnetinc_pc", "loginc")], function(x) round(mean(x), digits = 4))
median <- sapply(moral15[, c("hhnetinc_pc", "loginc")], function(x) round(median(x), digits = 4))
statistics <- rbind(mean, median, skewness, kurtosis)
statistics <- as.data.frame.matrix(statistics)
colnames(statistics) <- c("Household net income per capita", "Log household net income per capita")
write.csv(statistics, "Statistics of income variables 2015.csv")
#----------------------------------#