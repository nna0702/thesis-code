# README: This script merges raw data files into a master and removes duplicates

# Settings

remove(list = ls())
PathBase <- "C:/Users/tuananhle/Documents/NA/Thesis"
PathRaw <- paste(PathBase, "/Raw Data", sep = "")
PathProcessed <- paste(PathBase, "/Processed Data", sep = "")
PathOut <- paste(PathBase, "/Output", sep = "")
PathCode <- paste(PathBase, "/code", sep = "")
setwd(PathRaw)
#----------------------------------#

# Install packages

if (!require(haven)) {
  install.packages("haven")
  library(haven)
}
#----------------------------------#

# Read the datasets

filenames <- list.files(pattern = ".sas7bdat")
filenames <- setdiff(filenames, c("mast_pub_12.sas7bdat", "hhinc_10.sas7bdat"))
datalist <-  lapply(filenames, function(filename) {
  read_sas(filename)
})
#----------------------------------#

# Convert names of variables into lower case letters

for (i in 1:length(datalist)) {
  names(datalist[[i]]) <- tolower(names(datalist[[i]]))
}
#----------------------------------#

# Only use observations in 2011 and 2015

for (i in 1:length(datalist)) {
  datalist[[i]] <- datalist[[i]][datalist[[i]]$wave == 2011 | datalist[[i]]$wave == 2015, ]
}
#----------------------------------#

# Merge datasets

data <- Reduce(function(x, y) {
  merge(x, y, all = TRUE)
}, datalist)

#----------------------------------#

# Merge with master individual ID dataset

master <- read_sas("mast_pub_12.sas7bdat")
names(master) <- tolower(names(master))
data <- merge(master, data)
#----------------------------------#

# Merge with household-level dataset

household <- read_sas("hhinc_10.sas7bdat")
names(household) <- tolower(names(household))
household <- subset(household, household$wave == 2011 | household$wave == 2015)
data <- merge(data, household, all.x = TRUE, all.y = FALSE)
#----------------------------------#

# Remove duplicates 

data11 <- data[data$wave == 2011, ]            ## Extract the data in wave 2011
data15 <- data[data$wave == 2015, ]            ## Extract the data in wave 2015

## Wave 2011
data11 <- data11[(!duplicated(data11$idind)),] ## Remove duplicates in wave 2011
data <- rbind(data11, data15)                  ## Combine 2011 and 2015 datasets with unique values

## Wave 2015
data15 <- data15[(!duplicated(data15$idind)),] ## Remove duplicates in wave 2015
data <- rbind(data11, data15)                  ## Combine 2011 and 2015 datasets with unique values

## Double check uniqueness in each wave
stopifnot(length(unique(data[data$wave == 2011, ]$idind)) == NROW(data[data$wave == 2011, ]))
stopifnot(length(unique(data[data$wave == 2015, ]$idind)) == NROW(data[data$wave == 2015, ])) 
#----------------------------------#

# Retain observations which appear in both 2011 and 2015
#data <- data[duplicated(data$idind) | duplicated(data$idind, fromLast = TRUE),]
#stopifnot(NROW(data[data$wave == 2011, ]) == NROW(data[data$wave == 2015, ]))  # Cross check
#----------------------------------#

# Save the dataset 

setwd(PathProcessed)
write.csv(data, "data.csv")
#----------------------------------#
