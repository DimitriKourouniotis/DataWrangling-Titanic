# Titanic


install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

# load titanic_orignal.csv into data frame
# with readr
titanic_dataset <- read_csv("titanic_original.csv")
View(titanic_dataset)

# SOUTHAMPTON
# Find and replace missing "embarked" values with "S" for Southampton
# with dplyr 
titanic_dataset <- titanic_dataset %>%
  mutate(embarked = ifelse(is.na(embarked),"S",embarked))

# AGE
# Determine missing value for age (based on title) -a more detailed approach would differentiate with Master/Miss (youths) and all else (others) 
# 1. Create Column title
titanic_dataset$title <- sapply(titanic_dataset$name, FUN = function(x) {strsplit(x, split='[,.]')[[1]][2]})
titanic_dataset$title <- sub(' ','',titanic_dataset$title)

# get age of everyone averaged by title
 aggregate(titanic_dataset$age, list(titanic_dataset$title), mean, na.rm= TRUE )
 titanic_dataset %>% group_by(title) %>% summarise(mean= mean(age, na.rm=TRUE))
 

#Replace missing values of age with global average -  
titanic_dataset$age[which(is.na(titanic_dataset$age))] <- mean(titanic_dataset$age, na.rm=TRUE)

#Replace missing values of life boat with "none"
titanic_dataset$boat[which(is.na(titanic_dataset$boat))] <- "None"


#  mutate(col = replace(col,which(is.na(col)),0))
titanic_dataset <- titanic_dataset %>%
  mutate(has_cabin_number = ifelse(is.na(cabin),0,1))

write.csv(titanic_dataset, file = "titanic_clean.csv")
