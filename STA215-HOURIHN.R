## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Maria Hourihan

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")

##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################

#Quantitative Variable #1
table(data$Average_BPM_Number)
mean(data$Average_BPM_Number)
sd(data$Average_BPM_Number)
summary(data$Average_BPM_Number)

#Quantitative Variable #2
table(data$Number_of_Characters_Singing)
mean(data$Number_of_Characters_Singing)
sd(data$Number_of_Characters_Singing)
summary(data$Number_of_Characters_Singing)

#Qualitative Variable #1
table(data$Cut_Modified_or_Original)
mean(data$Cut_Modified_or_Original)
sd(data$Cut_Modified_or_Original)
summary(data$Cut_Modified_or_Original)

#Qualitative Variable #2
table(data$Themes_of_Love)
mean(data$Themes_of_Love)
sd(data$Themes_of_Love)
summary(data$Themes_of_Love)

#Qualitative Variable #3
table(data$Themes_of_War)
mean(data$Themes_of_War)
sd(data$Themes_of_War)
summary(data$Themes_of_War)


##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################

#EXAMINE Qualitative Varible #1
table(data$Themes_of_Love)

#EXAMINE Qualitative Varible #2
table(data$Themes_of_War)

#EXAMINE Qualitative Varible #3
table(data$Cut_Modified_or_Original)

#Descriptive Table
table(data$Themes_of_Love, data$Themes_of_War, data$Cut_Modified_or_Original)

#Contingency Table
contingency_table <-table(data$Themes_of_Love, data$Themes_of_War, data$Cut_Modified_or_Original) 

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################

chisq.test(table(data$Themes_of_Love, data$Themes_of_War))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################

#ANOVA Test
anova_adapted <- aov(Average_BPM_Number ~ Themes_of_Love, data = data)

#Summary of ANOVA
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################

#Plotting
plot(data$Themes_of_Love, data$Average_BPM_Number) 
plot(data$Themes_of_War, data$Average_BPM_Number) 
plot(data$Themes_of_Love, data$Number_of_Characters_Singing) 
plot(data$Themes_of_War, data$Number_of_Characters_Singing) 
plot(data$Number_of_Characters_Singing, data$Average_BPM_Number)

#Correlation

cor(data$Number_of_Characters_Singing, data$Average_BPM_Number)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################

linear_relationship <- lm(Average_BPM_Number ~ Number_of_Characters_Singing, data = data)
summary(linear_relationship)


##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################

plot(data$Number_of_Characters_Singing, data$Average_BPM_Number)

abline(linear_relationship, col = "red")

mean(data$Number_of_Characters_Singing)
mean(data$Average_BPM_Number)

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################

plot(data$Number_of_Characters_Singing, residuals(linear_relationship))

abline(h = 0, col = "red")
