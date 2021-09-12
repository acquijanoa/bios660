##---------------------------------------------
##  Project: BIOS 662 Homework 3 code
##
##  Author: Alvaro C. Quijano
##
##  Date: September 13, 2021
##
##---------------------------------------------

### loading libraries
library(ggplot2)
library(dplyr)

## Reading data from folder
df = read.table('data/HW3_CHOL.txt')
names(df) = c('Gender', 'Age','Cholesterol', 'CHD_Event') ## Assigning column names
df %>%  head() ### Displaying first 5 records in the dataset

### Extracting the information about mean, sd and count by CHD_event variable
df %>%  group_by(CHD_Event) %>% summarise(mu = mean(Cholesterol), 
                                          sd = sd(Cholesterol), 
                                          n = n())
