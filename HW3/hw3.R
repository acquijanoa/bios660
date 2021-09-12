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
df = read.table('HW3/data/HW3_CHOL.txt')
names(df) = c('Gender', 'Age','Cholesterol', 'CHD_Event') ## Assigning column names
df %>%  head() ### Displaying first 5 records in the dataset

### Extracting the information about mean, sd and count by CHD_event variable
df %>%  group_by(CHD_Event) %>% summarise(mu = mean(Cholesterol), 
                                          sd = sd(Cholesterol), 
                                          n = n())

### Result
# A tibble: 2 x 4
# CHD_Event    mu    sd     n
#   1         0  200.  35.9   460
#   2         1  213.  33.3    40

