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
                                          var = var(Cholesterol),
                                          n = n())

### Result
# A tibble: 2 x 4
# CHD_Event    mu    sd     n
#   1         0  200.  35.9   460
#   2         1  213.  33.3    40
sqrt(1107)
Sp2 = ((39*(33.3^2)) + (459*(35.9^2)) ) / 498
Sp2
Sp = sqrt(Sp2); Sp
# [1] 35.70322

tc = 13  / ( Sp * sqrt( (1/40) + (1/460)  ) ); tc
#[1] 2.208818

### Validating results with t.test function
x = df %>% 
  filter(CHD_Event == 1)  %>% 
  select(Cholesterol)
y = df %>% 
  filter(CHD_Event == 0)  %>% 
  select(Cholesterol)
t.test(x, y , var.equal = TRUE )



