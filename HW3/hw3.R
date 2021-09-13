##---------------------------------------------
##  Project: BIOS 662 Homework 3 - R Code
##
##  Author: Alvaro C. Quijano-Angarita
##
##  PID: 730500507
##
##  Date: September 13, 2021
##
##---------------------------------------------

### loading libraries
library(ggplot2)
library(dplyr)
library(stargazer)

## Reading data from folder
df = read.table('HW3/data/HW3_CHOL.txt')
names(df) = c('Gender', 'Age','Cholesterol', 'CHD_Event') ## Assigning column names
df %>%  head() ### Displaying first 5 records in the dataset

### Extracting the information about mean, sd and count by CHD_event variable
options(pillar.sigfig=5)
df %>%  group_by(CHD_Event) %>% summarise(mu = round(mean(Cholesterol),2 ), 
                                          sd = sd(Cholesterol), 
                                          var = var(Cholesterol),
                                          n = n())
### Result
# A tibble: 2 x 4
# CHD_Event    mu    sd   var     n
# <int> <dbl> <dbl> <dbl> <int>
#   1         0 199.85 35.87 1287.   460
#   2         1 213.02 33.28 1107.    40
sqrt(1107)
Sp2 = ((39*(33.3^2)) + (459*(35.9^2)) ) / 498
Sp2
Sp = sqrt(Sp2); Sp
# [1] 35.70322

tc = (213.02 - 199.85) / ( Sp * sqrt( (1/40) + (1/460)  ) ); tc
# [1] 2.237702

2* (1 - pt(2.2404, 498))
### Validating results with t.test function
x = df %>% 
  filter(CHD_Event == 1)  %>% 
  select(Cholesterol)
y = df %>% 
  filter(CHD_Event == 0)  %>% 
  select(Cholesterol)

t.test(x, y , var.equal = TRUE )

## T-student quantiles

qt(0.975, 498) # 1.964739
qt(0.025, 498) # -1.964739



#### b) Variances test 

Fc = var(y$Cholesterol)/  var(x$Cholesterol); Fc
# [1] 1.162018

Ft = qf(0.975, df1 = 459, 39); Ft
# [1] 1.672317

Ft2 = qf(0.025, df1 = 459, 39); Ft2
# [1] 0.6559809

### c) Bootstrap method

db = df %>%  
  filter(Gender == 'M' & 
           CHD_Event == 1 ) %>% 
  .$Cholesterol
nb = length(db); nb
meanb = mean(db); meanb
varb  = var(db); varb
Zb = vector()

## Boostrap method
### 10.000 simulations
for(i in 1:10000){
  sampleb = sample(db, 
                   size = nb, 
                   replace = T)
  Zb[i] = (mean(sampleb) - meanb ) / sqrt( var(sampleb) / nb )
}

### Plotting the sampling distirbution for z*(i) values
plot(density(Zb), 
     main = "Bootstrap sampling distribution for Z*(B) values")

### Computing bootstrap quantiles

upper.t = quantile(Zb, 0.975); upper.t
# quantile 97.5% 
# 2.016324 

lower.t = quantile(Zb, 0.025); lower.t
# quantile 2.5% 
# -2.116028 

#### Approximate 95% interval
lower.y = meanb + lower.t*sqrt(varb/nb); lower.y
upper.y = meanb + upper.t*sqrt(varb/nb); upper.y

print(paste0("(", 
             round(lower.y,4) , 
             ", ", 
             round(upper.y,4), ")" ))

### Median confidence interval

dMo = df %>%  
  filter(Gender == 'F' & 
           CHD_Event == 1 ) %>% 
  .$Cholesterol

## Median
Mo = median(dMo); Mo
# [1] 204.5

nMo = length(dMo)

table = cbind(0:14, 
      pbinom(0:14, size = nMo, prob = 0.5), 
      ifelse( pbinom(0:14, size = nMo, prob = 0.5) < 0.025,1, 0 ) ) ; table
stargazer(table)

### Confidence interval
paste(sort(dMo)[2+1],  sort(dMo)[nMo - 2])

### Wilcoxon test


x = df %>%  filter(Gender == 'M' &  CHD_Event == 0) %>%  .$Cholesterol 
y = df %>%  filter(Gender == 'M' &  CHD_Event == 1) %>%  .$Cholesterol 
wilcox.test(x, y = NULL,
            alternative = c("two.sided", "less", "greater"))


ks.test(x, y)
