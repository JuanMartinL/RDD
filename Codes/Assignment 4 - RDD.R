#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assigment 4
# Causal Inference and Research Design
# Juan Martín Londoño Zuluaga
# juanmar.londono@gmail.com
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#------------------------------------
# Setting directory
#------------------------------------

rm(list=ls())
setwd("D:/Desktop/Universidad/15° Semestre/Causal Inference and Research Design/Assigments/Assignment 4/RDD")  # Setting the directory

#------------------------------------
# Packages
#------------------------------------

library(stargazer);library(readxl);library(tidyverse);library(ggplot2)
library(rddensity) # McCray test
library(rdd)
library(estimatr) # Robust LM
library(stats)
library(rdrobust)

#------------------------------------
# Data Base
#------------------------------------

BDD <- read.csv("Data/Input/hansen_dwi.csv", 
                sep = ",", stringsAsFactors = F)


#------------------------------------
# 3. Create a dummy equaling 1 if bac1>= 0.08 and 0 otherwise (BAC: blood alcohol content)
#------------------------------------

BDD$dbac1 <- ifelse(BDD$bac1 >= 0.08, 1, 0)

#------------------------------------
# 4. Checking in Raw Data if manpulation takes place: whether pple can manipulate their BAC.
#------------------------------------

# Looking some descriptive statistics
summary(BDD$bac1)


# McCray Sorting Test
#-----------------------------------
DCdensity(BDD$bac1, cutpoint = 0.08, htest = T)
  # Rejected null hypothesis! Apparent sorting


# Local Polynomial Density Estimation
#-----------------------------------
density <- rddensity(BDD$bac1, c = 0.08)
density$test$p_jk
rdplotdensity(density, BDD$bac1)

## Placebo test
i = 1
for (placebo in seq(0.04, 0.1, 0.01)) {
  print(paste(placebo, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"), quote = F)
  print(rddensity(BDD$bac1, c = placebo, all = T)$test$p_jk)
  i = i + 1
}


# Looking at the histogram, searching for manipulating evidence
ggplot(data = BDD) +
  geom_histogram(aes(bac1), binwidth = 0.001) +
  geom_vline(xintercept = 0.08, linetype = "dashed", color = "red") +
  ggtitle("Histogram of BAC") +
  xlab("BAC") + 
  ylab("Frequency")



#------------------------------------
# 5. Recreate Table 2 but only white male, age and accident (acc) as dependent variables.
#------------------------------------

reg_w <- lm_robust(white ~ bac1 + dbac1 + dbac1*bac1, data = BDD[(BDD$bac1 >= 0.03 & BDD$bac1 <= 0.13),])
summary(reg_w)

reg_m <- lm_robust(male ~ bac1 + dbac1 + dbac1*bac1, data = BDD[(BDD$bac1 >= 0.03 & BDD$bac1 <= 0.13),])
summary(reg_m)

reg_a <- lm_robust(aged ~ bac1 + dbac1 + dbac1*bac1, data = BDD[(BDD$bac1 >= 0.03 & BDD$bac1 <= 0.13),])
summary(reg_a)



# Looking at the Kernels
smooth_dem0 <- BDD %>% 
  filter(dbac1 == 0) %>% 
  select(bac1, recidivism)
smooth_dem0 <- as_tibble(ksmooth(smooth_dem0$bac1, smooth_dem0$recidivism, 
                                 kernel = "box", bandwidth = 0.01))

smooth_dem1 <- BDD %>% 
  filter(dbac1 == 1) %>% 
  select(bac1, recidivism)
smooth_dem1 <- as_tibble(ksmooth(smooth_dem1$bac1, smooth_dem1$recidivism, 
                                 kernel = "box", bandwidth = 0.01))

# Plotting the kernels
ggplot() + 
  geom_smooth(aes(x, y), data = smooth_dem0) +
  geom_smooth(aes(x, y), data = smooth_dem1) +
  geom_vline(xintercept = 0.08) +
  xlab("BAC") +
  ylab("Recidivism")


# Local Polynomic Regression

# Plotting it
rdplot(y = BDD$recidivism,
       x = BDD$bac1, c = 0.08)
