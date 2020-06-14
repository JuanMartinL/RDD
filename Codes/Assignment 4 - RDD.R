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
  print(paste(i, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"), quote = F)
  print(rddensity(BDD$bac1, c = placebo, all = T)$test$p_jk)
  i = i + 1
}


# Looking at the histogram, searching for manipulating evidence
ggplot(data = BDD[(BDD$bac1 > 0.06 & BDD$bac1 < 0.09),]) +
  geom_histogram(aes(bac1), binwidth = 0.001) +
  geom_vline(xintercept = 0.08, linetype = "dashed", color = "red")



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


#------------------------------------
# 6. Recreate Figure 2 panel A-D. 
#------------------------------------

# Aggregating the data
categories <- BDD$bac1

demmeans <- split(BDD$recidivism, cut(BDD$bac1, length(BDD$bac1))) %>% 
  lapply(mean) %>% 
  unlist()

agg_BDD <- data.frame(recidivism = demmeans, bac1 = BDD$bac1)

# Plotting
BDD <- BDD %>% 
  mutate(gg_group = case_when(bac1 > 0.08 ~ 1, TRUE ~ 0))

ggplot(BDD, aes(bac1, recidivism)) +
  geom_point(aes(x = bac1, y = recidivism), data = agg_BDD) +
  stat_smooth(aes(bac1, recidivism, group = gg_group), method = "lm") +
  geom_vline(xintercept = 0.08) +
  xlim(0,0.2)


# Male
#----------------------------

# Aggregating the data
categories <- BDD$bac1

demmeans <- split(BDD$male, cut(BDD$bac1, length(BDD$bac1))) %>% 
  lapply(mean) %>% 
  unlist()

agg_BDD <- data.frame(male = demmeans, bac1 = BDD$bac1)

# Plotting
BDD <- BDD %>% 
  mutate(gg_group = case_when(bac1 > 0.08 ~ 1, TRUE ~ 0))

ggplot(BDD, aes(bac1, male)) +
  geom_point(aes(x = bac1, y = male), data = agg_BDD) +
  stat_smooth(aes(bac1, male, group = gg_group), method = "lm") +
  geom_vline(xintercept = 0.08) +
  xlim(0,0.2)

rdplot(y = BDD$male, x = BDD$bac1, c = 0.08)

reg_p <- RDestimate(male ~ bac1, data = BDD, cutpoint = 0.08, bw = c(0.03, 0.13))
plot(reg_p)
