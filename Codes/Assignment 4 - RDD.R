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

library(foreign)
library(stargazer);library(readxl)

#------------------------------------
# Data Base
#------------------------------------

BDD <- read.csv("Data/Input/hansen_dwi.csv", 
                sep = ",", stringsAsFactors = F)


#------------------------------------
# 3. 
#------------------------------------