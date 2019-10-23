#Scratch paper 


#install.packages("ISLR") 
library(ISLR) 
library(glmnet) 
library(gtools) 
library(dplyr) 
library(tidyverse) 

hitt = Hitters #backup 
head(Hitters) 
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters) 

#


outs = as.data.frame(Hitters$PutOuts); names(outs) = c("outs") 
#outs$decile = decile(vector = outs$outs) 
#outs$decile = ordered(outs$decile, levels = 1:10)
#dummies = predict(dummyVars(~ decile, data = outs), newdata = outs) 

#outs$deciles2 = quantcut(outs$outs, 10 )

outs = outs %>%
  mutate(quantile = ntile(as.numeric(outs), 10))
outs2 = fastDummies::dummy_cols(outs, select_columns = "quantile")
head(outs2) 
