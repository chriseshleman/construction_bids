
# Construction bids 
  # Consider: build a model, withholding the last two quarters (?) of observations, that predicts bids. 
  # And test the model on the withheld observations. How close were they? 
  # Finally, predict a batch of new bids before they actually come in. 
# Methods 
  # Base (manually constructed linear model) 
  # Random forest 
  # Lasso / Ridge or K nearest neighbor (take your pick) 

rm(list = ls()) # clear global environment 
cat("\014") # clear the console 

library(olsrr) 
library(data.table) 
library(ggplot2) 
library(lubridate) 
library(doBy) 
library(DataCombine) 
library(dplyr) 
library(compare) 
library(StatMeasures) 
library(randomForest)
library(caret) 


#setwd("~/OneDrive - The Port Authority of New York & New Jersey/MAIN WORK ESHLEMANC/Analysis - general/Construction") #home
setwd("C:/Users/ceshleman/OneDrive - The Port Authority of New York & New Jersey/MAIN WORK ESHLEMANC/Analysis - general/Construction") #work
bids = read.csv("./Bid data 2019q2.csv") 

bids$Year = as.factor(bids$Year) 
bids$Date = as.Date(bids$Date,"%m/%d/%Y") 

bids$Quarter = quarter(bids$Date) 
bids$Quarter = paste("Q",quarter(bids$Date), sep="") 
bids$Q = paste(bids$Year,bids$Quarter,sep="-")

econ = read.csv("./Economics 2019Q2.csv") 
econ$Q = paste(econ$Year,econ$Quarter,sep="-")

econ$Year = NULL  
econ$Quarter = NULL 
bids$Year = NULL 
bids$Quarter = NULL 
  
bids = merge(bids,econ, by = "Q", all.x=TRUE) 

rm(econ) 
getwd() 
## Add permits 
#permits = read.csv("./DOB_Permit_Issuance_New.csv") # Downloaded on 7/19 
#permit = permits

permits$date = as.Date(strptime(permits$Job.Start.Date, '%m/%d/%Y %H:%M:%S')) 
permits = subset(permits,permits$date<="2019-07-17") # Weed out the typos 
summary(permits$date) # Includes permits through two days ago. 

permits$Q = quarter(permits$date) 
permits$Q = paste("Q",permits$Q,sep="") 
permits$Q = paste(year(permits$date),permits$Q,sep="-") 
permits$count = 1 
permits_short = summaryBy(count ~ Q, FUN=sum, data=permits) # aggregate by quarter 
names(permits_short) = c("Q", "permits") 

permits_short = slide(permits_short, Var = "permits", slideBy = -1) 
names(permits_short) = c("Q","permits","permits_1") 
bids = merge(bids, permits_short, by = "Q", all.x=TRUE) 
rm(permits_short) 

#bid = bids 
bids = bid 

bids$Employment.in.construction = as.numeric(as.character(bids$Employment.in.construction)) 
bids$Output.in.construction = as.numeric(as.character(bids$Output.in.construction)) 
bids$Total.population = as.numeric(as.character(bids$Total.population)) 
bids$Total.employment = as.numeric(as.character(bids$Total.employment)) 
bids$Total.output = as.numeric(as.character(bids$Total.output)) 

bids$Format = as.factor(ifelse(bids$Format=="Public","Public","Other")) 



## Now the data is prepped. 
# So split it into train  / test sets. 
# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
smp_size = floor(0.8 * nrow(bids)) # 80% of the sample size 
set.seed(123) # set the seed to make partition reproducible, though I still don't know what this gets me 
train_ind = sample(seq_len(nrow(bids)), size = smp_size) 
train = bids[train_ind, ] 
test = bids[-train_ind, ]
#setdiff(train,test) # test for duplicates 


## Model and predict 
# Key stats for each model 
# A - Difference between Engr.Est and Low.Bid (with the average) 
# B - Absolute value of above (with the average) 
# C - A as percentage (with weighted average) 
# D - B as percentage (with weighted average) 
# Then compare teh outputs including the adjusted R-squared and the information criteria 


## A. Base model (manual selection) https://medium.com/@davidsb/datascience-for-developers-build-your-first-predictive-model-with-r-a798f684752f 
base = lm(Low.Bid ~ Engr.Est + Format + Loc + Typeology + Employment.in.construction + permits_1, data = train) 
  summary(base) 
  AIC(base) 

# Predict 
prediction = predict(base, test) 
cor(prediction, test$Low.Bid)
# So what's more accurate, the Engineering Estimate or the Prediction? 
# What I'm ultimately trying to understand is whether we can add value to the Engineering Estimate in a way that helps the agency better predict low bids. 
mean(preds_base$prediction-preds_base$Low.Bid) 


## B. Random forest https://www.r-bloggers.com/predicting-wine-quality-using-random-forests/ 
names(train) 
train2 = train[,-c(1,2,3,4,8:10,14)]
rf = randomForest(Low.Bid ~ ., data = train2) 
rf 
summary(rf) 
print(rf)
round(importance(rf), 2) 

prediction_rf = predict(rf, newdata = test)
cor(prediction_rf, test$Low.Bid)


## C. K nearest neighbors or lasso / ridge https://codeburst.io/implementing-a-simple-prediction-model-in-r-ab1dafa6b954 
# Round 1 
knn = train(Low.Bid ~ ., data=train2, method = "knn") 
knn 
summary(knn) 
print(knn) 

prediction_knn = predict(knn, test) 
cor(prediction_knn, test$Low.Bid) 
confusionMatrix(prediction_knn, test$Low.Bid)$overall['Accuracy'] 

prediction = as.data.frame(prediction); names(prediction) = c("prediction_lm")
prediction_rf = as.data.frame(prediction_rf); names(prediction_rf) = c("prediction_rf")
prediction_knn = as.data.frame(prediction_knn); names(prediction_knn) = c("prediction_knn")
preds_base = cbind(prediction,prediction_rf,prediction_knn,test) 
preds_base = preds_base[order(preds_base$Date),]# Order by date before exporting 
write.csv(preds_base,"./Predictions 20190719.csv")

# 
# 


########### HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 

preds_base$market_error = preds_base$Engr.Est-preds_base$Low.Bid
preds_base$prediction_error = preds_base$prediction-preds_base$Low.Bid
preds_base$market_error_pct = preds_base$market_error/preds_base$Low.Bid
preds_base$prediction_error_pct = preds_base$prediction_error/preds_base$Low.Bid
preds_base$market_error_abs = abs(preds_base$market_error) 
preds_base$prediction_error_abs = abs(preds_base$prediction_error) 

market_error = weighted.mean(preds$market_error, preds$Low.Bid, na.rm = FALSE) 
prediction_error = weighted.mean(preds$prediction_error, preds$Low.Bid, na.rm = FALSE) 
market_error_abs = weighted.mean(preds$market_error_abs, preds$Low.Bid, na.rm = FALSE) 
prediction_error_abs = weighted.mean(preds$prediction_error_abs, preds$Low.Bid, na.rm = FALSE) 


#
#




# a = as.table(market_error,prediction_error) 
market_error 
prediction_error 
market_error_abs 
prediction_error_abs 
# So do a weighted average for each (engineers off versus predicted) 
# of the error. 
# Do it twice, in dollars and in percentage points. 
# That's four data points. 
# Just print them and cut and paste them into the spreadsheet. 

preds$decile = decile(vector = preds$Low.Bid) 

#write.csv(preds,"./construction_bids/Bid data analysis 2019q2 20190718.csv") 


## Short to long with variables of interest 
# Calculate key stats 
# Average 
