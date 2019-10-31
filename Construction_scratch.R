#Scratch paper 


#install.packages("ISLR") 
library(ISLR) 
library(ggplot2) 
library(glmnet) 
library(gtools) 
library(dplyr) 
library(tidyverse) 

hitt = Hitters #backup 
head(Hitters) 
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters) 
head(Hitters) 

########
# Faceting options ggplot 
table(Hitters$Years) 
hitters = subset(Hitters,Hitters$Years<11) 
names(hitters) 
hit.summary = aggregate(Errors ~ Years, mean, data=na.omit(hitters)) 
ggplot(hitters, aes(x = as.factor(Years), y = Errors, scolor = Division)) + 
  geom_hline(colour="dark gray", yintercept=1) +
  geom_jitter(width=0.2) +
  geom_crossbar(data=hit.summary, aes(ymin = Errors, ymax = Errors),
                size=1,col="red", width = .5) + 
  ylab("Second Bid ÷ Estimate") + 
  theme(axis.title.y=element_text(size=10)) 



########
# Ridge Lasso and Elastic Net 
x = model.matrix(Salary~.,Hitters) [,-1] # The -1 removes the intercept 
y = Hitters$Salary # why not model.matrix? why does it give the "atomic vectors" error? 
#10^seq(10,02,length=100) 
#ridge.mod = glmnet(x,y,alpha=0, lambda=(10^seq(10,02,length=10^seq(10,02,length=100))))
# Each value of lambda now has its own ridge regression coefficients, stored in a matrix. 
# coef(ridge.mod) calls all the coefficients associated with each lambda!
#set.seed(1234) 
train = sample(1:nrow(x),nrow(x)/2) 
test = (-train) 
y.test=y[test] 
str(y[train]) 
summary(x) 
summary(x[train]) 
summary(x[test]) 
length(y) 
y 
str(x) 
str(y) 
str(train) 
str(test) 
str(y.test) 
# Run an elastic net
ridge.mod=glmnet(x[train,],y[train],alpha=0.5,lambda=(10^seq(10,02,length=100)))
ridge.pred=predict(ridge.mod,s=400000,newx=x[test,])
mean((ridge.pred-y.test)^2) #Model error
# Is this better than basic OLS? 
ridge.pred2=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred2-y.test)^2) #OLS error
#Cross-validation 
cv.out = cv.glmnet(x[train,], y[train],alpha=0.5) 
plot(cv.out) 
bestlam = cv.out$lambda.min
bestlam # The lambda value that results in smallest cv error. 
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,]) 
mean((ridge.pred-y.test)^2) # check MSE - the error associated with that lambda. 
# OK, regress using lambda chosen above USING FULL DATA SET 
out=glmnet(x,y,alpha=0.5) 
ridge.predict.test = predict(out,type="coefficients",s=bestlam) 
ridge.predict.test 


#######
# Repeat with bids data 
x2 = model.matrix(Second.Bid~.,train2) [,-1] # The -1 removes the intercept 
y2 = train2$Second.Bid # why not model.matrix? why does it give the "atomic vectors" error? 
train2 = sample(1:nrow(x2),nrow(x2)/2) 
test2 = (-train2) 
y.test2=y2[test2] 
str(x2)
str(y2) 
str(train2)
str(test2) 
str(y.test2) 
#

# Run an elastic net
ridge.mod=glmnet(x2[train2,],y2[train2],alpha=.85,lambda=(10^seq(10,02,length=100)))
ridge.pred=predict(ridge.mod,s=1000000,newx=x2[test2,])
options(scipen=999) 
mean((ridge.pred-y.test2)^2) #Model error
# Try with OLS
ridge.pred.ols=predict(ridge.mod,s=0,newx=x2[test2,]) 
mean((ridge.pred.ols-y.test2)^2) #OLS error 
# Now pick a lambda and compare results to OLS above. 
# Use cross-validation to select. 
cv.out = cv.glmnet(x2[train2,], y2[train2],alpha=.8) 
plot(cv.out) 
bestlam = cv.out$lambda.min
bestlam # The lambda value that results in smallest cv error. 
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x2[test2,]) 
mean((ridge.pred-y.test2)^2) # check MSE - the error associated with that lambda. 
(mean((ridge.pred-y.test2)^2)) / mean((ridge.pred.ols-y.test2)^2) 
# Yes, it beats OLS. 
# Now regress using the lambda chosen above on the full data set. 
out=glmnet(x2,y2,alpha=.8) 
ridge.predict.test = predict(out,type="coefficients",s=bestlam) 
ridge.predict.test 



#######
# Toy example 
data(cars) 
head(cars) 
head(cars[,-1]) 
head(cars[,1]) 
fit.glmnet.lasso.cv <- cv.glmnet(as.matrix(cars[, -1]),
                                 as.matrix(cars[, 1]),
                                 nfold = 5,
                                 alpha = 1)

#
#######
## Generate dummies 
outs = Hitters[c("PutOuts", "CRBI")]; names(outs) = c("outs","RBI") 
outs = outs %>%
  mutate(quantile = ntile(as.numeric(outs), 10))
outs = fastDummies::dummy_cols(outs, select_columns = "quantile")

smp_siz = floor(0.8*nrow(outs))
trans = sample(seq_len(nrow(outs)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
tran = outs[trans,] #creates the training dataset with row numbers stored in train_ind
tst = outs[-trans,] # creates the test dataset excluding the row numbers mentioned in train_ind

xx=data.matrix(tran[!outs]) #XP=data.matrix(train2[,-3]) 
yx=data.matrix(tran$outs) 

