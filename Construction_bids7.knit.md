---
title: "Predicting costs"
author: "Chris Eshleman"
date: "10/20/2019"
output:
  pdf_document:
    latex_engine: xelatex
  html_document:
    df_print: paged
  word_document: default
---

\pagenumbering{arabic}

Q: What's the cross-validation being used? 
Q: Can I relax the penalty I'm applying via lasso? YES - THERE'S 
SOMETHING TO THIS - Flesh it out! 

The agency estimates project costs internally. It may be able to use statistics to improve those predictions. 

Model variation in bids, withholding the two most recent quarters of observations. Then test those models on the withheld observations. How well do they help with predictions? 

#Methods and Data.
  1. Basic (manually constructed linear model), 
  2. Lasso (penalized regression - machine learning). 
  
The project-level data comes from internal agency cost estimation.  
The economic data is just quarterly stuff from the usual suspects and is specific to national and regional economic and labor market conditions. 

Load some programming tools that are commonly used for this analysis. Not all of these packages will be used, and at some point it'll be worth backing up and cleaning the list. 



#Overview. 
This effort bridges exploration of the agency's data with potential predictors from exogenous sources. 
The key idea is that if economic indicators (numbers) can add measurable value to the agency's cost estimation methods, it can help set the agency up for better informed next steps. Those next steps are yet to be defined. 


```r
#![Here's what we have in mind.](Estimator_Data.png)
```

The measures of "accuracy" are, for now, bivariate correlation. We discuss below why it's not yet time for fancier metrics, but the process above represents a first cut at trying to add a little more statistical value to the process. 

Data comes from a few sources and needs to be merged. Two usual suspects are the agency's internal project information and a set of economic indicators specific to Greater New York (18 counties on both sides of the Hudson River). 



Add economic data. 



## Add permits and steel prices.
The City of New York's database on permitting covers comercial and residential activity. 

I'd like to ask have robust data on the rest of the region, including 
(and namely) Jersey City, but it's weaker than the City's, which by itself is 
a decent barometer of construction activity in greater New York. 

Prices of construction materials and labor also figure into the agency's internal cost estimation, and I'll use steel prices for now. Future models might try and rope in other pricing data points, but the estimators are generally already taking prices into account when setting their numbers so this is likely of second- and third-order importance but the modeling selection algorithms may suggest using them. 



The Engineering News-Record tracks and aggregates construction cost data through an index. Use that to cover prices. 


```r
cci = read.csv("./cci.csv") 
names(cci) = tolower(names(cci)) 
cci$avg. = NULL 
cci = gather(cci, month, cci, jan:dec, factor_key=TRUE) 
cci$month = gsub("(^[[:alpha:]])", "\\U\\1", cci$month, perl=TRUE)
cci$month = as.Date(paste(cci$month,"01",cci$year, sep="-"), format="%b-%d-%Y") 
cci = cci[month(cci$month) == 2 | month(cci$month) == 5 | month(cci$month) == 8 | month(cci$month) == 11, ] 

cci$Quarter = paste("Q",quarter(cci$month), sep="") 
cci$Q = paste(year(cci$month),cci$Quarter,sep="-") 
cci$Quarter = NULL 
cci$month = NULL 
cci$year=NULL 

bids = merge(bids, cci, by = "Q", all.x=TRUE) 

rm(cci) 
```


#Munge 
Stats software treats different variables in different ways depending on individal formatting. It's worth taking a look at the data structure. 

I'll need to tell the software to reformat some of the variables, namely the economic indicators. 



One variable of interest is the bidding process. Institutional discussions and earlier modeling suggests the bidding process may influence the bids. Limits placed on the range of bidders, for example, could, on average and holding other things constant, increase the average (and lowest qualifying) bid - this is basic microeconomics. I'll simplify the bidding format variable by making it binary: "public" for projects without significant constraints and "other" for ones, such as projects closed to firms not deemed "small business enterprises," that aren't. 



There's room to also eventually include the names (anonymized is fine) of each project estimator to help modeling. Past work has suggested there isn't major causal variation between estimators — they generally do a pretty equivalent job in estimating bids. But having their names included nonetheless may prove to offer some control value. We can leave that to future modeling. 

#Restating objective. 

We want to understand whether and how we might help the agency estimate the actual cost of a project. That's invariably going to be represented by the low qualifying bid, and our starting point is the estimate coming from the Engineering Department. 

From here on we'll define "accuracy" as the ratio of dollars estimated over dollars bid. So a "1" would mean the engineering team nailed it, a "0.94" would mean they estimated 94 cents for every 1 dollar in the low bid, et cetera. 



If there are outliers in there - projects that, for an unexplainable reason, was way off, consider removing it. 



We may want to think of project size categorically. 

```r
bids$decile = decile(vector = bids$Low.Bid) 
bids$decile = ordered(bids$decile, levels = 1:10)
```

Back up data. 

```r
bid = bids # backup my data frame 
```


#Analysis
Now the data is prepped. 
So split it into the training  / test sets we talked about at the start. The bids start in 2015. 
  


# Motivation. 
Why do we think developing a conrolled multivariate (complicated) model will be worth it? Well, the average gap is $2.5 million, or 20%, off of our estimates. What's the raw (uncontrolled) bivariate relationship between engineering estimates and low bids? 

![](Construction_bids7_files/figure-latex/unnamed-chunk-14-1.pdf)<!-- --> 

```
## 
## Call:
## lm(formula = bids$Low.Bid ~ bids$Engr.Est)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -71323352   -523965   -139942    420586  54469074 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3.279e+05  5.162e+05   0.635    0.526    
## bids$Engr.Est 8.274e-01  1.079e-02  76.701   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7533000 on 237 degrees of freedom
## Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9611 
## F-statistic:  5883 on 1 and 237 DF,  p-value: < 2.2e-16
```

(The logarithm treatment is just to distribute it across the plot (one of the observations is an outlier).) 

The in-house engineers' guesses predict more than 98% of the variation in low bids. 

Some of the remaining variation can be explained with some guesswork. Calculate summary stats for key variables - this will help later: 


```r
loc.summary = aggregate(accuracy ~ Loc, mean, data=na.omit(bids)) 
type.summary = aggregate(accuracy ~ Typeology, mean, data=na.omit(bids)) 
dec.summary = aggregate(accuracy ~ decile, mean, data=na.omit(bids)) 
```

Location likely has some predictive power that engineers may not be able to capture or fully predict. Basically, projects that span the Hudson River wind up costing more, on average, than ones plunked squarely in either New York or New Jersey. What's the raw (uncontrolled) relationship between bid accuracy and location? 


```
## 
## Call:
## lm(formula = bids$accuracy ~ bids$Loc)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.86160 -0.23542 -0.05548  0.15351  2.55783 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.144748   0.062580  18.293   <2e-16 ***
## bids$LocNJ  -0.003834   0.071492  -0.054    0.957    
## bids$LocNY   0.016431   0.074665   0.220    0.826    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3755 on 236 degrees of freedom
## Multiple R-squared:  0.0006293,	Adjusted R-squared:  -0.00784 
## F-statistic: 0.07431 on 2 and 236 DF,  p-value: 0.9284
```

![](Construction_bids7_files/figure-latex/unnamed-chunk-16-1.pdf)<!-- --> 


The signal is stronger regarding the type of project, which has an identifiable (if yet uncontrolled) relationship with estimating accuracy. What's the raw (uncontrolled) relationship between estimation accuracy and the type of project? 


```
## 
## Call:
## lm(formula = bids$accuracy ~ as.factor(bids$Typeology))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.88863 -0.22069 -0.05558  0.14023  2.54722 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                      1.04997    0.04747  22.118   <2e-16 ***
## as.factor(bids$Typeology)Infra   0.12182    0.05761   2.115   0.0355 *  
## as.factor(bids$Typeology)Paving  0.16086    0.07113   2.262   0.0246 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3708 on 236 degrees of freedom
## Multiple R-squared:  0.02561,	Adjusted R-squared:  0.01735 
## F-statistic: 3.101 on 2 and 236 DF,  p-value: 0.04684
```

![](Construction_bids7_files/figure-latex/unnamed-chunk-17-1.pdf)<!-- --> 

We see signals that projects of medium size (in dollar terms) may be less evasive than much larger or smaller projects. What's the relationship between low bids and accuracy, when we start considering the size of low bids? 


```
## 
## Call:
## lm(formula = bids$accuracy ~ as.factor(bids$decile))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.73608 -0.23930 -0.02694  0.14444  2.62999 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               1.14810    0.02372  48.408  < 2e-16 ***
## as.factor(bids$decile).L -0.11517    0.07484  -1.539  0.12521    
## as.factor(bids$decile).Q  0.21620    0.07503   2.881  0.00434 ** 
## as.factor(bids$decile).C -0.03733    0.07486  -0.499  0.61851    
## as.factor(bids$decile)^4 -0.03753    0.07502  -0.500  0.61734    
## as.factor(bids$decile)^5  0.12608    0.07491   1.683  0.09373 .  
## as.factor(bids$decile)^6  0.07655    0.07500   1.021  0.30843    
## as.factor(bids$decile)^7 -0.03055    0.07501  -0.407  0.68420    
## as.factor(bids$decile)^8  0.04269    0.07495   0.570  0.56952    
## as.factor(bids$decile)^9 -0.13488    0.07537  -1.790  0.07482 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3666 on 229 degrees of freedom
## Multiple R-squared:  0.07545,	Adjusted R-squared:  0.03912 
## F-statistic: 2.077 on 9 and 229 DF,  p-value: 0.03252
```

![](Construction_bids7_files/figure-latex/unnamed-chunk-18-1.pdf)<!-- --> 

Facet by typeology: 



Visualizing and testing the data iteratively like this has offered some initial insight into what might be accounting for variation in accuracy. 


# Modeling and prediction

Try and use exogenous covariates to predict an alternative engineering estimate, without using the low bid information, that might be closer to the low bid. 
Call it "expected low bid" or something so we can remember what we're trying to get. 

##A. Base model (manual selection) 
Interpretation: specification was manual and intuitive. 

Note: ensure the "accuracy" variable we calculated earlier is dropped before modeling or I'll be introducing some dual (reverse) causality, which will confuse the models. 

Note 2: when a number appears in the output without context, it is likely an information criterion (and AIC), which may or may not provide value post-modeling. 

First remove accuracy. 

```r
train$accuracy=NULL 
test$accuracy=NULL 
```

Run model. 


```
## 
## Call:
## lm(formula = log(Low.Bid) ~ Engr.Est + Employment.in.construction + 
##     Format + Typeology + permits_1 + cci + decile, data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.56367 -0.10711 -0.00202  0.13250  0.47008 
## 
## Coefficients:
##                              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                 1.515e+01  5.745e-01  26.370  < 2e-16 ***
## Engr.Est                    6.433e-09  4.891e-10  13.153  < 2e-16 ***
## Employment.in.construction -2.872e-03  5.104e-03  -0.563  0.57422    
## FormatPublic                4.670e-02  3.317e-02   1.408  0.16074    
## TypeologyInfra              6.966e-03  3.803e-02   0.183  0.85486    
## TypeologyPaving             7.119e-02  4.592e-02   1.550  0.12265    
## permits_1                  -2.669e-06  3.130e-06  -0.853  0.39479    
## cci                         9.050e-05  1.879e-04   0.482  0.63063    
## decile.L                    3.892e+00  5.797e-02  67.142  < 2e-16 ***
## decile.Q                    5.400e-01  5.605e-02   9.634  < 2e-16 ***
## decile.C                    4.361e-01  5.192e-02   8.400 8.96e-15 ***
## decile^4                   -1.341e-01  4.886e-02  -2.745  0.00662 ** 
## decile^5                   -2.553e-02  4.779e-02  -0.534  0.59385    
## decile^6                   -1.002e-01  4.731e-02  -2.118  0.03545 *  
## decile^7                   -7.227e-02  4.808e-02  -1.503  0.13438    
## decile^8                   -2.164e-02  4.777e-02  -0.453  0.65104    
## decile^9                   -3.907e-03  4.789e-02  -0.082  0.93506    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2174 on 196 degrees of freedom
## Multiple R-squared:  0.9787,	Adjusted R-squared:  0.9769 
## F-statistic: 561.9 on 16 and 196 DF,  p-value: < 2.2e-16
```

The equation above throws extra information at the engineering estimate and tries to predict the low qualifying bid. If the result is noticably closer to the low qualifying bid than the original estiamte, you can use the delta as a post-estimation fudge factor to adjust your final estimate.

The numbers calculated above include a few metrics to use in comparing the three estimates against the objective data point at the low bid, which is what we're trying to predict. Ghose three estimates I'm talking about are: 
  1. The original, raw engineering cost estimate,
  2. The first alternative, where we built a model by hand to try and use a few more data points to enhance the original estimate, and; 
  3. The second alternative, a kitchen sink model that throws even more data points at the question. This followed an effort to use a penalized regression to identify the best covariates, but that penalization algorithm actually suggested there isn't much we can do to enhance the original estimate. (Note: this will prove prescient.)

I'll build a table near the very end of this script that summarize the metrics I'm using to understand how well these modeling efforts work. The metrics will be: 
  A. A basic t-test to understand whether there's even a statistically significant difference between the estimate I'm getting and the enhanced estimate I'm modeling with it, 
  B. A correlation between the two numbers, to try and understand the magnitude of that difference (if we can trust it really exists), 
  C. Two measures of the predictive modeling power of the models, an adjusted R-squared and the mean squared error (MSE). Both are common metrics of power. The first can be viewed discretely for each model but the second only provides a relative measure between models. 
  
What is the summary of the predicted values? 
How does it compare to the summary of low bids? 


```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   13.04   14.07   14.66   15.05   15.88   20.37
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##     97300   1298005   2542996  12507353   6956000 451841280
```

Looking at the two summaries above, what's more accurate, the original engineering estimate or the first enhanced prediction? Neither, really. In fact the estimate and prediction aren't even (statistically) significantly different. Maybe something more robust can come with a little creativity. 

(Note: the model should control to prevent negative values. To be done next time.)

It might have been worth trying with the log of prices, only because the statistical fit becomes multiplicative instead of linear, but that produced similar results. 


## B. Stepwise selection 

Do a backward stepwise selection only because early runs indicate there's little room to add much predictive power, so recognize that the default process backward will favor leaving variables in more than forward. 

COMING SOON. 



## C. Machine learning
Strip the bids data of unusable stuff, then set controls and run.
The package I'm using, glmnet, requires a little extra preparation.

The output below represents the algorithm's effort to look for variatbles that might be dependable in adding predictive power to the original engineering estimate. 


```
##  [1] "Q"                                            
##  [2] "Proj"                                         
##  [3] "Proj.Name"                                    
##  [4] "Date"                                         
##  [5] "Format"                                       
##  [6] "Engr.Est"                                     
##  [7] "Low.Bid"                                      
##  [8] "Var"                                          
##  [9] "Result"                                       
## [10] "Bids"                                         
## [11] "Loc"                                          
## [12] "Qtr"                                          
## [13] "LD"                                           
## [14] "Lead"                                         
## [15] "Typeology"                                    
## [16] "Consumer.price.index"                         
## [17] "Employment.in.communications"                 
## [18] "Employment.in.construction"                   
## [19] "Employment.in.education.and.health"           
## [20] "Employment.in.financial.and.business.services"
## [21] "Employment.in.financial.services"             
## [22] "Employment.in.government"                     
## [23] "Employment.in.other.services"                 
## [24] "Employment.in.production.industries"          
## [25] "Employment.in.professional.services"          
## [26] "Employment.in.real.estate"                    
## [27] "Employment.in.retail"                         
## [28] "Employment.in.transport.services"             
## [29] "Employment.in.wholesale"                      
## [30] "Output.in.communications"                     
## [31] "Output.in.construction"                       
## [32] "Output.in.financial.services"                 
## [33] "Output.in.government"                         
## [34] "Output.in.retail"                             
## [35] "Output.in.education.and.health"               
## [36] "Output.in.financial.and.business.services"    
## [37] "Output.in.other.services"                     
## [38] "Output.in.production.industries"              
## [39] "Output.in.professional.services"              
## [40] "Output.in.real.estate"                        
## [41] "Output.in.transport.services"                 
## [42] "Output.in.wholesale"                          
## [43] "Personal.disposable.income..nominal"          
## [44] "Personal.disposable.income..real"             
## [45] "Personal.income..nominal"                     
## [46] "Retail.sales..nominal"                        
## [47] "Retail.sales..real"                           
## [48] "Total.employment"                             
## [49] "Total.office.based.employment"                
## [50] "Total.output"                                 
## [51] "Total.population"                             
## [52] "permits"                                      
## [53] "permits_1"                                    
## [54] "cci"                                          
## [55] "decile"
```

![](Construction_bids7_files/figure-latex/unnamed-chunk-23-1.pdf)<!-- --> 

```
## 45 x 1 sparse Matrix of class "dgCMatrix"
##                                                                1
## (Intercept)                                   13.104229586130819
## Format                                         .                
## Engr.Est                                       0.000000008161281
## Loc                                            .                
## LD                                             .                
## Typeology                                      .                
## Consumer.price.index                           .                
## Employment.in.communications                   .                
## Employment.in.construction                     .                
## Employment.in.education.and.health             .                
## Employment.in.financial.and.business.services  .                
## Employment.in.financial.services               .                
## Employment.in.government                       .                
## Employment.in.other.services                   .                
## Employment.in.production.industries            .                
## Employment.in.professional.services            .                
## Employment.in.real.estate                      .                
## Employment.in.retail                           .                
## Employment.in.transport.services               .                
## Employment.in.wholesale                        .                
## Output.in.communications                       .                
## Output.in.construction                         .                
## Output.in.financial.services                   .                
## Output.in.government                           .                
## Output.in.retail                               .                
## Output.in.education.and.health                 .                
## Output.in.financial.and.business.services      .                
## Output.in.other.services                       .                
## Output.in.production.industries                .                
## Output.in.professional.services                .                
## Output.in.real.estate                          .                
## Output.in.transport.services                   .                
## Output.in.wholesale                            .                
## Personal.disposable.income..nominal            .                
## Personal.disposable.income..real               .                
## Personal.income..nominal                       .                
## Retail.sales..nominal                          .                
## Retail.sales..real                             .                
## Total.employment                               .                
## Total.office.based.employment                  .                
## Total.output                                   .                
## Total.population                               .                
## permits_1                                      .                
## cci                                            .                
## decile                                         0.336859651066883
```

ISL suggests "a large value of s corresponds to [lambda]=0 ... if s is sufficiently large, .... the ... estimates will be the same as the least squares estimates." 

The algorithm suggests using anything beyond the engineer's estimate itself to better predict the lowest qualifying good adds more uncertainty (in the form of noise that's tough to explain) than it adds value. (The "penalty" associated with adding variables is greater than the extra predictive power they bring.) The exception is project size, and that would provide a chance to look closer. 


```r
  options(scipen=999)
  estimate_c = lasso$fitted.values #predict(compare,train2) 
  mse_c = round(mse(test$Low.Bid,lasso$fitted.values),0)
  #accuracy_c = cor(test$Low.Bid,lasso$fitted.values) 
  #adjr_c = round(summary(lasso)$adj.r.squared,3)
  #ttest_c = t.test(lasso$fitted.values,test$Engr.Est)$p.value
```


#Accuracy 

Fascinating. 
I first tried calculating a metric that captures accuracy (the relationship between the estimate and the actual). We wanted to use a weighted average of the ratio between estimate and actuals. Something like this ... 

accuracy_a = weighted.mean((train$Engr.Est/train$Low.Bid),train$Low.Bid) 

... applied across all three prisms (uncontrolled and the two models). 

But that calculation just results in 1 (and exactly 1) for both models. 
So we tried something different, summing the estimates and (separately) the actuals, and then dividing those totals. But that also gave me 1 because the average estimate and the average actual are exactly the same, down to the dollar. 

How can we be estimating the value that well, on average? The model must be optimized with respect to the average dependent value. 

But this is a question for another time. For now I'll just need to lean on other metrics as indications of accuracy: 



Well, the correlations between estimates and actuals do show signals of tightening up slightly. But how robust is this, really? Are the numbers really different? And how would the two models perform on fresh data? 

##Compare new estimates to original estimates.

No statistically significance differences to be found in either case.

#Validate
##Run on the withheld data and check.

We've already got the models (both of them): "base" and "compare".



Results for the broader model: 




#Compare


This is a table comparing performance of: 
1. The raw estimates, 
2. The manually-developed model, and, 
3. The informed model 
along some key measures. The p-values (below 0.1 is strong) and correlation (closer to 1 is strong) can be considered discretely for the two models (and, for correlation, the raw estimate), as they reflect how strongly predicted bids and actual bids are related, and the adjusted R-squared (closest to 1 is best) and mean squared error (MSE; lower is best) data points offer technical insight into how well each model performs ... 

For the training exercise (data from 2015-Q1 2019): 


Results: 

```r
#t(results_train) 
```

For the test (data withheld, from Q2 and Q3 2019): 


# Discussion. 
The model we selected myself doesn't do much better than the engineer's estimate. And the model with automated variable selection via lasso doesn't really do any better than the one by hand. 

The big insight here seems to be the impact project size has on accuracy. 
