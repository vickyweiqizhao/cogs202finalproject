---
title: "COGS202_finalProject"
output:
      html_document:  
        keep_md: true
---



## Loading libraries
 We are mainly using GAMM4 package to run general additive mixed model, an extension of linear regression.

```r
library(psych)
library(plyr)
library(dplyr);
```

```
## Warning: package 'dplyr' was built under R version 3.3.2
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lme4);
```

```
## Loading required package: Matrix
```

```r
library(gamm4);
```

```
## Warning: package 'gamm4' was built under R version 3.3.2
```

```
## Loading required package: mgcv
```

```
## Warning: package 'mgcv' was built under R version 3.3.2
```

```
## Loading required package: nlme
```

```
## 
## Attaching package: 'nlme'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmList
```

```
## The following object is masked from 'package:dplyr':
## 
##     collapse
```

```
## This is mgcv 1.8-22. For overview type 'help("mgcv-package")'.
```

```
## This is gamm4 0.2-5
```

```r
library(lmtest);
```

```
## Warning: package 'lmtest' was built under R version 3.3.2
```

```
## Loading required package: zoo
```

```
## Warning: package 'zoo' was built under R version 3.3.2
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library(nlme);
library(ggplot2);
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following objects are masked from 'package:psych':
## 
##     %+%, alpha
```

```r
library(gridExtra);
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
library(gtable);

data <- read.csv('PLING_trimmedData_workingdata.csv')

index = which(complete.cases(data$BEH_WISC_DS_Raw) & complete.cases(data$BEH_VMI_Raw))

raw_data <- data[index,]
```

## Variables of interest

#### Demographic variables
SubjectID: `raw_data$SubjectID`
Age: `raw_data$Age`
Gender: `raw_data$Gender`
Scanner: `raw_data$DeviceSerialNumber.x`

#### Behavioral variables
WISC: `raw_data$BEH_WISC_DS_DSB_Raw`
VMI: `raw_data$BEH_VMI_Raw`

#### DTI variables

Corticospinal tract (CST): DTI_fiber_FA.L_CST, DTI_fiber_FA.R_CST

Anterior thalamic radiations (ATR): DTI_fiber_FA.R_ATR, DTI_fiber_FA.L_ATR 

Corpus callosum (CC): DTI_fiber_FA.CC

Superior longitudinal fasciculus: DTI_fiber_FA.R_SLF, DTI_fiber_FA.L_SLF

Inferior longitudinal fasciculus: DTI_fiber_FA.R_ILF, DTI_fiber_FA.L_ILF

Inferior frontal-occipital fasciculus (IFO): DTI_fiber_FA.R_IFO, DTI_fiber_FA.L_IFO 

Cingulum: CgC (cingulum): DTI_fiber_FA.R_CgC, DTI_fiber_FA.L_CgC, 

test code

```r
wisc_dem_fit <- gamm4(BEH_WISC_DS_Raw~ s(Age, k=4, fx=FALSE) + Gender, data= raw_data,  random = ~(1|SubjectID), na.action=na.omit)

summary(wisc_dem_fit$mer)
```

```
## Linear mixed model fit by REML ['lmerMod']
## 
## REML criterion at convergence: 1884.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.5029 -0.4707 -0.0200  0.5314  2.6532 
## 
## Random effects:
##  Groups    Name        Variance Std.Dev.
##  SubjectID (Intercept)  3.556   1.886   
##  Xr        s(Age)      99.597   9.980   
##  Residual               4.526   2.128   
## Number of obs: 397, groups:  SubjectID, 126; Xr, 2
## 
## Fixed effects:
##              Estimate Std. Error t value
## X(Intercept)  13.4024     0.2877   46.59
## XGenderM      -0.3275     0.4043   -0.81
## Xs(Age)Fx1     2.4755     0.4940    5.01
## 
## Correlation of Fixed Effects:
##            X(Int) XGndrM
## XGenderM   -0.713       
## Xs(Age)Fx1 -0.014  0.023
```

```r
# adjusted r square
r_sq = summary(wisc_dem_fit$gam)["r.sq"]
plot(wisc_dem_fit$gam)
```

![](COGS202_FinalProjectMarkdown_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# AIC
AIC(wisc_dem_fit$mer)
```

```
## [1] 1896.869
```


## Demographic model

Generating training and testing dataset

```r
# training dataset: 80% of the data
smp_size <- floor(0.80 * nrow(raw_data))

MSE_train = rep(NA, 1000)
MSE_test =  rep(NA, 1000)

for (i in 1:1000){
	
set.seed(i)

train_ind <- sample(seq_len(nrow(raw_data)), size = smp_size)

train <- raw_data[train_ind, ]
test <- raw_data[-train_ind, ]

# fitting demographic variables GAMM4 function
wisc_dem_fit_train <- gamm4(BEH_WISC_DS_Raw~ s(Age, k=4, fx=FALSE) + Gender, data= train,  random = ~(1|SubjectID), na.action=na.omit)

#summary(wisc_dem_fit_train$gam)

MSE_train[i] <- mean((train$BEH_WISC_DS_Raw - fitted(wisc_dem_fit_train$gam))^2)

# using the estimated parameter estimates to predict the test data
pred_test <- predict(wisc_dem_fit_train$gam, test, na.action=na.exclude)

MSE_test[i] <- mean((test$BEH_WISC_DS_Raw - pred_test)^2)

}

Mean_MSE_train = mean(MSE_train)
Mean_MSE_test = mean(MSE_test)

Mean_MSE_train
```

```
## [1] 7.936334
```

```r
Mean_MSE_test
```

```
## [1] 8.295481
```

## DTI model






