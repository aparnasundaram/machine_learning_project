## This is the code for the machine learning project.
## Created by Aparna Sundaram on 08/17/2019
## This program does basic EDA on the test data 
## to get a feel for the data and the variables. 

## Load libraries
library(tidyr)
library(ggplot2)
library(corrr)
library(DescTools)
library(dplyr)

## Read in the training dataset ##
prices = read.csv('test.csv', stringsAsFactors = F)
class(prices)
head(prices)
summary(prices)
colnames(prices)

sapply(prices, class)
str(prices)

## Subset the dataframe to only extract numeric columns to be able to check correlation
prices2 <- select_if(prices, is.numeric)
class(prices2)
colnames(prices2)
round(cor(prices2, use = "pairwise.complete.obs"),2)

pricessub = prices2[27:38] ##Subset to retrieve those columns that were not displaying corr in matrix.
colnames(pricessub)
round(cor(pricessub, use = "pairwise.complete.obs"),3)

## Check the distribution of these variables.

#### overallqual is multimodal and should be changed to categorical -- no missings ####
mean(prices$OverallQual)
median(prices$OverallQual)
Mode(prices$OverallQual)
table(prices$OverallQual, exclude = NULL)
plot(density(prices$OverallQual), main = "Overall Distribution of Quality")

#### year built -- multimodal, left skew -- no missings ####
mean(prices$YearBuilt)
median(prices$YearBuilt)
Mode(prices$YearBuilt)
table(prices$YearBuilt, exclude = NULL)
plot(density(prices$YearBuilt), main = "Overall Distribution of Build Year")


#### YearRemodAdd -- multimodal -- no missings ####
mean(prices$YearRemodAdd)
median(prices$YearRemodAdd)
Mode(prices$YearRemodAdd)
plot(density(prices$YearRemodAdd), main = "Overall Distribution of Remodel Year")
table(prices$YearBuilt, exclude = NULL)

#### TotalBsmtSF -- right skewed variable. Could be logged. -- Has 1 missing value ####
mean(prices$TotalBsmtSF, na.rm=T)
median(prices$TotalBsmtSF, na.rm = T)
Mode(prices$TotalBsmtSF, na.rm = T)
table(prices$TotalBsmtSF, exclude = NULL)
plot(density(prices$TotalBsmtSF, na.rm = T), main = "Overall Distribution of Bsmt SF")

#### X1stFlrSF -- skewed right. Could be logged. -- No missing ####
mean(prices$X1stFlrSF)
median(prices$X1stFlrSF)
Mode(prices$X1stFlrSF)
table(prices$X1stFlrSF, exclude = NULL)
plot(density(prices$X1stFlrSF), main = "Overall Distribution of 1st flr SF")

#### GrLivArea -- reasonably normal. May need to be logged. -- No missing ####
mean(prices$GrLivArea)
median(prices$GrLivArea)
Mode(prices$GrLivArea)
table(prices$GrLivArea, exclude = NULL)
plot(density(prices$GrLivArea), main = "Overall Distribution of Living Area Above Grade")

#### FullBath -- no missing ####
#bimodal distribution -- should be changed to categorical var 
mean(prices$FullBath)
median(prices$FullBath)
Mode(prices$FullBath)
table(prices$FullBath, exclude = NULL)
plot(density(prices$FullBath), main = "Overall Distribution of # full baths")


#### TotRmsAbvGrd -- no missings ####
#mean, median and mode coincide, but var is multimodal. Shld be categorical
mean(prices$TotRmsAbvGrd)
median(prices$TotRmsAbvGrd)
Mode(prices$TotRmsAbvGrd)
table(prices$TotRmsAbvGrd, exclude = NULL)
plot(density(prices$TotRmsAbvGrd), main = "Overall Distribution of Rooms above grade")

#### GarageCars -- Has 1 missing value ####
#Needs to be changed to a categorical variable, since it is not really numeric.
mean(prices$GarageCars, na.rm=T)
median(prices$GarageCars, na.rm = T)
Mode(prices$GarageCars, na.rm = T)
table(prices$GarageCars, exclude = NULL)
plot(density(prices$GarageCars, na.rm = T), main = "Overall Distribution of cars in garage")

#### GarageArea -- Log this variable. -- Has 1 missing value ####
mean(prices$GarageArea, na.rm=T)
median(prices$GarageArea, na.rm = T)
Mode(prices$GarageArea, na.rm = T)
table(prices$GarageArea, exclude = NULL)
plot(density(prices$GarageArea), main = "Overall Distribution of cars in garage")
summary(prices$GarageArea)

### For categorical variables, check the frequency to identify any missings.

#### MSZoning -- 4 missing values ####
table(prices$MSZoning, exclude = NULL)

#### Alley -- 1352 missings, but they denote no alley access. ####
table(prices$Alley, exclude = NULL)

#### LotShape -- no missings. ####
table(prices$LotShape, exclude = NULL)

#### Landcontour -- no missings. ####
table(prices$LandContour, exclude = NULL)

#### LotConfig -- no missings. ####
table(prices$LotConfig, exclude = NULL)

#### LandSlope -- no missings. ####
table(prices$LandSlope, exclude = NULL)

#### Neighborhood -- no missings. ####
table(prices$Neighborhood, exclude = NULL)

#### Condition1 -- no missings ####
table(prices$Condition1, exclude = NULL)

#### Condition2 -- no missings ####
table(prices$Condition2, exclude = NULL)

#### BldgType -- no missings ####
table(prices$BldgType, exclude = NULL)

#### HouseStyle -- no missings ####
table(prices$HouseStyle, exclude = NULL)

#### RoofStyle -- no missings ####
table(prices$RoofStyle, exclude = NULL)

#### RoofMatl -- no missings ####
table(prices$RoofMatl, exclude = NULL)

#### Exterior1st -- 1 missing value ####
table(prices$Exterior1st, exclude = NULL)

#### Exterior2nd -- 1 missing value ####
table(prices$Exterior2nd, exclude = NULL)

#### MasVnrType -- 16 missing values ####
table(prices$MasVnrType, exclude = NULL)

#### ExterQual -- no missings. ####
table(prices$ExterQual, exclude = NULL)

#### ExterCond -- no missings. ####
table(prices$ExterCond, exclude = NULL)

#### Foundation -- no missings. ####
table(prices$Foundation, exclude = NULL)

#### BsmtQual -- 44 missings -- no basement ####
table(prices$BsmtQual, exclude = NULL)

#### BsmtCond -- 45 missings -- no basement. ####
table(prices$BsmtCond, exclude = NULL)

#### BsmtExposure -- 44 missings -- no basement. ####
table(prices$BsmtExposure, exclude = NULL)

#### BsmtFinType1 -- 42 missings -- no basement. ####
table(prices$BsmtFinType1, exclude = NULL)

#### BsmtFinType2 -- 42 missings -- no basement. ####
table(prices$BsmtFinType2, exclude = NULL)

## Investigate the unbalanced missings on the Basement vars
xtabs(~BsmtFinType2 + BsmtExposure, data=prices, addNA = T)
xtabs(~BsmtFinType1 + BsmtExposure, data=prices, addNA = T)
xtabs(~BsmtQual + BsmtCond, data=prices, addNA = T)
xtabs(~BsmtExposure + BsmtCond, data=prices, addNA = T)
xtabs(~BsmtQual + BsmtExposure, data=prices, addNA = T)

pricessub = prices %>% 
  select(BsmtCond, BsmtQual, BsmtExposure, BsmtFinType1, BsmtFinType2) %>% 
  filter(is.na(BsmtCond) | is.na(BsmtQual) | is.na(BsmtFinType1)| is.na(BsmtFinType2))

pricessub

#### Heating -- no missings. ####
table(prices$Heating, exclude = NULL)

#### HeatingQC -- no missings. ####
table(prices$HeatingQC, exclude = NULL)

#### CentralAir -- no missings. ####
table(prices$CentralAir, exclude = NULL)

#### Electrical -- no missings.####
table(prices$Electrical, exclude = NULL)

#### KitchenQual - 1 missing value ####
table(prices$KitchenQual, exclude = NULL)

#### Functional -- 2 missing values. ####
table(prices$Functional, exclude = NULL)

#### FireplaceQu -- 730 missings -- no fireplace. ####
table(prices$FireplaceQu, exclude = NULL)

#### GarageType -- 76 missings -- no garage. ####
table(prices$GarageType, exclude = NULL)

#### GarageFinish -- 78 missings -- no garage. ####
table(prices$GarageFinish, exclude = NULL)

#### GarageQual - 78 missings --no garage. ####
table(prices$GarageQual, exclude = NULL)

#### GarageCond -- 78 missings -- no garage ####
table(prices$GarageCond, exclude = NULL)
xtabs(~GarageType + GarageCond, data=prices, exclude = NULL)
table(prices$GarageType, prices$GarageCond, exclude = NULL)

#### PavedDrive -- no missings. ####
table(prices$PavedDrive, exclude = NULL)

#### PoolQC -- 1456 missings -- no pool. ####
table(prices$PoolQC, exclude = NULL)

#### Fence -- 1169 missings -- no fence. ####
table(prices$Fence, exclude = NULL)

#### MiscFeature -- 1408 missing values -- no misc. features. ####
table(prices$MiscFeature, exclude = NULL)

#### SaleType -- 1 missing value ####
table(prices$SaleType, exclude = NULL)

#### SaleCondition -- no missing ####
table(prices$SaleCondition, exclude = NULL)

#### MSSubClass -- no missings. ####
table(prices$MSSubClass, exclude = NULL)
