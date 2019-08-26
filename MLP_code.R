## This is the code for the machine learning project.
## Created by Aparna Sundaram on 08/17/2019
## This program does basic EDA on the training data 
## to get a feel for the data and the variables. 

## Load libraries
library(tidyr)
library(ggplot2)
library(corrr)
library(DescTools)
library(dplyr)

## Read in the training dataset ##

prices = read.csv('train.csv', stringsAsFactors = F)
class(prices)
head(prices)
summary(prices)

sapply(prices, class)
str(prices)

## Check the Y variable -- SalePrice
mean(prices$SalePrice)
median(prices$SalePrice)
Mode(prices$SalePrice)
sd(prices$SalePrice)
var(prices$SalePrice)
table(prices$SalePrice, exclude = NULL) ##No missings in outcome var.
plot(density(prices$SalePrice), main = "Overall Distribution of Sale Price")

## The Y is very skewed indicating that it may need to be log transformed. 
## The correct transform can also be checked using the box-cox transform.


## X variables with NAs:
# LotFrontage, MasVnrArea, garage vars, bsmt vars, Alley, Electrical, FireplaceQu,

## Subset the dataframe to only extract numeric columns to be able to check correlation
prices2 <- select_if(prices, is.numeric)
class(prices2)
colnames(prices2)
round(cor(prices2, use = "pairwise.complete.obs"),2)

pricessub = prices2[27:38] ##Subset to retrieve those columns that were not displaying corr in matrix.
colnames(pricessub)
round(cor(pricessub, use = "pairwise.complete.obs"),3)

## Variables that are correlated more than 0.50 with sale price:
## OverallQual, YearBuilt, YearRemodAdd, TotalBSmtSF, X1stFlrSF,
## GrLivArea, FullBath, TotRmsAbvGrd, GarageCars, GarageArea

## Check the distribution of these variables.

# overallqual is multimodal and should be changed to categorical
mean(prices$OverallQual)
median(prices$OverallQual)
Mode(prices$OverallQual)
plot(density(prices$OverallQual), main = "Overall Distribution of Quality")

# year built -- multimodal, left skew
mean(prices$YearBuilt)
median(prices$YearBuilt)
Mode(prices$YearBuilt)
plot(density(prices$YearBuilt), main = "Overall Distribution of Build Year")
table(prices$YearBuilt)

#YearRemodAdd -- multimodal
mean(prices$YearRemodAdd)
median(prices$YearRemodAdd)
Mode(prices$YearRemodAdd)
plot(density(prices$YearRemodAdd), main = "Overall Distribution of Remodel Year")
table(prices$YearBuilt)

#TotalBsmtSF -- right skewed variable. Could be logged.
mean(prices$TotalBsmtSF)
median(prices$TotalBsmtSF)
Mode(prices$TotalBsmtSF)
plot(density(prices$TotalBsmtSF), main = "Overall Distribution of Bsmt SF")

#X1stFlrSF -- skewed right. Could be logged.
mean(prices$X1stFlrSF)
median(prices$X1stFlrSF)
Mode(prices$X1stFlrSF)
plot(density(prices$X1stFlrSF), main = "Overall Distribution of 1st flr SF")

#GrLivArea -- reasonably normal. May need to be logged.
mean(prices$GrLivArea)
median(prices$GrLivArea)
Mode(prices$GrLivArea)
plot(density(prices$GrLivArea), main = "Overall Distribution of Living Area Above Grade")

#FullBath -- bimodal distribution -- should be changed to categorical var
mean(prices$FullBath)
median(prices$FullBath)
Mode(prices$FullBath)
plot(density(prices$FullBath), main = "Overall Distribution of # full baths")
table(prices$FullBath)

#TotRmsAbvGrd -- mean, median and mode coincide, but var is multimodal. Shld be categorical.
mean(prices$TotRmsAbvGrd)
median(prices$TotRmsAbvGrd)
Mode(prices$TotRmsAbvGrd)
plot(density(prices$TotRmsAbvGrd), main = "Overall Distribution of Rooms above grade")
table(prices$TotRmsAbvGrd)

#GarageCars -- Needs to be changed to a categorical variable, since it is not really numeric.
mean(prices$GarageCars)
median(prices$GarageCars)
Mode(prices$GarageCars)
plot(density(prices$GarageCars), main = "Overall Distribution of cars in garage")

#GarageArea -- Log this variable. 
mean(prices$GarageArea)
median(prices$GarageArea)
Mode(prices$GarageArea)
plot(density(prices$GarageArea), main = "Overall Distribution of cars in garage")
summary(prices$GarageArea)

## subset dataframe to only include the above variables and then get corr. matrix:
prices3 = prices2 %>% 
  select(OverallQual, YearBuilt, YearRemodAdd, 
                TotalBsmtSF, X1stFlrSF, GrLivArea,
                FullBath, TotRmsAbvGrd, GarageCars, GarageArea, SalePrice) %>%
  correlate() %>%
  rearrange() %>%  # rearrange by correlations
  shave() # Shave off the upper triangle for a cleaner result

plot(prices2[,c(5,7,8,13,14,17,20,24)], pch = 19, cex = 1.5,
     col =prices2$SalePrice, lower.panel = NULL)

## For categorical columns do t-tests or ANOVAs to check for relatioship with saleprice:

prices_char = select_if(prices, is.character)
class(prices_char)

sprice = prices %>% select(SalePrice)

class(prices$MSSubClass)
round(cor(prices$MSSubClass, prices$SalePrice, use = "pairwise.complete.obs"),2)
prices$MSSubClass = as.factor(prices$MSSubClass)
class(prices$MSSubClass)

mssclass = prices %>% select(MSSubClass)

prices4 = cbind(prices_char, sprice, mssclass)
colnames(prices4)
describe(prices4)
summary(prices4)

table(prices4$MSZoning, exclude = NULL)
zontab = table(prices4$MSZoning)
prop.table(zontab)
summary(aov(prices4$SalePrice ~ prices4$MSZoning)) ##there is a diff. in mean SP by Zoning groups
ggplot(data = prices4, aes(x =reorder(MSZoning, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = MSZoning)) + geom_bar(aes(fill = SalePrice))

table(prices4$Street, exclude = NULL)
t.test(prices4$SalePrice ~ prices4$Street, data = prices4) ##There is no diff in mean SP by street type.
ggplot(data = prices4, aes(x =reorder(Street, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Street)) + geom_bar(aes(fill = SalePrice))


table(prices4$Alley, exclude = NULL) ##Too many missings. Should be changed to Y/N.
t.test(prices4$SalePrice ~ prices4$Alley, data = prices4) ##there is a diff. in mean SP by Alley groups
ggplot(data = prices4, aes(x =reorder(Alley, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Alley)) + geom_bar(aes(fill = SalePrice))

table(prices4$LotShape, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$LotShape)) ##there is a diff. in mean SP by Lotshape groups
ggplot(data = prices4, aes(x =reorder(LotShape, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = LotShape)) + geom_bar(aes(fill = SalePrice))

table(prices4$LandContour, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$LandContour)) ##there is a diff. in mean SP by LandCountour groups
ggplot(data = prices4, aes(x =reorder(LandContour, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = LandContour)) + geom_bar(aes(fill = SalePrice))

table(prices4$Utilities, exclude=NULL)
t.test(prices4$SalePrice ~ prices4$Utilities, data = prices4) ##Not a good variable, since no variance.

table(prices4$LotConfig, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$LotConfig)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(LotConfig, SalePrice, median), y= SalePrice)) + geom_boxplot()

table(prices4$LandSlope, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$LandSlope)) ##There is no diff by SP here.
ggplot(data = prices4, aes(x =reorder(LandSlope, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = LandSlope)) + geom_bar(aes(fill = SalePrice))

table(prices4$Neighborhood, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$Neighborhood)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Neighborhood, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Neighborhood)) + geom_bar(aes(fill = SalePrice))

table(prices4$Condition1, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$Condition1)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Condition1, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Condition1)) + geom_bar(aes(fill = SalePrice))

table(prices4$Condition2, exclude = NULL) ##unbalanced cell sizes. Combine with cond1.
summary(aov(prices4$SalePrice ~ prices4$Condition2)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Condition2, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Condition2)) + geom_bar(aes(fill = SalePrice))

table(prices4$BldgType, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$BldgType)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(BldgType, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = BldgType)) + geom_bar(aes(fill = SalePrice))

table(prices4$HouseStyle, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$HouseStyle)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(HouseStyle, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = HouseStyle)) + geom_bar(aes(fill = SalePrice))

table(prices4$RoofStyle, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$RoofStyle)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(RoofStyle, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = RoofStyle)) + geom_bar(aes(fill = SalePrice))

table(prices4$RoofMatl, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$RoofMatl)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(RoofMatl, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = RoofMatl)) + geom_bar(aes(fill = SalePrice))

table(prices4$Exterior1st, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$Exterior1st)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Exterior1st, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Exterior1st)) + geom_bar(aes(fill = SalePrice))

table(prices4$Exterior2nd, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$Exterior2nd)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Exterior2nd, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Exterior2nd)) + geom_bar(aes(fill = SalePrice))

table(prices4$MasVnrType, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$MasVnrType)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(MasVnrType, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = MasVnrType)) + geom_bar(aes(fill = SalePrice))

table(prices4$ExterQual, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$ExterQual))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(ExterQual, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = ExterQual)) + geom_bar(aes(fill = SalePrice))

table(prices4$ExterCond, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$ExterCond))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(ExterCond, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = ExterCond)) + geom_bar(aes(fill = SalePrice))

table(prices4$Foundation, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$Foundation))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Foundation, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Foundation)) + geom_bar(aes(fill = SalePrice))

table(prices4$BsmtQual, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$BsmtQual))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(BsmtQual, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = BsmtQual)) + geom_bar(aes(fill = SalePrice))

table(prices4$BsmtCond, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$BsmtCond))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(BsmtCond, SalePrice, median), y= SalePrice)) + geom_boxplot()

table(prices4$BsmtExposure, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$BsmtExposure))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(BsmtExposure, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = BsmtExposure)) + geom_bar(aes(fill = SalePrice))

table(prices4$BsmtFinType1, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$BsmtFinType1))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(BsmtFinType1, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = BsmtFinType1)) + geom_bar(aes(fill = SalePrice))

table(prices4$BsmtFinType2, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$BsmtFinType2))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(BsmtFinType2, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = BsmtFinType2)) + geom_bar(aes(fill = SalePrice))

table(prices4$Heating, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$Heating))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Heating, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Heating)) + geom_bar(aes(fill = SalePrice))

table(prices4$HeatingQC, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$HeatingQC))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(HeatingQC, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = HeatingQC)) + geom_bar(aes(fill = SalePrice))

table(prices4$CentralAir, exclude = NULL)
t.test(prices4$SalePrice ~ prices4$CentralAir, data = prices4) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(CentralAir, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = CentralAir)) + geom_bar(aes(fill = SalePrice))

table(prices4$Electrical, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$Electrical))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Electrical, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Electrical)) + geom_bar(aes(fill = SalePrice))

table(prices4$KitchenQual, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$KitchenQual))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(KitchenQual, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = KitchenQual)) + geom_bar(aes(fill = SalePrice))

table(prices4$Functional, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$Functional))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Functional, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Functional)) + geom_bar(aes(fill = SalePrice))

table(prices4$FireplaceQu, exclude = NULL) ## fair bit of missing here for no fireplace.
summary(aov(prices4$SalePrice ~ prices4$FireplaceQu))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(FireplaceQu, SalePrice, median), y= SalePrice)) + geom_boxplot()

table(prices4$GarageType, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$GarageType))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(GarageType, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = GarageType)) + geom_bar(aes(fill = SalePrice))

table(prices4$GarageFinish, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$GarageFinish))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(GarageFinish, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = GarageFinish)) + geom_bar(aes(fill = SalePrice))

table(prices4$GarageQual, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$GarageQual))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(GarageQual, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = GarageQual)) + geom_bar(aes(fill = SalePrice))

table(hprices$GarageCond, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$GarageCond))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(GarageCond, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = GarageCond)) + geom_bar(aes(fill = SalePrice))

table(prices4$PavedDrive, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$PavedDrive))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(PavedDrive, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = PavedDrive)) + geom_bar(aes(fill = SalePrice))

table(prices4$PoolQC, exclude = NULL) ## Poor variable. Can't be used.
summary(aov(prices4$SalePrice ~ prices4$PoolQC))  ##Not a good var now. Create Pool Y/N instead.

table(prices4$Fence, exclude = NULL) ## Too many missings. Omit, unless it is no fence?
summary(aov(prices4$SalePrice ~ prices4$Fence))  ## There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(Fence, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = Fence)) + geom_bar(aes(fill = SalePrice))

table(prices4$MiscFeature, exclude = NULL) # Too many missings
summary(aov(prices4$SalePrice ~ prices4$MiscFeature))  ##Not significantly assoc. with SP.

table(prices4$SaleType, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$SaleType))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(SaleType, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = SaleType)) + geom_bar(aes(fill = SalePrice))

table(prices4$SaleCondition, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$SaleCondition))  ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(SaleCondition, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = SaleCondition)) + geom_bar(aes(fill = SalePrice))

table(prices4$MSSubClass, exclude = NULL)
summary(aov(prices4$SalePrice ~ prices4$MSSubClass)) ##There is a diff by SP here.
ggplot(data = prices4, aes(x =reorder(MSSubClass, SalePrice, median), y= SalePrice)) + geom_boxplot()
ggplot(data = prices4,aes(x = MSSubClass)) + geom_bar()


## Categorical vars that matter for saleprice:
## MSZoning, Alley, Lotshape, LandContour, LotCofig, Neighborhood, Condition1 and 2, BldgType,
## HouseStyle, Roofstyle, RoofMatl, Exterior1st and 2nd, MasVnrType, ExterQual, ExterCond,
## Foundation, BsmtQual, BsmtCond, BsmtExposure, BsmtFintype1 and 2, Heating, HeatingQC, CentralAir,
## Electrical, KitchenQual, Functional, FireplaceQu, GarageType & Finish & Qual & Cond,
## PavedDrive, Fence, SaleType and SaleCondition.


## Obtain means and medians by groups in the above list of categorical vars.
datazone=prices4 %>% 
  select(MSZoning, SalePrice) %>%
  group_by(MSZoning) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataalley= prices4 %>% 
  select(Alley, SalePrice) %>%
  group_by(Alley) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

datalotshape=prices4 %>% 
  select(LotShape, SalePrice) %>%
  group_by(LotShape) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

datalandc=prices4 %>% 
  select(LandContour, SalePrice) %>%
  group_by(LandContour) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataneigh=prices4 %>% 
  select(Neighborhood, SalePrice) %>%
  group_by(Neighborhood) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

datacond1=prices4 %>% 
  select(Condition1, SalePrice) %>%
  group_by(Condition1) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

datacond2=prices4 %>% 
  select(Condition2, SalePrice) %>%
  group_by(Condition2) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

databldg=prices4 %>% 
  select(BldgType, SalePrice) %>%
  group_by(BldgType) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataHS=prices4 %>% 
  select(HouseStyle, SalePrice) %>%
  group_by(HouseStyle) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataRS=prices4 %>% 
  select(RoofStyle, SalePrice) %>%
  group_by(RoofStyle) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataRM=prices4 %>% 
  select(RoofMatl, SalePrice) %>%
  group_by(RoofMatl) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataE1=prices4 %>% 
  select(Exterior1st, SalePrice) %>%
  group_by(Exterior1st) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataE2=prices4 %>% 
  select(Exterior2nd, SalePrice) %>%
  group_by(Exterior2nd) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataMVT=prices4 %>% 
  select(MasVnrType, SalePrice) %>%
  group_by(MasVnrType) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataEQ=prices4 %>% 
  select(ExterQual, SalePrice) %>%
  group_by(ExterQual) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataEC=prices4 %>% 
  select(ExterCond, SalePrice) %>%
  group_by(ExterCond) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataFound=prices4 %>% 
  select(Foundation, SalePrice) %>%
  group_by(Foundation) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataBsQ=prices4 %>% 
  select(BsmtQual, SalePrice) %>%
  group_by(BsmtQual) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataBsC=prices4 %>% 
  select(BsmtCond, SalePrice) %>%
  group_by(BsmtCond) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataBsE=prices4 %>% 
  select(BsmtExposure, SalePrice) %>%
  group_by(BsmtExposure) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataFT1=prices4 %>% 
  select(BsmtFinType1, SalePrice) %>%
  group_by(BsmtFinType1) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataFT2=prices4 %>% 
  select(BsmtFinType2, SalePrice) %>%
  group_by(BsmtFinType2) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataht=prices4 %>% 
  select(Heating, SalePrice) %>%
  group_by(Heating) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

datahtQC=prices4 %>% 
  select(HeatingQC, SalePrice) %>%
  group_by(HeatingQC) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataCenAir=prices4 %>% 
  select(CentralAir, SalePrice) %>%
  group_by(CentralAir) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataEl=prices4 %>% 
  select(Electrical, SalePrice) %>%
  group_by(Electrical) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataKit=prices4 %>% 
  select(KitchenQual, SalePrice) %>%
  group_by(KitchenQual) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataFun=prices4 %>% 
  select(Functional, SalePrice) %>%
  group_by(Functional) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataFpQ=prices4 %>% 
  select(FireplaceQu, SalePrice) %>%
  group_by(FireplaceQu) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataGty=prices4 %>% 
  select(GarageType, SalePrice) %>%
  group_by(GarageType) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataGFin=prices4 %>% 
  select(GarageFinish, SalePrice) %>%
  group_by(GarageFinish) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataGQ=prices4 %>% 
  select(GarageQual, SalePrice) %>%
  group_by(GarageQual) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataGCon=prices4 %>% 
  select(GarageCond, SalePrice) %>%
  group_by(GarageCond) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataPaved=prices4 %>% 
  select(PavedDrive, SalePrice) %>%
  group_by(PavedDrive) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataFen=prices4 %>% 
  select(Fence, SalePrice) %>%
  group_by(Fence) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataSaleT=prices4 %>% 
  select(SaleType, SalePrice) %>%
  group_by(SaleType) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

dataSaleC=prices4 %>% 
  select(SaleCondition, SalePrice) %>%
  group_by(SaleCondition) %>% 
  summarise(avg=mean(SalePrice), med=median(SalePrice), minp=min(SalePrice), maxp=max(SalePrice))

## Rowbind the above files to make more condensed files
class(datacond1)
class(datacond2)

datacond = bind_rows(datacond1, datacond2)
dataroof = bind_rows(dataRS, dataRM)
dataexter = bind_rows(dataE1, dataE2)
dataext = bind_rows(dataEQ, dataEC)
databsmt = bind_rows(dataBsQ, dataBsE, dataBsC)
dataFT = bind_rows(dataFT1, dataFT2)
dataheat = bind_rows(dataht, datahtQC)
datagar = bind_rows(dataGty, dataGFin, dataGCon, dataGQ)
datasal = bind_rows(dataSaleC, dataSaleT)

##Crosstab the following groups of variables and do a chi-sq test.

# Condition1 and 2
contab=table(prices$Condition1, prices$Condition2)
chisq.test(contab) ##significant indicating dependence.
CrossTable(prices$Condition1, prices$Condition2) ##Gives SAS like crosstab. TG!
xtabs(~Condition1 + Condition2, data=prices) ##slightly better than table since it labels the rows and cols.

# Roofstyle, RoofMatl
rooftab=table(prices$RoofStyle, prices$RoofMatl)
chisq.test(rooftab) ##significant indicating dependence.
CrossTable(prices$RoofStyle, prices$RoofMatl) ##Gives SAS like crosstab. TG!
xtabs(~RoofStyle + RoofMatl, data=prices)

# Exterior1st and 2nd
exttab=table(prices$Exterior1st, prices$Exterior2nd)
chisq.test(exttab) ##significant indicating dependence.
CrossTable(prices$Exterior1st, prices$Exterior2nd) ##Gives SAS like crosstab. TG!
xtabs(~Exterior1st + Exterior2nd, data=prices)

# ExterQual, ExterCond
extertab=table(prices$ExterQual, prices$ExterCond)
chisq.test(extertab) ##significant indicating dependence.
CrossTable(prices$ExterQual, prices$ExterCond) ##Gives SAS like crosstab. TG!
xtabs(~ExterQual + ExterCond, data=prices)

# BsmtQual, BsmtCond, BsmtExposure, BsmtFintype1 and 2
bsmttab1=table(prices$BsmtQual, prices$BsmtCond)
chisq.test(bsmttab1) ##significant indicating dependence.

bsmttab2 = table(prices$BsmtQual, prices$BsmtExposure)
chisq.test(bsmttab2) ##significant indicating dependence.

bsmttab3 = table(prices$BsmtCond, prices$BsmtExposure)
chisq.test(bsmttab3) ##significant indicating dependence.

bsmtfintab = table(prices$BsmtFinType1, prices$BsmtFinType2)
chisq.test(bsmtfintab) ##significant indicating dependence.


# Heating, HeatingQC
heattab=table(prices$Heating, prices$HeatingQC)
chisq.test(heattab) ##significant indicating dependence.
CrossTable(prices$Heating, prices$HeatingQC) ##Gives SAS like crosstab. TG!
xtabs(~Heating + HeatingQC, data=prices)


#GarageType & Finish & Qual & Cond

gartab1=table(prices$GarageType, prices$GarageFinish)
chisq.test(gartab1) ##significant indicating dependence.

gartab2 = table(prices$GarageType, prices$GarageFinish)
chisq.test(gartab2) ##significant indicating dependence.

gartab3 = table(prices$GarageType, prices$GarageQual)
chisq.test(gartab3) ##significant indicating dependence.

gartab4 = table(prices$GarageType, prices$GarageCond)
chisq.test(gartab4) ##significant indicating dependence.


#SaleType and SaleCondition
saletab=table(prices$SaleType, prices$SaleCondition)
chisq.test(saletab) ##significant indicating dependence.
CrossTable(prices$SaleType, prices$SaleCondition) ##Gives SAS like crosstab. TG!
xtabs(~SaleType + SaleCondition, data=prices)

# Lotshape, LotCofig
lottab=table(prices$LotShape, prices$LotConfig)
chisq.test(lottab) ##significant indicating dependence.
CrossTable(prices$LotShape, prices$LotConfig) ##Gives SAS like crosstab. TG!
xtabs(~LotShape + LotConfig, data=prices)

# BldgType, HouseStyle
styletab=table(prices$BldgType, prices$HouseStyle)
chisq.test(styletab) ##significant indicating dependence.
CrossTable(prices$BldgType, prices$HouseStyle) ##Gives SAS like crosstab. TG!
xtabs(~BldgType + HouseStyle, data=prices)

# HouseStyle, FireplaceQu
firetab=table(prices$FireplaceQu, prices$HouseStyle)
chisq.test(firetab) ##significant indicating dependence.
CrossTable(prices$FireplaceQu, prices$HouseStyle) ##Gives SAS like crosstab. TG!
xtabs(~FireplaceQu + HouseStyle, data=prices)

# MSZoning, FireplaceQu
fireztab=table(prices$FireplaceQu, prices$MSZoning)
chisq.test(fireztab) ##not significant indicating independence.
CrossTable(prices$FireplaceQu, prices$MSZoning) ##Gives SAS like crosstab. TG!
xtabs(~FireplaceQu + MSZoning, data=prices)

# MSZoning, LandContour
landtab=table(prices$LandContour, prices$MSZoning)
chisq.test(landtab) ##not significant indicating independence.
CrossTable(prices$LandContour, prices$MSZoning) ##Gives SAS like crosstab. TG!
xtabs(~LandContour + MSZoning, data=prices)

## Categorical vars that matter for saleprice:
## MSZoning, Alley, Lotshape, LandContour, LotCofig, Neighborhood, Condition1 and 2, BldgType,
## HouseStyle, Roofstyle, RoofMatl, Exterior1st and 2nd, MasVnrType, ExterQual, ExterCond,
## Foundation, BsmtQual, BsmtCond, BsmtExposure, BsmtFintype1 and 2, Heating, HeatingQC, CentralAir,
## Electrical, KitchenQual, Functional, FireplaceQu, GarageType & Finish & Qual & Cond,
## PavedDrive, Fence, SaleType and SaleCondition.

