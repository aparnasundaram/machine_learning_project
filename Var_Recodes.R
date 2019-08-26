## This file recodes the different variables before includng them in the regression model
## Created by Aparna Sundaram on 08/18/2019

## Load libraries
library(tidyr)
library(ggplot2)
library(corrr)
library(DescTools)
library(car)
library(gmodels)
library(dplyr)

## Read in dataset

hprices = read.csv('train.csv', stringsAsFactors = F)

###-----------------------------------------###
### Outcome/Response variable -- Sale Price ###
###-----------------------------------------###

#Sale Price - take the log of Sale Price -- no missings 
hprices$Lsaleprice = log(hprices$SalePrice)

###--------------------------------------### 
### Manipulate the different X variables ###
###--------------------------------------###

#### Continuous variable recodes ####

#### TotalBsmtSF -- No missing in train. **1 in test.** ####
#create a new variable that is a duplicate of the total Basement SF
hprices$TotalBsmtSF2 = hprices$TotalBsmtSF

#set the basement SF value of zero to 1 to enable logging.
hprices$TotalBsmtSF2[hprices$TotalBsmtSF2 == 0] = 1

#Log the variable
hprices$Ltotbsmtsf = log(hprices$TotalBsmtSF2)

#The following code is redundant
#hprices$Ltotbsmtsf[hprices$TotalBsmtSF == 0] = 0

mean(hprices$TotalBsmtSF)
sd(hprices$TotalBsmtSF)
min(hprices$Ltotbsmtsf)
max(hprices$Ltotbsmtsf)

avg1 = mean(hprices$Ltotbsmtsf) ## this is better.
sd31 = mean(hprices$Ltotbsmtsf) + 3*sd(hprices$Ltotbsmtsf) ##didn't work quite so well.

table(hprices$TotalBsmtSF, exclude = NULL)
table(hprices$Ltotbsmtsf, exclude = NULL)
plot(hprices$Ltotbsmtsf, hprices$Lsaleprice, xlab = "Log of Bsmt SF", 
     ylab = "Log of home sale price",
     main = "Scatterplot of BSF by Sale Price")

#Finding outliers in totalbasement SF -- ignore the zeros.
outlier1=Outlier(hprices$Ltotbsmtsf)
outlier1= outlier1[outlier1>0]

#Convert the non-zero outlier values to avg1. 
hprices$Ltotbsmtsf[hprices$Ltotbsmtsf %in% outlier1] = avg1

## Code for checking the log value = -Inf.
outbsmtsf = hprices %>% 
  select(Ltotbsmtsf, TotalBsmtSF) %>% 
  filter(Ltotbsmtsf == -Inf)


#### X1stFlrSF -- No missing ####
hprices$L1stflrSF = log(hprices$X1stFlrSF)

mean(hprices$L1stflrSF)
sd(hprices$L1stflrSF)
min(hprices$L1stflrSF)
max(hprices$L1stflrSF)

avg2 = mean(hprices$L1stflrSF) ## this is better.
sd32 = mean(hprices$L1stflrSF) + 3*sd(hprices$L1stflrSF) ##didn't work quite so well.
outlier2 =Outlier(hprices$L1stflrSF)

#Convert the non-zero outlier values to avg1. 
hprices$L1stflrSF[hprices$L1stflrSF %in% outlier2] = avg2

table(hprices$X1stFlrSF, exclude = NULL)
plot(hprices$L1stflrSF, hprices$Lsaleprice, xlab = "Log of 1st Flr SF", 
     ylab = "Log of home sale price",
     main = "Scatterplot of SF by Sale Price")

#### GrLivArea -- No missing ####
hprices$Lgrlivarea = log(hprices$GrLivArea)

mean(hprices$Lgrlivarea)
sd(hprices$Lgrlivarea)
min(hprices$Lgrlivarea)
max(hprices$Lgrlivarea)

avg3= mean(hprices$Lgrlivarea) ## this is better.
sd33 = mean(hprices$Lgrlivarea) + 3*sd(hprices$Lgrlivarea) ##didn't work quite so well.
outlier3 =Outlier(hprices$Lgrlivarea)
hprices$Lgrlivarea[hprices$Lgrlivarea %in% outlier3] = avg3

table(hprices$GrLivArea, exclude = NULL)
plot(hprices$Lgrlivarea, hprices$Lsaleprice, xlab = "Log of abv grd living area", 
     ylab = "Log of home sale price",
     main = "Scatterplot of abv grade living area by Sale Price")

#### GarageArea -- No missing in train. **1 in test.** ####
hprices$Lgaragearea = log(hprices$GarageArea)

#set all -inf to zero
hprices$Lgaragearea[hprices$GarageArea == 0] = 0

mean(hprices$Lgaragearea)
sd(hprices$Lgaragearea)
min(hprices$Lgaragearea)
max(hprices$Lgaragearea)

avg4 = mean(hprices$Lgaragearea)
sd34 = mean(hprices$Lgaragearea) + 3*sd(hprices$Lgaragearea) ##didn't work quite so well.

outlier4 =Outlier(hprices$Lgaragearea)
outlier4

table(hprices$GarageArea, exclude = NULL)
plot(hprices$lgaragearea, hprices$Lsaleprice, xlab = "Log of garage SF", 
     ylab = "Log of home sale price",
     main = "Scatterplot of garage SF by Sale Price")

#### Total Square Footage of House ####
hprices$TotHouseArea = hprices$TotalBsmtSF + hprices$X1stFlrSF + 
  hprices$X2ndFlrSF + hprices$GarageArea

mean(hprices$TotalBsmtSF)
mean(hprices$X1stFlrSF)
mean(hprices$X2ndFlrSF)
mean(hprices$GrLivArea)
mean(hprices$GarageArea)
mean(hprices$TotHouseArea)

hprices$Ltothousearea = log(hprices$TotHouseArea)
mean(hprices$Ltothousearea)
min(hprices$Ltothousearea)
max(hprices$Ltothousearea)

avgnew = mean(hprices$Ltothousearea)
sdnew = mean(hprices$Ltothousearea) + 3*sd(hprices$Ltothousearea) ##didn't work quite so well.
outliernew =Outlier(hprices$Ltothousearea) 
outliernew
hprices$Ltothousearea[hprices$Ltothousearea %in% outliernew] = avgnew

summary(hprices$Ltothousearea)

plot(density(hprices$Ltothousearea), main = "Overall Distribution of House Area")
ggplot(data = hprices, aes(x = Ltothousearea, y = Lsaleprice)) +
  geom_point()

#### YearBuilt -- Changed to years between built and sold. No missings ####
##No missing
table(hprices$YrSold)
hprices$durbuilt = hprices$YrSold - hprices$YearBuilt
table(hprices$durbuilt)
round(cor(hprices$SalePrice, hprices$durbuilt, use = "pairwise.complete.obs"),2)
summary(hprices$durbuilt)

#### YearRemodAdd -- Changed to years between remodeled and sold. No missings ####
## No missing
table(hprices$YearRemodAdd)
hprices$durremod = hprices$YrSold - hprices$YearRemodAdd
table(hprices$durremod)
check1 = hprices[hprices$durremod == -1,] ##House was remodeled after purchase.
check1
hprices$durremod[hprices$durremod < 0] = 0 #Change all negatives to 0.
table(hprices$durremod)
round(cor(hprices$SalePrice, hprices$durremod, use = "pairwise.complete.obs"),2)

#### Categorical variable recodes.####

#### OverallQual -- No missing ####
hprices$OverallQual = as.factor(hprices$OverallQual)
hprices$hqualcat = NULL
hprices$hqualcat[hprices$OverallQual == 1 | hprices$OverallQual == 2 |
                   hprices$OverallQual == 3 | hprices$OverallQual == 4] = 1
hprices$hqualcat[hprices$OverallQual == 5] = 2
hprices$hqualcat[hprices$OverallQual == 6] = 3
hprices$hqualcat[hprices$OverallQual == 7] = 4
hprices$hqualcat[hprices$OverallQual == 8 | hprices$OverallQual == 9 |
                   hprices$OverallQual == 10] = 5
table(hprices$hqualcat, exclude = NULL)
table(hprices$OverallQual, exclude = NULL)
hprices$hqualcat = as.factor(hprices$hqualcat)

#### FullBath -- No missing ####
hprices$FullBath = as.factor(hprices$FullBath)
hprices$fullbathcat = NULL
hprices$fullbathcat[hprices$FullBath == 0] = 0
hprices$fullbathcat[hprices$FullBath == 1] = 1
hprices$fullbathcat[hprices$FullBath == 2] = 2
hprices$fullbathcat[hprices$FullBath == 3 | hprices$FullBath == 4] = 3
table(hprices$fullbathcat, exclude = NULL)
table(hprices$FullBath, exclude = NULL)
hprices$fullbathcat = as.factor(hprices$fullbathcat)

#### TotRmsAbvGrd -- No missing ####
hprices$TotRmsAbvGrd = as.factor(hprices$TotRmsAbvGrd)
hprices$totrmscat = NULL
hprices$totrmscat[hprices$TotRmsAbvGrd == 1 | hprices$TotRmsAbvGrd == 2 |
                   hprices$TotRmsAbvGrd == 3 | hprices$TotRmsAbvGrd == 4] = 1
hprices$totrmscat[hprices$TotRmsAbvGrd == 5] = 2
hprices$totrmscat[hprices$TotRmsAbvGrd == 6] = 3
hprices$totrmscat[hprices$TotRmsAbvGrd == 7] = 4
hprices$totrmscat[hprices$TotRmsAbvGrd == 8 | hprices$TotRmsAbvGrd == 9 |
                    hprices$TotRmsAbvGrd == 10 | hprices$TotRmsAbvGrd == 11|
                    hprices$TotRmsAbvGrd == 12 | hprices$TotRmsAbvGrd == 14] = 5
table(hprices$totrmscat, exclude = NULL)
table(hprices$TotRmsAbvGrd, exclude = NULL)
hprices$totrmscat = as.factor(hprices$totrmscat)

#### Garage Cars -- No missing in train. **1 in test.** ####
hprices$GarageCars = as.factor(hprices$GarageCars)
hprices$garcarcat = NULL
hprices$garcarcat[hprices$GarageCars == 1] = 1
hprices$garcarcat[hprices$GarageCars == 2] = 2
hprices$garcarcat[hprices$GarageCars == 3 | hprices$GarageCars == 4] = 3
hprices$garcarcat[hprices$GarageCars == 0] = 0
table(hprices$garcarcat, exclude = NULL)
table(hprices$GarageCars, exclude = NULL)
hprices$garcarcat = as.factor(hprices$garcarcat)

#### MSZoning -- created binary RL vs. others. -- No missing ####
hprices$zonenew = NULL
hprices$zonenew[hprices$MSZoning == "RL"] = 1
hprices$zonenew[hprices$MSZoning == "C (all)" | hprices$MSZoning == "FV" |
                  hprices$MSZoning == "RH" | hprices$MSZoning == "RM"] = 0
table(hprices$zonenew, exclude = NULL)
table(hprices$MSZoning, exclude = NULL)
hprices$zonenew = as.factor(hprices$zonenew)

#### LotShape -- No missing ####
hprices$Lotshdum = NULL
hprices$Lotshdum[hprices$LotShape == 'Reg'] = 1
hprices$Lotshdum[hprices$LotShape == 'IR1'| hprices$LotShape == 'IR2' | 
                   hprices$LotShape == 'IR3'] = 0
table(hprices$Lotshdum, exclude = NULL)
table(hprices$LotShape, exclude = NULL)
hprices$Lotshdum = as.factor(hprices$Lotshdum)

#### LandContour -- No missing ####
hprices$Lotcondum = NULL
hprices$Lotcondum[hprices$LandContour == 'Lvl'] = 1
hprices$Lotcondum[hprices$LandContour == 'Bnk' | hprices$LandContour == 'HLS' |
                    hprices$LandContour == 'Low'] = 0
table(hprices$Lotcondum, exclude = NULL)
table(hprices$LandContour, exclude = NULL)
hprices$Lotcondum = as.factor(hprices$Lotcondum)

#### Condition1 -- No missing -- redo this variable to dummy ####
hprices$Cond1dum = NULL
hprices$Cond1dum[hprices$Condition1 == 'Norm'] = 1
hprices$Cond1dum[hprices$Condition1 == 'RRAe' | hprices$Condition1 == 'RRAn' |
                   hprices$Condition1 == 'RRNe'| hprices$Condition1 == 'RRNn'] = 2
hprices$Cond1dum[hprices$Condition1 == 'Artery' | hprices$Condition1 == 'Feedr'] = 3
hprices$Cond1dum[hprices$Condition1 == 'PosA'| hprices$Condition1 == 'PosN'] = 4
table(hprices$Cond1dum, exclude = NULL)
table(hprices$Condition1, exclude = NULL)
hprices$Cond1dum = as.factor(hprices$Cond1dum)

#### Condition2 -- No missing -- not included in pandas file ####
hprices$Cond2dum = NULL
hprices$Cond2dum[hprices$Condition2 == 'Norm'] = 1
hprices$Cond2dum[hprices$Condition2 == 'RRAe'|hprices$Condition2 == 'RRAn'| 
                   hprices$Condition2 == 'RRNn'| hprices$Condition2 == 'RRNn'] = 2
hprices$Cond2dum[hprices$Condition2 == 'Artery'|hprices$Condition2 == 'Feedr'] = 3
hprices$Cond2dum[hprices$Condition2 == 'PosA'|hprices$Condition2 == 'PosN'] = 4
table(hprices$Cond2dum, exclude = NULL)
table(hprices$Condition2, exclude = NULL)
hprices$Cond2dum = as.factor(hprices$Cond2dum)

# Condition -- needs more thought and may not be needed if only 1 is chosen.
CrossTable(hprices$Condition1, hprices$Condition2) ##Gives SAS like crosstab.
xtabs(~Condition1 + Condition2, data=hprices)

#### BldgType -- No missing ####
hprices$bldgdum = NULL
hprices$bldgdum[hprices$BldgType == '1Fam'] = 1
hprices$bldgdum[hprices$BldgType == '2fmCon' | hprices$BldgType == 'Duplex' |
                  hprices$BldgType == 'Twnhs' | hprices$BldgType == 'TwnhsE'] = 0
table(hprices$bldgdum, exclude = NULL)
table(hprices$BldgType, exclude = NULL)
hprices$bldgdum = as.factor(hprices$bldgdum)

#### HouseStyle -- No missing ####
hprices$housedum = NULL
hprices$housedum[hprices$HouseStyle == '1Story']= 1
hprices$housedum[hprices$HouseStyle == '1.5Fin' | hprices$HouseStyle == '1.5Unf']= 2
hprices$housedum[hprices$HouseStyle == '2.5Unf'| hprices$HouseStyle == '2.5Fin'|
                   hprices$HouseStyle == '2Story']= 3
hprices$housedum[hprices$HouseStyle == 'SFoyer' | hprices$HouseStyle == 'SLvl']= 4
table(hprices$housedum)
table(hprices$HouseStyle)
hprices$housedum = as.factor(hprices$housedum)

#### Roofstyle -- No missing ####
hprices$roofsdum = NULL
hprices$roofsdum[hprices$RoofStyle == 'Gable']= 1
hprices$roofsdum[hprices$RoofStyle == 'Hip']= 0
hprices$roofsdum[hprices$RoofStyle == 'Gambrel'|hprices$RoofStyle == 'Mansard' | 
                   hprices$RoofStyle == 'Shed' | hprices$RoofStyle == 'Flat' ]= 0
table(hprices$roofsdum)
table(hprices$RoofStyle)
hprices$roofsdum = as.factor(hprices$roofsdum)

#### RoofMatl -- No missing ##### 
## include for now, but may need to rethink given the lack of balance in freq.

hprices$roofmdum = NULL
hprices$roofmdum[hprices$RoofMatl == 'CompShg']= 1
hprices$roofmdum[hprices$RoofMatl == 'ClyTile'|hprices$RoofMatl == 'Membran' | 
                   hprices$RoofMatl == 'Metal' | hprices$RoofMatl == 'Roll' |
                   hprices$RoofMatl == 'Tar&Grv' | hprices$RoofMatl == 'WdShake' |
                   hprices$RoofMatl == 'WdShngl']= 0
table(hprices$roofmdum, exclude = NULL)
table(hprices$RoofMatl, exclude = NULL)
hprices$roofmdum = as.factor(hprices$roofmdum)

#### Exterior 1st -- No missing in train. **1 in test.**####
hprices$extercat = NULL
hprices$extercat[hprices$Exterior1st == 'VinylSd']= 1
hprices$extercat[hprices$Exterior1st == 'HdBoard'] = 2
hprices$extercat[hprices$Exterior1st == 'MetalSd'] = 3
hprices$extercat[hprices$Exterior1st == 'Wd Sdng'] = 4
hprices$extercat[hprices$Exterior1st == 'Plywood'] = 5
hprices$extercat[hprices$Exterior1st == 'AsbShng' | hprices$Exterior1st == 'AsphShn'|
                   hprices$Exterior1st == 'BrkComm'|hprices$Exterior1st =='BrkFace'|
                   hprices$Exterior1st == 'CBlock'|hprices$Exterior1st =='CemntBd'|
                   hprices$Exterior1st == 'ImStucc'|hprices$Exterior1st =='Stone'|
                   hprices$Exterior1st == 'Stucco'|hprices$Exterior1st =='WdShing'] = 6
table(hprices$extercat, exclude = NULL)
table(hprices$Exterior1st, exclude = NULL)
hprices$extercat = as.factor(hprices$extercat)

#### Exterior 2nd -- No missing in train. **1 in test.** ####
xtabs(~Exterior1st + Exterior2nd, data=hprices)
CrossTable(hprices$Exterior1st, hprices$Exterior2nd) ##Gives SAS like crosstab.
hprices$extercat2 = NULL
hprices$extercat2[hprices$Exterior2nd == 'VinylSd']= 1
hprices$extercat2[hprices$Exterior2nd == 'HdBoard'] = 2
hprices$extercat2[hprices$Exterior2nd == 'MetalSd'] = 3
hprices$extercat2[hprices$Exterior2nd == 'Wd Sdng'] = 4
hprices$extercat2[hprices$Exterior2nd == 'Plywood'] = 5
hprices$extercat2[hprices$Exterior2nd == 'AsbShng' | hprices$Exterior2nd == 'AsphShn'|
                   hprices$Exterior2nd == 'Brk Cmn'|hprices$Exterior2nd =='BrkFace'|
                   hprices$Exterior2nd == 'CBlock'|hprices$Exterior2nd =='CmentBd'|
                   hprices$Exterior2nd == 'ImStucc'|hprices$Exterior2nd =='Stone'|
                   hprices$Exterior2nd == 'Stucco'|hprices$Exterior2nd =='Wd Shng'|
                   hprices$Exterior2nd == 'Other'] = 6
table(hprices$extercat2, exclude = NULL)
table(hprices$Exterior2nd, exclude = NULL)
hprices$extercat2 = as.factor(hprices$extercat2)

#### MasVnrType -- **has 8 missings in train and 16 in test** ####
hprices$Masncat = NULL
hprices$Masncat[hprices$MasVnrType == 'None']= 1
hprices$Masncat[hprices$MasVnrType == 'BrkFace'| hprices$MasVnrType == 'BrkCmn'] = 2
hprices$Masncat[hprices$MasVnrType == 'Stone'] = 3
table(hprices$Masncat, exclude = NULL)
table(hprices$MasVnrType, exclude = NULL)
hprices$Masncat = as.factor(hprices$Masncat)

#### ExterQual -- No missing ####
hprices$exterQdum = NULL
hprices$exterQdum[hprices$ExterQual == 'Ex'|hprices$ExterQual == 'Gd']= 1
hprices$exterQdum[hprices$ExterQual == 'TA'|hprices$ExterQual == 'Fa']= 0
table(hprices$exterQdum, exclude = NULL)
table(hprices$ExterQual, exclude = NULL)
hprices$exterQdum = as.factor(hprices$exterQdum)

#### ExterCond -- No missing ####
hprices$exterCdum = NULL
hprices$exterCdum[hprices$ExterCond == 'Ex'|hprices$ExterCond == 'Gd']= 1
hprices$exterCdum[hprices$ExterCond == 'TA'|hprices$ExterCond == 'Fa'|hprices$ExterCond == 'Po' ]= 0
table(hprices$exterCdum, exclude = NULL)
table(hprices$ExterCond, exclude = NULL)
hprices$exterCdum = as.factor(hprices$exterCdum)

#### Foundation -- No missing ####
hprices$foundcat = NULL
hprices$foundcat[hprices$Foundation == 'PConc'] = 1
hprices$foundcat[hprices$Foundation == 'CBlock']= 2
hprices$foundcat[hprices$Foundation == 'BrkTil' | hprices$Foundation == 'Slab' |
                   hprices$Foundation == 'Stone' | hprices$Foundation == 'Wood']= 3
table(hprices$foundcat, exclude = NULL)
table(hprices$Foundation, exclude = NULL)
hprices$foundcat = as.factor(hprices$foundcat)

#### BsmtQual -- All missings for no basement were recoded. **44 in test.** ####
hprices$bsmtQdum = NULL
hprices$bsmtQdum[hprices$BsmtQual == 'Ex' | hprices$BsmtQual == 'Gd'] = 1
hprices$bsmtQdum[hprices$BsmtQual == 'Fa' | hprices$BsmtQual == 'TA'] = 2
hprices$bsmtQdum[is.na(hprices$BsmtQual) & is.na(hprices$BsmtCond) & 
                         is.na(hprices$BsmtExposure) & is.na(hprices$BsmtFinType1) &
                         is.na(hprices$BsmtFinType2)] = 0
table(hprices$bsmtQdum, exclude = NULL)
table(hprices$BsmtQual, exclude = NULL)
hprices$bsmtQdum = as.factor(hprices$bsmtQdum)

#### BsmtCond -- Make binary. All missing values for no basement were recoded. **45 in test.** ####
table(hprices$BsmtCond, exclude = NULL)
hprices$bsmtCcat = NULL
hprices$bsmtCcat[hprices$BsmtCond == 'Gd'] = 1
hprices$bsmtCcat[hprices$BsmtCond == 'TA'] = 2
hprices$bsmtCcat[hprices$BsmtCond == 'Fa' | hprices$BsmtCond == 'Po'] = 3
hprices$bsmtCcat[is.na(hprices$BsmtQual) & is.na(hprices$BsmtCond) & 
                   is.na(hprices$BsmtExposure) & is.na(hprices$BsmtFinType1) &
                   is.na(hprices$BsmtFinType2)] = 0
table(hprices$bsmtCcat, exclude = NULL)
table(hprices$BsmtCond, exclude = NULL)
hprices$bsmtCcat = as.factor(hprices$bsmtCcat)

#### BsmtExposure -- **1 missing in train** left after no basement recodes. **44 in test.** ####
hprices$bsmtEdum = NULL
hprices$bsmtEdum[hprices$BsmtExposure == 'Av' | hprices$BsmtExposure == 'Gd'] = 1
hprices$bsmtEdum[hprices$BsmtExposure == 'Mn' | hprices$BsmtExposure == 'No'] = 2
hprices$bsmtEdum[is.na(hprices$BsmtQual) & is.na(hprices$BsmtCond) & 
                   is.na(hprices$BsmtExposure) & is.na(hprices$BsmtFinType1) &
                   is.na(hprices$BsmtFinType2)] = 0
table(hprices$bsmtEdum, exclude = NULL)
table(hprices$BsmtExposure, exclude = NULL)
hprices$bsmtEdum = as.factor(hprices$bsmtEdum)

#### BsmtFinType1 -- All missings for no basement recoded. **42 in test.** ####
hprices$bsmtFcat = NULL
hprices$bsmtFcat[hprices$BsmtFinType1 == 'GLQ'] = 1
hprices$bsmtFcat[hprices$BsmtFinType1 == 'ALQ' | hprices$BsmtFinType1 == 'Rec'] = 2
hprices$bsmtFcat[hprices$BsmtFinType1 == 'BLQ' | hprices$BsmtFinType1 == 'LwQ' | 
                   hprices$BsmtFinType1 == 'Unf'] = 3
hprices$bsmtFcat[is.na(hprices$BsmtQual) & is.na(hprices$BsmtCond) & 
                   is.na(hprices$BsmtExposure) & is.na(hprices$BsmtFinType1) &
                   is.na(hprices$BsmtFinType2)] = 0
table(hprices$bsmtFcat, exclude = NULL)
table(hprices$BsmtFinType1, exclude = NULL)
hprices$bsmtFcat = as.factor(hprices$bsmtFcat)

## BsmtFinType2 -- may want to omit, since almost all are unfinished. 
table(hprices$BsmtFinType2, exclude = NULL)

#### Heating -- No missing, but almost all are Gas A, so may want to omit. ####
hprices$heatdum = NULL
hprices$heatdum[hprices$Heating == 'GasA'] = 1
hprices$heatdum[hprices$Heating == 'Floor' | hprices$Heating == 'GasW' | 
                  hprices$Heating == 'Grav'| hprices$Heating == 'Wall' |
                  hprices$Heating == 'OthW'] = 0
table(hprices$Heating, exclude = NULL)
table(hprices$heatdum, exclude = NULL)
hprices$heatdum = as.factor(hprices$heatdum)

#### Heating QC -- No missing ####
table(hprices$HeatingQC, exclude = NULL)
hprices$heatQdum = NULL
hprices$heatQdum[hprices$HeatingQC == 'Ex'| hprices$HeatingQC == 'Gd'] = 1
hprices$heatQdum[hprices$HeatingQC == 'TA' | hprices$HeatingQC == 'Fa' | 
                   hprices$HeatingQC == 'Po'] = 0
table(hprices$heatQdum, exclude = NULL)
hprices$heatQdum = as.factor(hprices$heatQdum)

#### CentralAir -- No missings. Already dummy coded. ####
table(hprices$CentralAir, exclude = NULL)
class(hprices$CentralAir)
hprices$CentralAir = as.factor(hprices$CentralAir)

#### Electrical -- 1 missing value merged with 'other' ####
hprices$elecdum = NULL
hprices$elecdum[hprices$Electrical == 'SBrkr'] = 1
hprices$elecdum[hprices$Electrical == 'FuseA'| hprices$Electrical == 'FuseF' |
                  hprices$Electrical == 'FuseP' | hprices$Electrical == 'Mix'] = 0
table(hprices$elecdum, exclude = NULL)
table(hprices$Electrical, exclude = NULL)
hprices$elecdum = as.factor(hprices$elecdum)

#### KitchenQual -- no missings in train. **1 in test.** ####
hprices$kitchdum = NULL
hprices$kitchdum[hprices$KitchenQual == 'Ex' | hprices$KitchenQual == 'Gd'] = 1
hprices$kitchdum[hprices$KitchenQual == 'Fa'| hprices$KitchenQual == 'TA'] = 0
table(hprices$kitchdum, exclude = NULL)
table(hprices$KitchenQual, exclude = NULL)
hprices$kitchdum = as.factor(hprices$kitchdum)

#### Functional-- no missings in train. **2 in test.** ####
hprices$funcdum = NULL
hprices$funcdum[hprices$Functional == 'Typ'] = 1
hprices$funcdum[hprices$Functional == 'Maj1'| hprices$Functional == 'Maj2' |
                  hprices$Functional == 'Min1' | hprices$Functional == 'Min2' |
                  hprices$Functional == 'Mod' | hprices$Functional == 'Sev'] = 0
table(hprices$funcdum, exclude = NULL)
table(hprices$Functional, exclude = NULL)
hprices$funcdum = as.factor(hprices$funcdum)

#### FireplaceQu -- 690 missings for no fireplace recoded. Dropped from PD file. ####
hprices$firecat = NULL
hprices$firecat[hprices$FireplaceQu == 'Ex'| hprices$FireplaceQu == 'Gd'] = 1
hprices$firecat[hprices$FireplaceQu == 'Fa'| hprices$FireplaceQu == 'TA'| 
                  hprices$FireplaceQu == 'Po'] = 2
hprices$firecat[is.na(hprices$FireplaceQu)] = 0
table(hprices$firecat, exclude = NULL)
table(hprices$FireplaceQu, exclude = NULL)
hprices$firecat = as.factor(hprices$firecat)

#### GarageType -- 81 missings for no garage have been recoded. **76 in test.** ####
hprices$garTcat = NULL
hprices$garTcat[hprices$GarageType == 'Attchd'] = 1
hprices$garTcat[hprices$GarageType == 'Detchd'] = 2
hprices$garTcat[hprices$GarageType == '2Types'|hprices$GarageType == 'Basment'|
                  hprices$GarageType == 'BuiltIn' |hprices$GarageType == 'CarPort'] = 3
hprices$garTcat[is.na(hprices$GarageType)] = 0
table(hprices$garTcat, exclude = NULL)
table(hprices$GarageType, exclude = NULL)
hprices$garTcat = as.factor(hprices$garTcat)

#### GarageFinish -- 81 missings for no garage have been recoded. **78 in test.** ####
table(hprices$GarageFinish, exclude = NULL)
hprices$garFcat = NULL
hprices$garFcat[hprices$GarageFinish == 'Fin'] = 1
hprices$garFcat[hprices$GarageFinish == 'RFn'] = 2
hprices$garFcat[hprices$GarageFinish == 'Unf'] = 3
hprices$garFcat[is.na(hprices$GarageFinish)]  = 0
table(hprices$garFcat, exclude = NULL)
hprices$garFcat = as.factor(hprices$garFcat)

#### GarageQual -- 81 missings for no garage have been recoded. **78 in test.** ####
hprices$garQcat = NULL
hprices$garQcat[hprices$GarageQual == 'Ex'| hprices$GarageQual == 'Gd'] = 1
hprices$garQcat[hprices$GarageQual == 'TA'] = 2
hprices$garQcat[hprices$GarageQual == 'Fa' | hprices$GarageQual == 'Po'] = 3
hprices$garQcat[is.na(hprices$GarageQual)]  = 0
table(hprices$garQcat, exclude = NULL)
table(hprices$GarageQual, exclude = NULL)
hprices$garQcat = as.factor(hprices$garQcat)

#### GarageCond -- 81 missings for no garage have been recoded. **78 in test** ####
hprices$garCcat = NULL
hprices$garCcat[hprices$GarageCond == 'Ex'| hprices$GarageCond == 'Gd'] = 1
hprices$garCcat[hprices$GarageCond == 'TA'] = 2
hprices$garCcat[hprices$GarageCond == 'Fa'| hprices$GarageCond == 'Po'] = 3
hprices$garCcat[is.na(hprices$GarageCond)]  = 0
table(hprices$GarageCond, exclude = NULL)   
table(hprices$garCcat, exclude = NULL)
hprices$garCcat = as.factor(hprices$garCcat)

#### Paved Drive -- no missings ####
hprices$pavedum = NULL
hprices$pavedum[hprices$PavedDrive == 'Y'] = 1
hprices$pavedum[hprices$PavedDrive == 'N'| hprices$PavedDrive == 'P'] = 0
table(hprices$pavedum, exclude = NULL)   
table(hprices$PavedDrive, exclude = NULL)
hprices$pavedum = as.factor(hprices$pavedum)

#### Sale Type -- no missings in train. **1 in test.** ####
hprices$saleTdum = NULL
hprices$saleTdum[hprices$SaleType == 'WD'] = 1
hprices$saleTdum[hprices$SaleType == 'COD'| hprices$SaleType == 'Con'| 
                   hprices$SaleType == 'ConLD'| hprices$SaleType == 'ConLI'|
                   hprices$SaleType == 'ConLw'|hprices$SaleType == 'CWD' |
                   hprices$SaleType == 'New'| hprices$SaleType == 'Oth'] = 0
table(hprices$saleTdum, exclude = NULL)
table(hprices$SaleType, exclude = NULL)
hprices$saleTdum = as.factor(hprices$saleTdum)

#### Sale Condition -- no missings ####
hprices$saleCcat = NULL
hprices$saleCcat[hprices$SaleCondition == 'Normal'] = 1
hprices$saleCcat[hprices$SaleCondition == 'Abnorml'] =2
hprices$saleCcat[hprices$SaleCondition == 'Partial'] = 3
hprices$saleCcat[hprices$SaleCondition == 'AdjLand'|hprices$SaleCondition == 'Alloca'|
                   hprices$SaleCondition == 'Family'] = 4
table(hprices$saleCcat, exclude = NULL)
table(hprices$SaleCondition, exclude = NULL)
hprices$saleCcat = as.factor(hprices$saleCcat)
                   
#### MSSubClass -- no missings ####
hprices$MSSubcat = NULL
hprices$MSSubcat[hprices$MSSubClass == '20' | hprices$MSSubClass == '30'|
                   hprices$MSSubClass == '40'|hprices$MSSubClass == '45'|
                   hprices$MSSubClass == '50'] = 1
hprices$MSSubcat[hprices$MSSubClass == '60'|hprices$MSSubClass == '70'|
                   hprices$MSSubClass == '75'] = 2
hprices$MSSubcat[hprices$MSSubClass == '80'| hprices$MSSubClass == '85'|
                   hprices$MSSubClass == '90'] = 3
hprices$MSSubcat[hprices$MSSubClass == '120'| hprices$MSSubClass == '160'|
                  hprices$MSSubClass == '180'|hprices$MSSubClass == '190'] = 4
table(hprices$MSSubcat, exclude = NULL)
table(hprices$MSSubClass, exclude = NULL)
hprices$MSSubcat = as.factor(hprices$MSSubcat)


### Create dummy variables for Alley access, Pool, Fence and Fireplace -- present Y/N

#### Alley access -- mostly missings have been recoded to a binary variable.####
hprices$alleydum = NULL
hprices$alleydum[hprices$Alley == 'Grvl' | hprices$Alley == 'Pave'] = 1
hprices$alleydum[is.na(hprices$Alley)] = 0
table(hprices$alleydum, exclude = NULL)
table(hprices$Alley, exclude = NULL)
hprices$alleydum = as.factor(hprices$alleydum)

t.test(hprices$SalePrice ~ hprices$alleydum, data = hprices) ##there is a diff. in mean SP by Alley groups

#### Pool -- mostly missing, was recoded to a binary variable ####
## No significant difference in groups. Consider dropping. ##
hprices$pooldum = NULL
hprices$pooldum[hprices$PoolQC == 'Ex' | hprices$PoolQC == 'Fa'| 
                  hprices$PoolQC == 'Gd' ] = 1
hprices$pooldum[is.na(hprices$PoolQC)] = 0
table(hprices$pooldum, exclude = NULL)
table(hprices$PoolQC, exclude = NULL)
hprices$pooldum = as.factor(hprices$pooldum)
t.test(hprices$SalePrice ~ hprices$pooldum, data = hprices) ##there is a diff. in mean SP by Alley groups

#### Fence -- Mostly missing, was recoded to a binary variable ####
hprices$fencedum = NULL
hprices$fencedum[hprices$Fence == 'GdPrv' | hprices$Fence == 'GdWo'| 
                  hprices$Fence == 'MnPrv'| hprices$Fence == 'MnWw'] = 1
hprices$fencedum[is.na(hprices$Fence)] = 0
table(hprices$fencedum, exclude = NULL)
table(hprices$Fence, exclude = NULL)
hprices$fencedum = as.factor(hprices$fencedum)
t.test(hprices$SalePrice ~ hprices$fencedum, data = hprices) ##there is a diff. in mean SP by Alley groups

#### Fireplace -- a lot of missings, was recoded to a binary variable ####
hprices$fireYN = NULL
hprices$fireYN[hprices$FireplaceQu == 'Ex' | hprices$FireplaceQu == 'Fa'| 
                   hprices$FireplaceQu == 'Gd'| hprices$FireplaceQu == 'Po'|
                 hprices$FireplaceQu == 'TA'] = 1
hprices$fireYN[is.na(hprices$FireplaceQu)] = 0
table(hprices$fireYN, exclude = NULL)
table(hprices$FireplaceQu, exclude = NULL)
hprices$fireYN = as.factor(hprices$fireYN)
t.test(hprices$SalePrice ~ hprices$fireYN, data = hprices) ##there is a diff. in mean SP by Alley groups

#### Neighborhood -- convert to factor ####
hprices$Neighborhood = as.factor(hprices$Neighborhood)
table(hprices$Neighborhood, exclude = NULL)




#### Select out the recoded columns ####
#from the hprices dataframe
hprices2 = select(hprices, Id, 
                  Lsaleprice, Ltotbsmtsf, L1stflrSF, Lgrlivarea, 
                  Lgaragearea, durbuilt, durremod, hqualcat, fullbathcat, 
                  totrmscat, garcarcat, zonenew, Lotshdum, Lotcondum, Cond1dum, 
                  Cond2dum, bldgdum, housedum, roofsdum, roofmdum, extercat,
                  extercat2, Masncat, exterQdum, exterCdum, foundcat, bsmtQdum, 
                  bsmtCcat, bsmtEdum, bsmtFcat, heatdum, heatQdum, CentralAir, 
                  elecdum, kitchdum, funcdum, firecat, garTcat, garFcat, garQcat, 
                  garCcat, pavedum, saleTdum, saleCcat, MSSubcat, alleydum,
                  fencedum, fireYN, Neighborhood, Ltothousearea)


sapply(hprices2, class)

nrow(hprices2)
dim(hprices2)

#### Do KNN impute on all missing values ####
library(VIM)
library(mice)
library(caret)
library(Hmisc)
library(rpart)
library(DMwR)
library(dvmisc)
library(MASS) #The Modern Applied Statistics library.

##KNN imputation
prices_imputed = knnImputation(hprices2[, !names(hprices2) %in% "LSaleprice"]) 
anyNA(prices_imputed)

write.csv(prices_imputed, file='new_train.csv', row.names=F)


#### Run a multiple linear regression on the imputed data and do forward AIC ####

model.full = lm(Lsaleprice ~ ., data = prices_imputed)
summary(model.full)

plot(model.full)
influencePlot(model.full)
vif(model.full)
alias( lm(Lsaleprice ~ ., data = prices_imputed ) )
ld.vars <- attributes(alias(model.full)$Complete)$dimnames[[1]]
ld.vars



#We can use stepwise regression to help automate the variable selection process.
#Here we define the minimal model, the full model, and the scope of the models
#through which to search:
model.empty = lm(Lsaleprice ~ 1, data = prices_imputed) #The model with an intercept ONLY.
model.full = lm(Lsaleprice ~ ., data = prices_imputed) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)


# model.full = lm(Lsaleprice ~ hqualcat + Lgrlivarea + Neighborhood + bsmtFcat + 
#   garcarcat + CentralAir + Ltotbsmtsf + durremod + saleCcat + 
#   funcdum + bldgdum + bsmtEdum + bsmtCcat + fireYN + Cond1dum + 
#   garQcat + fullbathcat + Cond2dum + garFcat + heatQdum + L1stflrSF + 
#   pavedum + heatdum + Lotshdum + roofsdum + kitchdum + housedum + 
#   MSSubcat + foundcat + exterCdum + bsmtQdum, data = prices_imputed)

model.full = lm(Lsaleprice ~ Ltothousearea + Neighborhood + hqualcat + CentralAir + 
                  Lgrlivarea + durremod + bsmtFcat + garcarcat + saleCcat + 
                  funcdum + Ltotbsmtsf + bldgdum + bsmtEdum + bsmtCcat + fireYN + 
                  Cond1dum + garQcat + fullbathcat + Cond2dum + garFcat + heatQdum + 
                  pavedum + heatdum + roofsdum + Lotshdum + kitchdum + exterCdum + 
                  bsmtQdum + foundcat + L1stflrSF, data = prices_imputed)

summary(model.full)
get_mse(model.full, var.estimate = FALSE) ##MSE = 0.01782624

vif(model.full)
ld.vars <- attributes(alias(model.full)$Complete)$dimnames[[1]]
ld.vars


#### Regularized regression and cross validation ####
x = model.matrix(Lsaleprice ~ ., data=prices_imputed)[, -1] #Dropping the intercept column.
y = prices_imputed$Lsaleprice

#Values of lambda over which to check.
grid = 10^seq(5, -2, length = 100)

##### Fitting the ridge regression. Alpha = 0 for ridge regression. ####
library(glmnet)
ridge.models = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.models))
coef(ridge.models)

#What do the estimates look like for a smaller value of lambda?
ridge.models$lambda[100] #Lambda = 0.01.
coef(ridge.models)[, 100] #Estimates are closer to 0.
sqrt(sum(coef(ridge.models)[-1, 100]^2)) #L2 norm is 0.7546783.

ridge.models$lambda[80] #Lambda = 0.2595.
coef(ridge.models)[, 80] #Estimates are closer to 0.
sqrt(sum(coef(ridge.models)[-1, 80]^2)) #L2 norm is 0.475559.

#What do the estimates look like for a larger value of lambda?
ridge.models$lambda[15] #Lambda = 10,235.31.
coef(ridge.models)[, 15] #Most estimates are different from 0.
sqrt(sum(coef(ridge.models)[-1, 15]^2)) #L2 norm is 0.0001413462

ridge.models$lambda[5] #Lambda = 52140.08.
coef(ridge.models)[, 5] #Most estimates are different from 0.
sqrt(sum(coef(ridge.models)[-1, 5]^2)) #L2 norm is 2.77565e-05

#Visualizing the ridge regression shrinkage.
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

#Creating training and testing sets. Here we decide to use a 70-30 split with
#approximately 70% of our data in the training set and 30% of our data in the
#test set.
set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.test = y[test]

length(train)/nrow(x)
length(y.test)/nrow(x)


#Fit a ridge regression using a lambda of 5. We will now use the training set exclusively.
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge.lambda5 = predict(ridge.models.train, s = 5, newx = x[test, ])
mean((ridge.lambda5 - y.test)^2) ## MSE = 0.0760893

#What would happen if we fit a ridge regression with an extremely large value
#of lambda? Essentially, fitting a model with only an intercept:
ridge.largelambda = predict(ridge.models.train, s = 1e10, newx = x[test, ])
mean((ridge.largelambda - y.test)^2) ## MSE = 0.1730444

#Running 10-fold cross validation.
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge ##0.05094138
log(bestlambda.ridge) #-2.97708

#What is the test MSE associated with this best value of lambda?
ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2) ##MSE = 0.0264338

##Getting the best lambda and associated MSE in a different way.
ridge.bestlambdatrain = predict.cv.glmnet(cv.ridge.out, s ="lambda.min", newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2) ##MSE = 0.0264338

### Alternative method with caret
library(caret)
set.seed(0)
train_control = trainControl(method = 'cv', number=10)
tune.grid = expand.grid(lambda = grid, alpha=c(0))
ridge.caret = train(x[train, ], y[train],
                    method = 'glmnet',
                    trControl = train_control, tuneGrid = tune.grid)

### Plot the tuning object:
plot(ridge.caret, xTrans=log)

### Predicting with the final model
pred = predict.train(ridge.caret, newdata = x[test,])
mean((pred - y[test])^2)  ##MSE = 0.02646041


#### Fit a lasso regression ####
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)
dim(coef(lasso.models))
coef(lasso.models)

#Visualizing the lasso regression shrinkage.
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

#Running the lasso on an arbitrary lambda of 5
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.lambda5 = predict(lasso.models.train, s = 5, newx = x[test, ])
mean((lasso.lambda5 - y.test)^2) ## MSE = 0.1730567


#Running 10-fold cross validation.
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso #0.01
log(bestlambda.lasso) #-4.60517

#What is the test MSE associated with this best value of lambda?
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2) ##MSE = 0.0258614


#### Elastic-Net regression ####

# Build the model using the training set and 10 fold CV
set.seed(0)
net.model.train = train(x[train, ], y[train], method = "glmnet",
                        trControl = trainControl("cv", number = 10),tuneLength = 10)
# Best tuning parameter
bestlambdanet = net.model.train$bestTune 

# alpha     lambda
# 0.1    0.01955313

## Obtain the coefficients of the model
coef(net.model.train$finalModel, net.model.train$bestTune$lambda)

##Obtain the MSE
net.bestlambdatrain = predict(net.model.train, s = bestlambdanet, newx = x[test, ])
mean((net.bestlambdatrain - y.test)^2) ##MSE = 0.3132118
#Rsquare = R2(net.bestlambdatrain)


