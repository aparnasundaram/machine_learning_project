## This file recodes the different variables in the test dataset 
## before includng them in the regression model
## Created by Aparna Sundaram on 08/21/2019


## Load libraries
library(tidyr)
library(ggplot2)
library(corrr)
library(DescTools)
library(car)
library(gmodels)
library(dplyr)

## Read in dataset

tprices = read.csv('test.csv', stringsAsFactors = F)
nrow(tprices)

###--------------------------------------### 
### Manipulate the different X variables ###
###--------------------------------------###

#### Continuous variable recodes ####

#### TotalBsmtSF -- No missing in train. **1 in test.** ####
#create a new variable that is a duplicate of the total Basement SF
tprices$TotalBsmtSF2 = tprices$TotalBsmtSF

#set the basement SF value of zero to 1 to enable logging.
tprices$TotalBsmtSF2[tprices$TotalBsmtSF2 == 0] = 1

#Log the variable
tprices$Ltotbsmtsf = log(tprices$TotalBsmtSF2)

#The following code is redundant
#tprices$Ltotbsmtsf[tprices$TotalBsmtSF == 0] = 0

summary(tprices$TotalBsmtSF)
mean(tprices$TotalBsmtSF, na.rm=T)
sd(tprices$TotalBsmtSF, na.rm=T)
min(tprices$Ltotbsmtsf, na.rm=T)
max(tprices$Ltotbsmtsf, na.rm=T)

avg1 = mean(tprices$Ltotbsmtsf, na.rm=T) ## this is better.
sd31 = mean(tprices$Ltotbsmtsf, na.rm=T) + 3*sd(tprices$Ltotbsmtsf, na.rm = T) ##didn't work quite so well.

table(tprices$TotalBsmtSF, exclude = NULL)
table(tprices$Ltotbsmtsf, exclude = NULL)

#Finding outliers in totalbasement SF -- ignore the zeros.
outlier1=Outlier(tprices$Ltotbsmtsf, na.rm = T)
outlier1
outlier1= outlier1[outlier1>0]

#Convert the non-zero outlier values to avg1. 
tprices$Ltotbsmtsf[tprices$Ltotbsmtsf %in% outlier1] = avg1

nrow(tprices)

## Code for checking the log value = -Inf.
outbsmtsf = tprices %>% 
  select(Ltotbsmtsf, TotalBsmtSF) %>% 
  filter(Ltotbsmtsf == -Inf)
outbsmtsf


#### X1stFlrSF -- No missing ####
tprices$L1stflrSF = log(tprices$X1stFlrSF)

mean(tprices$L1stflrSF)
sd(tprices$L1stflrSF)
min(tprices$L1stflrSF)
max(tprices$L1stflrSF)

avg2 = mean(tprices$L1stflrSF) ## this is better.
sd32 = mean(tprices$L1stflrSF) + 3*sd(tprices$L1stflrSF) ##didn't work quite so well.
outlier2 =Outlier(tprices$L1stflrSF)
outlier2
#Convert the non-zero outlier values to avg1. 
tprices$L1stflrSF[tprices$L1stflrSF %in% outlier2] = avg2

table(tprices$X1stFlrSF, exclude = NULL)
summary(tprices$X1stFlrSF)
summary(tprices$L1stflrSF)

nrow(tprices)

#### GrLivArea -- No missing ####
tprices$Lgrlivarea = log(tprices$GrLivArea)

mean(tprices$Lgrlivarea)
sd(tprices$Lgrlivarea)
min(tprices$Lgrlivarea)
max(tprices$Lgrlivarea)

avg3= mean(tprices$Lgrlivarea) ## this is better.
sd33 = mean(tprices$Lgrlivarea) + 3*sd(tprices$Lgrlivarea) ##didn't work quite so well.
outlier3 =Outlier(tprices$Lgrlivarea)
outlier3
tprices$Lgrlivarea[tprices$Lgrlivarea %in% outlier3] = avg3

table(tprices$GrLivArea, exclude = NULL)
summary(tprices$GrLivArea)
summary(tprices$Lgrlivarea)

nrow(tprices)

#### GarageArea -- No missing in train. **1 in test.** ####
tprices$Lgaragearea = log(tprices$GarageArea)

#set all -inf to zero
tprices$Lgaragearea[tprices$GarageArea == 0] = 0

mean(tprices$Lgaragearea, na.rm=T)
sd(tprices$Lgaragearea, na.rm = T)
min(tprices$Lgaragearea, na.rm = T)
max(tprices$Lgaragearea, na.rm = T)

avg4 = mean(tprices$Lgaragearea, na.rm=T)
sd34 = mean(tprices$Lgaragearea, na.rm=T) + 3*sd(tprices$Lgaragearea, na.rm = T) ##didn't work quite so well.

outlier4 =Outlier(tprices$Lgaragearea, na.rm = T)
outlier4
outlier4= outlier4[outlier4>0]

#Convert the non-zero outlier values to avg4. 
tprices$Lgaragearea[tprices$Lgaragearea %in% outlier4] = avg4

table(tprices$GarageArea, exclude = NULL)
summary(tprices$GarageArea)
summary(tprices$Lgaragearea)

#### YearBuilt -- Changed to years between built and sold. No missings ####
##No missing
table(tprices$YrSold)
tprices$durbuilt = tprices$YrSold - tprices$YearBuilt

##change all -1s to 0. House was purchased before it was fully built.
tprices$durbuilt[tprices$durbuilt < 0] = 0
table(tprices$durbuilt)

nrow(tprices)

#### YearRemodAdd -- Changed to years between remodeled and sold. No missings ####
## No missing
table(tprices$YearRemodAdd)
tprices$durremod = tprices$YrSold - tprices$YearRemodAdd
table(tprices$durremod)
check1 = tprices[tprices$durremod < 0,] ##House was remodeled after purchase.
check1
tprices$durremod[tprices$durremod < 0] = 0 #Change all negatives to 0.
table(tprices$durremod)

nrow(tprices)

#### Categorical variable recodes.####

#### OverallQual -- No missing ####
tprices$OverallQual = as.factor(tprices$OverallQual)
tprices$hqualcat = NULL
tprices$hqualcat[tprices$OverallQual == 1 | tprices$OverallQual == 2 |
                   tprices$OverallQual == 3 | tprices$OverallQual == 4] = 1
tprices$hqualcat[tprices$OverallQual == 5] = 2
tprices$hqualcat[tprices$OverallQual == 6] = 3
tprices$hqualcat[tprices$OverallQual == 7] = 4
tprices$hqualcat[tprices$OverallQual == 8 | tprices$OverallQual == 9 |
                   tprices$OverallQual == 10] = 5
table(tprices$hqualcat, exclude = NULL)
table(tprices$OverallQual, exclude = NULL)
tprices$hqualcat = as.factor(tprices$hqualcat)

nrow(tprices)

#### FullBath -- No missing ####
tprices$FullBath = as.factor(tprices$FullBath)
tprices$fullbathcat = NULL
tprices$fullbathcat[tprices$FullBath == 0] = 0
tprices$fullbathcat[tprices$FullBath == 1] = 1
tprices$fullbathcat[tprices$FullBath == 2] = 2
tprices$fullbathcat[tprices$FullBath == 3 | tprices$FullBath == 4] = 3
table(tprices$FullBath, exclude = NULL)
table(tprices$fullbathcat, exclude = NULL)
tprices$fullbathcat = as.factor(tprices$fullbathcat)

nrow(tprices)

#### TotRmsAbvGrd -- No missing ####
tprices$TotRmsAbvGrd = as.factor(tprices$TotRmsAbvGrd)
tprices$totrmscat = NULL
tprices$totrmscat[tprices$TotRmsAbvGrd == 1 | tprices$TotRmsAbvGrd == 2 |
                    tprices$TotRmsAbvGrd == 3 | tprices$TotRmsAbvGrd == 4] = 1
tprices$totrmscat[tprices$TotRmsAbvGrd == 5] = 2
tprices$totrmscat[tprices$TotRmsAbvGrd == 6] = 3
tprices$totrmscat[tprices$TotRmsAbvGrd == 7] = 4
tprices$totrmscat[tprices$TotRmsAbvGrd == 8 | tprices$TotRmsAbvGrd == 9 |
                    tprices$TotRmsAbvGrd == 10 | tprices$TotRmsAbvGrd == 11|
                    tprices$TotRmsAbvGrd == 12 | tprices$TotRmsAbvGrd == 13 |
                    tprices$TotRmsAbvGrd == 14 | tprices$TotRmsAbvGrd == 15] = 5
table(tprices$totrmscat, exclude = NULL)
table(tprices$TotRmsAbvGrd, exclude = NULL)
tprices$totrmscat = as.factor(tprices$totrmscat)

#### Garage Cars -- No missing in train. **1 in test.** ####
tprices$GarageCars = as.factor(tprices$GarageCars)
tprices$garcarcat = NULL
tprices$garcarcat[tprices$GarageCars == 1] = 1
tprices$garcarcat[tprices$GarageCars == 2] = 2
tprices$garcarcat[tprices$GarageCars == 3 | tprices$GarageCars == 4 | 
                    tprices$GarageCars == 5] = 3
tprices$garcarcat[tprices$GarageCars == 0] = 0
table(tprices$garcarcat, exclude = NULL)
table(tprices$GarageCars, exclude = NULL)
tprices$garcarcat = as.factor(tprices$garcarcat)

#### MSZoning. No missing in train. **4 in test** ####
## -- created binary RL vs. others. --
tprices$zonenew = NULL
tprices$zonenew[tprices$MSZoning == "RL"] = 1
tprices$zonenew[tprices$MSZoning == "C (all)" | tprices$MSZoning == "FV" |
                  tprices$MSZoning == "RH" | tprices$MSZoning == "RM"] = 0
table(tprices$zonenew, exclude = NULL)
table(tprices$MSZoning, exclude = NULL)
tprices$zonenew = as.factor(tprices$zonenew)

nrow(tprices)
dim(tprices)

#### LotShape -- No missing ####
tprices$Lotshdum = NULL
tprices$Lotshdum[tprices$LotShape == 'Reg'] = 1
tprices$Lotshdum[tprices$LotShape == 'IR1'| tprices$LotShape == 'IR2' | 
                   tprices$LotShape == 'IR3'] = 0
table(tprices$Lotshdum, exclude = NULL)
table(tprices$LotShape, exclude = NULL)
tprices$Lotshdum = as.factor(tprices$Lotshdum)

#### LandContour -- No missing ####
tprices$Lotcondum = NULL
tprices$Lotcondum[tprices$LandContour == 'Lvl'] = 1
tprices$Lotcondum[tprices$LandContour == 'Bnk' | tprices$LandContour == 'HLS' |
                    tprices$LandContour == 'Low'] = 0
table(tprices$Lotcondum, exclude = NULL)
table(tprices$LandContour, exclude = NULL)
tprices$Lotcondum = as.factor(tprices$Lotcondum)

#### Condition1 -- No missing ####
tprices$Cond1dum = NULL
tprices$Cond1dum[tprices$Condition1 == 'Norm'] = 1
tprices$Cond1dum[tprices$Condition1 == 'RRAe' | tprices$Condition1 == 'RRAn' |
                   tprices$Condition1 == 'RRNe'| tprices$Condition1 == 'RRNn'] = 2
tprices$Cond1dum[tprices$Condition1 == 'Artery' | tprices$Condition1 == 'Feedr'] = 3
tprices$Cond1dum[tprices$Condition1 == 'PosA'| tprices$Condition1 == 'PosN'] = 4
table(tprices$Cond1dum, exclude = NULL)
table(tprices$Condition1, exclude = NULL)
tprices$Cond1dum = as.factor(tprices$Cond1dum)

nrow(tprices)
dim(tprices)

#### Condition2 -- No missing ####
tprices$Cond2dum = NULL
tprices$Cond2dum[tprices$Condition2 == 'Norm'] = 1
tprices$Cond2dum[tprices$Condition2 == 'RRAe'|tprices$Condition2 == 'RRAn'| 
                   tprices$Condition2 == 'RRNn'| tprices$Condition2 == 'RRNn'] = 2
tprices$Cond2dum[tprices$Condition2 == 'Artery'|tprices$Condition2 == 'Feedr'] = 3
tprices$Cond2dum[tprices$Condition2 == 'PosA'|tprices$Condition2 == 'PosN'] = 4
table(tprices$Cond2dum, exclude = NULL)
table(tprices$Condition2, exclude = NULL)
tprices$Cond2dum = as.factor(tprices$Cond2dum)

# Condition -- needs more thought and may not be needed if only 1 is chosen.
CrossTable(tprices$Condition1, tprices$Condition2) ##Gives SAS like crosstab.
xtabs(~Condition1 + Condition2, data=tprices)

#### BldgType -- No missing ####
tprices$bldgdum = NULL
tprices$bldgdum[tprices$BldgType == '1Fam'] = 1
tprices$bldgdum[tprices$BldgType == '2fmCon' | tprices$BldgType == 'Duplex' |
                  tprices$BldgType == 'Twnhs' | tprices$BldgType == 'TwnhsE'] = 0
table(tprices$bldgdum, exclude = NULL)
table(tprices$BldgType, exclude = NULL)
tprices$bldgdum = as.factor(tprices$bldgdum)

#### HouseStyle -- No missing ####
tprices$housedum = NULL
tprices$housedum[tprices$HouseStyle == '1Story']= 1
tprices$housedum[tprices$HouseStyle == '1.5Fin' | tprices$HouseStyle == '1.5Unf']= 2
tprices$housedum[tprices$HouseStyle == '2.5Unf'| tprices$HouseStyle == '2.5Fin'|
                   tprices$HouseStyle == '2Story']= 3
tprices$housedum[tprices$HouseStyle == 'SFoyer' | tprices$HouseStyle == 'SLvl']= 4
table(tprices$housedum)
table(tprices$HouseStyle)
tprices$housedum = as.factor(tprices$housedum)

#### Roofstyle -- No missing ####
tprices$roofsdum = NULL
tprices$roofsdum[tprices$RoofStyle == 'Gable']= 1
tprices$roofsdum[tprices$RoofStyle == 'Hip']= 0
tprices$roofsdum[tprices$RoofStyle == 'Gambrel'|tprices$RoofStyle == 'Mansard' | 
                   tprices$RoofStyle == 'Shed' | tprices$RoofStyle == 'Flat' ]= 0
table(tprices$roofsdum)
table(tprices$RoofStyle)
tprices$roofsdum = as.factor(tprices$roofsdum)

#### RoofMatl -- No missing ##### 
## include for now, but may need to rethink given the lack of balance in freq.

tprices$roofmdum = NULL
tprices$roofmdum[tprices$RoofMatl == 'CompShg']= 1
tprices$roofmdum[tprices$RoofMatl == 'ClyTile'|tprices$RoofMatl == 'Membran' | 
                   tprices$RoofMatl == 'Metal' | tprices$RoofMatl == 'Roll' |
                   tprices$RoofMatl == 'Tar&Grv' | tprices$RoofMatl == 'WdShake' |
                   tprices$RoofMatl == 'WdShngl']= 0
table(tprices$roofmdum, exclude = NULL)
table(tprices$RoofMatl, exclude = NULL)
tprices$roofmdum = as.factor(tprices$roofmdum)

nrow(tprices)
dim(tprices)

#### Exterior 1st -- No missing in train. **1 in test.**####
tprices$extercat = NULL
tprices$extercat[tprices$Exterior1st == 'VinylSd']= 1
tprices$extercat[tprices$Exterior1st == 'HdBoard'] = 2
tprices$extercat[tprices$Exterior1st == 'MetalSd'] = 3
tprices$extercat[tprices$Exterior1st == 'Wd Sdng'] = 4
tprices$extercat[tprices$Exterior1st == 'Plywood'] = 5
tprices$extercat[tprices$Exterior1st == 'AsbShng' | tprices$Exterior1st == 'AsphShn'|
                   tprices$Exterior1st == 'BrkComm'|tprices$Exterior1st =='BrkFace'|
                   tprices$Exterior1st == 'CBlock'|tprices$Exterior1st =='CemntBd'|
                   tprices$Exterior1st == 'ImStucc'|tprices$Exterior1st =='Stone'|
                   tprices$Exterior1st == 'Stucco'|tprices$Exterior1st =='WdShing'] = 6
table(tprices$extercat, exclude = NULL)
table(tprices$Exterior1st, exclude = NULL)
tprices$extercat = as.factor(tprices$extercat)

#### Exterior 2nd -- No missing in train. **1 in test.** ####
xtabs(~Exterior1st + Exterior2nd, data=tprices)
CrossTable(tprices$Exterior1st, tprices$Exterior2nd) ##Gives SAS like crosstab.
tprices$extercat2 = NULL
tprices$extercat2[tprices$Exterior2nd == 'VinylSd']= 1
tprices$extercat2[tprices$Exterior2nd == 'HdBoard'] = 2
tprices$extercat2[tprices$Exterior2nd == 'MetalSd'] = 3
tprices$extercat2[tprices$Exterior2nd == 'Wd Sdng'] = 4
tprices$extercat2[tprices$Exterior2nd == 'Plywood'] = 5
tprices$extercat2[tprices$Exterior2nd == 'AsbShng' | tprices$Exterior2nd == 'AsphShn'|
                    tprices$Exterior2nd == 'Brk Cmn'|tprices$Exterior2nd =='BrkFace'|
                    tprices$Exterior2nd == 'CBlock'|tprices$Exterior2nd =='CmentBd'|
                    tprices$Exterior2nd == 'ImStucc'|tprices$Exterior2nd =='Stone'|
                    tprices$Exterior2nd == 'Stucco'|tprices$Exterior2nd =='Wd Shng'|
                    tprices$Exterior2nd == 'Other'] = 6
table(tprices$extercat2, exclude = NULL)
table(tprices$Exterior2nd, exclude = NULL)
tprices$extercat2 = as.factor(tprices$extercat2)

#### MasVnrType -- **has 8 missings in train and 16 in test** ####
tprices$Masncat = NULL
tprices$Masncat[tprices$MasVnrType == 'None']= 1
tprices$Masncat[tprices$MasVnrType == 'BrkFace'| tprices$MasVnrType == 'BrkCmn'] = 2
tprices$Masncat[tprices$MasVnrType == 'Stone'] = 3
table(tprices$Masncat, exclude = NULL)
table(tprices$MasVnrType, exclude = NULL)
tprices$Masncat = as.factor(tprices$Masncat)

#### ExterQual -- No missing ####
tprices$exterQdum = NULL
tprices$exterQdum[tprices$ExterQual == 'Ex'|tprices$ExterQual == 'Gd']= 1
tprices$exterQdum[tprices$ExterQual == 'TA'|tprices$ExterQual == 'Fa']= 0
table(tprices$exterQdum, exclude = NULL)
table(tprices$ExterQual, exclude = NULL)
tprices$exterQdum = as.factor(tprices$exterQdum)

#### ExterCond -- No missing ####
tprices$exterCdum = NULL
tprices$exterCdum[tprices$ExterCond == 'Ex'|tprices$ExterCond == 'Gd']= 1
tprices$exterCdum[tprices$ExterCond == 'TA'|tprices$ExterCond == 'Fa'|tprices$ExterCond == 'Po' ]= 0
table(tprices$exterCdum, exclude = NULL)
table(tprices$ExterCond, exclude = NULL)
tprices$exterCdum = as.factor(tprices$exterCdum)

#### Foundation -- No missing ####
tprices$foundcat = NULL
tprices$foundcat[tprices$Foundation == 'PConc'] = 1
tprices$foundcat[tprices$Foundation == 'CBlock']= 2
tprices$foundcat[tprices$Foundation == 'BrkTil' | tprices$Foundation == 'Slab' |
                   tprices$Foundation == 'Stone' | tprices$Foundation == 'Wood']= 3
table(tprices$foundcat, exclude = NULL)
table(tprices$Foundation, exclude = NULL)
tprices$foundcat = as.factor(tprices$foundcat)

nrow(tprices)
dim(tprices)

#### BsmtQual -- All missings for no basement were recoded. **2 remain in test.** ####
tprices$bsmtQdum = NULL
tprices$bsmtQdum[tprices$BsmtQual == 'Ex' | tprices$BsmtQual == 'Gd'] = 1
tprices$bsmtQdum[tprices$BsmtQual == 'Fa' | tprices$BsmtQual == 'TA'] = 2
tprices$bsmtQdum[is.na(tprices$BsmtQual) & is.na(tprices$BsmtCond) & 
                   is.na(tprices$BsmtExposure) & is.na(tprices$BsmtFinType1) &
                   is.na(tprices$BsmtFinType2)] = 0
table(tprices$bsmtQdum, exclude = NULL)
table(tprices$BsmtQual, exclude = NULL)
tprices$bsmtQdum = as.factor(tprices$bsmtQdum)

#### BsmtCond -- All missing values for no basement were recoded. **3 remain in test.** ####
table(tprices$BsmtCond, exclude = NULL)
tprices$bsmtCcat = NULL
tprices$bsmtCcat[tprices$BsmtCond == 'Gd'] = 1
tprices$bsmtCcat[tprices$BsmtCond == 'TA'] = 2
tprices$bsmtCcat[tprices$BsmtCond == 'Fa' | tprices$BsmtCond == 'Po'] = 3
tprices$bsmtCcat[is.na(tprices$BsmtQual) & is.na(tprices$BsmtCond) & 
                   is.na(tprices$BsmtExposure) & is.na(tprices$BsmtFinType1) &
                   is.na(tprices$BsmtFinType2)] = 0
table(tprices$bsmtCcat, exclude = NULL)
table(tprices$BsmtCond, exclude = NULL)
tprices$bsmtCcat = as.factor(tprices$bsmtCcat)

#### BsmtExposure --After no basement recodes, **1 still missing in train and 2 in test.** ####
tprices$bsmtEdum = NULL
tprices$bsmtEdum[tprices$BsmtExposure == 'Av' | tprices$BsmtExposure == 'Gd'] = 1
tprices$bsmtEdum[tprices$BsmtExposure == 'Mn' | tprices$BsmtExposure == 'No'] = 2
tprices$bsmtEdum[is.na(tprices$BsmtQual) & is.na(tprices$BsmtCond) & 
                   is.na(tprices$BsmtExposure) & is.na(tprices$BsmtFinType1) &
                   is.na(tprices$BsmtFinType2)] = 0
table(tprices$bsmtEdum, exclude = NULL)
table(tprices$BsmtExposure, exclude = NULL)
tprices$bsmtEdum = as.factor(tprices$bsmtEdum)

#### BsmtFinType1 -- All missings for no basement recoded in both test and train. ####
tprices$bsmtFcat = NULL
tprices$bsmtFcat[tprices$BsmtFinType1 == 'GLQ'] = 1
tprices$bsmtFcat[tprices$BsmtFinType1 == 'ALQ' | tprices$BsmtFinType1 == 'Rec'] = 2
tprices$bsmtFcat[tprices$BsmtFinType1 == 'BLQ' | tprices$BsmtFinType1 == 'LwQ' | 
                   tprices$BsmtFinType1 == 'Unf'] = 3
tprices$bsmtFcat[is.na(tprices$BsmtQual) & is.na(tprices$BsmtCond) & 
                   is.na(tprices$BsmtExposure) & is.na(tprices$BsmtFinType1) &
                   is.na(tprices$BsmtFinType2)] = 0
table(tprices$bsmtFcat, exclude = NULL)
table(tprices$BsmtFinType1, exclude = NULL)
tprices$bsmtFcat = as.factor(tprices$bsmtFcat)

## BsmtFinType2 -- may want to omit, since almost all are unfinished. 
table(tprices$BsmtFinType2, exclude = NULL)

#### Heating -- No missing, but almost all are Gas A, so may want to omit. ####
tprices$heatdum = NULL
tprices$heatdum[tprices$Heating == 'GasA'] = 1
tprices$heatdum[tprices$Heating == 'Floor' | tprices$Heating == 'GasW' | 
                  tprices$Heating == 'Grav'| tprices$Heating == 'Wall' |
                  tprices$Heating == 'OthW'] = 0
table(tprices$Heating, exclude = NULL)
table(tprices$heatdum, exclude = NULL)
tprices$heatdum = as.factor(tprices$heatdum)

#### Heating QC -- No missing ####
table(tprices$HeatingQC, exclude = NULL)
tprices$heatQdum = NULL
tprices$heatQdum[tprices$HeatingQC == 'Ex'| tprices$HeatingQC == 'Gd'] = 1
tprices$heatQdum[tprices$HeatingQC == 'TA' | tprices$HeatingQC == 'Fa' | 
                   tprices$HeatingQC == 'Po'] = 0
table(tprices$heatQdum, exclude = NULL)
tprices$heatQdum = as.factor(tprices$heatQdum)

#### CentralAir -- No missings. Already dummy coded. ####
table(tprices$CentralAir, exclude = NULL)
class(tprices$CentralAir)
tprices$CentralAir = as.factor(tprices$CentralAir)

nrow(tprices)
dim(tprices)

#### Electrical -- 1 missing value merged with 'other' ####
tprices$elecdum = NULL
tprices$elecdum[tprices$Electrical == 'SBrkr'] = 1
tprices$elecdum[tprices$Electrical == 'FuseA'| tprices$Electrical == 'FuseF' |
                  tprices$Electrical == 'FuseP' | tprices$Electrical == 'Mix'] = 0
table(tprices$elecdum, exclude = NULL)
table(tprices$Electrical, exclude = NULL)
tprices$elecdum = as.factor(tprices$elecdum)

#### KitchenQual -- no missings in train. **1 in test.** ####
tprices$kitchdum = NULL
tprices$kitchdum[tprices$KitchenQual == 'Ex' | tprices$KitchenQual == 'Gd'] = 1
tprices$kitchdum[tprices$KitchenQual == 'Fa'| tprices$KitchenQual == 'TA'] = 0
table(tprices$kitchdum, exclude = NULL)
table(tprices$KitchenQual, exclude = NULL)
tprices$kitchdum = as.factor(tprices$kitchdum)

#### Functional-- no missings in train. **2 in test.** ####
tprices$funcdum = NULL
tprices$funcdum[tprices$Functional == 'Typ'] = 1
tprices$funcdum[tprices$Functional == 'Maj1'| tprices$Functional == 'Maj2' |
                  tprices$Functional == 'Min1' | tprices$Functional == 'Min2' |
                  tprices$Functional == 'Mod' | tprices$Functional == 'Sev'] = 0
table(tprices$funcdum, exclude = NULL)
table(tprices$Functional, exclude = NULL)
tprices$funcdum = as.factor(tprices$funcdum)

#### FireplaceQu -- 690 missings for no fireplace recoded. ####
tprices$firecat = NULL
tprices$firecat[tprices$FireplaceQu == 'Ex'| tprices$FireplaceQu == 'Gd'] = 1
tprices$firecat[tprices$FireplaceQu == 'Fa'| tprices$FireplaceQu == 'TA'| 
                  tprices$FireplaceQu == 'Po'] = 2
tprices$firecat[is.na(tprices$FireplaceQu)] = 0
table(tprices$firecat, exclude = NULL)
table(tprices$FireplaceQu, exclude = NULL)
tprices$firecat = as.factor(tprices$firecat)

#### GarageType -- 81/76 missings for no garage have been recoded.####
tprices$garTcat = NULL
tprices$garTcat[tprices$GarageType == 'Attchd'] = 1
tprices$garTcat[tprices$GarageType == 'Detchd'] = 2
tprices$garTcat[tprices$GarageType == '2Types'|tprices$GarageType == 'Basment'|
                  tprices$GarageType == 'BuiltIn' |tprices$GarageType == 'CarPort'] = 3
tprices$garTcat[is.na(tprices$GarageType)] = 0
table(tprices$garTcat, exclude = NULL)
table(tprices$GarageType, exclude = NULL)
tprices$garTcat = as.factor(tprices$garTcat)

#### GarageFinish -- 81/78 missings for no garage have been recoded ####
table(tprices$GarageFinish, exclude = NULL)
tprices$garFcat = NULL
tprices$garFcat[tprices$GarageFinish == 'Fin'] = 1
tprices$garFcat[tprices$GarageFinish == 'RFn'] = 2
tprices$garFcat[tprices$GarageFinish == 'Unf'] = 3
tprices$garFcat[is.na(tprices$GarageFinish)]  = 0
table(tprices$garFcat, exclude = NULL)
tprices$garFcat = as.factor(tprices$garFcat)

#### GarageQual -- 81/78 missings for no garage have been recoded.####
tprices$garQcat = NULL
tprices$garQcat[tprices$GarageQual == 'Ex'| tprices$GarageQual == 'Gd'] = 1
tprices$garQcat[tprices$GarageQual == 'TA'] = 2
tprices$garQcat[tprices$GarageQual == 'Fa' | tprices$GarageQual == 'Po'] = 3
tprices$garQcat[is.na(tprices$GarageQual)]  = 0
table(tprices$garQcat, exclude = NULL)
table(tprices$GarageQual, exclude = NULL)
tprices$garQcat = as.factor(tprices$garQcat)

#### GarageCond -- 81/78 missings for no garage have been recoded.####
tprices$garCcat = NULL
tprices$garCcat[tprices$GarageCond == 'Ex'| tprices$GarageCond == 'Gd'] = 1
tprices$garCcat[tprices$GarageCond == 'TA'] = 2
tprices$garCcat[tprices$GarageCond == 'Fa'| tprices$GarageCond == 'Po'] = 3
tprices$garCcat[is.na(tprices$GarageCond)]  = 0
table(tprices$GarageCond, exclude = NULL)   
table(tprices$garCcat, exclude = NULL)
tprices$garCcat = as.factor(tprices$garCcat)

nrow(tprices)
dim(tprices)

#### Paved Drive -- no missings ####
tprices$pavedum = NULL
tprices$pavedum[tprices$PavedDrive == 'Y'] = 1
tprices$pavedum[tprices$PavedDrive == 'N'| tprices$PavedDrive == 'P'] = 0
table(tprices$pavedum, exclude = NULL)   
table(tprices$PavedDrive, exclude = NULL)
tprices$pavedum = as.factor(tprices$pavedum)

#### Sale Type -- no missings in train. **1 in test.** ####
tprices$saleTdum = NULL
tprices$saleTdum[tprices$SaleType == 'WD'] = 1
tprices$saleTdum[tprices$SaleType == 'COD'| tprices$SaleType == 'Con'| 
                   tprices$SaleType == 'ConLD'| tprices$SaleType == 'ConLI'|
                   tprices$SaleType == 'ConLw'|tprices$SaleType == 'CWD' |
                   tprices$SaleType == 'New'| tprices$SaleType == 'Oth'] = 0
table(tprices$saleTdum, exclude = NULL)
table(tprices$SaleType, exclude = NULL)
tprices$saleTdum = as.factor(tprices$saleTdum)

#### Sale Condition -- no missings ####
tprices$saleCcat = NULL
tprices$saleCcat[tprices$SaleCondition == 'Normal'] = 1
tprices$saleCcat[tprices$SaleCondition == 'Abnorml'] =2
tprices$saleCcat[tprices$SaleCondition == 'Partial'] = 3
tprices$saleCcat[tprices$SaleCondition == 'AdjLand'|tprices$SaleCondition == 'Alloca'|
                   tprices$SaleCondition == 'Family'] = 4
table(tprices$saleCcat, exclude = NULL)
table(tprices$SaleCondition, exclude = NULL)
tprices$saleCcat = as.factor(tprices$saleCcat)

#### MSSubClass -- no missings ####
tprices$MSSubcat = NULL
tprices$MSSubcat[tprices$MSSubClass == '20' | tprices$MSSubClass == '30'|
                   tprices$MSSubClass == '40'|tprices$MSSubClass == '45'|
                   tprices$MSSubClass == '50'] = 1
tprices$MSSubcat[tprices$MSSubClass == '60'|tprices$MSSubClass == '70'|
                   tprices$MSSubClass == '75'] = 2
tprices$MSSubcat[tprices$MSSubClass == '80'| tprices$MSSubClass == '85'|
                   tprices$MSSubClass == '90'] = 3
tprices$MSSubcat[tprices$MSSubClass == '120'| tprices$MSSubClass == '150'|
                   tprices$MSSubClass == '160'|
                   tprices$MSSubClass == '180'|tprices$MSSubClass == '190'] = 4
table(tprices$MSSubcat, exclude = NULL)
table(tprices$MSSubClass, exclude = NULL)
tprices$MSSubcat = as.factor(tprices$MSSubcat)

nrow(tprices)
dim(tprices)

### Create dummy variables for Alley access, Pool, Fence and Fireplace -- present Y/N

#### Alley access -- mostly missings have been recoded to a binary variable.####
tprices$alleydum = NULL
tprices$alleydum[tprices$Alley == 'Grvl' | tprices$Alley == 'Pave'] = 1
tprices$alleydum[is.na(tprices$Alley)] = 0
table(tprices$alleydum, exclude = NULL)
table(tprices$Alley, exclude = NULL)
tprices$alleydum = as.factor(tprices$alleydum)

#### Pool -- mostly missing, was recoded to a binary variable ####
## No significant difference in groups. Consider dropping. ##
tprices$pooldum = NULL
tprices$pooldum[tprices$PoolQC == 'Ex' | tprices$PoolQC == 'Fa'| 
                  tprices$PoolQC == 'Gd' ] = 1
tprices$pooldum[is.na(tprices$PoolQC)] = 0
table(tprices$pooldum, exclude = NULL)
table(tprices$PoolQC, exclude = NULL)
tprices$pooldum = as.factor(tprices$pooldum)


#### Fence -- Mostly missing, was recoded to a binary variable ####
tprices$fencedum = NULL
tprices$fencedum[tprices$Fence == 'GdPrv' | tprices$Fence == 'GdWo'| 
                   tprices$Fence == 'MnPrv'| tprices$Fence == 'MnWw'] = 1
tprices$fencedum[is.na(tprices$Fence)] = 0
table(tprices$fencedum, exclude = NULL)
table(tprices$Fence, exclude = NULL)
tprices$fencedum = as.factor(tprices$fencedum)


#### Fireplace -- a lot of missings, was recoded to a binary variable ####
tprices$fireYN = NULL
tprices$fireYN[tprices$FireplaceQu == 'Ex' | tprices$FireplaceQu == 'Fa'| 
                 tprices$FireplaceQu == 'Gd'| tprices$FireplaceQu == 'Po'|
                 tprices$FireplaceQu == 'TA'] = 1
tprices$fireYN[is.na(tprices$FireplaceQu)] = 0
table(tprices$fireYN, exclude = NULL)
table(tprices$FireplaceQu, exclude = NULL)
tprices$fireYN = as.factor(tprices$fireYN)

#### Neighborhood -- convert to factor ####
tprices$Neighborhood = as.factor(tprices$Neighborhood)

nrow(tprices)
dim(tprices)

#### Select out the recoded columns ####
#from the hprices dataframe
tprices2 = select(tprices, Id, Ltotbsmtsf, L1stflrSF, Lgrlivarea, Lgaragearea, durbuilt, 
                  durremod, hqualcat, fullbathcat, totrmscat, garcarcat, zonenew, Lotshdum,
                  Lotcondum, Cond1dum, Cond2dum, bldgdum, housedum, roofsdum, roofmdum, extercat,
                  extercat2, Masncat, exterQdum, exterCdum, foundcat, bsmtQdum, bsmtCcat, bsmtEdum,
                  bsmtFcat, heatdum, heatQdum, CentralAir, elecdum, kitchdum, funcdum, firecat,
                  garTcat, garFcat, garQcat, garCcat, pavedum, saleTdum, saleCcat, MSSubcat, alleydum,
                  fencedum, fireYN, Neighborhood)

nrow(tprices2)
sapply(tprices2, class)

#### Do KNN impute on all missing values ####
library(VIM)
library(mice)
library(caret)
library(Hmisc)
library(rpart)
library(DMwR)

##KNN imputation
prices_imputed_test = knnImputation(tprices2[, !names(tprices2) %in% "LSaleprice"]) 
anyNA(prices_imputed_test)

nrow(prices_imputed_test)

write.csv(prices_imputed_test, file='new_test.csv', row.names=F)















