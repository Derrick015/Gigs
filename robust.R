##############################################################################
# Installing and loading libraries
##############################################################################
if (!require(tidyverse)) install.packages('tidyverse') # Data manipulation, exploration and visualization
library(tidyverse)

if (!require(moments)) install.packages('moments') # A package for  Pearson's kurtosis, Geary's kurtosis and skewness; tests related to them
library(moments)

if (!require(caret)) install.packages('caret')  # Classification and Regression Training
library(caret)

if (!require(MASS)) install.packages('MASS') # Support Functions and Datasets for Venables and Ripley's MASS. Functions and datasets to support Venables and Ripley
library(MASS)

if (!require(Metrics)) install.packages('Metrics') # A package for implementation of evaluation metrics in R that are commonly used in supervised machine learning
library(Metrics)

if (!require(car)) install.packages('car') # Companion to Applied Regression
library(car)

if (!require(robustbase)) install.packages('robustbase') # Correlation Plots
library(robustbase)

if (!require(Amelia)) install.packages('Amelia') # Program for missing data
library(Amelia)



##############################################################################
# Train Set Importation and Pre-processing
##############################################################################

##Data Importation##

train_set<-read.csv("C:/Users/Derrick Ofori/Documents/train.csv")

##Data Pre-processing##

# Log Transformation of saleprice variable to the distribution of the dependent variable normal
## histogram of SalePrice Variable is right skewed
qplot(SalePrice, 
      data = train_set, bins = 50, 
      main = "Right skewed distribution")

## Log transformation of salesprice
train_set$SalePrice <- log(train_set$SalePrice + 1)

## Normal distribution after transformation
qplot(SalePrice, 
      data = train_set, bins = 50, 
      main = "Normal distribution achieved after log transformation")


# Dropping Id as it is useless in prediction
train_set <- train_set[,-1]


# Map of missing data
missmap(train_set)

# Replacing some missing categorical variables with none
for (i in c("Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "GarageType", 
            "GarageFinish", "GarageQual", "GarageCond", "BsmtQual", "BsmtCond", 
            "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "MasVnrType")) {
  train_set[is.na(train_set[, i]), i] = "None"
}

# Group by neighborhood and fill N/A by the median
# LotFrontage of all the neighborhood
temp = aggregate(LotFrontage ~ Neighborhood, data = train_set, median)
temp2 = c()
for (h in train_set$Neighborhood[is.na(train_set$LotFrontage)]) {
  temp2 = c(temp2, which(temp$Neighborhood == h))
}
train_set$LotFrontage[is.na(train_set$LotFrontage)] = temp[temp2, 2]

## Replacing missing numerical data with 0
for (col in c("GarageYrBlt", "GarageArea", "GarageCars", "BsmtFinSF1", 
              "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", 
              "MasVnrArea")) {
  train_set[is.na(train_set[, col]), col] = 0
}

## Replace missing MSZoning values with'RL'
train_set$MSZoning[is.na(train_set$MSZoning)] = "RL"

## Remove Utilities with 0 variance
train_set = train_set[,-9]

## Replace missing Functional values with 'Typ'
train_set$Functional[is.na(train_set$Functional)] = "Typ"

## Replace missing Electrical values with 'SBrkr'
train_set$Electrical[is.na(train_set$Electrical)] = "SBrkr"

## Replace missing KitchenQual values by 'TA'
train_set$KitchenQual[is.na(train_set$KitchenQual)] = "TA"

## Replace missing SaleType values by 'WD'
train_set$SaleType[is.na(train_set$SaleType)] = "WD"

## Replace missing Exterior1st and Exterior2nd values by 'VinylSd'
train_set$Exterior1st[is.na(train_set$Exterior1st)] = "VinylSd"
train_set$Exterior2nd[is.na(train_set$Exterior2nd)] = "VinylSd"

## All NAs should be gone, 
missmap(train_set)

# Transform some numerical variables that are categorical
train_set$MSSubClass = as.character(train_set$MSSubClass)
train_set$OverallCond = as.character(train_set$OverallCond)
train_set$YrSold = as.character(train_set$YrSold)
train_set$MoSold = as.character(train_set$MoSold)

# Label encoding some categorical variables that may contain information in their ordering set
cols = c("FireplaceQu", "BsmtQual", "BsmtCond", "GarageQual", "GarageCond", 
         "ExterQual", "ExterCond", "HeatingQC", "PoolQC", "KitchenQual", "BsmtFinType1", 
         "BsmtFinType2", "Functional", "Fence", "BsmtExposure", "GarageFinish", 
         "LandSlope", "LotShape", "PavedDrive", "Street", "Alley", "CentralAir", 
         "MSSubClass", "OverallCond", "YrSold", "MoSold")

FireplaceQu = c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtQual = c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtCond = c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageQual = c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageCond = c("None", "Po", "Fa", "TA", "Gd", "Ex")
ExterQual = c("Po", "Fa", "TA", "Gd", "Ex")
ExterCond = c("Po", "Fa", "TA", "Gd", "Ex")
HeatingQC = c("Po", "Fa", "TA", "Gd", "Ex")
PoolQC = c("None", "Fa", "TA", "Gd", "Ex")
KitchenQual = c("Po", "Fa", "TA", "Gd", "Ex")
BsmtFinType1 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
BsmtFinType2 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
Functional = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")
Fence = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv")
BsmtExposure = c("None", "No", "Mn", "Av", "Gd")
GarageFinish = c("None", "Unf", "RFn", "Fin")
LandSlope = c("Sev", "Mod", "Gtl")
LotShape = c("IR3", "IR2", "IR1", "Reg")
PavedDrive = c("N", "P", "Y")
Street = c("Pave", "Grvl")
Alley = c("None", "Pave", "Grvl")
MSSubClass = c("20", "30", "40", "45", "50", "60", "70", "75", "80", "85", 
               "90", "120", "150", "160", "180", "190")
OverallCond = NA
MoSold = NA
YrSold = NA
CentralAir = NA
levels = list(FireplaceQu, BsmtQual, BsmtCond, GarageQual, GarageCond, 
              ExterQual, ExterCond, HeatingQC, PoolQC, KitchenQual, BsmtFinType1, 
              BsmtFinType2, Functional, Fence, BsmtExposure, GarageFinish, LandSlope, 
              LotShape, PavedDrive, Street, Alley, CentralAir, MSSubClass, OverallCond, 
              YrSold, MoSold)
i = 1
for (c in cols) {
  if (c == "CentralAir" | c == "OverallCond" | c == "YrSold" | c == "MoSold") {
    train_set[, c] = as.numeric(factor(train_set[, c]))
  } else train_set[, c] = as.numeric(factor(train_set[, c], levels = levels[[i]]))
  i = i + 1
}

# Including a relevant feature
train_set$TotalSF = train_set$TotalBsmtSF + train_set$X1stFlrSF + train_set$X2ndFlrSF

# Dummy variables for categorical features
# Get class for each feature
feature_classes <- sapply(names(train_set), function(x) {
  class(train_set[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for hot one encoding for categorical
# features
dummies <- dummyVars(~., train_set[categorical_feats])
categorical_1_hot <- predict(dummies, train_set[categorical_feats])

# Fixing skewed numerical varibles
## Determine skew for each numerical feature
skewed_feats <- sapply(numeric_feats, function(x) {
  skewness(train_set[[x]], na.rm = TRUE)
})

## Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

## Transform skewed features with boxcox transformation
for (x in names(skewed_feats)) {
  bc = BoxCoxTrans(train_set[[x]], lambda = 0.15)
  train_set[[x]] = predict(bc, train_set[[x]])
  # train_set[[x]] <- log(train_set[[x]] + 1)
}

# Reconstruct all data with the pre-processed data
train_set <- cbind(train_set[numeric_feats], categorical_1_hot)


##############################################################################
# Model building
##############################################################################

#Data Partitioning Train: 70%, Validation: 30%
set.seed(46, sample.kind = "Rounding")
y <- train_set$SalePrice
validation_index <- createDataPartition(y, times = 1, p = 0.3, list = FALSE)
validation <- train_set[validation_index,]
training <- train_set[-validation_index,]



# Robust Regression
set.seed(46, sample.kind = "Rounding")
rob<-lmrob(SalePrice~.,
           data = training)


# Residuals converge to zero as weights are increased
plot(residuals(rob) ~ weights(rob, type="robustness"))

# Predictions 
pred_step <- predict(rob, newdata = validation)
rmse(validation$SalePrice, pred_step)

# Retrain on whole trainset
set.seed(46, sample.kind = "Rounding")
final_rob <-lmrob(SalePrice~.,
           data = train_set)


##############################################################################
# Test Set Importation and Pre-processing
##############################################################################

## Data importation
test_set <- read.csv("C:/Users/Derrick Ofori/Documents/test.csv")


## Data Preprocessing

#Save ID so we can drop it from the merged data set
test_id = test_set$Id

# Dropping Id as it is useless in prediction
test_set <- test_set[,-1]

# Correcting Missing values
# Replacing some missing categorical variables with none
for (i in c("Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "GarageType", 
            "GarageFinish", "GarageQual", "GarageCond", "BsmtQual", "BsmtCond", 
            "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "MasVnrType")) {
  test_set[is.na(test_set[, i]), i] = "None"
}

# Group by neighborhood and fill N/A by the median
# LotFrontage of all the neighborhood
temp = aggregate(LotFrontage ~ Neighborhood, data = test_set, median)
temp2 = c()
for (h in test_set$Neighborhood[is.na(test_set$LotFrontage)]) {
  temp2 = c(temp2, which(temp$Neighborhood == h))
}
test_set$LotFrontage[is.na(test_set$LotFrontage)] = temp[temp2, 2]

## Replacing missing numerical data with 0
for (col in c("GarageYrBlt", "GarageArea", "GarageCars", "BsmtFinSF1", 
              "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", 
              "MasVnrArea")) {
  test_set[is.na(test_set[, col]), col] = 0
}

## Replace missing MSZoning values with'RL'
test_set$MSZoning[is.na(test_set$MSZoning)] = "RL"

## Remove Utilities with 0 variance
test_set = test_set[,-9]

## Replace missing Functional values with 'Typ'
test_set$Functional[is.na(test_set$Functional)] = "Typ"

## Replace missing Electrical values with 'SBrkr'
test_set$Electrical[is.na(test_set$Electrical)] = "SBrkr"

## Replace missing KitchenQual values by 'TA'
test_set$KitchenQual[is.na(test_set$KitchenQual)] = "TA"

## Replace missing SaleType values by 'WD'
test_set$SaleType[is.na(test_set$SaleType)] = "WD"

## Replace missing Exterior1st and Exterior2nd values by 'VinylSd'
test_set$Exterior1st[is.na(test_set$Exterior1st)] = "VinylSd"
test_set$Exterior2nd[is.na(test_set$Exterior2nd)] = "VinylSd"

# Transform some numerical variables that are categorical
test_set$MSSubClass = as.character(test_set$MSSubClass)
test_set$OverallCond = as.character(test_set$OverallCond)
test_set$YrSold = as.character(test_set$YrSold)
test_set$MoSold = as.character(test_set$MoSold)

# Label encoding some categorical variables that may contain information in their ordering set
cols = c("FireplaceQu", "BsmtQual", "BsmtCond", "GarageQual", "GarageCond", 
         "ExterQual", "ExterCond", "HeatingQC", "PoolQC", "KitchenQual", "BsmtFinType1", 
         "BsmtFinType2", "Functional", "Fence", "BsmtExposure", "GarageFinish", 
         "LandSlope", "LotShape", "PavedDrive", "Street", "Alley", "CentralAir", 
         "MSSubClass", "OverallCond", "YrSold", "MoSold")

FireplaceQu = c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtQual = c("None", "Po", "Fa", "TA", "Gd", "Ex")
BsmtCond = c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageQual = c("None", "Po", "Fa", "TA", "Gd", "Ex")
GarageCond = c("None", "Po", "Fa", "TA", "Gd", "Ex")
ExterQual = c("Po", "Fa", "TA", "Gd", "Ex")
ExterCond = c("Po", "Fa", "TA", "Gd", "Ex")
HeatingQC = c("Po", "Fa", "TA", "Gd", "Ex")
PoolQC = c("None", "Fa", "TA", "Gd", "Ex")
KitchenQual = c("Po", "Fa", "TA", "Gd", "Ex")
BsmtFinType1 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
BsmtFinType2 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
Functional = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")
Fence = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv")
BsmtExposure = c("None", "No", "Mn", "Av", "Gd")
GarageFinish = c("None", "Unf", "RFn", "Fin")
LandSlope = c("Sev", "Mod", "Gtl")
LotShape = c("IR3", "IR2", "IR1", "Reg")
PavedDrive = c("N", "P", "Y")
Street = c("Pave", "Grvl")
Alley = c("None", "Pave", "Grvl")
MSSubClass = c("20", "30", "40", "45", "50", "60", "70", "75", "80", "85", 
               "90", "120", "150", "160", "180", "190")
OverallCond = NA
MoSold = NA
YrSold = NA
CentralAir = NA
levels = list(FireplaceQu, BsmtQual, BsmtCond, GarageQual, GarageCond, 
              ExterQual, ExterCond, HeatingQC, PoolQC, KitchenQual, BsmtFinType1, 
              BsmtFinType2, Functional, Fence, BsmtExposure, GarageFinish, LandSlope, 
              LotShape, PavedDrive, Street, Alley, CentralAir, MSSubClass, OverallCond, 
              YrSold, MoSold)
i = 1
for (c in cols) {
  if (c == "CentralAir" | c == "OverallCond" | c == "YrSold" | c == "MoSold") {
    test_set[, c] = as.numeric(factor(test_set[, c]))
  } else test_set[, c] = as.numeric(factor(test_set[, c], levels = levels[[i]]))
  i = i + 1
}

# Including a relevant feature
test_set$TotalSF = test_set$TotalBsmtSF + test_set$X1stFlrSF + test_set$X2ndFlrSF

# Dummy variables for categorical features
# Get class for each feature
feature_classes <- sapply(names(test_set), function(x) {
  class(test_set[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for hot one encoding for categorical
# features
dummies <- dummyVars(~., test_set[categorical_feats])
categorical_1_hot <- predict(dummies, test_set[categorical_feats])

# Fixing skewed numerical varibles
## Determine skew for each numerical feature
skewed_feats <- sapply(numeric_feats, function(x) {
  skewness(test_set[[x]], na.rm = TRUE)
})

## Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

## Transform skewed features with boxcox transformation
for (x in names(skewed_feats)) {
  bc = BoxCoxTrans(test_set[[x]], lambda = 0.15)
  test_set[[x]] = predict(bc, test_set[[x]])
  # test_set[[x]] <- log(test_set[[x]] + 1)
}

# Reconstruct all data with the pre-processed data
test_set <- cbind(test_set[numeric_feats], categorical_1_hot)

##############################################################################
# Results 
##############################################################################

## Prediction with test set
pred1<- predict(final_rob,test_set)

# Saving lasso prediction results
df<-data.frame(Id=test_id, SalePrice=exp(pred1))
write.csv(df, "results_robust.csv", row.names = FALSE)


#
