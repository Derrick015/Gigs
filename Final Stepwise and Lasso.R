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

if (!require(glmnet)) install.packages('glmnet') # Lasso and Elastic-Net Regularized Generalized Linear Models
library(glmnet)

if (!require(Metrics)) install.packages('Metrics') # A package for implementation of evaluation metrics in R that are commonly used in supervised machine learning
library(Metrics)

if (!require(car)) install.packages('car') # Companion to Applied Regression
library(car)

##############################################################################
# Data Importation and Pre-processing
##############################################################################

##Data Importation##

train_set <- read.csv("./train.csv",stringsAsFactors = F)
test_set <- read.csv("./test.csv",stringsAsFactors = F)

##Data Pre-processing##

#Save ID so we can drop it from the merged data set
train_id = train_set$Id
test_id = test_set$Id

# Place N/A into empty saleprice column in test set
test_set$SalePrice = NA

# Check for outliers by plotting saleprice vs GrLivArea
qplot(train_set$GrLivArea, 
      train_set$SalePrice,
      main = "Outliers")

# Remove the outliers
train_set <- train_set[-which(train_set$GrLivArea > 4000 & 
                                train_set$SalePrice < 3e+05), 
]


## Check again after removal.
qplot(train_set$GrLivArea, train_set$SalePrice, main = "No Outliers")

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

# Bind the train and test set
full_set <- rbind(train_set,test_set)

# Dropping Id as it is useless in prediction
full_set <- full_set[,-1]


# Percentage of missing data in each variables.
# Note sales price will have about 50% N/A because we filled it
colMeans(is.na(full_set))*100

# Replacing some missing categorical variables with none
for (i in c("Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "GarageType", 
            "GarageFinish", "GarageQual", "GarageCond", "BsmtQual", "BsmtCond", 
            "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "MasVnrType")) {
  full_set[is.na(full_set[, i]), i] = "None"
}

# Group by neighborhood and fill N/A by the median
# LotFrontage of all the neighborhood
temp = aggregate(LotFrontage ~ Neighborhood, data = full_set, median)
temp2 = c()
for (h in full_set$Neighborhood[is.na(full_set$LotFrontage)]) {
  temp2 = c(temp2, which(temp$Neighborhood == h))
}
full_set$LotFrontage[is.na(full_set$LotFrontage)] = temp[temp2, 2]

## Replacing missing numerical data with 0
for (col in c("GarageYrBlt", "GarageArea", "GarageCars", "BsmtFinSF1", 
              "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", 
              "MasVnrArea")) {
  full_set[is.na(full_set[, col]), col] = 0
}

## Replace missing MSZoning values with'RL'
full_set$MSZoning[is.na(full_set$MSZoning)] = "RL"

## Remove Utilities with 0 variance
full_set = full_set[,-9]

## Replace missing Functional values with 'Typ'
full_set$Functional[is.na(full_set$Functional)] = "Typ"

## Replace missing Electrical values with 'SBrkr'
full_set$Electrical[is.na(full_set$Electrical)] = "SBrkr"

## Replace missing KitchenQual values by 'TA'
full_set$KitchenQual[is.na(full_set$KitchenQual)] = "TA"

## Replace missing SaleType values by 'WD'
full_set$SaleType[is.na(full_set$SaleType)] = "WD"

## Replace missing Exterior1st and Exterior2nd values by 'VinylSd'
full_set$Exterior1st[is.na(full_set$Exterior1st)] = "VinylSd"
full_set$Exterior2nd[is.na(full_set$Exterior2nd)] = "VinylSd"

## All NAs should be gone, except the test segment of SalePrice
# Check percentage of N/A in variables
colMeans(is.na(full_set))*100

# Transform some numerical variables that are categorical
full_set$MSSubClass = as.character(full_set$MSSubClass)
full_set$OverallCond = as.character(full_set$OverallCond)
full_set$YrSold = as.character(full_set$YrSold)
full_set$MoSold = as.character(full_set$MoSold)

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
    full_set[, c] = as.numeric(factor(full_set[, c]))
  } else full_set[, c] = as.numeric(factor(full_set[, c], levels = levels[[i]]))
  i = i + 1
}

# Including a relevant feature
full_set$TotalSF = full_set$TotalBsmtSF + full_set$X1stFlrSF + full_set$X2ndFlrSF

# Dummy variables for categorical features

# Get class for each feature
feature_classes <- sapply(names(full_set), function(x) {
  class(full_set[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for hot one encoding for categorical
# features
dummies <- dummyVars(~., full_set[categorical_feats])
categorical_1_hot <- predict(dummies, full_set[categorical_feats])

# Fixing skewed numerical varibles
## Determine skew for each numerical feature
skewed_feats <- sapply(numeric_feats, function(x) {
  skewness(full_set[[x]], na.rm = TRUE)
})

## Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

## Transform skewed features with boxcox transformation
for (x in names(skewed_feats)) {
  bc = BoxCoxTrans(full_set[[x]], lambda = 0.15)
  full_set[[x]] = predict(bc, full_set[[x]])
  # full_set[[x]] <- log(full_set[[x]] + 1)
}

# Reconstruct all data with the pre-processed data
full_set <- cbind(full_set[numeric_feats], categorical_1_hot)
dim(full_set)


##############################################################################
# Model building
##############################################################################

train<- full_set[1:1458, ]
test<- full_set[1459:2917, ]

#Data Partitioning Train: 70%, Validation: 30%
set.seed(76)
y <- train$SalePrice
validation_index <- createDataPartition(y, times = 1, p = 0.3, list = FALSE)
validation <- train[validation_index,]
training <- train[-validation_index,]

# 1. Lasso Regression
set.seed(76)
model_lasso = cv.glmnet(as.matrix(training[, -59]), 
                     training[, 59])

# Prediction with the validation set
pred <- predict(model_lasso, 
                newx = as.matrix(validation[, -59]),
                s = "lambda.min")
rmse(validation$SalePrice, pred)

# Retraining on whole training set and Final Submission
set.seed(123)
cv_lasso = cv.glmnet(as.matrix(training[, -59]), training[, 59])

## Predictions
pred_lasso = data.frame(exp(predict(cv_lasso, 
                               newx = as.matrix(test[, -59]), 
                               s = "lambda.min")) - 1)

# Saving lasso prediction results
df<-data.frame(Id=test_id, SalePrice=pred_lasso$X1)
write.csv(df, "results.csv", row.names = FALSE)


# 2. Stepwise Regression
model_step <- step(lm(formula = SalePrice ~ .,
                      data = training), 
                   direction = "forward")

# Assumptions
# A. The regression model is linear.....YES
#Residuals vs Fitted. Horizontal line shows almost linear relationship
plot(model_step, 1) 

# B. The mean of residuals is zero.....YES 
format(mean(model_step$residuals), 
       scientific=F) # (mean of residuals is approximately zero, this assumption holds true)

# C. Residuals are normally distributed.....NO
plot(model_step, 2) #Normal QQ.... not normal pattern

# D. Homoscedasticity of residuals.....NO
plot(model_step, 3) # heteroscedascity

# E No autocorrelation of residuals - YES
acf(model_step$residuals) #the very first line (to the left) shows the correlation of residual with itself (Lag0), therefore, it will always be equal to 1. Correlation values drop below the dashed blue line from lag1 itself. So autocorrelation can't be confirmed.
# test for randomness, H0:is random, H1:is pattern
lawstat::runs.test(model_step$residuals)
# Durbin-Watson test, H0:no autocorrelation, H1: there is autocorrelation
durbinWatsonTest(model_step)

# Predictions
pred_step <- predict(model_step, newdata = validation)
rmse(validation$SalePrice, pred_step)


