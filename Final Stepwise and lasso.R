##############################################################################
# Installing and loading libraries
##############################################################################
if (!require(tidyverse)) install.packages('tidyverse') # A package for missing data
library(tidyverse)

if (!require(Amelia)) install.packages('Amelia') # A package for missing data
library(Amelia)

if (!require(mice)) install.packages('mice') # Multivariate imputation by chained equation
library(mice)

if (!require(glmnet)) install.packages('glmnet') # Lasso and Elastic-Net Regularized Generalized Linear Models
library(glmnet)

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
## variable, which we ourselves had initialized to NA earlier.
colSums(is.na(full_set))
