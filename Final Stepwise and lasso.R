##############################################################################
# Installing and loading libraries
##############################################################################
if (!require(Amelia)) install.packages('Amelia') # A package for missing data
library(Amelia)

if (!require(mice)) install.packages('mice') # Multivariate imputation by chained equation
library(mice)

if (!require(fastDummies)) install.packages('fastDummies') # Creates dummy columns from columns that have categorical variables
library(fastDummies)

if (!require(glmnet)) install.packages('glmnet') # Lasso and Elastic-Net Regularized Generalized Linear Models
library(glmnet)

##############################################################################
# Data Importation and Preprocessing
##############################################################################

# Data Importation
train_set <- read.csv("./train.csv",stringsAsFactors = T)
test_set <- read.csv("./test.csv",stringsAsFactors = T)

# Bind test and train set to check for missing values
train<-train_set[-81] # Remove the sales price from the trainset
full_set<-bind_rows(train,test_set)

# The train and test set has 13965 missing values in total
sum(is.na(full_set))
# Percentage of missing data in each variables.
colMeans(is.na(full_set))*100
# Removing variables with more than 50% missing values
full_set<-full_set[, which(colMeans(!is.na(full_set)) > 0.5)]
# Plot of missing map
missmap(full_set)

# Imputation  for numeric, ordered and binary variables.
set.seed(76)
Impute <- mice(full_set[, c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2",
                          "Electrical", "GarageType", "GarageFinish", "GarageQual", "GarageCond",
                          "MasVnrType", "MSZoning", "Utilities", "Exterior1st", "Exterior2nd",
                          "KitchenQual", "Functional", "SaleType", "LotFrontage", "GarageYrBlt",
                          "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", 
                          "BsmtFullBath", "BsmtHalfBath", "GarageCars", "GarageArea")],method="pmm")


Impute_set<-complete(Impute)

# Selecting numeric and factors for feature scaling
full_set_factor <- select_if(full_set, is.factor)
full_set_numeric<- select_if(full_set, is.numeric)
full_set_numeric <- full_set_numeric[-1] # To remove id
str(full_set)
# Scaling numeric values
Numeric_scale<-data.frame(scale(full_set_numeric))

# Generating dummy variables for factors
dummy_factor <- dummy_cols(full_set_factor,
                           remove_first_dummy = TRUE)
dummy_factor<-select_if(dummy_factor, is.numeric)

# Bind dataset without the standardized numerical variables.
id<-full_set[1]
full_set_non <- cbind(id, full_set_numeric,
                      full_set_factor)

#Bind dataset with standardized numerical values
full_set_scale<- cbind(id, Numeric_scale,
                       dummy_factor)


saleprice <- train_set$SalePrice
# Train set for non standardized numerical variables
train_non <- full_set_non[1:1460,]
train_non_set <- cbind(train_non, saleprice) 
train_non_set <- train_non_set[-1]

# Test set for non standardized numerical variables
test_non <- full_set_non[1461:2919,]
test_non <-test_non[-1]


saleprice_scale <-scale(saleprice)
# Train set for standardized numerical variables
train_scale <- full_set_scale[1:1460,]
train_scale_set <- cbind(train_scale, saleprice_scale) 
train_scale_set <- train_scale_set[-1]

# Test set for standardized numerical variables
test_scale_set <- full_set_scale[1461:2919,]
test_scale_set <-test_scale_set[-1]



##############################################################################
#Modelling
##############################################################################

#Lasso regression model
train_a <- train_scale_set$saleprice_scale
train_b <- as.matrix(train_scale_set[-233])


lambdas <- 10^seq(-4, 5, by=.1)
set.seed(76)
lasso <- cv.glmnet(train_a, train_b, 
                   alpha = 1,# Setting alpha = 1 initials lasso regression
                   lambda = lambdas)


model_both <- step(lm(formula = saleprice ~ .,
                      data = train_non_set), 
                   direction = "both")

# Finding the optimal lambda 
best_lambda <- lasso$lambda.min 
best_lambda


dim(dummy_factor)

