#########################################################################################################
# 1. Presentation of the theoretical bases of the two methods
#########################################################################################################


#########################################################################################################
# 2. Presentation of dataset
#########################################################################################################

#Loading libraries
if (!require(tidyverse)) install.packages('tidyverse') #Data manipulation, exploration and visualization
library(tidyverse)
if (!require(mice)) install.packages('mice') # Multivariate imputation by chained equation
library(mice)
if (!require(VIM)) install.packages('VIM') 
library(VIM)
library(caret)
if (!require(glmnet)) install.packages('glmnet') # Lasso and Elastic-Net Regularized Generalized Linear Models
library(glmnet)

# Data Importation
train_set <- read.csv("./train.csv", stringsAsFactors = T)
test_set <- read.csv("./test.csv", stringsAsFactors = T)


full_set<-bind_rows(train_set,test_set) # Binding the data and test set
str(full_set) # Analysis of the the database shows 2919 obervatoins and 81 variables.

#3. Description of data using basic statistical analyzes
# Total percentage of missing values in variables and obervations
colMeans(is.na(train_set))*100
colMeans(is.na(test_set))*100

str(train_set)
## Remove columns with more than 50% NA
train_set<-train_set[, which(colMeans(!is.na(train_set)) > 0.6)]
test_set<-test_set[, which(colMeans(!is.na(test_set)) > 0.6)]



dim(train_set)
train_set$BsmtFinSF2
dim(dd)

dd<-train_set[complete.cases(train_set), ]
colMeans(is.na(dd))*100

cc <- test_set[complete.cases(test_set), ]

train_set$SalePrice

impute<-mice(train_set[,2:76],m=2,seed = 46, method="cart")

impute$imp$MasVnrType
train_set$MasVnrType
#Complete data


data_factor <- select_if()

n_train_set<-complete(impute,1)



list<-bind_cols(A,x)
list[16,]





i<- c(1, 4)
n_train_set[ , i] <- apply(n_train_set[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.integer(x)))


str(n_train_set)



n_train_set[,1] <- lapply(n_train_set[,1], as.numeric)

str(n_train_set)

n_train_set$MSSubClass<-as.numeric(n_train_set$MSSubClass)

african_crises$domestic_debt_in_default<-as.factor(african_crises$domestic_debt_in_default)



#pp
#Data Partitioning Train: 80%, Test: 20%
set.seed(46, sample.kind = "Rounding")
y<-n_train_set$SalePrice
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
itest_set <- n_train_set[test_index,]
itrain_set <- n_train_set[-test_index,]

# lasso
custom<- trainControl(method = "repeatedcv",
                      number=10,
                      repeats =5,
                      verboseIter=T)

set.seed(46)
lasso<- train (SalePrice ~ LotFrontage+LotArea,
               itrain_set,
               method ="glmnet",
               tuneGrid = expand.grid(alpha=1,
                                      lambda = seq (0.001,1,lenght = 5)),
               trContol = custom)

plot(lasso)

select_if(n_train_set,is.integer)
