## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(ssr)

dataset <- friedman1 # Load friedman1 dataset.

head(dataset)

set.seed(1234)

# Split the dataset into 70% for training and 30% for testing.
split1 <- split_train_test(dataset, pctTrain = 70)

# Choose 5% of the train set as the labeled set L and the remaining will be the unlabeled set U.
split2 <- split_train_test(split1$trainset, pctTrain = 5)

L <- split2$trainset # This is the labeled dataset.

U <- split2$testset[, -11] # Remove the labels since this is the unlabeled dataset.

testset <- split1$testset # This is the test set.


## ----message=FALSE, cache=F----------------------------------------------
# Define list of regressors.
regressors <- list("lm", knn=caret::knnreg)

# Fit the model and set maxits to 10. Depending on your system, this may take a couple of minutes.
model <- ssr("Ytrue ~ .", L, U, regressors = regressors, testdata = testset, maxits = 10)

## ----fig.height=5, fig.width=7-------------------------------------------
# Plot RMSE.
plot(model)

# Get the predictions on the testset.
predictions <- predict(model, testset)

# Calculate RMSE on the test set.
rmse.result <- sqrt(mean((predictions - testset$Ytrue)^2))
rmse.result

## ----fig.height=5, fig.width=7-------------------------------------------
plot(model, metric = "mae", ptype = 2)

## ----fig.height=5, fig.width=7, message=FALSE, warning=FALSE, cache=F, eval = FALSE----
#  
#  # Prepare data.
#  dataset <- friedman1
#  set.seed(1234)
#  split1 <- split_train_test(dataset, pctTrain = 70)
#  split2 <- split_train_test(split1$trainset, pctTrain = 5)
#  L <- split2$trainset
#  U <- split2$testset[, -11]
#  testset <- split1$testset
#  
#  # Define list of regressors.
#  regressors <- list("lm", knn=caret::knnreg)
#  
#  # Specify their parameters. k = 7 for knnreg in this case.
#  regressors.params <- list(NULL, list(k=7))
#  
#  model2 <- ssr("Ytrue ~ .", L, U,
#               regressors = regressors,
#               regressors.params = regressors.params,
#               maxits = 10,
#               testdata = testset)
#  
#  plot(model2)
#  

## ----fig.height=5, fig.width=7, message=FALSE, warning=FALSE, cache=F----

# Define a custom function.
myCustomModel <- function(theformula, data, myparam1){

  # This is just a wrapper around knnreg but can be anything.
  # Our custom function also accepts one parameter myparam1.
  
  # Now we train a knnreg and pass our custom parameter.
  m <- caret::knnreg(theformula, data, k = myparam1)
  
  return(m)
}

# Prepare the data
dataset <- friedman1
set.seed(1234)
split1 <- split_train_test(dataset, pctTrain = 70)
split2 <- split_train_test(split1$trainset, pctTrain = 5)
L <- split2$trainset
U <- split2$testset[, -11]
testset <- split1$testset

# Specify our custom function as regressor.
regressors <- list(myCustomModel)

# Specify the list of parameters.
regressors.params <- list(list(myparam1=7))

# Fit the model.
model3 <- ssr("Ytrue ~ .", L, U,
             regressors = regressors,
             regressors.params = regressors.params,
             testdata = testset)


## ----fig.height=5, fig.width=7, message=FALSE, warning=TRUE, cache=F-----

# Prepare the data
dataset <- friedman1
set.seed(1234)
split1 <- split_train_test(dataset, pctTrain = 70)
split2 <- split_train_test(split1$trainset, pctTrain = 5)
L <- split2$trainset
U <- split2$testset[, -11]
testset <- split1$testset

# Get the true labels for the unlabeled set.
U.y <- split2$testset[, 11]

# Define list of regressors.
regressors <- list("lm", knn=caret::knnreg)

# Fit the model.
model4 <- ssr("Ytrue ~ .", L, U,
              regressors = regressors,
              testdata = testset,
              maxits = 10,
              U.y = U.y)

plot(model4)

# Get the predictions on the testset.
predictions <- predict(model4, testset)

# Calculate RMSE on the test set.
sqrt(mean((predictions - testset$Ytrue)^2))


