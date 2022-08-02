#' @title MSE Function
#
#' @description This function calculate the Mean Squared Error of of the the predicted y value and the true y value.
#'
#' @param y A numeric input (of vector) of true y
#'
#' @param y_hat A numeric input (of vector) of predicted y
#'
#' @return MSE functio nreturn a numeric value indicating the mean square error of the input.
#
## MSE function
MSE = function(y, y_hat) {
  mse = mean((y - y_hat) ^ 2)
  return(mse)
}

#' @title Split Train Test Function
#'
#' @description Intake a data file path, split it into train test set by user input proportion
#'
#' @param data splitting data, default to Graduate Admission data set
#'
#' @param train_p Proportion of the training set (out of the entire dataset) default 0.8
#'
#' @param seed seeding during random sampling, default 0
#'
#' @import dplyr
#'
#' @return A list containing two dataframe called train and test.

split_data = function(data=gradApplication, train_p = 0.8, seed=0) {
  # check if na numbers and good to go
  colNa = colSums(is.na(data))
  if (sum(colNa) != 0) {
    warning("NA value exists in the data")
  }
  # create train, and test set
  set.seed(seed)
  data = dplyr::select(data, -Serial.No.)
  index_num = 1:nrow(data)
  trainsize = round(nrow(data) * train_p)
  testsize = round(nrow(data) * (1-train_p))
  test_index = sample(index_num, size = testsize)
  testdata = data[test_index, ]
  traindata = data[-test_index, ]
  return (list("train"=traindata,"test"=testdata))
}

#' @title Logit Function
#'
#' @description Logit Function for internal predictions
#'
#' @param x A numeric vector input to put through logistic function
#'
#' @return (log(x/(1-x)))
logit = function(x) {
  if (any(is.numeric(x)) == FALSE) {
    stop("Non-numeric Input")
  }
  return (log(x/(1-x)))
}


#' @title Logistic Function (Inverse Logit)
#'
#' @description Inverse Logit function to inverse during prediction
#'
#' @param x A numeric vector input to put through logistic function
#'
#' @return exp(x)/(1+exp(x))
logistic = function(x) {
  if (any(is.numeric(x)) == FALSE) {
    stop("Non-numeric Input")
  }
  return (exp(x)/(1+exp(x)))
}

#' @title Train Model Function
#'
#' @description This function is a wrapper function to training the model that
#' predict the acceptance rate of the prospective student.Currently we are supporting OLS,
#' Ridge and Lasso. CV tuning for Ridge and Lasso are already implemented in this function
#'
#' @param traindata Intake a dataframe as training dataset
#'
#' @param method Intake a string to indicate the method of modeling
#'
#' @import glmnet
#' @import caret
#'
#' @return A model trained using the train data
#'
train_model = function(traindata, method = "ols") {
  method = tolower(method)
  ## initialize mse storage
  mse_val = c()
  mse_test = c()
  lambda = 10^seq(3, -2, by = -.1)
  cv_mse = matrix(NA, nrow = length(lambda), ncol = 5)

  # train model
  if (method == "ols") {
    model = lm(logit(Chance.of.Admit) ~ GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + factor(Research),
                   data = traindata)

    return (model)
  }
    ## Create 5 Folds
    folds = createFolds(traindata$Chance.of.Admit, k = 5)
    data_cv_norate = dplyr::select(traindata, -Chance.of.Admit)
    data_cv_rate = traindata$Chance.of.Admit

    # 5 fold cv to tune for the best lambda
    for (k in 1:5) {
      fold = unlist(folds[k]) # convert list to vector
      train_norate = data_cv_norate[-fold,]
      train_rate = data_cv_rate[-fold]
      validation_norate = data_cv_norate[fold,]
      validation_rate = data_cv_rate[fold]

      for (j in 1:length(lambda)) {
        lamb = lambda[j]
        if (method == "ridge") {
          model = glmnet::glmnet(
            train_norate,
            logit(train_rate),
            family = "gaussian",
            lambda = lamb,
            alpha = 0,
            standardize = TRUE
          )
        } else if (method == "lasso") {
          model = glmnet::glmnet(
            train_norate,
            logit(train_rate),
            family = "gaussian",
            lambda = lamb,
            alpha = 1,
            standardize = TRUE
          )
        }
        yhat_val = predict(model, as.matrix(validation_norate),s = lamb)
        cv_mse[j,k] = MSE(validation_rate, logistic(yhat_val))
      }
    }
    # Getting CV mse, finding best lambda
    cv_mse_mean=rowMeans(cv_mse)
    lambda_cv = lambda[which.min(cv_mse_mean)]

    if (method == 'ridge') {
      model = glmnet::glmnet(
        train_norate,
        logit(train_rate),
        family = "gaussian",
        lambda = lambda_cv,
        alpha = 0,
        standardize = TRUE
      )
    } else if (method == 'lasso') {
      model = glmnet::glmnet(
        train_norate,
        logit(train_rate),
        family = "gaussian",
        lambda = lambda_cv,
        alpha = 1,
        standardize = TRUE
      )
    }
  return(model)
}

#' @title Predict Function
#'
#' @description A general wrapper function to predict using the inputted model
#'
#' @param model An inputted model
#'
#' @param newx inputted data to predict
#'
#' @param method The type of the model
#'
#' @export
predict_skr = function(model, newx, method) {
  method = tolower(method)
  if (any(is.na(newx)) == TRUE){
    stop("Input data is NA")
  }
  if ((method %in% c("ols", "ridge", "lasso"))==FALSE) {
    stop("Method Invalid, input method is an unimplemented method")
  }
  if (method == "ols") {
    return (logistic(predict(model, as.data.frame(newx))))
  }
  if (method == "lasso" | method == "ridge") {
    return (logistic(predict(model, as.matrix(newx))))
  }
}

#' @title Model Comparison Function
#'
#' @description This is a wrapper function that utilizes train_model and split_data
#' functions to train and compare the best model for our dataset.
#' Users only need this function to find the best model.
#'
#' @param data splitting data default to Graduate Admission data set
#'
#' @param train_p Proportion of the training set (out of the entire dataset) default 0.8
#'
#' @param seed seeding during random sampling, default 0
#'
#' @param methods, a vector of methods to compare, currently only support ols,
#' ridge regression, and lasso regression
#'
#' @return Returns a list that include "best_method" for the best method
#' and a best_model for the best model from the inputted method vector
#'
#' @import dplyr
#'
#' @export
#'
model_comparison = function(data=gradApplication,
                            train_p = 0.8, seed=0, methods=c("ols", "ridge", "lasso")) {
  methods = tolower(methods)
  if (length(train_p) != 1) {
    stop("train_p input should be a single value")
  }
  if (train_p > 1 | train_p < 0) {
    stop("Invalid training proportion, input should be a single value between 0 and 1")
  }

  if (train_p < 0.6) {
    warning("training proportion is low, recommend train_p >= 0.7")
  }

  splitted = split_data(data, train_p, seed)
  traindata = splitted$train
  testdata = splitted$test
  testdata_y = testdata$Chance.of.Admit
  testdata_x = select(testdata, -Chance.of.Admit)

  curr_mod_best = NA
  curr_mse = Inf
  best_m = NA
  for (m in methods) {
    if ((m %in% c("ols", "ridge", "lasso"))==FALSE) {
      stop("Method Invalid, input method contains unimplemented method")
    }
    model = train_model(traindata, m)
    test_pred = predict_skr(model, testdata_x, method=m)
    temp_mse = MSE(testdata_y, test_pred)
    if (temp_mse < curr_mse) {
      curr_mse = temp_mse
      curr_mod_best = model
      best_m = m
    }
  }

  return (list("best_method" = best_m, "best_model" = curr_mod_best))
}

