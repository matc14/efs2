#' @import caret
#' @importFrom MLmetrics Accuracy AUC F1_Score LogLoss
#' @importFrom mltools mcc
rf.build.model <- function(data.train,
                           data.test,
                           ytrain,
                           ytest) {
  model <- train(data.train,
    ytrain,
    method = "rf",
    ntree = 500,
    tuneGrid = data.frame(mtry = as.integer(sqrt(ncol(data.train)))),
    trControl = trainControl(method = "none")
  )
  ypred <- predict(model, data.test)
  accuracy <- Accuracy(ypred, ytest)
  auc <- AUC(ypred, ytest)
  mcc <- mcc(ypred, ytest)
  f1 <- F1_Score(ypred, ytest)
  log <- LogLoss(as.numeric(as.character(ypred)), as.numeric(as.character(ytest)))
  result <- c(accuracy, auc, mcc, f1, log)
  names(result) <- c('Accuracy', 'AUC', 'MCC', 'F1', 'LogLoss')
  print("Random forest log")
  return(result)
}

svm.build.model <- function(data.train,
                            data.test,
                            ytrain,
                            ytest) {
  model <- train(data.train,
    ytrain,
    method = "svmLinear",
    trControl = trainControl(method = "none")
  )
  ypred <- predict(model, data.test)
  accuracy <- Accuracy(ypred, ytest)
  auc <- AUC(ypred, ytest)
  mcc <- mcc(ypred, ytest)
  f1 <- F1_Score(ypred, ytest)
  log <- LogLoss(as.numeric(as.character(ypred)), as.numeric(as.character(ytest)))
  result <- c(accuracy, auc, mcc, f1, log)
  names(result) <- c('Accuracy', 'AUC', 'MCC', 'F1', 'LogLoss')
  print("svm log")
  return(result)
}

knn.build.model <- function(data.train,
                            data.test,
                            ytrain,
                            ytest) {
  model <- train(data.train,
    ytrain,
    method = "knn",
    trControl = trainControl(method = "none")
  )
  ypred <- predict(model, data.test)
  accuracy <- Accuracy(ypred, ytest)
  auc <- AUC(ypred, ytest)
  mcc <- mcc(ypred, ytest)
  f1 <- F1_Score(ypred, ytest)
  log <- LogLoss(as.numeric(as.character(ypred)), as.numeric(as.character(ytest)))
  result <- c(accuracy, auc, mcc, f1, log)
  names(result) <- c('Accuracy', 'AUC', 'MCC', 'F1', 'LogLoss')
  print("knn log")
  return(result)
}


#' Train model Random Forest
#'
#' @details
#' Train model Random Forest in cross-validation and variables selected in the cross-validatio
#'
#' @param x input data where columns are variables and rows are observations (all numeric)
#' @param y decision variable as a boolean vector of length equal to number of observations
#' @param list.selected.var A \code{\link{list}} with selected variables in cross-validation
#' @param list.index.cross A \code{\link{list}} with indexes obtained in cross-validation
#' @param nvar the number of first variables for which to train model Random Forest
#' @return A \code{\link{list}} with metrics Accuracy, AUC, MCC, F1
#'
#' @examples
#' \dontrun{
#'
#' class <- data$class
#' data$class <- NULL
#'
#' list.index.cross <- cross.val(
#'   x = data,
#'   y = decisions,
#'   method = "kfoldcv",
#'   params.cv = list(niter = 10, k = 3)
#' )
#'
#' list.selected.var <- feature.selection(
#'   x = data,
#'   y = class,
#'   method = "fs.utest",
#'   list.index.cross = list.index.cross,
#'   params = list(adjust = "holm", alpha = 0.05)
#' )
#'
#' model.result <- build.model.crossval(
#'   x = data,
#'   y = class,
#'   list.selected.var = list.selected.var,
#'   list.index.cross = list.index.cross,
#'   nvar = 10
#' )
#' }
#'
#' @export
build.model.crossval <- function(x,
                                 y,
                                 list.selected.var,
                                 list.index.cross,
                                 nvar,
                                 classifier) {
  if (!is.data.frame(x)) data <- as.data.frame(x)
  niter <- length(list.index.cross)
  ncross <- length(list.index.cross[[1]]$training)
  train.rf <- function(m, x, y, niter, ncross, list.selected.var, list.index.cross, nvar) {
    index <- expand.grid(j = 1:ncross, p = 1:niter)
    p <- index[m, 2]
    j <- index[m, 1]
    index.cross <- list.index.cross[[p]]
    x$class <- y
    data.train.cross <- na.omit(x[index.cross$training[[j]], ])
    data.test.cross <- na.omit(x[index.cross$testing[[j]], ])
    ytrain <- as.factor(data.train.cross$class)
    ytest <- as.factor(data.test.cross$class)
    data.train.cross$class <- NULL
    data.test.cross$class <- NULL
    var <- list.selected.var[[m]]$name
    if (length(var) > nvar) {
      var <- var[1:nvar]
      if (classifier == "rf") {
        metrics <- rf.build.model(data.train.cross[, c(var)],
          data.test.cross[, c(var)],
          ytrain = ytrain,
          ytest = ytest
        )
      } else if (classifier == "svm") {
        metrics <- svm.build.model(data.train.cross[, c(var)],
          data.test.cross[, c(var)],
          ytrain = ytrain,
          ytest = ytest
        )
      } else if (classifier == "knn") {
        metrics <- knn.build.model(data.train.cross[, c(var)],
          data.test.cross[, c(var)],
          ytrain = ytrain,
          ytest = ytest
        )
      }
      return(metrics)
    } else if (length(var) == 0 || length(var) == 1) {
      return(NULL)
    } else {
      if (classifier == "rf") {
        metrics <- rf.build.model(data.train.cross[, c(var)],
          data.test.cross[, c(var)],
          ytrain = ytrain,
          ytest = ytest
        )
      } else if (classifier == "svm") {
        metrics <- svm.build.model(data.train.cross[, c(var)],
          data.test.cross[, c(var)],
          ytrain = ytrain,
          ytest = ytest
        )
      } else if (classifier == "knn") {
        metrics <- knn.build.model(data.train.cross[, c(var)],
          data.test.cross[, c(var)],
          ytrain = ytrain,
          ytest = ytest
        )
      }
    }
  }
  N <- niter * ncross
  result.metrics <- lapply(1:N, function(m) {
    train.rf(m,
      x = x,
      y = y,
      niter = niter,
      ncross = ncross,
      list.selected.var = list.selected.var,
      list.index.cross = list.index.cross,
      nvar
    )
  })
  return(result.metrics)
}


#' Train model Random Forest for the top-N variables N = 5,10,15,20,30,40,50,75,100
#'
#' @details
#' Train model Random Forest in cross-validation and variables selected in the cross-validation
#' for the top-N variables N = 5,10,15,20,30,40,50,75,100
#'
#' @param x input data where columns are variables and rows are observations (all numeric)
#' @param y decision variable as a boolean vector of length equal to number of observations
#' @param list.selected.var A \code{\link{list}} with selected variables in cross-validation
#' @param list.index.cross A \code{\link{list}} with indexes obtained in cross-validation
#' @return A \code{\link{data.frame}} with metrics Accuracy, AUC, MCC for the top-N variables N = 5,10,15,20,30,40,50,75,100
#'
#' @examples
#' \dontrun{
#'
#' class <- data$class
#' data$class <- NULL
#'
#' list.index.cross <- cross.val(
#'   x = data,
#'   y = decisions,
#'   method = "kfoldcv",
#'   params.cv = list(niter = 10, k = 3)
#' )
#'
#' list.selected.var <- feature.selection(
#'   x = data,
#'   y = class,
#'   method = "fs.utest",
#'   list.index.cross = indexes,
#'   params = list(adjust = "holm")
#' )
#'
#' model.result <- model.result.top.var(
#'   x = data,
#'   y = class,
#'   list.selected.var = list.selected.var,
#'   list.index.cross = list.index.cross
#' )
#' }
#'
#' @importFrom stats sd
#' @export
model.result.top.var <- function(x,
                                 y,
                                 list.selected.var,
                                 list.index.cross,
                                 classifier) {
  N <- c(5, 10, 15, 20, 30, 40, 50, 75, 100)
  result.data <- data.frame(N)
  result.data[, c("mean.acc", "mean.auc", "mean.mcc", "mean.f1", "mean.log", "sd.acc", "sd.auc", "sd.mcc", "sd.f1", "sd.log")] <- NA
  for (n in N) {
    metrics <- build.model.crossval(
      x,
      y,
      list.selected.var,
      list.index.cross,
      n,
      classifier
    )
    non.null <- which(!sapply(metrics, is.null))
    metrics <- metrics[non.null]
    acc <- c()
    auc <- c()
    mcc <- c()
    f1 <- c()
    log <- c()
    for (i in 1:length(metrics)) {
      acc <- append(acc, metrics[[i]][1])
      auc <- append(auc, metrics[[i]][2])
      mcc <- append(mcc, metrics[[i]][3])
      f1 <- append(f1, metrics[[i]][4])
      log <- append(log, metrics[[i]][5])
    }
    result.data[result.data$N == n, "mean.acc"] <- sum(acc) / length(metrics)
    result.data[result.data$N == n, "mean.auc"] <- sum(auc) / length(metrics)
    result.data[result.data$N == n, "mean.mcc"] <- sum(mcc) / length(metrics)
    result.data[result.data$N == n, "mean.f1"] <- sum(f1) / length(metrics)
    result.data[result.data$N == n, "mean.log"] <- sum(log) / length(metrics)
    result.data[result.data$N == n, "sd.acc"] <- sd(acc)
    result.data[result.data$N == n, "sd.auc"] <- sd(auc)
    result.data[result.data$N == n, "sd.mcc"] <- sd(mcc)
    result.data[result.data$N == n, "sd.f1"] <- sd(f1)
    result.data[result.data$N == n, "sd.log"] <- sd(log)
  }
  return(result.data)
}
