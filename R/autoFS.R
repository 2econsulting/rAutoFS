#' @title autoFS
#' @description auto feature selection
#' @param data_hex H2ODataFrame
#' @param x independent variables
#' @param y dependent variable
#' @param num_of_model the number of model to extract automl vi
#' @param num_of_vi the number of vi features to extract
#' @examples
#' library(rAutoFS)
#' library(h2o)
#' h2o.init()
#' data(churn, package = "rAutoFS")
#' data_hex <- as.h2o(churn)
#' y = "Churn."
#' x = colnames(data_hex)[colnames(data_hex)!=y]
#' autoFS(data_hex, x, y, num_of_model=5, num_of_vi=10)
#' @export
autoFS <- function(data_hex, x, y, num_of_model=5, num_of_vi=10){
  
  # stop rule
  if(num_of_model>10) {stop("num_of_model should be <= 10")}
  if(num_of_vi>30) {stop("num_of_vi should be <= 30")}

  splits <- h2o.splitFrame(data_hex, ratio = c(0.5, 0.3), seed = 1234)
  train_hex <- splits[[1]]
  valid_hex <- splits[[2]]
  test_hex  <- splits[[3]]

  aml <- h2o.automl(
    x = x, y = y,
    training_frame = h2o.rbind(train_hex, valid_hex),
    nfolds = 3,
    leaderboard_frame = test_hex,
    max_runtime_secs = 60*60,
    max_models = 60,
    exclude_algos = c("DeepLearning"),
    seed = 1234
  )

  lb <- aml@leaderboard
  output <- automlVarImp(lb=lb, num_of_model=num_of_model, num_of_vi=num_of_vi)

  return(output)
}

