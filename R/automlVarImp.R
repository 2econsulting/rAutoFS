#' @title automlVarImp
#' @description extract variable importance of automl
#' @param lb automl leaberboard
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
#' splits <- h2o.splitFrame(data_hex, ratio = c(0.5, 0.3), seed = 1234)
#' train_hex <- splits[[1]]
#' valid_hex <- splits[[2]]
#' test_hex  <- splits[[3]]
#' aml <- h2o.automl(
#'   x = x, y = y,
#'   training_frame = h2o.rbind(train_hex, valid_hex),
#'   nfolds = 3,
#'   leaderboard_frame = test_hex,
#'   max_runtime_secs = 60*60,
#'   max_models = 60,
#'   exclude_algos = c("DeepLearning"),
#'   seed = 1234
#' )
#' lb <- aml@leaderboard
#' automlVarImp(lb=lb, num_of_model=5, num_of_vi=10)
#' @export
automlVarImp <- function(lb, num_of_model=5, num_of_vi=10){
  
  # stop rule
  if(num_of_model>10) {stop("num_of_model should be <= 10")}
  if(num_of_vi>30) {stop("num_of_vi should be <= 30")}
  
  # top model 
  top_modelId <- as.data.frame(lb$model_id)$model_id
  top_modelName <- paste0(gsub("_.*","",top_modelId),gsub(".*_","",top_modelId))
  top_vi_list <- list()
  for(i in 1:length(top_modelId)){
    tryCatch({
      varimp <- head(as.data.frame(h2o.varimp(h2o.getModel(top_modelId[i]))[, c("variable", "percentage")]), 50)
      colnames(varimp) <- paste(colnames(varimp),top_modelName[i],sep="_")
      varimp$rank <- 1:nrow(varimp)
      top_vi_list[[top_modelId[i]]] <- varimp
    },error = function(e) {cat(">> ", top_modelId[i], "doesn't have variable importances! \n")},
    warning = function(w) {cat(">> ", top_modelId[i], "doesn't have variable importances! \n")}
    )
    if(length(top_vi_list)==num_of_model) {break}
  }

  # top_vi
  top_vi_df <- Reduce(function(x,y) merge(x,y,"rank"), top_vi_list)
  top_vi_df <- top_vi_df[,-grep("percentage",colnames(top_vi_df))]
  colnames(top_vi_df) <- gsub("variable", "vi", colnames(top_vi_df))
  top_vi_df$vi <- apply(top_vi_df[,-1], 1, rAutoFS::getmode)
  top_vi <- unique(top_vi_df$vi)[1:num_of_vi]
  top_vi <- top_vi[!is.na(top_vi)]

  return(list("top_vi_df"=top_vi_df, "top_vi"=top_vi))
}


