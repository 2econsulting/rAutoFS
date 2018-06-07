#' @title getmode
#' @description get the most frequent value
#' @param v vector
#' @export
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

