#' Build MultiDimensional Feature Selector from IGs
#'
#' @details
#' Build MultiDimensional Feature Selector from IGs
#'
#' @param x input data where columns are variables and rows are observations (all numeric)
#' @param y decision variable as a boolean vector of length equal to number of observations
#' @param params method as accepted by \code{\link[stats]{p.adjust}} (\code{"BY"}
#' is recommended for FDR, see Details)
#' @return A \code{\link{data.frame}} with selected features and p.value
#'
#'
#' @examples
#' \dontrun{
#'
#' decisions <- data$class
#' data$class <- NULL
#'
#' fs.mdfs.1D(data, decisions, params = list(adjust = 'holm', alpha = 0.05))
#' }
#'
#'
#' @import FCBF
#' @importFrom stats p.adjust
#' @export
fs.fcbf <- function(x, y, params = list(feature.number = 100)){
  if (!is.data.frame(x)) data = as.data.frame(x)
  result <- fcbf(as.factor(x),
                as.factor(y),
                minimum_su = 0.25,
                n_genes_selected_in_first_step = NULL,
                verbose = FALSE,
                samples_in_rows = FALSE,
                balance_classes = FALSE)
  stats <- as.data.frame(result$stats)
  var.names <- row.names(stats)
  scores <- stats[,1]
  var.imp <- as.data.frame(cbind(var.names, scores))
  names(var.imp) <- c('name', 'score')
  var.imp <- var.imp[order(var.imp$score, decreasing=T),][1:params$feature.number,]
}
