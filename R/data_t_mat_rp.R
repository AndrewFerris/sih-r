#' @name data_t_mat_rp
#' @docType data
#' @aliases data_t_mat_rp
#' @title Example data for t_mat_rp
#' @description An example data source that is used for demonstrating the t_mat_rp function.
#' @usage data(data_t_mat_rp)
#' @format A 3x3 matrix with numeric elements.
#' @source Simulated
#' @author Andrew Ferris
#' @keywords dataset
#' @example data_t_mat_rp
#' @export

data_t_mat_rp <- matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3, ncol = 3)
row.names(data_t_mat_rp) <- c("A", "B", "C")
colnames(data_t_mat_rp) <- c("D", "E", "F")
save(data_t_mat_rp, file="data/data_t_mat_rp.rda")
