#' @title Example data for table_rp
#' 
#' @description An example data source that is used for
#' demonstrating the rp_table function.
#' 
#' @author Andrew Ferris
#' 
#' @example data(data_rp_table)
#' 
#' @export

data_rp_table <- data.frame(c("A1", "A1", "A1", "B1", "C1"),
                            c("A2", "C2", "C2", "A2", "B2"),
                            c("C3", "C3", "A3", "A3", "A3"),
                            c("C4", "C4", "A4", "B4", "C4"),
                            c(1,2,3,4,5),
                            stringsAsFactors = F)
row.names(data_rp_table) <- c("R1", "R2", "R3", "R4", "R5")
colnames(data_rp_table) <- c("Col1", "Col2", "Col3", "Col4", "Col5")
data_rp_table <- as.matrix(data_rp_table)
saveRDS(object = data_rp_table, file = "data_rp_table.rds")