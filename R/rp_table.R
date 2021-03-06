#' @name rp_table
#'
#' @title Create a Sankey Plot from Table
#'
#' @description Create a Sankey Plot from Table
#'
#' This function takes a n x m matrix where the first m - 1
#' columns are character factors and the final m column is
#' a numeric value to create a riverplot (sankey plot) over
#' the riverplot function in the package riverplot. At this
#' time style information is missing from the riverplot,
#' with a view to use RColorBrewer to develop palettes
#' which can be used to make the plotting more clear.
#'
#' @section Known Problems:
#' There is no style input at this time. This means that
#' all Sankey plots come out as greyscale.
#' Labels are not wrapped and hard to clear, making the
#' interpretation of the image difficult.
#' Variables are also no ordered, meaning that "rivers"
#' may not be straight even though they should be.
#'
#' @param table A table of class matrix with m colums
#' where the first m - 1 columns are character and the
#' final m column is numeric/integer.
#'
#' @return A Sankey Plot
#' @return The Sankey Plot object, a list of edges and
#' nodes.
#'
#' @author Andrew Ferris
#'
#' @example rp_table(data_rp_table)
#'
#' @export

globalVariables(c("unmatrix", "group_by", "summarise", "par", "plot"))

#load the required packages
library(gdata, quietly = TRUE, warn.conflicts = FALSE)
library(riverplot, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(RColorBrewer, quietly = TRUE, warn.conflicts = FALSE)

rp_table <- function(table = table){

  #create the edges object
  obj <- table[,1:ncol(table) - 1]
  N1 <- gdata::unmatrix(obj[,c(1:ncol(obj) - 1)])
  N2 <- gdata::unmatrix(obj[,c(2:ncol(obj))])
  Value <- rep(table[,c(ncol(table))], ncol(obj) - 1)
  edges <- data.frame(N1, N2, Value, stringsAsFactors = F)
  row.names(edges) <- NULL
  edges <- as.data.frame(edges %>%
                           group_by(N1, N2) %>%
                           summarise(Value = sum(as.integer(Value))))

  #create the nodes objects
  ID = NULL
  x = NULL
  y = NULL
  for(i in 2:ncol(table) - 1){
    ID <- c(ID, unique(table[,i]))
    x <- c(x, rep(i, length(unique(table[,i]))))
    y <- c(y, seq(1, length(unique(table[,i]))))
  }
  nodes <- data.frame(ID, x, y, stringsAsFactors = F)
  rownames(nodes) = nodes$ID

  #create the riverplot objects
  rp <- list(nodes = nodes, edges = edges)
  class(rp) <- c(class(rp), "riverplot")

  #plot object
  op <- par(cex=0.5)
  plot(rp)
  par(op)
  r_list <- list(nodes = nodes, edges = edges)
  return(r_list)
}
