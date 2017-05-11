#' @name t_mat_rp
#'
#' @title Create a Sankey Plot from Transition Matrix
#'
#' @description Create a Sankey Plot from Transition Matrix
#'
#' @section Known Problems:
#' There is no style input at this time. This means that
#' all Sankey plots come out as greyscale.
#' Labels are not wrapped and hard to clear, making the
#' interpretation of the image difficult.
#' Variables are also no ordered, meaning that "rivers"
#' may not be straight even though they should be.
#'
#' @param table A table of class matrix with m columns
#' and n rows where each element is the value of the
#' movement.
#'
#' @return A Sankey Plot
#' @return The Sankey Plot object, a list of edges and
#' nodes.
#'
#' @author Andrew Ferris
#'
#' @example t_mat_rp(data_t_mat_rp)
#'
#' @export

globalVariables(c("unmatrix", "plot"))

#load the required packages
library(gdata, quietly = TRUE, warn.conflicts = FALSE)
library(riverplot, quietly = TRUE, warn.conflicts = FALSE)

t_mat_rp <- function(transition_matrix){

  #create the edges object
  N1 <- gsub( "*:.", "", names(gdata::unmatrix(transition_matrix)))
  N2 <- gsub( ".*:", "", names(gdata::unmatrix(transition_matrix)))
  Value <- c(transition_matrix)
  edges <- data.frame(N1, N2, Value, stringsAsFactors = F)
  edges

  #create the nodes objects
  ID = unique(c(edges$N1, edges$N2))
  x <- as.integer(c(rep(1, length(row.names(transition_matrix))), rep(2, length(colnames(transition_matrix)))))
  y <- as.integer(c(seq(length(row.names(transition_matrix))), seq(length(colnames(transition_matrix)))))
  nodes <- data.frame(ID, x, y, stringsAsFactors = F)
  rownames(nodes) = nodes$ID
  nodes

  #create the riverplot objects
  rp <- list(nodes = nodes, edges = edges)
  class(rp) <- c(class(rp), "riverplot")

  #plot object
  plot(rp)
}
