#t_mat_rp
#allows the importation of transition matrices into the sankey plots via the riverplot package

#create a test data frame
transition_matrix <- matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3, ncol = 3)
row.names(transition_matrix) <- c("A", "B", "C")
colnames(transition_matrix) <- c("D", "E", "F")

t_mat_rp <- function(transition_matrix){
  #load the required packages
  library(gdata, quietly = TRUE)
  library(riverplot, quietly = TRUE)
  
  #create the edges object
  N1 <- gsub( "*:.", "", names(unmatrix(transition_matrix)))
  N2 <- gsub( ".*:", "", names(unmatrix(transition_matrix)))
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

t_mat_rp(df)
