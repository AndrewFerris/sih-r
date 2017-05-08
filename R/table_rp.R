table_rp <- function(table = table){
  #load the required packages
  library(gdata, quietly = TRUE)
  library(riverplot, quietly = TRUE)
  library(dplyr, quietly = TRUE)

  #create the edges object
  obj <- unique(t(apply(table[,1:ncol(table) - 1], 1, sort)))
  N1 <- unmatrix(obj[,c(1:ncol(obj) - 1)])
  N2 <- unmatrix(obj[,c(2:ncol(obj))])
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
  for(i in 2:ncol(data) - 1){
    ID <- c(ID, unique(data[,i]))
    x <- c(x, rep(i, length(unique(data[,i]))))
    y <- c(y, seq(1, length(unique(data[,i]))))
  }
  nodes <- data.frame(ID, x, y, stringsAsFactors = F)
  rownames(nodes) = nodes$ID
  nodes

  #create the riverplot objects
  rp <- list(nodes = nodes, edges = edges)
  class(rp) <- c(class(rp), "riverplot")

  #plot object
  plot(rp)
  r_list <- list(nodes = nodes, edges = edges)
  return(r_list)
}

data <- data.frame(c("A1", "A1", "A1", "B1", "C1"),
                   c("A2", "C2", "C2", "A2", "B2"),
                   c("C3", "C3", "A3", "A3", "A3"),
                   c("C4", "C4", "A4", "B4", "C4"),
                   c(1,2,3,4,5),
                   stringsAsFactors = F)
row.names(data) <- c("R1", "R2", "R3", "R4", "R5")
colnames(data) <- c("Col1", "Col2", "Col3", "Col4", "Col5")
data <- as.matrix(data)
temp <- table_rp(data)
str(temp)


ID = NULL
x = NULL
y = NULL
for(i in 2:ncol(data) - 1){
 ID <- c(ID, unique(data[,i]))
 x <- c(x, rep(i, length(unique(data[,i]))))
 y <- c(y, seq(1, length(unique(data[,i]))))
}
nodes <- data.frame(ID, x, y, stringsAsFactors = F)
rownames(nodes) = nodes$ID
nodes
