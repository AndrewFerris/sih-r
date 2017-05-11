.onAttach <- function(...){
  packageStartupMessage("Functions developed to increase the accessibility\n")
  packageStartupMessage("of the wider R programming community by the\n")
  packageStartupMessage("Centre for Translational Data Science\n")
  packageStartupMessage("Sydney Informatics Hub")
}

globalVariables(c("unmatrix", "%>%", "group_by", "summarise", "par", "plot"))
