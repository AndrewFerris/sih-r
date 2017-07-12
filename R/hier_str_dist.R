#' @name hier_str_dist
#' 
#' @title Hierarchical String Distance
#' 
#' @description This function uses traditional Levenshtein distances in an ordinal framework to formalise a distance between any two character vectors.
#' 
#' @param str_a A character vector
#' @param str_b A character vector
#' 
#' @return distance An integer representing the hierarchical Levenshtein distance between the two strings
#' 
#' @import stringdist
#' 
#' @author Andrew Ferris
#' 
#' @export
NULL

hier_str_dist <- function(str_a, str_b){
  # Check that both arguments are of class character
  if(class(str_a) != "character"){
    stop("String A is not of class character.")
  }
  if(class(str_b) != "character"){
    stop("String B is not of class character.")
  }
  
  # Split both strings into a vector for each letter
  string_a <- strsplit(x = str_a, split = "")[[1]]
  string_b <- strsplit(x = str_b, split = "")[[1]]
  
  # Create a letter_number function to convert each letter to its respective number
  letter_number <- function(x){
    utf8ToInt(x) - utf8ToInt("a") + 1L
  }
  
  # Use the letter_number function on each string
  string_a <- sapply(string_a, letter_number)
  string_b <- sapply(string_b, letter_number)
  
  # Calculate the length of each string
  len_a <- length(string_a)
  len_b <- length(string_b)
  
  # Initialise the distance vector to 0
  str_distance <- 0
  
  # Begin the process of checking each element
  if(len_a >= len_b){
    for(i in length(len_a)){
      if(string_a[i] >= string_b[i]){
        string_a[i] <- string_a[i]
      } else {
        string_a[i] <- string_b[i]
        str_distance <- str_distance + 1
      }
    }
  }
  
  # Return the string distance
  return(str_distance)
  
}