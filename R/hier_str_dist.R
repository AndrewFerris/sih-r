#' @name hier_str_dist
#' 
#' @title Hierarchical String Distance
#' 
#' @description This function uses traditional Levenshtein distances in an ordinal framework to formalise an asymmetric distance between String A and String B. It is specific to the work of the VERRC project and therefore caveats that the two strings are of equal length. Therefore insertions and deletions are not applied and it is 
#' 
#' @param str_a A character vector
#' @param str_b A character vector
#' 
#' @return distance An integer representing the hierarchical Levenshtein distance between the two strings
#' 
#' @import stringdist
#' 
#' @usage hier_str_dist(str_a = str_a, str_b = str_b)
#' 
#' @example hier_str_dist(str_a = "teststringa", str_b = "teststringb")
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
  
  # Calculate the length of each string
  len_a <- length(string_a)
  len_b <- length(string_b)
  
  # Test that the lengths of the two strings are the same
  if(len_a != len_b){
    stop("The two strings are not of the same length.")
  }
  
  # Create a letter_number function to convert each letter to its respective number
  letter_number <- function(x){
    utf8ToInt(x) - utf8ToInt("a") + 1L
  }
  
  # Use the letter_number function on each string
  string_a <- sapply(string_a, letter_number)
  string_b <- sapply(string_b, letter_number)
  
  # Initialise the distance vector to 0
  str_distance <- 0
  
  # Begin the process of checking each element
  for(i in 1:len_a){
    if(string_a[i] < string_b[i]){
      str_distance <- str_distance + 1
    }
  }
  
  # Create a list to return
  z <- list(string_a, string_b, len_a, len_b, str_distance)
  
  # Return the string distance
  return(z)
  
}