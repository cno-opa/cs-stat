#generic utility functions

require(stringr)

slugify <- function(col_names) {
  col_names <- tolower( gsub( '\\.', '_', str_trim(col_names) ) )
  return( gsub('(_)$|^(_)', '', col_names) )
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

monthStrToNum <- function(str) {
  str <- tolower(str)
  months <- list("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  match(str, months)
}

properize <- function(str) {
  f <- toupper( substr(str, 1, 1) )
  r <- substr(str, 2, nchar(str))
  return( paste0(f, r) )
}
