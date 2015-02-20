#generic utility functions

require(stringr)
require(dplyr)

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

toDate <- function(col) {
  as.POSIXct(col, format = "%m/%d/%Y")
}

my <- function(df, datecol, subset = TRUE) {
  df$my <- paste(month(datecol, label = TRUE), year(datecol))
  df <- arrange(df, datecol)
  df$my <- factor(df$my, levels = unique(df$my))

  #filter for last 12 months
  if(subset == TRUE){
    df <- subset(df, df$my %in% levels(df$my)[(length(levels(df$my))-12):length(levels(df$my))])
  }

  return(df)
}
