#generic utility functions

require(stringr)
require(dplyr)

slugify <- function(col_names) {
  col_names <- tolower( gsub( '\\.', '_', str_trim(col_names) ) )
  return( gsub('(_)$|^(_)', '', col_names) )
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

toDate <- function(col) {
  as.POSIXct(col, format = "%m/%d/%Y")
}

getOneYear <- function(df, date_col, from_date) {
  #`df` data frame to subset out into one year-long period
  #`date_col` column in said data frame which has the date by which you'd like to subset it out. do not pass this in quotes
  #`from` month of the reporting period, should be formatted "mmm yyyy"
  
  date_col <- eval(substitute(date_col), envir = df)
  l <- as.Date((as.yearmon(from_date) - 1), format = "%b %Y") #sets to reporting period minus one year
  u <- as.Date((as.yearmon(from_date) + .1), format = "%b %Y") #sets to reporting period plus one month
  df <- filter(df, date_col >= ymd(l) & date_col < ymd(u))
  return(df)
}
