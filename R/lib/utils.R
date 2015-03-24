# utils.R
# generic utility functions used by cleaners.R, plotters.R, and most of the scripts
#
#

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
  #`date_col` column in said data frame which has the month-year date by which you'd like to subset it out. do not pass this in quotes
  #`from` month of the reporting period, should be formatted "mmm yyyy"

  date_col <- eval(substitute(date_col), envir = df)
  date_col <- as.Date(as.yearmon(date_col), format = "%b %Y")
  l <- as.Date((as.yearmon(from_date) - 1), format = "%b %Y") #sets to reporting period minus one year
  u <- as.Date((as.yearmon(from_date) + .1), format = "%b %Y") #sets to reporting period plus one month
  df <- filter(df, ymd(date_col) >= ymd(l) & ymd(date_col) < ymd(u))
  return(df)
}

prettyPercentBreaks <- function(range, n) {
  # returns a range of pretty values in or near range boundaries for small ranges, like when you're dealing with percents
  # note this does not return negative values

  if(min(range) < 0) stop("Sorry, this just returns a range for positive values. Use pretty_breaks() to get a range with negative values")

  diff <- max(range) - min(range)
  incr <- diff/(n)
  ints <- seq(min(range), max(range), incr)

}
