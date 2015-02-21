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

my <- function(df, datecol, subset = TRUE) {
  df$my <- paste(month(df[,datecol], label = TRUE), year(df[,datecol]))
  df <- df[order(df[,datecol]),]
  df$my <- factor(df$my, levels = unique(df$my))

  #filter for last 12 months
  if(subset == TRUE){
    df <- subset(df, df$my %in% levels(df$my)[(length(levels(df$my))-12):length(levels(df$my))])
  }

  return(df)
}
