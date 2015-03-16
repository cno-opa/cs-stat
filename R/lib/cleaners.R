#
#
# Generic cleaner functions for the following input types:
#   * Service reports
#   * LAMA permits
#   * LAMA licenses
#   * LAMA violations
#   * LAMA report inspections
#
#

#TODO: Data audit

require(lubridate)
require(dplyr)


clean <- function(df, datecols, datein, FUN = toDate, subset = TRUE) {
  for(i in datecols) {
    df[,i] <- FUN(df[,i])
  }
  df <- my(df, datein, subset)
  names(df) <- slugify(names(df))

  return(df)
}

cleanServiceReport <- function(df) {
  df <- clean(df, "dateIn", "dateIn", mdy)
  df$queue <- as.character(df$queue)
  return(df)
}

cleanPermits <- function(df, subset = TRUE) {
  df <- clean(df, c("FilingDate", "IssueDate"), "FilingDate", subset = subset)
  df$daystoissue <- as.numeric(ymd(df$issuedate) - ymd(df$filingdate))/86400
  df <- filter(df, daystoissue >= 0 | !is.na(daystoissue)) #remove any entries with negative days to issue or nas
  df$type <- as.character(df$type)
  return(df)
}

cleanLicenses <- function(df) {
  df <- clean(df, c("ApplicationDate", "IssueDate"), "IssueDate")
  df$daystoissue <- as.numeric(ymd(df$issuedate) - ymd(df$applicationdate))/86400
  df$type <- as.character(df$type)
  return(df)
}

cleanInspections <- function(df, subset = TRUE) {
  df <- clean(df, c("Date", "Requested"), "Date", mdy, subset)
  return(df)
}

cleanComplaints <- function(df) {
  df <- clean(df, c("D_Filed", "FirstInspection"), "FirstInspection")
  df$daystoinspect <- as.numeric(ymd(df$firstinspection) - ymd(df$d_filed))/86400
  df$daystoinspect <- ifelse(df$daystoinspect < 0, NA, df$daystoinspect)
  return(df)
}
