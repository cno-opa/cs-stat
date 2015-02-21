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


clean <- function(df, datecols, datein, FUN = toDate) {
  for(i in datecols) {
    df[,i] <- FUN(df[,i])
  }
  df <- my(df, datein)
  names(df) <- slugify(names(df))

  return(df)
}

cleanServiceReport <- function(df) {
  df <- clean(df, "dateIn", "dateIn", mdy)
  df$queue <- as.character(df$queue)
  return(df)
}

cleanPermits <- function(df) {
  df <- clean(df, c("FilingDate", "IssueDate"), "FilingDate")
  df$daystoissue <- as.numeric(ymd(df$issuedate) - ymd(df$filingdate))/86400
  df$type <- as.character(df$type)
  return(df)
}

cleanLicenses <- function(df) {
  df <- clean(df, c("ApplicationDate", "IssueDate"), "ApplicationDate")
  df$daystoissue <- as.numeric(ymd(df$issuedate) - ymd(df$applicationdate))/86400
  df$type <- as.character(df$type)
  return(df)
}

cleanInspections <- function(df) {
  df <- clean(df, "Requested", "Requested", mdy)
  return(df)
}

cleanComplaints <- function(df) {
  df <- clean(df, c("D_Filed", "FirstInspection"), "D_Filed")
  df$daystoinspect <- as.numeric(ymd(df$firstinspection) - ymd(df$d_filed))/86400
  df$daystoinspect <- ifelse(df$daystoinspect < 0, NA, df$daystoinspect)
  return(df)
}

cleanViolations <- function(df) {

}
