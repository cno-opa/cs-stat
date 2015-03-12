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

cleanPermits <- function(df) {
  load("./data/context/online-permits.Rdata")
  df <- clean(df, c("FilingDate", "IssueDate"), "FilingDate")
  df$daystoissue <- as.numeric(ymd(df$issuedate) - ymd(df$filingdate))/86400
  df$type <- as.character(df$type)
  df <- filter(df, !submittaltype == 3) #remove accela entries
  df <- filter(df, !grepl("voided", exitreason))
  df <- filter(df, !is.na(filingdate))
  df <- filter(df, daystoissue >= 0 | !is.na(daystoissue)) #remove any entries with negative days to issue or nas
  df$online <- tolower(df$type) %in% online_permits$permit
  return(df)
}

cleanLicenses <- function(df) {
  df <- clean(df, c("ApplicationDate", "IssueDate"), "IssueDate")
  df$daystoissue <- as.numeric(ymd(df$issuedate) - ymd(df$applicationdate))/86400
  df$type <- as.character(df$type)
  return(df)
}

cleanInspections <- function(df, subset = TRUE) {
  df <- clean(df, "Requested", "Requested", mdy, subset)
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
