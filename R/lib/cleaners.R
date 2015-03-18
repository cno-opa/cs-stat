require(lubridate)
require(dplyr)
require(zoo)

#set reporting period
# cat("What is the reporting period?/n/n ##(Use mmm yyyy format, please)##")
# period <- readLines("stdin", 1, warn = FALSE)

clean <- function(df, datecols, date_start, FUN = toDate, date_end = NULL) {

  # convert date columns to date objects
  for(i in datecols) {
    df[,i] <- FUN(df[,i])
  }

  # create month-year categorical vectors according to record start date and record completion (end) date
  df$month_start <- as.factor(as.yearmon(df[,date_start]))
  if(!is.null(date_end)) {
    df$month_end <- as.factor(as.yearmon(df[,date_end]))
  }

  names(df) <- slugify(names(df))
  return(df)
}

cleanServiceReport <- function(df) {
  df <- clean(df, "dateIn", "dateIn", mdy)
  df$queue <- as.character(df$queue)
  return(df)
}

cleanPermits <- function(df) {
  df <- clean(df, c("FilingDate", "IssueDate"), "FilingDate", date_end = "IssueDate")
  df$daystoissue <- as.numeric(ymd(df$issuedate) - ymd(df$filingdate))/86400
  df <- filter(df, daystoissue >= 0 | !is.na(daystoissue)) #remove any entries with negative days to issue or nas
  df$type <- as.character(df$type)
  return(df)
}

cleanLicenses <- function(df) {
  df <- clean(df, c("ApplicationDate", "IssueDate"), "ApplicationDate", date_end = "IssueDate")
  df$daystoissue <- as.numeric(ymd(df$issuedate) - ymd(df$applicationdate))/86400
  df$type <- as.character(df$type)
  return(df)
}

cleanInspections <- function(df) {
  df <- clean(df, c("Date", "Requested"), "Requested", mdy, date_end = "Date")
  return(df)
}

cleanComplaints <- function(df) {
  df <- clean(df, c("D_Filed", "FirstInspection"), "D_Filed", date_end = "FirstInspection")
  df$daystoinspect <- as.numeric(ymd(df$firstinspection) - ymd(df$d_filed))/86400
  df$daystoinspect <- ifelse(df$daystoinspect < 0, NA, df$daystoinspect)
  return(df)
}
