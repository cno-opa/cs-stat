#clean.R
#loads, cleans, and bins data for plotting

#TODO: audit oss data to remove outliers. add in historical acc inputs

require(gdata)
require(lubridate)
require(dplyr)

init_clean <- function() {
#
#

#oss
cleanOss <- function() {
  load("./data/context/oss-lookup.Rdata")

  names(oss) <- slugify(names(oss))
  oss$completedby <- gsub(",", "", oss$completedby)
  oss$datein <- mdy(oss$datein)
  oss$queue <- as.character(oss$queue)
  oss$serviceprovided <- as.character(oss$serviceprovided)
  oss <- filter(oss, lengthofservice < 480) #remove entries that take over 8 hours, or one working day
  oss <- filter(oss, !grepl("appointment", tolower(serviceprovided)) & !grepl("meeting", tolower(serviceprovided))) #remove visits with "meeting" or "appointment"
  oss$category <- as.factor( oss_lookup$category[match(oss$queue, oss_lookup$lookup)] )
  oss$my <- paste(month(oss$datein, label = TRUE), year(oss$datein))
  oss <- arrange(oss, datein)
  oss$my <- factor(oss$my, levels = unique(oss$my))

  return(oss)
}

cleanPermitApps <- function() {

  toDate <- function(col) {
    as.POSIXct(col, format = "%m/%d/%Y")
  }

  assignUseType <- function(landuse, owner) {
    if(landuse == "ACC") {
      if(owner == "") {
        "undetermined"
      } else if(grepl("LLC", owner) |
         grepl("Association", owner) |
         grepl("Board", owner) |
         (sapply(gregexpr("\\W+", owner), length) + 1) > 3
        ) {
        "commercial"
      } else if((sapply(gregexpr("\\W+", owner), length) + 1) <= 3) {
        "residential"
      } else {
        "undetermined"
      }
    } else if(landuse == "HALF" | landuse == "RSFD" | landuse == "RSF2") {
      "residential"
    } else if(landuse == "COMM" | landuse == "N/A" | landuse == "MIXD" | landuse == "RMF" | landuse == "SUMF"){
      "commercial"
    } else {
      NA
    }
  }

  resolveUseType <- function() {
    ask <- function(list) {
      for(i in 1:nrow(list)) {
        cat(paste("Address: ", list$address[i]))
        cat(paste("\nDescription: ", list$descr[i]))
        cat("\n \nresidential, commercial, or undetermined? \n \n")
        input <- readLines("stdin", 1, warn = FALSE)
        list$usetype[i] <- input
      }
    }
    u <- filter(permit_apps, usetype == "undetermined")
    ask(u)
  }

  names(permit_apps) <- slugify(names(permit_apps))
  permit_apps$exitreason <- tolower(permit_apps$exitreason)
  permit_apps$d_exp <- toDate(permit_apps$d_exp)
  permit_apps$filingdate <- toDate(permit_apps$filingdate)
  permit_apps$issuedate <- toDate(permit_apps$issuedate)
  permit_apps$finaldate <- toDate(permit_apps$finaldate)
  permit_apps$currentstatusdate <- toDate(permit_apps$currentstatusdate)
  permit_apps$nextstatusdate <- toDate(permit_apps$filingdate)

  permit_apps <- filter(permit_apps, !submittaltype == 3) #remove accela entries
  permit_apps <- filter(permit_apps, !grepl("voided", exitreason))
  permit_apps <- filter(permit_apps, !is.na(filingdate))

  #determine residential vs.commercial use type
  permit_apps$usetype <- NA
  for(i in 1:nrow(permit_apps)) {
    permit_apps$usetype[i] <- assignUseType(permit_apps$landuseshort[i], permit_apps$owner[i])
  }
  resolveUseType()
}

#load
oss <- read.csv("./data/oss-service-report.csv", sep = ";", header = TRUE)
permit_apps <- read.csv("./data/permit-applications.csv", header = TRUE)

#execute
oss <- cleanOss()
permit_apps <- cleanPermitApps()

#save
save(list = ls(), file = "./data/data-cleaned.Rdata")

#
#end init_clean
}
