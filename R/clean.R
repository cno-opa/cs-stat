#clean.R
#loads, cleans, and bins data for plotting

#TODO: audit oss data to remove outliers. add in historical acc inputs. resolve NAs in permits$opa_category

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

cleanPermits <- function() {
  load("./data/context/permits-lookup.Rdata")
  load("./data/context/online-permits.Rdata")

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
    u <- filter(permits, usetype == "undetermined")
    ask(u)
  }

  names(permits) <- slugify(names(permits))
  permits$exitreason <- tolower(permits$exitreason)
  permits$type <- as.character(permits$type)
  permits$d_exp <- toDate(permits$d_exp)
  permits$filingdate <- toDate(permits$filingdate)
  permits$issuedate <- toDate(permits$issuedate)
  permits$finaldate <- toDate(permits$finaldate)
  permits$currentstatusdate <- toDate(permits$currentstatusdate)
  permits$nextstatusdate <- toDate(permits$nextstatusdate)
  permits$daystoissue <- (ymd(permits$issuedate) - ymd(permits$filingdate))/86400
  permits$my <- paste(month(permits$filingdate, label = TRUE), year(permits$filingdate))
  permits <- arrange(permits, filingdate)
  permits$my <- factor(permits$my, levels = unique(permits$my))

  permits <- filter(permits, !submittaltype == 3) #remove accela entries
  permits <- filter(permits, !grepl("voided", exitreason))
  permits <- filter(permits, !is.na(filingdate))

  #determine residential vs.commercial use type
  permits$usetype <- NA
  for(i in 1:nrow(permits)) {
    permits$usetype[i] <- assignUseType(permits$landuseshort[i], permits$owner[i])
  }
  resolveUseType()

  #permit type recode and online lookup
  permits$opa_category <- permits_lookup$opa_category[match(permits$type, permits_lookup$type)]
  permits$online <- tolower(permits$type) %in% online_permits$permit
}

#load
oss <- read.csv("./data/oss-service-report.csv", sep = ";", header = TRUE)
permits <- read.csv("./data/permits.csv", header = TRUE)

#execute
oss <- cleanOss()
permits <- cleanPermits()

#save
save(list = ls(), file = "./data/data-cleaned.Rdata")

#
#end init_clean
}
