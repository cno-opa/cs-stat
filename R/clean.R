#clean.R
#loads, cleans, and bins data for plotting

#TODO: audit oss data to remove outliers. resolve NAs in permits$opa_category

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
  oss <- subset(oss, oss$my %in% levels(oss$my)[(length(levels(oss$my))-12):length(levels(oss$my))])

  return(oss)
}

cleanPermits <- function() {
  load("./data/context/permits-lookup.Rdata")
  load("./data/context/online-permits.Rdata")
  load("./data/context/historical-usetypes.Rdata")

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

  lookupUseType <- function() {
    u <- which(permits$usetype == "undetermined", arr.ind = TRUE)
    for(i in u) {
      r <- permits$refcode[i]
      lookup <- hist_usetypes$usetype[hist_usetypes$refcode == r]
      permits$usetype[i] <- lookup
    }
    return(permits)
  }

  resolveUseType <- function() {
    u <- which(permits$usetype == "undetermined", arr.ind = TRUE)
    for(i in u) {
      cat(paste("Address: ", permits$address[i]))
      cat(paste("\nDescription: ", permits$descr[i]))
      cat("\n \nresidential, commercial, or undetermined? Enter r, c, or u \n \n")
      input <- readLines("stdin", 1, warn = FALSE)
      if(input == "r") {
        input <- "residential"
      } else if( input == "c") {
        input <- "commercial"
      } else {
        input <- "undetermined"
      }
      permits$usetype[i] <- input
    }
    return(permits)
  }

  hist_usetypes$refcode <- as.character(hist_usetypes$refcode)
  hist_usetypes$usetype <- as.character(hist_usetypes$usetype)

  names(permits) <- slugify(names(permits))
  permits$exitreason <- tolower(permits$exitreason)
  permits$type <- as.character(permits$type)
  permits$refcode <- as.character(permits$refcode)
  permits$d_exp <- toDate(permits$d_exp)
  permits$filingdate <- toDate(permits$filingdate)
  permits$issuedate <- toDate(permits$issuedate)
  permits$finaldate <- toDate(permits$finaldate)
  permits$currentstatusdate <- toDate(permits$currentstatusdate)
  permits$nextstatusdate <- toDate(permits$nextstatusdate)
  permits$daystoissue <- as.numeric((ymd(permits$issuedate) - ymd(permits$filingdate))/86400)
  permits$my <- paste(month(permits$filingdate, label = TRUE), year(permits$filingdate))
  permits <- arrange(permits, filingdate)
  permits$my <- factor(permits$my, levels = unique(permits$my))
  permits <- subset(permits, permits$my %in% levels(permits$my)[(length(levels(permits$my))-12):length(levels(permits$my))])

  permits <- filter(permits, !submittaltype == 3) #remove accela entries
  permits <- filter(permits, !grepl("voided", exitreason))
  permits <- filter(permits, !is.na(filingdate))

  #determine residential vs.commercial use type
  permits$usetype <- NA
  for(i in 1:nrow(permits)) {
    permits$usetype[i] <- assignUseType(permits$landuseshort[i], permits$owner[i])
  }
  permits <- lookupUseType()
  permits <- resolveUseType()
  hist_usetypes <- data.frame(refcode = permits$refcode, usetype = permits$usetype)
  save(hist_usetypes, file = "./data/context/historical-usetypes.Rdata") #save it for posterity

  #permit type recode and online lookup
  permits$opa_category <- permits_lookup$opa_category[match(permits$type, permits_lookup$type)]
  permits$online <- tolower(permits$type) %in% online_permits$permit

  return(permits)
}

cleanLic <- function() {

  toDate <- function(col) {
    as.POSIXct(col, format = "%m/%d/%Y")
  }

  opaCategorize <- function(type) {
    if (type == "Business License" | type == "Temporary Business License") {
      "Business"
    } else if (type == "Driver CPNC" | type == "Tour Guide") {
      "CPNC"
    } else if (type == "Electrical A - Contractor" |
               type == "Electrical C - Maintenance" |
               type == "Electrical D - Journeyman" |
               type == "Electrical E - Helper"
              ) {
      "Electrical"
    } else if (type == "Mechanical A - AC and Refrigeration" |
               type == "Mechanical A - Master Gasfitter" |
               type == "Mechanical B - Journeyman Gasfitter" |
               type == "Mechanical B - Journeyman Mechanic"
               ) {
      "Mechanical"
    } else {
      NA
    }
  }

  names(lic) <- slugify(names(lic))
  lic$applicationdate <- toDate(lic$applicationdate)
  lic$issuedate <- toDate(lic$issuedate)
  lic$my <- paste(month(lic$applicationdate, label = TRUE), year(lic$applicationdate))
  lic <- arrange(lic, applicationdate)
  lic$my <- factor(lic$my, levels = unique(lic$my))
  lic$daystoissue <- as.numeric(ymd(lic$issuedate) - ymd(lic$applicationdate))/86400
  lic$type <- as.character(lic$type)
  lic$opa_category <- NA
  for(i in 1:nrow(lic)) {
    lic$opa_category[i] <- opaCategorize(lic$type[i])
  }
  lic <- subset(lic, lic$my %in% levels(lic$my)[(length(levels(lic$my))-12):length(levels(lic$my))])

  return(lic)
}

cleanRev <- function() {
  names(rev) <- slugify(names(rev))
  rev$queue <- as.character(rev$queue)
  rev$queue[rev$queue == "Account Maint." | rev$queue == "Administration" | rev$queue == "Account Admin."] <- "Account Maintenance and Administration"
  rev$queue[rev$queue == "Business Regist."] <- "Business Intake"
  rev$datein <- mdy(rev$datein)
  rev$my <- paste(month(rev$datein, label = TRUE), year(rev$datein))
  rev <- arrange(rev, datein)
  rev$my <- factor(rev$my, levels = unique(rev$my))
  rev <- subset(rev, rev$my %in% levels(rev$my)[(length(levels(rev$my))-12):length(levels(rev$my))])

  return(rev)
}

cleanInspectBiz <- function() {
  names(inspect_biz) <- slugify(names(inspect_biz))
  inspect_biz$requested <- mdy(inspect_biz$requested)
  inspect_biz$my <- paste(month(inspect_biz$requested, label = TRUE), year(inspect_biz$requested))
  inspect_biz <- arrange(inspect_biz, requested)
  inspect_biz$my <- factor(inspect_biz$my, levels = unique(inspect_biz$my))
  inspect_biz <- subset(inspect_biz, inspect_biz$my %in% levels(inspect_biz$my)[(length(levels(inspect_biz$my))-12):length(levels(inspect_biz$my))])
  return(inspect_biz)
}

cleanInspectBldg <- function() {
  load("./data/context/inspections-bldg-historical.Rdata")

  names(inspect_bldg) <- slugify(names(inspect_bldg))
  inspect_bldg$date <- mdy(inspect_bldg$date)
  inspect_bldg$requested <- mdy(inspect_bldg$requested)
  inspect_bldg$my <- paste(month(inspect_bldg$requested, label = TRUE), year(inspect_bldg$requested))
  inspect_bldg <- arrange(inspect_bldg, requested)
  inspect_bldg$my <- factor(inspect_bldg$my, levels = unique(inspect_bldg$my))

  inspect_bldg_hist <- rbind(inspect_bldg_hist, inspect_bldg)
  inspect_bldg_hist <- filter(inspect_bldg_hist, days >= 0)
  save(inspect_bldg_hist, file = "./data/context/inspections-bldg-historical.Rdata")

  inspect_bldg_hist <- subset(inspect_bldg_hist, inspect_bldg_hist$my %in% levels(inspect_bldg_hist$my)[(length(levels(inspect_bldg_hist$my))-12):length(levels(inspect_bldg_hist$my))])
  return(inspect_bldg_hist)
}

#load
oss <- read.csv("./data/oss-service-report.csv", sep = ";", header = TRUE)
permits <- read.csv("./data/permits.csv", header = TRUE)
rev <- read.csv("./data/revenue.csv", sep = ";", header = TRUE)
lic <- read.csv("./data/licenses.csv", header = TRUE)
inspect_biz <- read.csv("./data/inspections-biz.csv", header = TRUE)
inspect_bldg <- read.csv("./data/inspections-bldg-recent.csv", header = TRUE)

#execute
oss <- cleanOss()
permits <- cleanPermits()
rev <- cleanRev()
lic <- cleanLic()
inspect_biz <- cleanInspectBiz()
inspect_bldg <- cleanInspectBldg()

#save
save(list = ls(), file = "./data/data-cleaned.Rdata")

#
#end init_clean
}
