#clean.R
#loads, cleans, and bins data for plotting

#TODO: audit oss data to remove outliers. resolve NAs in permits$opa_category. build generic cleaner functions for 1) permits, 2) licenses, 3) inspections, 4) service reports

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
  oss <- my(oss, oss$datein)

  return(oss)
}

cleanPermits <- function() {
  load("./data/context/permits-lookup.Rdata")
  load("./data/context/online-permits.Rdata")
  load("./data/context/historical-usetypes.Rdata")

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
  permits <- my(permits, permits$filingdate)

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

  categorize <- function(type) {
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
  lic <- my(lic, lic$applicationdate)
  lic$daystoissue <- as.numeric(ymd(lic$issuedate) - ymd(lic$applicationdate))/86400
  lic$type <- as.character(lic$type)
  lic$opa_category <- NA
  for(i in 1:nrow(lic)) {
    lic$opa_category[i] <- categorize(lic$type[i])
  }

  return(lic)
}

cleanRev <- function() {
  names(rev) <- slugify(names(rev))
  rev$queue <- as.character(rev$queue)
  rev$queue[rev$queue == "Account Maint." | rev$queue == "Administration" | rev$queue == "Account Admin."] <- "Account Maintenance and Administration"
  rev$queue[rev$queue == "Business Regist."] <- "Business Intake"
  rev$datein <- mdy(rev$datein)
  rev <- my(rev, rev$datein)

  return(rev)
}

cleanInspectBiz <- function() {
  names(inspect_biz) <- slugify(names(inspect_biz))
  inspect_biz$requested <- mdy(inspect_biz$requested)
  inspect_biz <- my(inspect_biz, inspect_biz$requested)

  return(inspect_biz)
}

cleanInspectBldg <- function() {
  load("./data/context/inspections-bldg-historical-all-2014.Rdata")

  names(inspect_bldg) <- slugify(names(inspect_bldg))
  inspect_bldg$date <- mdy(inspect_bldg$date)
  inspect_bldg$requested <- mdy(inspect_bldg$requested)
  inspect_bldg <- my(inspect_bldg, inspect_bldg$requested, subset = FALSE)

  inspect_bldg_hist <- rbind(inspect_bldg_hist, inspect_bldg)
  inspect_bldg_hist <- filter(inspect_bldg_hist, days >= 0)
  save(inspect_bldg_hist, file = "./data/context/inspections-bldg-historical-recent.Rdata")

  inspect_bldg_hist <- subset(inspect_bldg_hist, inspect_bldg_hist$my %in% levels(inspect_bldg_hist$my)[(length(levels(inspect_bldg_hist$my))-12):length(levels(inspect_bldg_hist$my))])
  return(inspect_bldg_hist)
}

cleanComplaints <- function() {
  categorize <- function(input) {
    if(input == "Building Code" | input == "Working Without Permit" | input == "Imminent Danger of Collapse"){
      "Building"
    }else if(input == "Zoning General" | input == "Zoning - Paving/Parking" | input == "Illegal Sign" | input == "Junk and Debris"){
      "Zoning"
    }else {
      NA
    }
  }

  names(complaints) <- slugify(names(complaints))
  complaints <- filter(complaints, origin == "Business" | origin == "Police" | origin == "Citizen")
  complaints$d_filed <- toDate(complaints$d_filed)
  complaints$firstinspection <- toDate(complaints$firstinspection)
  complaints <- my(complaints, complaints$d_filed)
  complaints$days <- as.numeric(ymd(complaints$firstinspection) - ymd(complaints$d_filed))/86400
  complaints$days <- ifelse(complaints$days < 0, NA, complaints$days)
  complaints$opa_category <- NA
  for(i in 1:nrow(complaints)) {
    complaints$opa_category[i] <- categorize(complaints$type[i])
  }

  return(complaints)
}

cleanHDLCPermits <-function() {
  exclude <- c("HDLC NO Hearing",
               "HDLC CBD Hearing",
               "HDLC NO ARC Meeting",
               "HDLC CBD ARC Meeting",
               "Returned for revision",
               "Materials resubmitted",
               "Review resubmitted materials"
                )

  names(hdlc_permits) <- slugify(names(hdlc_permits))
  hdlc_permits$filingdate <- toDate(hdlc_permits$filingdate)
  hdlc_permits$issuedate <- toDate(hdlc_permits$issuedate)
  hdlc_permits$daystoissue <- as.numeric(hdlc_permits$issuedate - hdlc_permits$filingdate)/86400
  hdlc_permits <- filter(hdlc_permits, !(hdlc_permits$currentstatus %in% exclude))
  hdlc_permits <- filter(hdlc_permits, !(hdlc_permits$nextstatus %in% exclude))
  hdlc_permits <- my(hdlc_permits, hdlc_permits$issuedate)
}

#load
oss <- read.csv("./data/oss-service-report.csv", sep = ";", header = TRUE)
permits <- read.csv("./data/permits.csv", header = TRUE)
rev <- read.csv("./data/revenue.csv", sep = ";", header = TRUE)
lic <- read.csv("./data/licenses.csv", header = TRUE)
inspect_biz <- read.csv("./data/inspections-biz.csv", header = TRUE)
inspect_bldg <- read.csv("./data/inspections-bldg-recent.csv", header = TRUE)
complaints <- read.csv("./data/complaints.csv", header = TRUE)
hdlc_permits <- read.csv("./data/hdlc-permits.csv", header = TRUE)

#execute
oss <- cleanOss()
permits <- cleanPermits()
rev <- cleanRev()
lic <- cleanLic()
inspect_biz <- cleanInspectBiz()
inspect_bldg <- cleanInspectBldg()
complaints <- cleanComplaints()

#save
save(list = ls(), file = "./data/data-cleaned.Rdata")

#
#end init_clean
}
