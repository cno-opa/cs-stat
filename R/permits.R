# wut for permits

cleanIssued <- function(permits) {
  load("./data/context/permits-lookup.Rdata")
  load("./data/context/historical-usetypes.Rdata")
  load("./data/context/online-permits.Rdata")

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
    permits$refcode <- as.character(permits$refcode)
    hist_usetypes$refcode <- as.character(hist_usetypes$refcode)

    u <- which(permits$usetype == "undetermined", arr.ind = TRUE)
    for(i in u) {
      r <- permits$refcode[i]
      lookup <- hist_usetypes$usetype[hist_usetypes$refcode == r]
      if(length(lookup) > 0) {
        permits$usetype[i] <- lookup
      }
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

  #issue time in seconds
  permits$sec_to_issue <- as.POSIXct(permits$IssueDate, format = "%m/%d/%Y %H:%M:%S") - as.POSIXct(permits$FilingDate, format = "%m/%d/%Y %H:%M:%S")

  #master cleanse
  permits <- cleanPermits(permits)

  permits <- filter(permits, !submittaltype == 3) #remove accela entries
  permits <- filter(permits, !grepl("voided", exitreason))
  permits <- filter(permits, !is.na(filingdate))

  #month issued
  permits$month_issued <- as.factor(as.yearmon(permits$issuedate))

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
  permits$online <- tolower(permits$type) %in% online_permits$permit
  permits$opa_category <- permits_lookup$opa_category[match(permits$type, permits_lookup$type)]

  return(permits)
}

# cleanHDLC <- function(permits) {
#   permits <- cleanPermits(permits)
#   return(permits)
# }

plotPermits <- function() {
#
#

theme_set(theme_opa())

resComm <- function() {
  d <- filter(issued, usetype == "residential" | usetype == "commercial") %>%
       group_by(my, usetype) %>%
       summarise(n = n())

  p <- lineOPA(d, "my", "n", "Residential and commercial permits issued", group = "usetype", legend.labels = c("Commercial", "Residential"))
  p <- buildChart(p)
  ggsave("./output/19-permits-res-comm.png", plot = p, width = 10, height = 7.5)
}

resCommIssueTime <- function() {
  d <- filter(issued, usetype == "residential" | usetype == "commercial") %>%
       group_by(month_issued, usetype) %>%
       summarise(mean_to_issue = mean(daystoissue, na.rm = TRUE))

  p <- lineOPA(d, "month_issued", "mean_to_issue", "Mean days to issue for residential and commercial permits", group = "usetype", legend.labels = c("Commercial", "Residential"))
  p <- buildChart(p)
  ggsave("./output/20-permits-res-comm-days-to-issue.png", plot = p, width = 10, height = 7.5)
}

sameDay <- function() {
  d <- filter(issued, opa_category == "Building - All Others" | opa_category == "Building - New Construction") %>%
       mutate(app_method = ifelse(createdby == "publicwebcrm", "online", "in person")) %>%
       mutate(under_48 = ifelse(sec_to_issue < 172800, TRUE, FALSE)) %>%
       group_by(month_issued, app_method, under_48) %>%
       summarise(n = n())

  t <- summarise_each(d, funs(sum))$n
  x <- list()
  for(i in t) {
    x <- append(x, i)
    x <- append(x, i)
  }
  d$total <- as.numeric(x)
  d$prop <- d$n/d$total

  d <- filter(d, under_48 == TRUE)
  d <- melt(d)
  d <- filter(d, variable == "prop")

  p <- lineOPA(d, "month_issued", "value", "Building permits issued within 48 hours of application", group = "app_method", percent = TRUE, legend.labels = c("In Person", "Online"))
  p <- buildChart(p)
  ggsave("./output/21-permits-48-hours.png", plot = p, width = 10, height = 7.5)
}

#execute
resComm()
resCommIssueTime()
sameDay()

#
#
}

# load
# applied <- read.csv("./data/permits-applied.csv", header = TRUE)
issued <- read.csv("./data/permits-issued.csv", header = TRUE)
#hdlc <- read.csv("./data/permits-hdlc.csv", header = TRUE)

# execute
#applied <- cleanPermits(applied)
issued <- cleanIssued(issued)
plotPermits()
