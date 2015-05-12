# permits.R
#
# data sources:
# ============================
#
# permits-applied.csv - csv of permits applied for, but not neccessarily issued. Generated from LAMA permits module query.
# permits-issued.csv - csv of permits issued. Generated from LAMA permit module query.
# permits-hdlc.csv - csv of HDLC permits only. Generated from LAMA permit module query.
#
# ============================
#
#


# clean
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

  # issue time in seconds
  permits$sec_to_issue <- as.POSIXct(permits$IssueDate, format = "%m/%d/%Y %H:%M:%S") - as.POSIXct(permits$FilingDate, format = "%m/%d/%Y %H:%M:%S")

  #master cleanse
  permits <- cleanPermits(permits)

  permits <- filter(permits, !submittaltype == 3) #remove accela entries
  permits <- filter(permits, !grepl("voided", exitreason))
  permits <- filter(permits, !is.na(filingdate))

  # determine residential vs.commercial use type
  permits$usetype <- sapply(permits$landuseshort, assignUseType, owner = permits$owner)
  permits <- lookupUseType()
  permits <- resolveUseType()
  hist_usetypes <- data.frame(refcode = permits$refcode, usetype = permits$usetype)
  save(hist_usetypes, file = "./data/context/historical-usetypes.Rdata") #save it for posterity

  # permit type recode and online lookup
  permits$online <- tolower(permits$type) %in% online_permits$permit
  permits$opa_category <- permits_lookup$opa_category[match(permits$type, permits_lookup$type)]

  return(permits)
}

cleanHDLC <- function(permits) {
  exclude <- read.csv("./data/permits-hdlc-exclude.csv", header = FALSE)
  colnames(exclude) <- c("numstring", "filed", "issued", "type", "landuse", "event")
  permits <- cleanPermits(permits)
  exclude <- filter(exclude, event != "NULL")
  permits <- filter(permits, !(permits$numstring %in% exclude$numstring))

  return(permits)
}

set_kpis <- function() {
  load("./data/kpi.Rdata")
  cutoff <- dateFromYearMon(r_period)
  cutup <- ymd(paste(
              strsplit(r_period, " ")[[1]][2],
              "01",
              "01",
              sep = "-"
              ))

  online <- applied[applied$filingdate <= cutoff & applied$filingdate >= cutup & applied$online == TRUE,] %>%
            summarise(measure = "Percent of permits applied for online", value = sum(createdby == "publicwebcrm")/n())

  median_comm <- filter(issued, usetype == "commercial" & issuedate <= cutoff & issuedate >= cutup) %>%
                 summarise(measure = "mean days to issue commercial permit", value = mean(daystoissue, na.rm = TRUE))

  median_res <- filter(issued, usetype == "residential" & issuedate <= cutoff & issuedate >= cutup) %>%
                summarise(measure = "mean days to issue residential permit", value = mean(daystoissue, na.rm = TRUE))

  kpi <- rbind(kpi,
               online,
               median_comm,
               median_res
              )

  save(kpi, file = "./data/kpi.Rdata")
}

# plot
plotPermits <- function() {
#
#

theme_set(theme_opa())

resComm <- function() {
  d <- getTwoYears(issued, month_end, r_period) %>%
       filter(usetype == "residential" | usetype == "commercial") %>%
       group_by(month_end, usetype) %>%
       summarise(n = n())

  p <- lineOPA(d, "month_end", "n", "Residential and commercial permits issued", group = "usetype", legend.labels = c("Commercial", "Residential"), labels = "format(n, big.mark = \",\", scientific = FALSE)")
  p <- buildChart(p)
  ggsave("./output/26-permits-res-comm.png", plot = p, width = 7.42, height = 5.75)
}

resCommIssueTime <- function() {
  d <- getTwoYears(issued, month_end, r_period) %>%
       filter(usetype == "residential" | usetype == "commercial") %>%
       group_by(month_end, usetype) %>%
       summarise(mean_to_issue = mean(daystoissue, na.rm = TRUE))

  p <- lineOPA(d, "month_end", "mean_to_issue", "Mean days to issue for residential and commercial permits", group = "usetype", legend.labels = c("Commercial", "Residential"), labels = "round(mean_to_issue)")
  p <- buildChart(p)
  ggsave("./output/27-permits-res-comm-days-to-issue.png", plot = p, width = 7.42, height = 5.75)
}

sameDay <- function() {
  d <- getTwoYears(issued, month_end, r_period) %>%
       filter(opa_category == "Building - All Others" | opa_category == "Building - New Construction") %>%
       mutate(app_method = ifelse(createdby == "publicwebcrm", "online", "in person")) %>%
       mutate(under_48 = ifelse(sec_to_issue < 172800, TRUE, FALSE)) %>%
       group_by(month_end, app_method, under_48) %>%
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

  p <- lineOPA(d, "month_end", "value", "Percent of building permits issued within 48 hours of application", group = "app_method", percent = TRUE, legend.labels = c("In Person", "Online"), labels = "percent(value)")
  p <- buildChart(p)
  ggsave("./output/28-permits-48-hours.png", plot = p, width = 7.42, height = 5.75)
}

hdlcReview <- function() {
  d <- getTwoYears(hdlc, month_end, r_period) %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(daystoissue <= 3)) %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Number and days to issue HDLC permits", fill = "variable", legend.labels = c("Issued in more than three days", "Issued in three days"))
  p <- buildChart(p)
  ggsave("./output/29-permits-hdlc.png", plot = p, width = 7.42, height = 5.75)
}

# execute
resComm()
resCommIssueTime()
sameDay()
hdlcReview()

#
#
}

# load
issued <- read.csv("./data/permits-issued.csv", header = TRUE)
hdlc <- read.csv("./data/permits-hdlc.csv", header = TRUE)
applied <- read.csv("./data/permits-applied.csv", header = TRUE)

# execute

issued <- cleanIssued(issued)
hdlc <- cleanHDLC(hdlc)
applied <- cleanPermits(applied)
  load("./data/context/online-permits.Rdata")
  applied$online <- tolower(applied$type) %in% online_permits$permit
plotPermits()
set_kpis()
