#cleaning and plotting for permit issuance

cleanAllPermits <- function() {
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

  permits <- cleanPermits(permits)
  permits$exitreason <- tolower(permits$exitreason)
  permits$refcode <- as.character(permits$refcode)

  permits <- filter(permits, !submittaltype == 3) #remove accela entries
  permits <- filter(permits, !grepl("voided", exitreason))
  permits <- filter(permits, !is.na(filingdate))
  permits <- filter(permits, daystoissue >= 0 | is.na(daystoissue)) #remove any entries with negative days to issue

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

cleanHDLC <- function() {
  exclude <- c("HDLC NO Hearing",
               "HDLC CBD Hearing",
               "HDLC NO ARC Meeting",
               "HDLC CBD ARC Meeting",
               "Returned for revision",
               "Materials resubmitted",
               "Review resubmitted materials"
                )

  hdlc_permits <- cleanPermits(hdlc_permits)
  hdlc_permits <- filter(hdlc_permits, !(hdlc_permits$currentstatus %in% exclude))
  hdlc_permits <- filter(hdlc_permits, !(hdlc_permits$nextstatus %in% exclude))
}

plotPermits <- function() {
#
#

theme_set(theme_opa())

permitOnline <- function() { #slide 15
  all <- filter(permits, online) %>%
         group_by(my, createdby) %>%
         summarise(n = n())

  denom <- summarise(all, all = sum(n))
  numer <- filter(all, createdby == "publicwebcrm") %>%
           summarise(online = sum(n))

  d <- left_join(denom, numer, by = "my")
  d$prop <- d$online/d$all

  ggplot(d, aes(x = my, y = prop, label = percent(prop))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Percent of permit applications recieved online\n\n\n", x = "Month", y = "Percent") +
    geom_text(size = 4, colour = "grey33", vjust = -.5)
    ggsave("./output/15permits-online.png", width = 10, height = 5.5)
}

commResPermit <- function() { #slides 16 and 17
  d <- filter(permits, !is.na(issuedate)) %>%
       group_by(my, usetype, opa_category) %>%
       summarise(n = n())

  p <- ggplot(data = d,
              aes(x = my, y = n, group = opa_category, colour = opa_category)
              ) +
        labs(x = "Month", y = "Number") +
        scale_colour_discrete(name = "")

  p + geom_line(data = filter(d, usetype == "commercial")) +
      labs(title = "Number of commercial permits issued\n\n\n")
      ggsave("./output/16commercial-permits-issued.png", width = 10, height = 5.5)

  p + geom_line(data = filter(d, usetype == "residential")) +
      labs(title = "Number of residential permits issued\n\n\n")
      ggsave("./output/17residential-permits-issued.png", width = 10, height = 5.5)
}

spIssueDays <- function() { #slide 18
  d <- filter(permits, division == "SP", !is.na(issuedate)) %>%
       group_by(my, usetype) %>%
       summarise(n = n(), mean = mean(daystoissue))

  #master
  p <- ggplot(d, aes(x = my, y = mean)) +
       labs(x = "Month", y = "Days") +
       theme(axis.text.x = element_text(angle = 45, hjust = .97))

  #residential
  p + geom_bar(data = filter(d, usetype == "residential"), stat = "identity", fill = "steelblue") +
      labs(title = "Average number of days to issue residential permits\n\n\n") +
      geom_hline(aes(yintercept = 8), colour = "orange", linetype = "dashed") +
      geom_hline(aes(yintercept = 17.9), colour = "tomato", linetype = "dashed")
      ggsave("./output/18days-to-issue-r.png", width = 10, height = 5.5)

  #commercial
  p + geom_bar(data = filter(d, usetype == "commercial"), stat = "identity", fill = "steelblue") +
      labs(title = "Average number of days to issue commercial permits\n\n\n") +
      geom_hline(aes(yintercept = 15), colour = "orange", linetype = "dashed") +
      geom_hline(aes(yintercept = 37.2), colour = "tomato", linetype = "dashed")
      ggsave("./output/18days-to-issue-c.png", width = 10, height = 5.5)
}

spIssueDaysDist <- function() { #slide 19
  permits$dayscat <- cut(permits$daystoissue,
                          c(0,15,30, Inf),
                          right = FALSE,
                          labels = c("< 15", "15 - 30", "> 30"))

  d <- filter(permits, division == "SP", !is.na(issuedate)) %>%
       group_by(my, usetype, dayscat) %>%
       summarise(n = n())

  #master
  p <- ggplot(d, aes(x = my, y = n, fill = dayscat)) +
      labs(x = "Month", y = "Number")

  #commercial
  p + geom_bar(data = filter(d, usetype == "commercial"), stat = "identity") +
      labs(title = "Distribution of days to issue commercial permits\n\n\n")
      ggsave("./output/19dist-days-to-issue-c.png", width = 10, height = 5.5)

  #residential
  p + geom_bar(data = filter(d, usetype == "residential"), stat = "identity") +
      labs(title = "Distribution of days to issue residential permits\n\n\n")
      ggsave("./output/19dist-days-to-issue-r.png", width = 10, height = 5.5)
}

permitsOneDay <- function() { #slide 20
  d <- filter(permits, division == "SP",
              opa_category == "Building - All Others" |
              opa_category == "Building - New Construction",
              daystoissue < 1) %>%
       group_by(my, online) %>%
       summarise(n = n())

  denom <- summarise(group_by(d, my), sum = sum(n))
  d <- left_join(d, denom, by = "my")
  d$prop <- d$n/d$sum

  ggplot(d, aes(x = my, y = prop, fill = online, label = percent(prop))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
    labs(title = "Percent of building permits issued within one day of application\n\n\n", x = "Month", y = "Percent") +
    geom_text(size = 4, colour = "grey33", vjust = -.5) +
    scale_fill_discrete(name = "Application method", labels = c("In person", "Online"))
    ggsave("./output/20-one-day-building-permits.png", width = 10, height = 5.5)
}

hdlcPermitsCharts <- function() { # slide 28
  d <- group_by(hdlc_permits, my) %>%
       summarise(mean = mean(daystoissue, na.rm = TRUE), nperstaff = n()/3) %>%
       melt()

  ggplot(d, aes(x = my, y = value, group = variable, colour = variable)) +
  geom_line() +
  labs(title = "Days to review applications and number of applications\n\n\n", x = "Month", y = "") +
  scale_colour_discrete(name = "", labels = c("Average number of days to review application", "Applications per staff"))
  ggsave("./output/28-hdlc-building-permits.png", width = 10, height = 5.5)
}

#plot calls
permitOnline()
commResPermit()
spIssueDays()
spIssueDaysDist()
permitsOneDay()
hdlcPermitsCharts()

#
#
}

#load
permits <- read.csv("./data/permits.csv", header = TRUE)
hdlc_permits <- read.csv("./data/hdlc-permits.csv", header = TRUE)

#execute
permits <- cleanAllPermits()
hdlc_permits <- cleanHDLC()
plotPermits()
