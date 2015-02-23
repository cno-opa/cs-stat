#clean and plot licenses data

cleanAllLicenses <- function() {
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

  lic <- cleanLicenses(lic)
  lic$opa_category <- NA
  for(i in 1:nrow(lic)) {
    lic$opa_category[i] <- categorize(lic$type[i])
  }

  return(lic)
}


plotLicenses <- function() {
#
#

theme_set(theme_opa())

busLicOnline <- function() { #slide 15 also
  all <- filter(lic, type == "Business License" | type == "Temporary Business License") %>%
         group_by(my, createdby) %>%
         summarise(n = n())

  denom <- summarise(all, all = sum(n))
  numer <- filter(all, createdby == "publicwebcrm") %>%
           summarise(online = sum(n))

  d <- left_join(denom, numer, by = "my")
  d$prop <- d$online/d$all

  ggplot(d, aes(x = my, y = prop, label = percent(prop))) +
    geom_bar(stat = "identity", fill = "goldenrod") +
    labs(title = "Percent of business license applications recieved online", x = "Month", y = "Percent") +
    geom_text(size = 4, colour = "grey33", vjust = -.5)
    ggsave("./output/15bus-license-online.png", width = 10, height = 5.5)
}

licOther <- function() { #slide 22
  d <- filter(lic, !is.na(issuedate)) %>%
       group_by(my, opa_category) %>%
       summarise(n = n(),
       meanissue = mean(daystoissue, na.rm = TRUE),
       sameday = sum(daystoissue == 0)) %>%
       melt()

  #master
  p <- ggplot(d, aes(x = my, y = value, fill = variable)) +
       labs(x = "Month")

  #business licenses
  p + geom_bar(data = filter(d, opa_category == "Business",
               variable == "n"),
               stat = "identity") +
      labs(title = "Business licenses issued", y = "Number") +
      guides(fill = FALSE)
      ggsave("./output/22bus-n-permits.png", width = 10, height = 5.5)

  p + geom_bar(data = filter(d, opa_category == "Business",
               variable == "meanissue"),
               stat = "identity") +
      labs(title = "Average number of days to issue", y = "Days") +
      guides(fill = FALSE)
      ggsave("./output/22bus-mean-days-permits.png", width = 10, height = 5.5)


  #electrical
  p + geom_bar(data = filter(d, opa_category == "Electrical",
               variable == "n" | variable == "sameday"),
               stat = "identity",
               position = "identity") +
      labs(title = "Number of same day electrical permits issued", y = "Number") +
      scale_fill_discrete(name = "", labels = c("All permits issued", "Issued same day"))
      ggsave("./output/22electrical-same-day.png", width = 10, height = 5.5)

  #mechanical
  p + geom_bar(data = filter(d, opa_category == "Mechanical",
              variable == "n" | variable == "sameday"),
              stat = "identity",
              position = "identity") +
      labs(title = "Number of same day mechanical permits issued", y = "Number") +
      scale_fill_discrete(name = "", labels = c("All permits issued", "Issued same day"))
      ggsave("./output/22mechanical-same-day.png", width = 10, height = 5.5)
}

licCpnc <- function() { #slide 23
  d <- filter(lic, !is.na(issuedate), opa_category == "CPNC") %>%
       group_by(my, type) %>%
       summarise(n = n(), meanissue = mean(daystoissue))

  #master
  p <- ggplot(d, aes(x = my, fill = type)) +
       labs(x = "Month") +
       scale_fill_discrete(name = "")

  #number of permits
  p + geom_bar(aes(y = n), stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
      labs(title = "Number of CPNC permits issued", y = "Number") +
      geom_text(size = 4, colour = "grey33", vjust = -.5, aes(label = n, y = n))
      ggsave("./output/23cpnc-number-permits.png", width = 10, height = 5.5)

  #average days to issue
  p + geom_bar(aes(y = meanissue), stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
      labs(title = "Average number of days to issue CPNC permit", y = "Days") +
      geom_text(size = 4, colour = "grey33", vjust = -.5, aes(label = round(meanissue), y = meanissue))
      ggsave("./output/23cpnc-days-issue-permits.png", width = 10, height = 5.5)
}

#plot calls
busLicOnline()
licOther()
licCpnc()

#
#
}

#load
lic <- read.csv("./data/licenses.csv", header = TRUE)

#execute
lic <- cleanAllLicenses()
plotLicenses()
