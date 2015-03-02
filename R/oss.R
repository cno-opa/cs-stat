#cleaning and plotting for One Stop Shop

cleanOss <- function() {
  load("./data/context/oss-lookup.Rdata")

  oss <- cleanServiceReport(oss)
  oss$serviceprovided <- as.character(oss$serviceprovided)
  oss <- filter(oss, lengthofservice < 480) #remove entries that take over 8 hours, or one working day
  oss <- filter(oss, !grepl("appointment", tolower(serviceprovided)) & !grepl("meeting", tolower(serviceprovided)))
  oss$category <- as.factor( oss_lookup$category[match(oss$queue, oss_lookup$lookup)] )

  return(oss)
}

plotOss <- function() {
#
#

theme_set(theme_opa())

ossSp <- function() { #slide 10
  d <-  filter(oss, category == "Safety and Permits" |
               category == "Safety and Permits - Electrical" |
               category == "Safety and Permits - Inspections" |
               category == "Safety and Permits - Mechanical" |
               category == "Safety and Permits - Plan Review") %>%
        group_by(my, category) %>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = category, colour = category)) +
        scale_colour_brewer(palette = "Dark2", name = "Category", labels = c("S&P", "S&P - Elec", "S&P - Inspec", "S&P - Mech", "S&P - Plan Rev"))

  #visitors
  p + geom_line(aes(y = n), size = 1) +
      labs( title = "Visitors by month\n\n\n", x = "Month", y = "Visitors" ) +
      ggsave("./output/10oss-sp-visitors.png", width = 10, height = 5.5)

  #mean wait time
  p + geom_line(aes(y = meanwait), size = 1) +
      labs( title = "Average wait time\n\n\n", x = "Month", y = "Minutes" ) +
      ggsave("./output/10oss-sp-wait.png", width = 10, height = 5.5)

  #mean service time
  p + geom_line(aes(y = meanserve), size = 1) +
      labs( title = "Average service time\n\n\n", x = "Month", y = "Minutes" ) +
      ggsave("./output/10oss-sp-service.png", width = 10, height = 5.5)
}

ossSpPermits <- function() { #slide 11
  #wait for any permit
  l <- filter(oss,
              queue == "BBSA Building Standards & Appeals" |
              queue == "Building Inspections" |
              queue == "Building Permit" |
              queue == "Dam. Assessment" |
              queue == "Demolition" |
              queue == "Document Retrieval" |
              queue == "Electrical Inspections" |
              queue == "Electrical License" |
              queue == "Electrical Permit" |
              queue == "Mechanical Inspections" |
              queue == "Mechanical License" |
              queue == "Mechanical Permit" |
              queue == "Occup. License" |
              queue == "Plan Review" |
              queue == "Stationary/Operat. Eng." |
              queue == "Zoning"
              )%>%
        group_by(my) %>%
        summarise(n = n(), mean = mean(timewaited))

  #wait for building permit
  b <- filter(oss, queue == "Building Permit") %>%
       group_by(my) %>%
       summarise(n = n(), mean = mean(timewaited))

  #any permit
  ggplot(l, aes(x = my, y = mean, group = 1)) +
  geom_line(colour = "steelblue", size = 1) +
  geom_hline(aes(yintercept = 18), colour = "tomato", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  labs( title = "Average wait time for any license or permit\n\n\n", x = "Month", y = "Minutes" )
  ggsave("./output/11oss-sp-wait-all.png", width = 10, height = 5.5)

  #building permit
  ggplot(b, aes(x = my, y = mean, group = 1)) +
  geom_line(colour = "steelblue", size = 1) +
  geom_hline(aes(yintercept = 18), colour = "tomato", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  labs( title = "Average wait time for new building permit\n\n\n", x = "Month", y = "Minutes" )
  ggsave("./output/11oss-sp-wait-building.png", width = 10, height = 5.5)
}

ossEtc <- function() { #slide 12
  d <- filter(oss,
              category == "Business Intake" |
              category == "CPC" |
              category == "HDLC" |
              category == "Payment" |
              category == "Special Event" |
              category == "VCC" |
              category == "Zoning"
              )%>%
        group_by(my, category) %>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = category, colour = category)) +
        labs( x = "Month", y = "Minutes" ) +
        scale_colour_discrete(name = "")

  #visitors
  p + geom_line(aes(y = n), size = 1) +
      labs(title = "Number of visitors\n\n\n", y = "Visitors")
      ggsave("./output/12oss-etc-visitors.png", width = 10, height = 5.5)

  #mean wait
  p + geom_line(aes(y = meanwait), size = 1) +
      labs(title = "Average wait time\n\n\n")
      ggsave("./output/12oss-etc-mean-wait.png", width = 10, height = 5.5)

  #mean service
  p + geom_line(aes(y = meanserve), size = 1) +
      labs(title = "Average service time\n\n\n")
      ggsave("./output/12oss-etc-mean-service.png", width = 10, height = 5.5)
}

ossOlp <- function() { #slide 13
  d <- filter(oss, queue == "Occup. License" | queue == "Payment") %>%
       group_by(my, queue) %>%
       summarise(n = n(), meanwait = mean(timewaited))

  #occupational license
  ggplot(filter(d, queue == "Occup. License"), aes(x = my, y = meanwait, group = queue)) +
    geom_line(colour = "steelblue", size = 1) +
    labs(title = "Average wait time to apply for a new occupational license\n\n\n",x = "Month", y = "Minutes") +
    geom_hline(aes(yintercept = 18), colour = "tomato", linetype = "dashed") +
    annotate("text", x = 1.5, y = 18.5, label = "Target") +
    ggsave("./output/13oss-occup-license.png", width = 10, height = 5.5)

  #make a payement
  ggplot(filter(d, queue == "Payment"), aes(x = my, y = meanwait, group = queue)) +
    geom_line(colour = "steelblue", size = 1) +
    labs(title = "Average wait time to make a payement\n\n\n",x = "Month", y = "Minutes") +
    geom_hline(aes(yintercept = 5), colour = "tomato", linetype = "dashed") +
    annotate("text", x = 1.5, y = 5.5, label = "Target") +
    ggsave("./output/13oss-payment-mean-wait.png", width = 10, height = 5.5)
}

ossCpnc <- function() { #slide 14
  d <- filter(oss,
              category == "Brake Tag" |
              category == "Citation" |
              category == "CPNC" |
              category == "Driver/Operator" |
              category == "Other" |
              category == "Tour Guide"
             ) %>%
        group_by(my, category) %>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = category, colour = category)) +
        labs( x = "Month", y = "Minutes" ) +
        scale_colour_discrete(name = "")

  #visitors
  p + geom_line(aes(y = n), size = 1) +
      labs(title = "Number of visitors\n\n\n", y = "Visitors")
      ggsave("./output/14oss-cpnc-visitors.png", width = 10, height = 5.5)

  #mean wait
  p + geom_line(aes(y = meanwait), size = 1) +
      labs(title = "Average wait time\n\n\n")
      ggsave("./output/14oss-cpnc-mean-wait.png", width = 10, height = 5.5)

  #mean service
  p + geom_line(aes(y = meanserve), size = 1) +
      labs(title = "Average service time\n\n\n")
      ggsave("./output/14oss-cpnc-mean-service.png", width = 10, height = 5.5)
}

#execute plot calls
ossSp()
ossSpPermits()
ossEtc()
ossOlp()
ossCpnc()


#
#end plotOss
}

#load
oss <- read.csv("./data/oss-service-report.csv", sep = ";", header = TRUE)

#execute
oss <- cleanOss()
plotOss()
