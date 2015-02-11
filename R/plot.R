#plot.R
#makes ALL the charts

#TODO: DRY. Remove artifacts

require(ggplot2)
require(dplyr)

init_plot <- function() {
#
#

#oss
oss_sp <- function() { #slide 10
  d <-  filter(oss, category == "Safety and Permits" | category == "Safety and Permits - Electrical" | category == "Safety and Permits - Inspections" | category == "Safety and Permits - Mechanical" | category == "Safety and Permits - Plan Review")%>%
        group_by(my, category)%>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = category, colour = category)) +
        theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
        scale_colour_discrete(name = "Category",
                              labels = c("S&P", "S&P - Elec", "S&P - Inspec", "S&P - Mech", "S&P - Plan Rev"))

  #visitors
  p + geom_line(aes(y = n)) +
      labs( title = "Visitors by month", x = "Month", y = "Visitors" ) +
      ggsave("./output/10oss-sp-visitors.png", width = 10, height = 5.5)
      cat("Saving safety and permits visitors line chart...\n")

  #mean wait time
  p + geom_line(aes(y = meanwait)) +
      labs( title = "Average wait time", x = "Month", y = "Minutes" ) +
      ggsave("./output/10oss-sp-wait.png", width = 10, height = 5.5)
      cat("Saving safety and permits mean wait time line chart...\n")

  #mean service time
  p + geom_line(aes(y = meanserve)) +
      labs( title = "Average service time", x = "Month", y = "Minutes" ) +
      ggsave("./output/10oss-sp-service.png", width = 10, height = 5.5)
      cat("Saving safety and permits mean service time line chart...\n")
}

oss_sp_permits <- function() { #slide 11
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
        group_by(my)%>%
        summarise(n = n(), mean = mean(timewaited))

  #wait for building permit
  b <- filter(oss, queue == "Building Permit")%>%
       group_by(my)%>%
       summarise(n = n(), mean = mean(timewaited))

  #any permit
  ggplot(l, aes(x = my, y = mean, group = 1)) +
  geom_line(colour = "steelblue") +
  geom_hline(aes(yintercept = 18), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs( title = "Average wait time for any license or permit", x = "Month", y = "Minutes" )
  ggsave("./output/11oss-sp-wait-all.png", width = 10, height = 5.5)
  cat("Saving safety and permits all permits mean wait time line chart...\n")

  #building permit
  ggplot(b, aes(x = my, y = mean, group = 1)) +
  geom_line(colour = "steelblue") +
  geom_hline(aes(yintercept = 18), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs( title = "Average wait time for new building permit", x = "Month", y = "Minutes" )
  ggsave("./output/11oss-sp-wait-building.png", width = 10, height = 5.5)
  cat("Saving safety and permits building permit mean wait time line chart...\n")
}

oss_etc <- function() { #slide 12
  d <- filter(oss,
              category == "Business Intake" |
              category == "CPC" |
              category == "HDLC" |
              category == "Payment" |
              category == "Special Event" |
              category == "VCC" |
              category == "Zoning"
              )%>%
        group_by(my, category)%>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = category, colour = category)) +
        theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
        labs( x = "Month", y = "Minutes" ) +
        scale_colour_discrete(name = "")

  #visitors
  p + geom_line(aes(y = n)) +
      labs(title = "Number of visitors", y = "Visitors")
      ggsave("./output/12oss-etc-visitors.png", width = 10, height = 5.5)
      cat("Saving OSS etc visitors line chart...\n")

  #mean wait
  p + geom_line(aes(y = meanwait)) +
      labs(title = "Average wait time")
      ggsave("./output/12oss-etc-mean-wait.png", width = 10, height = 5.5)
      cat("Saving OSS etc mean wait line chart...\n")

  #mean service
  p + geom_line(aes(y = meanserve)) +
      labs(title = "Average service time")
      ggsave("./output/12oss-etc-mean-service.png", width = 10, height = 5.5)
      cat("Saving OSS etc mean service line chart...\n")
}

oss_olp <- function() { #slide 13
  d <- filter(oss, queue == "Occup. License" | queue == "Payment") %>%
       group_by(my, queue) %>%
       summarise(n = n(), meanwait = mean(timewaited))

  #occupational license
  ggplot(filter(d, queue == "Occup. License"), aes(x = my, y = meanwait, group = queue)) +
  geom_line(colour = "steelblue") +
  labs(title = "Average wait time to apply for a new occupational license",x = "Month", y = "Minutes") +
  geom_hline(aes(yintercept = 18), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  ggsave("./output/13oss-occup-license.png", width = 10, height = 5.5)
  cat("Saving OSS occupational license mean wait line chart...\n")

  #make a payement
  ggplot(filter(d, queue == "Payment"), aes(x = my, y = meanwait, group = queue)) +
  geom_line(colour = "steelblue") +
  labs(title = "Average wait time to make a payement",x = "Month", y = "Minutes") +
  geom_hline(aes(yintercept = 5), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 5.5, label = "Target") +
  ggsave("./output/13oss-payment-mean-wait.png", width = 10, height = 5.5)
  cat("Saving OSS make a payment mean wait line chart...\n")
}

oss_cpnc <- function() { #slide 14
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
        theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
        labs( x = "Month", y = "Minutes" ) +
        scale_colour_discrete(name = "")

  #visitors
  p + geom_line(aes(y = n)) +
      labs(title = "Number of visitors", y = "Visitors")
      ggsave("./output/14oss-cpnc-visitors.png", width = 10, height = 5.5)
      cat("Saving OSS CPNC visitors line chart...\n")

  #mean wait
  p + geom_line(aes(y = meanwait)) +
      labs(title = "Average wait time")
      ggsave("./output/14oss-cpnc-mean-wait.png", width = 10, height = 5.5)
      cat("Saving OSS CPNC mean wait line chart...\n")

  #mean service
  p + geom_line(aes(y = meanserve)) +
      labs(title = "Average service time")
      ggsave("./output/14oss-cpnc-mean-service.png", width = 10, height = 5.5)
      cat("Saving OSS CPNC mean service line chart...\n")
}

#load
load("./data/data-cleaned.Rdata")

#execute
oss_sp()
oss_sp_permits()
oss_etc()
oss_olp()

#
#end init_plot
}
