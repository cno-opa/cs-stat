#plot.R
#makes ALL the charts

#TODO: DRY. Remove artifacts

require(ggplot2)
require(dplyr)
require(xtermStyle)

init_plot <- function() {
#
#

#oss
oss_sp <- function() {
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
      ggsave("./output/oss-sp-visitors.png", width = 10, height = 5.5)
      cat( style( "Saving safety and permits visitors line chart...\n", fg = 208) )

  #mean wait time
  p + geom_line(aes(y = meanwait)) +
      labs( title = "Average wait time", x = "Month", y = "Minutes" ) +
      ggsave("./output/oss-sp-wait.png", width = 10, height = 5.5)
      cat( style( "Saving safety and permits mean wait time line chart...\n", fg = 208) )

  #mean service time
  p + geom_line(aes(y = meanserve)) +
      labs( title = "Average service time", x = "Month", y = "Minutes" ) +
      ggsave("./output/oss-sp-service.png", width = 10, height = 5.5)
      cat( style( "Saving safety and permits mean service time line chart...\n", fg = 208) )
}

oss_sp_l <- function() {
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
  ggsave("./output/oss-sp-wait-all.png", width = 10, height = 5.5)
  cat( style( "Saving safety and permits all permits mean wait time line chart...\n", fg = 208) )

  #building permit
  ggplot(b, aes(x = my, y = mean, group = 1)) +
  geom_line(colour = "steelblue") +
  geom_hline(aes(yintercept = 18), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs( title = "Average wait time for new building permit", x = "Month", y = "Minutes" )
  ggsave("./output/oss-sp-wait-building.png", width = 10, height = 5.5)
  cat( style( "Saving safety and permits building permit mean wait time line chart...\n", fg = 208) )
}

oss_etc <- function() {
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
      ggsave("./output/oss-etc-visitors.png", width = 10, height = 5.5)
      cat( style( "Saving OSS etc visitors line chart...\n", fg = 208) )

  #mean wait
  p + geom_line(aes(y = meanwait)) +
      labs(title = "Average wait time")
      ggsave("./output/oss-etc-mean-wait.png", width = 10, height = 5.5)
      cat( style( "Saving OSS etc mean wait line chart...\n", fg = 208) )

  #mean service
  p + geom_line(aes(y = meanserve)) +
      labs(title = "Average service time")
      ggsave("./output/oss-etc-mean-service.png", width = 10, height = 5.5)
      cat( style( "Saving OSS etc mean service line chart...\n", fg = 208) )
}

#load
load("./data/data-cleaned.Rdata")

#execute
oss_sp()
oss_sp_l()
oss_etc()

#
#end init_plot
}
