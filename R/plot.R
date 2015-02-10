#plot.R
#makes ALL the charts

#TODO: make ALL the charts

require(ggplot2)
require(dplyr)
require(xtermStyle)

init_plot <- function() {
#
#

#oss
oss_sp <- function() {
  d <-  filter(oss, category == "Safety and Permits" | category == "Safety and Permits - Electrical" | category == "Safety and Permits - Inspections" | category == "Safety and Permits - Mechanical" | category == "Safety and Permits - Plan Review")%>%
        group_by(month(datein, label = TRUE), year(datein), category)%>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  names(d) <- c("month", "year", "category", "n", "meanwait", "meanserve")
  d$my <- paste(d$month, d$year)
  d <- arrange(d, year)
  d$my <- factor(d$my, levels = unique(d$my))

  ggplot(d, aes(x = my, y = n, group = category, colour = category)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs( title = "Visitors by month", x = "Month", y = "Visitors" ) +
  scale_colour_discrete(name = "Category",
                        labels = c("S&P", "S&P - Elec", "S&P - Inspec", "S&P - Mech", "S&P - Plan Rev")
                        ) +
  ggsave("./output/oss-sp.png", width = 10, height = 5.5)
  cat( style( "Saving safety and permits visitors line chart...\n", fg = 208) )

  ggplot(d, aes(x = my, y = meanwait, group = category, colour = category)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs( title = "Average wait time", x = "Month", y = "Minutes" ) +
  scale_colour_discrete(name = "Category",
                        labels = c("S&P", "S&P - Elec", "S&P - Inspec", "S&P - Mech", "S&P - Plan Rev")
                        ) +
  ggsave("./output/oss-sp-wait.png", width = 10, height = 5.5)
  cat( style( "Saving safety and permits mean wait time line chart...\n", fg = 208) )

  ggplot(d, aes(x = my, y = meanserve, group = category, colour = category)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs( title = "Average service time", x = "Month", y = "Minutes" ) +
  scale_colour_discrete(name = "Category",
                        labels = c("S&P", "S&P - Elec", "S&P - Inspec", "S&P - Mech", "S&P - Plan Rev")
                        ) +
  ggsave("./output/oss-sp-service.png", width = 10, height = 5.5)
  cat( style( "Saving safety and permits mean service time line chart...\n", fg = 208) )
}

#load

#execute

#
#end init_plot
}
