#you know what this does for inspections data

cleanBizInsp <- function() {
  inspect_biz <- cleanInspections(inspect_biz)
  return(inspect_biz)
}

cleanBldgInsp <- function() {
  load("./data/context/inspections-bldg-historical-all-2014.Rdata")

  inspect_bldg <- cleanInspections(inspect_bldg, subset = FALSE)
  inspect_bldg$date <- mdy(inspect_bldg$date)
  inspect_bldg_hist <- rbind(inspect_bldg_hist, inspect_bldg)
  inspect_bldg_hist <- filter(inspect_bldg_hist, days >= 0)
  save(inspect_bldg_hist, file = "./data/context/inspections-bldg-historical-recent.Rdata")

  inspect_bldg_hist <- subset(inspect_bldg_hist, inspect_bldg_hist$my %in% levels(inspect_bldg_hist$my)[(length(levels(inspect_bldg_hist$my))-12):length(levels(inspect_bldg_hist$my))])
  return(inspect_bldg_hist)

  return(inspect_bldg)
}

plotInspections <- function() {
#
#

inspectBizCharts <- function() { #slide 25
  d <- filter(inspect_biz, type == "Business License" | type == "Temporary Business License", days >= 0) %>%
       group_by(my) %>%
       summarise(n = n(), mean = mean(days))

  p <- ggplot(d, aes(x = my)) +
       labs(x = "Month")

  p + geom_bar(aes(y = mean), stat = "identity", fill = "springgreen4") +
      labs(y = "Days", title = "Average number of days to completing inspection request\n\n\n") +
      geom_text(size = 4, colour = "grey33", vjust = -.5, aes(label = round(mean), y = mean)) +
      geom_hline(aes(yintercept = 7), colour = "tomato", linetype = "dashed") +
      geom_hline(aes(yintercept = 1.1), colour = "steelblue", linetype = "dashed")
      ggsave("./output/25inspect-biz-mean-days.png", width = 10, height = 5.5)

  p + geom_line(aes(y = n, group = 1), colour = "tomato") +
      labs(y = "Number", title = "Number of inspection requests\n\n\n")
      ggsave("./output/25inspect-biz-n.png", width = 10, height = 5.5)
}

inspectBldgCharts <-function() { #slide 26
  d <- group_by(inspect_bldg, my) %>%
       summarise(n = n(), sameday = sum(days == 0)) %>%
       melt()

  ggplot(d, aes(x = my, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "identity") +
  labs(title = "Number of building inspections done and those done in the same day\n\n\n", x = "Month", y = "Number") +
  scale_fill_discrete(name = "", labels = c("All inspections", "Same day inspections"))
  ggsave("./output/26inspect-bldg.png", width = 10, height = 5.5)
}

#plot calls
inspectBizCharts()
inspectBldgCharts()

#
#
}

#load
inspect_biz <- read.csv("./data/inspections-biz.csv", header = TRUE)
inspect_bldg <- read.csv("./data/inspections-bldg-recent.csv", header = TRUE)

#execute
inspect_biz <- cleanBizInsp()
inspect_bldg <- cleanBldgInsp()
plotInspections()
