#clean and plot revenue report data

cleanRev <- function() {
  rev <- cleanServiceReport(rev)
  rev$queue[rev$queue == "Account Maint." | rev$queue == "Administration" | rev$queue == "Account Admin."] <- "Account Maintenance and Administration"
  rev$queue[rev$queue == "Business Regist."] <- "Business Intake"

  return(rev)
}

plotRev <- function() {
  d <- group_by(rev, my, queue) %>%
       summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = queue, colour = queue)) +
       labs( x = "Month", y = "Minutes" ) +
       scale_colour_discrete(name = "")

  #visitors
  p + geom_line(aes(y = n)) +
      labs(title = "Number of visitors", y = "Visitors")
      ggsave("./output/21rev-visitors.png", width = 10, height = 5.5)

  #mean wait
  p + geom_line(aes(y = meanwait)) +
      labs(title = "Average wait time")
      ggsave("./output/21rev-mean-wait.png", width = 10, height = 5.5)

  #mean service
  p + geom_line(aes(y = meanserve)) +
      labs(title = "Average service time")
      ggsave("./output/21rev-mean-service.png", width = 10, height = 5.5)
}

#load
rev <- read.csv("./data/revenue.csv", sep = ";", header = TRUE)

#execute
rev <- cleanRev()
plotRev()
