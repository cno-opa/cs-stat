#wut for inspections

#clean
cleanBiz <- function(data) {
  data <- cleanInspections(data) %>%
          filter(type == "Business License" | type == "Temporary Business License") %>%
          filter(days >= 0)
  return(data)
}

#plot

plotInsp <- function() {
#
#

plotBiz <- function() {
  d <- group_by(biz, month_end) %>%
       summarise(n = n(), under = sum(days < 7)) %>%
       melt()

  p <- schigoda(d, "month_end", "value", fill = "variable")
  p <- buildChart(p)
  ggsave("./output/27-inspections-biz.png", plot = p, width = 7, height = 6.25)
}

#execute
plotBiz()

#
#
}

#load
biz <- read.csv("./data/inspections-biz.csv", header = TRUE)

#execute
biz <- cleanBiz(biz)
plotBiz()
