# inspections.R
#
# data sources:
# ============================
#
# inspections-biz.csv - csv generated from LAMA inspections report.
#
# ============================
#
#


# clean
cleanBiz <- function(data) {
  data <- cleanInspections(data) %>%
          filter(inspection == "Zoning") %>%
          filter(type == "Business License" | type == "Temporary Business License") %>%
          filter(days >= 0)
  return(data)
}

# plot
plotInsp <- function() {
#
#

plotBiz <- function() {
  d <- getOneYear(biz, month_end, period) %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(days < 7)) %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Number of business license inspections and those under target time", fill = "variable", position = "identity", legend.labels = c("All inspections", "Inspected in seven days or less"))
  p <- buildChart(p)
  ggsave("./output/30-2-inspections-biz.png", plot = p, width = 7.42, height = 5.75)
}

# execute
plotBiz()

#
#
}

# load
inspections <- read.csv("./data/inspections-biz.csv", header = TRUE)

# execute
biz <- cleanBiz(inspections)
plotInsp()
