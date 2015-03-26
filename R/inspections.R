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

theme_set(theme_opa())

plotBiz <- function() {
  d <- getOneYear(biz, month_end, period) %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(days < 7)) %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Business license inspections and days to inspection", fill = "variable", position = "identity", legend.labels = c("Inspected in more than 7 days", "Inspected in 7 days or less"))
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
