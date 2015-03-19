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
          filter(type == "Business License" | type == "Temporary Business License") %>%
          filter(days >= 0)
  return(data)
}

# plot
plotInsp <- function() {
#
#

plotBiz <- function() {
  d_insp <- getOneYear(biz, month_start, period) %>%
            group_by(month_start) %>%
            summarise(n = n())

  d_time <- getOneYear(biz, month_end, period) %>%
            group_by(month_end) %>%
            summarise(target = sum(days < 7))

  names(d_insp)[1] <- "date"
  names(d_time)[1] <- "date"
  d <- left_join(d_insp, d_time) %>%
       melt()

  d$date <- as.factor(as.yearmon(d$date))

  p <- barOPA(d, "date", "value", "Number of business license inspections and those under target time", fill = "variable", position = "identity", legend.labels = c("All inspections", "Inspected in seven days or less"))
  p <- buildChart(p)
  ggsave("./output/30-2-inspections-biz.png", plot = p, width = 7, height = 6.25)
}

# execute
plotBiz()

#
#
}

# load
biz <- read.csv("./data/inspections-biz.csv", header = TRUE)

# execute
biz <- cleanBiz(biz)
plotInsp()
