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

set_kpis <- function() {
  load("./data/kpi.Rdata")
  cutoff <- dateFromYearMon(r_period)
  cutup <- ymd(paste(
              strsplit(r_period, " ")[[1]][2],
              "01",
              "01",
              sep = "-"
              ))

  b <- filter(inspections, date >= cutup & date <= cutoff) %>%
       summarise(measure = "mean days to business license inspection", value = mean(days, na.rm = TRUE))

  kpi <- rbind(kpi, b)

  save(kpi, file = "./data/kpi.Rdata")
}

# plot
plotInsp <- function() {
#
#

theme_set(theme_opa())

plotInsp <- function() {
  d <- getTwoYears(inspections, month_end, r_period) %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(days < 7)) %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Business license inspections and days to inspection", fill = "variable", position = "identity", legend.labels = c("Inspected in more than 7 days", "Inspected in 7 days or less"))
  p <- buildChart(p)
  ggsave("./output/42-inspections-biz.png", plot = p, width = 7.42, height = 5.75)
}

backlog <- function() {
  months_in_period <- seq(ymd(as.Date(as.yearmon(r_period) - 2, format = "%b %Y")),
                          ymd(as.Date(as.yearmon(r_period), format = "%b %Y")),
                          "month")
  n_open <- data.frame(date = months_in_period)

  calcOpen <- function(d) {
    date_end_month <- ymd( paste(year(d), month(d), days_in_month(month(d))) )

    opened_before <- filter(inspections, requested <= date_end_month)
    closed_before <- filter(inspections, date <= date_end_month)

    return(nrow(opened_before) - nrow(closed_before))
  }

  n_open$open_at_end <- lapply(n_open$date, calcOpen)
  n_open$date <- as.factor(as.yearmon(n_open$date))

  p <- lineOPA(n_open, "date", "open_at_end", "Business licenses inspection requests outstanding at end of month", labels = "format(open_at_end, big.mark = \",\", scientific = FALSE)")
  p <- buildChart(p)
  ggsave("./output/NEW-biz-inspections-backlog.png", plot = p, width = 7.42, height = 5.75)
}

# execute
plotInsp()
backlog()

#
#
}

# load
inspections <- read.csv("./data/inspections-biz.csv", header = TRUE)

# execute
inspections <- cleanBiz(inspections)
plotInsp()
set_kpis()
