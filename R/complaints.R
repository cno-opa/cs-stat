# complaints.R
#
# data sources:
# ============================
#
# complaints.R - csv pulled from LAMA Violations report.
#
# ============================
#
#

# clean
cleanSPComplaints <- function(data) {

  categorize <- function(type) {
    if(type == "Building Code" | type == "Working Without Permit" | type == "Imminent Danger of Collapse") {
      "Building"
    } else if(type == "Zoning General" | type == "Zoning - Paving/Parking" | type == "Illegal Sign" | type == "Junk and Debris") {
      "Zoning"
    } else {
      type
    }
  }

  data <- cleanComplaints(data)

  data <- filter(data, origin == "Business" | origin == "Police" | origin == "Citizen")
  data <- filter(data, !grepl("ESP", data$numstring))
  data$opa_category <- sapply(data$type, categorize)
  return(data)
}

set_kpis <- function() {
  load("./data/kpi.Rdata")
  cutoff <- dateFromYearMon(period)
  cutup <- ymd(paste(
              strsplit(period, " ")[[1]][2],
              "01",
              "01",
              sep = "-"
              ))

  building <- filter(complaints, opa_category == "Building" & firstinspection <= cutoff & firstinspection >= cutup) %>%
              summarise(measure = "Median days to respond to building complaints", value = median(daystoinspect, na.rm = TRUE))

  zoning <- filter(complaints, opa_category == "Zoning" & firstinspection <= cutoff & firstinspection >= cutup) %>%
              summarise(measure = "Median days to respond to zoning complaints", value = median(daystoinspect, na.rm = TRUE))

  kpi <- rbind(kpi,
               building,
               zoning)

  save(kpi, file = "./data/kpi.Rdata")
}


# plot
plotComplaints <- function() {
#
#

theme_set(theme_opa())

building <- function() {
  d <- getOneYear(complaints, month_end, period) %>%
       filter(opa_category == "Building") %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(daystoinspect <= 7, na.rm = TRUE)) %>%
       filter(month_end != "NA NA") %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Building inspections, and days to completion", fill = "variable", position = "identity", legend.labels = c("More than 7 days", "Less than 7 days"))
  p <- buildChart(p)
  ggsave("./output/40-complaints-building.png", plot = p, width = 7.42, height = 5.75)
}

zoning <- function() {
  d <- getOneYear(complaints, month_end, period) %>%
       filter(opa_category == "Zoning") %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(daystoinspect <= 7, na.rm = TRUE)) %>%
       filter(month_end != "NA NA") %>%
       melt()

  p <- barOPA(d, "month_end", "value", title = "Zoning inspections, and days to completion", fill = "variable", position = "identity", legend.labels = c("More than 7 days", "Less than 7 days"))
  p <- buildChart(p)
  ggsave("./output/41-complaints-zoning.png", plot = p, width = 7.42, height = 5.75)
}

openEndOfMonth <- function() {

  # this counts all complaints opened before the end of each month and closed after the end of each month, or not closed. It returns a number around 220 for each month, which makes me think there are 220 artifacts in LAMA

  countOpen <- function(month_year, df, date_start, date_end) {
    date_start <- eval(substitute(date_start), envir = df)

    month_year <- as.Date(as.yearmon(month_year))
    eom <- ymd(paste(year(month_year), month(month_year), days_in_month(month(month_year)), sep="-"))
    f <- filter(df, date_start <= eom)
    date_end <- eval(substitute(date_end), envir = f)
    f <- filter(f, date_end > eom | is.na(date_end))
    n <- nrow(f)
    return(n)
  }

  # supplementary measure

  d <- getOneYear(complaints, month_start, period) %>%
       group_by(month_start) %>%
       summarise(n = sum(daystoinspect > 30 | is.na(daystoinspect)))

  f <- filter(complaints, ymd(d_filed) > ymd("2013-01-01"))

  backlog <- data.frame(month = d$month_start, open = sapply(d$month_start, countOpen, df = f, date_start = d_filed, date_end = firstinspection))

  p <- lineOPA(d, "month_start", "n", "Complaints with no first inspection within 30 days", labels = "n")
  p <- buildChart(p)
  ggsave("./output/43-complaints-no-first-inspect.png", plot = p, width = 7.42, height = 5.75)

  p_backlog <- lineOPA(backlog, "month", "open", "Number of open complaints at end of each month", labels = "open")
  p_backlog <- buildChart(p_backlog)
  ggsave("./output/44-complaints-open-eom.png", plot = p_backlog, width = 7.42, height = 5.75)
}

# execute
building()
zoning()
openEndOfMonth()

#
#
}

# load
complaints <- read.csv("./data/complaints.csv", header = TRUE)

# execute
complaints <- cleanSPComplaints(complaints)
plotComplaints()
set_kpis()
