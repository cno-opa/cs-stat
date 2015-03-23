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
  ggsave("./output/28-complaints-building.png", plot = p, width = 7, height = 6.25)
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
  ggsave("./output/29-complaints-zoning.png", plot = p, width = 7, height = 6.25)
}

openEndOfMonth <- function() {

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

  d <- getOneYear(complaints, month_start, period) %>%
       group_by(month_start) %>%
       summarise(n = n())

  p <- lineOPA(d, "month_start", "n", "Number of complaints with no first inspection by end of month", labels = "n")
  p <- buildChart(p)
  ggsave("./output/30-complaints-open.png", plot = p, width = 7, height = 6.25)
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
