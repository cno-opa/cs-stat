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
  exclude <- filter(data, is.na(firstinspection) & stage == "8.Case-Closed" & initinspectionstatus == "")
  #d <- data
  data <- anti_join(data, exclude, by = "codeincidid")

  #create first inspection date based on key status date for entries with case closed and date hidden in keystatus col
  for(i in 1:nrow(data)) {
    if( is.na(data$firstinspection[i]) & data$stage[i] == "8.Case-Closed" & data$initinspectionstatus[i] != "" ) {
      data$firstinspection[i] <- toDate(data$keystatusdate[i])
    }
  }


  data$opa_category <- sapply(data$type, categorize)
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

  building <- filter(complaints, opa_category == "Building" & firstinspection <= cutoff & firstinspection >= cutup) %>%
              summarise(measure = "Mean days to respond to building complaints", value = mean(daystoinspect, na.rm = TRUE))

  zoning <- filter(complaints, opa_category == "Zoning" & firstinspection <= cutoff & firstinspection >= cutup) %>%
              summarise(measure = "Mean days to respond to zoning complaints", value = mean(daystoinspect, na.rm = TRUE))

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

months_in_period <- seq(ymd(as.Date(as.yearmon(r_period) - 2, format = "%b %Y")),
                        ymd(as.Date(as.yearmon(r_period), format = "%b %Y")),
                        "month")

calcOpenAge <- function(data, month) {
  date_end_month <- dateFromYearMon(month)

  #anti join method
  opened_before <- filter(data, d_filed <= date_end_month)
  closed_before <- filter(data, firstinspection <= date_end_month)
  o <- anti_join(opened_before, closed_before, by = "num")
  o$age <- date_end_month - o$d_filed
  
  write.csv(x = o, file = "last_month_open.csv")

  return(as.numeric(median(o$age, na.rm = TRUE), units = "days"))
}

calcMedianClose <- function(data, month) {
  f <- filter(data, as.character(month_end) == as.character(month))
  return( median(f$daystoinspect, na.rm = TRUE))
}

countOpen <- function(month_year, df, date_start, date_end) {
  date_start <- eval(substitute(date_start), envir = df)

  # #remove cases which have been closed without inspections noted
  #df <- filter(df, !grepl(".*been closed.*", keystatus))

  month_year <- as.Date(as.yearmon(month_year))
  eom <- ymd(paste(year(month_year), month(month_year), days_in_month(month(month_year)), sep="-"))
  f <- filter(df, date_start <= eom)
  date_end <- eval(substitute(date_end), envir = f)
  f <- filter(f, date_end > eom | is.na(date_end))
  n <- nrow(f)
  return(n)
}

building <- function() {
  d <- getTwoYears(complaints, month_end, r_period) %>%
       filter(opa_category == "Building") %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(daystoinspect <= 7, na.rm = TRUE)) %>%
       filter(month_end != "NA NA") %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Days to carry out building complaint inspections", fill = "variable", position = "identity", legend.labels = c("More than 7 days", "Less than 7 days"))
  p <- buildChart(p)
  ggsave("./output/40-complaints-building.png", plot = p, width = 7.42, height = 5.75)

  #age stats on building complaints
  f_complaints <- filter(complaints, opa_category == "Building")
  
  ages = data.frame(date = as.factor(as.yearmon(months_in_period)))
  ages$open_age <- sapply(ages$date, calcOpenAge, data = f_complaints)
  ages$median_close <- sapply(ages$date, calcMedianClose, data = f_complaints)
  ages <- melt(ages, id.vars = "date")

  p_age <- lineOPA(ages, "date", "value", "Age statistics on building complaints", group = "variable", labels = "round(value)", legend.labels = c("Median age of open complaints", "Median days to close complaints") )
  p_age <- buildChart(p_age)
  ggsave("./output/NEW-complaints-building-ages.png", plot = p_age, width = 7.42, height = 5.75)

  #volume of backlog
  backlog <- data.frame(month = d$month_end, open = sapply(d$month_end, countOpen, df = filter(complaints, opa_category == "Building"), date_start = d_filed, date_end = firstinspection))
  p_backlog <- lineOPA(backlog, "month", "open", "Number of open building complaints at end of each month", labels = "format(open, big.mark = \",\", scientific = FALSE)")
  p_backlog <- buildChart(p_backlog)
  ggsave("./output/NEW-complaints-open-eom-building.png", plot = p_backlog, width = 7.42, height = 5.75)
}

zoning <- function() {
  d <- getTwoYears(complaints, month_end, r_period) %>%
       filter(opa_category == "Zoning") %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(daystoinspect <= 7, na.rm = TRUE)) %>%
       filter(month_end != "NA NA") %>%
       melt()

  p <- barOPA(d, "month_end", "value", title = "Zoning inspections, and days to completion", fill = "variable", position = "identity", legend.labels = c("More than 7 days", "Less than 7 days"))
  p <- buildChart(p)
  ggsave("./output/41-complaints-zoning.png", plot = p, width = 7.42, height = 5.75)

  #age stats on zoning complaints
  f_complaints <- filter(complaints, opa_category == "Zoning")
  
  #write.csv(x = f_complaints, file = paste0("zoning_complaints", "_", today(), ".csv"))
  
  ages <- data.frame(date = as.factor(as.yearmon(months_in_period)))
  ages$open_age <- sapply(ages$date, calcOpenAge, data = f_complaints)
  ages$median_close <- sapply(ages$date, calcMedianClose, data = f_complaints)
  ages <- melt(ages, id.vars = "date")

  p_age <- lineOPA(ages, "date", "value", "Age statistics on zoning complaints", group = "variable", labels = "round(value)", legend.labels = c("Median age of open complaints", "Median days to close complaints") )
  p_age <- buildChart(p_age)
  ggsave("./output/NEW-complaints-zoning-ages.png", plot = p_age, width = 7.42, height = 5.75)

  #volume of backlog
  backlog <- data.frame(month = d$month_end, open = sapply(d$month_end, countOpen, df = filter(complaints, opa_category == "Zoning"), date_start = d_filed, date_end = firstinspection))
  p_backlog <- lineOPA(backlog, "month", "open", "Number of open zoning complaints at end of each month", labels = "format(open, big.mark = \",\", scientific = FALSE)")
  p_backlog <- buildChart(p_backlog)
  ggsave("./output/NEW-complaints-open-eom-zoning.png", plot = p_backlog, width = 7.42, height = 5.75)
}

openEndOfMonth <- function() {

  # supplementary measure
  d <- getTwoYears(complaints, month_start, r_period) %>%
       group_by(month_start) %>%
       summarise(n = sum(daystoinspect > 30 | is.na(daystoinspect)))

  f <- filter(complaints, ymd(d_filed) > ymd("2013-01-01"))

  backlog <- data.frame(month = d$month_start, open = sapply(d$month_start, countOpen, df = f, date_start = d_filed, date_end = firstinspection))

  p <- lineOPA(d, "month_start", "n", "Complaints with no first inspection within 30 days", labels = "format(n, big.mark = \",\", scientific = FALSE)")
  p <- buildChart(p)
  ggsave("./output/43-complaints-no-first-inspect.png", plot = p, width = 7.42, height = 5.75)

  p_backlog <- lineOPA(backlog, "month", "open", "Number of open complaints at end of each month", labels = "format(open, big.mark = \",\", scientific = FALSE)")
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
complaints <- read.csv("./data/complaints.csv", header = TRUE, stringsAsFactors = FALSE)

# execute
complaints <- cleanSPComplaints(complaints)
plotComplaints()
set_kpis()
