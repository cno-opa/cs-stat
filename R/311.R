# 311.R
#
# data sources:
# ============================
#
# 311-intake.xlsx - produced by 311 business analysts monthly.
# 311-source.csv - source data for all 311 requests. Must be cleaned a little in Excel and saved as csv.
#
# ============================
#
#


# clean
cleanQLS <- function() {
  qls <- melt(qls, id.vars = c("Measure", "Type"))
  names(qls) <- slugify(names(qls))
  names(qls) <- c("measure", "type", "date", "value")
  qls$date <- gsub("[.]", " ", qls$date)
  qls$date <- as.factor(as.yearmon(qls$date))

  #strip out % if they're in there
  convPercent <- function(x) {
    if(grepl("%", x)) {
      x <- gsub("%", "", x)
      x <- as.numeric(x)/100
    } else {
      x
    }
  }

  qls$value <- sapply(qls$value, convPercent)

  qls <- getTwoYears(qls, date, r_period)

  return(qls)
}

cleanOps <- function() {
  colnames(ops) <- sapply(ops[1,], function(x) as.character(x[1]))
  ops <- ops[-1,]
  ops <- melt(ops, id.vars = "Agent")
  names(ops) <- slugify(names(ops))
  ops$agent <- factor(ops$agent)
  levels(ops$agent) <- unique(ops$agent)
  ops$variable <- gsub("[.]", " ", ops$variable)
  ops$value <- gsub("%", "", ops$value)
  ops$value <- as.numeric(ops$value)

  return(ops)
}

cleanSource <- function() {
  names(sourceD) <- slugify(names(sourceD))
  sourceD$x_1 <- NULL
  sourceD$x_2 <- NULL
  sourceD$x_3 <- NULL
  sourceD$x_4 <- NULL
  sourceD$x_5 <- NULL
  sourceD$x_6 <- NULL
  sourceD$x_7 <- NULL
  sourceD$x_8 <- NULL
  sourceD$open_dt <- mdy(sourceD$open_dt)
  sourceD$closed_dt <- mdy(sourceD$closed_dt)
  sourceD$month_start <- as.factor(as.yearmon(sourceD$open_dt))
  sourceD$month_end <- as.factor(as.yearmon(sourceD$closed_dt))
  sourceD$open_end_month <- ifelse(sourceD$month_start != sourceD$month_end, TRUE, FALSE)
  sourceD$age__calendar <- as.numeric(as.character(sourceD$age__calendar))
  return(sourceD)
}

set_kpis <- function() {
  load("./data/kpi.Rdata")
  y <- strsplit(r_period, " ")[[1]][2]
  k <- filter(qls, measure == "Abandonment Rate" | measure == "First Call Resolution")
  k <- filter(k, grepl(y, date))
  k$value <- as.numeric(k$value)
  k <- group_by(k, measure) %>%
       summarise(value = mean(value))
  kpi <- rbind(kpi, k)
  save(kpi, file = "./data/kpi.Rdata")
}

# plots
plot311 <- function() {
#
#

theme_set(theme_opa())

callVol <- function() {
  d <- filter(qls, measure == "Calls") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Call Volume", labels = "format(n, big.mark = \",\", scientific = FALSE)")
  p <- buildChart(p)
  ggsave("./output/6-311-calls.png", plot = p, width = 7.42, height = 5.75)
}

callAbandon <- function() {
  d <- filter(qls, measure == "Abandonment Rate") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Abandonment Rate", percent = TRUE, labels = "percent(n)")
  p <- buildChart(p)
  ggsave("./output/7-311-abandonment.png", plot = p, width = 7.42, height = 5.75)
}

holdTime <- function() {
  d <- filter(qls, measure == "Avg. Hold Time (sec)") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Average hold time (seconds)", labels = "format(n, big.mark = \",\", scientific = FALSE)")
  p <- buildChart(p)
  ggsave("./output/8-311-hold-time.png", plot = p, width = 7.42, height = 5.75)
}

firstCall <- function() {
  d <- filter(qls, measure == "First Call Resolution") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "First call resolution rate", percent = TRUE, labels = "percent(n)")
  p <- buildChart(p)
  ggsave("./output/9-311-first-call.png", plot = p, width = 7.42, height = 5.75)
}

operators <- function() {
  d <- filter(ops, !is.na(value) & value > 0)
  p <- barOPA(d, "agent", "value", "Operator scores", ylab = "Score (%)", position = "dodge", fill = "variable")
  p <- p + theme(legend.text = element_text(size = rel(0.65))) +
           guides(fill = guide_legend(nrow = 2))
  p <- buildChart(p)
  ggsave("./output/10-311-operators.png", plot = p, width = 7.42, height = 5.75)
}

topRequest <- function() {
  top <- filter(qls,
                type == "service request",
                date == levels(qls$date)[length(levels(qls$date))]) %>%
          arrange(value)

  top <- top$measure[1:3]
  top <- as.character(top)

  sourceD$type <- as.character(sourceD$type)

       # filter(qls, measure == top[1] | measure == top[2] | measure == top[3]) <== use that when 311 doesn't insist on changing their type designations around
  d <- getTwoYears(sourceD, month_start, r_period) %>%
       filter(agrepl(top[1], type, max.distance = 0.3) | agrepl(top[2], type, max.distance = 0.3) | agrepl(top[3], type, max.distance = 0.3)) %>%
       group_by(month_start, type) %>%
       summarise(n = n()) %>%
       melt()

  p <- lineOPA(d,
               "month_start",
               "value",
               "Top service requests",
               group = "type",
               highlight = "Code Enforcement General Request")
  p <- p + theme(legend.text = element_text(size = rel(0.65))) +
           guides(fill = guide_legend(nrow = 2))
  p <- buildChart(p)
  ggsave("./output/11-311-top-requests.png", plot = p, width = 7.42, height = 5.75)
}

taxiComplaints <- function() {
  taxi <- filter(sourceD, title == "Taxi - Complaint")
  months_in_period <- seq(ymd(as.Date(as.yearmon(r_period) - 2, format = "%b %Y")),
                          ymd(as.Date(as.yearmon(r_period), format = "%b %Y")),
                          "month")

  # number closed per month
  n_closed <- getTwoYears(taxi, month_end, r_period) %>%
              group_by(month_end) %>%
              summarise(closed = n())

  # number opened per month
  n_opened <- getTwoYears(taxi, month_start, r_period) %>%
              group_by(month_start) %>%
              summarise(opened = n())

  # number still open at end of each month
  n_open <- data.frame(date = months_in_period)

  calcOpen <- function(date) {
    date_end_month <- ymd( paste(year(date), month(date), days_in_month(month(date))) )

    opened_before <- filter(taxi, open_dt <= date_end_month)
    closed_before <- filter(taxi, closed_dt <= date_end_month)

    return(nrow(opened_before) - nrow(closed_before))
  }

  n_open$open_at_end <- lapply(n_open$date, calcOpen)
  n_open$date <- as.factor(as.yearmon(n_open$date))

  # mean days to close for all complaints closed in each month
  d_closed <- getTwoYears(taxi, month_end, r_period) %>%
              group_by(month_end) %>%
              summarise(mean_close = mean(age__calendar))

  # mean age of every open complaint at end of each month
  d_open <- data.frame(date = months_in_period)

  calcOpenAge <- function(date) {
    date_end_month <- ymd( paste(year(date), month(date), days_in_month(month(date))) )

    #anti join method
    opened_before <- filter(taxi, open_dt <= date_end_month)
    closed_before <- filter(taxi, closed_dt <= date_end_month)
    o <- anti_join(opened_before, closed_before, by = "case_reference")
    o$age <- date_end_month - o$open_dt

    return( as.numeric(mean(o$age, na.rm = TRUE), units ="days") )
  }

  d_open$age_open_eom <- lapply(d_open$date, calcOpenAge)
  d_open$date <- as.factor(as.yearmon(d_open$date))

  # join into single table for plots
  names(n_closed)[1]  <- "date"
  names(n_opened)[1]  <- "date"
  names(n_open)[1]    <- "date"
  names(d_closed)[1]  <- "date"
  names(d_open)[1]    <- "date"

  d <- left_join(n_closed, n_opened) %>%
       left_join(n_open) %>%
       left_join(d_closed) %>%
       left_join(d_open)

  d$date <- as.factor(as.yearmon(d$date))
  d$open_at_end <- unlist(d$open_at_end)
  d$age_open_eom <- unlist(d$age_open_eom)

  d <- melt(d)

  p_open <- lineOPA(filter(d, variable == "open_at_end"),
                 "date",
                 "value",
                 "Number of open complaints against drivers at end of month",
                 labels = "format(value, big.mark = \",\", scientific = FALSE)"
                )
  p_open <- buildChart(p_open)
  ggsave("./output/46-311-taxi-complaints-open.png", plot = p_open, width = 7.42, height = 5.75)

  d_net <- data.frame(month = unique(d$date), net = (d$value[d$variable == "opened"] - d$value[d$variable == "closed"]))
  d_net$shade <- ifelse(d_net$net > 0, "pos", "neg")

  p_net <- barOPA(d_net, "month", "net", "Net complaints logged against taxi drivers per month", fill = "shade", labels = "format(net, big.mark = \",\", scientific = FALSE)") +
           scale_y_continuous(breaks = 0) +
           scale_fill_manual(values = c(darkBlue, red), guide = FALSE) +
           theme(axis.text.y = element_blank())

  p_net <- buildChart(p_net)
  ggsave("./output/45-311-taxi-complaints-net.png", plot = p_net, width = 7.42, height = 5.75)

  p_d <- lineOPA(filter(d, variable == "mean_close" | variable == "age_open_eom"),
                 "date",
                 "value",
                 "Age statistics on complaints against drivers",
                 group = "variable",
                 legend.labels = c("Mean days to close", "Age of open complaints at end of month"),
                 labels = "round(value)"
                )
  p_d <- buildChart(p_d)
  ggsave("./output/47-311-taxi-complaints-time.png", plot = p_d, width = 7.42, height = 5.75)

}

# execute
callVol()
callAbandon()
holdTime()
firstCall()
operators()
topRequest()
taxiComplaints()

#
#
}

# load
qls <- read.xls("./data/311.xlsx", sheet = "QLS", na.strings = c("", "#N/A", "NA", "#DIV/0!", "REF!"), strip.white = TRUE, perl = "C:/Strawberry/perl/bin/perl.exe")
ops <- read.xls("./data/311.xlsx", sheet = "operator performance", na.strings = c("", "#N/A", "NA", "#DIV/0!, #REF!"), strip.white = TRUE, perl = "C:/Strawberry/perl/bin/perl.exe")
sourceD <- read.csv("./data/311-source.csv", header = TRUE)

# execute
qls <- cleanQLS()
ops <- cleanOps()
sourceD <- cleanSource()
set_kpis()
plot311()
