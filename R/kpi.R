# kpi.R
#
# prints kpi measures to stdout. call function, print_kpis() when script is finished running
# also has standalone function for calculating KPIs, calcKPIs(), by quarter or annually from the data
#
# data sources:
# ============================
#
# kpi.Rdata - a table updated by the bundle scripts
# all the data sources the rest of R/ uses
#
# ============================
#
#

print_kpis <- function() {
  load('./data/kpi.Rdata')
  print(kpi)
  write.csv(kpi, "./output/kpi.csv", row.names = FALSE)
}

calcKPIs <- function(time_period, year_override = NA) {
  # time_period can be Q1, Q2, Q3, or annual
  # fn gets year based on Sys.time. set the year manually by using "year_override = 2013" for example
  # IMPORTANT! this fn depends on all the functions and data produced by the script. I suggest using calcKPIs() at the end of main.R
  # saves a CSV called "kpis-summary"

  # set time boundaries
  if(!is.na(year_override)) {
    y <- as.numeric(year_override)
  } else {
    y <- as.numeric(format(Sys.time(), "%Y"))
  }

  if(time_period == "annual") {
    l <- ymd(paste(y, "01", "01", sep = "-"))
    u <- ymd(paste(y, "12", "31", sep = "-"))
  } else if(time_period == "Q1") {
    l <- ymd(paste(y, "01", "01", sep = "-"))
    u <- ymd(paste(y, "03", "31", sep = "-"))
  } else if(time_period == "Q2") {
    l <- ymd(paste(y, "04", "01", sep = "-"))
    u <- ymd(paste(y, "06", "30", sep = "-"))
  } else if(time_period == "Q3") {
    l <- ymd(paste(y, "07", "01", sep = "-"))
    u <- ymd(paste(y, "09", "30", sep = "-"))
  } else if(time_period == "Q4") {
    l <- ymd(paste(y, "10", "01", sep = "-"))
    u <- ymd(paste(y, "12", "31", sep = "-"))
  } else {
    e <- simpleError("Please provide a time period argument, either annual, Q1, Q2, Q3, or Q4")
    stop(e)
  }

  # 311 KPIs
  qls$eom <- dateFromYearMon(qls$date)
  kpi_abandon_rate <- mean(as.numeric(filter(qls, measure == "Abandonment Rate", eom >= l & eom <= u)$value))
  kpi_call_res <- mean(as.numeric(filter(qls, measure == "First Call Resolution", eom >= l & eom <= u)$value))

  # initialize with 311 KPIs
  kpis <- data.frame(measure = c("311- Abandoment Rate", "311 - First Call Resolution Rate"), value = c(kpi_abandon_rate, kpi_call_res))

  # Safety and Permits KPIs
  kpi_wait_bldg            <- filter(oss, category == "Building" & datein >= l & datein <= u) %>%
                               summarise(measure = "Median wait for building permit", value = median(timewaited))

  kpi_wait_any             <- filter(oss, datein >= l & datein <= u) %>%
                               summarise(measure = "Median wait for any permit", value = median(timewaited))

  kpi_wait_biz             <- filter(oss, category == "Business" & datein >= l & datein <= u) %>%
                               summarise(measure = "Median wait for business permit", value = median(timewaited))

  kpi_wait_payment         <- filter(oss, category == "Payment" & datein >= l & datein <= u) %>%
                               summarise(measure = "Median wait to make a payment", value = median(timewaited))

  kpi_perc_online          <- applied[applied$filingdate <= u & applied$filingdate >= l & applied$online == TRUE,] %>%
                               summarise(measure = "Percent of permits applied for online", value = sum(createdby == "publicwebcrm")/n())

  kpi_avg_comm             <- filter(issued, usetype == "commercial" & issuedate >= l & issuedate <= u) %>%
                              summarise(measure = "Mean days to issue commercial permit", value = mean(daystoissue, na.rm = TRUE))

  kpi_avg_res              <- filter(issued, usetype == "residential" & issuedate >= l & issuedate <= u) %>%
                              summarise(measure = "Mean days to issue residential permit", value = mean(daystoissue, na.rm = TRUE))

  kpi_avg_bldg_complaint   <- filter(complaints, opa_category == "Building" & firstinspection >= l & firstinspection <= u) %>%
                              summarise(measure = "Mean days to respond to building complaints", value = mean(daystoinspect, na.rm = TRUE))

  kpi_avg_zoning_complaint <- filter(complaints, opa_category == "Zoning" & firstinspection >= l & firstinspection <= u) %>%
                              summarise(measure = "Mean days to respond to zoning complaints", value = mean(daystoinspect, na.rm = TRUE))

  kpi_avg_biz_complaint    <- filter(inspections, date >= l & date <= u) %>%
                              summarise(measure = "Mean days to business license inspection", value = mean(days, na.rm = TRUE))

  # bind all to kpis object
  kpis <- rbind(
                kpis,
                kpi_wait_bldg,
                kpi_wait_any,
                kpi_wait_biz,
                kpi_wait_payment,
                kpi_perc_online,
                kpi_avg_comm,
                kpi_avg_res,
                kpi_avg_bldg_complaint,
                kpi_avg_zoning_complaint,
                kpi_avg_biz_complaint
               )

  # save
  kpis$quarter <- time_period
  kpis$year <- y
  write.csv(kpis, "./output/kpi-summary.csv", row.names = FALSE)

  #
  #
}
