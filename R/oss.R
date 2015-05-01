# oss.R
#
# data sources:
# ============================
#
# oss.csv - csv generated from Lobby Central OSS report.
#
# ============================
#
#


# clean
cleanOss <- function() {
  load("./data/context/oss-lookup.Rdata")

  oss <- cleanServiceReport(oss)
  oss$serviceprovided <- as.character(oss$serviceprovided)
  oss <- filter(oss, lengthofservice < 480) #remove entries that take over 8 hours, or one working day
  oss <- filter(oss, !grepl("appointment", tolower(serviceprovided)) & !grepl("meeting", tolower(serviceprovided)))
  oss$category <- as.factor( oss_lookup$category[match(oss$queue, oss_lookup$lookup)] )
  oss <- getTwoYears(oss, month_start, r_period)

  return(oss)
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

  wait_build <- filter(oss, category == "Building" & datein <= cutoff & datein >= cutup) %>%
                summarise(measure = "mean wait for building permit", value = mean(timewaited))

  wait_any <- filter(oss, datein <= cutoff & datein >= cutup) %>%
                    summarise(measure = "mean wait for any permit", value = mean(timewaited))

  wait_biz <- filter(oss, category == "Business" & datein <= cutoff & datein >= cutup) %>%
                    summarise(measure = "mean wait for business license", value = mean(timewaited))

  wait_pay <- filter(oss, category == "Payment" & datein <= cutoff & datein >= cutup) %>%
                    summarise(measure = "mean wait for payment", value = mean(timewaited))

  kpi <- rbind(kpi,
               wait_build,
               wait_any,
               wait_biz,
               wait_pay)

  save(kpi, file = "./data/kpi.Rdata")
}

# plot
plotOss <- function() {
#
#

theme_set(theme_opa())

ossSP <- function() {
  d <-  filter(oss, category == "Building" |
               category == "Electrical" |
               category == "Inspections" |
               category == "Mechanical" |
               category == "Plan Review") %>%
        group_by(month_start, category)

  d_all <- group_by(d, month_start) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  d_cat <- group_by(d, month_start, category) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  p_vol <- lineOPA(filter(d_all, variable == "n"), "month_start", "value", "Safety and Permits customers", labels = "format(value, big.mark = \",\", scientific = FALSE)")
  p_times <- lineOPA(filter(d_all, variable != "n"), "month_start", "value", "Safety and Permits processing times (minutes)", group = "variable", legend.labels = c("Median wait time", "Median service time"), labels = "format(value, big.mark = \",\", scientific = FALSE)" )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/14-oss-sp-visitors.png", plot = p_vol, width = 7.42, height = 5.75)
  ggsave("./output/15-oss-sp-times.png", plot = p_times, width = 7.42, height = 5.75)

  # facet chart
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$month_start)) {
    if(d_cat$category[i] == "Plan Review" & d_cat$variable[i] == "medianserve" | d_cat$category[i] == "Mechanical" & d_cat$variable[i] == "medianserve" | d_cat$category[i] == "Electrical" & d_cat$variable[i] == "medianserve") {
      d_cat$highlight[i] <- "yes"
    }
  }

  #relabel
  d_cat$variable <- as.character(d_cat$variable)
  for(i in 1:nrow(d_cat)) {
    if(d_cat$variable[i] == "n") {
      d_cat$variable[i] <- "Customers"
    } else if (d_cat$variable[i] == "medianwait") {
      d_cat$variable[i] <- "Wait Time"
    } else if (d_cat$variable[i] == "medianserve") {
      d_cat$variable[i] <- "Service Time"
    } else {
      NA
    }
  }

  p_facet <- wiseChart(d_cat, "month_start", "value", "variable ~ category", "Saftey and Permits stats by queue (minutes)")
  p_facet <- buildChart(p_facet)
  ggsave("./output/16-oss-sp-facet.png", plot = p_facet, width = 7.42, height = 5.75)
}

ossCPNC <- function() {
  d <-  filter(oss,
              category == "Brake Tag" |
              category == "Citation" |
              category == "CPNC" |
              category == "Driver" |
              category == "Other" |
              category == "Tour Guide"
             ) %>%
        group_by(month_start, category)

  d_all <- group_by(d, month_start) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  d_cat <- group_by(d, month_start, category) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  p_vol <- lineOPA(filter(d_all, variable == "n"), "month_start", "value", "Taxi Cab Bureau customers", labels = "format(value, big.mark = \",\", scientific = FALSE)")
  p_times <- lineOPA(filter(d_all, variable != "n"), "month_start", "value", "Taxi Cab Bureau processing times (minutes)", group = "variable", legend.labels = c("Median wait time", "Median service time"), labels = "format(value, big.mark = \",\", scientific = FALSE)" )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/18-oss-cpnc-visitors.png", plot = p_vol, width = 7.42, height = 5.75)
  ggsave("./output/19-oss-cpnc-times.png", plot = p_times, width = 7.42, height = 5.75)

  # facet chart
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$month_start)) {
    if(d_cat$category[i] == "Driver" & d_cat$variable[i] == "n") {
      d_cat$highlight[i] <- "yes"
    }
  }

  # relabel
  d_cat$variable <- as.character(d_cat$variable)
  for(i in 1:nrow(d_cat)) {
    if(d_cat$variable[i] == "n") {
      d_cat$variable[i] <- "Customers"
    } else if (d_cat$variable[i] == "medianwait") {
      d_cat$variable[i] <- "Wait Time"
    } else if (d_cat$variable[i] == "medianserve") {
      d_cat$variable[i] <- "Service Time"
    } else {
      NA
    }
  }

  p_facet <- wiseChart(d_cat, "month_start", "value", "variable ~ category", "Taxi Cab Bureau stats by queue (minutes)")
  p_facet <- buildChart(p_facet)
  ggsave("./output/20-oss-cpnc-facet.png", plot = p_facet, width = 7.42, height = 5.75)
}

ossEtc <- function() {
  d <- filter(oss,
              category == "Business" |
              category == "CPC" |
              category == "HDLC" |
              category == "Payment" |
              category == "Event" |
              category == "VCC" |
              category == "Zoning"
              )%>%
        group_by(month_start, category)

  d_all <- group_by(d, month_start) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  d_cat <- group_by(d, month_start, category) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  p_vol <- lineOPA(filter(d_all, variable == "n"), "month_start", "value", "CPC, VCC, HDLC customers", labels = "format(value, big.mark = \",\", scientific = FALSE)")
  p_times <- lineOPA(filter(d_all, variable != "n"), "month_start", "value", "CPC, VCC, HDLC processing time (minutes)", group = "variable", legend.labels = c("Median wait time", "Median service time"), labels = "format(value, big.mark = \",\", scientific = FALSE)" )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/22-oss-etc-visitors.png", plot = p_vol, width = 7.42, height = 5.75)
  ggsave("./output/23-oss-etc-times.png", plot = p_times, width = 7.42, height = 5.75)

  # facet chart
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$month_start)) {
    if(d_cat$category[i] == "Event" & d_cat$variable[i] == "medianserve" | d_cat$category[i] == "Payment" & d_cat$variable[i] == "n") {
      d_cat$highlight[i] <- "yes"
    }
  }

  # relabel
  d_cat$variable <- as.character(d_cat$variable)
  for(i in 1:nrow(d_cat)) {
    if(d_cat$variable[i] == "n") {
      d_cat$variable[i] <- "Customers"
    } else if (d_cat$variable[i] == "medianwait") {
      d_cat$variable[i] <- "Wait Time"
    } else if (d_cat$variable[i] == "medianserve") {
      d_cat$variable[i] <- "Service Time"
    } else {
      NA
    }
  }

  p_facet <- wiseChart(d_cat, "month_start", "value", "variable ~ category", "CPC, VCC, HDLC stats by queue (minutes)")
  p_facet <- buildChart(p_facet)
  ggsave("./output/24-oss-etc-facet.png", plot = p_facet, width = 7.42, height = 5.75)

}

# execute
ossSP()
ossCPNC()
ossEtc()

#
# end plotOss
}

# load
oss <- read.csv("./data/oss.csv", sep = ";", header = TRUE)

# execute
oss <- cleanOss()
plotOss()
set_kpis()
