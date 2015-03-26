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
  oss <- getOneYear(oss, month_start, period)

  return(oss)
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

  p_vol <- lineOPA(filter(d_all, variable == "n"), "month_start", "value", "Safety and Permits customers", labels = "value")
  p_times <- lineOPA(filter(d_all, variable != "n"), "month_start", "value", "Safety and Permits processing times (minutes)", group = "variable", legend.labels = c("Median wait time", "Median service time"), labels = "value" )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/10-oss-sp-visitors.png", plot = p_vol, width = 7.42, height = 5.75)
  ggsave("./output/11-oss-sp-times.png", plot = p_times, width = 7.42, height = 5.75)

  # facet chart
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$month_start)) {
    if(d_cat$category[i] == "Plan Review" & d_cat$variable[i] == "medianserve") {
      d_cat$highlight[i] <- "yes"
    }
  }

  brks <- unique(d$month_start)[seq(1, 13, 5)]

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

  p_facet <- ggplot(d_cat, aes(month_start, value, group = category, colour = highlight)) +
             geom_line(size = 1) +
             facet_grid(variable ~ category, scales = "free_y") +
             labs(title = "Saftey and Permits stats by queue (times in minutes)", x = "", y = "") +
             scale_colour_manual(values = c("grey70", "tomato")) +
             scale_x_discrete(breaks = brks) +
             theme(panel.grid.major.y = element_blank(),
                   panel.background = element_rect(fill = "grey90"),
                   legend.position = "none",
                   strip.background = element_blank(),
                   strip.text.x = element_text(face = "bold"),
                   strip.text.y = element_text(face = "bold"),
                   axis.text.x = element_blank()
                  )
  p_facet <- buildChart(p_facet)
  ggsave("./output/12-oss-sp-facet.png", plot = p_facet, width = 7.42, height = 5.75)
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

  p_vol <- lineOPA(filter(d_all, variable == "n"), "month_start", "value", "Taxi Cab Bureau customers", labels = "value")
  p_times <- lineOPA(filter(d_all, variable != "n"), "month_start", "value", "Taxi Cab Bureau processing times (minutes)", group = "variable", legend.labels = c("Median wait time", "Median service time"), labels = "value" )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/13-oss-cpnc-visitors.png", plot = p_vol, width = 7.42, height = 5.75)
  ggsave("./output/14-oss-cpnc-times.png", plot = p_times, width = 7.42, height = 5.75)

  # facet chart
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$month_start)) {
    if(d_cat$category[i] == "Driver" & d_cat$variable[i] == "n") {
      d_cat$highlight[i] <- "yes"
    }
  }

  brks <- unique(d$month_start)[seq(1, 13, 5)]

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

  p_facet <- ggplot(d_cat, aes(month_start, value, group = category, colour = highlight)) +
             geom_line(size = 1) +
             facet_grid(variable ~ category, scales = "free_y") +
             labs(title = "Taxi Cab Bureau stats by queue (times in minutes)", x = "", y = "") +
             scale_colour_manual(values = c("grey70", "tomato")) +
             scale_x_discrete(breaks = brks) +
             theme(panel.grid.major.y = element_blank(),
                   panel.background = element_rect(fill = "grey90"),
                   legend.position = "none",
                   strip.background = element_blank(),
                   strip.text.x = element_text(face = "bold"),
                   strip.text.y = element_text(face = "bold"),
                   axis.text.x = element_blank()
                  )
  p_facet <- buildChart(p_facet)
  ggsave("./output/15-oss-cpnc-facet.png", plot = p_facet, width = 7.42, height = 5.75)
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

  p_vol <- lineOPA(filter(d_all, variable == "n"), "month_start", "value", "CPC, VCC, etc. customers", labels = "value")
  p_times <- lineOPA(filter(d_all, variable != "n"), "month_start", "value", "CPC, VCC, etc. processing time (minutes)", group = "variable", legend.labels = c("Median wait time", "Median service time"), labels = "value" )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/16-oss-etc-visitors.png", plot = p_vol, width = 7.42, height = 5.75)
  ggsave("./output/17-oss-etc-times.png", plot = p_times, width = 7.42, height = 5.75)

  # facet chart
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$month_start)) {
    if(d_cat$category[i] == "Event" & d_cat$variable[i] == "medianwait") {
      d_cat$highlight[i] <- "yes"
    }
  }

  brks <- unique(d$month_start)[seq(1, 13, 5)]

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

  p_facet <- ggplot(d_cat, aes(month_start, value, group = category, colour = highlight)) +
             geom_line(size = 1) +
             facet_grid(variable ~ category, scales = "free_y") +
             labs(title = "CPC, VCC, etc. stats by queue (time in minutes)", x = "", y = "") +
             scale_colour_manual(values = c("grey70", "tomato")) +
             scale_x_discrete(breaks = brks) +
             theme(panel.grid.major.y = element_blank(),
                   panel.background = element_rect(fill = "grey90"),
                   legend.position = "none",
                   strip.background = element_blank(),
                   strip.text.x = element_text(face = "bold"),
                   strip.text.y = element_text(face = "bold"),
                   axis.text.x = element_blank()
                  )
  p_facet <- buildChart(p_facet)
  ggsave("./output/18-oss-etc-facet.png", plot = p_facet, width = 7.42, height = 5.75)

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
