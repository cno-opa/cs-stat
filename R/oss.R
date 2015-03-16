#cleaning and plotting for One Stop Shop

cleanOss <- function() {
  load("./data/context/oss-lookup.Rdata")

  oss <- cleanServiceReport(oss)
  oss$serviceprovided <- as.character(oss$serviceprovided)
  oss <- filter(oss, lengthofservice < 480) #remove entries that take over 8 hours, or one working day
  oss <- filter(oss, !grepl("appointment", tolower(serviceprovided)) & !grepl("meeting", tolower(serviceprovided)))
  oss$category <- as.factor( oss_lookup$category[match(oss$queue, oss_lookup$lookup)] )

  return(oss)
}

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
        group_by(my, category)

  d_all <- group_by(d, my) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  d_cat <- group_by(d, my, category) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  p_vol <- lineOPA(filter(d_all, variable == "n"), "my", "value", "Safety and Permits volume")
  p_times <- lineOPA(filter(d_all, variable != "n"), "my", "value", "Safety and Permits timeliness", group = "variable", legend.labels = c("Median wait time", "Median service time") )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/10-oss-sp-visitors.png", plot = p_vol, width = 10, height = 7.5)
  ggsave("./output/11-oss-sp-times.png", plot = p_times, width = 10, height = 7.5)

  #facet chart
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$my)) {
    if(d_cat$category[i] == "Plan Review" & d_cat$variable[i] == "medianserve") {
      d_cat$highlight[i] <- "yes"
    }
  }

  brks <- unique(d$my)[seq(1, 13, 2)]

  #relabel
  d_cat$variable <- as.character(d_cat$variable)
  for(i in 1:nrow(d_cat)) {
    if(d_cat$variable[i] == "n") {
      d_cat$variable[i] <- "Visitors"
    } else if (d_cat$variable[i] == "medianwait") {
      d_cat$variable[i] <- "Median Wait"
    } else if (d_cat$variable[i] == "medianserve") {
      d_cat$variable[i] <- "Median Service"
    } else {
      NA
    }
  }

  p_facet <- ggplot(d_cat, aes(my, value, group = category, colour = highlight)) +
             geom_line(size = 1) +
             facet_grid(variable ~ category, scales = "free_y") +
             labs(title = "Saftey and Permits stats by queue", x = "", y = "") +
             scale_colour_manual(values = c("grey70", "tomato")) +
             scale_x_discrete(breaks = brks) +
             theme(panel.grid.major.y = element_blank(),
                   panel.background = element_rect(fill = "grey90"),
                   legend.position = "none",
                   strip.background = element_blank(),
                   strip.text.x = element_text(face = "bold"),
                   strip.text.y = element_text(face = "bold")
                  )
  ggsave("./output/12-oss-sp-facet.png", plot = p_facet, width = 10, height = 7.5)
}

ossCPNC <- function() {
  d <-  filter(oss,
              category == "Brake Tag" |
              category == "Citation" |
              category == "CPNC" |
              category == "Driver/Operator" |
              category == "Other" |
              category == "Tour Guide"
             ) %>%
        group_by(my, category)

  d_all <- group_by(d, my) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  d_cat <- group_by(d, my, category) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  p_vol <- lineOPA(filter(d_all, variable == "n"), "my", "value", "Taxi Cab Bureau volume")
  p_times <- lineOPA(filter(d_all, variable != "n"), "my", "value", "Taxi Cab Bureau timeliness", ylab = "Minutes", group = "variable", legend.labels = c("Median wait time", "Median service time") )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/13-oss-cpnc-visitors.png", plot = p_vol, width = 10, height = 7.5)
  ggsave("./output/14-oss-cpnc-times.png", plot = p_times, width = 10, height = 7.5)

  #facet grid
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$my)) {
    if(d_cat$category[i] == "Driver/Operator" & d_cat$variable[i] == "medianwait") {
      d_cat$highlight[i] <- "yes"
    }
  }

  brks <- unique(d$my)[seq(1, 13, 2)]

  #relabel
  d_cat$variable <- as.character(d_cat$variable)
  for(i in 1:nrow(d_cat)) {
    if(d_cat$variable[i] == "n") {
      d_cat$variable[i] <- "Visitors"
    } else if (d_cat$variable[i] == "medianwait") {
      d_cat$variable[i] <- "Median Wait"
    } else if (d_cat$variable[i] == "medianserve") {
      d_cat$variable[i] <- "Median Service"
    } else {
      NA
    }
  }

  p_facet <- ggplot(d_cat, aes(my, value, group = category, colour = highlight)) +
             geom_line(size = 1) +
             facet_grid(variable ~ category, scales = "free_y") +
             labs(title = "CPNC stats by queue", x = "", y = "") +
             scale_colour_manual(values = c("grey70", "tomato")) +
             scale_x_discrete(breaks = brks) +
             theme(panel.grid.major.y = element_blank(),
                   panel.background = element_rect(fill = "grey90"),
                   legend.position = "none",
                   strip.background = element_blank(),
                   strip.text.x = element_text(face = "bold"),
                   strip.text.y = element_text(face = "bold")
                  )
  ggsave("./output/15-oss-cpnc-facet.png", plot = p_facet, width = 10, height = 7.5)
}

ossEtc <- function() {
  d <- filter(oss,
              category == "Business Intake" |
              category == "CPC" |
              category == "HDLC" |
              category == "Payment" |
              category == "Special Event" |
              category == "VCC" |
              category == "Zoning"
              )%>%
        group_by(my, category)

  d_all <- group_by(d, my) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  d_cat <- group_by(d, my, category) %>%
           summarise(n = n(), medianwait = median(as.numeric(timewaited)), medianserve = median(as.numeric(lengthofservice))) %>%
           melt()

  p_vol <- lineOPA(filter(d_all, variable == "n"), "my", "value", "CPC, VCC, etc. volume")
  p_times <- lineOPA(filter(d_all, variable != "n"), "my", "value", "CPC, VCC, etc. timeliness", ylab = "Minutes", group = "variable", legend.labels = c("Median wait time", "Median service time") )

  p_vol <- buildChart(p_vol)
  p_times <- buildChart(p_times)

  ggsave("./output/16-oss-etc-visitors.png", plot = p_vol, width = 10, height = 7.5)
  ggsave("./output/17-oss-etc-times.png", plot = p_times, width = 10, height = 7.5)

  #facet grid
  d_cat$highlight <- "no"

  for(i in 1:length(d_cat$my)) {
    if(d_cat$category[i] == "Payment" & d_cat$variable[i] == "n") {
      d_cat$highlight[i] <- "yes"
    }
  }

  brks <- unique(d$my)[seq(1, 13, 2)]

  #relabel
  d_cat$variable <- as.character(d_cat$variable)
  for(i in 1:nrow(d_cat)) {
    if(d_cat$variable[i] == "n") {
      d_cat$variable[i] <- "Visitors"
    } else if (d_cat$variable[i] == "medianwait") {
      d_cat$variable[i] <- "Median Wait"
    } else if (d_cat$variable[i] == "medianserve") {
      d_cat$variable[i] <- "Median Service"
    } else {
      NA
    }
  }

  p_facet <- ggplot(d_cat, aes(my, value, group = category, colour = highlight)) +
             geom_line(size = 1) +
             facet_grid(variable ~ category, scales = "free_y") +
             labs(title = "VCC, HDLC, etc. stats by queue", x = "", y = "") +
             scale_colour_manual(values = c("grey70", "tomato")) +
             scale_x_discrete(breaks = brks) +
             theme(panel.grid.major.y = element_blank(),
                   panel.background = element_rect(fill = "grey90"),
                   legend.position = "none",
                   strip.background = element_blank(),
                   strip.text.x = element_text(face = "bold"),
                   strip.text.y = element_text(face = "bold")
                  )
  ggsave("./output/18-oss-etc-facet.png", plot = p_facet, width = 10, height = 7.5)

}

#execute
ossSP()
ossCPNC()
ossEtc()

#
#end plotOss
}

#load
oss <- read.csv("./data/oss.csv", sep = ";", header = TRUE)

#execute
oss <- cleanOss()
plotOss()
