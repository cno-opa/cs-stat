# rev.R
#
# data sources:
# ============================
#
# rev.csv - csv generated from Lobby Central OSS report.
#
# ============================
#
#


# clean
cleanRevD <- function(data) {
  categorize <- function(type) {
    if(type == "Account Maint." | type == "Administration" | type == "Account Admin.") {
      "Accounts"
    } else if(type == "Business Regist.") {
      "Business Intake"
    } else if(type == "Enforcement") {
      "Enforcement"
    } else if(type == "Alcoholic Beverage") {
      "ABO"
    } else {
      "Other"
    }
  }

  data <- cleanServiceReport(data)
  data$opa_category <- sapply(data$queue, categorize)

  return(data)
}

# plot
plotRev <- function() {
  d <- group_by(rev, month_start, opa_category) %>%
       summarise(n = n(), meanwait = mean(timewaited), meanserv = mean(lengthofservice)) %>%
       filter(opa_category != "Other") %>%
       melt()

  # facet chart
  d$highlight <- "no"

  for(i in 1:length(d$month_start)) {
   if(d$opa_category[i] == "Accounts" & d$variable[i] == "meanwait") {
     d$highlight[i] <- "yes"
   }
  }

  brks <- unique(d$month_start)[seq(1, 13, 5)]

  # relabel
  d$variable <- as.character(d$variable)
  for(i in 1:nrow(d)) {
    if(d$variable[i] == "n") {
      d$variable[i] <- "Customers"
    } else if (d$variable[i] == "meanwait") {
      d$variable[i] <- "Wait Time"
    } else if (d$variable[i] == "meanserve") {
      d$variable[i] <- "Service Time"
    } else {
      NA
    }
  }

  p_facet <-ggplot(d, aes(month_start, value, group = opa_category, colour = highlight)) +
            geom_line(size = 1) +
            facet_grid(variable ~ opa_category, scales = "free_y") +
            labs(title = "Revenue stats by queue (times in minutes)", x = "", y = "") +
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
  ggsave("./output/21-rev-facet.png", plot = p_facet, width = 7.42, height = 5.75)
}

# load
rev <- read.csv("./data/revenue.csv", sep = ";", header = TRUE)

# execute
rev <- cleanRevD(rev)
plotRev()
