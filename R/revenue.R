# wut for revenue

#plot
plotRev <- function() {
  d <- group_by(rev, month_start, opa_category) %>%
       summarise(n = n(), meanwait = mean(timewaited), meanserv = mean(lengthofservice)) %>%
       filter(opa_category != "Other") %>%
       melt()

  #facet chart
  d$highlight <- "no"

  for(i in 1:length(d$month_start)) {
   if(d$opa_category[i] == "Business Intake" & d$variable[i] == "n") {
     d$highlight[i] <- "yes"
   }
  }

  brks <- unique(d$month_start)[seq(1, 13, 5)]

  #relabel
  d$variable <- as.character(d$variable)
  for(i in 1:nrow(d)) {
    if(d$variable[i] == "n") {
      d$variable[i] <- "Visitors"
    } else if (d$variable[i] == "meanwait") {
      d$variable[i] <- "Mean Wait"
    } else if (d$variable[i] == "meanserve") {
      d$variable[i] <- "Mean Service"
    } else {
      NA
    }
  }

  p_facet <-ggplot(d, aes(month_start, value, group = opa_category, colour = highlight)) +
            geom_line(size = 1) +
            facet_grid(variable ~ opa_category, scales = "free_y") +
            labs(title = "Revenue stats by queue", x = "", y = "") +
            scale_colour_manual(values = c("grey70", "tomato")) +
            scale_x_discrete(breaks = brks) +
            theme(panel.grid.major.y = element_blank(),
                  panel.background = element_rect(fill = "grey90"),
                  legend.position = "none",
                  strip.background = element_blank(),
                  strip.text.x = element_text(face = "bold"),
                  strip.text.y = element_text(face = "bold")
                 )
  p_facet <- buildChart(p_facet)
  ggsave("./output/21-rev-facet.png", plot = p_facet, width = 7, height = 6.25)
}

#load
rev <- read.csv("./data/revenue.csv", sep = ";", header = TRUE)

#execute
rev <- cleanServiceReport(rev)
  rev$opa_category <- NA
  for(i in 1:nrow(rev)) {
    if(rev$queue[i] == "Account Maint." | rev$queue[i] == "Administration" | rev$queue[i] == "Account Admin.") {
      rev$opa_category[i] <- "Account Maintenance and Administration"
    } else if(rev$queue[i] == "Business Regist.") {
      rev$opa_category[i] <- "Business Intake"
    } else if(rev$queue[i] == "Enforcement") {
      rev$opa_category[i] <- "Enforcement"
    } else if(rev$queue[i] == "Alcoholic Beverage") {
      rev$opa_category[i] <- "ABO"
    } else {
      rev$opa_category[i] <- "Other"
    }
  }

plotRev()
