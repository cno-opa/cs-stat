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
    if(type %in% c("Account Maint.", "Administration", "Account Admin.", "Account Maintenance", "Account Recovery", "Revenue Administration")) {
      "Accounts"
    } else if(type %in% c("Business Regist.", "Business Registration/Renewals")) {
      "Business Intake"
    } else if(type %in% c("Enforcement", "Enforcement/Special Events")) {
      "Enforcement"
    } else if(type %in% c("Alcoholic Beverage", "ABO Accounts")) {
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
       summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice)) %>%
       filter(opa_category != "Other") %>%
       melt()

  # facet chart
  d$highlight <- "no"

  for(i in 1:length(d$month_start)) {
   if(d$opa_category[i] == "Enforcement" & d$variable[i] == "meanwait") {
     d$highlight[i] <- "yes"
   }
  }

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

  p_facet <- wiseChart(d, "month_start", "value", "variable ~ opa_category", "Stats by queue (times in mean minutes)")
  p_facet <- buildChart(p_facet)
  ggsave("./output/32-rev-facet.png", plot = p_facet, width = 7.42, height = 5.75)
}

# load
rev <- read.csv("./data/revenue.csv", sep = ";", header = TRUE)

# execute
rev <- cleanRevD(rev)
plotRev()
