#wut for licenses

cleanL <- function(df) {

  categorize <- function(type) {
    if (type == "Business License" | type == "Temporary Business License") {
      "Business"
    } else if (type == "Driver CPNC" | type == "Tour Guide") {
      "CPNC"
    } else if (type == "Electrical A - Contractor" |
               type == "Electrical C - Maintenance" |
               type == "Electrical D - Journeyman" |
               type == "Electrical E - Helper"
              ) {
      "Electrical"
    } else if (type == "Mechanical A - AC and Refrigeration" |
               type == "Mechanical A - Master Gasfitter" |
               type == "Mechanical B - Journeyman Gasfitter" |
               type == "Mechanical B - Journeyman Mechanic"
               ) {
      "Mechanical"
    } else {
      NA
    }
  }

  df <- cleanLicenses(df)
  df$opa_category <- NA
  for(i in 1:nrow(df)) {
    df$opa_category[i] <- categorize(df$type[i])
  }

  return(df)
}

plotL <- function() {
#
#

theme_set(theme_opa())

mech <- function() {
  d <- filter(l, opa_category == "Mechanical") %>%
       group_by(my) %>%
       summarise(all = n(), oneday = sum(daystoissue <= 1)) %>%
       filter(!is.na(oneday)) %>%
       melt()

  p <- schigoda(d, "my", "value", fill = "variable", legend.labels = c("All licenses", "Issued in one day"))
  p <- buildChart(p)
  ggsave("./output/22-licenses-mech.png", plot = p, width = 10, height = 7.5)
}

elec <- function() {
  d <- filter(l, opa_category == "Electrical") %>%
       group_by(my) %>%
       summarise(all = n(), oneday = sum(daystoissue <= 1)) %>%
       filter(!is.na(oneday)) %>%
       melt()

  p <- schigoda(d, "my", "value", fill = "variable", legend.labels = c("All licenses", "Issued in one day"))
  p <- buildChart(p)
  ggsave("./output/23-licenses-elec.png", plot = p, width = 10, height = 7.5)
}

biz <- function() {
  d <- filter(l, opa_category == "Business") %>%
       group_by(my) %>%
       summarise(all = n(), tenday = sum(daystoissue <= 10), fiveday = sum(daystoissue <= 5), oneday = sum(daystoissue <= 1) ) %>%
       filter(my != "NA NA") %>%
       melt()

  p <- schigoda(d, "my", "value", fill = "variable", legend.labels = c("All licenses", "Ten days", "Five days", "One day"))
  p <- buildChart(p)
  ggsave("./output/24-licenses-biz.png", plot = p, width = 10, height = 7.5)
}

cpnc <- function() {
  d <- filter(l, opa_category == "CPNC") %>%
       group_by(my, type) %>%
       summarise(n = n(), days_to_issue = mean(daystoissue)) %>%
       filter(my != "NA NA") %>%
       melt()

  p_n <- lineOPA(filter(d, variable == "n"), "my", "value", "Number of CPNC licenses issued", group = "type")
  p_n <- buildChart(p_n)
  ggsave("./output/25-licenses-cpnc-n.png", plot = p_n, width = 10, height = 7.5)

  p_m <- lineOPA(filter(d, variable == "days_to_issue"), "my", "value", "Average days to issue CPNC licenses", group = "type")
  p_m <- buildChart(p_m)
  ggsave("./output/26-licenses-cpnc-mean.png", plot = p_m, width = 10, height = 7.5)
}

#execute
mech()
elec()
biz()
cpnc()

#
#
}

#load
l <- read.csv("./data/licenses.csv", header = TRUE)

#execute
l <- cleanL(l)
plotL()
