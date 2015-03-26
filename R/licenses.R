# licenses.R
#
# data sources:
# ============================
#
# licenses.csv - csv generated from LAMA licenses module query.
#
# ============================
#
#

# clean
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
  df$opa_category <- sapply(df$type, categorize)

  return(df)
}

# plot
plotL <- function() {
#
#

theme_set(theme_opa())

mech <- function() {
  d <- getOneYear(l, month_end, period) %>%
       filter(opa_category == "Mechanical") %>%
       group_by(month_end) %>%
       summarise(all = n(), oneday = sum(daystoissue <= 1)) %>%
       filter(!is.na(oneday)) %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Mechanical licenses, number and days to issue", fill = "variable", position = "identity", legend.labels = c("More than one day", "Same day"))
  p <- buildChart(p)
  ggsave("./output/22-licenses-mech.png", plot = p, width = 7.42, height = 5.75)
}

elec <- function() {
  d <- getOneYear(l, month_end, period) %>%
       filter(opa_category == "Electrical") %>%
       group_by(month_end) %>%
       summarise(all = n(), oneday = sum(daystoissue <= 1)) %>%
       filter(!is.na(oneday)) %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Electrical licenses, number and days to issue", fill = "variable", position = "identity", legend.labels = c("More than one day", "Same day"))
  p <- buildChart(p)
  ggsave("./output/23-licenses-elec.png", plot = p, width = 7.42, height = 5.75)
}

biz <- function() {
  d <- getOneYear(l, month_end, period) %>%
       filter(opa_category == "Business") %>%
       group_by(month_end) %>%
       summarise(all = n(), tenday = sum(daystoissue <= 10), fiveday = sum(daystoissue <= 5), oneday = sum(daystoissue <= 1) ) %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Business licenses, number and days to issue", fill = "variable", position = "identity", legend.labels = c("More than ten days", "Ten days", "Five days", "One day"))
  p <- buildChart(p)
  ggsave("./output/24-licenses-biz.png", plot = p, width = 7.42, height = 5.75)
}

cpnc <- function() {
  d <- getOneYear(l, month_end, period) %>%
       filter(opa_category == "CPNC") %>%
       group_by(month_end, type) %>%
       summarise(n = n(), days_to_issue = mean(daystoissue)) %>%
       melt()

  p_n <- lineOPA(filter(d, variable == "n"), "month_end", "value", "Number of CPNC licenses issued", group = "type", labels = "value")
  p_n <- buildChart(p_n)
  ggsave("./output/25-licenses-cpnc-n.png", plot = p_n, width = 7.42, height = 5.75)

  p_m <- lineOPA(filter(d, variable == "days_to_issue"), "month_end", "value", "Average days to issue a CPNC license", group = "type", labels = "round(value)")
  p_m <- buildChart(p_m)
  ggsave("./output/26-licenses-cpnc-mean.png", plot = p_m, width = 7.42, height = 5.75)
}

# execute
mech()
elec()
biz()
cpnc()

#
#
}

# load
l <- read.csv("./data/licenses-issued.csv", header = TRUE)

# execute
l <- cleanL(l)
plotL()
