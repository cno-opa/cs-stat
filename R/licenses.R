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

  p <- schigoda(d, "month_end", "value", "Number of mechanical licenses issued, and issued in one day", fill = "variable", legend.labels = c("All licenses", "Issued in one day"))
  p <- buildChart(p)
  ggsave("./output/22-licenses-mech.png", plot = p, width = 7, height = 6.25)

  pb <- barOPA(d, "month_end", "value", "Number of mechanical licenses issued, and issued in one day", fill = "variable", position = "identity", legend.labels = c("All licenses", "Issued in one day"))
  pb <- buildChart(pb)
  ggsave("./output/22b-licenses-mech.png", plot = pb, width = 7, height = 6.25)
}

elec <- function() {
  d <- getOneYear(l, month_end, period) %>%
       filter(opa_category == "Electrical") %>%
       group_by(month_end) %>%
       summarise(all = n(), oneday = sum(daystoissue <= 1)) %>%
       filter(!is.na(oneday)) %>%
       melt()

  p <- schigoda(d, "month_end", "value", title = "Number of electrical licenses issued, and issued in one day", fill = "variable", legend.labels = c("All licenses", "Issued in one day"))
  p <- buildChart(p)
  ggsave("./output/23-licenses-elec.png", plot = p, width = 7, height = 6.25)

  pb <- barOPA(d, "month_end", "value", "Number of electrical licenses issued, and issued in one day", fill = "variable", position = "identity", legend.labels = c("All licenses", "Issued in one day"))
  pb <- buildChart(pb)
  ggsave("./output/23b-licenses-elec.png", plot = pb, width = 7, height = 6.25)
}

biz <- function() {
  d <- getOneYear(l, month_end, period) %>%
       filter(opa_category == "Business") %>%
       group_by(month_end) %>%
       summarise(all = n(), tenday = sum(daystoissue <= 10), fiveday = sum(daystoissue <= 5), oneday = sum(daystoissue <= 1) ) %>%
       melt()

  p <- schigoda(d, "month_end", "value", title = "Number and days to issue for business licenses", fill = "variable", legend.labels = c("All licenses", "Ten days", "Five days", "One day"))
  p <- buildChart(p)
  ggsave("./output/24-licenses-biz.png", plot = p, width = 7, height = 6.25)

  pb <- barOPA(d, "month_end", "value", "Number and days to issue for business licenses", fill = "variable", position = "identity", legend.labels = c("All licenses", "Ten days", "Five days", "One day"))
  pb <- buildChart(pb)
  ggsave("./output/24b-licenses-biz.png", plot = pb, width = 7, height = 6.25)
}

cpnc <- function() {
  d <- getOneYear(l, month_end, period) %>%
       filter(opa_category == "CPNC") %>%
       group_by(month_end, type) %>%
       summarise(n = n(), days_to_issue = mean(daystoissue)) %>%
       melt()

  p_n <- lineOPA(filter(d, variable == "n"), "month_end", "value", "Number of CPNC licenses issued", group = "type")
  p_n <- buildChart(p_n)
  ggsave("./output/25-licenses-cpnc-n.png", plot = p_n, width = 7, height = 6.25)

  p_m <- lineOPA(filter(d, variable == "days_to_issue"), "month_end", "value", "Average days to issue CPNC licenses", group = "type")
  p_m <- buildChart(p_m)
  ggsave("./output/26-licenses-cpnc-mean.png", plot = p_m, width = 7, height = 6.25)
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
l <- read.csv("./data/licenses.csv", header = TRUE)

# execute
l <- cleanL(l)
plotL()
