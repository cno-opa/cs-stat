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

mech <- function() {
  d <- filter(l, opa_category == "Mechanical") %>%
       group_by(my) %>%
       summarise(all = n(), oneday = sum(daystoissue <= 1)) %>%
       filter(!is.na(oneday)) %>%
       melt()

  p <- schigoda(d, "my", "value", fill = "variable")
  p <- buildChart(p)
  ggsave("./output/21-licenses-mech.png", plot = p, width = 10, height = 7.5)
}

#execute
mech()

#
#
}

#load
l <- read.csv("./data/licenses.csv", header = TRUE)

#execute
l <- cleanL(l)
