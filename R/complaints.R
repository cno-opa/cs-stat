#wut for complaints

#clean
cleanSPComplaints <- function(data) {

  categorize <- function(type) {
    if(type == "Building Code" | type == "Working Without Permit" | type == "Imminent Danger of Collapse") {
      "Building"
    } else if(type == "Zoning General" | type == "Zoning - Paving/Parking" | type == "Illegal Sign" | type == "Junk and Debris") {
      "Zoning"
    } else {
      type
    }
  }

  data <- cleanComplaints(data)
  data <- filter(data, origin == "Business" | origin == "Police" | origin == "Citizen")
  data$month_filed <- as.factor(as.yearmon(data$d_filed))
  data$open_end_of_month <- ifelse(month(data$d_filed) < month(data$firstinspection), TRUE, FALSE)
  data$opa_category <- NA
  for(i in 1:nrow(data)) {
    data$opa_category[i] <- categorize(data$type[i])
  }
  return(data)
}

#plot
plotComplaints <- function() {
#
#

theme_set(theme_opa())

building <- function() {
  d <- filter(complaints, opa_category == "Building") %>%
       group_by(my) %>%
       summarise(n = n(), target = sum(daystoinspect <= 7, na.rm = TRUE)) %>%
       filter(my != "NA NA") %>%
       melt()

  p <- schigoda(d, "my", "value", fill = "variable", legend.labels = c("All", "Inspections in less than 7 days"))
  p <- buildChart(p)
  ggsave("./output/28-complaints-building.png", plot = p, width = 10, height = 7.5)
}

zoning <- function() {
  d <- filter(complaints, opa_category == "Zoning") %>%
       group_by(my) %>%
       summarise(n = n(), target = sum(daystoinspect <= 7, na.rm = TRUE)) %>%
       filter(my != "NA NA") %>%
       melt()

  p <- schigoda(d, "my", "value", fill = "variable", legend.labels = c("All", "Inspections in less than 7 days"))
  p <- buildChart(p)
  ggsave("./output/29-complaints-zoning.png", plot = p, width = 10, height = 7.5)
}

openEndOfMonth <- function() {
  d <- filter(complaints, open_end_of_month == TRUE) %>%
       group_by(month_filed) %>%
       summarise(n = n())

  p <- lineOPA(d, "month_filed", "n", "Number of complaints with no first inspection by end of month")
  p <- buildChart(p)
  ggsave("./output/30-complaints-open.png", plot = p, width = 10, height = 7.5)
}

#execute
building()
zoning()
openEndOfMonth()

#
#
}

#load
complaints <- read.csv("./data/complaints.csv", header = TRUE)

#execute
complaints <- cleanSPComplaints(complaints)
plotComplaints()
