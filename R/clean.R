#clean.R
#loads, cleans, and bins data for plotting

#TODO: everything

require(gdata)
require(lubridate)
require(dplyr)

init_clean <- function() {
#
#

#oss
clean_oss <- function() {
  #assumes oss data has been loaded
  
  load("./data/context/oss-lookup.Rdata")
  
  names(oss) <- slugify(names(oss))
  oss$completedby <- gsub(",", "", oss$completedby)
  oss$datein <- mdy(oss$datein)
  oss$queue <- as.character(oss$queue)
  oss$serviceprovided <- as.character(oss$serviceprovided)
  oss <- filter(oss, lengthofservice < 480) #remove entries that take over 8 hours, or one working day
  oss <- filter(oss, !grepl("appointment", tolower(serviceprovided)) & !grepl("meeting", tolower(serviceprovided))) #remove visits with "meeting" or "appointment"
  oss$category <- as.factor( oss_lookup$category[match(oss$queue, oss_lookup$lookup)] )

  return(oss)
}

#load
oss <- read.csv("./data/oss-service-report-2014.csv", sep = ";", header = TRUE)

#execute
oss <- clean_oss()

#save
save(list = ls(), file = "./data/data-cleaned.Rdata")

#
#end init_clean
}
