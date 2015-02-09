#clean.R
#loads, cleans, and bins data for plotting

#TODO: everything

require(gdata)
require(lubridate)

init_clean <- function() {
#
#

#oss
clean_oss <- function() {
  #assumes oss data has been loaded
  names(oss) <- slugify(names(oss))
  oss$completedby <- gsub(",", "", oss$completedby)
  oss$datein <- mdy(oss$datein)
  oss$queue <- as.character(oss$queue)
  oss$category <- as.factor( oss_lookup$category[match(oss$queue, oss_lookup$lookup)] )
}

#load
oss <- read.csv("./data/oss-service-report.csv", sep = ";", header = TRUE)


#
#end init_clean
}
