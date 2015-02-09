#clean.R
#loads, cleans, and bins data for plotting

#TODO: everything

require(gdata)

init_clean <- function() {
#
#

#oss
clean_oss <- function(data) {
  names(data) <- slugify(names(data))
}

#load
oss <- read.csv("./data/oss-service-report.csv", sep = ";", header = TRUE)


#
#end init_clean
}
