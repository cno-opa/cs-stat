# runs the whole kit and kaboodle

.libPaths("C:\\Rpackages")

# get user to set reporting period
cat("What is the reporting period?\n\n##(Use mmm yyyy format, please)##\n\n")
r_period <- readLines("stdin", 1, warn = FALSE)

# initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  cat("Running!\n")
}

reset_kpi <- function() {
  kpi <- data.frame(measure = NA, value = NA)
  save(kpi, file = "./data/kpi.Rdata")
}

#function to source files stored on Github. use this to source shared scripts stored on Github by passing the raw URL as u in this function. see below
source_https <- function(u, unlink.tmp.certs = FALSE) {
    require(RCurl)

    if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
    script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
    if(unlink.tmp.certs) unlink("cacert.pem")

    eval(parse(text = script), envir= .GlobalEnv)
}

# sequence of script executions
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R")
#source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/mappers.R")
init("R/lib")
reset_kpi()
init("R")
print_kpis()

#calcKPIs("Q1")

# finish
cat("Finished!")
