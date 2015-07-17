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

# sequence of script executions
init("R/lib")
reset_kpi()
init("R")
print_kpis()

# calcKPIs("Q1")

# finish
cat("Finished!")
