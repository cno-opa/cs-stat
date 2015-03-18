#runs the whole kit and kaboodle

.libPaths("C:\\Rpackages")

#set reporting period
cat("What is the reporting period?\n\n##(Use mmm yyyy format, please)##\n\n")
period <- readLines("stdin", 1, warn = FALSE)

#initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  cat("Running!\n")
}

#sequence of script executions
init("R/lib")
init("R")

#finish
cat("Finished!")
