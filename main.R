#!usr/bin/Rscript
#runs the whole kit and kaboodle

.libPaths("C:\\Rpackages")

#initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  cat("Running!\n")
}

#sequence of script executions
init("R")
init_clean()
init_plot()

#finish
cat("Finished!")
