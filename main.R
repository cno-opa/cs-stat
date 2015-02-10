#!usr/bin/Rscript
#runs the whole kit and kaboodle

require(xtermStyle)

#initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  cat( style( "Running!\n", fg = 069) )
}

#sequence of script executions
init("R")
init_clean()
init_plot()

#finish
cat( style( "Finished!", fg = 069 ) )
