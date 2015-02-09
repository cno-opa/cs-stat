#!usr/bin/Rscript
#runs the whole kit and kaboodle

require(xtermStyle)

#initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  print( style( "Running!", fg = 208) )
}

#sequence of script executions
init("R")
