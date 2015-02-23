# Customer Service STAT

## To do

 * Refactor everything

## Usage

Make sure all the packages listed in __Dependencies__ (see below) are installed on your machine and then run `main.R`. This looks for data files in a `data` directory and writes output to a `output` one, so make sure you have those. You need the following files, named accordingly, to create this STAT report:

 * complaints.csv
 * hdlc-permits.csv
 * inspections-biz.csv
 * inspections-bldg-recent.csv
 * licenses.csv
 * oss-service-report.csv
 * permits.csv
 * revenue.csv
 * vcc.csv

__How it works:__ `main.R` looks in the `R/lib` subdirectory and sources all the R files in there, and then does the same in `R`. `lib` contains helper functions and all dependencies that the individual scripts in `R` need. Anything in `R` should be able to run on its own once `R/lib` has been sourced. If you extend this script, make sure your code follows that workflow.

This report draws on a lot of separate data sources, and several different types of data sources (permits, service report, etc. -- see `cleaners.R`). Each file in `R` is responsible for its type of data source. Some of the analyses depend on historical data, which is contained in the `data/context` directory.

## Dependencies

 * lubridate
 * dplyr
 * ggplot2
 * stringr
 * scales
 * reshape2
