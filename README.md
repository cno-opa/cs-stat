# Customer Service STAT

## To do

 * Make scans of OSS data more robust my converting to lowercase and removing garbage
 * Consider breaking up chart calls into separate files for easier reading

## Usage

Make sure all the packages listed in __Dependencies__ (see below) are installed on your machine and then run `main.R`. This looks for Excel files in a `data` directory and writes output to a `output` one, so make sure you have those.

__How it works:__ `main.R` looks in the `R` subdirectory and sources all the R files in there. It then executes those files in a sequence explicitly declared in `main.R`. If you extend this bundle of scripts, make sure you add a call to execute any new files you add to `R` or any other subdirectory, and execute these new scripts _after_ any scripts they depend on.

After the `clean.R` cleans and transforms the data for use, the data object is saved as `data/data-cleaned.Rdata`. Any extensions should access that data file and not overwrite it. If you need to modify data and save it to be accessed by other scripts, please save the file as something else. I know this isn't the most robust way of maintaining data state, but it's simpler than passing data objects between scripts in `R` and `main.R`.

## Dependencies

 * gdata
 * lubridate
 * dplyr
 * ggplot2
 * stringr
 * scales
 * reshape2
