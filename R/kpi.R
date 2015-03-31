# kpi.R
#
# prints kpi measures to stdout. call wrapper function, print_kpis() when script is finished running
#
# data sources:
# ============================
#
# kpi.Rdata - a table updated by the bundle scripts
#
# ============================
#
#

print_kpis <- function() {
  load('./data/kpi.Rdata')
  print(kpi)
  write.csv(kpi, "./output/kpi.csv", row.names = FALSE)
}
