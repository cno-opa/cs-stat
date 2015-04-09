# vcc.R
#
# data sources:
# ============================
#
# vcc.csv - csv of VCC permits. Generated from LAMA permits query.
# vcc-xf.csv - csv of VCC permits with information not stored in LAMA, such as whether permit is filed in response to a violation, and whether is is staff approvable. Produced by VCC staff monthly.
# vcc-hist.Rdata - same data as vcc.csv, just going back to 2014.
# vcc-hist-xf.Rdata - same data as vcc-xf, just going back to 2014. Both of these are needed for this script to work.
#
# ============================
#
#


# clean
cleanVCC <- function(permits) {
  permits <- cleanPermits(permits)
  permits$numstring <- gsub("(-*[A-Z])", "", permits$numstring)
  return(permits)
}

cleanVCCXf <- function(data) {
  names(data) <- slugify(names(data))
  data$x <- NULL
  return(data)
}

crossAndBuild <- function() {
  load("./data/context/vcc-hist.Rdata")

  # get staff approvable and whether or not in response to a violation
  vcc$staff <- NA
  vcc$violation <- NA
  for(i in 1:nrow(vcc)) {
    r <- vcc$numstring[i]
    lookup <- match(r, vcc_xf$permit_)
    lookup_val_staff <- vcc_xf$level[lookup]
    lookup_val_violation <- vcc_xf$in_response_to_a_violation[lookup]

    vcc$staff[i] <- as.character(lookup_val_staff)
    vcc$violation[i] <- as.character(lookup_val_violation)
  }

  # save looked-up data
  save(vcc, file = paste0("./data/context/vcc-cross-ref-", Sys.Date(), ".csv"))

  # join to preprocessed historical data
  vcc_all <- rbind(vcc, vcc_hist)

  # fix weirdness with date factors
  vcc_all$month_start <- as.factor(as.yearmon(vcc_all$month_start))
  vcc_all$month_end <- as.factor(as.yearmon(vcc_all$month_end))

  # determine if target was met
  vcc_all$under_target <- NA
  for(i in 1:nrow(vcc_all)) {
    if( year(as.yearmon(vcc_all$month_end[i])) == 2014 ) {
      if(vcc_all$daystoissue[i] <= 5) {
        vcc_all$under_target[i] <- TRUE
      } else if(vcc_all$daystoissue[i] > 5) {
        vcc_all$under_target[i] <- FALSE
      } else {
        vcc_all$under_target[i] <- NA
      }
    }

    if( year(as.yearmon(vcc_all$month_end[i])) == 2015 ) {
      if(vcc_all$daystoissue[i] <= 7) {
        vcc_all$under_target[i] <- TRUE
      } else if(vcc_all$daystoissue[i] > 7) {
        vcc_all$under_target[i] <- FALSE
      } else {
        vcc_all$under_target[i] <- NA
      }
    }
  }

  return(vcc_all)

}

# plot
plotVCC <- function() {
#
#

theme_set(theme_opa())

timeliness <- function() {

  d <- getTwoYears(vcc_all, month_end, period) %>%
       filter(staff == "staff") %>%
       group_by(month_end) %>%
       summarise(n = n(), target = sum(under_target == TRUE)) %>%
       melt()

  p <- barOPA(d, "month_end", "value", title = "Staff approvable reviews finished over and under target time", fill = "variable", position = "identity", legend.labels = c("Over target time", "Under target time"))
  p <- buildChart(p)
  ggsave("./output/50-vcc-review.png", plot = p, width = 7.42, height = 5.75)
}

responsiveness <- function() {

  d <- getTwoYears(vcc_all, month_end, period) %>%
       filter(staff == "staff") %>%
       group_by(month_end) %>%
       summarise(not_response = sum(violation == "N"), response = sum(violation == "Y")) %>%
       melt()

  p <- barOPA(d, "month_end", "value", "Number of applications approved due to violations", fill = "variable", position = "stack", legend.labels = c("In response to violations", "Not in response to violations"))
  p <- p + scale_fill_manual(values = c(lightBlue, darkBlue), labels = c("Not in response to violations", "In response to violations"))
  p <- buildChart(p)
  ggsave("./output/51-vcc-responses.png", plot = p, width = 7.42, height = 5.75)
}

# execute
timeliness()
responsiveness()

#
#
}

# load
vcc <- read.csv("./data/vcc.csv", header = TRUE)
vcc_xf <- read.csv("./data/vcc-xf.csv", header = TRUE)

# execute
vcc <- cleanVCC(vcc)
vcc_xf <- cleanVCCXf(vcc_xf)
vcc_all <- crossAndBuild()
plotVCC()
