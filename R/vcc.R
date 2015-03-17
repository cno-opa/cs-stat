#wut for VCC

#clean
cleanVCC <- function(permits) {
  permits <- cleanPermits(permits, subset = FALSE)
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

  #get staff approvable and whether or not in response to a violation
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

  #save looked-up data
  save(vcc, file = paste0("./data/context/vcc-cross-ref-", Sys.Date()))

  #join to preprocessed historical data
  vcc_all <- rbind(vcc, vcc_hist)

  #determine if target was met
  vcc_all$under_target <- NA
  for(i in 1:nrow(vcc_all)) {
    if( year(as.yearmon(vcc_all$my[i])) == 2014 ) {
      if(vcc_all$daystoissue[i] <= 5) {
        vcc_all$under_target[i] <- TRUE
      } else if(vcc_all$daystoissue[i] > 5) {
        vcc_all$under_target[i] <- FALSE
      } else {
        vcc_all$under_target[i] <- NA
      }
    }

    if( year(as.yearmon(vcc_all$my[i])) == 2015 ) {
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

#plot

plotVCC <- function() {
#
#

theme_set(theme_opa())

timeliness <- function() {
  date_cutoff <- max(as.yearmon(vcc_all$my)) - 1

  d <- filter(vcc_all, staff == "staff" & as.yearmon(my) >= date_cutoff) %>%
       group_by(my) %>%
       summarise(n = n(), target = sum(under_target == TRUE)) %>%
       melt()

  p <- schigoda(d, "my", "value", fill = "variable", legend.labels = c("All", "Inspections within target time"))
  p <- buildChart(p)
  ggsave("./output/31-vcc-review.png", plot = p, width = 7, height = 6.25)
}

responsiveness <- function() {
  date_cutoff <- max(as.yearmon(vcc_all$my)) - 1

  d <- filter(vcc_all, staff == "staff" & as.yearmon(my) >= date_cutoff) %>%
       group_by(my) %>%
       summarise(violation_responses = sum(violation == "Y"), not_responses = sum(violation == "N")) %>%
       melt()

  d$my <- as.factor(as.yearmon(d$my))

  p <- barOPA(d, "my", "value", "Number of applications approved due to violations", fill = "variable", position = "stack", legend.labels = c("In response to violations", "Not in response to violations"))
  p <- buildChart(p)
  ggsave("./output/32-vcc-responses.png", plot = p, width = 7, height = 6.25)
}

#execute
timeliness()
responsiveness()

#
#
}

#load
vcc <- read.csv("./data/vcc.csv", header = TRUE)
vcc_xf <- read.csv("./data/vcc-xf.csv", header = TRUE)

#execute
vcc <- cleanVCC(vcc)
vcc_xf <- cleanVCCXf(vcc_xf)
vcc_all <- crossAndBuild()
plotVCC()
