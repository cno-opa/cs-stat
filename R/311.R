# wut for 311

#clean
cleanQLS <- function() {
  qls <- melt(qls)
  names(qls) <- slugify(names(qls))
  names(qls) <- c("measure", "type", "date", "value")
  qls$date <- gsub("[.]", " ", qls$date)
  qls$date <- as.factor(as.yearmon(qls$date))

  return(qls)
}

cleanOps <- function() {
  ops <- melt(ops, id.vars = "Agent")
  names(ops) <- slugify(names(ops))
  ops$variable <- gsub("[.]", " ", ops$variable)
  ops$value <- gsub("%", "", ops$value)
  ops$value <- as.numeric(ops$value)

  return(ops)
}

cleanSource <- function() {
  names(sourceD) <- slugify(names(sourceD))
  sourceD$x_1 <- NULL
  sourceD$x_2 <- NULL
  sourceD$x_3 <- NULL
  sourceD$x_4 <- NULL
  sourceD$x_5 <- NULL
  sourceD$x_6 <- NULL
  sourceD$x_7 <- NULL
  sourceD$x_8 <- NULL
  sourceD$open_dt <- mdy(sourceD$open_dt)
  sourceD$closed_dt <- mdy(sourceD$closed_dt)
  sourceD$my <- as.factor(as.yearmon(sourceD$open_dt))
  return(sourceD)
}

#plots
plot311 <- function() {
#
#

theme_set(theme_opa())

callVol <- function() {
  d <- filter(qls, measure == "Calls") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Call Volume")
  p <- buildChart(p)
  ggsave("./output/4-311-calls.png", plot = p, width = 7, height = 6.25)
}

callAbandon <- function() {
  d <- filter(qls, measure == "Abandonment Rate") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Abandonment Rate", percent = TRUE)
  p <- buildChart(p)
  ggsave("./output/5-311-abandonment.png", plot = p, width = 7, height = 6.25)
}

holdTime <- function() {
  d <- filter(qls, measure == "Avg. Hold Time (sec)") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Average hold time")
  p <- buildChart(p)
  ggsave("./output/6-311-hold-time.png", plot = p, width = 7, height = 6.25)
}

firstCall <- function() {
  d <- filter(qls, measure == "First Call Resolution") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "First call resolution", percent = TRUE)
  p <- buildChart(p)
  ggsave("./output/7-311-first-call.png", plot = p, width = 7, height = 6.25)
}

operators <- function() {
  p <- barOPA(ops, "agent", "value", "Operator scores", ylab = "Score (%)", position = "dodge", fill = "variable")
  p <- p + theme(legend.text = element_text(size = rel(0.65))) +
           guides(fill = guide_legend(nrow = 2))
  p <- buildChart(p)
  ggsave("./output/8-311-operators.png", plot = p, width = 7, height = 6.25)
}

topRequest <- function() {
  top <- filter(qls,
                type == "service request",
                date == levels(qls$date)[length(levels(qls$date))]) %>%
          arrange(value)

  top <- top$measure[1:3]
  top <- as.character(top)

  sourceD$type <- as.character(sourceD$type)
  date_cut <- max(sourceD$open_dt)
  date_cut <- ymd(paste( (year(date_cut) - 1), month(date_cut), "01", sep = "-"))

       #filter(qls, measure == top[1] | measure == top[2] | measure == top[3]) <== use that when 311 doesn't insist on changing their type designations around
  d <- filter(sourceD, agrepl(top[1], type, max.distance = 0.3) | agrepl(top[2], type, max.distance = 0.3) | agrepl(top[3], type, max.distance = 0.3)) %>%
       filter(open_dt > date_cut) %>%
       group_by(my, type) %>%
       summarise(n = n()) %>%
       melt()

  p <- lineOPA(d,
               "my",
               "value",
               "Top service requests",
               group = "type",
               highlight = "Street Light")
  p <- buildChart(p)
  ggsave("./output/9-311-top-requests.png", plot = p, width = 7, height = 6.25)
}

#execute
callVol()
callAbandon()
holdTime()
firstCall()
operators()
topRequest()

#
#
}

#load
qls <- read.xls("./data/311.xlsx", sheet = "QLS", na.strings = c("", "#N/A", "NA", "#DIV/0!", "REF!"), strip.white = TRUE, perl = "C:/Strawberry/perl/bin/perl.exe")
ops <- read.xls("./data/311.xlsx", sheet = "operator performance", na.strings = c("", "#N/A", "NA", "#DIV/0!, #REF!"), strip.white = TRUE, perl = "C:/Strawberry/perl/bin/perl.exe")
sourceD <- read.csv("./data/311-source.csv", header = TRUE)

#execute
qls <- cleanQLS()
ops <- cleanOps()
sourceD <- cleanSource()
plot311()
