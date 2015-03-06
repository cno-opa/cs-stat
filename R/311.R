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

#plots
plot311 <- function() {
#
#

  theme_set(theme_opa())

callVol <- function() {
  d <- filter(qls, measure == "Calls") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Call Volume", "Date", "Calls", labels = "n")
  p <- buildChart(p)
  ggsave("./output/4-311-calls.png", plot = p, width = 10, height = 7.5)
}

callAbandon <- function() {
  d <- filter(qls, measure == "Abandonment Rate") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Abandonment Rate", "Date", "Rate", labels = "percent(n)", percent = TRUE)
  p <- buildChart(p)
  ggsave("./output/5-311-abandonment.png", plot = p, width = 10, height = 7.5)
}

holdTime <- function() {
  d <- filter(qls, measure == "Avg. Hold Time (sec)") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "Average hold time", "Date", "Seconds", labels = "n")
  p <- buildChart(p)
  ggsave("./output/6-311-hold-time.png", plot = p, width = 10, height = 7.5)
}

firstCall <- function() {
  d <- filter(qls, measure == "First Call Resolution") %>%
       group_by(date) %>%
       summarise(n = value)

  p <- lineOPA(d, "date", "n", "First call resolution", "Date", "Rate", labels = "percent(n)", percent = TRUE)
  p <- buildChart(p)
  ggsave("./output/7-311-first-call.png", plot = p, width = 10, height = 7.5)
}

operators <- function() {
  p <- barOPA(ops, "agent", "value", "Operator scores", "Operator", "Score (%)", position = "dodge", fill = "variable")
  p <- buildChart(p)
  ggsave("./output/8-311-operators.png", plot = p, width = 10, height = 7.5)
}

topRequest <- function() {
  top <- filter(qls,
                type == "service request",
                date == levels(qls$date)[length(levels(qls$date))]) %>%
          arrange(value)

  top <- top$measure[1:3]

  d <- filter(qls, measure == top[1] | measure == top[2] | measure == top[3])

  p <- lineOPA(d,
               "date",
               "value",
               "Top service requests",
               "Date",
               "Requests",
               labels = "value",
               group = "measure",
               highlight = "Street Light")

  p <- buildChart(p)
  ggsave("./output/9-311-top-requests.png", plot = p, width = 10, height = 7.5)
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

#execute
qls <- cleanQLS()
ops <- cleanOps()
plot311()
