#plot.R
#makes ALL the charts

#TODO: DRY

require(ggplot2)
require(dplyr)
require(scales)
require(reshape2)

init_plot <- function() {
#
#

#oss
oss_sp <- function() { #slide 10
  d <-  filter(oss, category == "Safety and Permits" | category == "Safety and Permits - Electrical" | category == "Safety and Permits - Inspections" | category == "Safety and Permits - Mechanical" | category == "Safety and Permits - Plan Review")%>%
        group_by(my, category)%>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = category, colour = category)) +
        theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
        scale_colour_discrete(name = "Category",
                              labels = c("S&P", "S&P - Elec", "S&P - Inspec", "S&P - Mech", "S&P - Plan Rev"))

  #visitors
  p + geom_line(aes(y = n)) +
      labs( title = "Visitors by month", x = "Month", y = "Visitors" ) +
      ggsave("./output/10oss-sp-visitors.png", width = 10, height = 5.5)
      cat("Saving safety and permits visitors line chart...\n")

  #mean wait time
  p + geom_line(aes(y = meanwait)) +
      labs( title = "Average wait time", x = "Month", y = "Minutes" ) +
      ggsave("./output/10oss-sp-wait.png", width = 10, height = 5.5)
      cat("Saving safety and permits mean wait time line chart...\n")

  #mean service time
  p + geom_line(aes(y = meanserve)) +
      labs( title = "Average service time", x = "Month", y = "Minutes" ) +
      ggsave("./output/10oss-sp-service.png", width = 10, height = 5.5)
      cat("Saving safety and permits mean service time line chart...\n")
}

oss_sp_permits <- function() { #slide 11
  #wait for any permit
  l <- filter(oss,
              queue == "BBSA Building Standards & Appeals" |
              queue == "Building Inspections" |
              queue == "Building Permit" |
              queue == "Dam. Assessment" |
              queue == "Demolition" |
              queue == "Document Retrieval" |
              queue == "Electrical Inspections" |
              queue == "Electrical License" |
              queue == "Electrical Permit" |
              queue == "Mechanical Inspections" |
              queue == "Mechanical License" |
              queue == "Mechanical Permit" |
              queue == "Occup. License" |
              queue == "Plan Review" |
              queue == "Stationary/Operat. Eng." |
              queue == "Zoning"
              )%>%
        group_by(my)%>%
        summarise(n = n(), mean = mean(timewaited))

  #wait for building permit
  b <- filter(oss, queue == "Building Permit")%>%
       group_by(my)%>%
       summarise(n = n(), mean = mean(timewaited))

  #any permit
  ggplot(l, aes(x = my, y = mean, group = 1)) +
  geom_line(colour = "steelblue") +
  geom_hline(aes(yintercept = 18), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs( title = "Average wait time for any license or permit", x = "Month", y = "Minutes" )
  ggsave("./output/11oss-sp-wait-all.png", width = 10, height = 5.5)
  cat("Saving safety and permits all permits mean wait time line chart...\n")

  #building permit
  ggplot(b, aes(x = my, y = mean, group = 1)) +
  geom_line(colour = "steelblue") +
  geom_hline(aes(yintercept = 18), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs( title = "Average wait time for new building permit", x = "Month", y = "Minutes" )
  ggsave("./output/11oss-sp-wait-building.png", width = 10, height = 5.5)
  cat("Saving safety and permits building permit mean wait time line chart...\n")
}

oss_etc <- function() { #slide 12
  d <- filter(oss,
              category == "Business Intake" |
              category == "CPC" |
              category == "HDLC" |
              category == "Payment" |
              category == "Special Event" |
              category == "VCC" |
              category == "Zoning"
              )%>%
        group_by(my, category)%>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = category, colour = category)) +
        theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
        labs( x = "Month", y = "Minutes" ) +
        scale_colour_discrete(name = "")

  #visitors
  p + geom_line(aes(y = n)) +
      labs(title = "Number of visitors", y = "Visitors")
      ggsave("./output/12oss-etc-visitors.png", width = 10, height = 5.5)
      cat("Saving OSS etc visitors line chart...\n")

  #mean wait
  p + geom_line(aes(y = meanwait)) +
      labs(title = "Average wait time")
      ggsave("./output/12oss-etc-mean-wait.png", width = 10, height = 5.5)
      cat("Saving OSS etc mean wait line chart...\n")

  #mean service
  p + geom_line(aes(y = meanserve)) +
      labs(title = "Average service time")
      ggsave("./output/12oss-etc-mean-service.png", width = 10, height = 5.5)
      cat("Saving OSS etc mean service line chart...\n")
}

oss_olp <- function() { #slide 13
  d <- filter(oss, queue == "Occup. License" | queue == "Payment") %>%
       group_by(my, queue) %>%
       summarise(n = n(), meanwait = mean(timewaited))

  #occupational license
  ggplot(filter(d, queue == "Occup. License"), aes(x = my, y = meanwait, group = queue)) +
  geom_line(colour = "steelblue") +
  labs(title = "Average wait time to apply for a new occupational license",x = "Month", y = "Minutes") +
  geom_hline(aes(yintercept = 18), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 18.5, label = "Target") +
  ggsave("./output/13oss-occup-license.png", width = 10, height = 5.5)
  cat("Saving OSS occupational license mean wait line chart...\n")

  #make a payement
  ggplot(filter(d, queue == "Payment"), aes(x = my, y = meanwait, group = queue)) +
  geom_line(colour = "steelblue") +
  labs(title = "Average wait time to make a payement",x = "Month", y = "Minutes") +
  geom_hline(aes(yintercept = 5), colour = "orange", linetype = "dashed") +
  annotate("text", x = 1.5, y = 5.5, label = "Target") +
  ggsave("./output/13oss-payment-mean-wait.png", width = 10, height = 5.5)
  cat("Saving OSS make a payment mean wait line chart...\n")
}

oss_cpnc <- function() { #slide 14
  d <- filter(oss,
              category == "Brake Tag" |
              category == "Citation" |
              category == "CPNC" |
              category == "Driver/Operator" |
              category == "Other" |
              category == "Tour Guide"
             ) %>%
        group_by(my, category) %>%
        summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = category, colour = category)) +
        theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
        labs( x = "Month", y = "Minutes" ) +
        scale_colour_discrete(name = "")

  #visitors
  p + geom_line(aes(y = n)) +
      labs(title = "Number of visitors", y = "Visitors")
      ggsave("./output/14oss-cpnc-visitors.png", width = 10, height = 5.5)
      cat("Saving OSS CPNC visitors line chart...\n")

  #mean wait
  p + geom_line(aes(y = meanwait)) +
      labs(title = "Average wait time")
      ggsave("./output/14oss-cpnc-mean-wait.png", width = 10, height = 5.5)
      cat("Saving OSS CPNC mean wait line chart...\n")

  #mean service
  p + geom_line(aes(y = meanserve)) +
      labs(title = "Average service time")
      ggsave("./output/14oss-cpnc-mean-service.png", width = 10, height = 5.5)
      cat("Saving OSS CPNC mean service line chart...\n")
}

permit_online <- function() { #slide 15
  all <- filter(permits, online) %>%
         group_by(my, createdby) %>%
         summarise(n = n())

  denom <- summarise(all, all = sum(n))
  numer <- filter(all, createdby == "publicwebcrm") %>%
           summarise(online = sum(n))

  d <- left_join(denom, numer, by = "my")
  d$prop <- d$online/d$all

  ggplot(d, aes(x = my, y = prop, label = percent(prop))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Percent of permit applications recieved online", x = "Month", y = "Percent") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
  geom_text(size = 4, colour = "grey33", vjust = -.5)
  ggsave("./output/15permits-online.png", width = 10, height = 5.5)
  cat("Saving percent of permits applied for online chart...\n")
}

bus_lic_online <- function() { #slide 15 also
  all <- filter(lic, type == "Business License" | type == "Temporary Business License") %>%
         group_by(my, createdby) %>%
         summarise(n = n())

  denom <- summarise(all, all = sum(n))
  numer <- filter(all, createdby == "publicwebcrm") %>%
           summarise(online = sum(n))

  d <- left_join(denom, numer, by = "my")
  d$prop <- d$online/d$all

  ggplot(d, aes(x = my, y = prop, label = percent(prop))) +
  geom_bar(stat = "identity", fill = "goldenrod") +
  labs(title = "Percent of business license applications recieved online", x = "Month", y = "Percent") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
  geom_text(size = 4, colour = "grey33", vjust = -.5)
  ggsave("./output/15bus-license-online.png", width = 10, height = 5.5)
  cat("Saving percent of business licenses applied for online chart...\n")
}

comm_res_permit <- function() { #slides 16 and 17
  d <- filter(permits, !is.na(issuedate)) %>%
       group_by(my, usetype, opa_category) %>%
       summarise(n = n())

  p <- ggplot(data = d,
              aes(x = my, y = n, group = opa_category, colour = opa_category)
              ) +
        labs(x = "Month", y = "Number") +
        theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
        scale_colour_discrete(name = "")

  p + geom_line(data = filter(d, usetype == "commercial")) +
      labs(title = "Number of commercial permits issued")
      ggsave("./output/16commercial-permits-issued.png", width = 10, height = 5.5)
      cat("Saving number of commercial permits issued chart...\n")

  p + geom_line(data = filter(d, usetype == "residential")) +
      labs(title = "Number of residential permits issued")
      ggsave("./output/17residential-permits-issued.png", width = 10, height = 5.5)
      cat("Saving number of residential permits issued chart...\n")
}

sp_issue_days <- function() { #slide 18
  d <- filter(permits, division == "SP", !is.na(issuedate)) %>%
       group_by(my, usetype) %>%
       summarise(n = n(), mean = mean(daystoissue))

  #master
  p <- ggplot(d, aes(x = my, y = mean)) +
       labs(x = "Month", y = "Days") +
       theme(axis.text.x = element_text(angle = 45, hjust = .97))

  #residential
  p + geom_bar(data = filter(d, usetype == "residential"), stat = "identity", fill = "steelblue") +
      labs(title = "Average number of days to issue residential permits") +
      geom_hline(aes(yintercept = 8), colour = "orange", linetype = "dashed") +
      geom_hline(aes(yintercept = 17.9), colour = "tomato", linetype = "dashed")
      ggsave("./output/18days-to-issue-r.png", width = 10, height = 5.5)
      cat("Saving days to issue residential permits chart...\n")

  #commercial
  p + geom_bar(data = filter(d, usetype == "commercial"), stat = "identity", fill = "steelblue") +
      labs(title = "Average number of days to issue commercial permits") +
      geom_hline(aes(yintercept = 15), colour = "orange", linetype = "dashed") +
      geom_hline(aes(yintercept = 37.2), colour = "tomato", linetype = "dashed")
      ggsave("./output/18days-to-issue-c.png", width = 10, height = 5.5)
      cat("Saving days to issue commercial permits chart...\n")
}

sp_issue_days_dist <- function() { #slide 19
  permits$dayscat <- cut(permits$daystoissue,
                          c(0,15,30, Inf),
                          right = FALSE,
                          labels = c("< 15", "15 - 30", "> 30"))

  d <- filter(permits, division == "SP", !is.na(issuedate)) %>%
       group_by(my, usetype, dayscat) %>%
       summarise(n = n())

  #master
  p <- ggplot(d, aes(x = my, y = n, fill = dayscat)) +
      labs(x = "Month", y = "Number") +
      theme(axis.text.x = element_text(angle = 45, hjust = .97))

  #commercial
  p + geom_bar(data = filter(d, usetype == "commercial"), stat = "identity") +
      labs(title = "Distribution of days to issue commercial permits")
      ggsave("./output/19dist-days-to-issue-c.png", width = 10, height = 5.5)
      cat("Saving days to issue commercial permits distribution chart...\n")

  #residential
  p + geom_bar(data = filter(d, usetype == "residential"), stat = "identity") +
      labs(title = "Distribution of days to issue residential permits")
      ggsave("./output/19dist-days-to-issue-r.png", width = 10, height = 5.5)
      cat("Saving days to issue residential permits distribution chart...\n")
}

permits_one_day <- function() { #slide 20
  d <- filter(permits, division == "SP",
              opa_category == "Building - All Others" |
              opa_category == "Building - New Construction",
              daystoissue < 1) %>%
       group_by(my, online) %>%
       summarise(n = n())

  # d <- subset(d, d$my %in% levels(d$my)[(length(levels(d$my))-12):length(levels(d$my))])

  denom <- summarise(group_by(d, my), sum = sum(n))
  d <- left_join(d, denom, by = "my")
  d$prop <- d$n/d$sum

  ggplot(d, aes(x = my, y = prop, fill = online, label = percent(prop))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  labs(title = "Percent of building permits issued within one day of application", x = "Month", y = "Percent") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  geom_text(size = 4, colour = "grey33", vjust = -.5) +
  scale_fill_discrete(name = "Application method", labels = c("In person", "Online"))
  ggsave("./output/20-one-day-building-permits.png", width = 10, height = 5.5)
  cat("Saving percent of building permits issued within one day of application chart...\n")

}

revenue <- function() { #slide 21
  d <- group_by(rev, my, queue) %>%
       summarise(n = n(), meanwait = mean(timewaited), meanserve = mean(lengthofservice))

  #master
  p <- ggplot(d, aes(x = my, group = queue, colour = queue)) +
       theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
       labs( x = "Month", y = "Minutes" ) +
       scale_colour_discrete(name = "")

  #visitors
  p + geom_line(aes(y = n)) +
      labs(title = "Number of visitors", y = "Visitors")
      ggsave("./output/21rev-visitors.png", width = 10, height = 5.5)
      cat("Saving reveue bureau visitors line chart...\n")

  #mean wait
  p + geom_line(aes(y = meanwait)) +
      labs(title = "Average wait time")
      ggsave("./output/21rev-mean-wait.png", width = 10, height = 5.5)
      cat("Saving reveue bureau mean wait line chart...\n")

  #mean service
  p + geom_line(aes(y = meanserve)) +
      labs(title = "Average service time")
      ggsave("./output/21rev-mean-service.png", width = 10, height = 5.5)
      cat("Saving reveue bureau mean service line chart...\n")
}

lic_other <- function() { #slide 22
  d <- filter(lic, !is.na(issuedate)) %>%
       group_by(my, opa_category) %>%
       summarise(n = n(),
       meanissue = mean(daystoissue, na.rm = TRUE),
       sameday = sum(daystoissue == 0)) %>%
       melt()

  #master
  p <- ggplot(d, aes(x = my, y = value, fill = variable)) +
       theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
       labs(x = "Month")

  #business licenses
  p + geom_bar(data = filter(d, opa_category == "Business",
               variable == "n"),
               stat = "identity") +
      labs(title = "Business licenses issued", y = "Number") +
      guides(fill = FALSE)
      ggsave("./output/22bus-n-permits.png", width = 10, height = 5.5)
      cat("Saving number of business licenses issued chart...\n")

  p + geom_bar(data = filter(d, opa_category == "Business",
               variable == "meanissue"),
               stat = "identity") +
      labs(title = "Average number of days to issue", y = "Days") +
      guides(fill = FALSE)
      ggsave("./output/22bus-mean-days-permits.png", width = 10, height = 5.5)
      cat("Saving mean days to issue business license chart...\n")


  #electrical
  p + geom_bar(data = filter(d, opa_category == "Electrical",
               variable == "n" | variable == "sameday"),
               stat = "identity",
               position = "identity") +
      labs(title = "Number of same day electrical permits issued", y = "Number") +
      scale_fill_discrete(name = "", labels = c("All permits issued", "Issued same day"))
      ggsave("./output/22electrical-same-day.png", width = 10, height = 5.5)
      cat("Saving electrical permits issued same day chart...\n")

  #mechanical
  p + geom_bar(data = filter(d, opa_category == "Mechanical",
              variable == "n" | variable == "sameday"),
              stat = "identity",
              position = "identity") +
      labs(title = "Number of same day mechanical permits issued", y = "Number") +
      scale_fill_discrete(name = "", labels = c("All permits issued", "Issued same day"))
      ggsave("./output/22mechanical-same-day.png", width = 10, height = 5.5)
      cat("Saving mechanical permits issued same day chart...\n")
}

lic_cpnc <- function() { #slide 23
  d <- filter(lic, !is.na(issuedate), opa_category == "CPNC") %>%
       group_by(my, type) %>%
       summarise(n = n(), meanissue = mean(daystoissue))

  #master
  p <- ggplot(d, aes(x = my, fill = type)) +
       theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
       labs(x = "Month") +
       scale_fill_discrete(name = "")

  #number of permits
  p + geom_bar(aes(y = n), stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
      labs(title = "Number of CPNC permits issued", y = "Number") +
      geom_text(size = 4, colour = "grey33", vjust = -.5, aes(label = n, y = n))
      ggsave("./output/23cpnc-number-permits.png", width = 10, height = 5.5)
      cat("Saving CPNC number of permits issued chart...\n")

  #average days to issue
  p + geom_bar(aes(y = meanissue), stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
      labs(title = "Average number of days to issue CPNC permit", y = "Days") +
      geom_text(size = 4, colour = "grey33", vjust = -.5, aes(label = round(meanissue), y = meanissue))
      ggsave("./output/23cpnc-days-issue-permits.png", width = 10, height = 5.5)
      cat("Saving CPNC average days to issue chart...\n")
}

inspect_biz_charts <- function() { #slide 25
  d <- filter(inspect_biz, type == "Business License" | type == "Temporary Business License", days >= 0) %>%
       group_by(my) %>%
       summarise(n = n(), mean = mean(days))

  p <- ggplot(d, aes(x = my)) +
       labs(x = "Month") +
       theme(axis.text.x = element_text(angle = 45, hjust = .97))

  p + geom_bar(aes(y = mean), stat = "identity", fill = "springgreen4") +
      labs(y = "Days", title = "Average number of days to completing inspection request") +
      geom_text(size = 4, colour = "grey33", vjust = -.5, aes(label = round(mean), y = mean)) +
      geom_hline(aes(yintercept = 7), colour = "tomato", linetype = "dashed") +
      geom_hline(aes(yintercept = 1.1), colour = "steelblue", linetype = "dashed")
      ggsave("./output/25inspect-biz-mean-days.png", width = 10, height = 5.5)
      cat("Saving mean number of days to inspect business license chart...\n")

  p + geom_line(aes(y = n, group = 1), colour = "tomato") +
      labs(y = "Number", title = "Number of inspection requests")
      ggsave("./output/25inspect-biz-n.png", width = 10, height = 5.5)
      cat("Saving number of business license inspections chart...\n")
}

inspect_bldg_charts <-function() { #slide 26
  d <- group_by(inspect_bldg, my) %>%
       summarise(n = n(), sameday = sum(days == 0)) %>%
       melt()

  ggplot(d, aes(x = my, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  labs(title = "Number of building inspections done and those done in the same day", x = "Month", y = "Number") +
  scale_fill_discrete(name = "", labels = c("All inspections", "Same day inspections"))
  ggsave("./output/26inspect-bldg.png", width = 10, height = 5.5)
  cat("Saving number of building inspections chart...\n")
}

hdlc_permits_charts <- function() { # slide 28
  d <- group_by(hdlc_permits, my) %>%
       summarise(mean = mean(daystoissue, na.rm = TRUE), nperstaff = n()/3) %>%
       melt()

  ggplot(d, aes(x = my, y = value, group = variable, colour = variable)) +
  geom_line() +
  labs(title = "Days to review applications and number of applications", x = "Month", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = .97), legend.position = "top") +
  scale_colour_discrete(name = "", labels = c("Average number of days to review application", "Applications per staff"))
}

#load
load("./data/data-cleaned.Rdata")

#execute
oss_sp()
oss_sp_permits()
oss_etc()
oss_olp()
oss_cpnc()
permit_online()
bus_lic_online()
comm_res_permit()
sp_issue_days()
sp_issue_days_dist()
permits_one_day()
revenue()
lic_other()
lic_cpnc()
inspect_biz_charts()
inspect_bldg_charts()
hdlc_permits_charts()

#
#end init_plot
}
