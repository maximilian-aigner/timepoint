set.seed(1)
library(tidyverse)
library(lubridate)

activity <- read.csv("~/Downloads/prediction_log/train_log.csv")
activity$time <- lubridate::ymd_hms(activity$time)

n_days <- activity %>% group_by(username) %>%
  summarise(count_days = n_distinct(floor_date(time, "1 day")),
            count_courses = n_distinct(course_id))

interesting_users <-
  c(1071211,30976,252158,190691,613,60462,27878,591786,1133680,222220)
activity_small <- subset(activity, username %in% interesting_users)

# consolidate sessions
activity_small <- subset(activity_small, !duplicated(session_id))
activity_small <- activity_small[!(colnames(activity_small) %in% c("object","action"))]

activity_small$course_id <- as.numeric(factor(activity_small$course_id))
activity_small$session_id <- as.numeric(factor(activity_small$session_id))
activity_small$enroll_id <- as.numeric(factor(activity_small$enroll_id))
activity_small$username <- as.numeric(factor(activity_small$username))

colors <- viridis::viridis(48)
pdf("mooc.pdf")
plot(username ~ time, data = activity_small, col = colors, pch = 18,
     axes=F,bty='l',
     ylab = "", xlab = "")
axis(2, at=1:10, las=2,line = NA, lwd = 0,lwd.ticks=0)
abline(h=1:10, lty = 'dashed', col = 'lightgray', lwd = 0.3)
axis(1, at = pretty_dates(activity_small$time, n = 2),
     labels = format(pretty_dates(activity_small$time, n = 2), "%Y-%m"),
     lwd = 0, lwd.ticks = 1, line = .7)
dev.off()
#axis.Date(1, x = activity_small$time)#, at = c(as.Date("2015-01-01"), as.Date("2016-01-1"), as.Date("2017-01-01")),
          #labels = TRUE)
