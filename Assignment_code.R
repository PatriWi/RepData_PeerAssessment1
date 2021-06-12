library(dplyr)
library(ggplot2)

activity <- read.csv("activity.csv")
activity$date <- as.POSIXct(activity$date, format = "%Y- %m -%d")
activity$steps <- as.double(activity$steps)

task1 <- activity %>%
    group_by(date) %>%
    summarise(steps = sum(steps, na.rm = TRUE))

png(filename="histogram1.png", width=480,height=480)
hist(
    task1$steps,
    xlab = "Number of steps taken per day",
    xlim = c(0, max(task1$steps)),
    breaks = nrow(task1),
    main = "Distribution of number of steps taken per day",
    col = "turquoise3"
)
box()
dev.off ()



mean_task1 <- mean(task1$steps)
mean_task1

median_task1 <- median(task1$steps)
median_task1




task2 <- activity %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = TRUE))

png(filename="timeseries1.png", width=480,height=480)
ggplot(data = task2, aes(x = interval, y = steps)) +
    geom_line(color = 6, size = 0.5) +
    xlab("5-minute interval") +
    ylab("average number of steps taken")+
    labs(title = "time series plot of averaged steps per 5-min interval",
         color = "types")
dev.off ()


interval_max_steps <- which.max(task2$interval)
interval_max_steps


na_rows <- nrow(activity %>% filter(is.na(steps)))
na_rows

activity_noan <-
    left_join(activity,
              task2 %>% rename(steps_mean = steps),
              by = c("interval" = "interval")) %>%
    mutate(steps = case_when(is.na(steps) ~ steps_mean, TRUE ~ steps)) %>% 
    select(-steps_mean)


task1_noan <- activity_noan %>%
    group_by(date) %>%
    summarise(steps = sum(steps, na.rm = TRUE))

png(filename="histogram2.png", width=480,height=480)
hist(
    task1_noan$steps,
    xlab = "Number of steps taken per day/no NAs",
    xlim = c(0, max(task1_noan$steps)),
    breaks = nrow(task1_noan),
    main = "Distribution of number of steps taken per day",
    col = "turquoise3"
)
box()
dev.off ()

mean_task1_noan <- mean(task1_noan$steps)
mean_task1_noan
mean_task1 

median_task1_noan <- median(task1_noan$steps)
median_task1_noan
median_task1

#Before computing the missing values they were counted
#as being 0 due to na.rm = TRUE. But that means
#that they were counted as the person did not take 
# any steps at all. After replacing the NAs with the
#mean of the 5 minute interval the distribution changed. There is
#a lower frequency of zeros and the median and the mean changed. They are 
#higher bc the zeros due to the nas are ignored



task4 <- activity_noan %>%
    mutate(dtype = as.factor(case_when(
        weekdays(date) %in% c("Samstag", "Sonntag") ~ "weekend",
        TRUE ~ "weekday"
    )))  
    

task4_plot <- task4 %>%
    group_by(dtype, interval) %>%
    summarise_at(vars(steps), funs(mean(.,na.rm =TRUE)))
    
png(filename="timeseries2.png", width=480,height=480)
ggplot(data = task4_plot, aes(x = interval, y = steps, color = dtype)) +
    geom_line(size = 0.5) +
    facet_wrap(~dtype ,ncol = 1, nrow = 2)+
    xlab("5-minute interval") +
    ylab("average number of steps taken")+
    labs(title = "time series plots of averaged steps per 5-min interval",
         color = "types")
dev.off ()
