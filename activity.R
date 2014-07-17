# Reproducible Research peer assignment 1


### open the file and look at it
library(timeDate)
if (!exists("activity.csv")){
files <- unzip("./RepData_PeerAssessment1/activity.zip", exdir = ".");
}

file <- read.csv("activity.csv", header = TRUE, nrows = -1, );
file$date <- as.Date(file$date);

dates <- unique(file$date);
dates <- as.Date(dates);
days <- unique(format(dates, "%a"));
intervals <- unique(file$interval);

daily_total_steps <- sapply(dates, function(x) sum(file$steps[file$date == x], na.rm =TRUE));
mean_total <- mean(daily_total_steps, na.rm = TRUE);
median_total <- median(daily_total_steps, na.rm = TRUE);
print(mean_total); print(median_total);

mean_steps_by_interval <- sapply(intervals, function(x) mean(file$steps[file$interval ==x], na.rm = TRUE))

weekday_steps <- sapply(dates, function(x) if(isWeekday(x)){sum(file$steps[file$date == x], na.rm =TRUE)});

hist(daily_total_steps, col = "red", breaks = 10, main = "", xlab = "", ylab = "");
title(main = "Histogram: Total Steps Taken", ylab = "Frequency", xlab = "Total Steps Taken")


plot(mean_steps_by_interval ~ intervals, type = "l", main = "", xlab = "", ylab = "");
title(main = "Average No. of Steps Taken by Interval over Two Month Period");
title(xlab = "5 min Interval", ylab = "Average Steps Taken");
axis(2, cex.axis = 0.6);

# the interval that produces the largest number of steps

max <- max(mean_steps_by_interval, na.rm = TRUE);
max_index <- which(mean_steps_by_interval == max);
print(c("the average maximum number of steps taken in a five minute interval is ", round(max, 0))); 
print(c("The max activity occurs at interval ", intervals[max_index]));

na <- which(is.na(file$steps));
l_na <- length(na);
print(c("Total missing periods were", days, " out of a total sample size of ", nrow(file), ". 
        This amounts to ", nrow(file)/288 " missing days of data."));

for (i in 1:length(days))
        {
        if(i == 1)
                {
                new_data <- as.data.frame(new_set(days[i]));
                colnames(new_data) <- colnames(file);
                }
        else
                {
                new_data <- rbind(new_data, new_set(days[i]));
                colnames(new_data) <- colnames(file);
                }
        }

new_daily_total_steps <- sapply(dates, function(x) sum(new_data$steps[new_data$date == x], na.rm =TRUE));
new_mean_steps_by_interval <- sapply(intervals, function(x) mean(new_data$steps[new_data$interval ==x], na.rm = TRUE))                   

hist(new_daily_total_steps);
plot(new_mean_steps_by_interval ~ intervals, type = "l", main = "", xlab = "", ylab = "");
title(main = "New Average No. of Steps Taken by Interval over Two Month Period");
title(xlab = "5 min Interval", ylab = "Average Steps Taken");
axis(2, cex.axis = 0.6);

new_data$date <- as.Date(new_data$date, origin = "1970-01-01")
weekday_set <- subset(new_data, isWeekday(date), select = colnames(file));
weekend_set <- subset(new_data, isWeekend(date), select = colnames(file));
add_data <- isWeekday(new_data$date)
new_data <- as.data.frame(cbind(new_data, add_data));
colnames(new_data) <- c("steps", "date", "interval", "weekday");


weekday_profile <- sapply(intervals, function(x) mean(weekday_set$steps[weekday_set$interval ==x], na.rm = TRUE));
weekend_profile <- sapply(intervals, function(x) mean(weekend_set$steps[weekend_set$interval ==x], na.rm = TRUE));

week_profile <- as.data.frame(cbind(intervals, weekend_profile, weekday_profile));
colnames(week_profile) <- c("interval", "weekend", "weekdays")


new_set <- function(day)
        {
        day_set <- subset(file, format(date, "%a") == day, select = colnames(file));
        interval <- unique(day_set$interval);
        dates <- unique(day_set$date)
        day_means <- sapply(interval, function(x) mean(day_set$steps[day_set$interval == x], na.rm =TRUE));

        new_vals <- as.data.frame(cbind(day_means, dates, interval), colnames = colnames(day_set));

        replace <- which(is.na(day_set$steps));

        interval <- day_set$interval[replace];
        date <- as.Date(day_set$date[replace], origen = "1970-01-01");
        
        index_new_vals <- sapply(replace, function(x) which(new_vals$interval == day_set$interval[x]))

        steps <- sapply(index_new_vals, function(x) day_means[x]);

        replacements <- as.data.frame(cbind(steps, date, interval));
        colnames(replacements) <- colnames(day_set);

        

        count = 1;
        if(count %in% replace)
                        { 
                        new_set <- as.data.frame(cbind(replacements[count,1],replacements[count,2],replacements[count,3])) ;
                        colnames(new_set) <- colnames(day_set);
                        count = count+1;
                        } 
                        else
                        {
                        new_set <- as.data.frame(cbind(day_set[1,1],day_set[1,2],day_set[1,3]));
                        colnames(new_set) <- colnames(day_set);
                        }
        for(i in as.integer(2:nrow(day_set)))
               {
                if(i %in% replace)
                       {
                        new_set <- rbind(new_set,replacements[count,]);
                        count = count+1;
                        }
                
               else
                        
                        new_set <- rbind(new_set, day_set[i,]);
                        
                }

        
        



        return(new_set);
}