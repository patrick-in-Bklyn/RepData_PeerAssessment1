Reproducible Research: Assignment 1
========================================================

This personal monitoring device measured the number steps an individual takes at five minute intervals. The sample data was collected over a two month period between October and November of 2012. 


```r
if (!exists("activity.csv")){
files <- unzip("./RepData_PeerAssessment1/activity.zip", exdir = ".");
}
```

```
## Warning: error 1 in extracting from zip file
```

```r
file <- read.csv("activity.csv", header = TRUE, nrows = -1, );
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

I subset the data to generate plots of the total daily activity and activity by time period. First, I created vectors of dates and then the intervals. and I used these to calculate the daily total setps in the  


```r
dates <- unique(file$date);
```

```
## Error: object of type 'closure' is not subsettable
```

```r
dates <- as.Date(dates);
```

```
## Error: object 'dates' not found
```

```r
days <- unique(format(dates, "%a"));
```

```
## Error: object 'dates' not found
```

```r
intervals <- unique(file$interval);
```

```
## Error: object of type 'closure' is not subsettable
```

```r
daily_total_steps <- sapply(dates, function(x) sum(file$steps[file$date == x], na.rm =TRUE));
```

```
## Error: object 'dates' not found
```

```r
mean_total <- mean(daily_total_steps, na.rm = TRUE);
```

```
## Error: object 'daily_total_steps' not found
```

```r
median_total <- median(daily_total_steps, na.rm = TRUE);
```

```
## Error: object 'daily_total_steps' not found
```

```r
print(mean_total); 
```

```
## Error: object 'mean_total' not found
```

```r
print(median_total);
```

```
## Error: object 'median_total' not found
```
When we filter the data for each day and then count the average steps by 5 minute interval across the two month period, we see that, on average, the subject walked ```{r print(mean_total)}''' The median steps taken were ```{r print(median_total)}``` on daily basis. We see this in the histogram of the total steps data below. 


```r
hist(daily_total_steps, col = "red", breaks = 10, main = "", xlab = "", ylab = "");
```

```
## Error: object 'daily_total_steps' not found
```

```r
title(main = "Histogram: Total Steps Taken", ylab = "Frequency", xlab = "Total Steps Taken")
```

```
## Error: plot.new has not been called yet
```

