Reproducible Research: Assignment 1
========================================================

Total Daily Activity
=====================
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
file$date <- as.Date(file$date);
```

```
## Error: object of type 'closure' is not subsettable
```

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
max_total <- max(daily_total_steps, na.rm = TRUE);
```

```
## Error: object 'daily_total_steps' not found
```

```r
sd_total <- sd(daily_total_steps, na.rm=TRUE);
```

```
## Error: object 'daily_total_steps' not found
```

```r
total_steps_raw <- sum(daily_total_steps, na.rm = TRUE)
```

```
## Error: object 'daily_total_steps' not found
```






















