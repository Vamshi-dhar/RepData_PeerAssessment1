README
====================================
## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Assignment

This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a **single R
markdown** document that can be processed by **knitr** and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you
used to generate the output you present. When writing code chunks in
the R markdown document, always use `echo = TRUE` so that someone else
will be able to read the code. **This assignment will be evaluated via
peer assessment so it is essential that your peer evaluators be able
to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any
plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this
assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You
will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of
the URL to your GitHub repository and the SHA-1 commit ID for your
repository state.

NOTE: The GitHub repository also contains the dataset for the
assignment so you do not have to download the data separately.

#### Loading required pacakges:

loading packages that are helpfull in our analysis.

``` r
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
```

Loading and preprocessing the data:
-----------------------------------

Lets start our analysis by downlaoding, uploading and discovering the data.

``` r
link <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(link, destfile = "data.zip")
unzip("data.zip")
amd <- fread("activity.csv", header = TRUE ,na.strings = "NA")
amd <- tbl_df(amd)
summary(amd)
```

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

What is mean total number of steps per day?
-------------------------------------------

#### Caliculating total no of steps per each day:

``` r
tot_stepsinday <- aggregate(steps ~ date, data = amd, sum, na.rm = TRUE)
```

#### Ploting histogram:

``` r
ggplot(data = tot_stepsinday, aes(x = tot_stepsinday$steps)) +
        geom_histogram(aes(fill = ..count..), bins = 15) +
        scale_fill_gradient("Count", low = "yellow", high = "blue") +
        xlab( "Total no of steps per day") +
        ggtitle("Histogram of total number of steps taken per day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

#### Caliculating Mean and Median:

``` r
Mean <- round(mean(tot_stepsinday$steps))
Median <- median(tot_stepsinday$steps)
```

**Observation :** Mean and Median of the number of steps taken per day was caliculated in the above code, they are found to be Mean = 10766 and Median = 10765

What is the average daily activity pattern?
-------------------------------------------

``` r
avg_pattrn <- aggregate(steps ~ interval, data = amd, mean, na.rm = TRUE)
d <- which(avg_pattrn$steps == max(avg_pattrn$steps))
max <- avg_pattrn[d,1]
ggplot(avg_pattrn, aes(x = interval, y = steps,label = 
                               paste(avg_pattrn[d,1], "th interval",sep = ""))) +
        geom_line() +
        geom_text(data = avg_pattrn[d,],vjust=0, colour="red")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

**Observation :**interval that contains the maximum number of steps is 835

Imputing missing values:
------------------------

``` r
miss <- sum(!complete.cases(amd))
```

Total number of missing values in the dataset are 2304

Imputation of missing values:
-----------------------------

#### Filling missing values with its *mean*

-   Strategy used to impute missing values here is simple by *mean*

``` r
imputedamd <- amd
imputedamd$steps[imputedamd$steps %in% NA] <- mean(imputedamd$steps, na.rm = TRUE)
colSums(is.na(imputedamd))
```

    ##    steps     date interval 
    ##        0        0        0

``` r
tot_stepsinday_imputed <- aggregate(steps ~ date, data = amd, sum, na.rm = TRUE)
ggplot(data = tot_stepsinday_imputed, aes(x = steps)) +
        geom_histogram(aes(fill = ..count..), bins = 15) +
        scale_fill_gradient("Count", low = "yellow", high = "blue")+
        xlab( "Total no of steps per day") +
        ggtitle("Histogram of total number of steps taken per day in imputed dataset")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-9-1.png)

**Observation :** There is no significant change in histograms before and after imputing missing valyes:

``` r
imputed_Mean <- round(mean(tot_stepsinday_imputed$steps))
imputed_Median <- median(tot_stepsinday_imputed$steps)
```

| Parameter    | Mean  | Median |
|--------------|-------|--------|
| actual data  | 10766 | 10765  |
| imputed data | 10766 | 10765  |

**Observation :** As observed above, Mean and Median values does not change even after imputation of missing values.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

#### using imputed dataset here:

``` r
imputedamd$date <- as.Date(strptime(imputedamd$date, format="%Y-%m-%d"))
imputedamd$day <- weekdays(imputedamd$date)
for (i in 1:nrow(imputedamd)) {
        if (imputedamd[i,]$day %in% c("Saturday","Sunday")) {
                imputedamd[i,]$day<-"weekend"
        }
        else{
                imputedamd[i,]$day<-"weekday"
        }
}
stepsperday <- aggregate(imputedamd$steps ~ imputedamd$interval + imputedamd$day, imputedamd, mean)
```

#### Ploting weekend vs weekday to compare weekend and weekday pattern

``` r
names(stepsperday ) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsperday , type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-12-1.png)

\*\*\*\*\*\*\*\*\*End of the project\*\*\*\*\*\*\*\*\*\*\*\*
============================================================
