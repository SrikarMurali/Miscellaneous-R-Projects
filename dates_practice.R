

###Dates and Times Practice
library(lubridate)
library(zoo)


dat_orig <- read.csv('C:/Users/srika/Documents/R/RPrograms/date_time_examples.csv', stringsAsFactors = FALSE)


#check
dat_orig

dat <- dat_orig

##Date Format
#as.Date() convertsdata to Date Format

dat$dates_only <- as.Date(dat$dates_only, format="%m/%d/%Y")

str(dat$dates_only)

#Need to specify format or else it will be in wrong format and will not return an Error in most cases


##POSIXct - used when you have times as well - cannot use as.Date with time information

dat_orig

#as.POSIXct can handle time data as well as dates

as.POSIXct(dat$date_times, format="%d-%m-%Y %H:%M:%S")


#Can change timezone

dat_copy <- dat_orig
as.POSIXct(dat_copy$date_times, format="%d-%m-%Y%H:%M:%S", tz='GMT')

#check out structure in detail
posixct_test <- as.POSIXct(dat_copy$date_times, format="%d-%m-%Y%H:%M:%S", tz='GMT')
posixct_test
str(posixct_test)
unclass(posixct_test)

# date is the number of days since 1970 posixCT is the date in seconds since the UNIX epoch

#Bring excel values as numbers - different versions of excel have different date formats

#POSIXlt - local time - stored as list
t <- unclass(as.POSIXlt(dat_copy$date_times, format="%d-%m-%Y%H:%M:%S"))
t$yday


#corrected
dat4 <- dat_orig
dat4$dates_only <- as.Date(dat4$dates_only, format='%m/%d/%Y')
dat4$date_times <- as.POSIXct(dat4$date_times, format="%d-%m-%Y%H:%M:%S", tz='GMT')


#lubridate - masks R's base date features

month(dat4$date_times)
year(dat4$date_times)


#zoo - also masks base date features

as.yearmon(dat4$dates_only)

#other libraries: chron, timeDate, xts
conflicts(, TRUE)#see which packages mask what

difftime(dat4[1,1], dat4[2, 1])
date_decimal(1998.762)
