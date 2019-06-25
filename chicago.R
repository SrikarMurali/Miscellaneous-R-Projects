library(highcharter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
library(lubridate)
library(xts)
library(maps)
library(ggmap)
library(gridExtra)


cCrime12_16 <- read.csv('C:/Users/srika/Documents/R/RPrograms/Chicago_Crimes_2012_to_2017.csv')

cCrime12_16 <- cCrime12_16[cCrime12_16$Year != '2017',]

cCrime12_16$Day <- factor(day(as.POSIXlt(cCrime12_16$Date, format="%m/%d/%Y %I:%M:%S %p")))
cCrime12_16$Month <- factor(month(as.POSIXlt(cCrime12_16$Date, format = "%m/%d/%Y %I:%M:%S %p"), label = TRUE))
cCrime12_16$Year <- factor(year(as.POSIXlt(cCrime12_16$Date, format = "%m/%d/%Y %I:%M:%S %p")))
cCrime12_16$Weekday <- factor(wday(as.POSIXlt(cCrime12_16$Date, format = "%m/%d/%Y %I:%M:%S %p"), label = TRUE))

cCrime12_16$Date <- as.Date(cCrime12_16$Date, "%m/%d/%Y %I:%M:%S %p")


##creating timeseries

by_Date <- na.omit(cCrime12_16) %>%
  group_by(Date) %>%
  summarise(Total = n())
tseries <- xts(by_Date$Total, order.by = as.POSIXct(by_Date$Date))

##timeseries of arrest made
arrestByDate <- na.omit(cCrime12_16[cCrime12_16$Arrest == 'True',]) %>%
  group_by(Date) %>%
  summarise(Total = n())
arrest_tseries <- xts(arrestByDate$Total, order.by = as.POSIXct(by_Date$Date))

##location
byLoc <- cCrime12_16 %>%
  group_by(Location.Description) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

## primary type

byType <- cCrime12_16 %>%
  group_by(Primary.Type) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))


## District
byDis <- cCrime12_16 %>%
  group_by(District) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))



## fbi code
byCode <- cCrime12_16 %>%
  group_by(FBI.Code) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))


## arrest
byArr <- cCrime12_16 %>%
  group_by(Arrest) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))


## domestic
byDom <- cCrime12_16 %>%
  group_by(Domestic) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

## year
byYear <- cCrime12_16 %>%
  group_by(Year) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

## latitude and longitude of crime

LatLoncts <- as.data.frame(table(round(cCrime12_16$Longitude, 2), round(cCrime12_16$Latitude, 2)))
LatLoncts$Long <- as.numeric(as.character(LatLoncts$Var1))
LatLoncts$Lat <- as.numeric(as.character(LatLoncts$Var2))
LatLoncts2 <- subset(LatLoncts, Freq > 0)

## latitude and longitude of arrests
arrests.dat <- na.omit(cCrime12_16[cCrime12_16$Arrest == 'True',])
LatLongarrcts <- as.data.frame(table(round(arrests.dat$Longitude, 2), round(arrests.dat$Latitude, 2)))
LatLongarrcts$Long <- as.numeric(as.character(LatLongarrcts$Var1))
LatLongarrcts$Lat <- as.numeric(as.character(LatLongarrcts$Var2))
LatLongarrcts2 <- subset(LatLongarrcts, Freq > 0)


## hchart time

hchart(tseries, name = 'Crimes') %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = 'Time Series Plot of Arrests Made in Chicago (2012-2016)') %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department",
             style = list(fondSize = '12px'))

## distributiing crimes by month
hchart(arrest_tseries) %>%
  hc_add_theme(hc_theme_google())%>%
  hc_title(text = 'Time Series Plot Of Arrests made in Chicago (2012-2016)') %>%
  hc_credits(enabled = TRUE, text = "Taken from City of Chicago and Chicago Police Department", style = list(fontSize = '12px'))


arr_cts <- arrests.dat %>%
  group_by(Year, Month) %>%
  summarise(Total = n())

arresets <- ggplot(arr_cts, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = 'white') +
  scale_fill_viridis() +
  geom_text(aes(label = Total), color = 'white') +
  theme_bw() +
  ggtitle('Arrests by Year and Month (2012-2016)')
arresets
## arrests are lowest in december - most likely due to holidays
cCount <- cCrime12_16 %>%
  group_by(Year, Month) %>%
  summarise(Total = n())
crimes <- ggplot(cCount, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = 'white') +
  scale_fill_viridis() +
  geom_text(aes(label=Total), color = 'white') +
  theme_bw() +
  ggtitle('Crimes by Year and Month (2012-2016)')
crimes

##Crimes are lowest in february - maybe due to weather
##december and november usually have low crime rates - most likely due to thanksgiving and christmas

grid.arrange(crimes, arresets, ncol=2)

hchart(byYear, 'column', hcaes(x = Year, y = Total, color = Year)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c('#440154', '#21908C', '#FDE725'))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Number of Crimes by Year") %>%
  hc_credits(enabled = TRUE, text = "Taken from the City of Chicago and the Chicago Police Department", stle = list(fontSize = '12px'))  %>%
  hc_legend(enabled = FALSE)  

## crimes have decreased each year but arrests have not
streets <- cCrime12_16[cCrime12_16$Location.Description == 'STREET',]
##street timeseries
streetsByDate <- na.omit(streets) %>%
  group_by(Date) %>%
  summarise(Total = n())
streetsTSeries <- xts(streetsByDate$Total, order.by = as.POSIXct(by_Date$Date))
streetsTSeries

residence <- cCrime12_16[cCrime12_16$Location.Description == 'RESIDENCE',]
##street timeseries
residencesByDate <- na.omit(residence) %>%
  group_by(Date) %>%
  summarise(Total = n())
residenceTSeries <- xts(residencesByDate$Total, order.by = as.POSIXct(by_Date$Date))
residenceTSeries

apartment <- cCrime12_16[cCrime12_16$Location.Description == 'APARTMENT',]
##street timeseries
apartmentsByDate <- na.omit(apartment) %>%
  group_by(Date) %>%
  summarise(Total = n())
apartmentsTSeries <- xts(apartmentsByDate$Total, order.by = as.POSIXct(by_Date$Date))
apartmentsTSeries

sidewalk <- cCrime12_16[cCrime12_16$Location.Description == 'SIDEWALK',]
##street timeseries
sidewalkByDate <- na.omit(sidewalk) %>%
  group_by(Date) %>%
  summarise(Total = n())
sidewalkTSeries <- xts(sidewalkByDate$Total, order.by = as.POSIXct(by_Date$Date))
sidewalkTSeries

c<-hchart(streetsTSeries, name = 'Streets') %>%
  hc_add_series(residenceTSeries, name = 'Residence') %>%
  hc_add_series(apartmentsTSeries, name = 'Apartment') %>%
  hc_add_series(sidewalkTSeries, name = 'Sidewalk') %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = 'Taken from City of Chicago and the Chicago Police Department', style = list(fontSize = '12px')) %>%
  hc_title(text = 'Crimes in Streets/Residences/Apartments/Sidewalks') %>%
  hc_legend(enabled = TRUE)
c

##visible reduction in crime
g <- hchart(byType, 'column', hcaes(Primary.Type, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c('#440154', '#21908C','#FDE725'))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = 'Crime Types') %>%
  hc_credits(enabled = TRUE, text = 'Taken from City of Chicago and Chicago Police Department', style = list(fontSize = '12px')) %>%
  hc_legend(enabled = TRUE)
g


## Theft is most common crime

theft <- cCrime12_16[cCrime12_16$Primary.Type == 'THEFT',]
## theft timeseries
theftByDate <- na.omit(theft) %>%
  group_by(Date) %>%
  summarise(Total = n())
theftTSeries <- xts(theftByDate$Total, order.by = as.POSIXct(by_Date$Date))
theftTSeries

battery <- cCrime12_16[cCrime12_16$Primary.Type == 'BATTERY',]
## battery time series
batteryByDate <- na.omit(battery) %>%
  group_by(Date) %>%
  summarise(Total = n())
batteryTSeries <- xts(batteryByDate$Total, order.by = as.POSIXct(by_Date$Date))
batteryTSeries

criminalDamage <- cCrime12_16[cCrime12_16$Primary.Type == 'CRIMINAL DAMAGE',]
## criminal damage time series
criminalDamageByDate <- na.omit(criminalDamage) %>%
  group_by(Date) %>%
  summarise(Total = n())
criminalDamageTSeries <- xts(criminalDamageByDate$Total, order.by = as.POSIXct(by_Date$Date))
criminalDamageTSeries

narcotics <- cCrime12_16[cCrime12_16$Primary.Type == 'NARCOTICS',]
## narcotics time series
narcoticsByDate <- na.omit(narcotics) %>%
  group_by(Date) %>%
  summarise(Total = n())
narcoticsTSeries <- xts(narcoticsByDate$Total, order.by = as.POSIXct(by_Date$Date))
narcoticsTSeries

crimeChart <- hchart(theftTSeries, name = 'Thefts') %>%
  hc_add_series(batteryTSeries, name='Battery') %>%
  hc_add_series(criminalDamageTSeries, name = 'Criminal Damage') %>%
  hc_add_series(narcoticsTSeries, name = 'Narcotics') %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = 'Taken from City of Chicago Administration and the Chicago Police Department', style = list(fontSize = '12px')) %>%
  hc_title(text = 'Crimesin Thefts/Batterys/Criminal Damage/Narcotics') %>%
  hc_legend(enabled = TRUE)
crimeChart

typeChart <- hchart(byType, 'column', hcaes(Primary.Type, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c('#440154', '#21908C', '#FDE725'))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = 'Crime Types') %>%
  hc_credits(enabled = TRUE, text = 'Taken from City of Chicago and the Chicago Police', style = list(fontSize = '12px')) %>%
  hc_legend(enabled = TRUE)
typeChart  


## homicides

homicide <- cCrime12_16[cCrime12_16$Primary.Type == 'HOMICIDE',]

homicideYear <- homicide %>%
  group_by(Year) %>%
  summarise(Total = n())

homicideChart <- hchart(homicideYear, 'column', hcaes(Year, Total, color = Year)) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = 'Homicides in Chicago (2012-2016)') %>%
  hc_credits(enabled = TRUE, text = 'Taken from the City of Chicago and the Chicago Police Department', style = list(fontSize = '12px'))
homicideChart

homicideCt <- homicide %>%
  group_by(Year, Month) %>%
  summarise(Total = n())

homicideMonthChart <- ggplot(homicideCt, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = 'white') +
  scale_fill_viridis() +
  geom_text(aes(label = Total), color = 'white') +
  ggtitle("Homicides in Chicago by Month and Year (2012-2016")
homicideMonthChart
## lowest seem to be in February-April. Maybe because of the cold weather`` ?