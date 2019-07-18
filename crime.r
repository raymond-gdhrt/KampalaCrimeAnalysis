#install.packages("knitr")
#install.packages("dplyr")
#install.packages("prophet")
#install.packages("xts")
#install.packages("highcharter")
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, fig.width = 8, fig.height = 7)

library(dplyr)
library(prophet)
library(xts)
library(highcharter)

kampalacrimes20122015 <- read.csv("C:/Users/Administrator.LISEN-20180705S/Desktop/FINAL PROJECT/kampala_Crimes_2012_to_2017.csv")

kampalacrimes20122017 <- kampalacrimes20122015[kampalacrimes20122015$Year %in% c('2012', '2013', '2014', '2015'), c('Date', 'ID')]


## Creating timeseries
kampalacrimes20122017$Date <- as.Date(kampalacrimes20122017$Date,"%m/%d/%y %H:%M")
head(kampalacrimes20122015$Date)
head(kampalacrimes20122017$Date)
Sys.Date()

by_Date <- na.omit(kampalacrimes20122017) %>% group_by(Date) %>% summarise(Total = n())
tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date))
head(kampalacrimes20122017$Date)
#hist(kampalacrimes20122015$Location.Description)
#v <- c("B","A","B","A","B","D","C","D","D","D","C")
barplot(table(kampalacrimes20122015$Location.Description))

pie(table(kampalacrimes20122015$Location.Description),main="Pie Chart of Countries")

df <- kampalacrimes20122017 %>% group_by(Date) %>% summarise(y = n()) %>% mutate(y = log(y))

names(df) <- c("ds", "y")
df$ds <- factor(df$ds)




### Times Series plot of kampala Crimes 2012-2016



hchart(tseries, name = "Crimes") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = "Sources: Uganda Bureau Of Statistics and the Uganda Police Force", style = list(fontSize = "12px")) %>%
  hc_title(text = "Times Series plot of Kampala District Crimes") %>%
  hc_legend(enabled = TRUE)




### Overview of Prophet(Facebook's Forecasting R Package)

#* Prophet is a procedure for forecasting time series data. 
#It is based on an additive model where non-linear trends are 
#fit with yearly and weekly seasonality, plus holidays. 
#It works best with daily periodicity data with at least one 
#year of historical data. Prophet is robust to missing data, 
#shifts in the trend, and large outliers.

#* Prophet is open source software released by Facebook's Core Data Science team.

### Fitting the model



m <- prophet(df)



### Creating dataframe with historical dates and future dates to forecast


future <- make_future_dataframe(m, periods = 365 * 7)

head(future)

tail(future)


### Forecasting by using predict method


forecast <- predict(m, future)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


### How to evaluate a forecasting model?

# Once a model has been selected and its parameters estimated, 
#the model is used to make forecasts. The performance of the model 
#can only be properly evaluated after the data for the forecast period
#have become available. A number of methods have been developed to help 
#in assessing the accuracy of forecasts.

### Basic plotting of the forecast

plot(m, forecast)



### Plotting forecast broken down into trend, weekly and yearly


prophet_plot_components(m, forecast)

if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(leaflet)) install.packages("leaflet")
df_category <- sort(table(kampalacrimes20122015$Region),decreasing = TRUE)
df_category <- data.frame(df_category[df_category > 0])
colnames(df_category) <- c("Category", "Frequency")
df_category$Percentage <- df_category$Frequency / sum(df_category$Frequency)
datatable(df_category, options = list(scrollX='400px'))


df_category <- sort(table(kampalacrimes20122015$Type.of.Crime),decreasing = TRUE)
df_category <- data.frame(df_category[df_category > 0])
colnames(df_category) <- c("Category", "Frequency")
df_category$Percentage <- df_category$Frequency / sum(df_category$Frequency)
datatable(df_category, options = list(scrollX='400px'))
### References 

#* https://facebookincubator.github.io/prophet/
#* https://facebookincubator.github.io/prophet/docs/quick_start.html#r-api
#* https://www.otexts.org/fpp/1/4
#* https://www.otexts.org/fpp/1/1}