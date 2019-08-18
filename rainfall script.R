# This script solves the following contents
# first prepare the daily dataset into monthly and yearly using sum function
# Draw line and quantile plot
# Draw trend series of annual data set
# Draw mean monthly precipitation using bar plot
# Draw sum of monthly precipitation using box plot
# Trend and significance value of monthly rainfall
# Add libararies function which can be used here

library("readxl")
library(ggplot2)
library(dplyr)
library(lubridate)
library(hydroTSM)
library(data.table)
library(tidyverse)
library(astsa)
library(Kendall)
my_data <- read_excel("C:/Users/snasreen/Downloads/skill_eval.xlsx")

Mon_date = strftime(my_data$DATE, "%Y-%m")
#convert daily data into monthly 
Mon_Rainfall <- my_data$Rainfall
aggr_dailytomon = aggregate( Mon_Rainfall ~ Mon_date, FUN = sum)
#convert daily data into annual
Year_date = strftime(my_data$DATE, "%Y")
Year_Rainfall <- my_data$Rainfall
aggr_dailytoannual = aggregate( Year_Rainfall ~ Year_date, FUN = sum)

x <- aggr_dailytomon$Mon_date
y <- aggr_dailytomon$Mon_Rainfall
x1 <- aggr_dailytoannual$Year_date
y1 <- aggr_dailytoannual$Year_Rainfall

# line plot and quantile plot of yearly data set x1, y1
#png('precip_annual.png',width=480,height=480)
plot(x1,y1, type="b", col="blue", lwd=2, pch=5, xlab="time(years)", ylab="Rainfall")
#dev.off()
#png('precip_qq-annual.png',width=480,height=480)
qqnorm(y1, pch = 1, frame = FALSE)
qqline(y1, col = "steelblue", lwd = 2)
#dev.off()

# Find the significance of trend of man kandel method 
library(boot)
library(tseries)
data_yearly <- data.frame(years=x1,Rainfall_years=y1)
annual_trend_Series<- ts (data_yearly$Rainfall_years, frequency = 1, start = 1950) # freq 12 => Monthly data. 
str(data_yearly)
adf.test(annual_trend_Series)
kpss.test(annual_trend_Series)
plot.ts(annual_trend_Series, col="blue")
adf.test(annual_trend_Series)
kpss.test(annual_trend_Series)
plot(annual_trend_Series, col="darkgrey")
lines(lowess(time(annual_trend_Series), annual_trend_Series), col="blue", lwd=2)
par(mfrow=c(2,1))
acf(annual_trend_Series)
pacf(annual_trend_Series)
stlRes <- stl(annual_trend_Series, s.window = "periodic")
res <- MannKendall(annual_trend_Series)
print(res)
summary(res)
MKtau <- function(z) MannKendall(z)$tau
tsboot(annual_trend_Series, MKtau, R=500, l=5, sim="fixed")
boot.out <- tsboot(annual_trend_Series, MKtau, R=500, l=5, sim="fixed")
boot.ci(boot.out, type="perc")



# Convert daily data into monthly data set using sum function and box plot

my_data_new <- mutate(my_data, mo = month(DATE), yr = year(DATE)) %>%
  filter(DATE >= "1950-01-01") %>%
  group_by(yr, mo) %>% 
  summarise(prs = sum(Rainfall, na.rm = TRUE))
#mon <- factor(my_data$mo, levels = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec"))
ggplot(my_data_new) + 
  geom_boxplot(aes(x = mo, y = prs, group = mo))+
  labs(title = "12 Month Precipitation - Czech Repblic (1950-2019)",y = "cummulative rainfall",x = "Date(Months)") + 
  theme_bw(base_size = 15)


# Monthly mean precipitation using bar plot
aggr_dailytomon$Mon_date(substring(DateTimeParse(month.name, "%b"),5,2))

my_data <- setDT(my_data)[, .(mn_value = mean(Rainfall)), by = .(yr = year(DATE),
    mon = months(DATE))] 
ggplot(data = my_data, aes(x = mon, y = mn_value)) +
  geom_bar(stat="identity",color="purple") +
  labs(title = "Total daily precipitation in Czech Republic",
       #subtitle = "Fall 2013",
       x = "Date(Months)", y = "Monthly mean Precipitation")


# Trend and significance of monthly rainfall data set
library(boot)
library(tseries)
data_monthly <- data.frame(months=my_data$mo,Rainfall_months=my_data$prs)
Monthtrend_Series<- ts (data_monthly$Rainfall_months, frequency = 12, start = 1950) # freq 12 => Monthly data. 
str(data_monthly)
adf.test(Monthtrend_Series)
kpss.test(Monthtrend_Series)
plot(Monthtrend_Series, col="darkgrey")
lines(lowess(time(Monthtrend_Series), Monthtrend_Series), col="blue", lwd=2)
par(mfrow=c(2,1))
acf(Monthtrend_Series)
pacf(Monthtrend_Series)
stlRes <- stl(Monthtrend_Series, s.window = "periodic")
res <- MannKendall(Monthtrend_Series)
print(res)
summary(res)
MKtau <- function(z) MannKendall(z)$tau
tsboot(Monthtrend_Series, MKtau, R=500, l=5, sim="fixed")
boot.out <- tsboot(Monthtrend_Series, MKtau, R=500, l=5, sim="fixed")
boot.ci(boot.out, type="perc")


