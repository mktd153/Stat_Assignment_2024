# we're directly pulling data from the FRED so we need to install a few new packages first
install.packages ("fredr")
install.packages ("tidyverse")
library(dplyr)
library(tidyverse)
library(fredr)


# Question 1 & 2
## we used our own api key and you can request it from the FRED website
### https://fredaccount.stlouisfed.org/apikeys
fredr_set_key ("467279e44c3e723a9fa904ea4abc907e")

# draw data from FRED using the code and then drawing using the series ID
test <- fredr_series_search_text("sp500")
sp500_raw <- fredr_series_observations(series_id = "SP500"); head(sp500_raw)

# tidy up the data by renaming and removing unnecessary columns
colnames(sp500_raw)[3] <- "prices"
sp500_raw1 <- subset(sp500_raw, select = -c(realtime_start,realtime_end,series_id) )

## tidying by also removing the NA values
sp500_raw2 <- sp500_raw1 %>% filter(!is.na(prices))
head(sp500_raw2)

### we are using the data from the 1 Dec 2019 until 31 Jan 2024
#### so we are removing the uncessary rows
sp500_raw3  <- sp500_raw2 %>% filter( date >= "2019-12-01")
sp500       <- sp500_raw3 %>% filter( date <= "2024-01-31")
head(sp500)

#plot the graph
ggplot (sp500, aes(x=date, y=prices))+
  geom_line (color="red")+
  labs (title="Time Series SP500 from Dec 2019 to Jan 2024",
        y="Closing Price of Index",
        x= "Date",
        caption = "Fig. 1")


# Question 3
## differencing the log of the Prices with a backshift operator of 1
Rt <-  log10(sp500$prices[(2):length(sp500$prices)])  -
  log10(sp500$prices[1:length(sp500$prices)-1])

###we prefer to see the data in a table/frame
sp500_difflog <- data.frame (
  time = c(1:length(Rt)),
  prices = Rt
); head(sp500_difflog)

#plotting the data frame for the difference
ggplot ( sp500_difflog, aes(x = time, y = prices))+
  geom_line (color="black")+
  labs (title="Difference of Log. of Prices with a Backshift Operator = 1",
        y="difference of prices, delta p ",
        x= "time, t",
        caption = "Fig. 2")


# Question 4
## histogram with Prices
ggplot(data = sp500, aes(x = prices)) +
  geom_histogram(bins=200)+
  labs ( title = "Histogram of the Prices",
         x = " prices, p",
         caption = "Fig. 3")

## histogram with differences of log of prices
ggplot(data = sp500_difflog, aes(x = prices)) +
  geom_histogram(bins=200)+
  labs ( title = "Histogram of the Difference of Log of Prices",
         x = "diff. log prices, delta p",
         caption = "Fig. 4")


# Question 5
## Calculating the mean of the dataset sp500_difflog1
m <- mean (Rt)
v <- var ( Rt, na.rm = FALSE)

ecdf_func <- ecdf(Rt)

data_ecdf_cdf <- data.frame (
                x_1 = ecdf_func(Rt),
                y_1 = pnorm(Rt, mean = m, sd = sqrt(v))
                )

ggplot() +
  geom_point( data = data_ecdf_cdf, aes( x = x_1, y = y_1 ), color ="black", size = 0.5)+
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 0.5)+
  labs (title="CDF vs ECDF",
        y= "Cumulative Distribution Function, cdf",
        x= "Empirical Distribution Function, ecdf",
        caption = "Fig. 5")

# Question 6
## with mu = 0 confidence level 99%
t.test (Rt, mu= 0,
        alternative = c("two.sided"),
        conf.level = 0.99)

## with mu = mean(Rt), confidence level 95%
t.test (Rt, mu = mean(Rt) ,
        alternative = c("two.sided"),
        conf.level = 0.95)



