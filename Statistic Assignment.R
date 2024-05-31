# i'm directly pulling data from the FRED so i need to install a few new packages
install.packages ("fredr")
install.packages ("tidyverse")
library(dplyr)
library(fredr)
library(tidyverse)

#i used my own api key and you can request it from the FRED website 
## https://fredaccount.stlouisfed.org/apikeys
fredr_set_key ("467279e44c3e723a9fa904ea4abc907e")

#draw data from FRED using the code and then drawing using the series ID
test <- fredr_series_search_text("sp500")
sp500_raw <- fredr_series_observations(series_id = "SP500"); head(sp500_raw)

#tidy up the data by renaming and removing unnecessary columns
colnames(sp500_raw)[3] <- "prices"
sp500_raw1 <- subset(sp500_raw, select = -c(realtime_start,realtime_end,series_id) )

##tidying by also removing the NA values
sp500_raw2 <- sp500_raw1 %>% filter(!is.na(prices))
head(sp500_raw2)

### i am using the data from the 1 Dec 2019 until 31 Jan 2024
#### so i'm removing the uncessary rows 
sp500_raw3  <- sp500_raw2 %>% filter( date >= "2019-12-01")
sp500       <- sp500_raw3 %>% filter( date <= "2024-01-31")
head(sp500)

#plot the graph
ggplot (sp500, aes(x=date, y=prices))+
  geom_line (color="red")+
  labs (title="Time Series SP500 from Dec 2019 to Jan 2024",
        y="Closing Price of Index",
        x= "Date",
        caption = "FRED ID: SP500")

#differencing the graph with a lag of 1
sp500_diff1 <-  sp500$prices[(2):length(sp500$prices)]  - 
                sp500$prices[1:length(sp500$prices)-1]
##we actually don't need a data frame but i prefer to visualise it with a table
sp500_diff <- data.frame (
  time = c(1:length(sp500_diff1)),
  prices = sp500_diff1
)

#plotting the data frame for the difference
ggplot ( sp500_diff, aes(x = time, y = prices))+
  geom_line (color="black")+
  labs (title="Time Series SP500 from Dec 2019 to Jan 2024",
        y="difference of prices with lag,t = 1",
        x= "time",
        caption = "FRED ID: SP500")



