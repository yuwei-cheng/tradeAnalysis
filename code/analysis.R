#--------------------------------Load libraries---------------------------------
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)
library(forecast)
library(tseries)
library(zoo)
#--------------------------------Functions--------------------------------------
# Split different types of contract
splitCon = function(x,name)
{
  x %>% subset(contract_type %in% c(name)) %>% 
    select(close, trade_time, symbol, vol)
}

# selectSWAP = function(x,y)
# {
#   x %>% subset(contract_type %in% c("SWAP")) %>% 
#     select(close, trade_time, symbol, vol) %>%
#     subset(trade_time %in% y$trade_time)
# }

makedf = function(x,y)
{
  common_time = intersect(x$trade_time, y$trade_time)
  x = x %>% subset(trade_time %in% common_time)
  y = y %>% subset(trade_time %in% common_time)
  
  # print(dim(x))
  # print(dim(y))
  
  df = merge(x,y,by = "trade_time") %>%
    # rename("curweek" = "close.x") %>%
    # rename("swap" = "close.y") %>%
    mutate(diff = close.x - close.y) %>%
    mutate(return = (close.x - close.y)/close.y)
  df = df[!duplicated(df),]
  return(df)
}

returnDist = function(x, t1, t2)
{
  print(signif(summary(x$return[x$trade_time<t1]), 3))
  print(signif(summary(x$return[x$trade_time>t1 & x$trade_time<t2]), 3))
  print(signif(summary(x$return[x$trade_time>t2]), 3))
}

returnPlot = function(x,t1,t2)
{
  par(mfrow = c(1,3), mar = c(3,3,1,1))
  hist(x$return[x$trade_time<t1], xlab = "", ylab = "return", main = NULL)
  hist(x$return[x$trade_time>t1 & x$trade_time<t2],
       xlab = "", ylab = "return",  main = NULL)
  hist(x$return[x$trade_time>t2],  xlab = "", ylab = "return",  main = NULL)
  par(mfrow = c(1,1))
}
#--------------------------------Load data--------------------------------------
data = read.csv("./data/BTC/btc_usdt_60min_all.csv")
dim(data)
dim(data[!duplicated(data),])
# Exchange platform: OKEX
# TS_code: BTC_USDT
# Freq: on hour, 60 minutes
# Is_contract: Yes

data = data %>% select(-X, - exchange, - ts_code, -freq, -is_contract) %>%
  mutate(trade_time = ymd_hm(trade_time))

data$contract_type = do.call(rbind,
        str_split(data$symbol, "-", n = Inf, simplify = FALSE))[,3]

data = data %>% 
  mutate(contract_type =  
           ifelse(contract_type != "SWAP",
                  contract_type, "180101")) %>% 
  mutate(type = ymd(contract_type)) %>%
  mutate(date = as.Date(trade_time)) %>%
  mutate(difference = type - date) %>%
  mutate(contract_type  = 
           ifelse(difference <0, "SWAP",
                  ifelse(difference >= 90, "4 Next Season",
                         ifelse(difference >=14, "3 Current Season",
                                ifelse(difference >= 7, "2 Next Week",
                                       "1 Current Week")))))%>%
  select(-difference, -date, -type)

current_week  = splitCon(data, "1 Current Week")
next_week = splitCon(data, "2 Next Week")
current_season = splitCon(data, "3 Current Season")
next_season = splitCon(data, "4 Next Season")
swap = splitCon(data, "SWAP")

ymd_hms(intersect(swap$trade_time, current_week$trade_time))

intersect = function(x, y)
{
  x[x%in%y]
}

intersect(swap$trade_time, current_week$trade_time)

swap1 = swap %>% subset()

current_week = current_week %>% subset(trade_time %in% swap$trade_time)
df1 = makedf(current_week, swap)

swap = selectSWAP(data, next_week)
next_week = next_week %>% subset(trade_time %in% swap$trade_time)
df2 = makedf(next_week, swap)

swap = selectSWAP(data, current_season)
current_season = current_season %>% subset(trade_time %in% swap$trade_time)
df3 = makedf(current_season, swap)

swap = selectSWAP(data, next_season)
next_season = next_season %>% subset(trade_time %in% swap$trade_time)
df4 = makedf(next_season, swap)

df1 %>% ggplot() +
  geom_line(aes(x = trade_time, y = vol.y)) + theme_bw() +
  geom_vline(xintercept = df1$trade_time[c(820, 8019)],
             linetype = "dotted",
             color = "red")+
  scale_x_datetime(breaks = date_breaks("2 month"),
                   minor_breaks = date_breaks("7 day"))+
  labs(x = NULL, title = "当周合约和永续合约")

vol = data %>% subset(contract_type == "SWAP") 
# %>%
#   ggplot() + geom_line(aes(x = trade_time, y = vol)) +
#   theme_bw()

adf.test(vol$vol, alternative = c("stationary"))

x = diff(vol$vol)
plot(pacf(vol$vol))
auto.arima()

data$date = as.Date(data$trade_time)

vol %>%
  group_by(date) %>%
  summarize(sum = sum(vol)) %>% 
  ggplot() + geom_line(aes(x = date, y = sum)) + theme_bw() +
  # scale_y_continuous(limits = c(5,8))+
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^5, 10^8)
  ) +
  geom_vline(xintercept = vol$date[weekdays(vol$date) == "星期五"],
             color = "red", linetype = "dotted") +
  scale_x_date(date_breaks = "1 month", date_label = "%b-%d")

vol$weekdays = weekdays(vol$date)

vol %>% subset(weekdays == "星期五") %>%
  ggplot() + geom_line(aes(x = trade_time, y = vol))




# plot(rollmean(vol$vol, k = 21, align = "center"),
#      type = "l")




# Price
data %>%
  ggplot() +
  geom_line(aes(x = trade_time, y=close, colour = contract_type)) +
  theme_bw() + labs(x = "Trade Time", y = "BTC Close Price",
                    colour = "Type")+
  scale_x_datetime(date_labels = "%b-%d", breaks = "2 month") +
  scale_y_continuous(breaks = seq(10000, 80000, 10000)) +
  # scale_color_viridis(discrete = TRUE, option = "D")
  scale_color_manual(values=c("#999999", "#E69F00", 
                              "#56B4E9", "#d62828", "#003566"))
# Volume
data %>%
  ggplot() + facet_wrap(~ contract_type) +
  geom_line(aes(x = trade_time, y = log(vol)),color = "steelblue") +
  theme_bw() + labs(x = "Trade Time", y = "BTC vol in log scale",
                    colour = "Type")+
  scale_x_datetime(date_labels = "%b-%d", breaks = "2 month")

# Identify days which has extremely low volume
data %>% subset(contract_type == "SWAP") %>% 
  subset(vol == min(vol))

data %>% subset(contract_type == "1 Current Week") %>% 
  subset(vol == min(vol))

data %>% subset(contract_type == "2 Next Week") %>% 
  subset(vol == min(vol))

data %>% subset(contract_type == "3 Current Season") %>% 
  subset(vol == min(vol))

data %>% subset(contract_type == "4 Next Season") %>% 
  subset(vol == min(vol))

# Spread analysis