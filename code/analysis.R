# Load in data
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)

data = read.csv("./data/BTC/btc_usdt_60min_all.csv")

# Exchange platform: OKEX
# TS_code: BTC_USDT
# Freq: on hour, 60 minutes
# Is_contract: Yes

data = data %>% select(-X, - exchange, - ts_code, -freq, -is_contract) %>%
  mutate(trade_time = ymd_hm(trade_time))

data$contract_type = do.call(rbind,
        str_split(data$symbol, "-", n = Inf, simplify = FALSE))[,3]

data = 
data %>% mutate(contract_type =  ifelse(contract_type != "SWAP",
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

current_week  = data %>% subset(contract_type %in% c("1 Current Week")) %>% 
  select(close, trade_time, symbol)

next_week     = data %>% subset(contract_type %in% c("2 Next Week")) %>% 
  select(close, trade_time, symbol)

current_season= data %>% subset(contract_type %in% c("3 Current Season")) %>% 
  select(close, trade_time, symbol)

next_season   = data %>% subset(contract_type %in% c("4 Next Season")) %>% 
  select(close, trade_time, symbol)

swap = data %>% subset(contract_type %in% c("SWAP")) %>% 
  select(close, trade_time, symbol) %>%
  subset(trade_time %in% current_week$trade_time)

current_week = current_week %>% subset(trade_time %in% swap$trade_time)

df1 = merge(current_week, swap, by = "trade_time") %>%
  rename("curweek" = "close.x") %>%
  rename("swap" = "close.y") %>%
  mutate(diff = curweek - swap) %>%
  mutate(return = (curweek - swap)/swap)

swap = data %>% subset(contract_type %in% c("SWAP")) %>% 
  select(close, trade_time, symbol) %>%
  subset(trade_time %in% next_week$trade_time)

next_week = next_week %>% subset(trade_time %in% swap$trade_time)

df2 = merge(next_week, swap, by = "trade_time") %>%
  rename("nextweek" = "close.x") %>%
  rename("swap" = "close.y") %>%
  mutate(diff = nextweek - swap) %>%
  mutate(return = (nextweek - swap)/swap)

swap = data %>% subset(contract_type %in% c("SWAP")) %>% 
  select(close, trade_time, symbol) %>%
  subset(trade_time %in% current_season$trade_time)

current_season = current_season %>% subset(trade_time %in% swap$trade_time)

df3 = merge(current_season, swap, by = "trade_time") %>%
  rename("curseason" = "close.x") %>%
  rename("swap" = "close.y") %>%
  mutate(diff = curseason - swap) %>%
  mutate(return = (curseason - swap)/swap)

swap = data %>% subset(contract_type %in% c("SWAP")) %>% 
  select(close, trade_time, symbol) %>%
  subset(trade_time %in% next_season$trade_time)

next_season = next_season %>% subset(trade_time %in% swap$trade_time)

df4 = merge(next_season, swap, by = "trade_time") %>%
  rename("nxtseason" = "close.x") %>%
  rename("swap" = "close.y") %>%
  mutate(diff = nxtseason - swap) %>%
  mutate(return = (nxtseason - swap)/swap)


df1 %>% ggplot() +
  geom_line(aes(x = trade_time, y = return)) + theme_bw() +
  geom_vline(xintercept = df1$trade_time[820], 
             linetype = "dotted",
             color = "red")+
  scale_x_datetime(breaks = date_breaks("2 month"),
                   minor_breaks = date_breaks("7 day"))

df2 %>% ggplot() +
  geom_line(aes(x = trade_time, y = return)) + theme_bw() +
  geom_vline(xintercept = df1$trade_time[820], 
             linetype = "dotted",
             color = "red")+
  scale_x_datetime(breaks = date_breaks("2 month"),
                   minor_breaks = date_breaks("14 day"))

df3 %>% ggplot() +
  geom_line(aes(x = trade_time, y = return)) + theme_bw() +
  geom_vline(xintercept = df1$trade_time[820], 
             linetype = "dotted",
             color = "red")+
  scale_x_datetime(breaks = date_breaks("2 month"),
                   minor_breaks = date_breaks("1 month"))

df4 %>% ggplot() +
  geom_line(aes(x = trade_time, y = return)) + theme_bw() +
  geom_vline(xintercept = df1$trade_time[c(820, 8019)], 
             linetype = "dotted",
             color = "red")+
  scale_x_datetime(breaks = date_breaks("2 month"),
                   minor_breaks = date_breaks("1 month"))
  
summary(df1$return[df1$trade_time<df1$trade_time[820]])
summary(df1$return[df1$trade_time>df1$trade_time[820] & 
                     df1$trade_time<df1$trade_time[8019]])
summary(df1$return[df1$trade_time>df1$trade_time[8019]])

summary(df2$return[df2$trade_time<df1$trade_time[820]])
summary(df2$return[df2$trade_time>df1$trade_time[820] & 
                     df2$trade_time<df1$trade_time[8019]])
summary(df2$return[df2$trade_time>df1$trade_time[8019]])








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