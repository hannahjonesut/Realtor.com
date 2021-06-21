library(dplyr)
library(tidyverse)
library(tidyr)
library(gamlr)
library(foreach)
library(ggplot2)
library(stringr)

usa_data<- read.csv('https://raw.githubusercontent.com/hannahjonesut/Realtor.com/main/RDC_Inventory_Country_History_Assessment.csv?token=ASRSTUEJMVJZYFZFQAVYF2TAZ76HS')
metro_data <- read.csv('https://raw.githubusercontent.com/hannahjonesut/Realtor.com/main/RDC_Inventory_Metro_History_Assessment.csv?token=ASRSTUHRS3O5BGP42ZDXU23AZ76JS')

#hh rank is based on HH count in zip code, with 1 being greatest aka most dense
#https://www.realtor.com/research/data/

metro_hh <- metro_data %>%
  mutate(rank_simple = ifelse(HouseholdRank>=1 & HouseholdRank<=5, 1, 
                              ifelse(HouseholdRank>5 & HouseholdRank<=10, 2, 
                                     ifelse(HouseholdRank>10 & HouseholdRank<=15, 3, 
                                            ifelse(HouseholdRank>15 & HouseholdRank<=20, 4, 
                                                   ifelse(HouseholdRank>20 & HouseholdRank<=25, 5,
                                                          ifelse(HouseholdRank>25 & HouseholdRank<=30, 6,
                                                                 ifelse(HouseholdRank>30 & HouseholdRank<=35, 7, 
                                                                        ifelse(HouseholdRank>35 & HouseholdRank<=40, 8, 
                                                                               ifelse(HouseholdRank>40 & HouseholdRank<=45, 9,
                                                                                      ifelse(HouseholdRank>45, 10, 0)))))))))))%>%
  group_by(rank_simple, month_date_yyyymm)%>%
  summarize( avg_listprice = mean(average_listing_price), new_list_count = mean(new_listing_count), days_on_mkt = mean(median_days_on_market))

ggplot(data = metro_hh)+
  geom_smooth(aes(x = month_date_yyyymm, y = avg_listprice, color = as.factor(rank_simple)))+
  labs(x="Date (YYYYMM)",  y = "Average List Price", legend = "Household Rank (1 = most households, 10 = least households)", title = "Average List Price by Household Rank (2016 - 2021)")

ggplot(data = metro_hh)+
  geom_smooth(aes(x = month_date_yyyymm, y = new_list_count, color = as.factor(rank_simple)))+
  labs(x="Date (YYYYMM)",  y = "Total Number of Listings", legend = "Household Rank (1 = most households, 10 = least households)", title = "Total Listings by Household Rank (2016 - 2021)")


usa_2020_2021 <- usa_data%>%
  filter(month_date_yyyymm >= 202001)%>%
  mutate(year = as.numeric(substr(month_date_yyyymm, 1, 4)), month = as.numeric(substr(month_date_yyyymm, 5, 6)), pct_inc = price_increased_count/total_listing_count)

#look at percent of price increased over total listings 
ggplot(data = usa_2020_2021)+
  geom_point(aes(x = median_days_on_market, y = pct_inc))+
  facet_grid(cols = vars(month), rows = vars(year))+
  labs(x="Median Days on Market by Month",  y = "% of Listings that Increased Price by Year" , title = "Days on Market vs % Price Increased, Monthly")
  
#price reduced freq as a fn of days on mkt-- when do people drop price?



usa_2020_2021 <- usa_data%>%
  filter(month_date_yyyymm >= 202001)%>%
  mutate(year = as.numeric(substr(month_date_yyyymm, 1, 4)), month = as.numeric(substr(month_date_yyyymm, 5, 6)))%>%
  mutate(quarter = ifelse(month>0 & month <=3, 1, 
                          ifelse(month>3 & month <=6, 2,
                                 ifelse(month>6 & month <=9, 3,
                                        ifelse(month>9 & month<=12, 4, 0))))) %>%
  group_by(year, quarter)%>%
  summarize(pct_inc =mean(price_increased_count/total_listing_count), med_days_on_mkt = median(median_days_on_market))

#look at percent of price increased over total listings 
ggplot(data = usa_2020_2021)+
  geom_point(aes(x = med_days_on_mkt, y = pct_inc))+
  facet_grid(cols = vars(quarter), rows = vars(year))+
  labs(x="Median Days on Market by Month",  y = "% of Listings that Increased Price by Year" , title = "Days on Market vs % Price Increased, Monthly")

