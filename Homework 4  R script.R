
library(lubridate)
library (tidyverse)
library(ggplot2)
library(mapproj)
library(maps)

#1.How many matches did Liverpool win from 2001 (inclusive) to 2010 (inclusive)?
epl <- read.csv(file.choose())
epl$Match_Date=mdy(epl$Match_Date)

epl1 <- epl %>% 
  filter( HomeTeam == "Liverpool" & FTR == "H"|
            AwayTeam == "Liverpool" & FTR == "A",
          Match_Date<= "2010-12-31" & Match_Date >= "2001-01-01")

nrow(epl1)

#From 2001 to 2010, Liverpool won 200 matches

#2.Find out the top 5 years in which the highest number of matches were played?

epl2 <- epl %>% 
  mutate( Year = year(Match_Date)) 

epl2%>% 
  group_by(Year) %>%
  summarise(Year = unique(Year), count = n_distinct(X)) %>% 
  arrange(desc(count))
#1994, 1995, 2017, 2006 & 2004 has the highest number of matches played

#3.What is the number of games played in each month in the year 2015.
epl3 <- epl2 %>% 
  mutate(Month = month(Match_Date, label = T, abbr = T))
epl3 %>% 
  filter (Year == "2005") %>% 
  group_by(Month) %>% 
  summarize( Month = unique(Month), count_match = n_distinct(X))

#4.Which month in 2017 had the highest number of matches played in a day?

epl3 %>% 
  filter(Year =="2017") %>% 
  group_by(Match_Date) %>% 
  summarize(Match_Date =unique(Match_Date), count_match = n_distinct(X)) %>% 
  arrange (desc(count_match))

#May 21st has the highest number of matches played in 2017

#5.In 2017-18 season, what is the number of matches played on each day of the week?

epl4 <- epl3 %>% 
  mutate(Dayname = wday(Match_Date, label = T, abbr = T))

epl4 %>% 
  filter(Season =="2017-18") %>% 
  group_by(Dayname) %>% 
  summarize(Dayname =unique(Dayname), count_match = n_distinct(X)) %>% 
  arrange (desc(count_match))

#6.Replicate the following plot shows the number of matches played in each month in the 2017-18 season. (Hint: Use theme_bw())

epl4$Month = factor(epl4$Month, levels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May"))

epl4 %>% 
  filter(Season =="2017-18") %>% 
  group_by(Month) %>% 
  summarize(Month =unique(Month), count_match = n_distinct(X)) %>% 
  ggplot(aes(Month, count_match)) + geom_col() + theme_bw() + 
  ggtitle("Matches played in the 2017??? 2018 Season")+
  ylab("Number of Matches")


#7.The following plot shows the number of matches played in a week in the 2017-18 season. Replicate it in R. (Hint: Use theme_bw(), and colors ranging from "lightblue"  to "darkblue")

epl5 <- epl4 %>% 
  mutate(Dayname = wday(Match_Date, label = T, abbr = T))

epl6 <- epl5 %>% 
  filter(Season =="2017-18") %>% 
  group_by(Month, Dayname) %>% 
  summarize(Month =unique(Month), Dayname = unique(Dayname), Count= n_distinct(X)) %>% 
  select(Month, Dayname, Count)

epl6$Dayname = factor(epl6$Dayname,levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

epl6 %>% 
  group_by(Dayname) %>% 
  ggplot(aes(Month, Dayname))+
  geom_tile(aes(fill = Count)) +
  theme_bw() + 
  scale_fill_gradient(low="lightblue", high="darkblue")+
  ggtitle("Matches played in 2017-18 season by Month and Day of Week")+
  xlab("Month of Season 2017-18") +
  ylab("Day of Week")

#8. the President.txt dataset to create the following US map. The numbers on the map represent the number of presidents from each state (home states). The colors (low=white and high=darkred) reflects the total number of presidents from each state (white color means zero).

usstate <-map_data("state")

president <- read.delim(file.choose())
president$State = tolower(president$State)
president <- mutate(president, region = State)

president1 <- president %>% 
  group_by(State) %>% 
  summarize(region = unique(State), count = n_distinct(President))

president2 <- left_join(usstate, president1, by = "region" )
president2[is.na(president2)]<-0

president2 %>% 
  ggplot(aes(long, lat, group=group, fill = count))+
  geom_polygon( color = "black") + 
  scale_fill_gradient( low = "white", high = "darkred",breaks = seq(0,8,2))+
  theme_void()+labs(fill ="Number of Presidents")


