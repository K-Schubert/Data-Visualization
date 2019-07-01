library(robotstxt)
library(rvest)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)
library(ggrepel)

############
### TdF ####
############

paths_allowed(
  paths = c("https://en.wikipedia.org/wiki/List_of_Tour_de_France_general_classification_winners")
)

tdf <- read_html("https://en.wikipedia.org/wiki/List_of_Tour_de_France_general_classification_winners")
tdf

tdf %>%
  html_nodes("table") %>%
  html_table(fill=T) %>%
  extract2(3) -> winners

winners

# Remove rows with no data
winners %>%
  filter(!(Country == 'World War I'), !(Country == 'World War II')) -> winners

# Remove rows with no Total Time
winners %>%
  select('Time/Points') %>%
  apply(1, nchar) > 2 -> ind
  
winners <- winners[ind, ]

# Reformat Year as numeric
winners %>%
  select(Year) %>%
  mapply(str_sub, start=1, end=4, .) %>%
  as.numeric -> years

winners$Year <- years

# Reformat Stage wins as numeric
winners$`Stage wins` <- as.numeric(winners$`Stage wins`)

# Reformat Stages in lead as numeric
winners$`Stages in lead` <- as.numeric(winners$`Stages in lead`)

# Reformat Distance as numeric
winners %>%
  select(Distance) %>%
  mapply(str_sub, start=1, end=5, .) %>%
  sub(',', '', .) %>%
  as.numeric -> distance
  
winners$Distance <- distance

# Reformat Total Time in minutes
winners %>%
  rename(Time='Time/Points') -> winners

winners %>%
  select(Time) %>%
  mapply(sub, 'h.*', '', .) %>%
  as.numeric -> h

winners %>%
  select(Time) %>%
  mapply(sub, "'.*", '', .) %>%
  str_sub(-2, -1) %>%
  as.numeric -> min

winners %>%
  select(Time) %>%
  mapply(sub, '".*', '', .) %>%
  str_sub(-2, -1) %>%
  as.numeric -> sec
  
winners$Time <- h*60 + min + sec/60

# Bar chart wins by country
winners %>% 
  group_by(Country) %>%
  summarise(Freq=n()) %>%
  arrange(Freq) %>%
  ggplot() +
  geom_bar(stat='identity', aes(x=factor(Country, levels=Country), y=Freq, fill=Country)) +
  #scale_fill_brewer(palette = "Spectral") +
  ggtitle("Tdf Wins by Country (1903-2018") +
  xlab('Countries') + 
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

# Diverging bars
winners %>% 
  group_by(Country) %>%
  summarise(Freq=n()) %>%
  arrange(Freq) %>%
  mutate(type=ifelse(Freq < mean(Freq), "below", "above")) %>%
  mutate(rel_avg=round(Freq-mean(Freq), 2)) %>%
  arrange(rel_avg) %>%
  ggplot(aes(x=factor(Country, levels=Country), y=rel_avg)) + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="Wins", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Number of wins compared to average wins", 
       title= "Diverging Bars") + 
  xlab('Country') +
  coord_flip()

# Lollipop chart
winners %>% 
  group_by(Country) %>%
  summarise(Freq=n()) %>%
  arrange(Freq) %>%
  mutate(type=ifelse(Freq < mean(Freq), "below", "above")) %>%
  mutate(rel_avg=round(Freq-mean(Freq), 2)) %>%
  arrange(rel_avg) %>%
  ggplot(aes(x=factor(Country, levels=Country), y=rel_avg, label=rel_avg)) + 
  geom_point(stat='identity', size=6, color='steelblue')  +
  geom_segment(aes(y = 0, 
                   x = factor(Country, levels=Country), 
                   yend = rel_avg, 
                   xend = factor(Country, levels=Country)), 
               color = "steelblue") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Number of wins compared to average wins") + 
  ylim(-10, 25) +
  xlab('Country') + 
  coord_flip()

# Time vs Year
winners %>%
  ggplot(aes(x=Year, y=Time)) + 
  geom_point(aes(col=Cyclist)) + 
  stat_smooth(method='lm', formula = y~poly(x, 3), se=F, color='firebrick') + 
  theme(legend.position = "none") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Time vs Year')

# Distance vs Year
lab <- ifelse(winners$'Stage wins'>5, winners$Cyclist, "")

winners %>%
  ggplot(aes(x=Year, y=Distance)) + 
  geom_point(aes(col=Cyclist)) + 
  #geom_text(aes(label=lab), size=2) +
  geom_label_repel(aes(label=lab), size=2) +
  stat_smooth(method='lm', formula = y~poly(x, 4), se=F, color='red') + 
  theme(legend.position = "none") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Distance vs Year')

# Bar chart of stage wins per cyclist
winners %>%
  ggplot(aes(x=Cyclist, y=`Stage wins`, fill=Cyclist)) + 
  geom_bar(stat='identity') + 
  theme(legend.position = "none") +
  theme(text = element_text(size=5), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Stage wins per TdF winner') + 
  coord_flip()

