library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

data = read.csv("lab_sodra.csv", fileEncoding="UTF-8")
summary(data)

#1
data1 = data %>%
  filter(ecoActCode == 412000)

data1 %>%
  ggplot(aes(x=avgWage)) +
  geom_histogram(bins = 100)
#2

data2 = data %>%
  filter(ecoActCode == 412000) %>% group_by(code) %>% 
  summarise(suma = sum(avgWage)) %>% 
  arrange(desc(suma)) %>% head(5)
merged <- merge(data2, data, by = "code")

merged %>% 
  ggplot(aes(x = month, y = avgWage, group = name, colour = name)) + geom_line() +
  theme_minimal() + scale_y_continuous(breaks=seq(0,8000,1000)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = 1), axis.text.x = element_blank(),
        axis.ticks = element_line(colour = "black", size = 1))

#3
merged %>% group_by(name) %>% 
  summarise(maxApd = max(numInsured)) %>% 
  ggplot(aes(x = reorder(name, -maxApd), y = maxApd, fill = name)) +
  geom_col() + ylab("apdrausti") + xlab("name") +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(size = 1), 
        axis.ticks = element_line(colour = "black"))
