## HEADER ####
## Florence Galliers
## Last edited: 2020-01-26
## Tidy Tuesday Week 5

library(tidytuesdayR)
library(tidyverse)
library(ggplot2)

# load data
tuesdata <- tidytuesdayR::tt_load('2021-01-26')
plastics <- tuesdata$plastics

head(plastics)

# columns 4 : 11 need to be pivot_longered
plastics_tidy <- plastics %>%
  pivot_longer(names(plastics)[4:11],
               names_to = "type",
               values_to = "count")

head(plastics_tidy)

summary(plastics_tidy)

# split into 2019 and 2020
plastics_2019 <- plastics_tidy %>%
  filter(year == "2019")

plastics_2020 <- plastics_tidy %>%
  filter(year == "2020")

# Just look at one country
usa <- plastics_tidy %>%
  filter(country == "United States of America")

usa$parent_company <- as.factor(usa$parent_company)
unique(usa$parent_company)

usa2020 <- usa %>%
  filter(year == "2020")
usa2020$parent_company <- as.factor(usa2020$parent_company)

plot(usa2020$grand_total)

highest <- which(usa2020$grand_total > 100)

highest_usa <- usa2020[highest, ]

highest_usa$type <- as.factor(highest_usa$type)

ggplot(highest_usa,
       aes(parent_company, grand_total),
       colour = count) +
  geom_col(position = "stack")
