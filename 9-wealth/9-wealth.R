## HEADER ####
## TidyTuesday Week 9
## Florence Galliers
## 2021-02-10

library(tidytuesdayR)
library(plyr)
library(treemap)

tuesdata <- tidytuesdayR::tt_load('2021-02-23')

employed <- tuesdata$employed
earn <- tuesdata$earn

head(employed)
head(earn)

summary(employed)

employed$industry <- as.factor(employed$industry)

count_data <- count(employed[which(employed$employ_n == 0 & employed$race_gender == "Women" & employed$year == 2020), 1])

subset_employed <- employed[which(employed$employ_n == 0 & employed$race_gender == "Women" & employed$year == 2020), ]

treemap(count_data,
        index=c("industry"),
        vSize="freq",
        type="index"
) 
