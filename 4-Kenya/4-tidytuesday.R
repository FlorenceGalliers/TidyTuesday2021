## TidyTuesday Week 4 ####
## Florence Galliers 
## 2020-01-19

# install packages and data
install.packages("remotes")
install.packages("tidytuesdayR")
remotes::install_github("Shelmith-Kariuki/rKenyaCensus")
tuesdata <- tidytuesdayR::tt_load('2021-01-19')
library(ggplot2)
library(tmap)

# load crops dataset from tidytuesdayR
crops <- tuesdata$crops

# get shapefiles
counties <- rKenyaCensus::KenyaCounties_SHP

# remove Kenya from dataset so just the counties are left
crops2 <- crops[-1, ]

# rename SubCounty column to County
names(crops2)[names(crops2) == "SubCounty"] <- "County"

# combine crop2 dataset with counties dataset
data <- merge(counties,
              crops2,
              by="County")

# map data to check shapefiles
tm_shape(data) +
  tm_polygons()

# rename two more columns
names(crops2)[names(crops2) == "Cashew Nut"] <- "Cashew"
names(crops2)[names(crops2) == "Khat (Miraa)"] <- "Khat"

# remove farming column as this is the highest for all of them
crops2 <- crops2[ ,-2]

# Find largest number of people growing a particular crop in each county
crops2$maxvalue <- pmax(crops2$Tea, 
     crops2$Coffee,
     crops2$Avocado,
     crops2$Citrus,
     crops2$Mango,
     crops2$Coconut,
     crops2$Macadamia,
     crops2$Cashew,
     crops2$Khat,
     na.rm = T)

# the above returns only the maximum value, not which column it comes from
# try another method

# remove counties that have NA for all crops
to_remove <- c(8,10,11,23,25,47)
crops3 <- crops2[-to_remove, ]
# make new data frame showing most popular crops for each county
popular <- colnames(crops3)[apply(crops3, 1, which.max)]
# combine these together
summary <- data.frame(crops3[, 1], popular)

# Merge counties shape files with summary data frame
data2 <- merge(counties,
              summary,
              by="County",
              all=T)

# Make a map!
tm_shape(data2) +
  tm_polygons(col = "popular",
              title = "") +
  tm_layout(title = "Which crop is grown by the most\nhouseholds in each county of Kenya?",
            title.size = 1,
            title.position = c("left", "top"),
            bg.color = "aliceblue",
            legend.title.size = 0.8,
            legend.title.fontface = "bold",
            inner.margins = c(0.1, 0.1, 0.1, 0.1)) +
  tm_credits("    TidyTuesday \n        W4, 2021 \n  @florencelydia11 \ndata:rKenyaCensus",
             size = 0.6,
             col = "grey60",
             position = c(0.75, 0.03))
