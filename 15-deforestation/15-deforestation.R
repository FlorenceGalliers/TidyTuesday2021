## HEADER ####
## Tidy Tuesday Week 15 
## Florence Galliers
## 2020-04-06

# Set up and libraries
library(tidyverse, ggplot)
library(waffle)
library(RColorBrewer)
library(patchwork)
library(scales)
library(ggtext)

## Import Data
tuesdata <- tidytuesdayR::tt_load('2021-04-06')
forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss
soybean_use <- tuesdata$soybean_use
vegetable_oil <- tuesdata$vegetable_oil


## Explore Data
summary(brazil_loss)
unique(brazil_loss$entity) # Only Brazil so this column isn't much use
unique(brazil_loss$code) # As above
# Year goes from 2001 to 2013

# create new variable called Brazil, without these columns
brazil <- brazil_loss %>%
  select(-entity, -code) 
# convert year to factor variable
brazil$year <- as.factor(brazil$year)
# pivot longer
b <- brazil %>%
  pivot_longer(
    2:12,
    names_to = "cause",
    values_to = "loss")
# convert cause to factor variable
b$cause <- factor(b$cause, ordered = T)

# remove 2001 so we just have 12 years
b <- b %>%
  filter(year != "2001")

# look at factor levels of causes of deforestation - combine some together
levels(b$cause)

levels(b$cause) <- c("Commercial crops and tree plantations", "Natural disturbances and fire", "Flooding due to dams",
                     "Mining", "Natural disturbances and fire", "Roads and infrastructure",
                     "Pasture", "Roads and infrastructure", "Selective Logging",
                     "Small Scale Clearing", "Commercial crops and tree plantations")
levels(b$cause)

# create new variable, merging the same causes in the same year together.
b2 <- b %>%
  group_by(year, cause) %>%
  summarise(sum(loss)) %>%
  rename("loss" = "sum(loss)")

b2 <- b2 %>%
  filter(year == 2002 | year == 2007 | year == 2013)


colpal <- c("#66C2A5",
            "#8DA0CB",
            "#E78AC3",
            "#A6D854",
            "#FFD92F",
            "#FC8D62",
            "#E5C494",
            "#B3B3B3")

cause_names <- levels(b2$cause)

p1 <- ggplot(b2, 
       aes(fill = cause,
           values = loss)) +
  geom_pictogram(n_rows = 10,
                 aes(label = cause,
                     values = loss,
                     color = cause),
                 flip = T,
                 size = 7,
                 make_proportional = T) +
  facet_wrap(~year, ncol=4) +
  labs(title = "<span style = 'color:#FC8D62;'>Pasture</span> is the largest direct driver of deforestation in Brazil",
       subtitle = "Despite total area of forest loss falling from 2002 to 2013, the largest proportion of loss was due to clearing of pasture for beef cattle") +
  scale_label_pictogram(
    name = NULL,
    values = "tree",
    labels = cause_names
  ) +
  scale_color_manual(
    name = NULL,
    values = colpal,
  labels = cause_names
  ) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "grey30"),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(color = "grey80"),
        strip.text = element_text(color = "white",
                                  size = 10),
        plot.title = element_markdown(size = 16, color = "white", face = "bold",
                                      hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey80"))



# Sum of deforestation over time
total <- b %>%
  group_by(year) %>%
  summarise(sum(loss)) %>%
  rename("sum" = "sum(loss)")

# make a line graph
p2 <- ggplot(total, aes(x = year, y = sum, group = 1)) +
  geom_line(color = "white",
            size = 1.5) +
  ylab("Annual forest loss (hectares)\n ") +
  xlab("Year") +
  labs(caption = "@florencelydia11 | Tidy Tuesday | Week 15") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "grey30"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.title.y = element_text(vjust = 0.5),
    plot.caption = element_text(color = "grey80")
  )

(p1)/p2


