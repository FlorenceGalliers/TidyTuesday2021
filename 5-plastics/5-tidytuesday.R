## HEADER ####
## Florence Galliers
## Last edited: 2020-01-26
## Tidy Tuesday Week 5

library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(extrafont)
library(showtext)
#font_import()

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

# Just get companies with more than 100 pieces of plastic 
highest <- which(usa2020$grand_total > 100)
highest_usa <- usa2020[highest, ]
# convert to factor
highest_usa$type <- as.factor(highest_usa$type)

ggplot(highest_usa,
       aes(x = parent_company,
           y = count,
           group = parent_company,
           fill = type)) +
  geom_bar(stat = "identity",
           position = "stack")

# exclude 'null' and 'unbranded'
highest_usa2 <- highest_usa %>%
  filter(parent_company != "null")
highest_usa2 <- highest_usa2 %>%
  filter(parent_company != "Unbranded")

# add in fonts to match company logos
font_add(family = "avant-garde", 
         regular = "./ITCAvantGardeStd-Bold.otf")
font_add(family = "pepsi", 
         regular = "./pepsi.ttf")
font_add(family = "cocacola", 
         regular = "./LOKICOLA.ttf")
font_add(family = "helvetica-rounded", 
         regular = "./HelveticaRounded.otf")
font_add(family = "santana", 
         regular = "./Santana-Black.ttf")
font_add(family = "consular", 
         regular = "./consular.ttf")
font_add(family = "syntax", 
         regular = "./Syntax-Bold.ttf")
showtext_auto()

# Main Plot
ggplot(highest_usa2,
       aes(x = reorder(parent_company, -count),
           y = count,
           group = parent_company,
           fill = type)) +
  geom_bar(stat = "identity",
           position = "stack") +
  labs(title = "Which companies are the largest sources of branded plastic pollution in the USA?",
       fill = "Type of Plastic",
       caption = "TidyTuesday Week 5
Florence Galliers") +
  ylab("Count of plastic") +
  xlab("") +
  scale_fill_brewer(palette = "Spectral") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 17.5,
                             colour = "grey90",
                             face = "bold"),
        title = element_text(colour = "grey50"),
        axis.title.y = element_text(size = 10,
                                    hjust = 0.52,
                                    colour = "grey90"),
        axis.text.y = element_text(colour = "grey90"),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 10, colour = "white"),
        legend.text = element_text(colour = "grey90"),
        legend.background = element_rect(fill = "grey10"),
        legend.position = c(0.8, 0.9),
        legend.direction = "horizontal") +
  guides(fill = guide_legend(reverse = T)) +
  annotate("text", 
           x = 1, y = -20, 
           label = "The Kroger
Company",
           colour = "#084999",
           size = 4,
           family = "avant-garde") +
  annotate("text", 
           x = 2, y = -15, 
           label = "Pepsico",
           colour = "#28458E",
           size = 5,
           family = "pepsi") +
  annotate("text", 
           x = 3, y = -15, 
           label = "CocaCola",
           colour = "#F40009",
           size = 6,
           family = "cocacola") +
  annotate("text", 
           x = 4, y = -15, 
           label = "Nestle",
           colour = "#ddc694",
           size = 6,
           family = "helvetica-rounded") +
  annotate("text", 
           x = 5, y = -15, 
           label = "STARBUCKS",
           colour = "#00704A",
           size = 5,
           family = "santana") +
  annotate("text", 
           x = 6, y = -15, 
           label = "FAGE",
           colour = "#0053A0",
           size = 6) +
  annotate("text", 
           x = 7, y = -14, 
           label = "phillip morris",
           colour = "#4ea2da",
           size = 6,
           family = "consular") +
  annotate("text", 
           x = 8, y = -14, 
           label = "SUNTORY",
           colour = "#5bc2dc",
           size = 5,
           family = "syntax") +
  annotate("text",
           x = 3.6, y = 340,
           label = "PET plastic is a polyester
and is commonly used to 
make plastic drinks bottles
and food containers",
           size = 4,
           colour = "#c3d674") +
  annotate("curve",
           xend = 2.3, yend = 280,
           x = 2.9, y = 315,
           curvature = -.2,
           arrow = arrow(type = "closed", 
                         length = unit(0.40, "lines")),
           colour = "#7b8749") +
  annotate("curve",
           xend = 3.1, yend = 200,
           x = 3.3, y = 290,
           curvature = -.2,
           arrow = arrow(type = "closed", 
                         length = unit(0.40, "lines")),
           colour = "#7b8749") +
  annotate("curve",
           xend = 4, yend = 180,
           x = 4.2, y = 290,
           curvature = -.2,
           arrow = arrow(type = "closed", 
                         length = unit(0.40, "lines")),
           colour = "#7b8749") +
  annotate("text",
           x = 5.6, y = 240,
           label = "PP Plastic is a 
polypropeylene and is used
to make products such as 
yoghurt containers and 
carry out beverage cups",
           size = 4, 
           colour = "#9DDF9E") +
  annotate("curve",
           xend = 6.1, yend = 100,
           x = 6.1, y = 180,
           curvature = -.2,
           arrow = arrow(type = "closed", 
                         length = unit(0.40, "lines")),
           colour = "#74a375") +
  annotate("curve",
           xend = 4.8, yend = 100,
           x = 4.8, y = 210,
           curvature = .2,
           arrow = arrow(type = "closed", 
                         length = unit(0.40, "lines")),
           colour = "#74a375") +
  annotate("curve",
           xend = 1.5, yend = 470,
           x = 5.1, y = 300,
           curvature = .3,
           arrow = arrow(type = "closed", 
                         length = unit(0.40, "lines")),
           colour = "#74a375") 


