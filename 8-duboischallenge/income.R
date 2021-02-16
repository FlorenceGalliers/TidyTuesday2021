## HEADER ####
## TidyTuesday Week 8 (Du Bois Challenge)
## Florence Galliers
## 2020-02-16

# Packages ####
library(ggplot2)
library(gt)
library(tidyr)
library(scales)
library(ggnewscale)
library(grid)
library(gridExtra)
# Import Fonts
library(extrafont)
library(showtext)
#font_import()

# Import Data ####
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
income <- tuesdata$income

# function to add dollar sign
dollar <- function (x) {number_format(
  accuracy = 1,
  prefix = "$"
) (x)}

avrg <- dollar(income$`Actual Average`)

income$`Actual Average` <- avrg
  
# Main Plot ####
income2 <- income %>%
  pivot_longer(names(income[3:7]),
               names_to = "category",
               values_to = "pct")

income2$pct[4] <- 0
income2$pct[5] <- 9.9

income2$category <- as.factor(income2$category)
income2$category <- factor(income2$category, 
                           levels = c("Rent",
                                      "Food",
                                      "Clothes",
                                      "Tax",
                                      "Other"))
income2$Class <- as.factor(income2$Class)
income2$Class <- factor(income2$Class,
                        levels = c("$100-200",
                                   "$200-300",
                                   "$300-400",
                                   "$400-500",
                                   "$500-750",
                                   "$750-1000",
                                   "Over $1000"))
income2$Class <- factor(income2$Class,
                        levels = rev(levels(income2$Class)))

# subset the data to remove 0 values
income2 <- income2 %>%
  subset(pct != 0)

# define colour palette
colours <- c("#111210",
             "#80607f",
             "#d78f7e",
             "#b1b5bd",
             "#dbd0c3")

# create function to add in % symbols to labels
pp <- function (x) {number_format(
  accuracy = 1,
  suffix = "%"
) (x)}
# test this out
pp(income2$pct[3])


# make the main plot
plot1 <- ggplot(income2, aes(x = Class,
                    y = -pct, 
                    fill = category,
                    label = pp(pct))) +
  geom_bar(stat = "identity", 
           position = "stack",
           width = 0.7,
           colour = "black",
           size = 0.2) +
  labs(title = "INCOME AND EXPENDITURE OF 150 NEGRO FAMILIES IN ATLANTA, GA., U.S.A",
       caption = "TidyTuesday Week 8 | Florence Galliers  | #DuBoisChallenge") +
  geom_text(stat = "identity",
            position = position_stack(0.5),
            family = "Tomorrow",
            size = 4) +
  scale_fill_manual(values = c("#111210",
                               "#80607f",
                               "#d78f7e",
                               "#b1b5bd",
                               "#dbd0c3"), 
                    labels = c("RENT.", "FOOD.", "CLOTHES.", "DIRECT TAXES.", "OTHER EXPENSES AND SAVINGS")) +
  coord_flip(clip = "off") +
  theme(
    plot.background = element_rect(fill = "#f2dfce"),
    panel.background = element_rect(fill = "#f2dfce"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(family = "Tomorrow",
                             size = 10,
                             colour = "black"),
    axis.text.x = element_blank(),
    legend.position = c(0.505, 1),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(family = "Tomorrow", 
                               size = 10),
    legend.key.width = unit(5.34, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.background = element_rect(fill = "#f2dfce"),
    plot.title = element_text(family = "Tomorrow-Medium", 
                              hjust = 0.4,
                              vjust = 4,
                              size = 25),
    plot.margin = unit(c(1.2, 1.2, 1.2, 1.2), "cm")
  ) +
  guides(fill = guide_legend(label.position = "top")) +
  annotate("text", x = 7, y = -90, label = "19%", colour = "white", size = 4, family = "Tomorrow") +
  annotate("text", x = 6, y = -89, label = "22%", colour = "white", size = 4, family = "Tomorrow") +
  annotate("text", x = 5, y = -88,label = "23%", colour = "white", size = 4, family = "Tomorrow") +
  annotate("text", x = 4, y = -92, label = "18%", colour = "white", size = 4, family = "Tomorrow") +
  annotate("text", x = 3, y = -94, label = "13%", colour = "white", size = 4, family = "Tomorrow") +
  annotate("text", x = 7, y = -105, label = avrg[1], colour = "black", size = 3.5, family = "Tomorrow") +
  annotate("text", x = 6, y = -105, label = avrg[2], colour = "black", size = 3.5, family = "Tomorrow")+
  annotate("text", x = 5, y = -105, label = avrg[3], colour = "black", size = 3.5, family = "Tomorrow")+
  annotate("text", x = 4, y = -105, label = avrg[4], colour = "black", size = 3.5, family = "Tomorrow")+
  annotate("text", x = 3, y = -105, label = avrg[5], colour = "black", size = 3.5, family = "Tomorrow")+
  annotate("text", x = 2, y = -105, label = avrg[6], colour = "black", size = 3.5, family = "Tomorrow")+
  annotate("text", x = 1, y = -105, label = avrg[7], colour = "black", size = 3.5, family = "Tomorrow")+
  annotate("text", x = 7.3, y = -105, label = "AVERAGE", colour = "black", size = 3.5, family = "Tomorrow") +
  annotate("text", x = 7, y = 3, label = "POOR.", colour = "black", size = 3, family = "Tomorrow-Light")+
  annotate("text", x = 6, y = 3, label = "POOR.", colour = "black", size = 3, family = "Tomorrow-Light")+
annotate("text", x = 5, y = 3, label = "FAIR.", colour = "black", size = 3, family = "Tomorrow-Light")+
annotate("text", x = 4, y = 3, label = "FAIR.", colour = "black", size = 3, family = "Tomorrow-Light")+
annotate("text", x = 3, y = 3, label = "COMFORT.", colour = "black", size = 3, family = "Tomorrow-Light")+
annotate("text", x = 2, y = 3, label = "COMFORT.", colour = "black", size = 3, family = "Tomorrow-Light")+
annotate("text", x = 1, y = 3, label = "WELL\nTO-DO.", colour = "black", size = 3, family = "Tomorrow-Light")
  

plot1

