## HEADER ####
## TidyTuesday Week 8 (Du Bois Challenge)
## Florence Galliers
## 2020-02-16

# Packages ####
library(patchwork)
library(gt)
library(tidyr)
library(scales)
library(ggnewscale)

# Import Fonts
library(extrafont)
library(showtext)
#font_import()

# Import Data ####
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
income <- tuesdata$income

income

# Left hand Table ####
gt(income[, 1:2 ]) %>%
  tab_options(
    table_body.hlines.color = "black",
    table_body.vlines.color = "black",
    table_body.border.bottom.color = "black",
    table.font.names = "Tomorrow-Light",
    heading.align = "center"
  )
  
# Top Table ####

# Main Graph ####
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

# define colour palette
colours <- c("#111210",
             "#80607f",
             "#d78f7e",
             "#b4ada5",
             "#dbd0c3")

# create function to add in % symbols to labels
pp <- function (x) {number_format(
  accuracy = 1,
  suffix = "%"
) (x)}
# test this out
pp(income2$pct[3])


# make the main plot
ggplot(income2, aes(x = Class,
                    y = -pct, 
                    fill = category,
                    label = pp((pct)))) +
  geom_bar(stat = "identity", 
           position = "stack") +
  geom_text(stat = "identity",
            position = position_stack(0.5)) +
  scale_fill_manual(values = c("#111210",
                               "#80607f",
                               "#d78f7e",
                               "#b4ada5",
                               "#dbd0c3")) +
  coord_flip() +
  theme(
    plot.background = element_rect(fill = "#f2dfce"),
    panel.background = element_rect(fill = "#f2dfce"),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  ) 


# Dollar Image ####

# Title ####
