##
# Script for completing homework 4
# Manipulating data tables to make them tidy and
# plotting them with ggplot
# Reproducible Data Analysis
# Robert LaBean
##

# Load needed libraries

library(tidyverse)

# Read file

baby_names <- read.csv("data/chris_names_wide.csv")

# Manipulate the data to make it tidy

baby_names_t <- pivot_longer(baby_names, cols = -"year", names_to = "sex",
                             values_to = "number")

# Plot "year" on x-axis, "count" on y, different color for "sex"

color_plot <- ggplot(data = baby_names_t) +
  geom_point(mapping = aes(x = year, y = number, color = sex))
print(color_plot)

# Plot same axes as before but with a smoothed line

loess_plot <- ggplot(data = baby_names_t) +
  geom_smooth(mapping = aes(x = year, y = number))
print(loess_plot)

# Boxplot with "sex" on x-axis and "number" on y

box_plot <- ggplot(data = baby_names_t) +
  geom_boxplot(mapping = aes(x = sex, y = number))
print(box_plot)

