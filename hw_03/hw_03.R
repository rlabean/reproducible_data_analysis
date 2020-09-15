###
# A script for completing homework 3
# Reproducible Data Analysis GEOL 490
# Robert LaBean
###

# Load libraries

library(tidyverse)


# Read the file "med_enz.csv"  

med_enz <- read.csv("data/med_enz.csv")

# Determine class, structure, number of rows, 
# and "glimpse med_enz"

class(med_enz)
str(med_enz)
nrow(med_enz)
glimpse(med_enz)

# Creating a plot of the "activity.nmM.hr" column of med_enz

p <- ggplot(data = med_enz, aes(x = activity.nM.hr)) + 
  geom_histogram()
print(p)
ggsave(filename = "plots/hmk_2_plot.png", plot = p, height = 3, width = 4, units = "in", dpi = 300)



