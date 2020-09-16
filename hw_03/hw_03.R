###
# A script for completing homework 3
# Reproducible Data Analysis GEOL 490
# Robert LaBean
###

# Load libraries

library(tidyverse)

# Identifying class types

class(c(TRUE, FALSE, FALSE))
class(c(1, 2, 3))
class(c(1.3, 2.4, 3.5))
class(c("a", "b", "c"))

class(c(1, 2, "a")) # This is a character vector because one 
# of the components is a character

class(c(TRUE, FALSE, 2)) # This is a numeric vector because
# the number 2 is numeric, and TRUE and FALSE are stored a 1 & 0


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



