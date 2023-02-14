# load in necessary functions
library(readr)
library(tidyverse)

# Read files
gapminder_1997 = read.csv("gapminder_1997.csv")
View(gapminder_1997)

# Explore different R commands
?read.csv
sum (5,6)
Sys.Date()

# Creating a plot
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap , y = lifeExp , color = continent,
      size = pop/1000000)+
  labs(x = "GDP Per Capita" , y = "Life Expectancy (yrs)",
       tittle = "Do people in wealthy countries life longer",
       size = "Population (in millions)") +
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette = "Set1")
  # Add life expectancy to y-aixis
  
# Read in full gapminder dataset
gapminder_data = read.csv(file = "gapminder_data.csv")

# Plot time (x-axis) and life expect (y-axis) points
ggplot(data = gapminder_data) + 
  # Add the aes objects time & lifeExpt
  aes(x = year , y = lifeExp , color = continent,
      group = country) +
  # Add the human readable labels
  labs(x = "Time (years)" , y = "Life Expectancy (years)") +
  # Points
  geom_line() +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# challenge question
ggplot(data = gapminder_1997) + 
  # Add the aes objects time & lifeExpt
  aes(x = continent , y = lifeExp , color = continent) +
  # Add the human readable labels
  labs(x = "Continent" , y = "Life Expectancy (years)") +
  # Points
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(alpha = 0.5 , size = 5) #make point colors less intense
  theme_minimal() +
  scale_color_brewer(palette = "Set1")