#---
# Date: 2024-10-17
# Author: M.Lewis
# Title: Exploring the esquisse package in R for data visualisation
#---

# Install/load package
#install.packages("esquisse")
library(esquisse)



# Load in built-in R dataset data 
#install.packages("palmerpenguins")
library(palmerpenguins)

data <- palmerpenguins::penguins

#Print first 6 rows
head(data)

#Two ways of importing data into esquisee

# 1 - Launch the esquisse app
esquisser()

# 2 - launch esquisse app with specified dataset
esquisser(data) #Experiment with esquisse and export code






# Viewing esquisse - three options
esquisser(data) # default (dialog window)

esquisser(data, viewer = "pane") # in the RStudio viewer pane

esquisser(data, viewer = "browser") # in your web browser