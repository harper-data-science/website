## HEADER ####
## who: EH
## what: Bootcamp 7 code
## when last edited: 2024-03-13

## CONTENTS ####

## 3 ####
# Try this:

# Download the 7-chickwts.xlsx file, read it into a data 
# object in R called "chicks", 
# and convert the "feed" variable to a factor if necessary.

# Do not neglect looking inside the "raw" data file
# Is it as you expect?  Is the data dictionary present and clear?

# Load necessary libraries
library(openxlsx)

# Read file
setwd("D:/Dropbox/git-harper-data-science/website/pages/2024-03-13-RSB07") 
# NB change to YOUR file path...
chicks <- read.xlsx("7-chickwts.xlsx")

# Convert feed to factor if needed
class(chicks$feed) # Character
chicks$feed <- factor(chicks$feed)
class(chicks$feed) # Factor


## 3.2 ####
# Try this:

# Summarize the whole dataset
# summary() provides summary statistics for numeric variables and counts
summary(chicks)

# we might want to look at summary for different levels of feed
?summary
summary(object = chicks$weight[which(chicks$feed == "casein")])
summary(object = chicks$weight[which(chicks$feed == "horsebean")])
# etc. - this method is easy but inelegant?

# aggregate()
?aggregate

# mean
aggregate(x = chicks$weight, by = list(chicks$feed), FUN = mean)

# standard deviation
aggregate(x = chicks$weight, by = list(chicks$feed), FUN = sd)

# You can make your own function for the FUN argument
# stadard error of mean, SEM = standard deviation / square root of sample size
aggregate(x = chicks$weight, by = list(chicks$feed), 
          FUN = function(x){ sd(x)/sqrt(length(x)) })

# You can apply several functions and name them!
aggregate(x = chicks$weight, by = list(feed = chicks$feed), 
          FUN = function(x){ c(mean = mean(x), 
                               sd = sd(x),  
                               SEM = sd(x)/sqrt(length(x)))})

## 4.3 ####
# The least you can do
help(hist)
hist(x = chicks$weight)

# Argument main
hist(x = chicks$weight,
     main = "Distribution of chick weights (all feeds)")


# x axis title
hist(x = chicks$weight,
     main = "Distribution of chick weights (all feeds)",
     xlab = "Chick weight (grams)")

# Add vertical line for mean weight
hist(x = chicks$weight,
     main = "Distribution of chick weights (all feeds)",
     xlab = "Chick weight (grams)")

help(abline)
abline(v = mean(chicks$weight), col = "green", lty = 3, lwd = 5)

# Try a boxplot

help(boxplot)
boxplot(x = chicks$weight)

# I have seen worse graphs, but I can't remember when.
# Flash challenge: Improve the graph

# weight as a function of feed
boxplot(formula = weight ~ feed,
        data = chicks)
# This is probably a good representation of our hypothesis
# Flash challenge: Improve the graph...
