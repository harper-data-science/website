## HEADER
## who: Ed H
## what: RSB10
## when: 2024-04-03

## fish ####
library(openxlsx)

# NB your file may be in a different location to mine!
fish <- read.xlsx('10-fish.xlsx')

# Download the fish data .xlsx file linked above and load it into R
# (I named my data object "fish") 
# Try this:

names(fish)
table(fish$Species)

# slice out the rows for Perch

fish$Species=="Perch" #just a reminder
table(fish$Species=="Perch")

perch <- fish[fish$Species=="Perch" , ]
head(perch)


# Try this:
# A simple regression of perch Height as the predictor variable (x)
# and Width as the dependent (y) variable

# First make a plot
plot(
  y = perch$Height,
  x = perch$Width,
  ylab = "Height",
  xlab = "Width",
  main = "My perch regression plot",
  pch = 20,
  col = "goldenrod",
  cex = 1,
  xlim = c(0,9),
  ylim = c(0,13)
  
)

#hist(perch$Height)
# Does it look there is a strong linear relationship
# (it looks very strong to me)

# In order to draw on the line of best fit we must calculate the regression

# ?lm 

# We usually would store the model output in an object

mylm <- lm(formula = Height ~ Width, # read y "as a function of" x 
           data =  perch)
mylm # NB the intercept (0.30), and the slope (1.59)


# We use the abline() function to draw the regression line onto our plot
# NB the 

# ?abline

abline(reg = mylm) # Not bad

# Some people like to summarize the regression equation on their plot
# We can do that with the text() function
# y = intercept + slope * x

# ?text

text(x = 3,    # x axis placement
     y = 11,   # y axis placement
     labels = "y = 0.30 + (1.59) * x")

text(x=7, y=4,
     labels = 'Hello')

## Test assumptions ####
# Try this:

# Test Gaussian residuals

# Make our plot and regression line again
plot(y = perch$Height, x = perch$Width,
     ylab = "Height", xlab = "Width",
     main = "My perch RESIDUAL plot",
     pch = 20, col = "blue", cex = 1)
abline(reg = mylm)

# We can actually "draw on" the magnitude of residuals
arrows(x0 = perch$Width,
       x1 = perch$Width,
       y0 = predict(mylm), # start residual line on PREDICTED values
       y1 = predict(mylm) + residuals(mylm), # length of residual
       length = 0) # makes arrowhead length zero (or it looks weird here)

# residual distribution
# Try this:

library(car) # for qqPlot()
par(mfrow = c(1,1)) # Print graphs into 1x2 grid (row,column)

hist(residuals(mylm), main = "")
qqPlot(residuals(mylm))

## Gussie up the histogram ####

# Make a new histogram
hist(residuals(mylm), 
     xlim = c(-2, 2), ylim = c(0,.9),
     main = "",
     prob = T) # We want probability density this time (not frequency)

# Add a density line to just help visualize "where the data are"
lines(                       # lines() function
  density(residuals(mylm)),   # density() function
  col = "green4", lty = 1, lwd = 3) # Mere vanity

# Make x points for theoretical Gaussian
x <- seq(-1,+1,by=0.02) 

# Draw on theoretical Gaussian for our residual parameters
curve(dnorm(x, mean = mean(residuals(mylm)),
            sd = sd(residuals(mylm))),
      add = T,
      col = "blue", lty = 3, lwd = 3) # mere vanity

# Draw on expected mean
abline(v = 0, # vertical line at the EXPECTED resid. mean = 0
       freq = F,
       col = "red", lty = 2, lwd = 3) # mere vanity


# Add legend
legend(x = .6, y = .9,
       legend = c("Our residuals", "Gaussian", "Mean"),
       lty = c(1,3,2),
       col = c("green4", "blue","red"), lwd = c(3,3,3))


## Shapiro test ####
# Try this:

shapiro.test(residuals(mylm))



summary(mylm)
