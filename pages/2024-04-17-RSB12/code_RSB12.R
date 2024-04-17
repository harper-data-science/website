## HEADER ####
## who:
## what:
## when:

## 2.1
## 2 sample t-test long format data

density <- c(rep('high', 7), rep('low', 7))
height <- c(2,3,4,3,4,3,2,
            6,8,6,9,7,8,7)

(long.data <- data.frame(density,height))

## 2 sample t-test wide format data

(wide.data <- data.frame(high.ht = c(2,3,4,3,4,3,2),
                         low.ht = c(6,8,6,9,7,8,7)))

# Boxplot
boxplot(height ~ density, data = long.data,
        main = "2 independent samples")

# Optional: add raw data points
# jitter() nudges the x-axis placement 
# so that the points do not overlap
set.seed(42)
points(x = jitter(c(1,1,1,1,1,1,1,
                    2,2,2,2,2,2,2), 
                  amount = .2),
       y = long.data$height,
       col = "red", pch = 16, cex = .8) # Mere vanity

## 1 sample ####
# The data
mysam <- c(2.06, 1.77, 1.9, 1.94, 1.91, 1.83, 
           2.08, 1.84, 2.15, 1.84, 
           2.05, 2.19, 1.64, 1.81, 1.83)

boxplot(mysam, 
        main = "Is your sample population different\nfrom the dashed line?")

points(x = jitter(rep(1,15), amount = .1),
       y = mysam,
       col = "red", pch = 16, cex = .8) # Mere vanity

abline(h = 2.0,
       col = "blue", lty = 2, lwd = 2)  # Mere vanity

## paired ##

# Biochar application, measure N before and after

# Data 
# (the code are kind of ugly, but run it to "make" biochar)
biochar <- structure(list(
  plot = c("A", "B", "C", "D", "E", "F", "G", "H", "I", 
           "J", "K", "L", "M", "N", "O"), 
  N.first = c(13.4, 16.7, 17.9, 18.5, 18.6, 18.6, 18.7, 
              20.5, 20.6, 21.5, 24.2, 24.5, 25, 27.1, 28.1), 
  N.second = c(16, 16.7, 18.7, 18.7, 22.1, 22.7, 23.1, 
               23.1, 23.2, 23.5, 25.4, 25.9, 27.6, 28, 29.7)), 
  class = "data.frame", 
  row.names = c(NA, -15L))

biochar


# boxplot() would work, but hides pairwise relationship
# Try this:
set.seed(1)
plot(x = jitter(c(rep(1,15), rep(2,15)),amount = .02),
     y = c(biochar$N.first, biochar$N.second),
     xaxt = "n", xlim = c(0.5, 2.5),
     cex = .8, col = "blue", pch = 16,  # Mere vanity
     xlab = "Biochar treatment",
     ylab = "Soil N",
     main = "Do the lines tend to increase?")

mtext(side = 1, at = 1:2, text = c("before", "after"), line = 1)

# Get crazy: add horizontal lines to visualize the plot pairs
for(i in 1:15){
  lines(x = c(1.05,1.95),
        y = c(biochar$N.first[i], biochar$N.second[i]),
        lty = 2, lwd = 1, col = "red") # Mere vanity
}        


## stacked hist ####
# step 1 graph a hist() of the continuous variable
# separately for each factor level
set.seed(1)
height <- c(rnorm(30,185,10),
            rnorm(30,150,10))
sex <- c('m','m','m','m','m','m','m','m','m','m',
         'm','m','m','m','m','m','m','m','m','m',
         'm','m','m','m','m','m','m','m','m','m',
         'f','f','f','f','f','f','f','f','f','f',
         'f','f','f','f','f','f','f','f','f','f',
         'f','f','f','f','f','f','f','f','f','f')
data <- data.frame(height,sex)
rm(height, sex)

par(mfrow = c(2,1))                                # set so hist's "stack"

# draw males hist
hist(data$height[data$sex == 'm'],                 # select males
     xlim = c(min(data$height), max(data$height)), # set limit for ALL data
     col = "blue", freq = F,
     xlab = "Height (cm)", main = "Males") 

mv <- data$height[data$sex == 'm']
xcoord <- seq(min(mv),                          # make x coordinates
              max(mv),
              length = length(mv))
ycoord <- dnorm(x = xcoord,                     # make y coordinates
                mean = mean(mv),
                sd = sd(mv))

lines(x = xcoord, y = ycoord,                  # draw curve
      col = 'lightblue', lwd = 3)

# draw females hist
hist(data$height[data$sex == 'f'],                 # select females
     xlim = c(min(data$height), max(data$height)), # set limit for ALL data
     col = "red", freq = F,
     xlab = "Height (cm)", main = "Females")

mv <- data$height[data$sex == 'f']
xcoord <- seq(min(mv),                          # make x coordinates
              max(mv),
              
              length = length(mv))
ycoord <- dnorm(x = xcoord,                     # make y coordinates
                mean = mean(mv),
                sd = sd(mv))
lines(x = xcoord, y = ycoord,                   # draw curve
      col = 'pink', lwd = 3)


## wrong shapiro ####
shapiro.test(data$height[data$sex == 'm'])


## 2 sample t-test ####
# 2-sample t-test
# Try this
# data
density <- c("high","high","high","high","high","high","high",
             "low","low","low","low","low","low","low")
height <- c(2.1,3.5,4.3,3.2,4.5,3.7,2.7, 
            6.1,8,6.9,9.1,7.5,8,7.4)
(treegrowth <- data.frame(density,height))

t.test(formula = height ~ density, 
       data = treegrowth)

# test stat, df or n, p value
# t-test: t = -8.63, 11.87, P < 0.0001

# Try this:

# Data
earwigs <- c(22.1, 16.3, 19.1, 19.9, 19.2, 17.7, 
             22.5, 17.7, 24.1, 17.8, 
             21.9, 24.9, 13.8, 17.2, 
             17.6, 19.9, 17.1, 10, 10.7, 22)

# Flash Challenge: Assess this data for adherence to the Gaussian assumption

mymu <- 17.0 # Our mu

# ?t.test #notice the mu argument

t.test(x = earwigs,
       mu = mymu)

# Data
# Try this:
# Data
cort.t0 <- c(0.59, 0.68, 0.74, 0.86, 0.54, 0.85, 0.7, 0.81, 0.79, 0.76, 
             0.49, 0.64, 0.74, 0.51, 0.57, 0.74, 0.77, 0.72, 0.52, 0.49)

cort.t1 <- c(1.13, 0.81, 0.77, 0.72, 0.45, 0.9, 0.7, 0.7, 0.98, 0.96, 1.1, 
             0.63, 0.91, 1.1, 0.99, 0.72, 1.11, 1.2, 0.77, 0.91)

# ?t.test # NB the "paired" argument

t.test(x = cort.t0,
       y = cort.t1,
       paired = TRUE)


## wilcoxon
## **Mann-Whitney U-test** ####
diet <- c(3, 3, 1, 2, 2, 2, 2, 0, 2, 2, 1, 2, 3, 1, 1)

diet.bone <- c(5, 6, 1, 2, 3, 5, 1, 7, 5, 1, 2, 2, 5, 2, 4)


# Gaussian assumption
library(car)
hist(diet.bone)

par(mfrow = c(1,1))

# ?wilcox.test
wilcox.test(x = diet, y = diet.bone)
