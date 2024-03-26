## HEADER ####
## who: EH
## what: Bootcamp 8 code
## when last edited: 2024-03-20

## CONTENTS ####

## 2.1 ####
# Try this:

# Adult domestic cats weight approximately 4 Kg on average
# with a standard deviation of approximately +/1 0.5 Kg

# Let's simulate some fake weight data for 10,000 cats
help(rnorm) # A very useful function
help(set.seed) # If we use this, we can replicate "random data"

help(hist)

set.seed(42)
cats <- rnorm(n = 10000,  # 10,000 cats
              mean = 4,   
              sd = 0.5)

cats[1:10] # The first 10 cats

hist(x = cats,
     xlab = "Cat weight (Kg)")

# Try this:
# simulation of samples

help(sample) # randomly sample the cats vector
help(vector) # Initialize a variable to hold our sample means

# We will do a "for loop" with for()


mymeans <- vector(mode = "numeric",
                  length = 100)
mymeans # Empty

for(i in 1:100){
  mysample <- sample(x = cats, # Takes a random sample
                     size = 30)
  
  mymeans[i] <- mean(mysample) # stores sample mean in ith vector address
}

mymeans # Our samples


hist(x = mymeans, breaks = 10,
     xlab = "Mean of samples",
     main = "100 cat weight samples (n = 30/sample)")
abline(v = mean(mymeans), col = "red", lty = 2, lwd = 2)


## 3.3 ####

## Gaussian variations ####
# Try this:

# 4 means
(meanvec <- c(10, 7, 10, 10))
# 4 standard deviations
(sdvec <- c(2, 2, 1, 3))

# Make a baseline plot
x <- seq(0,20, by = .1)

# Probabilities for our first mean and sd
y1 <- dnorm(x = x, 
            mean = meanvec[1],
            sd = sdvec[1])

# Baseline plot of 1st mean and sd
plot(x = x, y = y1, ylim = c(0, .4),
     col = "goldenrod",
     lwd = 2, type = "l",
     main = "Gaussian fun 
     \n mean -> curve position; sd -> shape",
     ylab = "Density",
     xlab = "(Arbitrary) Measure")

# Make distribution lines
mycol <- c("red", "blue", "green")
for(i in 2:4){
  y <- dnorm(x = x, 
             mean = meanvec[i],
             sd = sdvec[i])
  lines(x = x, y = y, 
        col = mycol[i-1],
        lwd = 2, type = "l")
}

# Add a legend
legend(title = "mean (sd)",
       legend = c("10 (2)", "  7 (2)", 
                  "10 (1)", "10 (3)"),
       lty = c(1,1,1,1), lwd = 2,
       col = c("goldenrod", "red", "blue", "green"),
       x = 15, y = .35)


## 3.4 ####

## q-q- Gaussian ####

# Try This:

library(car) # Might need to install {car}

# Set graph output to 2 x 2 grid
# (we will set it back to 1 x 1 later)
par(mfrow = c(2,2))  

# Small Gaussian sample
set.seed(42)
sm.samp <- rnorm(n = 10, 
                 mean = 10, sd = 2)

qqPlot(x = sm.samp, 
       dist = "norm", # C'mon guys, Gaussian ain't normal!
       main = "Small sample Gaussian")

par(mfrow = c(1,1))  

# Large Gaussian sample
set.seed(42)
lg.samp <- rnorm(n = 1000, 
                 mean = 10, sd = 2)

qqPlot(x = lg.samp, 
       dist = "norm", 
       main = "Large sample Gaussian")


# Non- Gaussian sample
set.seed(42)
uni <- runif(n = 1000, 
             min = 3, max = 17)
hist(uni)
qqPlot(x = uni, 
       dist = "norm", 
       main = "Big deviation at top")


## 4.2 ####

rpois(1000, 4)


# Try this:

# E.g. (simulated) Number of ewes giving birth to triplets
# The counts were made in one year 1n 100 similar flocks (<20 ewes each)

set.seed(42)
mypois <- rpois(n = 100, lambda = 3)

hist(mypois,
     main = "Ewes with triplets",
     xlab = "Count of Triplets")


# Try this:
# 3 lambdas
(lambda <- c(1, 3, 5))

# Make a baseline plot
x <- seq(0, 15, by = 1)

# Probabilities for our first lambda
y1 <- dpois(x = x, 
            lambda = lambda[1])

# Baseline plot Pois
plot(x = x, y = y1, ylim = c(0, .4),
     col = "goldenrod",
     lwd = 2, type = "b",
     main = "Poisson fun",
     ylab = "Density",
     xlab = "(Arbitrary) Count")

# Make distribution lines
mycol <- c("red", "blue")
for(i in 1:2){
  y <- dpois(x = x, 
             lambda = lambda[i+1])
  lines(x = x, y = y, 
        col = mycol[i],
        lwd = 2, type = "b")
}

# Add a legend
legend(title = "lambda",
       legend = c("1", "3", "5"),
       lty = c(1,1,1,1), lwd = 2,
       col = c("goldenrod", "red", "blue"),
       x = 8, y = .35)

## 5.2 ####
# Try this:

# dormouse presence:
set.seed(42)
(my_occ <- rbinom(n = 50, # Number of "experiments", here nestboxes checked
                  size = 1, # Number of checks, one check per nestbox
                  prob = .3)) # Probability of presence

mosaicplot(table(my_occ), col = c('red','goldenrod'))


# Try this:
# Flip a fair coin:
set.seed(42)
(coin <- rbinom(n = 1000, # Number of "experiments", 20 people flipping a coin
                size = 10, # Number of coin flips landing on "heads" out of 10 flips per person
                prob = .5)) # Probability of "heads"


mosaicplot(table(coin), col = 1:unique(coin))
