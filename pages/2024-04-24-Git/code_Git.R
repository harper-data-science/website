## HEADER ####
## who:
## what:
## when:

## 3.1 ####
# Try this
# Data in "wide format" 
A <- c(687, 691, 793, 675, 700, 753, 704, 717)
B <- c(618, 680, 592, 683, 631, 691, 694, 732)
C <- c(618, 687, 763, 747, 687, 737, 731, 603)
D <- c(600, 657, 669, 606, 718, 693, 669, 648)
E <- c(717, 658, 674, 611, 678, 788, 650, 690)

head(chicken.wide <- data.frame(A, B, C, D, E))

# Data in "long format"  ####
# The hard way
weight <- c(A,B,C,D,E)

(sire <- c(rep("A", 8),
          rep("B", 8),
          rep("C", 8),
          rep("D", 8),
          rep("E", 8) ))



head(data.frame(weight, sire))

tail(data.frame(weight, sire))

# The "programm-ey" way
weight1 <- c(A,B,C,D,E)

sire1 <- vector(mode = "character", length = 40)

for(i in 1:5) { 
  sire1[(8*i-8)+c(1:8)] <- rep(LETTERS[i], 8) 
  }

head(data.frame(weight1, sire1))

library(reshape2) # For melt()

new.long <- melt(chicken.wide)

names(new.long)

names(new.long) <- c('Sire', 'Weight')
names(new.long)

## 4 ####
## **Assumptions** ####

## - Gaussian residuals ####
# Make the model object with aov()

# ?aov
m1 <- aov(formula = Weight ~ Sire, 
          data = new.long)

# Graph to examine Gaussian assumption of residuals
# NB we use rstandard()
par(mfrow = c(1,2))
hist(rstandard(m1),
     main = "Gaussian?")

# Look at residuals with qqPlot()
library(car) # For qqPlot()

qqPlot(x = m1,
       main = "Gaussian?")
par(mfrow=c(1,1))


## 4.1 ####
shapiro.test(rstandard(m1))

## 4.2 ####

# Plot for homoscedasticity check
plot(formula = rstandard(m1) ~ fitted(m1),
     ylab = "m1: residuals",
     xlab = "m1: fitted values",
     main = "Spread similar across x?")

abline(h = 0,
       lty = 2, lwd = 2, col = "red")

# Make the mean residual y points (just to check)
y1 <- aggregate(rstandard(m1), 
                by = list(new.long$Sire), 
                FUN = mean)[,2]

# Make the x unique fitted values (just to check)
x1 <- unique(round(fitted(m1), 6))

points(x = x1, y = y1, 
       pch = 16, cex = 1.2, col = "blue")

plot(m1)


# NHST to examine  assumption of homoscedasticity
# (homoscedasticiyy good, heteroscedasticity bad)

bartlett.test(formula = weight~sire, data = new.long)

## 5 #### 
## basic boxplot ####

# It always pays to make a nice plot

# Do you think sire affects offspring weight?
boxplot(Weight ~ Sire, 
        data = new.long, 
        main = "Is this plot good enough?") 

## **Make a better graph** ####

boxplot(Weight ~ Sire, data = new.long,
        ylab = "Weight (g)",
        xlab = "Sire",
        main = "Effect of Sire on 8-wk weight",
        cex = 0) # Get rid of the outlier dot (we will draw it back)

# Make horizontal line for grand mean
abline(h = mean(new.long$Weight), 
       lty = 2, lwd = 2, col = "red") # Mere vanity

# Draw on raw data
set.seed(42)
points(x = jitter(rep(1:5, each = 8), amount = .1),
       y = new.long$Weight,
       pch = 16, cex = .8, col = "blue") # Mere vanity

## 6.2 ####
## Perform 1-way ANOVA ####
# Try this

# NB if the factor is a character, it "should" be coerced to a factor
# by R, "the passive aggressive butler"
# If in doubt, explicitly make the vector class == factor()
m1 <- aov(formula = Weight ~ Sire, 
          data = new.long)

anova(m1)
summary(m1)

# post hoc test

TukeyHSD(m1)

