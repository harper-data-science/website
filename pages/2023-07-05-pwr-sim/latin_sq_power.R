library(openxlsx)
library(lme4)
library(lmerTest)
library(visreg)
library(emmeans)
library(ggeffects)
library(readxl)
library(lubridate)

data <- read_excel('data/dummy_dat.xlsx')

aggregate(data$pH~data$Diet, FUN = mean)
sd(data$pH)

data$Postfeeding_conc <- factor(data$Postfeeding_conc)
data$Period <- factor(data$Period)
data$Time <- factor(format(data$Time, format='%H:%M'))


interaction.plot(x.factor = data$Time, 
                 trace.factor = data$Diet,
                 response = data$pH)

interaction.plot(x.factor = data$Diet, 
                 trace.factor = data$Cow,
                 response = data$pH)

mean(data$pH)
aggregate(data$pH, list)


myfit <- lm(pH ~ Diet + Postfeeding_conc, data)
anova(myfit)

data1 <- data[data$Cow !='Freya',]

library(lme4)
myfit <- lmer(pH ~ Diet * Time + (1|Cow), data)
summary(myfit)
anova(myfit)

visreg(myfit, 'Diet')
visreg(myfit, 'Time')

interaction.plot(x.factor = data$Diet, 
                 trace.factor = data$Time,
                 response = data$pH)

ggpredict(myfit, 'Diet')


sample(unique(data$Cow), 5)

library(lattice)
xyplot(pH ~ factor(Diet) + Time|Cow, groups = Cow, data = data, 
       pch=19, lwd=2, type=c('p','r'))

