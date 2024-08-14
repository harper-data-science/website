library(openxlsx)
library(lmerTest)
library(visreg)

## About the data ####
data <- read.xlsx("resid.xlsx")

## EDA ####
boxplot(LivCU~Farm, data = data)
boxplot(LivCU~Treatment, data = data)

boxplot(log(LivCU)~Farm, data = data)
boxplot(log(LivCU)~Treatment, data = data)


d1 <- data[complete.cases(data),]
interaction.plot(x.factor = d1$Treatment,
                 trace.factor = d1$Farm,
                 response = log(d1$LivCU))

hist(data$LivCU)
hist(log(data$LivCU))


## Analysis ####
# remind of the principle assumptions of linear models... (4 of them)
lme0 <- lmer(log(LivCU) ~ Farm + Treatment + (1|SheepID),
             data = data)
summary(lme0)
anova(lme0)

plot(lme0)

hist(residuals(lme0))
shapiro.test(residuals(lme0))

## take2 ####
data2 <- data[1:135,1:4]
names(data2) <- c("SheepID", "Farm", "Treatment", "CU1")
data2$CU2 <- data[136:270,"LivCU"]

data2$diff <- data2$CU2-data2$CU1
table(data2$diff < 0)

lm0 <- lm(diff ~ Farm + Treatment, data = data2)
summary(lm0)
anova(lm0)
hist(residuals((lm0)))
shapiro.test(residuals(lm0))

boxplot(diff~Farm, data = data2)
boxplot(diff~Treatment, data = data2)
d2 <- data2[complete.cases(data2),]

interaction.plot(x.factor = d2$Treatment,
                 trace.factor = d2$Farm,
                 response = d2$diff)


visreg(lm0, "Treatment", by = "Farm")


#####
lme1 <- lmer(sqrt(LivCU) ~ Farm + Treatment + (1|SheepID),
             data = data)
shapiro.test(residuals(lme1))
hist(residuals(lme1))
