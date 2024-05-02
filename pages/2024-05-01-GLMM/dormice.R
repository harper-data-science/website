## HEADER ####
## who: Ed H and Jazz W
## what: HARUG dormouse extravaganza
## when 2024-05-01

## CONTENTS ####
## 00 Setup
## 01 Dormouse presence or absence
## 02 Dormouse nest presence
## 03 Variance ground/box/outside
## 04 Misc code
## 05 glmm for torpor state

## 00 Setup ####
source("scripts/libraries.R")
source("scripts/data.R")


## 01 Dormouse presence or absence ####
# Model with counts of presence and absence
glm0 <- glm(cbind(NoBoxesWithDormice,50-NoBoxesWithDormice) ~ 
              MetTmin + MetTmax + MetRainMm + Site, 
             family = binomial(link = "logit"), 
             data = visit)
summary(glm0)
# visreg(glm0)
# plot(glm0)

ggplot(visit, aes(x=MetTmin, y=propWithDormouse)) +
  geom_point(color="red") +
  geom_smooth(method="glm", method.args=list(family="binomial"), 
              fullrange=TRUE, se=F) + 
  ylab("Proportion occupied nest boxes \n per visit") +
  xlab("Minimum Temp (C)") + 
  theme_classic()

ggplot(visit, aes(x=MetTmax, y=propWithDormouse)) +
  geom_point(color="red") +
  geom_smooth(method="glm", method.args=list(family="binomial"), 
              fullrange=TRUE, se=FALSE) + 
  ylab("Proportion occupied nest boxes \n per visit") +
  xlab("Maximum Temp (C)") + 
  theme_classic()

ggplot(visit, aes(x=MetRainMm, y=propWithDormouse)) +
  geom_point(color="red") +
  geom_smooth(method="glm", method.args=list(family="binomial"), 
              fullrange=TRUE, se=FALSE) + 
  ylab("Proportion occupied nest boxes \n per visit") +
  xlab("Rainfall (mm)") + 
  theme_classic()

ggplot(visit, aes(x=Site, y=propWithDormouse)) +
  geom_boxplot(color="blue", outlier.size=0) +
  geom_jitter(width = .1, color="red") + 
  ylab("Proportion occupied nest boxes \n per visit") + 
  theme_classic()

## 02 Dormouse nest presence ####
# Model with counts of presence and absence
glm1 <- glm(cbind(NoBoxesWithNests,50-NoBoxesWithNests) ~ 
              MetTmin + MetTmax + MetRainMm + Site, 
            family = binomial(link = "logit"), 
            data = visit)
summary(glm1)

ggplot(visit, aes(x=MetTmin, y=propWithNest)) +
  geom_point(color="red") +
  geom_smooth(method="glm", method.args=list(family="binomial"), 
              fullrange=TRUE, se=F) + 
  ylab("Proportion nests per nest box \n per visit") +
  xlab("Minimum Temp (C)") + 
  theme_classic()

ggplot(visit, aes(x=MetTmax, y=propWithNest)) +
  geom_point(color="red") +
  geom_smooth(method="glm", method.args=list(family="binomial"), 
              fullrange=TRUE, se=F) + 
  ylab("Proportion nests per nest box \n per visit") +
  xlab("Maximum Temp (C)") + 
  theme_classic()

ggplot(visit, aes(x=MetRainMm, y=propWithNest)) +
  geom_point(color="red") +
  geom_smooth(method="glm", method.args=list(family="binomial"), 
              fullrange=TRUE, se=FALSE) + 
  ylab("Proportion nests per nest box \n per visit") +
  xlab("Rainfall (mm)") + 
  theme_classic()

ggplot(visit, aes(x=Site, y=propWithNest)) +
  geom_boxplot(color="blue", outlier.size=0) +
  geom_jitter(width = .1, color="red") + 
  ylab("Proportion nests per nest box \n per visit") + 
  theme_classic()

## 03 Variance ground/box/outside ####

# pairs(iButton[,-1])

iButLong <- data.frame(temp = c(iButton$iButTavgGround,
                                iButton$iButTavgInternal,
                                iButton$iButTavgExternal),
                       source = rep(c("ground", "internal", "external"), each=383))

ggplot(iButLong, aes(x=source, y=temp)) +
  geom_boxplot() + 
  geom_jitter(width=.1) +
  xlab("iButton location relative to nest box") +
  ylab("Temperature (C)") + 
  theme_classic()

aggregate(iButLong$temp, by=list(iButLong$source),
          FUN = function(x) c(mean=mean(x), sd=sd(x)))

kruskal.test(temp~source, data=iButLong)
anova(lm(temp~source, data=iButLong))

leveneTest(temp~source, data=iButLong)

var.test(iButton$iButTavgGround,
         iButton$iButTavgInternal)
var.test(iButton$iButTavgGround,
         iButton$iButTavgExternal)
var.test(iButton$iButTavgExternal,
         iButton$iButTavgInternal)

## 04 Misc code ####

# Dormouse count versus time
ggplot(visit, aes(x = Date, 
                  y = NoDormice, 
                  group = Site, 
                  color = Site)) +  
  geom_line() +
  labs(x = "Date", 
       y = "Number of Dormice", 
       color = "Site") +
  theme_classic()

## 05 glmm for torpor state ####
glmm0 <- glmer(torporfac ~ 
              MetTmin + MetTmax + MetRainMm + Site + (1|Date), 
            family = binomial(link = "logit"), 
            data = dormouse)
summary(glmm0)

# visreg(glmm0, "MetTmin")

ggplot(dormouse, aes(x=torporfac, y=MetTmin)) +
  geom_boxplot(color="blue", outlier.size=0) +
  geom_jitter(width = .1, color="red") + 
  ylab("Minimum Temp (C)") + 
  xlab("Torpor status") +
  theme_classic()

  ggplot(dormouse, aes(x=torporfac, y=MetTmax)) +
    geom_boxplot(color="blue", outlier.size=0) +
    geom_jitter(width = .1, color="red") + 
    ylab("Maximum Temp (C)") + 
    xlab("Torpor status") +
  theme_classic()
  
  ggplot(dormouse, aes(x=torporfac, y=MetRainMm)) +
    geom_boxplot(color="blue", outlier.size=0) +
    geom_jitter(width = .1, color="red") + 
    ylab("Rainfall (mm)") + 
    xlab("Torpor status") +
  theme_classic()
  
x <- barplot(prop.table(matrix(table(dormouse$torporfac, dormouse$Site), 
                          nrow=2, byrow=T),2),
        ylab="Proportion active or in torpor",
        xlab = "Site")
mtext(text = c("Coleridge", "Mardon Mire"), side = 1, 
      line = 1, at = x)
legend(x = .6, y=1, legend = c("torpid","active"), 
       col = c("gray80","gray20" ), bty="n", pch = 15)
  
 


