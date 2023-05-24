## HEADER ####
## who: Ed
## what: milkers
## when: recent

## CONTENTS ####
## 00 Setup
## 01 Graphs
## 02 Stats

## 00 Setup ####
library(openxlsx)
library(lme4)
library(lmerTest)
library(visreg)
library(tidyverse)
library(ggplot2)


data <- read.xlsx('milk_yield.xlsx')

dataL <- data %>%
  pivot_longer(
    cols = starts_with('Week_'),   # Columns to be transformed
    names_to = 'Week',   # New column name for the 'Week' variable
    values_to = 'Yield',  # New column name for the values
    names_prefix = 'Week_',  # Remove the 'Week_' prefix from the week numbers
    names_transform = list(Week = as.integer)  # Convert week numbers to integer
  )

dataL$Week <- as.factor(dataL$Week)
dataL$Treat <- as.factor(dataL$Treat)
dataL$parity <- as.factor(dataL$parity)
dataL$Allocation <- as.factor(dataL$Allocation)

rm(data)


## 01 Graphs ####

## Treatment
ggplot(dataL[dataL$Week == 0,], aes(x = Week, y = Yield, color = Treat)) +
  geom_boxplot() +
  labs(x = "Week", y = "Weekly avg daily yield (l)", title = "Weekly Yield of Each Cow") +
  theme_classic() +
  theme(legend.title = element_blank())

## Yield per Cow
ggplot(dataL, aes(x = Week, y = Yield, group = Cow)) +
  geom_line() +
  labs(x = "Week", y = "Weekly avg daily yield (l)", title = "Weekly Yield of Each Cow") +
  theme_classic() +
  theme(legend.title = element_blank())

## Treatment
ggplot(dataL, aes(x = Week, y = Yield, group = Cow, color = Treat)) +
  geom_line() +
  labs(x = "Week", y = "Weekly avg daily yield (l)", 
       title = "Weekly Yield of Each Cow") +
  theme_classic() +
  theme(legend.title = element_blank())

## Parity
ggplot(dataL, aes(x = Week, y = Yield, group = Cow, color = parity)) +
  geom_line() +
  labs(x = "Week", y = "Weekly avg daily yield (l)", 
       title = "Weekly Yield of Each Cow") +
  theme_classic() +
  theme(legend.title = element_blank())

## Allocation
ggplot(dataL, aes(x = Week, y = Yield, group = Cow, color = Allocation)) +
  geom_line() +
  labs(x = "Week", y = "Weekly avg daily yield (l)", 
       title = "Weekly Yield of Each Cow") +
  theme_classic() +
  theme(legend.title = element_blank())

## 02 Stats ####

lme0 <- lmer(Yield ~ Treat + Week + parity + (1|Cow),
     data = dataL, REML = TRUE)

#summary(lme0)
VarCorr(lme0)
summary(lme0)
anova(lme0)
visreg(lme0, "Treat")
visreg(lme0, "parity")
visreg(lme0, "Week")

lme0 <- lmer(Yield ~ Treat + Week + parity + (1|Cow),
             data = dataL, REML = F)
lme1 <- lmer(Yield ~ Treat + Week + (1|Cow),
             data = dataL, REML = F)

AIC(lme0, lme1)
anova(lme0, lme1)


interaction.plot(x.factor = dataL$Allocation,
                 trace.factor = dataL$Treat,
                 response = dataL$Yield)


# Calculate mean yield per week for each treatment
mean_yield <- dataL %>%
  group_by(Week, Treat) %>%
  summarise(Mean_Yield = mean(Yield, na.rm = TRUE))

# Create line graph
ggplot(mean_yield, aes(x = Week, y = Mean_Yield, color = Treat)) +
  geom_line(aes(group = Treat)) +
  geom_point() +
  labs(x = "Week", y = "Mean Milk Yield", title = "Mean Milk Yield Over Different Weeks by Treatment") +
  theme_minimal() +
  scale_color_discrete(name = "Treatment")

