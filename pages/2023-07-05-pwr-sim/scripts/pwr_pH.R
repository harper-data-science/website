## Header ####
## who: Ed H for Liam S
## what: power simulation for 4x4 Latin Sq experiment, n=8
## when: last edited 2023-06-21
# 
# The goal here is to simulate power for several sample
# size scenarios.  A balanced Latin sq. design would
# typically have an equal sample size for each cohort
# (e.g. n=8, 2 per cohort with 4 cohorts in a 4x4 design).
# However only 7 or 6 subjects available.  The question
# is how does this affect power?  Assume the analysis
# will be linear mixed effects model (as an alternative to
# ANOVA for fully balanced design), with Cow as a random
# effect and Diet treatment (4 levels) as a fixed effects.

library(openxlsx)
library(readxl)

library(lme4) # v. 1.1-27.1
library(lmerTest)
library(broom.mixed) # v. 0.2.7
library(purrr) # v. 0.3.4
library(emmeans)
library(ggeffects)
library(lubridate)
library(tidyverse)

library(ggplot2) # v. 3.3.5
library(visreg)

## pH POWER sim ####

ncow = c(8,7,6)
ndiet = 4
mu = 6.12       # overall mean of dep var > mean(data$pH)
b0 = 6.090      # diet a (control) mean intercept ggpredict(myfit, 'Diet')
b3 = .7         # diet d marginal
b1 = b3/3       # diet b marginal
b2 = b3/3*2     # diet c marginal
# 
# sdcow = 0.04   # cow std dev > Cow in lmer Random eff summary
# sd = c(0.29-.29*.1, 0.29,0.29+.29*.1)      # obs .29 +-10% std dev > residuals in lmer Random eff summary
# 
# i = 1
# j = 1
# lme_fun = function(ncow1 = NULL, 
#                    ndiet1 = ndiet, 
#                    mu1 = mu, 
#                    sigma_s = sdcow, 
#                    sigma = NULL) {
#   
#   cow = rep(names[1:ncow1], each = ndiet1)
#   diet = rep(letters[1:ndiet1], ncow1 )
#   dieteff = rnorm(ncow1*ndiet, 0, sigma)
#   coweff = rnorm(ncow1, 0, sigma_s)
#   coweff = rep(coweff, each = ndiet1)
#   
#   dat = data.frame(cow, coweff, diet, dieteff)
#   diet1 = rep( c(0,1,0,0), ncow1)
#   diet2 = rep( c(0,0,1,0), ncow1)
#   diet3 = rep( c(0,0,0,1), ncow1)
#   dat$pH = b0 + b1*diet1 + b2*diet2 + b2*diet3 + coweff + dieteff
#   
#   anova(lmer(pH ~ diet + (1|cow), data = dat ))$'Pr(>F)'
# }
# 
# lme_fun(ncow1 = ncow[2],
#         sigma = sd[1])
# 
# 
# ncow = c(8,7,6)
# # obs .29 +-10% std dev > residuals in lmer Random eff summary
# sd = c(0.29-.29*.1, 
#        0.29, 
#        0.29+.29*.1) 
# 
# set.seed(16)
# sims = replicate(500, lme_fun(ncow1 = ncow[3],
#                               sigma = sd[3]), 
#                  simplify = FALSE )
# 
# 
# ps <- data.frame(p_val = unlist(sims))
# 
# ggplot(ps, aes(x = p_val) ) +
#   geom_density(fill = "blue", alpha = .25) +
#   geom_vline(xintercept = .05)
# 
# tab <- table(ps$p_val < 0.05)
# (tab[2]/(sum(tab)))

## file: pH POWER ####
pwrdat <- data.frame(
  power = c(.93, .87, .802,
            .864, .77, .73,
            .83, .7, .674),
  samplesize = c(8, 8, 8, 7, 7, 7, 6, 6, 6),
  sd = c(0.261, 0.290, 0.319,
         0.261, 0.290, 0.319,
         0.261, 0.290, 0.319),
  var = factor(rep(c('low', 'med','high'), 3), 
               levels = c('low', 'med','high'))
)

# png("figs/power_pH.png", width = 1000, height = 1000, res = 150)
# 
# ggplot(pwrdat, aes(x=factor(samplesize), 
#                    y = power, 
#                    color = factor(var), group = factor(var))) +
#   geom_point() + 
#   geom_point(data = pwrdat %>% filter(samplesize == 7 & var == 'med'),
#              pch = 21, size = 4, lwd = 2,
#              col = 'purple',
#              fill = 'purple',
#              alpha = 0.5) +
#   geom_line() +
#   xlab('Sample size') + 
#   ylab('Power') +
#   labs(color = "Variance\nestimate\n(+-10%)") + 
#   ylim(0.6, 1.0) +
#   ggtitle('Power analysis for pH') +
#   geom_hline(yintercept=.8, linetype = 'dashed', col = 'red') +
#   theme_minimal() +
#   theme(text = element_text(size = 16))
# 
# dev.off()

ggplot(pwrdat, aes(x=factor(samplesize), 
                   y = power, 
                   color = var, group = var)) +
  geom_point() + 
  geom_point(data = pwrdat %>% filter(samplesize == 7 & var == 'med'),
             pch = 21, size = 4,
             col = 'purple',
             fill = 'purple',
             alpha = 0.5) +
  geom_line() +
  xlab('Sample size') + 
  ylab('Power') +
  labs(color = "Variance\nestimate\n(+-10%)") + 
  ylim(0.6, 1.0) +
  ggtitle('Power analysis for pH') +
  geom_hline(yintercept=.8, linetype = 'dashed', col = 'red') +
  theme_minimal() +
  theme(text = element_text(size = 16))

## Parameters: Ph ####

# Get data and parameter estimates for pwr sim

# data <- read_excel('data/pH.xlsx')

# example data has only 3 levels, the experiment has 4
## ASSUME the expected range is same as observed here
# aggregate(data$pH~data$Diet, FUN = mean)
# sd(data$pH)
# 
# data$Postfeeding_conc <- factor(data$Postfeeding_conc)
# data$Period <- factor(data$Period)
# data$Time <- factor(format(data$Time, format='%H:%M'))


# interaction.plot(x.factor = data$Time, 
#                  trace.factor = data$Diet,
#                  response = data$pH)

### pH Plot pilot data ####
# interaction.plot(x.factor = data$Diet,
#                  trace.factor = data$Cow,
#                  response = data$pH,
#                  #ylim = c(5.7,6.5),
#                  ylab = 'Mean pH',
#                  xlab = 'Diet treatment',
#                  main = 'pH pilot data',
#                  trace.label = 'Cow ID')

### file: pH Plot pilot data ####
# png("figs/pilot_pH.png", width = 1000, height = 1000, res = 150)
# # 2. Create the plot
# interaction.plot(x.factor = data$Diet, 
#                  trace.factor = data$Cow,
#                  response = data$pH,
#                  #ylim = c(5.7,6.5),
#                  ylab = 'Mean pH',
#                  xlab = 'Diet treatment',
#                  main = 'pH pilot data',
#                  trace.label = 'Cow ID')
# # 3. Close the file
# dev.off()

### pH get mopdel parameters ####
### mu ####
# mean(data$pH)
# 
# myfit <- lmer(pH ~ Diet + (1|Cow), data)
# summary(myfit)
# anova(myfit)
### Treatment range ####
# ggpredict(myfit)
# 
# visreg(myfit, 'Diet')
# visreg(myfit, 'Time')

