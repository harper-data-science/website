# Simulation for mixed effects models

library(openxlsx)
library(readxl)

library(lme4) # v. 1.1-27.1
library(lmerTest)
library(broom.mixed) # v. 0.2.7
library(purrr) # v. 0.3.4
library(emmeans)
library(ggeffects)
library(lubridate)

library(ggplot2) # v. 3.3.5
library(visreg)

ncow = 8    # n cows
ndiet = 4   # n diets
ncohort = 4 # n cohorts in Latin Sq (4x4)
mu = 6.12   # overall mean of dep var > mean(data$pH)
sdcow = 0.04# cow std dev > Cow in lmer Random eff summary
sd = 0.29   # obs std dev > residuals in lmer Random eff summary
names <- c('Luna','Scarlett','Mila','Hazel',
           'Nova','Layla','Ellie','Isla')

# first assume no cohort effect
( cow = rep(names[1:ncow], each = ndiet) )
( diet = rep(letters[1:ndiet], ncow ) )
( coweff = rnorm(ncow, 0, sdcow) )
( coweff = rep(coweff, each = ndiet) )
( dieteff = rnorm(ncow*ndiet, 0, sd) )

( dat = data.frame(cow, coweff, diet, dieteff) )

( dat$pH = with(dat, mu + coweff + dieteff ) )

## random effect only ####
fit1 = lmer(pH ~ 1 + (1|cow), data = dat)
fit1

twolevel_fun = function(ncow = 5, ndiet = 4, mu = 10, sigma_s = 2, sigma = 1) {
  coweff = rep( rnorm(ncow, 0, sigma_s), each = ndiet)
  cow = rep(LETTERS[1:ncow], each = ndiet)
  dieteff = rnorm(ncow*ndiet, 0, sigma)
  resp = mu + coweff + dieteff
  dat = data.frame(cow, resp)
  lmer(resp ~ 1 + (1|cow), data = dat)
}

set.seed(16)
twolevel_fun()

sims = replicate(100, twolevel_fun(), simplify = FALSE )
sims[[100]]

tidy(fit1)

tidy(fit1, effects = "fixed")

tidy(fit1, effects = "ran_pars", scales = "vcov")

suppressPackageStartupMessages( library(dplyr) ) # v. 1.0.7

cow_sims = c(5, 20, 100) %>%
  set_names() %>%
  map(~replicate(1000, twolevel_fun(ncow = .x) ) )

cow_vars = cow_sims %>%
  modify_depth(2, ~tidy(.x, effects = "ran_pars", scales = "vcov") ) %>%
  map_dfr(bind_rows, .id = "cow_num") %>%
  filter(group == "cow")
head(cow_vars)

ggplot(cow_vars, aes(x = estimate) ) +
  geom_density(fill = "blue", alpha = .25) +
  facet_wrap(~cow_num) +
  geom_vline(xintercept = 4)

cow_vars = mutate(cow_vars, cow_num = forcats::fct_inorder(cow_num) )

add_prefix = function(string) {
  paste("Number cows:", string, sep = " ")
}

groupmed = cow_vars %>%
  group_by(cow_num) %>%
  summarise(mvar = median(estimate) )

ggplot(cow_vars, aes(x = estimate) ) + 
  geom_density(fill = "blue", alpha = .25) +
  facet_wrap(~cow_num, labeller = as_labeller(add_prefix) ) +
  geom_vline(aes(xintercept = 4, linetype = "True variance"), size = .5 ) +
  geom_vline(data = groupmed, aes(xintercept = mvar, linetype = "Median variance"),
             size = .5) +
  theme_bw(base_size = 14) +
  scale_linetype_manual(name = "", values = c(2, 1) ) +
  theme(legend.position = "bottom",
        legend.key.width = unit(.1, "cm") ) +
  labs(x = "Estimated Variance", y = NULL)

cow_vars %>%
  group_by(cow_num) %>%
  summarise_at("estimate", 
               list(min = min, mean = mean, med = median, max = max) )

cow_vars %>%
  group_by(cow_num) %>%
  summarise(mean(estimate < 4) )

## mixed effects ####
names <- c('Luna','Scarlett','Mila','Hazel',
           'Nova','Layla','Ellie','Isla')

ncow = 8
ndiet = 4
ncohort = 4 # n cohorts in Latin Sq (4x4)
mu = 6.12   # overall mean of dep var > mean(data$pH)

b0 = 6.090     # diet a (control) mean intercept ggpredict(myfit, 'Diet')
b1 = .023     # diet b marginal
b2 = .046     # diet c marginal
b3 = .7     # diet d marginal
sdcow = 0.04   # cow std dev > Cow in lmer Random eff summary
sd = 0.29      # obs std dev > residuals in lmer Random eff summary

( cow = rep(names[1:ncow], each = ndiet) )
( diet = rep(letters[1:ndiet], ncow ) )
( dieteff = rnorm(ncow*ndiet, 0, sd) )
( coweff = rnorm(ncow, 0, sdcow) )
( coweff = rep(coweff, each = ndiet) )

( dat = data.frame(cow, coweff, diet, dieteff) )

( diet1 = rep( c(0,1,0,0), ncow) )
( diet2 = rep( c(0,0,1,0), ncow) )
( diet3 = rep( c(0,0,0,1), ncow) )
( dat$pH = b0 + b1*diet1 + b2*diet2 + b2*diet3 + coweff + dieteff )


(lme0 <- lmer(pH ~ diet + (1|cow), data = dat ))

interaction.plot(x.factor = dat$diet, 
                 trace.factor = dat$cow,
                 response = dat$pH)
points(aggregate(dat$pH~dat$diet, FUN = mean), add = T, 
       col='red', pch=16, cex=1.5)


ncow = c(8,7,6)
ndiet = 4
mu = 6.12       # overall mean of dep var > mean(data$pH)
b0 = 6.090      # diet a (control) mean intercept ggpredict(myfit, 'Diet')
b3 = .7         # diet d marginal
b1 = b3/3       # diet b marginal
b2 = b3/3*2     # diet c marginal
    
sdcow = 0.04   # cow std dev > Cow in lmer Random eff summary
sd = c(0.29-.29*.1, 0.29,0.29+.29*.1)      # obs .29 +-10% std dev > residuals in lmer Random eff summary

i = 1
j = 1
lme_fun = function(ncow1 = NULL, 
                        ndiet1 = ndiet, 
                        mu1 = mu, 
                        sigma_s = sdcow, 
                        sigma = NULL) {
  
  cow = rep(names[1:ncow1], each = ndiet1)
  diet = rep(letters[1:ndiet1], ncow1 )
  dieteff = rnorm(ncow1*ndiet, 0, sigma)
  coweff = rnorm(ncow1, 0, sigma_s)
  coweff = rep(coweff, each = ndiet1)
  
  dat = data.frame(cow, coweff, diet, dieteff)
  diet1 = rep( c(0,1,0,0), ncow1)
  diet2 = rep( c(0,0,1,0), ncow1)
  diet3 = rep( c(0,0,0,1), ncow1)
  dat$pH = b0 + b1*diet1 + b2*diet2 + b2*diet3 + coweff + dieteff

  anova(lmer(pH ~ diet + (1|cow), data = dat ))$'Pr(>F)'
  }

lme_fun(ncow1 = ncow[2],
        sigma = sd[1])


ncow = c(8,7,6)
# obs .29 +-10% std dev > residuals in lmer Random eff summary
sd = c(0.29-.29*.1, 
       0.29, 
       0.29+.29*.1) 

set.seed(16)
sims = replicate(500, lme_fun(ncow1 = ncow[3],
                              sigma = sd[3]), 
                 simplify = FALSE )


ps <- data.frame(p_val = unlist(sims))

ggplot(ps, aes(x = p_val) ) +
  geom_density(fill = "blue", alpha = .25) +
  geom_vline(xintercept = .05)

tab <- table(ps$p_val < 0.05)
(tab[2]/(sum(tab)))

pwrdat <- data.frame(
  power = c(.93, .87, .802,
             .864, .77, .73,
             .83, .7, .674),
  samplesize = c(8, 8, 8, 7, 7, 7, 6, 6, 6),
  sd = c(0.261, 0.290, 0.319,
          0.261, 0.290, 0.319,
          0.261, 0.290, 0.319),
  var = factor(rep(c('low', 'med','high'), 3), levels = c('low', 'med','high'))
)

ggplot(pwrdat, aes(x=factor(samplesize), 
                   y = power, 
                   color = factor(var), group = factor(var))) +
  geom_point() + 
  geom_point(data=pwrdat %>% filter(samplesize == 7 & var == 'med'),
             pch = 21, size = 4, lwd = 2,
             col = 'purple',
             fill = 'purple',
             alpha = 0.5) +
  geom_line() +
  xlab('Sample size') + 
  ylab('Power') +
  labs(color = "Variance\n(+-10%)") + 
  ylim(0.6, 1.0) +
  ggtitle('Power analysis for pH') +
  geom_hline(yintercept=.8, linetype = 'dashed', col = 'red') +
    theme_minimal()


# https://aosmith.rbind.io/2018/01/09/simulate-simulate-part1/
# https://aosmith.rbind.io/2018/04/23/simulate-simulate-part-2/