##HEADER####
##Who: Hayden Tempest & Ed
##What: WAA and earwig numbers in 11 orchards
##Created: <2022-10-19>
##Last modified: <2022-10-19>

##CONTENTS####
## 00 Setup
## 01 Messing around with model fitting

## 00 Setup

# libraries
library(openxlsx)
library(lme4)
library(lmerTest)
library(lmtest)
library(visreg)
library(car)
library(dplyr)
library(MASS)
library(ggplot2)
library(randomForest)
library(lme4)
library(effects)


# set the working directory
setwd(r'(C:\Users\hayden.tempest\Desktop\Hayden Tempest\Harper Adams PhD\Second year report)')


# data

Data <-read.xlsx('Surveys_1_&_2.xlsx')

Data$Orchard_name <- factor(Data$Orchard_name)
Data$Geographic_area <- factor(Data$Geographic_area)
Data$Org_or_Con <- factor(Data$Org_or_Con)
Data$Rows_per_bed <- factor(Data$Rows_per_bed)
Data$Productivity_2021 <- as.numeric(Data$Productivity_2021)
Data$Grower <- factor(Data$Grower)
Data$Survey <- factor(Data$Survey)
Data$Date <- as.Date(Data$Date)
Data$Colony_1_number <- as.numeric(Data$Colony_1_number)
Data$Colony_2_number <- as.numeric(Data$Colony_2_number)
Data$Colony_3_number <- as.numeric(Data$Colony_3_number)
Data$GAA_presence_absence <- as.factor(Data$GAA_presence_absence)
Data$GAA_number <- as.numeric(Data$GAA_number)
Data$Moss_score <- factor(Data$Moss_score)
Data$Algae_score <- factor(Data$Algae_score)

# Make WAA colony count a binary variable
Data$WAA_presence <- rep(0, nrow(Data))
Data$WAA_presence[which(Data$WAA_colony_number > 0)] <- 1
Data$WAA_presence <- factor(Data$WAA_presence)

# Make Earwig trap catch a binary variable
Data$Earwig_presence <- rep(0, nrow(Data))
Data$Earwig_presence[which(Data$Earwig_trap_catch > 0)] <- 1
Data$Earwig_presence <- factor(Data$Earwig_presence)


##WAA MODELS####
## 01 Model group 1 ####

## Model 20

Mod20 <- glmer(formula = WAA_presence ~ 1 + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod20)

#This models the hierarchical structure of the random effects of 
#'Tree_ID' nested in 'Orchard_name' nested in 'Grower'
#the model's AIC = 145.2
#This should be the most basic model I present. It is incomplete, 
#so next lets add survey as a factor

## Model 21

Mod21 <- glmer(formula = WAA_presence ~ Survey + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod21)

#Model 20 with survey added
#the model's AIC = 115.9
#This shows survey has a clear effect, next lets add org_or_con. 
#I expect this will be problematic due to nesting in both grower 
#and orchard

## Model 22

Mod22 <- glmer(formula = WAA_presence ~ Survey + Org_or_Con + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod22)

#Model 21 with 'Org_or_Con' added
#The model's AIC is 115.9
#With grower and orchard included, org or con doesn't have 
#significant explanatory power, although it didn't produce an 
#error code. Let's next replace org or con with earwig presence

## Model 23

Mod23 <- glmer(formula = WAA_presence ~ Survey + Earwig_presence + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod23)

#Model 21 with 'Earwig_presence' added
#the model's AIC = 117.9
#The model's increased AIC value is unremarkable, but the 
#parameter estimates and p values clearly show something is going 
#badly wrong with this model. All p values for fixed effects are 
#set at 2*10^-16, which I'm beginning to suspect is the lowest 
#possible value. This gives model 21 as the best predictive model 
#so far. Lets now go back and try to break down orchard as a factor 
#again, this time avoiding age


    ## Results of group

#Favoured model = 21
#Model's formula = WAA_presence ~ Survey + (1|Grower/Orchard_name/Tree_ID)
#Model's AIC = 115.9
#Discussion: This group seems fairly cut and dry. Model 21 has the 
#best AIC, and when additional fixed factors were fitted alongside 
#survey, none were significant. This model of course 'cheats' by 
#using grower and orchard directly as factors, so it provides a 
#baseline for what models in groups 2, 3, and 4 are aiming towards.



## 02 Model group 2 ####

## Model 24

Mod24 <- glmer(formula = WAA_presence ~ Survey + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod24)

#Model with 'Tree_ID' as a random effect and 'Survey' as fixed
#the model's AIC = 251.6
#Not a bad start, but error messages and clearly in need of more 
#explanatory factors

## Model 25

Mod25 <- glmer(formula = WAA_presence ~ Survey + Org_or_Con + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod25)

#Model 24 with 'Org_or_Con' added as a fixed effect
#the model's AIC = 168.3
#A clear improvement, now let's check earwigs

## Model 26

Mod26 <- glmer(formula = WAA_presence ~ Survey + Org_or_Con + Earwig_presence + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod26)

#the model's AIC = 169
#Earwigs aren't adding anything here, which isn't surprising.
#Lets try adding rows per bed next, as I haven't looked at this 
#so far

## Model 32

Mod32 <- glmer(formula = WAA_presence ~ Survey + Org_or_Con*Earwig_presence + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod32)

#the model's AIC = 174.1
#The most pertinent comparison here is to model 26, which included 
#earwigs as a fixed effect but WITHOUT an interaction with 
#org vs con. Here we can see something interesting, in model 26 
#earwigs had no significant effect, and the AIC was the same as 
#model 25 without them (169). However in this model, the AIC has 
#got worse, rising to 174.1, however, now earwig presence, and 
#earwigs*org/con are showing up as significant factors. This is 
#quite a pickle. Lets proceed to adding beds per row back in with 
#this new interaction

## Model 27

Mod27 <- glmer(formula = WAA_presence ~ Survey + Org_or_Con + Rows_per_bed + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod27)

#the model's AIC = 146.6
#I almost wish this hadn't had so clear an effect, because I'm 
#concerned about the fact no organic orchards had multi-row beds.
#Let's now reintroduce grower as a random effect, and remove rows 
#per bed for now

## Models 30 and 31

conventional <- subset(Data,Org_or_Con=="Conventional")

Mod30 <- glmer(formula = WAA_presence ~ Survey + (1|Tree_ID),
               family = binomial,
               data = conventional)

summary(Mod30)

Mod31 <- glmer(formula = WAA_presence ~ Survey + Rows_per_bed + (1|Tree_ID),
               family = binomial,
               data = conventional)

summary(Mod31)

#The AIC of model 30 = 102.1 
#the AIC of model 31 = 65
#These two models verify that rows per bed does hold some 
#explanatory weight when grower is excluded from the model. 
#We must be careful of course, because the difference between 
#Northfield and Cayyes compared to the other conventional orchards 
#may be due to some other part of Figgis' management scheme, and not 
#the multi-row beds


## Model 33

Mod33 <- glmer(formula = WAA_presence ~ Survey + Rows_per_bed + Org_or_Con*Earwig_presence + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod33)

#the model's AIC = 159.6
#The relevant comparisons here are to model 27 (AIC = 146.6), 
#and 32 (AIC = 174.1). Similar to the contrasts between models 31 
#and 32, and models 26 and 27, we can see variables having similar 
#effects on the models. Multi-row beds have a significant effect, 
#and improve the AIC value. The earwig*org/con interaction shows 
#as significant, but worsens the AIC. Ed's graphical exploration of 
#the data is needed to parse what the models are producing. The 
#predictor effect plot for his model xx (which is the same as 
#model 32) shows that earwig presence lowers the probability of 
#finding WAA in conventional orchards, but not in organic ones. 
#Note that all told this model has 8 explanatory variables, which 
#is getting close to the maximum number of 11. Another very 
#important note is that this model produces two error messages. 
#One says the model failed to converge, the other says the model is 
#"nearly unidentifiable" and suggests rescaling variables. Neither 
#model 27, nor model 32 produced these. Troubling


    ## Results of group

#Favoured model = 32
#Model's formula = WAA_presence ~ Survey + Org_or_Con*Earwig_presence + (1|Tree_ID)
#Model's AIC = 174.1
#Discussion: In this case things are much harder to chose. The best 
#model by AIC is 27 (AIC = 146.6), but this excludes earwigs as a 
#factor. Model 33 (AIC = 159.6) has rows per bad and earwig presence, 
#plus a better AIC, but it produced some serious error messages. 
#While the favoured model does not have the best AIC, every fixed 
#factor is significant, it includes the interesting interaction 
#between earwigs and org_or_con, and doesn't produce error messages.



## 03 Model group 3 ####

## Model 50

Mod50 <- glmer(formula = WAA_presence ~ Survey + (1|Grower/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod50)

#the model's AIC = 153.9
#This model was missing from the group's analysis, so I've added 
#it in. What the model sits in between the favoured models of 
#group 1 and 2 due to its inclusion of grower but not orchard

## Model 28

Mod28 <- glmer(formula = WAA_presence ~ Survey + Org_or_Con + (1|Grower/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod28)

#the model's AIC = 154.4
#Not as big a leap as I expected. Whereas model 25 had org vs con 
#as a highly significant effect, with grower included this is no 
#longer the case. Nesting is an obvious issue here. Clearly without 
#the org*age interaction this model loses a lot of its power due to 
#Stuppington orchard. While Ed is obviously right about age 
#otherwise being meaningless, Stuppington's anomalousness should 
#be discussed and explained if possible. 
#Lets try adding rows per bed back now

## Model 29

Mod29 <- glmer(formula = WAA_presence ~ Survey + Rows_per_bed + (1|Grower/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod29)

#The model's AIC = 156.4
#Interesting. Not only is org vs con made redundant by grower, so 
#is rows per bed. This shouldn't really come as a surprise, only 
#Figgis has multi-row beds. The last thing I might try doing for 
#WAA presence is removing the data from organic orchards just to 
#look at rows per bed again


## Model 54

Mod54 <- glmer(formula = WAA_presence ~ Survey + (1|Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod54)

#the model's AIC = 119
#This model provides a more direct way to compare 
#grower and orchard, which felt missing from the group


    ## Results of group

#Favoured model = 50
#Model's formula = WAA_presence ~ Survey + (1|Grower/Tree_ID)
#Model's AIC = 153.9
#Discussion: Overall this group exists to explore the power of 
#grower as an explanatory variable. While not as powerful as the 
#group 1 models involving orchard, grower still makes other orchard 
#level descriptors redundant.



## 04 Model group 4 ####

## Model 53

Mod53 <- glmer(formula = WAA_presence ~ Orchard_age + Survey + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod53)

#the model's AIC = 251.6
#Without org_or_con in the same model, orchard age 
#shows as not significant

## Model 52

Mod52 <- glmer(formula = WAA_presence ~ Orchard_age + Org_or_Con + Survey + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod52)

#the model's AIC = 169.8
#This model produces an error code and displays all 
#factors as significant to the maximum possible p value.

## Model 17

Mod17 <- glmer(formula = WAA_presence ~ Survey + Orchard_age*Org_or_Con + (1|Tree_ID),
              family = binomial,
              data = Data)

summary(Mod17)

#The model's AIC = 149.1
#Orchard age is insignificant on its own now, but its 
#interaction with organic is highly significant. 
#Another sign this model is bogus, organic is producing 
#a negative parameter estimate which flies in the face 
#of the survey data

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Model 48

Mod48 <- glmer(formula = WAA_presence ~ Orchard_age*Org_or_Con + Survey + Earwig_presence*Org_or_Con + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod48)

#the model's AIC = 151.6
#Follow up to model 17. Error codes about failing to 
#converge, and maximal p values all round

## Model 49

Mod49 <- glmer(formula = WAA_presence ~ Orchard_age*Org_or_Con + Survey + Rows_per_bed + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod49)

#the model's AIC = 124.6
#While the models in this group are 'bogus' as Ed put it, they 
#round out a thorough exploration of orchard age. As the only 
#measured difference between Stuppington and the other organic 
#orchards, age clearly has a lot of power in the model to explain 
#this anomalous result. But, given the very poor relationship 
#between orchard age and WAA presence elsewhere it's very important 
#to consider that an unmeasured factor could be responsible
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#In light of Ed's help exploring the data further, orchard age 
#needs to be dropped from the models. Below will be a few models 
#repeated but with orchard age and it's interaction(s) removed

## Results of group

#Favoured model = 17
#Model's formula = WAA_presence ~ Orchard_age + Org_or_Con + Survey + Orchard_age*Org_or_Con + (1|Grower/Tree_ID)
#Model's AIC = 106.7
#Discussion: The notes above go into detail on why orchard age is 
#problematic as a variable, and it's potential usefulness in 
#explaining the low WAA presence at Stuppington. The other models 
#in the group show how other factors which showed potential in 
#group 2 are eclipsed in explanatory power by this single interaction. 



##EARWIG MODELS####

#Earwig modelling can hopefully benefit from some of the lessons 
#learned in WAA modelling. Hopefully this will be shorter.

## 05 Model group 5 ####

## Model 34

Mod34 <- glmer(formula = Earwig_presence ~ 1 + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod34)

#the model's AIC = 209.1
#As before I'll start with the simplest model. Interestingly, 
#when compared with model 20 the AIC is much higher. I think 
#this means the earwig data is simply more random than the WAA 
#data? Lets see if we can find out more

## Model 35

Mod35 <- glmer(formula = Earwig_presence ~ Survey + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod35)

#the model's AIC = 208.2
#Interesting! The model barely improves, and survey is considered 
#not significant (p = 0.085). Clearly my impression that I found 
#more earwigs during survey 2 is not supported by the 
#presence/absence data. It's worth noting the abundance data might 
#show a different story (and in fact looks like it does)

## Models 36, 37, and 38

Mod36 <- glmer(formula = Earwig_presence ~ Org_or_Con + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod36)

Mod37 <- glmer(formula = Earwig_presence ~ WAA_presence + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod37)

Mod38 <- glmer(formula = Earwig_presence ~ Rows_per_bed + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod38)

#model 36 AIC = 210.5
#model 37 AIC = 207.7
#model 38 AIC = 208.2
#No model is much of an improvement on model 34. WAA presence is 
#close to significant (p = 0.063). Perhaps with an interaction 
#between this and org/con we'll see this become significant

## Model 39

Mod39 <- glmer(formula = Earwig_presence ~ Org_or_Con*WAA_presence + (1|Grower/Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod39)

#the model's AIC = 211.2
#I was wrong, a higher p value and no significant interaction. 
#Time to strip out orchard and grower as factors and see what 
#happens


    ## Results of group

#Favoured model = 34
#Model's formula = Earwig_presence ~ 1 + (1|Grower/Orchard_name/Tree_ID)
#Model's AIC = 209.1
#Discussion: In this case the simplest model was best. While the 
#rejection of nested variables like org/con or rows per bed was 
#expected, the result for survey was not. The fact that earwig 
#abundance may have increased was discussed above already, but the 
#presence/absence data is what is used for modelling, so survey 
#will be ignored for now. As with group 1, this model provides us 
#with a target for groups 6 and 7 to aim for.



## 06 Model group 6 ####

## Model 40

Mod40 <- glmer(formula = Earwig_presence ~ Survey + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod40)

#the model's AIC = 278.1
#Once again, survey is coming up as not significant, org vs con next

## Model 41

Mod41 <- glmer(formula = Earwig_presence ~ Org_or_Con + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod41)

#the model's AIC = 263.2
#Aha, without orchard and grower to hide it, 'Org_or_Con' is 
#significant. Next I'll try WAA alone and as an interaction

## Models 42 and 43

Mod42 <- glmer(formula = Earwig_presence ~ Org_or_Con + WAA_presence + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod42)

Mod43 <- glmer(formula = Earwig_presence ~ Org_or_Con*WAA_presence + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod43)

#Model 42 AIC = 245.7
#Model 43 AIC = 239.2
#WAA presence is significant in both models, and the WAA*org/con 
#interaction is significant in model 43. No error codes. Next 
#stop, rows per bed

## Model 44

Mod44 <- glmer(formula = Earwig_presence ~ Org_or_Con*WAA_presence + Rows_per_bed + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod44)

#the model's AIC = 228.6
#Comparing this model to model 43 is bizarre, the AIC has 
#improved, but rows per bed is shown as not significant. Instead, 
#the intercept, which was not significant in model 43 is now 
#showing as significant. Also, there's an error code about the 
#model being "nearly unidentifiable" again. I have no idea how to 
#interpret this really. For now I'll drop this as a factor and move 
#onto the moss and algae scores

## Model 45

Mod45 <- glmer(formula = Earwig_presence ~ Org_or_Con*WAA_presence + Algae_score + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod45)

#the model's AIC = 241.5
#Algae score is not significant, and the intercept is back to not 
#being significant too. On to moss

## Model 46

Mod46 <- glmer(formula = Earwig_presence ~ Org_or_Con*WAA_presence + Moss_score + (1|Tree_ID),
               family = binomial,
               data = Data)

summary(Mod46)

#the model's AIC = 234
#The AIC value seems to be a marginal improvement on model 43. 
#The intercept is significant again. Only 'Moss_score2' as a level 
#is significant. Seems like some visual exploration of the data is 
#in order

tab12 <- table(Data$Moss_score, Data$Earwig_presence)
plot(tab12, main = 'Moss score', ylab = 'Earwig present')

plot(predictorEffect("Moss_score", Mod46), lines=list(multiline=TRUE))

#There's only a single datapoint for moss score 3, so the standard 
#error is huge for this level. Ignoring 3, there is a weak downward 
#trend from 0 to 2 suggesting you're slightly less likely to find 
#earwigs on a tree with a higher moss score. This is the opposite 
#of what I'd have expected, but the standard deviations for all 4 
#levels show some overlap. Overall I think this has to be dropped, 
#but could be presented

#Next lets add grower back in as a random effect

    ## Results of group

#Favoured model = 43
#Model's formula = Earwig_presence ~ Org_or_Con*WAA_presence + (1|Tree_ID)
#Model's AIC = 239.2
#Discussion: While model 43 wins out here, models 44 (AIC = 228.6) 
#and 46 (AIC = 234) need discussing. Both models have better AIC 
#scores. In the case of model 44, the error codes and parameter 
#estimates make this model impossible to interpret. Also while not 
#scientific, my feeling while carrying out the surveys was that 
#earwig levels in the Figgis orchards seemed unremarkable, neither 
#particularly high nor low. Model 46 tentatively shows an effect of 
#moss. While certainly interesting, especially because the effect is 
#in the opposite direction I would have predicted, the effect is too 
#weak and the errors too big for this beat the simpler model 43. In 
#regards to the winning model, it should be clear that WAA being 
#present should absolutely not have a direct causal relationship 
#that decreases earwig presence. The factors are predictive only. 
#The same interaction with org/con also crops us as in model 32. 
#Once thing I had not considered, the presence of WAA could lead to 
#more spraying which might negatively impact earwigs in turn. So 
#while not direct, this effect could run both ways.



## 07 Model group 7 ####

## Model 47

Mod47 <- glmer(formula = Earwig_presence ~ Org_or_Con*WAA_presence + (1|Grower/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod47)

#the model's AIC = 220.6
#Once grower is added back in, all the fixed effects become 
#non-significant. It clearly has a lot of explanatory power. I'm 
#not sure if any further modelling needs to be done for now, so 
#I'll leave this here and add a further explanation if more model 
#fitting seems in order

## Model 51

Mod51 <- glmer(formula = Earwig_presence ~ 1 + (1|Grower/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod51)

#the model's AIC = 217.9
#I felt this was missing just to tie the group together.

## Model 55

Mod55 <- glmer(formula = Earwig_presence ~ 1 + (1|Orchard_name/Tree_ID),
               family = binomial,
               data = Data)

summary(Mod55)

#the model's AIC = 208.6
#A more direct way to compare grower to orchard


    ## Results of group

#Favoured model = 51
#Model's formula = 1 + (1|Grower/Tree_ID)
#Model's AIC = 217.9
#Discussion: Much as with group 3 for WAA, this group shows that 
#grower encompasses most orchard level factors, but is still less 
#powerful than orchard itself. The most pertinent comparison is to 
#model 34 (AIc = 209.1), which is the same but with orchard added 
#as correctly nested random factor. This means there is some 
#variation between the orchards belonging to the same grower that 
#this model fails to capture









## Ed's example work (outdated) ####
# Orchard_age+Geographic_area+Org_or_Con+Survey+GAA_presence_absence + Date
mybin0 <-glm(formula = factor(WAA_presence) ~ Orchard_age + Geographic_area + Org_or_Con + Survey + GAA_presence_absence + Date,
             family = binomial,
             data = Data) 

Anova(mybin0)

summary(mybin0)

ggplot(Data, aes(x=Orchard_age, y=WAA_presence)) + 
  geom_point(color = "red") +
  stat_smooth(method="glm", color="blue", se=FALSE,
              method.args = list(family=binomial))
table(Data$Geographic_area, Data$WAA_presence)

visreg(mybin0, "Geographic_area")
visreg(mybin0, "Org_or_Con")
visreg(mybin0, "Survey")
visreg(mybin0, "GAA_presence_absence")
visreg(mybin0, "Date")

# Model 2
mybin1 <-glm(formula = WAA_presence ~ Earwig_presence,
             family = binomial,
             data = Data) 

Anova(mybin1)
summary(mybin1)
visreg(mybin1, "Earwig_presence")

mosaicplot(table(Data$WAA_presence, Data$Earwig_presence))

# Random Forest analysis 

rf0 <- randomForest(factor(WAA_presence) ~ ., data = Data[,-c(23:24)])

rf0

plot(importance(rf0))

varImpPlot(rf0)
