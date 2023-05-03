## HEADER ####
## who: Ed H
## what: poaching data HARUG!
## when: last edited 2023-05-03

## CONTENTS ####
## 00 Setup
## 01 OBJ pre2017 
## 01.1 Did the number of pangolin events decrease pre2017?
## 01.2 proportion change post 2017 all orders
## 02 OBJ did prop. Africa increase relative to asia over time?

## 00 Setup ####

library(stringr)
library(lubridate)

# rows have either 6 or 7 fields... 
# Get the begin line of each row

source('scripts/data.R')

write.csv(masterdata, 'masterdata.csv', row.names = F)


## 01 OBJ pre2017 ####
### 01.1 Did the number of pangolin events decrease pre2017? ####

# Create count of events by year and pre2017 for pango
phol1 <- aggregate(event~year + post2017,
          data = masterdata[which(masterdata$order == 'Pholidota'),],
          FUN = length)
boxplot(event~post2017, data = phol1,
        ylab = '', xlab = '', 
        names = c('Pre 2017', 'post 2017'))
stripchart(event~post2017, data = phol1,
           add = T, vertical = T,
           pch = 16, col = c('red', 'dodgerblue'),
           method = 'jitter')
mtext('# Incidences', side = 1, line = 3, cex = 1.3)
mtext('2017 legislation', side = 2, line = 7, cex = 1.3)

hist(lm(event~post2017, data = phol1)$residuals)
shapiro.test(lm(event~post2017, data = phol1)$residuals)

wilcox.test(event~post2017, data = phol1)
# t.test(event~post2017, data = phol1)


### 01.2 proportion change post 2017 all orders ####
order2017 <- table(masterdata$order, masterdata$post2017)
mycol <- c('goldenrod', 'goldenrod1', 'seagreen', 'seagreen1', 
           'blue4', 'dodgerblue', 'plum', 'plum1', 
           'brown', 'brown1')
mycol <- c('blue4', 'dodgerblue')
par(mar = c(5,7,4,2))
barplot(t(order2017), horiz = F, las = 1,
        col = mycol, beside = T,)
mtext('# Incidences', side = 2, line = 3.5, cex = 1.3)
mtext('Taxonomic Order', side = 1, line =2.5, cex = 1.3)
par(mar = c(5,4,4,2))
legend(x = 'top', horiz = T, col = mycol, pch = 15,
       legend = c(c('Pre 2017', 'post 2017')))

# overall test
chisq.test(order2017)

chisq.test(order2017[c('Artiodactyla', 'Pholidota'),])
chisq.test(order2017[c('Carnivora', 'Pholidota'),])
chisq.test(order2017[c('Perissodactyla', 'Pholidota'),])
chisq.test(order2017[c('Proboscidea', 'Pholidota'),])


## 02 OBJ did prop. Africa increase relative to asia over time? ####
length(table(masterdata$year))
masterdata$decade <- droplevels(factor(masterdata$decade))
continents <- table(masterdata$decade, masterdata$continent)[-1,]
continents2 <- round(prop.table(continents, 1), 2)

mycol <- c('springgreen3', 'darkgreen')
par(mar = c(5,7,4,2))
barplot(t(continents), horiz = F, las = 1,
        col = mycol, beside = T,)
mtext('# Incidences', side = 2, line = 3.5, cex = 1.3)
mtext('Decade', side = 1, line =2.5, cex = 1.3)
par(mar = c(5,4,4,2))
legend(x = 'top', horiz = T, col = mycol, pch = 15,
       legend = c(c('Africa', 'Asia')))

par(mar = c(5,7,4,2))
barplot(t(continents2), horiz = F, las = 1,
        col = mycol, beside = T, ylim = c(0,1.2))
mtext('% Incidences', side = 2, line = 3.5, cex = 1.3)
mtext('Decade', side = 1, line =2.5, cex = 1.3)
par(mar = c(5,4,4,2))
legend(x = 'top', horiz = T, col = mycol, pch = 15,
       legend = c(c('Africa', 'Asia')))

chisq.test(continents)

## 01 graphs ###

mycol <- c('goldenrod1', 'seagreen', 'dodgerblue', 'plum', 'tan')

par(mar = c(5,9,4,2))
x <- barplot(table(masterdata$order), horiz = T, las = 1,
        col = mycol)
mtext('# Incidences', side = 1, line = 3, cex = 1.3)
mtext('Taxonomic Order', side = 2, line = 7, cex = 1.3)
text(c('Elephants', 'Pangolins', 'Rhinos et al.',
       'Tigers et al.', 'Antelope et al.'),
     x = 600, y = x[5:1])
par(mar = c(5,4,4,2))


masterdata$species[which(masterdata$order == 'Artiodactyla')]

class(masterdata$date2)
table(masterdata$date2 < dmy('31/12/2016')) 
table(masterdata$pre2017)

temp <- which(masterdata$order == "Pholidota" | masterdata$order == "Proboscidea" )
dat1 <- masterdata[temp,]

t1 <- table(dat1$pre2017, dat1$order)

barplot(t(t1), beside = T,
        col = c('seagreen', 'plum'),
        names.arg = c('pre \'17', 'post \'17'))

chisq.test(t(t1))
