library(pwr)

cohen.ES('f2', 'small')
cohen.ES('f2', 'medium')
cohen.ES('f2', 'large')

pwr.f2.test(u = 2,  # numerator df
            v = ,  # denominator df
            f2 =c(.10),
            sig.level = 0.05,
            power = 0.80)

f2 <- c(.10, .15, .20)
denomdf <- c(120, 80, 60) 
denomdf_2 <- c(97, 65, 49) # "num df = 2, 3 fac levels"

plot(denomdf ~ f2,
     ylim = c(20,150),
     ylab = '~ Sample size (80% power)',
     xlab = 'Cohen\'s f2',
     type = 'b',
     pch = 16,
     lwd=1.5,
     col = 'blue')

lines(denomdf_2 ~ f2,
      ylab = 'Sample size (80% power)',
      xlab = 'Cohen\'s f2',
      type = 'b',
      pch = 16,
      lwd=1.5,
      col = 'darkgreen')

legend(x=.16, y=140,
       bty='n',
       legend = c('5 treatments', '3 treatments'),
       lty=1,
       lwd=2,
       col = c('blue', 'darkgreen'))
