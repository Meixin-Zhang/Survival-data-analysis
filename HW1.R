addicts <- read.csv("/Users/mxzhang/Documents/MS-2nd-year/Winter/BIOST 537/Assignments/Problem set 1/addicts.csv")
source("fitparametric.R")
getwd()

library(survival)
library(foreign)
install.packages("flexsurv")
library(flexsurv)
install.packages("msm")
library(msm)
library(coin)
install.packages("zoo")
library(zoo)
install.packages("formatR")
library(formatR)
library(haven)
library(numDeriv)

# Question 4a
summary(addicts$time)
table(addicts$event)
prop.table(table(addicts$event))

# Question 4b
s.addicts <- with(addicts, Surv(time, event))
fitparametric(s.addicts, dist="exp")
fitparametric(s.addicts, dist="weibull")
fitparametric(s.addicts, dist="gengamma")


fs1<-flexsurvreg(Surv(addicts$time, addicts$event)~1,data = addicts, dist="exp")
fs1
fs2<-flexsurvreg(Surv(addicts$time, addicts$event)~1,data = addicts, dist="weibull")
fs2
fs3<-flexsurvreg(Surv(addicts$time, addicts$event)~1,data = addicts, dist="gengamma")
fs3

# Question 4c
summary(addicts$time)
plot(survfit(Surv(time,event)~1,data = addicts),conf.int = FALSE,
     mark.time = FALSE, xlim = c(0,1076), cex = 1.5, xlab = "time (in days)", 
     ylab = "survival probability",
     lwd = 1.5)
axis(1, at=seq(0,1076, by=200), labels=seq(0,1076, by=200))
lines(fs1, col = 3,ci=FALSE, lwd = 1.8, lty = 3)
lines(fs2, col = 4,ci=FALSE, lwd = 1.8, lty = 3)
lines(fs3, col = 2,ci=FALSE, lwd = 1.8, lty = 3)
legend("topright", legend = c("exponential distribution", 
                              "Weibull distribution",
                              "generalized gamma distribution",
                              "nonparametric estimator"),
       fill = c(3,4,2,1),cex = 0.8)

# Question 4d
-2*(fs2$loglik-fs3$loglik)
1- pchisq(1.128078, df = 1)

# Question e
# a
median.weibull <-function(shape, scale) {
  qweibull (0.5 , shape = shape , scale = scale )
  }
summary(fs2 , fn=median.weibull , t=1,B=10000)
# b
fit_weibull <- fitparametric(s.addicts, dist = "weibull", feature = "survival",
                              t = 366)
# c
fit_weibull_condition<- fitparametric(s.addicts, dist = "weibull",
                              feature ="condsurvival", t = 731, t0 = 366)
                             
# Question f
-2*(fs1$loglik-fs2$loglik)
1- pchisq(8.013391, df = 1)

# Question G
fsclinic1<-flexsurvreg(Surv(time, event)~1,data = addicts[addicts$clinic==1,], dist="exp")
fsclinic1
fsclinic2<-flexsurvreg(Surv(time, event)~1,data = addicts[addicts$clinic==2,], dist="exp")
fsclinic2
fsclinic1$res
delta <- fsclinic1$res[1] - fsclinic2$res[1]
delta_se <- sqrt(fsclinic1$res[4] ^ 2 +
                   fsclinic2$res[4] ^ 2)
delta
delta_se
T_W <- abs(delta) / delta_se
T_W
2 * pnorm(-T_W)

# Question h
fsprison0<-flexsurvreg(Surv(time, event)~1,data = addicts[addicts$prison==0,], dist="exp")
fsprison0
fsprison1<-flexsurvreg(Surv(time, event)~1,data = addicts[addicts$prison==1,], dist="exp")
fsprison1
delta2 <- fsprison0$res[1] - fsprison1$res[1]
delta_se2 <- sqrt(fsprison0$res[4] ^ 2 +
                   fsprison1$res[4] ^ 2)
T_W2 <- abs(delta2) / delta_se2
T_W2
2 * pnorm(-T_W2)
delta2
delta_se2
