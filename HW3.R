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

ccg803 <- read.csv("~/Documents/MS-2nd-year/Winter/BIOST 537/Assignments/Problem set 3/ccg803.csv")

# Problem 1
# 1
ccg.surv<-Surv(time=ccg803$duration,
               event=ccg803$relapse)
ccg.cox1<-coxph(ccg.surv~rx, data = ccg803)
ccg.cox1
exp(confint.default(ccg.cox1))

# 2
ccg.cox2<-coxph(ccg.surv~rx+wbc+age, data = ccg803, ties="exact")
ccg.cox2
exp(confint.default(ccg.cox2))

# 3
ccg803$wbc01<-ifelse(ccg803$wbc<100, 0, 1)
table(ccg803$wbc01)
ccg.surv3.1<-Surv(time=ccg803[ccg803$wbc01==0,]$duration,
                  event=ccg803[ccg803$wbc01==0,]$relapse)
ccg.cox3.1<-coxph(ccg.surv3.1~rx+wbc+age, data = ccg803[ccg803$wbc01==0,])
ccg.cox3.1
exp(confint.default(ccg.cox3.1))
ccg.surv3.2<-Surv(time=ccg803[ccg803$wbc01==1,]$duration,
                  event=ccg803[ccg803$wbc01==1,]$relapse)
ccg.cox3.2<-coxph(ccg.surv3.2~rx+wbc+age, data = ccg803[ccg803$wbc01==1,])
ccg.cox3.2
exp(confint.default(ccg.cox3.2))
ccg.cox3.3<-coxph(ccg.surv~rx*wbc01+wbc+age, data = ccg803)
ccg.cox3.3<-coxph(ccg.surv~rx+wbc01+rx*wbc01+wbc+age, data = ccg803)
ccg.cox3.3
exp(confint.default(ccg.cox3.3))
ccg.cox3.3.2<-coxph(ccg.surv~rx+wbc01+wbc+age, data = ccg803)
ccg.cox3.3.2
anova(ccg.cox3.3.2, ccg.cox3.3)

# 4
ccg.cox4<-coxph(ccg.surv~rx+wbc+age+institution, data = ccg803)
ccg.cox4
exp(confint.default(ccg.cox4))

# 5
plot(survfit(ccg.cox2, newdata=data.frame(rx=1,age=5,wbc=40) , se = FALSE) ,
          col=2, lwd=2, xlab = "time (in days)", 
     ylab = "survival probability")
lines(survfit(ccg.cox2, newdata=data.frame(rx=0,age=5,wbc=40) , se = FALSE) ,
      col=1, lwd=2)
lines(survfit(ccg.cox2, newdata=data.frame(rx=1,age=5,wbc=200) , se = FALSE) ,
      col=3, lwd=2)
lines(survfit(ccg.cox2, newdata=data.frame(rx=0,age=5,wbc=200) , se = FALSE) ,
      col=4, lwd=2)
legend("topright", 27,0.97, fill = c(2,1,3,4),legend = c("5yr-old treated wbc = 40","5yr-old control wbc = 40",
                                             "5yr-old treated wbc = 200","5yr-old control wbc = 200"))

# Problem 2
# 1.
addicts <- read.csv("~/Documents/MS-2nd-year/Winter/BIOST 537/Assignments/Problem set 1/addicts.csv")
addict.surv<-Surv(time=addicts$time,
               event=addicts$event)
addict.cox1<-coxph(addict.surv~dose+prison+clinic, data = addicts)
addict.cox1
exp(confint.default(addict.cox1))

# 2.
addict.cox2<-coxph(addict.surv~dose+prison+strata(clinic), data = addicts)
addict.cox2
exp(confint.default(addict.cox2))

# 3
addict.cox3<-coxph(addict.surv~dose*prison+strata(clinic), data = addicts)
addict.cox3
addict.cox3.3<-coxph(addict.surv~dose*prison+clinic, data = addicts)
addict.cox3.3
exp(confint.default(addict.cox3))
est <-exp(70*coef(addict.cox3)["dose"]+coef(addict.cox3)["prison"]+120*coef(addict.cox3)["dose:prison"])
est
library(msm)
mean_age <-mean(trace$age)
se <-deltamethod(g =~ exp((70*x1+1*x2+120*x3)),
                 mean =coef(addict.cox3)[c("dose", "prison", "dose:prison")],
                 cov =vcov(addict.cox3)[c("dose", "prison", "dose:prison"),c("dose", "prison", "dose:prison")])
se
c(est-1.96*se, est+1.96*se)
