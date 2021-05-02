library(foreign)
library(survival)
library(flexsurv)
library(msm)
addicts <- read.csv("~/Documents/MS-2nd-year/Winter/BIOST 537/Assignments/Problem set 1/addicts.csv")
s.addicts <- with(addicts, Surv(time, event))

# Question 1 (a)
weibull.AFT <-flexsurvreg(s.addicts~dose+clinic+prison, data = addicts,dist = "weibull")
weibull.AFT
cox1<-coxph(s.addicts~dose+clinic+prison, data = addicts)
cox1
Gengamma.AFT <-flexsurvreg(s.addicts~dose+clinic+prison, data = addicts,dist = "gengamma")
Gengamma.AFT

# change
(1.017-1.025)/1.025
(2.048-2.032)/2.032
(0.834-0.795)/0.795


# question 1 b
est<-exp(coef(weibull.AFT)["dose"]*(-60)+coef(weibull.AFT)["clinic"]-coef(weibull.AFT)["prison"])
est
se <-deltamethod(g =~ exp(-60*x1+x2-x3),
                 mean =coef(weibull.AFT)[c("dose", "clinic","prison")],
                 cov =vcov(weibull.AFT)[c("dose", "clinic","prison"),
                                        c("dose", "clinic","prison")])
se
c(est-1.96*se, est+1.96*se)

est1 <-exp(coef(weibull.AFT)["dose"]*40+2* coef(weibull.AFT)["clinic"])*log(2)^
  (1/ exp(coef(weibull.AFT)["shape"]))*exp(coef(weibull.AFT)["scale"])
est1
se1 <-deltamethod(g =~ exp(40*x1+2*x2+0*x3)* log(2)^(1/ exp(x4))* exp(x5),
                 mean =coef(weibull.AFT)[c("dose", "clinic","prison", "shape", "scale")],
                 cov =vcov(weibull.AFT)[c("dose", "clinic","prison", "shape", "scale"),
                                        c("dose", "clinic","prison", "shape", "scale")])
se1
c(est1-1.96*se1, est1+1.96*se1)

est2 <-exp(coef(weibull.AFT)["dose"]*100+ coef(weibull.AFT)["clinic"]+coef(weibull.AFT)["prison"])*log(2)^
  (1/ exp(coef(weibull.AFT)["shape"]))*exp(coef(weibull.AFT)["scale"])
est2
se2 <-deltamethod(g =~ exp(100*x1+x2+x3)* log(2)^(1/ exp(x4))* exp(x5),
                 mean =coef(weibull.AFT)[c("dose", "clinic","prison", "shape", "scale")],
                 cov =vcov(weibull.AFT)[c("dose", "clinic","prison", "shape", "scale"),
                                        c("dose", "clinic","prison", "shape", "scale")])
se2
c(est2-1.96*se2, est2+1.96*se2)

# Question 1 (c)
addicts$dumclinic<-ifelse(addicts$clinic==1,1,2)

weibull.AFT2 <-flexsurvreg(s.addicts~dose+dumclinic+dose*prison, data = addicts, dist = "weibull")

weibull.AFT2
est3<-exp(coef(weibull.AFT2)["dose"]*20+20*coef(weibull.AFT2)["dose:prison"])
est3
se3 <-deltamethod(g =~ exp(20*x1+20*x2),
                 mean =coef(weibull.AFT2)[c("dose", "dose:prison")],
                 cov =vcov(weibull.AFT2)[c("dose", "dose:prison"),
                                        c("dose", "dose:prison")])
se3
c(est3-1.96*se3, est3+1.96*se3)

est4<-exp(coef(weibull.AFT2)["dose"]*20)
est4
se4 <-deltamethod(g =~ exp(20*x1),
                  mean =coef(weibull.AFT2)["dose"],
                  cov =vcov(weibull.AFT2)["dose",
                                          "dose"])
se4
c(est4-1.96*se4, est4+1.96*se4)

# Question2
diabetes <- read.csv("~/Documents/MS-2nd-year/Winter/BIOST 537/Assignments/
                     Problem set 4/diabetes.csv")
diabetes$followup<-diabetes$age+diabetes$time/12
surv.diabete<-Surv(diabetes[diabetes$treat==0,]$age, diabetes[diabetes$treat==1,]$followup, 
                   diabetes[diabetes$treat==0,]$status)
plot(surv.diabete, lwd = 1.5, xlab = "Age (in years)",
     main="Kaplan-Meier survival estimates", ylab = "Survival probability", col="green")
surv.diabete2<-with(diabetes[diabetes$treat==0,], Surv(followup, status))
par(new=TRUE)
plot(surv.diabete2,col="red")
legend("bottomleft",c("ignoring delayed entry","accounting for delayed entry"),
       col = c("red","green"),lwd = c(1.5,1.5), cex = 0.8)
print(surv.diabete)
kmdiabete<-survfit(surv.diabete~1)
kmdiabete
kmdiabete2<-survfit(surv.diabete2~1)
kmdiabete2
