# problem set 2
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
install.packages("survMisc")
library(survMisc)
setwd("/Users/mxzhang/Documents/MS-2nd-year/Winter/BIOST 537/BIOST537")
source("getmedianres.R")

# Q1
Q1 <- read.csv("~/Documents/MS-2nd-year/Winter/BIOST 537/Assignments/Problem set 2/Q1.csv")
s.Q1maintain<-with(Q1[Q1$Group==1,],Surv(time,event))
s.Q1control<-with(Q1[Q1$Group==0,],Surv(time,event))
kmmaintain<-survfit(s.Q1maintain~1)
kmcontrol<-survfit(s.Q1control~1)
summary(kmmaintain)
summary(kmcontrol)
namaintain=basehaz(coxph(s.Q1maintain~1))
nacontrol=basehaz(coxph(s.Q1control~1))
namaintain
nacontrol

# Q2
# (a)
addicts <- read.csv("/Users/mxzhang/Documents/MS-2nd-year/Winter/
                    BIOST 537/Assignments/Problem set 1/addicts.csv")
s.addicts <- with(addicts, Surv(time, event))
kmaddict<-survfit(s.addicts~1)
summary(kmaddict, times = seq(0, 50,1))
plot(kmaddict,main="Kaplan-Meier survivor estimate", 
     ylab = "Survival probability", xlab="Time (in days)", cex=1.5)
summary(kmaddict, times = seq(360, 370,1))

# (b)
# i
summary(kmaddict, times = seq(380, 400,1))
summary(kmaddict, times = seq(450, 500,1))
summary(kmaddict, times = seq(500, 530,1))
summary(kmaddict, times = seq(530, 560,1))
# ii
print(kmaddict)

# (c)
# (i)
kmaddict2<-survfit(s.addicts~prison, data = addicts)
plot(kmaddict2, col = c("blue","red"), lwd = 1.5, xlab = "Time (in days)",
     main="Kaplan-Meier survival estimates", ylab = "Survival probability")
legend("topright",c("incarcerated","unincarcerated"), 
       col = c("red","blue"), lwd = c(1.5,1.5), cex = 1.2)
# (ii)
summary(kmaddict2, times = 240)
addictsprisonsterr<-sqrt(0.0384^2+0.0475^2)
addictsprisonest<-0.7634-0.6555
addictsprisonest/addictsprisonsterr
(1-pnorm(addictsprisonest/addictsprisonsterr))*2

#(iii)
survdiff(s.addicts~prison, data = addicts)

#(iv)
comp(ten(kmaddict2))$tests$lrTests


#(v)
plot(kmaddict2, fun = "cumhaz",xlab = "Time (in days)",
     main="Nelson-Aalen cumulative hazard estimates", 
     ylab = "Cumulative hazard",
     col = c("blue","red"),lwd = 1.5)
legend("bottomright",c("incarcerated","unincarcerated"),
       col = c("red","blue"),lwd = c(1.5, 1.5))

#(d)
addicts$dose_60<-ifelse(addicts$dose>60,1,0)

# (i)
kmaddict3<-survfit(s.addicts~dose_60, data = addicts)
plot(kmaddict3, col = c("blue","red"), lwd = 1.5, xlab = "Time (in days)",
     main="Kaplan-Meier survival estimates", ylab = "Survival probability")
legend("topright",c("> 60 mg/day","<= 60 mg/day"), 
       col = c("red","blue"), lwd = c(1.5,1.5), cex = 1.2)

# (ii)
summary(kmaddict3, times = 240)
addictsdosesterr<-sqrt(0.0425^2+0.0365^2)
addictsdoseest<-0.8582-0.6162
addictsdoseest/addictsprisonsterr
(1-pnorm(addictsdoseest/addictsprisonsterr))*2

#(iii)
survdiff(s.addicts~dose_60, data = addicts)

#(iv)
comp(ten(kmaddict3))$tests$lrTests

#(v)
plot(kmaddict3, fun = "cumhaz",xlab = "Time (in days)",
     main="Nelson-Aalen cumulative hazard estimates", 
     ylab = "Cumulative hazard",
     col = c("blue","red"),lwd = 1.5)
legend("topleft",c("> 60 mg/day","<= 60 mg/day"),
       col = c("red","blue"),lwd = c(1.5, 1.5))


# (e)
survdiff(s.addicts~prison+strata(clinic), data = addicts)

# (f)
summary(kmaddict, times = 120)
0.5*0.884 #0.442
summary(kmaddict, times = seq(280, 300,2))
summary(kmaddict, times = seq(370, 400,2))
summary(kmaddict, times = seq(450, 480,2))
summary(kmaddict, times = seq(550, 600,2)) 
summary(kmaddict, times = seq(520, 550,2))
summary(kmaddict, times = seq(535, 545,1))#540
summary(kmaddict, times = 240)
0.5*0.715 # 0.3575
summary(kmaddict, times = seq(600, 650,2))
summary(kmaddict, times = seq(670, 700,2))
summary(kmaddict, times = seq(650, 670,1))#661
summary(kmaddict, times = 365)
0.5*0.606 # 0.303
summary(kmaddict, times = seq(700, 750,2))
summary(kmaddict, times = seq(745, 755,1))#749


getmedianres(s.addicts,times = 120, confint = TRUE)
getmedianres(s.addicts,times = 240, confint = TRUE)
getmedianres(s.addicts,times = 365, confint = TRUE)

