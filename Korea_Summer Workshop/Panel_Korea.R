#1
lung = read.table("c:\\data\\lung.dat",header=T)
print(lung)

# Plot of individual trajectories
plot(lung$Time, lung$FEV1, type="n", xlab="Time(years)", ylab="FEV1(liters)",xaxt="n")
axis(1, at=seq(0, 21, by=3)) 
for(i in unique(lung$ID)) lines(lung[lung$ID==i,]$Time, lung[lung$ID==i,]$FEV1,col=i)

# Plot of individual trajectories and smoothing curves by smoking status
lung_ns=lung[lung$Smoker==0,]
lung_s=lung[lung$Smoker==1,]

plot(lung$Time, lung$FEV1, type="n", xlab="Time(years)", ylab="FEV1(liters)",main="Non-smoker",xaxt="n")
axis(1, at=seq(0, 21, by=3)) 
for(i in unique(lung_ns$ID)) lines(lung_ns[lung_ns$ID==i,]$Time, lung_ns[lung_ns$ID==i,]$FEV1,col=i)
lines(loess.smooth(lung_ns$Time,lung_ns$FEV1), col="black", lwd=3)

plot(lung$Time, lung$FEV1, type="n", xlab="Time(years)", ylab="FEV1(liters)",main="Smoker",xaxt="n")
axis(1, at=seq(0, 21, by=3)) 
for(i in unique(lung_s$ID)) lines(lung_s[lung_s$ID==i,]$Time, lung_s[lung_s$ID==i,]$FEV1,col=i)
lines(loess.smooth(lung_s$Time,lung_s$FEV1,span=0.5), col="black", lwd=3)

# Plot of means by smoking status
Mean_FEV1=aggregate(FEV1~Smoker+Time,data=lung, mean)
plot(Mean_FEV1$Time, Mean_FEV1$FEV1, type="n", xlab="Time(years)", ylab="FEV1(liters)",xaxt="n")
axis(1, at=seq(0, 21, by=3)) 
for(i in unique(Mean_FEV1$Smoker)) lines(Mean_FEV1[Mean_FEV1$Smoker==i,]$Time, Mean_FEV1[Mean_FEV1$Smoker==i,]$FEV1,col=i+1)
legend(12,3.5, c("Non-smoker","Smoker"),lty=c(1,1), col=c(1,2)) 

library(nlme)

# Model 1: Saturated model with an unstructured covariance matrix
glsout1=gls(FEV1~Smoker*Time, data=lung, cor=corSymm(form = ~ 1 | ID),weights = varIdent(form = ~1|Time))
summary(glsout1)

# Model 2: Saturated model with a compound symmetry covariance matrix
glsout2=gls(FEV1~Smoker*Time, data=lung, cor=corCompSymm(form = ~ 1 | ID))
summary(glsout2)

# Model 3: Main effects model with a compound symmetry covariance matrix
glsout3=gls(FEV1~Smoker+Time, data=lung, cor=corCompSymm(form = ~ 1 | ID))
summary(glsout3)

# Plot of means and predicted means by smoking status over time
Mean_FEV1=aggregate(FEV1~Smoker+Time,data=lung, mean)
Mean_pred=aggregate(glsout3$fitted~Smoker+Time,data=lung, mean)
colnames(Mean_pred)=c("Smoker","Time", "Pred")
plot(Mean_FEV1$Time, Mean_FEV1$FEV1, type="n", xlab="Time(years)", ylab="FEV1(liters)",xaxt="n")
axis(1, at=seq(0, 21, by=3)) 
for(i in unique(Mean_FEV1$Smoker)) points(Mean_FEV1[Mean_FEV1$Smoker==i,]$Time, Mean_FEV1[Mean_FEV1$Smoker==i,]$FEV1,col=i+1)
for(i in unique(Mean_pred$Smoker)) lines(Mean_pred[Mean_pred$Smoker==i,]$Time, Mean_pred[Mean_pred$Smoker==i,]$Pred,col=i+1)
legend(12,3.5, c("Non-smoker","Smoker"),lty=c(1,1), col=c(1,2)) 

# Random Intercept model
lmeout=lme(FEV1~Smoker*Time, data=lung, random=~1|ID)
summary(lmeout)

# Prediction of the random intercept model
library(lattice)
lmepred=lmeout$fitted
colnames(lmepred)=c("pm","p")

lung1=cbind(lung,lmepred)

xyplot(FEV1+pm+p~Time|ID, data=lung1[lung1$ID %in% c(1:6), ],
       type=c("p","l","l"), col.line=c("white","black", "red"),         
       panel=panel.superpose,distribute.type = TRUE, 
       xlab="Time(years)", ylab="FEV1(liters)", as.table=TRUE)

#2
tlc = read.table("c:\\data\\tlc.dat",header=T)
print(tlc)

# Plot of individual trajectories
plot(tlc$Week, tlc$Lead, type="n", xlab="Time(weeks)", ylab="Mean Blood Lead Level",xaxt="n")
axis(1, at=seq(0, 6, by=2)) 
for(i in unique(tlc$ID)) lines(tlc[tlc$ID==i,]$Week, tlc[tlc$ID==i,]$Lead,col=i)

# Plot of individual trajectories and smoothing curves by treatment status
tlc_p=tlc[tlc$Group==0,]
tlc_s=tlc[tlc$Group==1,]

plot(tlc$Week, tlc$Lead, type="n", xlab="Time(weeks)", ylab="Mean Blood Lead Level",main="Placebo",xaxt="n")
axis(1, at=seq(0, 6, by=2))
for(i in unique(tlc_p$ID)) lines(tlc_p[tlc_p$ID==i,]$Week, tlc_p[tlc_p$ID==i,]$Lead,col=i)
lines(loess.smooth(tlc_p$Week,tlc_p$Lead), col="black", lwd=3)

plot(tlc$Week, tlc$Lead, type="n", xlab="Time(weeks)", ylab="Mean Blood Lead Level",main="Succimer",xaxt="n")
axis(1, at=seq(0, 6, by=2))
for(i in unique(tlc_s$ID)) lines(tlc_s[tlc_s$ID==i,]$Week, tlc_s[tlc_s$ID==i,]$Lead,col=i)
lines(loess.smooth(tlc_s$Week,tlc_s$Lead), col="black", lwd=3)

# Plot of means by treatment status
Mean_Lead=aggregate(Lead~Group+Week,data=tlc, mean)
plot(Mean_Lead$Week, Mean_Lead$Lead, type="n", xlab="Time(weeks)", ylab="Mean Blood Lead Level",xaxt="n")
axis(1, at=seq(0, 6, by=2)) 
for(i in unique(Mean_Lead$Group)) lines(Mean_Lead[Mean_Lead$Group==i,]$Week, Mean_Lead[Mean_Lead$Group==i,]$Lead,col=i+1)
legend(4,26.5, c("Placebo","Succimer"),lty=c(1,1), col=c(1,2)) 

library(nlme)

# Model 1: Model with categorical time and an unstructured covariance matrix
glsout1=gls(Lead~Group*factor(Week), data=tlc, cor=corSymm(form = ~ 1 | ID),weights = varIdent(form = ~1|Week))
summary(glsout1)

# Model 2: Model with piecewise linear time trend and an unstructured covariance matrix
tlc$Week_p=pmax(tlc$Week-1,0)
glsout2=gls(Lead~Group*Week+Group*Week_p, data=tlc, cor=corSymm(form = ~ 1 | ID),weights = varIdent(form = ~1|Week))
summary(glsout2)

# Plot of means and predicted means by smoking status over time
Mean_Lead=aggregate(Lead~Group+Week,data=tlc, mean)
Mean_pred=aggregate(glsout2$fitted~Group+Week,data=tlc, mean)
colnames(Mean_pred)=c("Group","Week", "Pred")
plot(Mean_Lead$Week, Mean_Lead$Lead, type="n", xlab="Time(weeks)", ylab="Mean Blood Lead Level",xaxt="n")
for(i in unique(Mean_Lead$Group)) points(Mean_Lead[Mean_Lead$Group==i,]$Week, Mean_Lead[Mean_Lead$Group==i,]$Lead,col=i+1)
for(i in unique(Mean_pred$Group)) lines(Mean_pred[Mean_pred$Group==i,]$Week, Mean_pred[Mean_pred$Group==i,]$Pred,col=i+1)
legend(4,26.5, c("Placebo","Succimer"),lty=c(1,1), col=c(1,2)) 



