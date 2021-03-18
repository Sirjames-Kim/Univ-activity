#Regression analysis 1 class in Korea university summer statistics workshop

#1
Repair = read.csv("Repair.csv", header = TRUE)
Repair

summary(Repair) # Descriptive statistics
cov(Repair) # Covariance
cor(Repair) # Pearson correlation
cor.test(x=Repair[,1],y=Repair[,2]) #Testing for rho=0


fit = lm(Minutes~Units, data = Repair) #Fit a simple regression model
summary(fit)
anova(fit) #ANOVA table

par(mfrow = c(2,2))
plot(fit)


par(mfrow = c(1,1))
plot(Repair$Units, fit$residuals, col = "blue", cex = 0.7, 
     xlab = "Units", ylab = "Residuals", main="Residuals for minutes")
abline(h = 0, col = "blue")


plot(fit$fitted.values, Repair$Minutes, col = "blue", cex = 0.7)
abline(0,1, col = "blue")


conf.limit = predict(fit, interval = "confidence")
pred.limit = predict(fit, interval = "prediction")

plot(Repair$Units, Repair$Minutes, cex = 0.7, col = "blue",
     xlab = "Units", ylab = "Minutes", main="Fit Plot for minutes")
lines(Repair$Units, conf.limit[,"fit"], col = "blue", lwd = 2)
lines(Repair$Units, conf.limit[,"lwr"], col = "red", lwd = 1.5)
lines(Repair$Units, conf.limit[,"upr"], col = "red", lwd = 1.5)
lines(Repair$Units, pred.limit[,"lwr"], col = "gray", lwd = 1.5, lty = 2)
lines(Repair$Units, pred.limit[,"upr"], col = "gray", lwd = 1.5, lty = 2)

legend("topleft", legend = c("Fit","95% Confidence Limits","95% Prediction Limits"),
       col = c("blue","red","gray"), lty = c(1,1,2), lwd = c(2,1.5,1.5))

#2 
Performance = read.csv("Performance.csv", header = TRUE)
Performance

fit = lm(Y ~ X1+X2+X3+X4+X5, data = Performance) #Fit

summary(fit)
anova(fit)


par(mfrow = c(2,2))
plot(fit)


par(mfrow = c(3,3), pty="s")

plot(Performance$X1, fit$residuals, col = "blue", cex = 0.7, xlab = "X1", ylab = "Residuals")
abline(h = 0, col = "blue")

plot(Performance$X2, fit$residuals, col = "blue", cex = 0.7, xlab = "X2", ylab = "Residuals")
abline(h = 0, col = "blue")

plot(Performance$X3, fit$residuals, col = "blue", cex = 0.7, xlab = "X3", ylab = "Residuals")
abline(h = 0, col = "blue")

plot(Performance$X4, fit$residuals, col = "blue", cex = 0.7, xlab = "X4", ylab = "Residuals")
abline(h = 0, col = "blue")

plot(Performance$X5, fit$residuals, col = "blue", cex = 0.7, xlab = "X5", ylab = "Residuals")
abline(h = 0, col = "blue")

# Model Selection
fit.fw = step(fit, direction = "forward", k = 2) # Model Selection : AIC (BIC if k=log(n))
fit.fw$anova
summary(fit.fw)

fit.bw = step(fit, direction = "backward", k = 2) # Model Selection : AIC (BIC if k=log(n))
fit.bw$anova
summary(fit.bw)

fit.step = step(fit, direction = "both", k = 2) # Model Selection : AIC (BIC if k=log(n))
fit.step$anova
summary(fit.step)

#install.packages("leaps")
library(leaps)

fit.CP = leaps(x = Performance[,c("X1","X2","X3","X4","X5")], # Model Selection : CP
               y = Performance[,"Y"],method = "Cp")
fit.CP
plot(fit.CP$size, fit.CP$Cp); abline(a=0,b=1)


fit.adjR2 = leaps(x = Performance[,c("X1","X2","X3","X4","X5")], # Model Selection : adjR2
                  y = Performance[,"Y"],method = "adjr2")
fit.adjR2

#3
Anscombe = read.csv("Anscombe.csv", header = TRUE)
Anscombe

par(mfrow = c(2,2))

fit1 = lm(Y1~X1, data = Anscombe)
plot(Anscombe$X1, Anscombe$Y1, col = "blue", cex = 0.7, xlab = "X1", ylab = "Y1")
abline(fit1, col = "blue")

fit2 = lm(Y2~X2, data = Anscombe)
plot(Anscombe$X2, Anscombe$Y2, col = "blue", cex = 0.7, xlab = "X2", ylab = "Y2")
abline(fit2, col = "blue")

fit3 = lm(Y3~X3, data = Anscombe)
plot(Anscombe$X3, Anscombe$Y3, col = "blue", cex = 0.7, xlab = "X3", ylab = "Y3")
abline(fit3, col = "blue")

fit4 = lm(Y4~X4, data = Anscombe)
plot(Anscombe$X4, Anscombe$Y4, col = "blue", cex = 0.7, xlab = "X4", ylab = "Y4")
abline(fit4, col = "blue")

#4
Bacteria = read.csv("Bacteria.csv", header = TRUE)
Bacteria

fit = lm(N_t~t, data = Bacteria)
summary(fit)
anova(fit)

par(mfrow = c(2,2))
plot(fit)

par(mfrow = c(1,1))
plot(Bacteria$t, fit$residuals, xlab = "t", ylab = "Residual", col = "blue", cex = 0.7)
abline(h = 0, col = "blue")

hist(fit$residuals, freq = FALSE, breaks = 12, xlim = c(-150,150))
lines(density(fit$residuals))

plot(fit$fitted.values, Bacteria$N_t, xlim = c(-30,300),ylim = c(0,350), 
     col = "blue", cex = 0.7, xlab = "predicted value", ylab = "N_t")
abline(0,1, col = "blue")

conf.limit = predict(fit, interval = "confidence")
pred.limit = predict(fit, interval = "prediction")

plot(Bacteria$t, Bacteria$N_t, xlim = c(0,15), ylim = c(-150,400), col = "blue", cex = 0.7, 
     xlab = "t", ylab = "N_t")
lines(Bacteria$t, conf.limit[,"fit"], col = "blue", lwd = 2)
lines(Bacteria$t, conf.limit[,"lwr"], col = "red", lwd = 1.5)
lines(Bacteria$t, conf.limit[,"upr"], col = "red", lwd = 1.5)
lines(Bacteria$t, pred.limit[,"lwr"], col = "gray", lwd = 1.5, lty = 2)
lines(Bacteria$t, pred.limit[,"upr"], col = "gray", lwd = 1.5, lty = 2)

legend("topright", legend = c("Fit","95% Confidence Limits","95% Prediction Limits"),
       col = c("blue","red","gray"), lty = c(1,1,2), lwd = c(2,1.5,1.5))

#5
Workers = read.csv("Workers.csv", header = TRUE)
Workers


fit = lm(Y~X, data = Workers)
summary(fit)
anova(fit)

par(mfrow = c(2,2))
plot(fit)

par(mfrow = c(1,1))
plot(Workers$X, fit$residuals, col = "blue", cex = 0.7, xlab = "X", ylab = "Residual")
abline(h = 0, col = "blue")

conf.limit = predict(fit, interval = "confidence")
pred.limit = predict(fit, interval = "prediction")

plot(Workers$X, Workers$Y, xlab = "X", ylab = "Y", col = "blue", cex = 0.7)
lines(Workers$X, conf.limit[,"fit"], col = "blue", lwd = 2)
lines(Workers$X, conf.limit[,"lwr"], col = "red", lwd = 1.5)
lines(Workers$X, conf.limit[,"upr"], col = "red", lwd = 1.5)
lines(Workers$X, pred.limit[,"lwr"], col = "gray", lwd = 1.5, lty = 2)
lines(Workers$X, pred.limit[,"upr"], col = "gray", lwd = 1.5, lty = 2)

legend("topleft", legend = c("Fit","95% Confidence Limits","95% Prediction Limits"),
       col = c("blue","red","gray"), lty = c(1,1,2), lwd = c(2,1.5,1.5))

# Transformation tY~tX

Workers$tX = 1/(Workers$X)
Workers$tY = Workers$Y/Workers$X

fit.trans = lm(tY~tX, data = Workers)
summary(fit.trans)
anova(fit.trans)

par(mfrow = c(2,2))
plot(fit.trans)

par(mfrow = c(1,1))
plot(Workers$tX, fit.trans$residuals, col = "blue", cex = 0.7,
     xlab = "tX", ylab = "Residual")
abline(h = 0, col = "blue")

conf.limit = predict(fit.trans, interval = "confidence")
pred.limit = predict(fit.trans, interval = "prediction")

plot(Workers$tX, Workers$tY, ylim = c(0.05,0.25), col = "blue", cex = 0.7,
     xlab = "tX", ylab = "tY")
lines(Workers$tX, conf.limit[,"fit"], col = "blue", lwd = 2)
lines(Workers$tX, conf.limit[,"lwr"], col = "red", lwd = 1.5)
lines(Workers$tX, conf.limit[,"upr"], col = "red", lwd = 1.5)
lines(Workers$tX, pred.limit[,"lwr"], col = "gray", lwd = 1.5, lty = 2)
lines(Workers$tX, pred.limit[,"upr"], col = "gray", lwd = 1.5, lty = 2)

legend("topleft", legend = c("Fit","95% Confidence Limits","95% Prediction Limits"),
       col = c("blue","red","gray"), lty = c(1,1,2), lwd = c(2,1.5,1.5))

# Transformation logY~X

Workers$X2 = (Workers$X)^2
Workers$logY = log(Workers$Y)

fit.trans2 = lm(logY~X, data = Workers)
summary(fit.trans2)
anova(fit.trans2)

par(mfrow = c(2,2))
plot(fit.trans2)

par(mfrow = c(1,1))
plot(Workers$X, fit.trans2$residuals, col = "blue", cex = 0.7,
     xlab = "X", ylab = "Residual")
abline(h = 0, col = "blue")

conf.limit = predict(fit.trans2, interval = "confidence")
pred.limit = predict(fit.trans2, interval = "prediction")

plot(Workers$X, Workers$logY, ylim = c(3,6), col = "blue", cex = 0.7,
     xlab = "X", ylab = "logY")
lines(Workers$X, conf.limit[,"fit"], col = "blue", lwd = 2)
lines(Workers$X, conf.limit[,"lwr"], col = "red", lwd = 1.5)
lines(Workers$X, conf.limit[,"upr"], col = "red", lwd = 1.5)
lines(Workers$X, pred.limit[,"lwr"], col = "gray", lwd = 1.5, lty = 2)
lines(Workers$X, pred.limit[,"upr"], col = "gray", lwd = 1.5, lty = 2)

legend("topleft", legend = c("Fit","95% Confidence Limits","95% Prediction Limits"),
       col = c("blue","red","gray"), lty = c(1,1,2), lwd = c(2,1.5,1.5))

# Transformation logY ~ X + X2

fit.trans3 = lm(logY~X+X2, data = Workers)
summary(fit.trans3)
anova(fit.trans3)

par(mfrow = c(2,2))
plot(fit.trans3)

par(mfcol = c(2,2))
plot(Workers$X, fit.trans3$residuals, col = "blue", cex = 0.7,
     xlab = "X", ylab = "Residual")
abline(h = 0, col = "blue")

plot(Workers$X2, fit.trans3$residuals, col = "blue", cex = 0.7,
     xlab = "X2", ylab = "Residual")
abline(h = 0, col = "blue")

#6
stock = read.csv("stock.csv",header=TRUE)
stock

fit6 = lm(Expend ~ Stock, data=stock)
summary(fit6)

#install.packages("car")
library(car)
durbinWatsonTest(fit6) #Durbin-Watson test

plot(1:NROW(stock), fit6$residual, type="b", xlab="id", ylab="Residual")
abline(h=0)

#7
Eeo = read.csv("EEO.csv",header=TRUE)
Eeo

summary(Eeo)
cor(Eeo)

### Correlation test
cor.test(Eeo$FAM, Eeo$PEER)
cor.test(Eeo$FAM, Eeo$SCHOOL)
cor.test(Eeo$PEER, Eeo$SCHOOL)

### Fit a regression model
fit7 = lm(ACHV ~ FAM + PEER + SCHOOL,data=Eeo)
summary(fit7)
anova(fit7)

### vif
library(car)
vif(fit7)

### tolerance
1/vif(fit7)

### Condition index
#install.packages("perturb")
library(perturb)

colldiag(fit7)

### Fit simple regression models
fit7.1 = lm(ACHV ~ FAM, data=Eeo)
fit7.2 = lm(ACHV ~ PEER, data=Eeo)
fit7.3 = lm(ACHV ~ SCHOOL, data=Eeo)
summary(fit7.1)
summary(fit7.2)
summary(fit7.3)

#8
nyrivers = read.csv("NYRivers.csv",header=TRUE)
nyrivers

fit8 = lm(y~x1+x2+x3+x4,data=nyrivers)
summary(fit8)
anova(fit8)

### Residual plot
par(mfrow=c(2,2))

plot(nyrivers$x1,fit8$residual, xlab="Agriculture", ylab="Residual")
abline(h=0)
plot(nyrivers$x2,fit8$residual, xlab="Forest", ylab="Residual")
abline(h=0)
plot(nyrivers$x3,fit8$residual, xlab="Residential", ylab="Residual")
abline(h=0)
plot(nyrivers$x4,fit8$residual, xlab="Commercial/industrial", ylab="Residual")
abline(h=0)

### Regression Diagnostics

influence.measures(fit8)

### cook distance plot

par(mfrow=c(2,2))

plot(fit8,which=4)

plot(1:NROW(nyrivers),dffits(fit8),xlab="Observation",ylab="dffits")
abline(h=0)

plot(hatvalues(fit8),rstudent(fit8),xlab="leverage",ylab="rstudent")

par(mfrow=c(2,3))

plot(1:NROW(nyrivers),dfbeta(fit8)[,1],xlab="observation",ylab="dfbeta",main="Intercept")
abline(h=0)

plot(1:NROW(nyrivers),dfbeta(fit8)[,2],xlab="observation",ylab="dfbeta",main="x1")
abline(h=0)

plot(1:NROW(nyrivers),dfbeta(fit8)[,3],xlab="observation",ylab="dfbeta",main="x2")
abline(h=0)

plot(1:NROW(nyrivers),dfbeta(fit8)[,4],xlab="observation",ylab="dfbeta",main="x3")
abline(h=0)

plot(1:NROW(nyrivers),dfbeta(fit8)[,5],xlab="observation",ylab="dfbeta",main="x4")
abline(h=0)

ii  = which(nyrivers$river=="Neversink")
fit8.1 = lm(y~x1+x2+x3+x4, data=nyrivers[-ii,])
summary(fit8.1)
anova(fit8.1)

ii  = which(nyrivers$river=="Hackensack")
fit8.2 = lm(y~x1+x2+x3+x4, data=nyrivers[-ii,])
summary(fit8.2)
anova(fit8.2)

#9
salary = read.csv("salary.csv",header=TRUE)
head(salary)

salary$e1 = ifelse(salary$E==1,1,0)
salary$e2 = ifelse(salary$E==2,1,0)

salary$category=0
salary$category = ifelse(salary$E==1 & salary$M==0, 1, salary$category)
salary$category = ifelse(salary$E==1 & salary$M==1, 2, salary$category)
salary$category = ifelse(salary$E==2 & salary$M==0, 3, salary$category)
salary$category = ifelse(salary$E==2 & salary$M==1, 4, salary$category)
salary$category = ifelse(salary$E==3 & salary$M==0, 5, salary$category)
salary$category = ifelse(salary$E==3 & salary$M==1, 6, salary$category)

head(salary)

### Additive model

fit9.1 = lm(S ~ X + e1 + e2 + M , data=salary)
summary(fit9.1)
anova(fit9.1)

par(mfcol=c(2,2))
plot(salary$X, rstudent(fit9.1), xlab="X", ylab="Studentized Residual")
abline(h=0)

plot(salary$category, rstudent(fit9.1), xlab="X", ylab="Studentized Residual")
abline(h=0)

### Multiplicative model

salary$e1m = salary$e1*salary$M
salary$e2m = salary$e2*salary$M

fit9.2=lm(S ~ X + e1 + e2 + M + e1m + e2m , data=salary)
summary(fit9.2)

par(mfcol=c(2,2))
plot(salary$X,rstudent(fit9.2),xlab="X", ylab="Studentized Residual")
abline(h=0)

plot(salary$category,rstudent(fit9.2),xlab="X", ylab="Studentized Residual")
abline(h=0)

### After deleting outliers

ii = which(salary$S==23780)
salary2 = salary[-ii,]

fit9.3=lm(S ~ X + e1 + e2 + M + e1m + e2m , data=salary2)
summary(fit9.3)

par(mfcol=c(2,2))
plot(salary2$X, rstudent(fit9.3), xlab="X", ylab="Studentized Residual")
abline(h=0)

plot(salary2$category, rstudent(fit9.3), xlab="X", ylab="Studentized Residual")
abline(h=0)

#10
employment=read.csv("employment.csv", header=TRUE)
employment

fit10.1 = lm(JPERF~TEST, data=employment)
summary(fit10.1)
anova(fit10.1)

par(mfcol=c(2,2))

plot(employment$TEST, fit10.1$residual, xlab="test", ylab="residual")
abline(h=0)

conf.limit = predict(fit10.1, interval = "confidence")
pred.limit = predict(fit10.1, interval = "prediction")


plot(employment$TEST, employment$JPERF, xlab = "TEST", ylab = "JPERF", 
     cex = 0.7, col = "blue")
lines(sort(employment$TEST), conf.limit[order(employment$TEST),1], col = "blue", lwd = 2)
lines(sort(employment$TEST), conf.limit[order(employment$TEST),2], col = "red", lwd = 1.5)
lines(sort(employment$TEST), conf.limit[order(employment$TEST),3], col = "red", lwd = 1.5)
lines(sort(employment$TEST), pred.limit[order(employment$TEST),2], col = "gray", lwd = 1.5, lty = 2)
lines(sort(employment$TEST), pred.limit[order(employment$TEST),3], col = "gray", lwd = 1.5, lty = 2)

legend("topleft", legend = c("Fit","95% Confidence Limits","95% Prediction Limits"),
       col = c("blue","red","gray"), lty = c(1,1,2), lwd = c(2,1.5,1.5))

# Interaction
employment$RaceTest = employment$RACE*employment$TEST

fit10.2 = lm(JPERF ~ TEST + RACE + RaceTest , data=employment)
summary(fit10.2)

par(mfrow=c(2,2))
plot(employment$TEST,fit10.2$residuals, xlab="TEST",ylab="residuals")
abline(h=0)
plot(employment$RACE,fit10.2$residuals, xlab="RACE",ylab="residuals")
abline(h=0)
plot(employment$RaceTest,fit10.2$residuals, xlab="RACE*TEST",ylab="residuals")
abline(h=0)
