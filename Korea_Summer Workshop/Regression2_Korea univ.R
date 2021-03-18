#Regression analysis 2 class in korea university summer statistics workshop

#1
AD <- read.csv("AD.csv")
AD

#Fitting a logistic regression model  
fit <- glm(y ~ x, weights = count, data = AD, family = binomial("logit"))
summary(fit)


#Performing the LR test
anova(fit, test="Chisq")


#Computing OR
exp(fit$coefficients)

#2
crab <- read.table("Crab.csv", sep=",", header=TRUE)
dim(crab)
head(crab)

crab$color <- factor(crab$color)
crab$spine <- factor(crab$spine)
contrasts(crab$color)
contrasts(crab$spine)

#Fitting a logistic regression model  
fit <- glm(y ~ color+width, data = crab, family = binomial("logit"))
summary(fit)

#Performing the LR test
anova(fit, test="Chisq")

#Computing OR
exp(fit$coefficients)

#Fitting a logistic regression model  
fit <- glm(y ~ color+spine+width, data = crab, family = binomial("logit"))
summary(fit)

#3
heart <- read.table("heart.csv", sep=",", header=F)
heart
names(heart) <- c("pressure","total","ndisease")
heart

y <- heart$ndisease/heart$total

#Fitting a logistic regression model  
fit <- glm(y ~ pressure, weights=total, data = heart, family = binomial("logit"))
summary(fit)

#Performing the LR test
anova(fit, test="Chisq")


#Computing OR
exp(fit$coefficients)


#Diagonostics
residuals(fit, type="pearson")
residuals(fit, type="deviance")

influence.measures(fit) 

par(mfrow=c(2,2))
r <- residuals(fit, type="pearson")
plot(r, ylab="Pearson Residual")

r <- residuals(fit, type="deviance")
plot(r, ylab="Deviance Residual")

#4
reader = read.csv("Readers.csv")
head(reader)

reader$gender <- factor(reader$gender)
contrasts(reader$gender)

fit = glm(subs~gender+age+socio+polit, family=binomial, data=reader)
summary(fit)

fit2 = step(fit, dirction="both")
summary(fit2)
fitted(fit2)

fit3 = glm(subs~age+socio, family=binomial, data=reader)
summary(fit3)

#LR test : all beta = 0
fit0 = glm(subs~1, family=binomial, data=reader)
anova(fit0, fit2, test="Chisq")

#Score test : all beta = 0
fit0 = glm(subs~1, family=binomial, data=reader)
anova(fit0, fit2, test="Rao")

#ROC curve
library(ROCR)
pred = prediction(fitted(fit2), reader$subs)
perf = performance(pred, "tpr", "fpr")
plot(perf)
AUC = performance(pred, "auc")@y.values
AUC

#5
install.packages("VGAM")
library("VGAM")

alligator = read.csv("Alligator.csv")
head(alligator)
dim(alligator)

levels(alligator$choice) 

fit = vglm(choice ~ length, family=multinomial, data=alligator) #the lase one "O" is the baseline-category!
summary(fit)

fit2 = vglm(choice ~ length, family=multinomial(refLevel = "O"), data=alligator) #"O" is the baseline-category!
summary(fit2)

fit3 = vglm(choice ~ length, family=multinomial(refLevel = "I"), data=alligator) #"I" is the baseline-category!
summary(fit3)

#OR estimates
exp(summary(fit2)@coefficients[3:4])

#LR test
fit0 = vglm(choice ~ 1, family=multinomial(refLevel = "O"), data=alligator) #no predictors
LR.stat = -2*(logLik(fit0) - logLik(fit2))
LR.stat

df = summary(fit0)@df.residual - summary(fit2)@df.residual
df

p.value = 1 - pchisq(LR.stat, df=df)
p.value

#6
install.packages("VGAM")
library("VGAM")

Belief = read.csv("Belief.csv")
Belief

levels(Belief$Race)
Belief$Race = relevel(Belief$Race, ref="Black") #Changing levels
levels(Belief$Race)

levels(Belief$Gender)
Belief$Gender = relevel(Belief$Gender, ref="Male") #Changing levels
levels(Belief$Gender)

y = as.matrix(Belief[,3:5])
y

fit = vglm(y ~ Gender+Race, family=multinomial(refLevel = "No"), data=Belief) #baseline="No"
summary(fit)


fit2 = vglm(y ~ Gender+Race, family=multinomial(refLevel = "Undecided"), data=Belief) #baseline="Undecided"
summary(fit2)

#OR estimates
exp(summary(fit)@coefficients[3:6])

#LR test : all beta = 0
fit0 = vglm(y ~ 1, family=multinomial(refLevel = "No"), data=Belief)
LR.stat = -2*(logLik(fit0) - logLik(fit)); LR.stat
df = summary(fit0)@df.residual - summary(fit)@df.residual; df
p.value = 1 - pchisq(LR.stat, df=df); p.value

#7
library("VGAM")

Ideology = read.csv("Ideology.csv")
Ideology

levels(Ideology$Gender)
Ideology$Gender = relevel(Ideology$Gender, ref="Female") #Changing levels
levels(Ideology$Gender)

levels(Ideology$Party)
Ideology$Party = relevel(Ideology$Party, ref="Republican") #Changing levels
levels(Ideology$Party)

y = as.matrix(Ideology[,3:7]); y

#Party+Gender
fit = vglm(y ~ Gender+Party, family=cumulative(parallel=TRUE), data=Ideology)
summary(fit)

#OR estimates
exp(summary(fit)@coefficients[5:6])

#Check the proportional odds assumption
fit = vglm(y ~ Gender+Party, family=cumulative(parallel=TRUE), data=Ideology)
fit.new = vglm(y ~ Gender+Party, family=cumulative(parallel=FALSE), data=Ideology)
df = summary(fit)@df.residual - summary(fit.new)@df.residual
deviance(fit) - deviance(fit.new)
p.value = 1 - pchisq(deviance(fit) - deviance(fit.new), df=df)
p.value

#LR test : all beta = 0
fit0 = vglm(y ~ 1, family=cumulative(parallel=TRUE), data=Ideology)
LR.stat = -2*(logLik(fit0) - logLik(fit)); LR.stat
df = summary(fit0)@df.residual - summary(fit)@df.residual; df
p.value = 1 - pchisq(LR.stat,df=df); p.value

#Party
fit1 = vglm(y ~ Party, family=cumulative(parallel=TRUE), data=Ideology)
summary(fit1)

#OR estimates
exp(summary(fit1)@coefficients[5])

#Check the proportional odds assumption
fit1 = vglm(y ~ Party, family=cumulative(parallel=TRUE), data=Ideology)
fit.new = vglm(y ~ Party, family=cumulative(parallel=FALSE), data=Ideology)
df = summary(fit1)@df.residual - summary(fit.new)@df.residual
deviance(fit1) - deviance(fit.new)
p.value = 1 - pchisq(deviance(fit1) - deviance(fit.new), df=df)
p.value

#Model comparison
LR.stat = -2*(logLik(fit1) - logLik(fit)); LR.stat
df = summary(fit1)@df.residual - summary(fit)@df.residual; df
p.value = 1 - pchisq(LR.stat,df=df); p.value

#8
library("VGAM")

Mental = read.csv("Mental.csv")
Mental$Impair = factor(Mental$Impair, levels=c("well","mild","moderate","impaired"), ordered=TRUE)
Mental$Impair
head(Mental)

Mental$SES = as.factor(Mental$SES)
levels(Mental$SES)
Mental$SES = relevel(Mental$SES, ref="0") #Changing levels
levels(Mental$SES)

#SES+Life
fit = vglm(Impair ~ Life + SES, family=cumulative(parallel=TRUE), data=Mental)
summary(fit)

#Model comparison
fit0 = vglm(Impair ~ 1, family=cumulative(parallel=TRUE), data=Mental)
LR.stat = -2*(logLik(fit0) - logLik(fit)); LR.stat
df = summary(fit0)@df.residual - summary(fit)@df.residual; df
p.value = 1 - pchisq(LR.stat,df=df); p.value

#Life+SES+Life*SES
fit1 = vglm(Impair ~ Life*SES, family=cumulative(parallel=TRUE), data=Mental)
summary(fit1)

#Model comparison
LR.stat = -2*(logLik(fit) - logLik(fit1)); LR.stat
df = summary(fit)@df.residual - summary(fit1)@df.residual; df
p.value = 1 - pchisq(LR.stat,df=df); p.value

#Check the proportional odds assumption
fit = vglm(Impair ~ Life + SES, family=cumulative(parallel=TRUE), data=Mental)
fit.new = vglm(Impair ~ Life + SES, family=cumulative(parallel=FALSE), data=Mental)
df = summary(fit)@df.residual - summary(fit.new)@df.residual
p.value = 1 - pchisq(deviance(fit) - deviance(fit.new), df=df); p.value

#LR test : all beta = 0
fit0 = vglm(Impair ~ -Life - SES, family=cumulative(parallel=TRUE), data=Mental)
LR.stat = -2*(logLik(fit0) - logLik(fit)); LR.stat
df = summary(fit0)@df.residual - summary(fit)@df.residual; df
p.value= 1 - pchisq(LR.stat,df=df); p.value