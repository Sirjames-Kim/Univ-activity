install.packages("MASS") #package install
library("MASS") # package 불러오기

set.seed(100)
mu <- c(0,0)
S <- matrix(c(1,0.8,0.8,1), nrow=2) ## bivariate normal with rho=0.8
xy <- mvrnorm(n=100, mu=mu, Sigma=S)
xbar <- mean(xy[,1])
sum(xy[,1])/100
ybar <- mean(xy[,2])
sum(xy[,2])/100
plot(xy, xlab="x", ylab="y", main="Correlation coefficient=0.8")
abline(v=xbar) #vertical,수직선
abline(h=ybar) #horizontal,수평선



#-------------------------수업예제_2-----------------------------

dt <- read.table("P029a.txt", header=TRUE, sep="\t")
## cov(dt$X, dt$Y)
n <- nrow(dt)
x <- dt[,1]
x <- dt$X
y <- dt$Y
mean.x <- sum(x)/n
mean.y <- sum(y)/n
cov.xy <- 1/(n-1)*sum((x-mean.x)*(y-mean.y))



#-------------------------수업예제_3-----------------------------
dt <- read.table("P029b.txt", header=TRUE, sep="\t")
par(mfrow=c(2,2))
plot(dt$X1, dt$Y1, xlab="X1", ylab="Y1", main="(a)")
lm1 <- lm(Y1 ~ X1, data=dt)
abline(a=lm1$coeff[1], b=lm1$coeff[2])
plot(dt$X2, dt$Y2, xlab="X2", ylab="Y2", main="(b)")
lm2 <- lm(Y2 ~ X2, data=dt)
abline(a=lm2$coeff[1], b=lm2$coeff[2])
plot(dt$X3, dt$Y3, xlab="X3", ylab="Y3", main="(c)")
lm3 <- lm(Y3 ~ X3, data=dt)
abline(a=lm3$coeff[1], b=lm3$coeff[2])
plot(dt$X4, dt$Y4, xlab="X4", ylab="Y4", main="(d)")
lm4 <- lm(Y4 ~ X4, data=dt)
abline(a=lm4$coeff[1], b=lm4$coeff[2])
cor(dt$X1,dt$Y1)

cor(dt$X2,dt$Y2)

cor(dt$X3,dt$Y3)

cor(dt$X4,dt$Y4)

>
  
  #-------------------------수업예제_4-----------------------------

dt <- read.table("P031.txt", header=TRUE, sep="\t")
names(dt)
y <- dt$Minutes
x <- dt$Units
n <- nrow(dt)
mean.y <- sum(y)/n
mean.x <- sum(x)/n
cov.xy <- 1/(n - 1)*sum((y - mean.y)*(x - mean.x))
sd.y <- sqrt(1/(n - 1)*sum((y - mean.y)^2))
sd.x <- sqrt(1/(n - 1)*sum((x - mean.x)^2))
cor.xy <- cov.xy/(sd.x*sd.y)
cor(x,y)

cor.xy

plot(x,y,xlab="Units",ylab="Minutes")




#-------------------------수업예제_5-----------------------------

beta1 <- sum((y - mean.y)*(x - mean.x))/sum((x - mean.x)^2)
beta0 <- mean.y - beta1*mean.x
beta1

beta0