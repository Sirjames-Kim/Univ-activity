#Variance analysis in Korea university summer statistics workshop

## Vectors
a <- c(1,2,5.3,6,-2,4) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector

a[c(2,4)] # 2nd and 4th elements of vector

### Matrices
# generates 5 x 4 numeric matrix 
y<-matrix(1:20, nrow=5,ncol=4)

# another example
cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2") 
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames))
x <- matrix(1:20,5,4) 
x[,4] # 4th column of matrix
x[3,] # 3rd row of matrix 
x[2:4,1:3] # rows 2,3,4 of columns 1,2,3

## Data frame

d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
names(mydata) <- c("ID","Color","Passed") # variable names

myframe <- data.frame(ID = c("dog","cat","rat","pig","bird"), Age=c(10,20,30,40,50),
                      X1 = c(11,22,33,44,55),X2 = c(53,72,48,52,47),X3 = c(50,120,300,450,200))
myframe[3:5] # columns 3,4,5 of data frame
myframe[c("ID","Age")] # columns ID and Age from data frame
myframe$X1 # variable x1 in the data frame

## Lists

# example of a list with 4 components - 
# a string, a numeric vector, a matrix, and a scaler 
w <- list(name="Fred", mynumbers=a, mymatrix=y, age=5.3)

# example of a list containing two lists 
list1 <- list(name="Fred", mynumbers=a, mymatrix=y, age=5.3)
list2 <- list(name="Mark", mynumbers=b, mymatrix=x, age=6.2)
v <- c(list1,list2)

mylist <- list(name="Fred", mynumbers=a, mymatrix=y, age=5.3)
mylist[[2]] # 2nd component of the list
mylist[["mynumbers"]] # component named mynumbers in list

## Factors

# variable gender with 20 "male" entries and 
# 30 "female" entries 
gender <- c(rep("male",20), rep("female", 30)) 
gender <- factor(gender) 
# stores gender as 20 1s and 30 2s and associates
# 1=female, 2=male internally (alphabetically)
# R now treats gender as a nominal variable 
summary(gender)

## An ordered factor is used to represent an ordinal variable.
# variable rating coded as "large", "medium", "small"
rating <- c("large", "medium", "small")
rating <- ordered(rating)
# recodes rating to 1,2,3 and associates
# 1=large, 2=medium, 3=small internally
# R now treats rating as ordinal

## Useful Functions
length(object) # number of elements or components
str(object)    # structure of an object 
class(object)  # class or type of an object
names(object)  # names

c(object,object,...)       # combine objects into a vector
cbind(object, object, ...) # combine objects as columns
rbind(object, object, ...) # combine objects as rows 

object     # prints the object

ls()       # list current objects
rm(object) # delete an object

newobject <- edit(object) # edit copy and save as newobject 
fix(object)               # edit in place

## Two sample t-test example
parent<-read.csv("parent.csv")
attach(parent)
plot(score~factor(group)) # side by side boxplot
bartlett.test(score~factor(group)) # bartlett's test for equal variance
t.test(score~factor(group)) # two sample t-test under unequal variance
t.test(score~factor(group), var.equal=TRUE) # two sample t-test under equal variance

## Wilcoxon rank sum test
wilcox.test(score~factor(group))
detach()

## One-way ANOVA
learning<-read.csv("learning.csv")
attach(learning)
boxplot(score~factor(method))
oneway.test(score~factor(method))
oneway.test(score~factor(method), var.equal=T)
aov.out<-aov(score~factor(method))
summary(aov.out)

## post hoc pairwise analysis
pairwise.t.test(score, factor(method), p.adjust="non", pool.sd=T)
pairwise.t.test(score, factor(method), p.adjust="bonferroni", pool.sd=T)
install.packages("agricolae")
library(agricolae)
comparison<-duncan.test(aov.out, "factor(method)")
print(comparison)

## ANOVA in linear regression

lmout<-lm(score~factor(method))
summary.aov(lmout)
summary(lmout)

detach()

## Two-way ANOVA

rbdex<-read.csv("rbdex.csv")
attach(rbdex)

par(mfrow=c(1,2))
plot(pctloss~factor(blend)+factor(block))

twowayout<-lm(pctloss~factor(blend)+factor(block)) ## main effects
summary.aov(twowayout)
summary(twowayout)

twowayout.inter<-lm(pctloss~factor(blend)+factor(block)+factor(blend)*factor(block)) ## main effects
summary.aov(twowayout.inter)
summary(twowayout.inter)

interaction.plot(blend, block, pctloss)
interaction.plot(block, blend, pctloss)

##ANCOVA

detach(rbdex)
height<-data.frame(read.csv("height.csv"))
attach(height)

plot(age[trt==1], ht[trt==1], ylab="height", xlab="age", 
     ylim=c(134, 150), xlim=c(108, 141), pch=15, col="blue")
points(age[trt==2], ht[trt==2], pch=15, col="red")

htout<-lm(ht~age+factor(trt)+age*factor(trt))
summary.aov(htout)
summary(htout)