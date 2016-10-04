### R code from vignette source '3-multipleRegression-part2.Rnw'

###################################################
### code chunk number 1: ppCorrect
###################################################
pp <- data.frame(
  screening=c(2007371, 935573), abortion=c(289750,327000))
plot(1:2, c(min(pp), max(pp)), type='n', xlab='Year', ylab='Number')
lines(1:2, pp$screening, type='l', col='pink', lwd=3)
lines(1:2, pp$abortion, col='red', lwd=3)


###################################################
### code chunk number 2: SimpleRegressionSummary
###################################################
library(ggplot2)
set.seed(123)
index <- sample(1:nrow(diamonds), 50) # try a subset first
diamonds2 <- diamonds[index,]
fit <- lm(price ~ carat, data=diamonds2)
fit


###################################################
### code chunk number 3: Intervals
###################################################
confint(fit)
head(predict(fit, interval='confidence'))


###################################################
### code chunk number 4: Multicollinearity1
###################################################
summary(lm(price ~ x, data=diamonds2))


###################################################
### code chunk number 5: Multicollinearity2
###################################################
summary(lm(price ~ y, data=diamonds2))


###################################################
### code chunk number 6: Multicollinearity3
###################################################
summary(lm(price ~ x+y, data=diamonds2))


###################################################
### code chunk number 7: LinearCarat
###################################################
summary(lm(price ~ carat, data=diamonds2))


###################################################
### code chunk number 8: QuadraticCarat
###################################################
summary(lm(price ~ carat + I(carat^2), data=diamonds2))


###################################################
### code chunk number 9: Correlation
###################################################
with(diamonds2, plot(carat, carat^2, 
 main=paste('cor =', round(cor(carat, carat^2), digits=3)))
)


###################################################
### code chunk number 10: Correlation
###################################################
with(diamonds2, plot(carat-mean(carat), (carat-mean(carat))^2, 
 main=paste('cor =', 
 round(cor(carat-mean(carat), (carat-mean(carat))^2), digits=3)))
)


###################################################
### code chunk number 11: Poly
###################################################
summary(lm(price ~ poly(carat, 2), data=diamonds2))


###################################################
### code chunk number 12: readSurgical
###################################################
X <- read.table('/Users/ovitek/Dropbox/Olga/Teaching/CS6220/Fall15/LectureNotes/3-multipleRegression/surgical.txt', sep='')
dimnames(X)[[2]] <- c('blood', 'prog', 'enz', 'liver', 
 'age', 'female', 'modAlc', 'heavyAlc', 'surv', 'lsurv')
dim(X)
head(X)
sum(is.na(X))


###################################################
### code chunk number 13: corSurgical
###################################################
round(cor(X[,-c(9:10)]), digits=2)


###################################################
### code chunk number 14: pairsSurgical
###################################################
pairs(X)


###################################################
### code chunk number 15: exhastive
###################################################
library(leaps)
# By default - exhaustive search
regfit.full <- regsubsets(lsurv ~ ., nvmax=3, data=X[,-9])
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$which


###################################################
### code chunk number 16: exhastive1
###################################################
reg.summary$rsq
reg.summary$adjr2
which.max(reg.summary$adjr2)
coef(regfit.full, 3)


###################################################
### code chunk number 17: exhastive2
###################################################
plot(reg.summary$adjr2, xlab='Number of variables', 
    ylab='adj R2', type='l')


###################################################
### code chunk number 18: forward
###################################################
regfit.full1 <- regsubsets(lsurv ~ ., method='forward', 
 data=X[,-9])
reg.summary1 <- summary(regfit.full1)
#reg.summary1
reg.summary1$adjr2
which.max(reg.summary1$adjr2)


###################################################
### code chunk number 19: cv
###################################################
library(DAAG)
lm.full <- lm(lsurv ~ ., data=X[,-9])
CVlm(X[,-9], lm.full)


###################################################
### code chunk number 20: cvPlotObs
###################################################
CVlm(X[,-9], lm.full, printit=FALSE, plotit='Observed')


###################################################
### code chunk number 21: cvPlotRes
###################################################
CVlm(X[,-9], lm.full, printit=FALSE, plotit='Residual')


