#Multiple Linear Regression Analysis
data(seatpos,package = "faraway") #loading the data 
library(faraway)
#attach(seatpos)
head(seatpos)
summary(seatpos)
#dim(seatpos)
library(car)

#install.packages("faraway")

#fitting the model

model<-lm(hipcenter~., data = seatpos)

summary(model)

#1. testing for multicollinearity

#i) condition numbers

c <- model.matrix(model)[,-1]  #eigendecomposition of the predictor space excluding the intercept
e <- eigen(t(c) %*% c)
e$val
sqrt(e$val[1]/e$val)

#ii) correlation matrix
round(cor(seatpos),2)

#iii) scatterplot matrix
pairs(seatpos)            #use  col = "" for colours

#iv) variance inflation factor
vif(model)

set.seed(133)
noise = rnorm(n = nrow(seatpos), mean = 0, sd = 10)    #adding random noise to test the effect of collinearity
model_noise = lm(hipcenter + noise ~ ., data = seatpos)
summary(model_noise)
coef(model)
coef(model_noise)

#2. Checking unusual observations
par(mfrow = c(1, 1))
#i) leverage
hatv <- hatvalues(model)
head(hatv)
sum(hatv)    #sum of all leverages equal number of parameters in the model
drivers<- row.names(seatpos)
halfnorm(hatv,labs=drivers,ylab="Leverages")
abline(h=0.474, col="blue")       # cutoff point = 2*p/n, any point > h, is a leverage


#ii) outliers

stud <- rstudent(model)
stud[which.max(abs(stud))]
qt(.05/(38*2),28)              #Bonferroni critical value

#since 2.39 < |-3.57|, observation 31 is not an outlier

#iii) influential points - use Cook's distance

cook <- cooks.distance(model)
halfnorm(cook,2,labs=drivers,ylab="Cook’s distances")
abline(h=0.11) # as a heuristic a data value is considered to be influential if cook's D_i >4/n

#removing the observation with the largest's cooks distance to check how it influences the fit
modcook <- lm(hipcenter~.,seatpos,subset=(cook < max(cook)))
sumary(modcook)

coef(model)
coef(modcook)




#3. Assumptions about the error
par(mfrow = c(1, 4)) # Create a 2 x 2 plotting matrix
plot(model)

require(lmtest)
shapiro.test(residuals(model))  #test for normality
bptest(model)                   #test for homoscedasticity Breusch-Pagan
dwtest (model)               #test for correlated errors


#4. Transformation of y
require(MASS)
boxcox(model, plotit=T)   #cannot be used to transform -ve response values
boxcox(lmod, plotit=T, lambda=seq(0.5,1.5,by=0.1))


#5. Selection of the "best" model
#i) AIC                        #AIC = nlog(RSS/n) + 2p
require(leaps)
par(mfrow = c(1, 1)) # 
b <- regsubsets(hipcenter~., data = seatpos)
rs <- summary(b)
rs$which
AIC <- 38*log(rs$rss/38) + (2:9)*2
plot(AIC ~ I(1:8), ylab="AIC", xlab="Number of Predictors")

model2<-lm(hipcenter~Age+Ht+Leg, data = seatpos)
summary(model2)
#significance of regression coefficients using CI #if B_j = 0 falls within a CI
confint(model2)

