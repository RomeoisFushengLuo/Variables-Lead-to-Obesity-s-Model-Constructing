## Visualizing the data, Exploratory analysis

library(visreg)

Train <- read.table(file='Train.txt', header=TRUE,sep="")

Train[1:10,]

str(Train) # Look at the data in details, like numbers of variables and observations

library(car)
scatterplotMatrix(Train) ## gives slightly more info

## Let's start by fitting a linear model first.
fit <- lm(brozek~., data=Train)
coef(fit)
summary(fit)

## Step-wise Regression
## Afterwards, we will apply the two automatic variable selection procedures(best subsets regression and stepwise regression) in R to wisely select the models.
fit_step1 <- step(fit)
fit0 <- lm(brozek~1, data=Train)
fit_step2 <- step(fit0, scope = brozek~neck + chest + abdom + hip + thigh + knee + ankle +  biceps + forearm + wrist)

## Best Subsets Regression
library(leaps)

a <- regsubsets(brozek~., data=Train)
summary.out <- summary(a)
summary.out
summary.out$cp

## Based on the value of Cp(Mallow's Cp) which can be automaticly derived by R, and the judging criteria is that the less is preferred. 
plot(a, scale="Cp")

fit.desired <- lm(brozek ~ abdom + neck + hip + wrist + forearm, data=Train)
summary(fit.desired)

## Model Checking and Validation
residualPlots(fit.desired, tests=FALSE)
crPlots(fit.desired, terms=~hip+neck+abdom)

## Find New Models that fit the best
fit.fixed <- lm(brozek ~ abdom + neck + hip + I(hip^2) + I(neck^2) + wrist + forearm, data=Train)
residualPlots(fit.fixed, tests=FALSE)
crPlots(fit.fixed, terms=~hip+neck+abdom+I(neck^2)+I(hip^2))
summary(fit.fixed)

##QQplot
qqPlot(fit.fixed)
influencePlot(fit.fixed)

library(dplyr)
Test <- read.table(file='Test.txt', header=TRUE,sep="")
Test[1:10,]

# We select the true value of brozek in the Test file.
TestResponses = select(Test, brozek)$brozek
TestResponses
predictions <- predict(fit.fixed, newdata=select(Test, -brozek))
errors <- abs(predictions - TestResponses)
plot(errors, xlab='sequence number', ylab='errors between predictions and reality')
mse_MSE <- mean((predictions - TestResponses)^2)
mse_MSE
predictions.full <- predict(fit, newdata=select(Test, -brozek))
mse_MSE.full <- mean((predictions.full - TestResponses)^2)
mse_MSE.full

plot(predictions.full, col='blue')
points(predictions, col = 'black', pch=2)
points(TestResponses, col = 'red', pch=3)
legend("topleft", legend = c("Full model", "Our model", "Real values"), pch = 1:3, col = c("blue", "black", "red", ncol = 3) )