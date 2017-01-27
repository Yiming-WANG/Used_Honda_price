UH=read.csv("e:/used.csv")
save(UH,file ="e:/used_hondas.RData")
load("e:/used_hondas.RData")
head(UH)


#try to predict used hondas' price
#Use Age of the car make sence rather than use the year of the car made.
#Plot data
plot(UH$Mileage,UH$Price)
plot(UH$Location,UH$Price)
plot(UH$Color,UH$Price)
plot(UH$Age,UH$Price)

#correlation
cor(UH$Price,UH$Mileage)
cor(UH$Price,UH$Age)
cor(UH$Mileage,UH$Age)


#depending on the graphs, it looks more like a first order model.
#Will test with first order moder, then add interactive terms. 
#Will also, test second order model and third order(just give it a try).


paste0(UH$Color)
#Take Color as dummy variable. "White" would be base level
UH$C1 = ifelse(UH$Color ==  "Black" , 1, 0)
UH$C2 = ifelse(UH$Color ==  "Brown" , 1, 0)
UH$C3 = ifelse(UH$Color ==  "Grey" , 1, 0)

paste0(UH$Location)
#Take Location as dummy variable. "Santa Cruz" would be base level
UH$L1 = ifelse(UH$Location ==  "St.Paul" , 1, 0)
UH$L2 = ifelse(UH$Location ==  "Durham" , 1, 0)


#Add possible interactive terms
UH$M_A=UH$Mileage*UH$Age

#Add possible Quadratic terms
UH$M_SQ=UH$Mileage^2
UH$A_SQ=UH$Age^2

#Add Cube terms
UH$M_CU=UH$Mileage^3
UH$A_CU=UH$Age^3


#First order model without interaction
library(leaps)
yvar = c("Price")
xvars = c("Mileage","Age","C1","C2","C3","L1","L2")
model=leaps( x=UH[,xvars], y=UH[,yvar], names=xvars, nbest=2, method="adjr2")
model$which
model$adjr2
model=leaps( x=UH[,xvars], y=UH[,yvar], names=xvars, nbest=2, method="Cp")
model$which
model$Cp

#First order model with interaction
yvar = c("Price")
xvars = c("Mileage","Age","C1","C2","C3","L1","L2","M_A")
model=leaps( x=UH[,xvars], y=UH[,yvar], names=xvars, nbest=2, method="adjr2")
model$which
model$adjr2
model=leaps( x=UH[,xvars], y=UH[,yvar], names=xvars, nbest=2, method="Cp")
model$which
model$Cp

#Second order model with interaction
yvar = c("Price")
xvars = c("Mileage","Age","C1","C2","C3","L1","L2","M_A","M_SQ","A_SQ")
model=leaps( x=UH[,xvars], y=UH[,yvar], names=xvars, nbest=2, method="adjr2")
model$which
model$adjr2
model=leaps( x=UH[,xvars], y=UH[,yvar], names=xvars, nbest=2, method="Cp")
model$which
model$Cp

#Third order model with interaction
yvar = c("Price")
xvars = c("Mileage","Age","C1","C2","C3","L1","L2","M_A","M_SQ","A_SQ","M_CU","A_CU")
model=leaps( x=UH[,xvars], y=UH[,yvar], names=xvars, nbest=2, method="adjr2")
model$which
model$adjr2
model=leaps( x=UH[,xvars], y=UH[,yvar], names=xvars, nbest=2, method="Cp")
model$which
model$Cp

#Consider both adjR^2 (should be high) and Cp (Cp=p and should be low), 
#pick 12 models from all models above for further analysis.
model1=lm(Price~Mileage+Age+C2,data=UH)
model2=lm(Price~Mileage+Age+C2+L1,data=UH)
model3=lm(Price~Mileage+Age+C1+C2+C3+L1+L2,data=UH)

in.model1=lm(Price~Mileage+Age+C2+L1+M_A,data=UH)
in.model2=lm(Price~Mileage+Age+C2+L1+L2+M_A,data=UH)
in.model3=lm(Price~Mileage+Age+C1+C2+C3+L1+L2+M_A,data=UH)

sq.model1=lm(Price~Mileage+L1+M_A+M_SQ,data=UH)
sq.model2=lm(Price~Mileage+C2+M_A+M_SQ,data=UH)
sq.model3=lm(Price~Mileage+C2+L1+M_A+M_SQ,data=UH)
sq.model4=lm(Price~Mileage+Age+C1+C2+C3+L1+L2+M_A+M_SQ+A_SQ,data=UH)

cu.model1=lm(Price~Mileage+C2+L1+M_A+M_CU+A_CU,data=UH)
cu.model2=lm(Price~Mileage+Age+C1+C2+C3+L1+L2+M_A+M_SQ+A_SQ+M_CU+A_CU,data=UH)


#Check if betas are significant.
summary(model1)
summary(model2)
summary(model3)
summary(in.model1)
summary(in.model2)
summary(in.model3)
summary(sq.model1)
summary(sq.model2)
summary(sq.model3)
summary(sq.model4)
summary(cu.model1)
summary(cu.model2)

#The betas in following 3 models, all < .05, keep them
summary(model1)
summary(in.model1)
summary(sq.model2)


#Check multicollinearity
library(car)
vif(model1)
vif(in.model1)
vif(sq.model2)

#because of the interactive term M_A, M_A and Mileage has a high VIF. It is reasonable. 

#Use Training and testing partition of data to test models.
train.percent = .70
test.percent = .30
sample = sample(1:nrow(UH), train.percent * nrow(UH)); head(sample)
train = UH[sample,]; head(train)
test = UH[-sample,]; head(test)


evaluate_model <- function(description, formula, plot=TRUE) {
  train.fit = lm(formula, data=train)
  train.summary = summary(train.fit)
  Price_Hat = predict(train.fit, test)  # fit test data using train model
  cor.Price_hat.Price = cor(Price_Hat, test$Price)
  if (plot==TRUE) {
    plot(Price_Hat, test$Price, main=description, xlab="Predicted Price", ylab="Actual Price")
    abline(0,1)  # 45 degree angle, cosmetic
  }
  train.rmse = train.summary$sigma
  predictors = dim(train.summary$coefficients)[1]  # includes beta0
  test.df = nrow(test) - predictors  # degrees of freedom
  test.rmse = sqrt(sum((test$Price - Price_Hat) ^ 2) / (test.df))
  percent.error = (test.rmse - train.rmse) / train.rmse * 100
  dat = data.frame(description, cor.Price_hat.Price, train.rmse, test.rmse, percent.error)
  return(dat)
}

evaluate_model("model1", model1)
evaluate_model("in.model1", in.model1)
evaluate_model("sq.model2", sq.model2)

#Pick up in.model1: Price~Mileage+Age+C2+L1+M_A as the best model because of low percent.error.


#heteroscedasticity
library(MASS)
plot(in.model1)

standardized.residuals = rstandard(in.model1)

standardized.residuals[85-1]
standardized.residuals[43-1]
standardized.residuals[42-1]
standardized.residuals[29-1]

#None of the point's Cook's distance > 1
#None of the point locates out of 3s.
#So, could be treated as no outlier, no infuencial points.


#Plot model
require(effects)
mean = mean(UH$Age)
sd = sd(UH$Age)
fit=lm(Price~Mileage+Age+C2+L1+Mileage:Age,data=UH)
plot(effect("Mileage:Age", fit,, list(Age=c(mean-sd, mean, mean+sd))), multiline=TRUE)

summary(fit)
#The final model for predicting used Honda is:
#Price=22540-0.09775*Mileage-668.5*Age-1104*C2-817.2*L1+0.003513*Mileage*Age


#Predict Price:
#i.	Mileage=20000, Age=1, Color= Brown, location= Durham, 
#Mileage=20000, Age=1, C2=1,L1=0
predict1=predict(fit, data.frame(Mileage=20000, Age=1, C2=1,L1=0))
print(predict1)

#Mileage=50000, Age=7, Color= Black, location= Santa Cruz
#Mileage=50000, Age=7, C2=0, L1=0
predict2=predict(fit, data.frame(Mileage=50000, Age=7, C2=0,L1=0))
print(predict2)

#Mileage=80000, Age=2, Color= White, location= St. Paul
#Mileage=80000, Age=2, C2=0,L1=1
predict3=predict(fit, data.frame(Mileage=80000, Age=2, C2=0,L1=1))
print(predict3)
