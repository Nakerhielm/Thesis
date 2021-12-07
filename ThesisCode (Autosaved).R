####################################
#R Code for Thesis
#############################################
setwd("/Users/Nathan1/Documents")
 a= read.csv("RACC2018Data.csv")
 attach(a)
 #Define our y variable 
y=Team.Score-Opp.Score+Spread
a$y=Team.Score-Opp.Score+Spread
#Get an overview of y
summary(y)
#Debug
is.na(y)
#Perform least square regression
output1=lm(y~Off.Pass.Pct+Off.Pass.Yds+Off.Pass.TD+Off.Run.Avg.Yds+Off.Run.TD+Off.Avg.Yds.Play+Off.Tot.First.Down+Off.No.Pen+Off.Pen.Yds+Off.Tot.TO+Def.Pass.Pct+Def.Pass.Yds+Def.Pass.TD+Def.Run.Avg.Yds+Def.Run.TD+Def.Avg.Yds.Play+Def.Tot.First.Down+Def.No.Pen+Def.Pen.Yds+Def.Tot.TO)
#Get summary 
summary(output1)
#Compare fitted to residual plot
plot(fitted(output1), resid(output1), main="Fitted vs. Residual of Output1", xlab="Fitted", ylab="Residual")

#Histogram of y
hist(y, main="Histogram of Cover.Spread")

#Determine the plots for output1
plot(output1)

#Determine which variables are extraneous for our model and which ones we should keep
step(output1, direction="both")
step(output1, direction="backward")

##lm(formula = y ~ Off.Pass.TD + Off.Run.TD + Off.Tot.First.Down + 
  #  Off.Pen.Yds + Off.Tot.TO + Def.Pass.Pct + Def.Run.TD + #Def.Avg.Yds.Play + 
    #Def.Pen.Yds + Def.Tot.TO)

#Determine Multiple R-squared for the variables we want to keep
output2=lm(y~Off.Pass.TD+Off.Run.TD+Off.Tot.First.Down+Off.Tot.TO+Def.Run.TD+ Def.Avg.Yds.Play+Def.Pen.Yds+Def.Tot.TO)
summary(output2)
#Fitted vs Residual plot of Output 2
plot(fitted(output2), resid(output2), main="Fitted vs. Residual of Output2", xlab="Fitted", ylab="Residual")
par(mfrow=c(2,2))
plot(output2)

#Correlation of the variables to y

#Marginal Model Plots
install.packages("car")
library(car)
mmps(output1)

b=subset(a, select=c(y,Off.Tot.First.Down,Off.Run.TD, Off.Pass.TD))
plot(b)

#Plot with 9 plots on per page
par(mfrow=c(3,3))
plot(Off.Pass.Pct, y)
plot(Off.Pass.Yds,y)
plot(Off.Run.Avg.Yds,y)
plot(Off.Avg.Yds.Play,y)
plot(Off.Tot.First.Down,y)
plot(Off.No.Pen, y)
plot(Off.Pen.Yds, y)
plot(Def.Pass.Pct,y)
plot(Def.Pass.Yds, y)

par(mfrow=c(3,2))
plot(Def.Run.Avg.Yds, y)
plot(Def.Avg.Yds.Play,y)
plot(Def.Tot.First.Down,y)
plot(Def.No.Pen, y)
plot(Def.Pen.Yds, y)
#Determine which values are outliers
3
a[50,] #Shows 50th row in dataset

#VIF of Output2
install.packages("car")
library(car)


#Histograms of 14 Continuous Covariates
par(mfrow=c(3,3))
hist(Off.Pass.Pct, breaks=20, main="Histogram of Off.Pass.Pct")
hist(Off.Pass.Yds, breaks=20, main="Histogram of Off.Pass.Yds")
hist(Off.Run.Avg.Yds, breaks=20, main="Histogram of Off.Run.Avg.Yds")
hist(Off.Avg.Yds.Play, breaks=20, main="Histogram of Off.Avg.Yds.Play")
hist(Off.Tot.First.Down, breaks=20, main="Histogram of Off.Tot.First.Down")
hist(Off.No.Pen, breaks=20, main="Histogram of Off.No.Pen")
hist(Off.Pen.Yds, breaks=20, main="Histogram of Off.Pen.Yds")
hist(Def.Pass.Pct, breaks=20, main="Histogram of Def.Pass.Pct")
hist(Def.Pass.Yds, breaks=20, main="Histogram of Def.Pass.Yds")

par(mfrow=c(3,2))
hist(Def.Run.Avg.Yds, breaks=20, main="Histogram of Def.Run.Avg.Yds")
hist(Def.Avg.Yds.Play, breaks=20, main="Histogram of Def.Avg.Yds.Play")
hist(Def.Tot.First.Down, breaks=20, main="Histogram of Def.Tot.First.Down")
hist(Def.No.Pen, breaks=20, main="Histogram of Def.No.Pen")
hist(Def.Pen.Yds, breaks=20, main="Histogram of Def.Pen.Yds")



#Power Transformation of 14 Continuous Covariates 
#First, we redefine variables that have 0/negative values
New.Off.Pass.Pct=Off.Pass.Pct+0.2
New.Off.Pass.Yds=Off.Pass.Yds+1
New.Off.No.Pen=Off.No.Pen+0.1
New.Off.Pen.Yds=Off.Pen.Yds+0.1
New.Def.Run.Avg.Yds=Def.Run.Avg.Yds+0.65
New.Def.No.Pen=Def.No.Pen+0.1
New.Def.Pen.Yds=Def.Pen.Yds+0.1


#Now, we perform the power transformation
library(car)
powerTransform(cbind(New.Off.Pass.Pct,New.Off.Pass.Yds, Off.Run.Avg.Yds,Off.Avg.Yds.Play,Off.Tot.First.Down, New.Off.No.Pen, New.Off.Pen.Yds, Def.Pass.Pct, Def.Pass.Yds, New.Def.Run.Avg.Yds, Def.Avg.Yds.Play, Def.Tot.First.Down, New.Def.No.Pen, New.Def.Pen.Yds,y+64.6))



#Transformation Analysis of Off.Run.Avg.Yds
out1=lm(y~Off.Run.Avg.Yds)
out2=lm(y~sqrt(Off.Run.Avg.Yds))
out3=lm(y~log(Off.Run.Avg.Yds))


#Obtain Multiple R^2
summary(out1)
summary(out2)
summary(out3)

summary(Off.Run.Avg.Yds)
#Scatter Plot with the regression lines
plot(Off.Run.Avg.Yds, y, main="Scatter Plot of Off.Run.Avg.Yds", ylab="Cover.Spread",)
#Add linear regression model line
abline(out1)
f=seq(0,14,0.001)
yf=-24.327+11.125*sqrt(f)
lines(f,yf,col='red', lty=2)
yf2=-14.244+9.408*log(f)
lines(f,yf2, col='blue', lty=2)
legend(
  "bottomright", 
  lty=c(1,1,1), 
  col=c("black","red", "blue"), 
  legend = c("No Transformation", "Square Root", "Log")
)

#Residual Plot

par(mfrow=c(3,1))
plot(fitted(out1), resid(out1), main="Fitted vs. Residual Off.Run.Avg.Yds")
plot(fitted(out2), resid(out2), main="Fitted vs. Residual sqrt(Off.Run.Avg.Yds)")
plot(fitted(out3), resid(out3), main="Fitted vs. Residual log(Off.Run.Avg.Yds)")



#Transformation Analysis of Off.Avg.Yds.Play
out1=lm(y~Off.Avg.Yds.Play)
out2=lm(y~sqrt(Off.Avg.Yds.Play))

#Obtain Multiple R^2
summary(out1)
summary(out2)

#Scatter Plot with the regression lines
plot(Off.Avg.Yds.Play, y, main="Scatter Plot of Off.Avg.Yds.Play", ylab="Cover.Spread")
#Add linear regression model line
abline(out1)
f=seq(1.6,14,0.001)
yf=-52.55+21.17*sqrt(f)
lines(f,yf,col='red', lty=2)
#Residual Plot

par(mfrow=c(2,1))
plot(fitted(out1), resid(out1), main="Fitted vs. Residual Off.Avg.Yds.Play")
plot(fitted(out2), resid(out2), main="Fitted vs. Residual sqrt(Off.Avg.Yds.Play)")



#Transformation Analysis of Off.Tot.First.Down
out1=lm(y~Off.Tot.First.Down)
out2=lm(y~sqrt(Off.Tot.First.Down))

#Obtain Multiple R^2
summary(out1)
summary(out2)

summary(Off.Tot.First.Down)
#Scatter Plot with the regression lines
plot(Off.Tot.First.Down, y, main="Scatter Plot of Off.Tot.First.Down", ylab="Cover.Spread")
#Add linear regression model line
abline(out1)
f=seq(7.5,38.5,0.001)

yf=-29.865+6.443*sqrt(f)
lines(f,yf,col='red', lty=2)

#Residual Plot

par(mfrow=c(2,1))
plot(fitted(out1), resid(out1), main="Fitted vs. Residual Off.Tot.First.Down")
plot(fitted(out2), resid(out2), main="Fitted vs. Residual sqrt(Off.Tot.First.Down)")

#Transformation Analysis of New.Off.No.Pen
out1=lm(y~New.Off.No.Pen)
out2=lm(y~sqrt(New.Off.No.Pen))

#Obtain Multiple R^2
summary(out1)
summary(out2)

summary(New.Off.No.Pen)
#Scatter Plot with the regression lines
plot(New.Off.No.Pen, y, main="Scatter Plot of New.Off.No.Pen", ylab="Cover.Spread")
#Add linear regression model line
abline(out1)
f=seq(0,17.5,0.001)

yf=15.736+-6.836*sqrt(f)
lines(f,yf,col='red', lty=2)

#Residual Plot

par(mfrow=c(2,1))
plot(fitted(out1), resid(out1), main="Fitted vs. Residual New.Off.No.Pen")
plot(fitted(out2), resid(out2), main="Fitted vs. Residual sqrt(New.Off.No.Pen)")

#Transformation Analysis of New.Off.Pen.Yds
out1=lm(y~New.Off.Pen.Yds)
out2=lm(y~sqrt(New.Off.Pen.Yds))

#Obtain Multiple R^2
summary(out1)
summary(out2)

summary(New.Off.Pen.Yds)
#Scatter Plot with the regression lines
plot(New.Off.Pen.Yds, y, main="Scatter Plot of New.Off.Pen.Yds", ylab="Cover.Spread")
#Add linear regression model line
abline(out1)
f=seq(0,135,0.001)

yf=12.6767+-1.8956*sqrt(f)
lines(f,yf,col='red', lty=2)

#Residual Plot

par(mfrow=c(2,1))
plot(fitted(out1), resid(out1), main="Fitted vs. Residual New.Off.Pen.Yds")
plot(fitted(out2), resid(out2), main="Fitted vs. Residual sqrt(New.Off.Pen.Yds)")

#Transformation Analysis of Def.Pass.Yds
out1=lm(y~ Def.Pass.Yds)
out2=lm(y~sqrt(Def.Pass.Yds))
out3=lm(y~log(Def.Pass.Yds))


#Obtain Multiple R^2
summary(out1)
summary(out2)
summary(out3)

summary(Def.Pass.Yds)
#Scatter Plot with the regression lines
plot(Def.Pass.Yds, y, main="Scatter Plot of Def.Pass.Yds", ylab="Cover.Spread")
#Add linear regression model line
abline(out1)
f=seq(36,511,0.001)
yf=13.4897+-0.9487*sqrt(f)
lines(f,yf,col='red', lty=2)
yf2=28.910+-5.522*log(f)
lines(f,yf2, col='blue', lty=2)
#Residual Plot

par(mfrow=c(3,1))
plot(fitted(out1), resid(out1), main="Fitted vs. Residual Def.Pass.Yds")
plot(fitted(out2), resid(out2), main="Fitted vs. Residual sqrt(Def.Pass.Yds)")
plot(fitted(out3), resid(out3), main="Fitted vs. Residual log(Def.Pass.Yds)")


##Transformation Analysis of New.Def.No.Pen
#####
out1=lm(y~ New.Def.No.Pen)
out2=lm(y~sqrt(New.Def.No.Pen))

#Obtain Multiple R^2
summary(out1)
summary(out2)

summary(New.Def.No.Pen)
#Scatter Plot with the regression lines
plot(New.Def.No.Pen, y, main="Scatter Plot of New.Def.No.Pen", ylab="Cover.Spread")
#Add linear regression model line
abline(out1)
f=seq(0,16.5,0.001)

yf=-4.590+1.880*sqrt(f)
lines(f,yf,col='red', lty=2)

#Residual Plot

par(mfrow=c(2,1))
plot(fitted(out1), resid(out1), main="Fitted vs. Residual New.Def.No.Pen")
plot(fitted(out2), resid(out2), main="Fitted vs. Residual sqrt(New.Def.No.Pen)")

#Transformation Analysis of New.Def.Pen.Yds
##
out1=lm(y~New.Def.Pen.Yds)
out2=lm(y~sqrt(New.Def.Pen.Yds))

#Obtain Multiple R^2
summary(out1)
summary(out2)

summary(New.Def.Pen.Yds)
#Scatter Plot with the regression lines
plot(New.Def.Pen.Yds, y, main="Scatter Plot of New.Def.Pen.Yds", ylab="Cover.Spread")
#Add linear regression model line
abline(out1)
f=seq(0,134.5,0.001)

yf=-3.7318+0.5241*sqrt(f)
lines(f,yf,col='red', lty=2)

#Residual Plot

par(mfrow=c(2,1))
plot(fitted(out1), resid(out1), main="Fitted vs. Residual New.Def.Pen.Yds")
plot(fitted(out2), resid(out2), main="Fitted vs. Residual sqrt(New.Def.Pen.Yds)")

#Do discrete transformations with unaltered continuous covariates
output_original=lm(y~Off.Pass.Pct+Off.Pass.Yds+Off.Pass.TD+Off.Run.Avg.Yds+Off.Run.TD+Off.Avg.Yds.Play+Off.Tot.First.Down+Off.No.Pen+New.Off.Pen.Yds+Off.Tot.TO+Def.Pass.Pct+Def.Pass.Yds+Def.Pass.TD+Def.Run.Avg.Yds+Def.Run.TD+Def.Avg.Yds.Play+Def.Tot.First.Down+New.Def.No.Pen+Def.Pen.Yds+Def.Tot.TO)
library(car)
mmps(output_original)

#Discrete Transformation
output_transform=lm(y~Off.Pass.Pct+Off.Pass.Yds+Off.Pass.TD+Off.Run.Avg.Yds+Off.Run.TD+Off.Avg.Yds.Play+sqrt(Off.Tot.First.Down)+Off.No.Pen+sqrt(New.Off.Pen.Yds)+Off.Tot.TO+Def.Pass.Pct+Def.Pass.Yds+Def.Pass.TD+Def.Run.Avg.Yds+Def.Run.TD+Def.Avg.Yds.Play+Def.Tot.First.Down+sqrt(New.Def.No.Pen)+Def.Pen.Yds+Def.Tot.TO)
summary(output_transform)

##Compare Marginal Model Plot
library(car)
mmps(output_transform)

#Stat wise selection
output_stat_wise=lm(y~Off.Pass.Pct+Off.Pass.Yds+sqrt(Off.Pass.TD)+sqrt(Off.Run.Avg.Yds)+Off.Run.TD+Off.Avg.Yds.Play+sqrt(Off.Tot.First.Down)+Off.No.Pen+sqrt(New.Off.Pen.Yds)+sqrt(Off.Tot.TO)+Def.Pass.Pct+Def.Pass.Yds+Def.Pass.TD+Def.Run.Avg.Yds+Def.Run.TD+Def.Avg.Yds.Play+Def.Tot.First.Down+sqrt(New.Def.No.Pen)+Def.Pen.Yds+sqrt(Def.Tot.TO))
step(output_stat_wise, direction='backward')



#Remove covariates one at a time with p-values>0.1, starting with all 20 variables in output_remove
output_remove=lm(y~sqrt(Off.Pass.TD)+Off.Run.TD+sqrt(Off.Tot.First.Down)+sqrt(Off.Tot.TO)+Def.Run.TD+Def.Avg.Yds.Play+Def.Pen.Yds+sqrt(Def.Tot.TO))

summary(output_remove)
plot(output_remove)
plot(fitted(output_remove),resid(output_remove))

##Compare the manual process to the stat wise selection
step(output_remove, direction='backward')


##Find VIF of the model with the 8 covariates
model_1=lm(y~Off.Pass.TD+Off.Run.TD+sqrt(Off.Tot.First.Down)+Off.Tot.TO+Def.Run.TD+Def.Avg.Yds.Play+Def.Pen.Yds+Def.Tot.TO)
library(car)
vif(model_1)

#Rename the variables
sqrt.Off.Pass.TD=sqrt(Off.Pass.TD)
sqrt.Off.Tot.First.Down=sqrt(Off.Tot.First.Down)
sqrt.Off.Tot.TO=sqrt(Off.Tot.TO)
sqrt.Def.Tot.TO=sqrt(Def.Tot.TO)

#Final Model
model_final=lm(y~sqrt.Off.Pass.TD+Off.Run.TD+sqrt.Off.Tot.First.Down+sqrt.Off.Tot.TO+Def.Run.TD+Def.Avg.Yds.Play+Def.Pen.Yds+sqrt.Def.Tot.TO)
summary(model_final)

#VIF of Final Model
library(car)
vif(model_final)

#Plots of Final Model
par(mfrow=c(2,2))
plot(model_final)

library(car)
mmps(model_final)

#Shapiro-Wilk Test 
t1=shapiro.test(resid(model_final))
names(t1)

#Gives exact value
t1$p.value 




##Compare with model with no transformations
output1=lm(y~Off.Pass.Pct+Off.Pass.Yds+Off.Pass.TD+Off.Run.Avg.Yds+Off.Run.TD+Off.Avg.Yds.Play+Off.Tot.First.Down+Off.No.Pen+Off.Pen.Yds+Off.Tot.TO+Def.Pass.Pct+Def.Pass.Yds+Def.Pass.TD+Def.Run.Avg.Yds+Def.Run.TD+Def.Avg.Yds.Play+Def.Tot.First.Down+Def.No.Pen+Def.Pen.Yds+Def.Tot.TO)

#Perform model selection, removing one at a time 

model_m2=lm(y~Off.Pass.TD+Off.Run.TD+Off.Tot.TO+Def.Run.TD+Def.Avg.Yds.Play+Def.Pen.Yds+Def.Tot.TO)

summary(output1_remove)
#Plots
par(mfrow=c(2,2))
plot(output1_remove)

library(car)
mmps(output1_remove)


##Cross-Validate with 2019 Data
b=read.csv("RACCData2019.csv")

#Define variables appropriately 
b$y=b$Team.Score-b$Opp.Score+b$Spread
b$sqrt.Off.Pass.TD=sqrt(b$Off.Pass.TD)
b$sqrt.Off.Tot.First.Down=sqrt(b$Off.Tot.First.Down)
b$sqrt.Off.Tot.TO=sqrt(b$Off.Tot.TO)
b$sqrt.Def.Tot.TO=sqrt(b$Def.Tot.TO)

#Get predicted values based on 2019 data with model_final
try=predict(model_final, b)

#Make sure the predict command was accurate 
try2=summary(model_final)$coef[1]+summary(model_final)$coef[2]*b$sqrt.Off.Pass.TD+summary(model_final)$coef[3]*b$Off.Run.TD+summary(model_final)$coef[4]*b$sqrt.Off.Tot.First.Down+summary(model_final)$coef[5]*b$sqrt.Off.Tot.TO+summary(model_final)$coef[6]*b$Def.Run.TD+summary(model_final)$coef[7]*b$Def.Avg.Yds.Play+summary(model_final)$coef[8]*b$Def.Pen.Yds+summary(model_final)$coef[9]*b$sqrt.Def.Tot.TO

test=try-try2

#Get mean square error
mse_final=(sum(b$y-try2)^2)/118

#Repeat using Model M2
try=predict(model_m2, b)
try2=summary(model_m2)$coef[1]+summary(model_m2)$coef[2]*b$Off.Pass.TD+summary(model_m2)$coef[3]*b$Off.Run.TD+summary(model_m2)$coef[4]*b$Off.Tot.TO+summary(model_m2)$coef[5]*b$Def.Run.TD+summary(model_m2)$coef[6]*b$Def.Avg.Yds.Play+summary(model_m2)$coef[7]*b$Def.Pen.Yds+summary(model_m2)$coef[8]*b$Def.Tot.TO
test=try-try2

mse_m2=(sum(b$y-try2)^2)/118

#Load in Big Ten 2018 Data
c=read.csv("BigTen2018.csv")

#Define variables
c$y=c$Team.Score-c$Opp.Score+c$Spread
c$sqrt.Off.Pass.TD=sqrt(c$Off.Pass.TD)
c$sqrt.Off.Tot.First.Down=sqrt(c$Off.Tot.First.Down)
c$sqrt.Off.Tot.TO=sqrt(c$Off.Tot.TO)
c$sqrt.Def.Tot.TO=sqrt(c$Def.Tot.TO)

#Get predicted values based on 2019 data with model_final
try=predict(model_final, c)
try2=summary(model_final)$coef[1]+summary(model_final)$coef[2]*c$sqrt.Off.Pass.TD+summary(model_final)$coef[3]*c$Off.Run.TD+summary(model_final)$coef[4]*c$sqrt.Off.Tot.First.Down+summary(model_final)$coef[5]*c$sqrt.Off.Tot.TO+summary(model_final)$coef[6]*c$Def.Run.TD+summary(model_final)$coef[7]*c$Def.Avg.Yds.Play+summary(model_final)$coef[8]*c$Def.Pen.Yds+summary(model_final)$coef[9]*c$sqrt.Def.Tot.TO

#Want to get all zeros
test=try-try2

mse_big_ten=(sum(c$y-try2)^2)/98
mse_big_ten

#Assess model_final with Big 12 2018 regular season data
d=read.csv("Big122018.csv")
d$y=d$Team.Score-d$Opp.Score+d$Spread
d$sqrt.Off.Pass.TD=sqrt(d$Off.Pass.TD)
d$sqrt.Off.Tot.First.Down=sqrt(d$Off.Tot.First.Down)
d$sqrt.Off.Tot.TO=sqrt(d$Off.Tot.TO)
d$sqrt.Def.Tot.TO=sqrt(d$Def.Tot.TO)

#Get predicted values based on 2019 data with model_final
try=predict(model_final, d)
try2=summary(model_final)$coef[1]+summary(model_final)$coef[2]*d$sqrt.Off.Pass.TD+summary(model_final)$coef[3]*d$Off.Run.TD+summary(model_final)$coef[4]*d$sqrt.Off.Tot.First.Down+summary(model_final)$coef[5]*d$sqrt.Off.Tot.TO+summary(model_final)$coef[6]*d$Def.Run.TD+summary(model_final)$coef[7]*d$Def.Avg.Yds.Play+summary(model_final)$coef[8]*d$Def.Pen.Yds+summary(model_final)$coef[9]*d$sqrt.Def.Tot.TO

#Want to get all zeros
test=try-try2
test

mse_big_12=(sum(d$y-try2)^2)/70
mse_big_12

#Load 2018 ACC Postseason Data
p=read.csv("RACC2018Postseason.csv")
p$y=p$Team.Score-p$Opp.Score+p$Spread
p$sqrt.Off.Pass.TD=sqrt(p$Off.Pass.TD)
p$sqrt.Off.Tot.First.Down=sqrt(p$Off.Tot.First.Down)
p$sqrt.Off.Tot.TO=sqrt(p$Off.Tot.TO)
p$sqrt.Def.Tot.TO=sqrt(p$Def.Tot.TO)

#Get predicted values based on 2019 data with model_final
try=predict(model_final, p)
try2=summary(model_final)$coef[1]+summary(model_final)$coef[2]*p$sqrt.Off.Pass.TD+summary(model_final)$coef[3]*p$Off.Run.TD+summary(model_final)$coef[4]*p$sqrt.Off.Tot.First.Down+summary(model_final)$coef[5]*p$sqrt.Off.Tot.TO+summary(model_final)$coef[6]*p$Def.Run.TD+summary(model_final)$coef[7]*p$Def.Avg.Yds.Play+summary(model_final)$coef[8]*p$Def.Pen.Yds+summary(model_final)$coef[9]*p$sqrt.Def.Tot.TO

#Want to get all zeros
test=try-try2
test

mse_acc_post=(sum(p$y-try2)^2)/12
mse_acc_post

