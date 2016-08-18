###Quantlet 5: Multivariate regression

#In order to let the quantlet work independently of other quantlets, upload the data
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF','TRAN','HOUS','KIDS','SHOP','PERS','EAT','SLEE','TELE','LEAS')

#save as dataframe
time=as.data.frame(time)

#perform a full multivariate regression model with all variables
attach(time)
Model1=lm(TELE~PROF+TRAN+HOUS+KIDS+SHOP+PERS+EAT+SLEE+LEAS,data=time)

#check variance inflation factor
install.packages('fmsb')
library('fmsb')
VIF(Model1)
summary(Model1)

#draw a correlation matrix
install.packages('corrplot')
library('corrplot')
c=cor(time)
corrplot(c, method="circle")

#scatterplots matrix
pairs(~time$PROF+time$TRAN+time$HOUS+time$KIDS+time$SHOP)

#'reduced' multivariate regression (without variables, which can cause multicollinearity)
Model2=lm(TELE~PERS+EAT+SLEE+LEAS, data=time)
summary(Model2)

#diagnostic plots (residuals vs fitted values, normal qq, scale-location, residuals vs leverage)
layout(matrix(c(1,2,3,4),2,2))
plot(Model2)

#Cooks distance
plot(Model2, which=4)

#Bonferroni outlier test
install.packages('car')
library('car')
outlierTest(Model2)

#Leverage plots
leveragePlots(Model2)

#influence plot (outlyingness, leverage,influence of each point)
influencePlot(Model2, id.method="identify", main="Influence Plot" ,sub="Circles")

#distribution of standartized residuals
install.packages('MASS')
library(MASS)
sresid=studres(Model2)
hist(sresid,freq=FALSE, main="Distributionof Studentized Residuals",breaks=10)
xfit=seq(min(sresid), max(sresid), length=40)
yfit=dnorm(xfit)
lines(xfit, yfit)

#Breush-Pagan test for Heteroscedasticity
ncvTest(Model2)

#CERES plots
ceresPlots(Model2)

#check for multicollinearity
VIF(Model2)
sqrt(VIF(Model2)) > 2 

#check for autocorrelation
durbinWatsonTest(Model2)

#test on the models assumptions (skeweness, kurtosis, heteroskedasticity)
install.packages('gvlma')
library('gvlma')
gvmodel2=gvlma(Model2)
summary(gvmodel2)

#to achieve better predictive power: add each of the previously discarded variables to Model2
Model3Prof=lm(TELE~PROF+PERS+EAT+SLEE+LEAS, data=time)
Model3Tran=lm(TELE~TRAN+PERS+EAT+SLEE+LEAS, data=time)
Model3Hous=lm(TELE~HOUS+PERS+EAT+SLEE+LEAS, data=time)
Model3Kids=lm(TELE~KIDS+PERS+EAT+SLEE+LEAS, data=time)
Model3Shop=lm(TELE~SHOP+PERS+EAT+SLEE+LEAS, data=time)

#create manually two dummy variables and regress
GEND <- as.vector(c(0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1)) # women = 1, men = 0.
GEO <- as.vector(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) # West=1, East=0.

# create new dataframe with dummy variables
timeD <- data.frame(cbind(time, GEND, GEO))

# build a model with dummy variables
ModelDummy <- lm( formula = TELE ~ PERS + EAT + SLEE + HOUS + GEND + GEO, data = timeD)
summary(ModelDummy)

# regression diagnostic plot commands and test commands for 'ModelDummy' are identical to those of previous models. 
