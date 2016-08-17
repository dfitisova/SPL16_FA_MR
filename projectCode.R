### Quantlet1: Preparation


#Import data
time1=read.csv("//Users/DashaF/Documents/R/timebudget/timebudget_red.txt", header=T, sep=";")
time1=time[,-11]

#In order to be able to get 'time' database without access to folder, we also imported it by retyping
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF', 'TRAN', 'HOUS', 'KIDS', 'SHOP', 'PERS' ,'EAT', 'SLEE' ,'TELE', 'LEAS')

#Check if table from folder equals to retyped one
identical(time, time1)

#save as dataframe
time=as.data.frame(time)

#-----------------------------------------------------------------------------------------

### Quantlet2: Exploratory data analysis

#In order to let the quantlet work independently of other quantlets, upload the data
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF', 'TRAN', 'HOUS', 'KIDS', 'SHOP', 'PERS' ,'EAT', 'SLEE' ,'TELE', 'LEAS')

#save as dataframe
time=as.data.frame(time)

#descriptive statistics
summary(time)

#visualize the overall dataset, compare time distributions per each occupation 
boxplot(time, col=topo.colors(10), ylab="time")

#make a histogram using `ggplot2`package
#pdf(file="histPROF.pdf")
#par(mfrow=c(1,1))
#install.packages('ggplot2')
#library('ggplot2')
#ggplot(data=time, aes(time$PROF)) + geom_histogram(aes(y=density),breaks=seq (0,700, by=20), col="black", fill="blue", alpha=.2) + geom_density(col=2) + labs(title="Professional Activity") + labs(x="Hours", y="Count")
#dev.off()

#Plot densities (separated)
#par(mfrow=c(3,3), mar=c(2,1,1,1))
#dfplot=function(data.frame){df=data.frame 
#ln=length(names(data.frame))
#for(i in 1:ln){plot(density(df[ ,i], main=names(df)[i], main=colnames(df)[i]))}}
#dfplot(time)
#-----------------------------------------------------------------------------------------

### Quantlet3: Principal Component Analysis (preparatory step before Factor Analysis)

#In order to let the quantlet work independently of other quantlets, upload the data
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF', 'TRAN', 'HOUS', 'KIDS', 'SHOP', 'PERS' ,'EAT', 'SLEE' ,'TELE', 'LEAS')

#save as dataframe
time=as.data.frame(time)

#Run PCA to determine number of factors
pca.time=princomp(time, scores=T, cor=T)
summary(pca.time)

#Plot the scree plot
plot(pca.time) #as hystogram
plot(pca.time, type="l", main=NULL) #as line

#PC loadings
pca.time$loadings

#Biplots
#3 plots side by side
par(mfrow=c(1,3)) #3 in a row
bip1=biplot(pca.time, choices=1:2, cex.lab=1.5, cex=1.3)
bip2=biplot(pca.time, choices=2:3, cex.lab=1.5, cex=1.3)
bip3=biplot(pca.time, choices=c(1,3), cex.lab=1.5, cex=1.3)
dev.off() #switch off the graphics properties

#Representation of the variables in PC space
r=nrow(time) #number of rows
c=ncol(time) #number of columns
x1=sqrt((r-1)*apply(time,2,var)) #estimated std error for each variable
x2=time-matrix(apply(as.matrix(time),2,mean),nrow=r,ncol=c,byrow=T) #X-mean(X)
x=as.matrix(x2/matrix(x1,nrow=r,ncol=c,byrow=T)) #standardized data
g<-eigen(t(x)%*%(x)) #(X transposed)*X
g1=g$values #Eigenvalues
g2=g$vectors #Eigenvectors

#1PC vs 2PC
b=g2[,1:2] #first 2 vectors corresponding to largest 2 Eigenvalues
z=b*sqrt(matrix(g1[1:2],nrow(b),ncol(b),byrow=TRUE)) #coordinates of variables
ucircle=cbind(cos((0:360)/180*pi),sin((0:360)/180*pi))#boundary circle
s=plot(ucircle,type="l",lty="solid",col="blue",xlab="First Factor",ylab="Second Factor",cex.lab=1.6,cex.axis=1.2,cex.main=1.8,lwd=2)
abline(h=0.0,v=0.0)
label=c("prof","tran","hous","kids","shop","pers","eati","slee","tele","leis")
text(z,label,cex=1.6)

#2PC vs 3PC
t=g2[,2:3]
z=t*sqrt(matrix(g1[2:3],nrow(t),ncol(t),byrow=TRUE)) 
ucircle=cbind(cos((0:360)/180*pi),sin((0:360)/180*pi))
p=plot(ucircle,type="l",lty="solid",col="red",xlab="Second Factor",ylab="Third Factor",cex.lab=1.6,cex.axis=1.2,cex.main=1.8,lwd=2)
abline(h=0.0,v=0.0)
label=c("prof","tran","hous","kids","shop","pers","eati","slee","tele","leis")
text(z,label,cex=1.6)

#1PC vs 3PC
q=g2[,1:3] 
z=q*sqrt(matrix(g1[1:3],nrow(q),ncol(q),byrow=TRUE)) 
ucircle=cbind(cos((0:360)/180*pi),sin((0:360)/180*pi))
e=plot(ucircle,type="l",lty="solid",col="green",xlab="First Factor",ylab="Third Factor ",cex.lab=1.6,cex.axis=1.2,cex.main=1.8,lwd=2)
abline(h=0.0,v=0.0)
label=c("prof","tran","hous","kids","shop","pers","eati","slee","tele","leis")
text(z,label,cex=1.6)

#Plot side by side
par(mfrow=c(1,3))
e
s
p
dev.off()

#Initial observations in 3-dim
install.packages("rgl")
library(rgl)
plot3d(pca.time$scores[,1:3])

#Initial observations in 3-dim #2
install.packages("pca3d")
library(pca3d)
pca3d(pca.time, components=1:3)

#-----------------------------------------------------------------------------------------

###Quantlet 4: Factor Analysis

#In order to let the quantlet work independently of other quantlets, upload the data
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF', 'TRAN', 'HOUS', 'KIDS', 'SHOP', 'PERS' ,'EAT', 'SLEE' ,'TELE', 'LEAS')

#save as dataframe
time=as.data.frame(time)

#The function factanal() internally uses solve() which is a numerical way to calculate the inverse.Some of the numbers in "time" used in the inverse calculation are very small (whereas others are too big), it assumes that they are zero, leading to the assumption that it is a singular matrix. Have to TRANSFORM data-->SQRT (there ate 0s in dataset->no exp() and no log() can be used)
time2=sqrt(time)

  ##Part 1: FA without rotation

fa.time=factanal(time2, factors=3, rotation="none") #the number of factors was defined by PCA
fa.time

#Factor loadings
load=fa.time$loadings
load

#Factors are not distinct->need rotation
pairs(load) #no correlation-> orthogonal rotation-> for ex., varimax

  ##Part 2: varimax rotation

fa.time.rot=factanal(time2, factors=3, rotation="varimax")
fa.time.rot
load.rot=fa.time.rot$loadings
load.rot

#Plot factor loiadings by factors 
pairs(fa.time.rot$loadings, col=1:ncol(time2), upper.panel=NULL, pch=16, cex=2.3)
par(xpd=TRUE) 
legend('topright', bty='n', pch=16, cex=0.65, pt.bg=TRUE, col=1:ncol(time2), attr(fa.time.rot$loadings, 'dimnames')[[1]], title="Variables")
par(mfrow=c(1,3))

#Plot loadings: factor 1 by factor 2
load.rot <- fa.time.rot$loadings[,1:2] 
plot(load.rot,type="n",xlab="Factor 1",ylab="Factor 2", cex.lab=1.6) #set up plot 
text(load.rot,labels=names(time2),cex=1.6) #add variable names
abline(h=0,v=0)

#Plot loadings: factor 2 by factor 3
load.rot2 <- fa.time.rot$loadings[,2:3] 
plot(load.rot2,type="n",xlab="Factor 2",ylab="Factor 3", cex.lab=1.6) #set up plot 
text(load.rot2,labels=names(time2),cex=1.6) #add variable names
abline(h=0,v=0)

#Plot loadings: factor 1 by factor 3
load.rot3=cbind(fa.time.rot$loadings[,1],fa.time.rot$loadings[,3])
plot(load.rot3,type="n",xlab="Factor 1",ylab="Factor 3", cex.lab=1.6) #set up plot 
text(load.rot3,labels=names(time2),cex=1.6) #add variable names
abline(h=0,v=0)

#-----------------------------------------------------------------------------------------

###Quantlet 5: Multivariate regression

###### !!!!!!!!! ADD GENDER AND GEO TO THIS TABLE
#In order to let the quantlet work independently of other quantlets, upload the data
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF', 'TRAN', 'HOUS', 'KIDS', 'SHOP', 'PERS' ,'EAT', 'SLEE' ,'TELE', 'LEAS')

#save as dataframe
time=as.data.frame(time)

#perform a full multivariate regression model
Model1=lm(TELE~PROF+TRAN+HOUS+KIDS+SHOP+PERS+EAT+SLEE+LEAS,data=time)

#check variance inflation factor
install.packages('fmsb')
library('fmsb')
VIF(Model1)
summary(Model1)

#correlation matrix
install.packages('corrplot')
library('corrplot')
c=cor(time)
corrplot(c, method="circle")

#scatterplots matrix
pairs(~time$PROF+time$TRAN+time$HOUS+time$KIDS+time$SHOP)

#'reduced' multivariate regression (without variables, which can cause multicollinearity)
Model2=lm(TELE~PERS+EAT+SLEE+LEAS, data=time)
summary(Model2)
lm(formula=TELE~PERS+EAT+SLEE+LEAS, data=time)

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
#       !!!!!!!!!!   NEXT LINE IS NOT WORKING (no function studres)
sresid=studres(Model2)
hist(sresid,freq=FALSE, main="Distributionof Studentized Residuals",breaks=10)
xfit=seq(min(sresid), max(sresid), length=40)
yfit=dnorm(xfit)
lines(xfit, yfit)

#CERES plots
ceresPlots(Model2)

#check for multicollinearity
VIF(Model2)
sqrt(VIF(Model2))

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
lm( formula = TELE ~ PERS + EAT + SLEE + HOUS + GEND + GEO, data = TimebudgetD)
summary(ModelDummy)
## !!!!!!!!!!!   Probably further the same as above? --> VIF, Bonferroni, CERES usw.



