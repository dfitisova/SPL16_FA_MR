### Quantlet3: Principal Component Analysis (preparatory step before Factor Analysis)

#In order to let the quantlet work independently of other quantlets, upload the data
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF','TRAN','HOUS','KIDS','SHOP','PERS','EAT','SLEE','TELE','LEAS')

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
