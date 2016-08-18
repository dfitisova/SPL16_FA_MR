### Quantlet2: Exploratory data analysis

#In order to let the quantlet work independently of other quantlets, upload the data
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF','TRAN','HOUS','KIDS','SHOP','PERS','EAT','SLEE','TELE','LEAS')

#save as dataframe
time=as.data.frame(time)

#descriptive statistics
summary(time)

#visualize the overall dataset, compare time distributions per each occupation 
boxplot(time, col=topo.colors(10), ylab="time")

#make a single histogram using `ggplot2`package
install.packages('ggplot2')
library("ggplot2")

ggplot(data=time, aes(time$PROF)) +
        geom_histogram(aes(y =..density..),
                       breaks=seq(0, 700, by = 20),
                       col="black",
                       fill="blue",
                       alpha = .2) +
        geom_density(col=2) +
        labs(title="Professional Activity") +
        labs(x="Hours", y="Count")
        
dev.off()

# create a histogram with other graphical parameters, without the density line 

par(mfrow=c(1,1))
ggplot(data=time, aes(time$PROF)) +
        geom_histogram(breaks=seq(0, 900, by =20),
                       col="black",
                       aes(fill=..count..))  +
        geom_density(col=1) +
        labs(title="Work") +
        labs(x="Hours", y="Count")
dev.off()


# create density plots of 9 variables: 

# shorten the dataframe, excluding one variable
time2 <- time[,-9]

# set graphic parameters
par(mfrow=c(3,3),mar=c(2,1,1,1)) 

# write function that creates density plots for each variable
dfplot <- function(data.frame) {
        df <- data.frame
        ln <- length(names(data.frame))
        for(i in 1:ln){
                plot(density(df[,i],main=names(df)[i]), main=colnames(df)[i])
        }
}


# create density plots series 
par(mfrow=c(3,3),mar=c(2,1,1,1))
with(time2, dfplot(time2))
title(main = "Density")
dev.off()



