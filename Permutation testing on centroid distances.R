library(tidyverse)
library(FactoMineR)
library(factoextra)
library(klaR)

Example_data<-read.csv(file = "Student_Data.csv") # A csv file with 4 multinomial outcomes. rowSums of these rows = 100, 1 additional column to store categorical grouping information.

time<-Example_data[,2:5]#Call out the columns of data 
style<-Example_data[,1]#and the grouping variable


SABDAT<-as.matrix(time)#The barycenter coordinate transformation wants a matrix object
xyz<-quadtrafo(e=SABDAT[,1],f = SABDAT[,4],g = SABDAT[,2],h = SABDAT[,3])#convert the 4 dimension data into the 3d pyramid coordinates x y and z axis.

AskingQcords<-data.frame(xyz,style)
AverageIP<-as.vector(colMeans(na.omit(AskingQcords[style=="In-person",1:3])))
AverageR<-as.vector(colMeans(na.omit(AskingQcords[style=="Remote",1:3])))
AverageTotal<-as.vector(colMeans(na.omit(AskingQcords[,1:3])))

distasnces<-AverageIP-AverageR
squares<-distasnces^2
sums<-sum(squares)
Dist<-sqrt(sums)
#Is that Distance bigger than the Distance  by chance alone?

Observed_bigger_than_random<-NA
for(i in 1:10000){
  randomstyle<-sample(style,length(style)) #Just shuffle the colors
  tempIP<-colMeans(na.omit(AskingQcords[randomstyle=="In-person",1:3]))
  tempR<-colMeans(na.omit(AskingQcords[randomstyle=="Remote",1:3]))
  tempD<-sqrt(sum((tempIP-tempR)^2)) #Calculate the random distance
  Observed_bigger_than_random[i]<- Dist > tempD #Is random bigger than observed?
}

pval<-(1-sum(Observed_bigger_than_random)/10000)#Inherently two tailed as the squaring needed to calculate distance makes all observed distances positive.