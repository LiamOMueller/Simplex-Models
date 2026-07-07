Example_data<-read.csv(file = "Student_Data.csv") # A csv file with 4 multinomial outcomes. rowSums of these rows = 100, 1 additional column to store categorical grouping information.

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(klaR)
library(plotly)
time<-Example_data[,2:5]#Call out the columns of data 
style<-Example_data[,1]#and the grouping variable

#Build a vector for colors
stylecols<-NA

for(i in 1:length(style)){
  if(style[i]=="In-person"){
    stylecols[i]<-"#88CCEE"
  }
  if(style[i]=="Remote"){
    stylecols[i]<-"#CC6677"
  }}


SABDAT<-as.matrix(time)#The barycenter coordinate transformation wants a matrix object
xyz<-quadtrafo(e=SABDAT[,1],f = SABDAT[,4],g = SABDAT[,2],h = SABDAT[,3])#convert the 4 dimension data into the 3d pyramid coordinates x y and z axis.
Falsecords<-quadtrafo(e=c(1,0,0,0), f = c(0,0,0,1), g = c(0,1,0,0),h = c(0,0,1,0))#convert the maximum 4 dimension into the 3d pyramid coordinates so we know where the corners are in 3D space.

###Build the sides of the pyramid
#3 and 4

L3_4<-data.frame(x=as.vector(c(0,.5)),
                 y=as.vector(c(0,0.8660254)),
                 z=as.vector(c(0,0)))
#L3_4<-data.frame(L3_4,group="geometry 2")


#3 and 1
L3_1<-data.frame(x=as.vector(c(0,1 )),
                 y=as.vector(c(0,0)),
                 z=as.vector(c(0,0)))

#3 and 2

L3_2<-data.frame(x=as.vector(c(0,0.5 )),
                 y=as.vector(c(0,0.2886751)),
                 z=as.vector(c(0,0.8164966)))

#4 and 1
L4_1<-data.frame(x=as.vector(c(0.5,1 )),
                 y=as.vector(c(0.8660254,0)),
                 z=as.vector(c(0,0)))

#4 and 2

L4_2<-data.frame(x=as.vector(c(0.5,0.5 )),
                 y=as.vector(c(0.8660254,0.2886751)),
                 z=as.vector(c(0,0.8164966)))

#1 and 2
L1_2<-data.frame(x=as.vector(c(1,0.5 )),
                 y=as.vector(c(0,0.2886751)),
                 z=as.vector(c(0,0.8164966)))

xyzdf<-data.frame(xyz/100,group="geometry 1")#Data frame of coordinates for plotly
figC<-plot_ly()|>add_trace(data=xyzdf,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="markers",color = stylecols,colors=c("#88CCEE","#CC6677","black"),alpha=0.9,scene = "scene4")|> 
  
  add_trace(data = L3_4,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L1_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L3_1,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L4_1,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L4_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L3_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")

fig2C<-figC|>layout(showlegend=F)

vertex<-data.frame(x=as.vector(c(1,.5,0,.5)),y=as.vector(c(-.02,0.2986751,-.02,0.9660254)),z=as.vector(c(-.10,0.8164966,-.10,-.10)),label=as.vector(c("Lecture","Individual","Group","Other")))

fig3C<-fig2C|>add_trace(data=vertex,x=~x,y=~y,z=~z,mode="text",text=~label,type="scatter3d",size=I(16),scene = "scene4")

label_loc<-data.frame(x=as.vector(c(0.1)),y=as.vector(c(-.02)),z=as.vector(c(.85)),label=as.vector(c("Student Responses")))
fig4C<-fig3C|>add_trace(data=label_loc,x=~x,y=~y,z=~z,mode="text",text=~label,type="scatter3d",size=I(16),scene = "scene4")

figCsolo<-fig4C|>layout(title="",scene4=list( xaxis = list(title = '', showgrid = FALSE, showticklabels = FALSE, zerolinecolor = '#ffff'),yaxis = list(title = '', showgrid = FALSE, showticklabels = FALSE, zerolinecolor = '#ffff'),zaxis = list(title = '', showgrid = FALSE, showticklabels = FALSE, zerolinecolor = '#ffff')))
figCsolo


##Centroid plot

SABDAT<-as.matrix(time)#The barycenter coordinate transformation wants a matrix object
xyz<-quadtrafo(e=SABDAT[,1],f = SABDAT[,4],g = SABDAT[,2],h = SABDAT[,3])#convert the 4 dimension data into the 3d pyramid coordinates x y and z axis.
Falsecords<-quadtrafo(e=c(1,0,0,0), f = c(0,0,0,1), g = c(0,1,0,0),h = c(0,0,1,0))#convert the maximum 4 dimension into the 3d pyramid coordinates so we know where the corners are in 3D space.

###Build the sides of the pyramid
#3 and 4

L3_4<-data.frame(x=as.vector(c(0,.5)),
                 y=as.vector(c(0,0.8660254)),
                 z=as.vector(c(0,0)))
#L3_4<-data.frame(L3_4,group="geometry 2")


#3 and 1
L3_1<-data.frame(x=as.vector(c(0,1 )),
                 y=as.vector(c(0,0)),
                 z=as.vector(c(0,0)))

#3 and 2

L3_2<-data.frame(x=as.vector(c(0,0.5 )),
                 y=as.vector(c(0,0.2886751)),
                 z=as.vector(c(0,0.8164966)))

#4 and 1
L4_1<-data.frame(x=as.vector(c(0.5,1 )),
                 y=as.vector(c(0.8660254,0)),
                 z=as.vector(c(0,0)))

#4 and 2

L4_2<-data.frame(x=as.vector(c(0.5,0.5 )),
                 y=as.vector(c(0.8660254,0.2886751)),
                 z=as.vector(c(0,0.8164966)))

#1 and 2
L1_2<-data.frame(x=as.vector(c(1,0.5 )),
                 y=as.vector(c(0,0.2886751)),
                 z=as.vector(c(0,0.8164966)))


AskingQcords<-data.frame(xyz,style)
AverageIP<-as.vector(colMeans(na.omit(AskingQcords[style=="In-person",1:3])))
AverageR<-as.vector(colMeans(na.omit(AskingQcords[style=="Remote",1:3])))

xyzdf<-data.frame(xyz/100,group="geometry 1")
AverageR1<-data.frame(x=as.vector(AverageR[1]/100),y=as.vector(AverageR[2]/100),z=as.vector(AverageR[3]/100),label="Remote")
AverageIP1<-data.frame(x=as.vector(AverageIP[1]/100),y=as.vector(AverageIP[2]/100),z=as.vector(AverageIP[3]/100),label="In-person")
modality<-rbind(AverageIP1,AverageR1)

figD<-plot_ly()|>
  
  add_trace(data=modality,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="markers",color=~label,colors=c("black","#88CCEE","#CC6677"),size=I(900),scene = "scene4")%>% 
  
  add_trace(data = L3_4,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L1_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L3_1,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L4_1,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L4_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")%>%
  
  add_trace(data = L3_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene4")




fig2D<-figD|>layout(showlegend=F)

vertex<-data.frame(x=as.vector(c(1,.5,0,.5)),y=as.vector(c(-.02,0.2986751,-.02,0.9660254)),z=as.vector(c(-.10,0.8164966,-.10,-.10)),label=as.vector(c("Lecture","Individual","Group","Other")))

fig3D<-fig2D|>add_trace(data=vertex,x=~x,y=~y,z=~z,mode="text",text=~label,type="scatter3d",size=I(16),scene = "scene4")

label_loc<-data.frame(x=as.vector(c(0.1)),y=as.vector(c(-.02)),z=as.vector(c(.85)),label=as.vector(c("Centroids")))

fig4D<-fig3D|>add_trace(data=label_loc,x=~x,y=~y,z=~z,mode="text",text=~label,type="scatter3d",size=I(16),scene = "scene4")

figDCenters<-fig4D|>layout(title="",scene4=list( xaxis = list(title = '', showgrid = FALSE, showticklabels = FALSE, zerolinecolor = '#ffff'),yaxis = list(title = '', showgrid = FALSE, showticklabels = FALSE, zerolinecolor = '#ffff'),zaxis = list(title = '', showgrid = FALSE, showticklabels = FALSE, zerolinecolor = '#ffff')))
figDCenters
