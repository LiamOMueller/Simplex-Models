library(Ternary)
library(klaR)
library(scatterplot3d)
library(rgl)
library(pandoc)
library(htmlwidgets)

#####Setup data####
data<-read.csv(file = "")
style<-data[,1]#define the column of differences. This is what the colors will be defined from
time<-dat[,2:5]#4 columns where rows sum to 1 (or 100, but see graph fix below)


#####Set the colors for your plot#####
stylecols<-NA

for(i in 1:length(style)){
  if(style[i]=="In-person"){
    stylecols[i]<-"royalblue"
  }
  if(style[i]=="Remote"){
    stylecols[i]<-"#f06553"
  }}


#####barycenter the data####
SABDAT<-as.matrix(time)
xyz<-quadtrafo(e=SABDAT[,1],f = SABDAT[,4],g = SABDAT[,2],h = SABDAT[,3])#convert the 4D data into the 3D pyramid coords.

######Make the plot in RGL#####
open3d()# Open a new RGL device
bg3d(color = "white") # Setup the background color

#Make the pyramid outline
#3 and 4
segments3d(x=as.vector(c(0,.5)),
           y=as.vector(c(0,0.8660254)),
           z=as.vector(c(0,0)),col="black",lwd=4)

#3 and 1
segments3d(x=as.vector(c(0,1 )),
           y=as.vector(c(0,0)),
           z=as.vector(c(0,0)),col="black",lwd=4)
#3 and 2 
segments3d(x=as.vector(c(0,0.5 )),
           y=as.vector(c(0,0.2886751)),
           z=as.vector(c(0,0.8164966)),col="black",lwd=4)

#4 and 1
segments3d(x=as.vector(c(0.5,1 )),
           y=as.vector(c(0.8660254,0)),
           z=as.vector(c(0,0)),col="black",lwd=4)
#4 and 2
segments3d(x=as.vector(c(0.5,0.5 )),
           y=as.vector(c(0.8660254,0.2886751)),
           z=as.vector(c(0,0.8164966)),col="black",lwd=4)

#1 and 2
segments3d(x=as.vector(c(1,0.5 )),
           y=as.vector(c(0,0.2886751)),
           z=as.vector(c(0,0.8164966)),col="black",lwd=4)
#Add data to plot, in this example, % data had to be scaled back to proportion
rgl.spheres(xyz[,1]/100, xyz[,2]/100, xyz[,3]/100, r = 0.02, color = stylecols,alpha=.5)


#####Label your apex, be careful about which is which!#####
text3d(x = 1,y = -.10,z = 0,texts = "Lecture",col="black",cex=1.4)
text3d(x = .442,y =  0.2986751,z = 0.8164966,texts = "Solo",col="black",cex=1.4)
text3d(x = .0,y =  -.07,z = 0,texts = "Group",col="black",cex=1.4)
text3d(x = .5,y =  0.9660254,z = 0,texts = "Other",col="black",cex=1.4)

######Make the plot in plotly#####

####Build tetrahedron side positions

#1 and 2
L1_2<-data.frame(x=as.vector(c(1,0.5 )),
                 y=as.vector(c(0,0.2886751)),
                 z=as.vector(c(0,0.8164966)))

#1 and 3
L3_1<-data.frame(x=as.vector(c(0,1 )),
                 y=as.vector(c(0,0)),
                 z=as.vector(c(0,0)))

#1 and 4
L4_1<-data.frame(x=as.vector(c(0.5,1 )),
                 y=as.vector(c(0.8660254,0)),
                 z=as.vector(c(0,0)))

#2 and 3

L3_2<-data.frame(x=as.vector(c(0,0.5 )),
                 y=as.vector(c(0,0.2886751)),
                 z=as.vector(c(0,0.8164966)))

#2 and 4

L4_2<-data.frame(x=as.vector(c(0.5,0.5 )),
                 y=as.vector(c(0.8660254,0.2886751)),
                 z=as.vector(c(0,0.8164966)))

#3 and 4

L3_4<-data.frame(x=as.vector(c(0,.5)),
                 y=as.vector(c(0,0.8660254)),
                 z=as.vector(c(0,0)))




#Set data frame for plotly
xyzdf<-data.frame(xyz/100,group="geometry 1")

#Build empty tetrahedron 
figA<-plot_ly()%>%
  
  add_trace(data=xyzdf,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="markers",color = stylecols,colors=c("black","black","black"),alpha=0.3,scene = "scene2")%>% 
  
  add_trace(data = L3_4,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene2")%>%
  
  add_trace(data = L1_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene2")%>%
  
  add_trace(data = L3_1,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene2")%>%
  
  add_trace(data = L4_1,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene2")%>%
  
  add_trace(data = L4_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene2")%>%
  
  add_trace(data = L3_2,x= ~x,y= ~y,z= ~z,type="scatter3d",mode="lines",color="black",colors="black",size=I(10),scene = "scene2")



###Build labels
fig2A<-figA%>%layout(showlegend=F)
#fig2A

vertex<-data.frame(x=as.vector(c(1,.5,0,.5)),y=as.vector(c(-.02,0.2986751,-.02,0.9660254)),z=as.vector(c(-.10,0.8164966,-.10,-.10)),label=as.vector(c("Lecture","Individual","Group","Other")))

###Add points
fig3A<-fig2A%>%add_trace(data=vertex,x=~x,y=~y,z=~z,mode="text",text=~label,type="scatter3d",size=I(16),scene = "scene2")

label_loc<-data.frame(x=as.vector(c(0.1)),y=as.vector(c(-.02)),z=as.vector(c(.85)),label=as.vector(c("A")))
fig4A<-fig3A%>%add_trace(data=label_loc,x=~x,y=~y,z=~z,mode="text",text=~label,type="scatter3d",size=I(16),scene = "scene2")
#fig4A


#library(htmlwidgets)
#saveWidget(fig4A, "Figure1A.html")

#####Part 2, are your groups different?#####

#Find the centers
remotecenter<-colMeans(remotecords)#Originally designed for a remote vs in preson study, you can change these names, but you will have to proofread everything below
IPcenter<-colMeans(IPcords)

#calculate the distance from one to the other

diffs<-remotecenter-IPcenter #Directions in 3d, not the most useful unless you think in barycenters, but you can back trnasform to the 4d data if you want to.

dist<-sqrt(((remotecenter[1]-IPcenter[1])^2)+((remotecenter[2]-IPcenter[2])^2)+((remotecenter[3]-IPcenter[3])^2)) #thanks pythagoras! You can interpret this as the propotion of variation between the two groups you are interested in. 

#How offten is our observed difference happening if the groups are random? A bootstrapping approach to p values.

nulldist<-NA
for(i in 1:100000){#Can modify this for a more precise p value
  tempstyle<-sample(style,size = length(style),replace = FALSE)
  tempremotecords <- xyz[tempstyle=="Remote",] #All the red dots
  tempIPcords <- xyz[tempstyle=="In-person",] 
  tempremotecenter<-colMeans(tempremotecords)
  tempIPcenter<-colMeans(tempIPcords)
  nulldist[i]<-sqrt(((tempremotecenter[1]-tempIPcenter[1])^2)+((tempremotecenter[2]-tempIPcenter[2])^2)+((tempremotecenter[3]-tempIPcenter[3])^2))
}
hist(nulldist)
sum(nulldist>dist)/100000#Make sure this number is equal to the number of times the loop is run.
