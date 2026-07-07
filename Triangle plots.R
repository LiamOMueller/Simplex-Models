library(tidyverse)
library(FactoMineR)
library(factoextra)
library(klaR)
library(plotly)
library(Ternary)

##### 3 multinomial outcomes#####
B_cell<-c(23.8,27.5,26.1,52.3,63,65.9)
T_cell<-c(60.3,60.7,60,40.7,30.2,27.8)
Macrophage<-c(15.9,11.8,13.9,7,6.8,6.3)
Condition<-c("Control","Control","Control","KO","KO","KO")
Cell_example<-data.frame(B_cell,T_cell,Macrophage,Condition)


## Building a ternary plot in plotly

# A function for titles on the corners
axis <- function(title) {
  list(
    title = title,
    titlefont = list(
      size = 20
    ),
    tickfont = list(
      size = 15
    ),
    tickcolor = 'rgba(0,0,0,0)',
    ticklen = 5
  )
}
 #Building the triangle and populating points
fig <- Cell_example |> plot_ly()
fig2A <- fig |> add_trace(
  type = 'scatterternary',
  mode = 'markers',
  a = ~B_cell,
  b = ~T_cell,
  c = ~Macrophage,
  text = ~Condition,
  marker = list( 
    symbol = 100,
    color = c('#88CCEE','#88CCEE','#88CCEE', '#CC6677','#CC6677','#CC6677'),
    size = 14,
    line = list('width' = 2)
  )
)
# Incorporating labels
fig3A <- fig2A |> layout(
  title = "",
  ternary = list(
    sum = 100,
    aaxis = axis('B cells'),
    baxis = axis('T cells'),
    caxis = axis('Macrophage')
  )
)
#adding larger margins to help visuals
m<-list(
  l = 80,
  r = 80,
  b = 80,
  t = 80
)
fig4A<-fig3A|>layout(
  margin = m)



fig4A