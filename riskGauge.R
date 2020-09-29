library(dplyr)
riskGauge <- function(pos,breaks=c(0, 10, 35, 65, 90, 100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  pos_per <-  paste(round(pos,2),"%")#create output for gauge with % 
  
  pos <-ifelse(pos>=100,100,round(pos,1)) #round input value so title below works with decimals 
  title <- case_when(
    between(pos,0,9.9) ~ "Low risk, but still be careful!",
    between(pos,10,34.9)~ "Moderate risk, be careful!",
    between(pos,35,64.9)~ "High risk, stay home!",
    between(pos,65,89.9) ~ "Very high risk, stay home!",
    between(pos,90,100) ~ "Extreme risk, stay home!",
    pos >100 ~ "Extreme risk, stay home!")
  #create title
  
  
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="blue")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="yellow")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="orange")+
    geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.4),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos_per,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) +
    labs(title = title)+
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=18))
}
