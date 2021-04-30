if (!exists("accelerometer.data"))
  accelerometer.data <- read.csv("data/accelerometer-all.dat")

if (!exists("mytheme")) {
  mytheme <- geom_blank()
}

treedat <- accelerometer.data

source("R/rename_activities.R")
treedat <- rename_activities(treedat)

#treedat <- completedata
treedat$ACTIVITY <- factor(treedat$ACTIVITY)
#treedat <- treedat[treedat$ACTIVITY %in% c("Folding","Clapping","Writing"),]
treedat <- treedat[treedat$ACTIVITY %in% c("Jogging","Walking","Folding clothes","Drinking"),]



treedat$ACTIVITY <- droplevels(treedat$ACTIVITY)

require(MASS)

library(gridExtra)

mylda <- MASS::lda(ACTIVITY~XAVG+YVAR, treedat)
myqda <- MASS::qda(ACTIVITY~XAVG+YVAR, treedat)




plotdec <- function(mylda, treedat,title) {
  pr.lda <- predict(mylda)$class
  reso<-250
  
  xx <- seq(min(treedat$XAVG),max(treedat$XAVG),length.out = reso)
  yy <- seq(min(treedat$YVAR),max(treedat$YVAR),length.out=reso)
  grd <- expand.grid(XAVG=xx,YVAR=yy)
  grd$ACTIVITY <- predict(mylda,newdata=grd)$class
  grd$C1 <- ifelse(grd$ACTIVITY=="Walking",0,1)
  grd$C2 <- ifelse(grd$ACTIVITY=="Drinking",0,1)
  grd$C3 <- ifelse(grd$ACTIVITY=="Jogging",0,1)
  grd$C4 <- ifelse(grd$ACTIVITY=="Folding clothes",0,1)
  pl <- ggplot()+
#    geom_point(data=grd,aes(x=XAVG,y=YVAR,col=ACTIVITY))+
    
    # shape=21 : unfilled circle, 22: unfilled rect
    # 23: diamond unfilled, 24 unfilled triangle
  geom_point(col="black",
             data=treedat,aes(x=XAVG,y=YVAR,fill=ACTIVITY,
                              shape=ACTIVITY),
             size=4)+
    scale_shape_manual(values=21:24)+
    
    geom_contour(data=grd,aes(x=XAVG,y=YVAR,z=C1),colour="black")+
    geom_contour(data=grd,aes(x=XAVG,y=YVAR,z=C2),colour="black")+
    geom_contour(data=grd,aes(x=XAVG,y=YVAR,z=C4),colour="black")+
    ggtitle(title)+
    mytheme+
    xlab("Average of \nsensor's x-axis")+
    ylab("Variance of sensor's y-axis")+
    theme(text=element_text(size=16))
  
  return(pl)
}



p1<-plotdec(mylda, treedat, "LDA") + theme(legend.position="bottom")
p2<-plotdec(myqda, treedat, "QDA")

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend<-g_legend(p1)

pl <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))