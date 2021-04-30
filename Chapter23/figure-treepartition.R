
if (!exists("mytheme"))  mytheme<- papaja::theme_apa()

if (!exists("accelerometer.data"))
  accelerometer.data <- read.csv("data/accelerometer-all.dat")

treedat <- accelerometer.data
#treedat <- completedata
treedat$ACTIVITY <- factor(treedat$ACTIVITY)
#treedat <- treedat[treedat$ACTIVITY %in% c("Folding","Clapping","Writing"),]
treedat <- treedat[treedat$ACTIVITY %in% c("Jogging","Walking","Folding","Drinking"),]

#treedat <- treedat[treedat$ACTIVITY %in% c("Walking","Standing","Folding","Eating Sandwich"),]
treedat$ACTIVITY<-droplevels(treedat$ACTIVITY)


treepart <- ggplot2::ggplot(data=treedat,
                            mapping=aes(x=XAVG,y=YVAR,col=ACTIVITY,
                                        shape=ACTIVITY))+geom_point(size=3)+
  geom_segment(aes(x = 2.748, y = 1, xend = 2.748, yend = 0),col='black',lty="dashed")+
  geom_segment(aes(x = -6.458, y = 1, xend = -6.458, yend = 0),col='black',lty="dashed")+
  geom_segment(aes(x = -6.458, y = .616, xend = 2.748, yend = .616),col='black',lty="dashed")+
  mytheme+
  theme(legend.position = c(0.8, 0.8))+
  theme(text=element_text(size=16))+
  xlab("Average of sensor's x-axis")+
  ylab("Variance of sensor's y-axis")
 # (treepart)+

plot(treepart)

#ggsave(filename="machinelearning_files/figure-docx/ab_figtree_right.png",plot = treepart,width=6,height=6)

