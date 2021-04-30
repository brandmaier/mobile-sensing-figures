library(ggplot2)

if (!exists("accelerometer.data"))
  accelerometer.data <- read.csv("data/accelerometer-all.dat")

if (!exists("mytheme"))
  mytheme <- geom_blank()


feat.data <- accelerometer.data

feat.data$TYPE <- rep(NA, nrow(feat.data))
feat.data$TYPE[feat.data$ACTIVITY %in% c("Walking","Stairs","Sitting","Standing")] <- "Non-hand"
feat.data$TYPE[feat.data$ACTIVITY %in% c("Writing","Walking","Clapping","Typing","Brushing","Folding")] <- "Hand (generic)"
feat.data$TYPE[feat.data$ACTIVITY %in% c("Eating Sandwich","Eating Chips","Eating Soup","Eating Pasta","Drinking")] <- "Hand (eating)"
feat.data$TYPE[feat.data$ACTIVITY %in% c("Kicking","Catch","Dribbling","Jogging")] <- "Sports"


# rename activity labels
feat.data$ACTIVITY[feat.data$ACTIVITY=="Brushing"]="Brushing teeth"
feat.data$ACTIVITY[feat.data$ACTIVITY=="Catch"]="Catching a ball"
feat.data$ACTIVITY[feat.data$ACTIVITY=="Eating Soup"]="Eating soup"
feat.data$ACTIVITY[feat.data$ACTIVITY=="Eating Pasta"]="Eating pasta"
feat.data$ACTIVITY[feat.data$ACTIVITY=="Eating Sandwich"]="Eating sandwich"
feat.data$ACTIVITY[feat.data$ACTIVITY=="Eating Chips"]="Eating chips"
feat.data$ACTIVITY[feat.data$ACTIVITY=="Stairs"]="Taking the stairs"
feat.data$ACTIVITY[feat.data$ACTIVITY=="Folding"]="Folding clothes"

#feat.data$lbl <- as.numeric(factor(feat.data$ACTIVITY))

osv <- c(2,4,2,4,3,3,3,3,3,2,4,4,1,1,1,2,1,2)
#override.shape <- c(15,17,16,3)[osv]#[as.numeric(factor(feat.data$TYPE))]

#override.shape <- 0:17

pl <- ggplot(feat.data)+
  
 # geom_point(size=3,
 #            aes(x=XAVG,y=YVAR,col=ACTIVITY,shape=TYPE))+
 # guides(shape=guide_legend(override.aes=list(shape=override.shape),ncol=2))+
  scale_shape_manual(values=0:17)+
  #geom_text(aes(x=XAVG,y=YVAR,label=lbl))+
  geom_point(size=3,
                         aes(x=XAVG,y=YVAR,col=ACTIVITY, shape=ACTIVITY))+
             
  xlab("Average of sensor's x-axis")+ylab("Variance of sensor's y-axis")+

  mytheme+#+  theme(legend.position="bottom")
guides(col=guide_legend(ncol=2))+
#  scale_shape(guide = FALSE)+
 # scale_linetype(guide = FALSE)+
 guides(colour = guide_legend(ncol=2), shape=guide_legend(ncol=2))+
 # guides(colour = guide_legend(override.aes=list(shape=override.shape), ncol=2)   )+
 
  theme(text=element_text(size=16))
  NULL
(pl)
