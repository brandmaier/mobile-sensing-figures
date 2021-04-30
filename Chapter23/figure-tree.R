require(partykit)

if (!exists("my_theme")) my_theme <- theme_light()

if (!exists("accelerometer.data"))
  accelerometer.data <- read.csv("data/accelerometer-all.dat")


treedat <- accelerometer.data
#treedat <- completedata
treedat$ACTIVITY <- factor(treedat$ACTIVITY)
#treedat <- treedat[treedat$ACTIVITY %in% c("Folding","Clapping","Writing"),]
treedat <- treedat[treedat$ACTIVITY %in% c("Jogging","Walking","Folding","Drinking"),]

#treedat <- treedat[treedat$ACTIVITY %in% c("Walking","Standing","Folding","Eating Sandwich"),]
treedat$ACTIVITY<-droplevels(treedat$ACTIVITY)
#levels(treedat$ACTIVITY) <-c("Walking","Jogging","Standing","Brushing Teeth")
#tree <- ctree(ACTIVITY~XAVG+YAVG+ZAVG+XPEAK+YPEAK+ZPEAK+XVAR+YVAR+ZVAR+YZCOR+XZCOR+XYCOR+RESULTANT, treedat)
tree <- ctree(ACTIVITY~XAVG+YAVG+ZAVG+XPEAK+YPEAK+ZPEAK+XVAR+YVAR+ZVAR+YZCOR+XZCOR+XYCOR, treedat)


#tree <- ctree(ACTIVITY~XAVG+XVAR, treedat)

#png("machinelearning_files/figure-docx/ab_figtree_left.png")

#fcols <- rev(c("#C77CFF", "#00BFC4" ,"#F8766D" ,"#7CAE00"))
fcols <- c("#F8766D","#7CAE00","#00BFC4","#C77CFF")

plot(tree,margins = c(3, 0, 0, 0), tp_args=list(rot=90,fill=fcols,gap=0,just=c("right","top")),gp = gpar(fontsize = 10),
     newpage=TRUE, pop=TRUE)

#dev.off()
#plot(pl)
