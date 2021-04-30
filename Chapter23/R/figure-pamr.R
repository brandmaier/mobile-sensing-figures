require(pamr)

set.seed(235498)


treedat <- accelerometer.data
#treedat <- completedata
#treedat <- treedat[treedat$ACTIVITY %in% c("Folding","Clapping","Writing"),]
#treedat <- treedat[treedat$ACTIVITY %in% c("Jogging","Walking","Folding","Drinking"),]
treedat$ACTIVITY <- as.character(treedat$ACTIVITY)
#treedat <- treedat[treedat$ACTIVITY %in% c("Jogging","Walking","Stairs"),]

treedat$ACTIVITY[!(treedat$ACTIVITY %in% c("Jogging","Walking","Drinking"))] <- "Other"

unique(treedat$ACTIVITY)

treedat$ACTIVITY <- factor(treedat$ACTIVITY)


#treedat$ACTIVITY <- droplevels(treedat$ACTIVITY)


pamr.data <- list(x=t(treedat[,-1]),y=treedat$ACTIVITY, genenames=names(treedat[,-1]))

model <-  pamr::pamr.train(data=pamr.data)

cvmodel <- pamr::pamr.cv(fit=model, data=pamr.data)


#pamr.plotcen(model, pamr.data,7)

#pamr.plotcen(model, pamr.data,threshold = 90)

pamr.plotcen(model, pamr.data,threshold = 20.855)
