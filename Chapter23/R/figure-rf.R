
require(partykit)

set.seed(4350)

treedat <- accelerometer.data

treedat$ACTIVITY <- factor(treedat$ACTIVITY)
treedat <- treedat[treedat$ACTIVITY %in% c("Jogging","Walking","Folding","Drinking"),]
treedat$ACTIVITY<-droplevels(treedat$ACTIVITY)

treedat$RND<-rnorm(n=nrow(treedat))

forest <- cforest(ACTIVITY~XAVG+YAVG+ZAVG+XPEAK+YPEAK+ZPEAK+XVAR+YVAR+ZVAR+YZCOR+XZCOR+XYCOR+RND,
                  treedat, ntree=1000)


vim <- partykit::varimp(forest,conditional=FALSE)
vim.cond <- partykit::varimp(forest,conditional=TRUE)


par(mfrow=c(1,2))
barplot(sort(vim),horiz=TRUE,las=1, main="Marginal")
barplot(sort(vim.cond),horiz=TRUE,las=1, main="Conditional")

