if (!exists("accelerometer.data"))
  accelerometer.data <- read.csv("data/accelerometer-all.dat")

if (!exists("mytheme")) {
  mytheme <- geom_blank()
}

treedat <- accelerometer.data
#treedat <- completedata

source("R/rename_activities.R")
treedat <- rename_activities(treedat)
treedat$ACTIVITY <- factor(treedat$ACTIVITY)
#treedat <- treedat[treedat$ACTIVITY %in% c("Folding","Clapping","Writing"),]
treedat <- treedat[treedat$ACTIVITY %in% c("Jogging","Walking","Folding clothes","Drinking"),]


treedat$ACTIVITY <- droplevels(treedat$ACTIVITY)

require(MASS)

library(gridExtra)

mylda <- MASS::lda(ACTIVITY~XAVG+YVAR, treedat)

confmat <- table(predict(mylda)$class, treedat$ACTIVITY)
