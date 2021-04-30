# The first 25 columns are the Prosodic features from audio.
#
#Columns 26-90 are the Mel cepstral features from audio.#
#
#Columns 91-105 is the Formant frequency features from audio.
#
#Columns 106-145 are the Gabor features from images.
#
#The last column is the class label (1 - 6), in which 
#1 represents anger, 2 represents disgust, 
#3 represents fear, 4 represents happiness, 
#5 represent sadness, and 6 represents surprise.#
#
#The features are extracted from Ryerson University RML emotion dataset:
#  
#  http://www.rml.ryerson.ca/rml-emotion-database.html

# FROM: https://github.com/Yifeng-He/Audio-Visual-Emotion-Recognition/blob/master/emotion_data.csv
emo <- read_csv("data/emotion_data.csv",col_names = FALSE)
names(emo)[146]<-"emotion"
names(emo)[1:25] <- paste0("Prosodic",1:25)
names(emo)[26:90] <- paste0("MEL",1:65)
names(emo)[91:105] <- paste0("Formant",1:15)
names(emo)[106:145] <- paste0("Gabor",1:40)

emo_nms <- c("anger","disgust","fear","happiness","sadness","surprise")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols <- gg_color_hue(6)

set.seed(3240)
rowsel <- sample(1:nrow(emo),size = 80)

#emosub <- emo[rowsel,1:25]
emosub <- emo[rowsel,26:90]
emosub <- emo[rowsel,26:30]
#emosub <- emo[rowsel,31:32]
#emosub <- emo[rowsel,106:145]
#emosub <- emo[rowsel, 91:105]
emoclass <- pull(emo[rowsel,ncol(emo)])

df <- data.frame(emosub)

D <- dist(df)
plot(D)

clust <- hclust(D)
dend <- as.dendrogram(clust)
labnum <- labels(dend)

lbs <- emo_nms[ emoclass[labnum]]
library(dendextend)
labels_colors(dend) <- cols[emoclass]
dend <- dend %>% set("labels", lbs)
plot(dend)


#
# prediction 
#
library(party)
forest <- cforest(emotion~., emo)
vim <- sort(varimp(forest),decreasing=TRUE)

barplot(vim[1:10],horiz = FALSE, las=2)


fit <- cmdscale(D,eig=TRUE, k=2) 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = lbs, cex=.7)
