## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=FALSE,message=FALSE-----------------------------------------
library(tidyverse)
library(caret)
library(class)


## ------------------------------------------------------------------------
data <- read.csv("wisc_bc_data.csv")[,-1]
data$diagnosis <- factor(data$diagnosis, levels = c("B","M"), ordered = T)
data[,2:ncol(data)] <- sapply(data[,2:ncol(data)], as.numeric)
row.names(data) <- as.numeric(row.names(data))


## ------------------------------------------------------------------------
head(data)


## ------------------------------------------------------------------------
colSums(is.na(data))


## ------------------------------------------------------------------------
table(data$diagnosis)


## ------------------------------------------------------------------------
round(prop.table(table(data$diagnosis))*100, digits = 2)


## ------------------------------------------------------------------------
norm.minmax <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}


## ------------------------------------------------------------------------
norm.zscore <- function(x) {
  return((x - mean(x))/sd(x))
}


## ------------------------------------------------------------------------
data_norm1 <- as.data.frame(lapply(data[,2:ncol(data)], norm.minmax))
data_norm2 <- as.data.frame(lapply(data[,2:ncol(data)], norm.zscore))


## ------------------------------------------------------------------------
test.size <- 0.20
split_row <- as.integer((1 - test.size) * nrow(data))


## ------------------------------------------------------------------------
train1 <- data_norm1[1:split_row,]
test1 <- data_norm1[(split_row+1):nrow(data_norm1),]


## ------------------------------------------------------------------------
train2 <- data_norm2[1:split_row,]
test2 <- data_norm2[(split_row+1):nrow(data_norm2),]


## ------------------------------------------------------------------------
label.train <- data[1:split_row, 1]
label.test <- data[(split_row+1):nrow(data), 1]


## ------------------------------------------------------------------------
k <- ceiling(sqrt(nrow(data)))


## ------------------------------------------------------------------------
#label.train
data_pred1 <- knn(train = train1, test = test1, cl = label.train, k = k)


## ------------------------------------------------------------------------
data_pred2 <- knn(train = train2, test = test2, cl = label.train, k = k)


## ------------------------------------------------------------------------
confusionMatrix(data_pred1, label.test)


## ------------------------------------------------------------------------
confusionMatrix(data_pred2, label.test)



## ----echo=F--------------------------------------------------------------
library(mltools)


## ------------------------------------------------------------------------
scores <- c()
ks <- 2:as.integer(sqrt(nrow(data)))

for (k in ks) {
  preds <- knn(train = train1, test = test1, cl = label.train, k = k)
  f1 <- as.numeric(confusionMatrix(preds, label.test)$byClass["F1"])
  scores <- append(scores, f1)
}


## ------------------------------------------------------------------------
ymin <- 0.95
ymax <- 1.00

plot(ks, scores, type = "l", lwd=2,
     main="F1 Score per num. of neighbors",
     xlab = "# neighbors", ylab = "Score", ylim = c(ymin, ymax))

# Best k
points(ks[which.max(scores)], max(scores), pch=21, col="red")

# Vertical line
lines(x = c(ks[which.max(scores)], ks[which.max(scores)]),
      y = c(ymin, max(scores)),
      lty=3, col="darkgray", lwd=1.5)

# Horizontal line
lines(x = c(min(ks), ks[which.max(scores)]),
      y = c(max(scores), max(scores)),
      lty=3, col="darkgray", lwd=1.5)

legend("bottomright", pch=21, col="red", legend = "Best k")

