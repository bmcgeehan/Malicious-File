setwd("/Users/Brendan/Desktop/Methods III/")
library(pROC)
library(mlbench)
library(caret)


##Load the data file from Excel
table <- read.csv("antivirus.train.csv", header= FALSE)

##Create a list object from the data. Each element of the list represents a single observation (row)
data.list <- list()
for(i in 1:dim(table)[1])
{
  data.list[[i]] <- unname(table[i,])
}
data.list <- split(table, seq(nrow(table)))

##Recode the data as a matrix whose first column is the labels 
##and whose remaining columns 2:531 are binary indicators of whether that feature (column) was found to be sigificant for the observation (row)
str <- ""
data.matrix <- matrix(c(rep(0, 531*373)), nrow=373)
counter <- 0

##Code the (binary) predictor matrix
for (i in 1:373)
{
  for (j in 2:254)
  {
    if (data.list[[i]][j] == -1)
    {
      break
    }
    else if (data.list[[i]][j] != -1)
    {
      str <- as.numeric(strsplit(as.character(unlist(data.list[[i]][j])), ":")[[1]][1])
      data.matrix[i, str] <- 1
    }
  }
}
class <- rep(0, 373)
data.matrix <- cbind(class, data.matrix)
data.matrix <- as.data.frame(data.matrix)
##Code the label (First) column
for (i in 1:373)
{
  str <- as.numeric(unlist(data.list[[i]][1]))
  data.matrix[i,1] <- str
}

##Save the recoded data file
saveRDS(data.matrix, file="antivirus.train.rda")


##Clearing out any vectors that show no variability
colnames(data.matrix) = c("Class", seq(1:ncol(data.matrix[,-1])))
cols = apply(data.matrix, 2, function(i) length(unique(i)) > 1)
datareduced <- data.matrix[,cols]






#Correlation Distance Clustering
dist <- as.dist(1-correlation)
clust <- hclust(as.dist(1-correlation) )##Want to change this distance metric
corgroups <- cutree(clust, k=20)


#Jaccard Distance Clustering
library(vegan)
dist2 <- vegdist(t(datareduced[,-1]), method="jaccard")
dist2 <- as.matrix(dist2)
clust2 <- hclust(dist2)
plot(clust2)
groups <- cutree(clust2, h=.95)

# Get order
ord <- clust$order
ord2 <- clust2$order

#Jaccard Distance Plot
df <- dist2[ord2,]
p <- df %>%
  melt() %>% 
  ggplot(aes(Var1, Var2, fill = value)) + geom_tile() + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + scale_fill_gradient2(low = "red",  high = "blue")
ggplotly(p)

##Correlation Plot
df <- correlation[ord,]
p <- df %>%
  melt() %>% 
  ggplot(aes(Var1, Var2, fill = value)) + geom_tile() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank()) + scale_fill_gradient2(low = "red",  high = "blue") + labs(title="Feature Correlation Matrix", x="Variables", y="Variables", colour="Cor")
ggplotly(p)

#Logistic PCA/ Sparse Logistic PCA

library(logisticPCA)
svmModel <- logisticSVD(dat, 2)
svd4 <- logisticSVD(dat, 16)
csvd <- cv.lsvd(dat, 2:5)
plot(svmModel)
cvpca <- cv.lpca(dat, 2)
logpca_model = logisticPCA(dat, k = 2, m = which.min(cvpca))
dat <- as.matrix(dat)
row.names(dat) <- c(rep("Nonmalicious", 72), rep("Malicious", 301))
class <- row.names(dat)
svddata <- as.data.frame(svmModel$A)
colnames(svddata) <- c("PC1", "PC2")


logpcadata <- as.data.frame(logpca_model$PCs)
colnames(logpcadata) <- c("PC1", "PC2")

clogpca_model = convexLogisticPCA(dat, k = 2, m = which.min(cvpca))
cpcadata <- as.data.frame(clogpca_model$PCs)
colnames(cpcadata) <- c("PC1", "PC2")
ggplot(svddata, aes(PC1, PC2)) + geom_point(data=svddata[1:72,], aes(colour="NonMalicious")) + geom_point(data=svddata[73:373,], aes(colour="Malicious")) + scale_colour_manual(values = c('Malicious'='blue','NonMalicious'='red')) + ggtitle("Principal Components") + labs(colour="File Type")

ggplot(logpcadata, aes(PC1, PC2)) + geom_point(data=logpcadata[1:72,], aes(colour="NonMalicious")) + geom_point(data=logpcadata[73:373,], aes(colour="Malicious")) + scale_colour_manual(values = c('Malicious'='blue','NonMalicious'='red')) + ggtitle("Principal Components") + labs(colour="File Type")

ggplot(cpcadata, aes(PC1, PC2)) + geom_point(data=cpcadata[1:72,], aes(colour="NonMalicious")) + geom_point(data=cpcadata[73:373,], aes(colour="Malicious")) + scale_colour_manual(values = c('Malicious'='blue','NonMalicious'='red')) + ggtitle("Principal Components") + labs(colour="File Type")

##Want to Explore by Group (show histograms and correlations by group)
#Just splti set by 1 and -1s

data1 <- datareduced[which(datareduced$Class == 1),]
data2 <- datareduced[which(datareduced$Class == -1),]
colmeans1 <- colMeans(data1[,-1])
colmeans2 <- colMeans(data2[,-1])
meandiffs <- abs(colmeans1 - colmeans2)
sort(meandiffs)

totals <- rowSums(dat)
totals <- as.data.frame(cbind(datareduced$Class, totals))
cor(totals$V1, totals$totals)




###Correlation with output
corxy <- cor(datareduced[,-1], as.numeric(levels(datareduced$Class)[datareduced$Class]))
y <- as.numeric(levels(datareduced$Class)[datareduced$Class])
sort(abs(corxy), decreasing=TRUE, index.return=TRUE)

cor(dat[,c(11,12,124,125,54)])


####Models


control <- trainControl(method="repeatedcv", number=10, repeats=10, 
                        classProbs = TRUE, summaryFunction = multiClassSummary, 
                        verboseIter = TRUE, savePredictions = TRUE)
# train the SVM
newdata <- datareduced
levels(newdata$Class)[levels(newdata$Class)==1] <- "Nonmalicious"
levels(newdata$Class)[levels(newdata$Class)==-1] <- "Malicious"
levels(newdata$Class)
newdata$Class[1:72] <- "Nonmalicious"
set.seed(7)
modelSvm <- train(Class~., data=newdata, method="svmRadial", trControl=control)
# train the RF model
set.seed(7)
modelrf <- train(Class~., data=newdata, method="rf", trControl=control, importance=TRUE)
# train the Logistic model
set.seed(7)
modellog <- train(Class~., data=newdata, method="glmnet", family="binomial", trControl=control)
# collect resamples
results <- resamples(list(SVM=modelSvm, RF=modelrf, LL=modellog))
bwplot(results)

bestpreds <- modelpred[which(modelpred$alpha == .55 & modelpred$lambda > .1),]

modelpred[which(modelpred$pred != modelpred$obs & modelpred$alpha == 1 & modelpred$lambda > .1),]

##Linear SVM

set.seed(7)
modelSvm2 <- train(Class~., data=newdata2, method="svmLinear", trControl=control)

#Boosted Model
control2 <- trainControl(method="cv", 
                        verboseIter = TRUE, savePredictions = TRUE)
modelgbm <- train(x=newdata2[,-1], y=newdata2$Class, method="gbm", trControl=control2)

#Reduced Models
newdata2 <- newdata[, colnames(newdata) %in% c("Class","19","20", "139", "140", "447")]
set.seed(7)
modelSvm2 <- train(Class~., data=newdata2, method="svmRadial", trControl=control)

modellog2<- train(Class~., data=newdata2, method="glm", trControl=control)

preds <- modelSvm3$pred

#changed control to 10 repeats
#switch between ica and pca in preprocess param
library(fastICA)
set.seed(10)
modelSvm3 <- train(Class~., data=newdata2, method="svmRadial", preProcess="pca", trControl=control)
set.seed(10)
modellog3 <- train(Class~., data=newdata2, method="glmnet", preProcess="pca", trControl=control)

ic <- fastICA(newdata2[,-1], 2)
ics <- as.data.frame(ic$S)
ggplot(ics, aes(V1, V2)) + geom_point(data=ics[1:72,], aes(colour="NonMalicious")) + geom_point(data=ics[73:373,], aes(colour="Malicious")) + scale_colour_manual(values = c('Malicious'='blue','NonMalicious'='red')) + ggtitle("Independent Components") + labs(colour="File Type")


##Thought: Lasso Logistic over others because interpretability and estimation