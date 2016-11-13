install.packages("FNN")
library(xlsx)
install.packages("rknn")
install.packages("neuralnet")
install.packages("stats")

#---

bacteria.t.row <- read.xlsx("C:/2015/금강level1과23을2_3.xlsx", sheetName= "Training")
bacteria.v.row <- read.xlsx("C:/2015/금강level1과23을2_3.xlsx", sheetName= "Validation")

bacteria.t <- within (bacteria.t.row, {
  Site <- NULL;
  No <- NULL;
  Date <- NULL;
})

bacteria.t$level <- as.factor(bacteria.t$level)

bacteria.v <- within (bacteria.v.row, {
  Site <- NULL;
  No <- NULL;
  Date <- NULL;
})

bacteria.v$level <- as.factor(bacteria.v$level)


#-- model 1 / kknn - decision tree
train.knn.1 <- cbind(temp.1=bacteria.t$temp.1, temp.2=bacteria.t$temp.2, inflow.1=bacteria.t$inflow.1, height.1=bacteria.t$height.1, level=bacteria.t$level)
val.knn.1 <- cbind(temp.1=bacteria.v$temp.1, temp.2=bacteria.v$temp.2, inflow.1=bacteria.v$inflow.1, height.1=bacteria.v$height.1, level=bacteria.v$level)
train.knn.1 <- as.data.frame(train.knn.1)
val.knn.1 <- as.data.frame(val.knn.1)
train.knn.1$level <- as.factor(train.knn.1$level)
val.knn.1$level <- as.factor(val.knn.1$level)


bacteria.kknn.1 <- train.kknn(level~., train.knn.1, kmax = 11, kernel = "optimal")
summary(bacteria.kknn.1)

pred.kknn.1 <- predict(bacteria.kknn.1, val.knn.1[, -5])
pred.kknn.1

CM.kknn.1 <- table(val.knn.1[, 5], pred.kknn.1)
CM.kknn.1

accuracy.kknn.1 <- (sum(diag(CM.kknn.1)))/sum(CM.kknn.1)
accuracy.kknn.1

plot(bacteria.kknn.1)


#--- model 2 / kknn-logistic

train.knn.2 <- cbind(wind_mean.3=bacteria.t$wind_mean.3, TP.1=bacteria.t$TP.1, height.2=bacteria.t$height.2, wind_mean.1=bacteria.t$wind_mean.1, outflow.1=bacteria.t$outflow.1, temp.2=bacteria.t$temp.2, rain.1=bacteria.t$rain.1, sun.2=bacteria.t$sun.2, level=bacteria.t$level)
val.knn.2 <- cbind(wind_mean.3=bacteria.v$wind_mean.3, TP.1=bacteria.v$TP.1, height.2=bacteria.v$height.2, wind_mean.1=bacteria.v$wind_mean.1, outflow.1=bacteria.v$outflow.1, temp.2=bacteria.v$temp.2, rain.1=bacteria.v$rain.1, sun.2=bacteria.v$sun.2, level=bacteria.v$level)
train.knn.2 <- as.data.frame(train.knn.2)
val.knn.2 <- as.data.frame(val.knn.2)
train.knn.2$level <- as.factor(train.knn.2$level)
val.knn.2$level <- as.factor(val.knn.2$level)


bacteria.kknn.2 <- train.kknn(level~., train.knn.2, kmax = 11, kernel = "optimal")
summary(bacteria.kknn.2)

pred.kknn.2 <- predict(bacteria.kknn.2, val.knn.2[, -9])
pred.kknn.2

CM.kknn.2 <- table(val.knn.2[, 9], pred.kknn.2)
CM.kknn.2

accuracy.kknn.2 <- (sum(diag(CM.kknn.2)))/sum(CM.kknn.2)
accuracy.kknn.2

plot(bacteria.kknn.2)

#-- model 3 / kknn - bagging
train.knn.3 <- cbind(temp.1=bacteria.t$temp.1, temp.2=bacteria.t$temp.2, inflow.1=bacteria.t$inflow.1, sun.1=bacteria.t$sun.1, level=bacteria.t$level)
val.knn.3 <- cbind(temp.1=bacteria.v$temp.1, temp.2=bacteria.v$temp.2, inflow.1=bacteria.v$inflow.1, sun.1=bacteria.v$sun.1, level=bacteria.v$level)
train.knn.3 <- as.data.frame(train.knn.3)
val.knn.3 <- as.data.frame(val.knn.3)
train.knn.3$level <- as.factor(train.knn.3$level)
val.knn.3$level <- as.factor(val.knn.3$level)


bacteria.kknn.3 <- train.kknn(level~., train.knn.3, kmax = 20, kernel = "optimal")
summary(bacteria.kknn.3)

pred.kknn.3 <- predict(bacteria.kknn.3, val.knn.3[, -5])
pred.kknn.3

CM.kknn.3 <- table(val.knn.3[, 5], pred.kknn.3)
CM.kknn.3

accuracy.kknn.3 <- (sum(diag(CM.kknn.3)))/sum(CM.kknn.3)
accuracy.kknn.3

plot(bacteria.kknn.3)


#-- model 4 / kknn - randomforest 7 variables
train.knn.4 <- cbind(TP.2=bacteria.t$TP.2, DO.1=bacteria.t$DO.1, TP.1=bacteria.t$TP.1, TN.1=bacteria.t$TN.1, TN.2=bacteria.t$TN.2, temp.1=bacteria.t$temp.1, temp.2=bacteria.t$temp.2, level=bacteria.t$level)
val.knn.4 <- cbind(TP.2=bacteria.v$TP.2, DO.1=bacteria.v$DO.1, TP.1=bacteria.v$TP.1, TN.1=bacteria.v$TN.1, TN.2=bacteria.v$TN.2, temp.1=bacteria.v$temp.1, temp.2=bacteria.v$temp.2, level=bacteria.v$level)
train.knn.4 <- as.data.frame(train.knn.4)
val.knn.4 <- as.data.frame(val.knn.4)
train.knn.4$level <- as.factor(train.knn.4$level)
val.knn.4$level <- as.factor(val.knn.4$level)


bacteria.kknn.4 <- train.kknn(level~., train.knn.4, kmax = 20, kernel = "optimal")
summary(bacteria.kknn.4)

pred.kknn.4 <- predict(bacteria.kknn.4, val.knn.4[, -8])
pred.kknn.4

CM.kknn.4 <- table(val.knn.4[, 8], pred.kknn.4)
CM.kknn.4

accuracy.kknn.4 <- (sum(diag(CM.kknn.4)))/sum(CM.kknn.4)
accuracy.kknn.4

#-- model 5 / kknn - randomforest 4 variables
train.knn.5 <- cbind(TP.1=bacteria.t$TP.1, TN.2=bacteria.t$TN.2, temp.1=bacteria.t$temp.1, temp.2=bacteria.t$temp.2, level=bacteria.t$level)
val.knn.5 <- cbind(TP.1=bacteria.v$TP.1, TN.2=bacteria.v$TN.2, temp.1=bacteria.v$temp.1, temp.2=bacteria.v$temp.2, level=bacteria.v$level)
train.knn.5 <- as.data.frame(train.knn.5)
val.knn.5 <- as.data.frame(val.knn.5)
train.knn.5$level <- as.factor(train.knn.5$level)
val.knn.5$level <- as.factor(val.knn.5$level)


bacteria.kknn.5 <- train.kknn(level~., train.knn.5, kmax = 20, kernel = "optimal")
summary(bacteria.kknn.5)

pred.kknn.5 <- predict(bacteria.kknn.5, val.knn.5[, -8])
pred.kknn.5

CM.kknn.5 <- table(val.knn.5[, 5], pred.kknn.5)
CM.kknn.5

accuracy.kknn.5 <- (sum(diag(CM.kknn.5)))/sum(CM.kknn.5)
accuracy.kknn.5

#-- knn.var
library(rknn)

bacteria.t.knn.var <- read.xlsx("C:/2015/bacteria.knn.var2_3.xlsx", sheetName= "Training")
bacteria.v.knn.var <- read.xlsx("C:/2015/bacteria.knn.var2_3.xlsx", sheetName= "Validation")

bacteria.t.knn.var <- within (bacteria.t.knn.var, {
  level <- NULL;
  bacteria <- NULL;
  Chl.a <- NULL;
})

bacteria.v.knn.var <- within (bacteria.v.knn.var, {
  level <- NULL;
  bacteria <- NULL;
  Chl.a <- NULL;
})

r(35, m = floor(sqrt(35)), eta = 0.99, nu = 20, rmax = p, nsim = 1000,
  lambda = 0.01, method = c("binomial"))

bacteria.knn.rknnBeg <- rknnBeg(bacteria.t.knn.var, bacteria.t$level, k=9, r=500, mtry = trunc(sqrt(ncol(bacteria.t.knn.var)),fixed.partition = FALSE, pk = 0.5, stopat = 4, cluster=NULL, seed = NULL))

bestset(bacteria.knn.rknnBeg, criterion=c("mean_accuracy"))
bestset(bacteria.knn.rknnBeg, criterion=c("mean_support"))

bacteria.rknn <- rknn(bacteria.t.knn.var, bacteria.v.knn.var, bacteria.t$level, k=1, r=500, mtry=trunc(sqrt(ncol(bacteria.t.knn.var))), cluster=NULL, seed=NULL)
summary(bacteria.rknn)
confusion(bacteria.v$level, bacteria.rknn$pred)

#--- model6 rknn 선택 변수 기반
#"temp.2"      "temp.1"      "TN.2"        "wind_mean.2"
#"temp.1"      "temp.2"      "wind_real.2" "rain.1"    
#"temp.1"      "temp.2"      "wind_real.2" "wind_mean.1"

train.knn.6 <- cbind(temp.1=bacteria.t$temp.1, temp.2=bacteria.t$temp.2, wind_mean.1=bacteria.t$wind_mean.1, wind_real.2=bacteria.t$wind_real.2, level=bacteria.t$level)
val.knn.6 <- cbind(temp.1=bacteria.v$temp.1, temp.2=bacteria.v$temp.2, wind_mean.1=bacteria.v$wind_mean.1, wind_real.2=bacteria.v$wind_real.2, level=bacteria.v$level)
train.knn.6 <- as.data.frame(train.knn.6)
val.knn.6 <- as.data.frame(val.knn.6)
train.knn.6$level <- as.factor(train.knn.6$level)
val.knn.6$level <- as.factor(val.knn.6$level)


bacteria.kknn.6 <- train.kknn(level~., train.knn.6, kmax = 20, kernel = "optimal")
summary(bacteria.kknn.6)

pred.kknn.6 <- predict(bacteria.kknn.6, val.knn.6[, -5])
pred.kknn.6

CM.kknn.6 <- table(val.knn.6[, 5], pred.kknn.6)
CM.kknn.6

accuracy.kknn.3 <- (sum(diag(CM.kknn.3)))/sum(CM.kknn.3)
accuracy.kknn.3

#-- Neural Network
library(neuralnet)
library(nnet)

bacteria.t.pca <- within (bacteria.t, {
  Site <- NULL;
  No <- NULL;
  Date <- NULL;
  level <- NULL;
  Chl.a <- NULL;
  bacteria <- NULL;
})

bacteria.pca <- prcomp(bacteria.t.pca, scale=TRUE)

bacteria.t.pca1 <- predict(bacteria.pca, newdata=bacteria.t.pca)
bacteria.t.pca1 <- as.data.frame(bacteria.t.pca1)
bacteria.t.pca1$level <- bacteria.t$level
bacteria.t.pca1 <- bacteria.t.pca1[, -(14:35)]

bacteria.v.pca <- within (bacteria.v, {
  Site <- NULL;
  No <- NULL;
  Date <- NULL;
  level <- NULL;
  Chl.a <- NULL;
  bacteria <- NULL;
})

bacteria.v.pca1 <- predict(bacteria.pca, newdata=bacteria.v.pca)
bacteria.v.pca1 <- as.data.frame(bacteria.v.pca1)
bacteria.v.pca1$level <- bacteria.v$level
bacteria.v.pca1 <- bacteria.v.pca1[, -(14:35)]

ideal <- class.ind(bacteria.t.pca1$level)

bacteria.nnet = nnet(bacteria.t.pca1[,-14],ideal[,],size=10, maxit=200, softmax=TRUE)
pred.nnet <- predict(bacteria.nn,bacteria.t.pca1[,-14],type="class")

CM.nnet <- table(predict(bacteria.nn,bacteria.v.pca1[,-14],type="class"),bacteria.v.pca1[,14])

#--- neuralnet
bacteria.t.pca1 <- bacteria.t.pca1[,-14]
bacteria.t.pca1 <- cbind(bacteria.t.pca1[, 1:13], ideal[,1:2])
bacteria.t.pca1$level1 <- bacteria.t.pca1[,14]
bacteria.t.pca1$level2 <- bacteria.t.pca1[,15]
bacteria.t.pca1 <- bacteria.t.pca1[,-(14:15)]

bacteria.neuralnet <- neuralnet(level1 + level2 ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13,bacteria.t.pca1, hidden=c(5),threshold = 0.01,        
                                stepmax = 1e+05, rep = 10, startweights = NULL, 
                                learningrate.limit = NULL, 
                                learningrate.factor = list(minus = 0.5, plus = 1.2), 
                                learningrate=NULL, lifesign = "none", 
                                lifesign.step = 1000, algorithm = "rprop+", 
                                err.fct = "sse", act.fct = "logistic", 
                                linear.output = TRUE, exclude = NULL, 
                                constant.weights = NULL, likelihood = FALSE)

pred.neuralnet <- compute(bacteria.neuralnet,bacteria.v.pca1[,-(14:15)])
pred.neuralnet$net.result

for (i in 1:nrow(pred.neuralnet$net.result)){
  if (pred.neuralnet$net.result[i,1] > pred.neuralnet$net.result[i,2]) {
    pred.neuralnet$net.result[i,1]=1
    pred.neuralnet$net.result[i,2]=0
  } else {
    pred.neuralnet$net.result[i,1]=0
    pred.neuralnet$net.result[i,2]=1
  }
}

pred.neuralnet$net.result

bacteria.v.pca1 <- bacteria.v.pca1[,-(14:15)]
bacteria.v.pca1$level <- bacteria.v$level
bacteria.v.pca1$neuralnet <- bacteria.v$level

for (i in 1:nrow(pred.neuralnet$net.result)){
  if (pred.neuralnet$net.result[i,1]==1) {
    bacteria.v.pca1$neuralnet[i]=1
  } else {
    bacteria.v.pca1$neuralnet[i]=2
  }
}

cm.neuralnet <- table(bacteria.v.pca1$neuralnet, bacteria.v.pca1$level)
cm.neuralnet

plot(bacteria.neuralnet, rep = NULL, x.entry = NULL, x.out = NULL,
     radius = 0.15, arrow.length = 0.2, intercept = TRUE,
     intercept.factor = 0.4, information = TRUE,
     information.pos = 0.1, col.entry.synapse = "black",
     col.entry = "black", col.hidden = "black",
     col.hidden.synapse = "black", col.out = "black",
     col.out.synapse = "black", col.intercept = "blue",
     fontsize = 12, dimension = 6, show.weights = TRUE,
     file = NULL)
