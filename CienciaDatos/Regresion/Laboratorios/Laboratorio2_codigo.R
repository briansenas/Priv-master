#install.packages("ISLR")
#install.packages("MASS")
# install.packages("kknn")

require(ISLR) #o install.packages("ISLR")
require(MASS)
attach(Boston)
require(kknn)

fitknn1 <- kknn(medv ~ ., Boston, Boston)
# Por defecto k = 7, distance = 2, kernel = "optimal�
# y scale=TRUE

plot(medv~lstat)
points(lstat,fitknn1$fitted.values,col="blue",pch=20)

yprime = fitknn1$fitted.values
# o tambi�n yprime=predict(fitknn1,Boston)
sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE

fitknn5 <- kknn(medv ~ . - chas, Boston, Boston)
yprime = fitknn5$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE

fitknn6 <- kknn(medv ~ . - chas - ptratio -zn, Boston, Boston)
yprime = fitknn6$fitted.values; sqrt(sum((Boston$medv-yprime)^2)/length(yprime)) #RMSE

plot(medv~lstat)
points(lstat,fitknn1$fitted.values,col="blue",pch=20)
points(lstat,fitknn5$fitted.values,col="red",pch=20)
points(lstat,fitknn6$fitted.values,col="green",pch=20)

#------------- 5-fold cross-validation LM todas las variables
nombre <- "california"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=lm(Y~.,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#------------- 5-fold cross-validation KNN todas las variables
nombre <- "california"
run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=kknn(Y~.,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))

#------------------- COMPARATIVAS GENERALES ENTRE ALGORITMOS

#leemos la tabla con los errores medios de test
resultados <- read.csv("regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[,1]

#leemos la tabla con los errores medios de entrenamiento
resultados <- read.csv("regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) <- resultados[,1]

##TABLA NORMALIZADA - lm (other) vs knn (ref) para WILCOXON
# + 0.1 porque wilcox R falla para valores == 0 en la tabla

difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, 	abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

#Aplicaci�n del test de WILCOXON
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

#Aplicaci�n del test de Friedman
test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman

#Aplicaci�n del test post-hoc de HOLM
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)