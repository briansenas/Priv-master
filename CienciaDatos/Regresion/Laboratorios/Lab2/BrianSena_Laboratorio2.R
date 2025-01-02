# startregion imports
require(kknn)
# endregion imports

# startregion config
# We get our default working directory
getwd()
# We set our new working directory to be able to read files correctly
setwd("Master/CienciaDatos/Regresion/Laboratorios/Lab2")
# endregion config

# startregion input
read_dat_file <- function(x) { 
  # Read all lines from the .dat file
  lines <- readLines(x)
  # Extract column names from the lines starting with '@attribute'
  attr_lines <- grep("@attribute", lines)
  column_names <- gsub("@attribute\\s+(\\w+).*", "\\1", lines[attr_lines])
  # Find where the data starts
  data_start <- grep("@data", lines) + 1
  # Read the data using extracted column names
  read.table(
    text = lines[data_start:length(lines)],
    header = FALSE,
    sep = ",",
    col.names = column_names
  )
}

set_predictors_name <- function(x) {
  #Asignación automática, facilita el acceso a los campos 
  n <- length(names(x)) - 1 
  names(x)[1:n] <- paste ("X", 1:n, sep="") 
  names(x)[n+1] <- "Y"  
  x
}

California <- read_dat_file("california.dat")
California <- set_predictors_name(California)
# endregion input
attach(California)

# startregion basic_knn
fitknn1 <- kknn(Y ~ ., California, California)
# por defecto k = 7, distance = 2, kernel = "optimal�
# y scale=true
plot(Y~X8)
points(X8,fitknn1$fitted.values,col="blue",pch=20)
# Vemos que con este modelo que utiliza todas las variables tenemos 39131.14
yprime = fitknn1$fitted.values; sqrt(sum((California$Y-yprime)^2)/length(yprime)) #rmse
# endregion basic_knn

# startregion knn_based_on_lm_interactions
#Probamos con las iteracciones tal y cuál fuimos avanzando en el laboratorio anterior
fitknn5 <- kknn(Y ~.+X8*X3, California, California)
# Obtenemos un total de 39235.98; Hemos empeorado al añadir la iteracción.
yprime = fitknn5$fitted.values; sqrt(sum((California$Y-yprime)^2)/length(yprime)) #rmse

fitknn6 <- kknn(Y ~ .+X8*X3+I(X8^2), California, California)
# Obtenemos un total de 39209.15; Seguimos peor que el modelo base
yprime = fitknn6$fitted.values; sqrt(sum((California$Y-yprime)^2)/length(yprime)) #rmse

fitknn7 <- kknn(Y ~ .+X8*X3+I(X8^2)+I(X8^3), California, California)
# Con este polinómio logramos superar el modelo base con 39057.54
yprime = fitknn7$fitted.values; sqrt(sum((California$Y-yprime)^2)/length(yprime)) #rmse
# Probamos con nuestro mejor modelo alcanzado sin utilizar las iteracciones de la diapositiva 
fitknn8 <- kknn(Y~.+X8*X3+I(X8^2)+I(X8^3)+I(X8^2*X3)+I(X3^2), California, California)
# Vemos que el mejor modelo paramétrico no tiene porque ser el mejor no-paramétrico
# Ya que con todas las iteracciones obtenemos el peor de todos con 39331.57
yprime = fitknn8$fitted.values; sqrt(sum((California$Y-yprime)^2)/length(yprime)) #rmse

plot(medv~lstat)
points(lstat,fitknn1$fitted.values,col="blue",pch=20)
points(lstat,fitknn5$fitted.values,col="red",pch=20)
points(lstat,fitknn6$fitted.values,col="green",pch=20)
# endregion knn_based_on_lm_interactions

# startregion using-cross-validation
#------------- 5-fold cross-validation lm todas las variables
read_train_test <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  in_names <- length(names(x_tra)) - 1
  names(x_tra)[1:in_names] <- paste ("X", 1:in_names, sep="")
  names(x_tra)[in_names+1] <- "Y"
  names(x_tst)[1:in_names] <- paste ("X", 1:in_names, sep="")
  names(x_tst)[in_names+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  list(train=x_tra, test=test)
}

nombre <- "california"
run_lm_fold <- function(i, x, tt = "test") {
  data <- read_train_test(i, x, tt)
  fitmulti=lm(Y~.,data$train)
  yprime=predict(fitmulti,data$test)
  sum(abs(data$test$Y-yprime)^2)/length(yprime) ##mse
}
lmmsetrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmmsetest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#------------- 5-fold cross-validation knn todas las variables

nombre <- "california"
run_knn_fold <- function(i, x, tt = "test") {
  data = read_train_test(i, x, tt)
  fitmulti=kknn(Y~.,data$train,data$test)
  yprime=fitmulti$fitted.values
  sum(abs(data$test$Y-yprime)^2)/length(yprime) ##mse
}
knnmsetrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnmsetest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
# endregion using-cross-validation
# startregion test-best-models-in-cv
run_knn_model_fold <- function(i, x, model="Y~.", tt = "test") {
  data = read_train_test(i, x, tt)
  fit = kknn(model, data$train, data$test)
  yprime=fit$fitted.values
  mse <- sum(abs(data$test$Y-yprime)^2)/length(yprime) ##mse
}
model1 = "Y~.+X8*X3+I(X8^2)+I(X8^3)" 
knnmsetrain2 <- mean(sapply(1:5,run_knn_model_fold,nombre, model, "train"))
knnmsetest2<-mean(sapply(1:5,run_knn_model_fold,nombre, model, "test"))

run_lm_model_fold <- function(i, x, model="Y~.", tt = "test") {
  data <- read_train_test(i, x, tt)
  fitmulti=lm(model,data$train)
  yprime=predict(fitmulti,data$test)
  sum(abs(data$test$Y-yprime)^2)/length(yprime) ##mse
}
model2 = "Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)+X7*X8*X4*X5*X6"
lmmsetrain2 <- mean(sapply(1:5,run_lm_model_fold,nombre, model, "train"))
lmmsetest2 <- mean(sapply(1:5,run_lm_model_fold,nombre, model, "test"))

model3 = "Y~.+X8*X3+I(X8^2)+I(X8^3)+I(X8^2*X3)+I(X3^2)"
lmmsetrain3 <- mean(sapply(1:5,run_lm_model_fold,nombre, model, "train"))
lmmsetest3 <- mean(sapply(1:5,run_lm_model_fold,nombre, model, "test"))
# Vemos que hemos emperado el modelo al incluir iteracciones
# endregion test-best-models-in-cv
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
