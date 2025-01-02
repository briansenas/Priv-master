require(ISLR) #o install.packages("ISLR")
require(MASS)

# install.packages("ISLR")
# install.packages("MASS")

plot(medv~age,Boston) #probando una a una

temp <- Boston
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""), ylab=names(temp)[y])
}
par(mfrow=c(3,4)) #Si margin too large => (2,3)
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))

par(mfrow=c(3,3)) #Si margin too large => (2,3)
x <- sapply(c(1, 5, 6, 7, 8, 10, 11, 12, 13), plotY, dim(temp)[2])
par(mfrow=c(1,1))

fit1=lm(medv~lstat,data=Boston)
#or fit1=lm(Boston$medv~Boston$lstat)
fit1

fit2=lm(medv~rm,data=Boston)
fit2

summary(fit1)
par(mfrow=c(2,1))
plot(medv~lstat,Boston)
abline(fit1,col="red")
confint(fit1)

summary(fit2)
plot(medv~rm,Boston)
abline(fit2,col="blue")
par(mfrow=c(1,1))
confint(fit2)

sqrt(sum(fit1$residuals^2)/(length(fit1$residuals)-2)) #MSE
sqrt(sum(fit1$residuals^2)/(length(fit1$residuals)-2)) #Error estandard de los residuos

predict(fit1,data.frame(lstat=c(5,10,15)))

yprime=predict(fit1,data.frame(lstat=Boston$lstat)) #Valores estimados para todos los datos
#o directamente #yprime=predict(fit1,Boston)
sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime)) #MSE

temp <- Boston
plot(temp[,-dim(temp)[2]],pch=16,col=gray(1-(temp[,dim(temp)[2]]/max(temp[,dim(temp)[2]]))))

fit3=lm(medv~lstat+age,data=Boston)
summary(fit3)

fit4=lm(medv~lstat+rm,data=Boston)
summary(fit4)

fit5=lm(medv~.,data=Boston) #TODAS para descendente
summary(fit5)

#...

fit7=lm(medv~.-age-indus-chas-crim,data=Boston)
summary(fit7)

#interactions

attach(Boston)
fit8=lm(medv~lstat*rm,Boston)
summary(fit8)
plot(medv~lstat)
points(lstat,fitted(fit8),col="green",pch=20)

fitbien1=lm(medv~zn+nox+rm+dis+rad+tax+ptratio+black+lstat+lstat*rm) #se incorpora a lo anterior
summary(fitbien1)

fit9=lm(medv~lstat +I(lstat^2),Boston)
summary(fit9)
plot(medv~lstat)
points(lstat,fitted(fit9),col="red",pch=20)

fitbien2=lm(medv~zn+nox+rm+dis+rad+tax+ptratio+black+lstat+lstat*rm+I(log(lstat))) #Quitar zn ???
summary(fitbien2)

fitprueba=lm(medv~lstat +rm +I(lstat * rm) +I(lstat^2) +I(lstat^2 * rm),Boston)
summary(fitprueba)
plot(medv~lstat)
points(lstat,fitted(fitprueba),col="red",pch=20)

yprime=predict(fitbien2,Boston)
sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime))
yprime=predict(fitprueba,Boston)
sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime))

#LECTURA KEEL california

xtra <- read.csv("california.dat", comment.char="@", header = FALSE)
#head(xtra)

#Asignaci�n manual
names(xtra) <- c("Longitude", "Latitude", "HousingMedianAge",
                 "TotalRooms", "TotalBedrooms", "Population", "Households",
                 "MedianIncome", "MedianHouseValue") 

#Asignaci�n autom�tica, facilita el acceso a los campos
n <- length(names(xtra)) - 1
names(xtra)[1:n] <- paste ("X", 1:n, sep="")
names(xtra)[n+1] <- "Y"