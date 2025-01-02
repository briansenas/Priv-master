# start_region HIP_dataset
# ------ HIP dataset
# Otra forma de cargar los datos
# install.packages("astrodatR")
library(astrodatR)
data("HIP")
# Comprueba la dimensión y los nombre de columnas
head(HIP, n = 10)
cat("Dimensión del dataset HIP:\n")
dim(HIP)
cat("Nombre de las columnas:\n")
colnames(HIP)
library(tidyverse)
cat("Tipos de datos\n")
HIP %>% str
cat("Todas las columnas son de tipo numérico. Con HIP_num siendo la única entera\n")
cat("Podemos calcular las tendencias centrales con summary\n")
summary(HIP)
cat("O también podemos hacerlo manualmente\n")
apply(HIP, 2, mean)
apply(HIP, 2, median)
cat("O también con tidyverse\n")
# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
mode_func <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
HIP %>%
  replace(is.na(.), 0) %>%
  summarize(
    across(
      colnames(HIP),
      list(
        mean = mean,
        median = median,
        mode = mode_func,
        # quantiles = quantile
        )
      )
  )
# Vemos en clase que también podemos calcular las medidas con map_dbl / map
HIP %>% map(mean)
HIP %>% map_dbl(median)
# Basícamente los map_<dtype> devolveran el resultado en el formato especificado
cat("Para calcular las medidas de dispersión podemos utilizar min/max\n")
HIP %>%
  replace(is.na(.), 0) %>%
  summarize(
    across(
      colnames(HIP),
      list(
        min = min, 
        max = max
        )
      )
  )
cat("Vemos lo que hace la función range\n")
?range 
cat("Nos sirve para calcular el min/max! Por ejemplo para RA\n")
range(HIP$RA)
cat("Calculamos la varianza y la dispersión para RA\n")
HIP %>% summarize(var = var(RA), sd = sd(RA))
f = function(x) c(median(x), mad(x))
f(HIP[, 3])
?mad
cat("Podemos utilizar la función f para calcular la mediana y la desviación de la misma\n")
cat("El resultado de aplicar apply(HIP, 2, f) es aplicar la función a sus columnas:\n")
apply(HIP, 2, f)
cat("Para calcular quantiles podemos utilizar diversas funciones\n")
quantile(HIP$RA, probs = c(0.1, 0.5))
cat("En la función summary() no vemos la distancia IQR\n")
summary(HIP)
cat("Pero la podemos obtener utilizando IQR()\n")
IQR(HIP$RA)
cat("Para dibujar gráficas vimos ggplot")
HIP %>%
  mutate(color = pmDE > 0) %>%
  ggplot(aes(x = RA, y = pmDE)) +
  geom_point(aes(color = color)) + 
  theme_minimal() + 
  scale_color_manual(values = c("blue", "red")) + 
  labs(color = "pmDE is greater than 0?")
cat("Buscamos un patrón para RA y pmRA\n")
HIP %>% 
  ggplot(aes(x = RA, y = pmRA)) +
  geom_point() +
  theme_minimal()
cat("Parece seguir una especie de función cuadrática...\n")
cat("Para hacer un scatterplot podemos utilizar el plot de R\n")
plot(HIP)
cat("En tidyverse hace falta importar otro paquete\n")
# install.packages("GGally")
library(GGally)
ggpairs(HIP)
cat("Acceder a variables del dataset usando attach")
filtered_hip <- HIP %>%
  filter(
    (RA > 50 & RA < 100) &
    (pmRA > 90 & pmRA < 130) &
    (pmDE > -60 & pmDE < -10) &
    (e_Plx < 5) & 
    (Vmag > 4 | B.V < 0.2)
  )
cat("Las dimensiones del nuevo dataset son:\n")
dim(filtered_hip)
filtered_hip %>% 
  ggplot(aes(x = Vmag, y = B.V)) +
  geom_point() + 
  theme_minimal()
cat("Vemos un conjunto de características que parece correlacionado positivamente.")
# end_region HIP_Dataset

# start_region InsectSprays Dataset
# ------- InsectSprays Dataset
head(InsectSprays, 5)
colnames(InsectSprays)
InsectSprays %>%
  ggplot(aes(x = count)) +
  geom_boxplot(aes(fill = spray)) + 
  theme_minimal() + 
  coord_flip() + 
# end_region InsectSprays Dataset
# start_region Carseats
# --------- Carseats
# install.packages("ISLR")
library(ISLR)
# install.packages("dlookr")
library(dlookr)
library(moments)
head(Carseats, n = 5)
Carseats %>% str
plot_normality(Carseats)
cat("Observando las gráficas podemos imaginar que:\n")
cat("Advertising: Positive Skewness?\n")
cat("No obstante podemos medirlo y comprobarlo con un test estadístico:\n")
Carseats %>% 
  summarize(
    across(where(is.numeric), list(skew = skewness))
    )
cat("Casi todos los valores son cercanos a uno. Adversiting aparenta tener un skew positivo\n")
agos_func <- function(x) agostino.test(x)$p
Carseats %>% 
  summarize(
    across(where(is.numeric), list(pvalue = agos_func))
    )
cat("Vemos que podemos rechazar la hipótesis de normalidad de los datos de Advertising\n")
cat("Es decir, tiene Skew Positivo.\n")
cat("Para averiguar más información sobre la distribución de los datos podmeos mirar la kurtosis\n")
anscom <- function(x) anscombe.test(x)$p
Carseats %>% 
  summarize(
    across(where(is.numeric), list(kurt = kurtosis, pval = anscom))
    )
cat("Advertising tiene una kurtosis positiva.\n")
cat("Para todos los casos de _pval < 0.05 es probable que hayan outliers.\n")
posible_outliers <- c("Income", "Advertising", "Population", "Age", "Education")
plot(Carseats)
cat("Con el scatterplot matrix podemos observar las variables correlacionadas positiva y negativamente:")
cat("Ejemplo de relación positiva:\n")
Carseats %>% 
  ggplot(aes(x = Price, y = CompPrice)) +
  geom_point()
cor_price <- cor(Carseats$Price, Carseats$CompPrice)
sprintf("El valor de correlación es: %f", cor_price)
cat("Ejemplo de relación negativa:\n")
Carseats %>% 
  ggplot(aes(x = Sales, y = Price)) +
  geom_point()
cor_price <- cor(Carseats$Sales, Carseats$Price)
sprintf("El valor de correlación es: %f", cor_price)
# end_region Carseats
# start_region IRIS
# ------------ IRIS
head(iris, n = 5)
summary(iris)
iris %>%
  ggplot(aes(x = Petal.Width)) + 
  geom_histogram(bins = 9, aes(color = Species)) + 
  theme_minimal() + 
  facet_wrap(~Species) +
  labs(
    title = "Histogram of Petal Width",
    x = "Petal Width",
    y = "Frecuencia",
    color = "Especie de planta"
)

quantiles(iris)

iris %>% 
  ggplot(aes(x = Petal.Width)) + 
  geom_boxplot(aes(fill = Species)) + 
  facet_wrap(~ Species) +
  coord_flip() + 
  theme_minimal()

# TODO: Solve the multiplot thing about normalization manually.
cat("Podemos verificar el boxplot de la longitud de pétalo con:\n")
iris %>% 
  ggplot(aes(x = Petal.Length)) + 
  geom_boxplot(aes(fill = Species)) + 
  facet_wrap(~ Species) + 
  theme_minimal() + 
  coord_flip()

cat("Podemos verificar la correlación entre las variables usando scatterplot matrix:\n")
plot(iris)

iris %>% 
  summarize(ratio = Sepal.Length / Sepal.Width)
# end_region IRIS
# start_region SWISS
# ----------- SWISS
head(swiss, n = 5)
swiss %>% str
rownames(swiss)
nswiss <- swiss %>% mutate(names = rownames(swiss))
cat("Para obtener información sobre la distribución del conjunto de datos SWISS haría uso de histogramas.\n")
cat("O también podría utilizar boxplot si quiero más detalles de la dispersión\n")
library(ggplot2)
swiss_named <- swiss %>% mutate(names = rownames(swiss))
swiss_long <- pivot_longer(swiss_named, colnames(swiss))
head(swiss_long)
ggplot(swiss_long, aes(x = names, y = value)) +
  geom_bar(stat="identity", fill = "skyblue") +
  labs(
    title = "Boxplot de las variables en el dataset swiss",
    x = "Variables",
    y = "Valores"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~ name)
cat("También podríamos utiliar plot_normality()\n")
# end_region SWISS

# start_region Aceitunas
# ------------ Aceitunas
# install.packages("extracat") # Me da un error.
# end_region Aceitunas

# start_region HSAUR2
# ------------- HSAUR2
# install.packages("HSAUR2")
library(HSAUR2)
data(Lanza)
head(Lanza)
Lanza %>%
  ggplot(aes(x = study)) + 
  geom_histogram(stat="count") + 
  theme_minimal()

Lanza %>% 
  ggplot(aes(x = treatment, y = classification)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  facet_wrap(~ study)
cat("La distribución parece negativamente skewed con kurtosis positiva en la mayoría de los casos \n.")
cat("Deberíamos asegurar la hipótesis mediante tests estadísticos.\n")
cat("La razón de la cuál sugiero eso es debido a la forma pronunciada del histograma hacia las notas >= 4\n")
# end_region HSAUR2