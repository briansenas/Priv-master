# Cargamos el conjunto de datos.
datos <- mtcars 
head(datos)

# Obtenemos las columnas numéricas 
columnas.num = sapply(c(1:ncol(datos)) , function(x) is.numeric(datos[, x]))
columnas.num
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

datos.num = datos[, columnas.num]
head(datos.num)

# Eliminamos columnas redundantes.
columnas.valores.distintos <- datos %>% summarise_all(n_distinct)
columnas.a.quitar <- which(columnas.valores.distintos < 10)
columnas.a.quitar

datos.num[, columnas.a.quitar] <- NULL
head(datos.num)

# Obtenemos los histogramas.
par(mfrow=c(2,3))
hist <- sapply(
  c(1:ncol(datos.num)), 
  function(x){
    h <- hist(datos.num[, x])
    plot(h, 
         xlab = names(datos.num)[x],
         main = "",
    )
})
par(mfrow=c(1,1))
# Seleccionamos una columna cualquiera para analizar
indice.columna = 1
columna        = datos.num[, indice.columna]
nombre.columna = names(datos.num) [indice.columna]

# Obtenemos los cuartiles
cuartil.primero <- quantile(columna, 0.25)
cuartil.segundo <- quantile(columna, 0.5)
cuartil.tercero <- quantile(columna, 0.75)
iqr <- cuartil.tercero - cuartil.primero

# Obtenemos los valores considerados lejanos de la media.
extremo.superior.outlier.IQR <- cuartil.tercero + 1.5*iqr
extremo.inferior.outlier.IQR <- cuartil.primero - 1.5*iqr
extremo.superior.outlier.IQR.extremo <- cuartil.tercero + 3*iqr
extremo.inferior.outlier.IQR.extremo <- cuartil.primero - 3*iqr

son.outliers.IQR <- 
  columna < extremo.inferior.outlier.IQR | 
    columna > extremo.superior.outlier.IQR
son.outliers.IQR.extremo <- 
  columna < extremo.inferior.outlier.IQR.extremo | 
    columna > extremo.superior.outlier.IQR.extremo
son.outliers.IQR
son.outliers.IQR.extremo
sum(son.outliers.IQR) == 1
sum(son.outliers.IQR.extremo) == 0

# 3.1.2 Obtemoes los índices y valores que son outliers IQR
claves.outliers.IQR <- which(son.outliers.IQR)
df.outliers.IQR <- datos.num[claves.outliers.IQR, ]
nombres.outliers.IQR <- row.names(df.outliers.IQR)
valores.outliers.IQR <- df.outliers.IQR[, indice.columna]

claves.outliers.IQR.extremo <- which(son.outliers.IQR.extremo)
df.outliers.IQR.extremo <- datos.num[claves.outliers.IQR.extremo, ]
nombres.outliers.IQR.extremo <- row.names(df.outliers.IQR.extremo)
valores.outliers.IQR.extremo <- df.outliers.IQR.extremo[, indice.columna]

claves.outliers.IQR
df.outliers.IQR
nombres.outliers.IQR
valores.outliers.IQR
df.outliers.IQR.extremo
nombres.outliers.IQR.extremo
valores.outliers.IQR.extremo

# 3.1.3 Cómputo de los outliers IQR 
son.outliers.IQR = son_outliers_IQR (datos.num, indice.columna)
head(son.outliers.IQR)
## [1] FALSE FALSE FALSE FALSE FALSE FALSE

claves.outliers.IQR  = claves_outliers_IQR (datos.num, indice.columna)
claves.outliers.IQR
## [1] 20

son.outliers.IQR.extremos    = son_outliers_IQR (datos.num, indice.columna, 3)
head(son.outliers.IQR.extremos)
## [1] FALSE FALSE FALSE FALSE FALSE FALSE

claves.outliers.IQR.extremos = claves_outliers_IQR (datos.num, indice.columna, 3)
claves.outliers.IQR.extremos
## integer(0)

# Z-Score
datos.num.zscore = scale(datos.num)
head(datos.num.zscore)
columna.norm <- datos.num.zscore[, indice.columna]
valores.outliers.IQR.norm <- columna.norm[son.outliers.IQR]
valores.outliers.IQR.norm

datos.num.zscore.outliers.IQR <- datos.num.zscore[son.outliers.IQR]
datos.num.zscore.outliers.IQR

# Gráfico
plot_2_colores(
  columna.norm, 
  son.outliers.IQR,
  titulo = "mpg"
)

# Diagrama de cajas
diag_caja_outliers_IQR(
  datos.num, indice.columna,
)

# Diagrama de cajas con etiquetas
diag_caja(
  datos.num, 
  indice.columna, 
  claves.a.mostrar =  claves.outliers.IQR
)
# Diagrama de cajas del conjunto 
diag_caja_juntos(
  datos.num, 
  titulo = "Visualizar outliers de alguna columna",
  claves.a.mostrar = claves.outliers.IQR
)

## Test de hipótesis
## Test de Grubbs
test.Grubbs = grubbs.test(columna, two.sided = TRUE)
test.Grubbs$p.value
# H0 :El valor más alejado de la media proviene de la misma distribución normal que el resto de datos
# El p-value es > 0.05, por lo que el test no puede rechazar
## Obtenemos un posible outlier con la librería 
valor.posible.outlier = outlier(columna)
valor.posible.outlier
## Obtenemos el nombre de ese dato
es.posible.outlier = outlier(columna, logical = TRUE)
clave.posible.outlier = which(es.posible.outlier == TRUE)
clave.posible.outlier

## Comprobación del test de normalidad
datos.num.sin.outlier = datos.num[-clave.posible.outlier,]
columna.sin.outlier = datos.num.sin.outlier[,indice.columna]
columna.sin.outlier

## Visualizando distribución sin outlier.
ajusteNormal = fitdist(columna.sin.outlier , "norm")
denscomp (ajusteNormal,  xlab = nombre.columna)
ggqqplot(columna.sin.outlier) 
# Con los dos gráficos anteriores observamos que, efectivamente, podemos aceptar que la distribución subyacente es una Normal. Por tanto, si el test de Grubbs hubiese rechazado, aceptaríamos que el valor más alejado de la media es un outlier.
## Test estadístico de normalidad
# H0:La distribución subyacente de la variable es una Normal
shapiro.test(columna.sin.outlier)
# En resumen, podemos concluir que los valores presentes en la columna mpg son compatibles con una distribución Normal y que el Toyota Corolla podría haberse considerado un outlier con garantía estadística en el caso de que el test de Grubbs hubiese salido significativo.

## 3.2.3 Construcción de una función propia.
#######################################################################
# Aplica el test de Grubbs sobre la columna ind.col de datos y devuelve una lista con:

# nombre.columna: Nombre de la columna datos[, ind.col]
# clave.mas.alejado.media: Clave del valor O que está más alejado de la media
# valor.mas.alejado.media: Valor de O en datos[, ind.col]
# nombre.mas.alejado.media: Nombre de O en datos
# es.outlier: TRUE/FALSE dependiendo del resultado del test de Grubbs sobre O
# p.value:  p-value calculado por el test de Grubbs
# es.distrib.norm: Resultado de aplicar el test de Normalidad 
#    de Shapiro-Wilk sobre datos[, ind.col]
#    El test de normalidad se aplica sin tener en cuenta el 
#    valor más alejado de la media (el posible outlier O)
#    TRUE si el test no ha podido rechazar
#       -> Sólo podemos concluir que los datos no contradicen una Normal
#    FALSE si el test rechaza 
#       -> Los datos no siguen una Normal
# p.value.test.normalidad: p-value del test de Shapiro

# Requiere el paquete outliers

test_Grubbs <- function(data.frame, indice.columna, alpha = 0.05){
  columna <- data.frame[, indice.columna]
  nombre.columna <- colnames(data.frame)[indice.columna]
  test.Grubbs = grubbs.test(columna, two.sided = TRUE)
  p.value <- test.Grubbs$p.value
  es.outlier <- p.value < 0.05
  # H0 :El valor más alejado de la media proviene de la misma distribución normal que el resto de datos
  # El p-value es > 0.05, por lo que el test no puede rechazar
  ## Obtenemos el nombre de ese dato
  es.posible.outlier = outlier(columna, logical = TRUE)
  clave.mas.alejado.media = which(es.posible.outlier == TRUE)
  valor.mas.alejado.media = columna[clave.mas.alejado.media]
  nombre.mas.alejado.media = rownames(data.frame)[clave.mas.alejado.media]
  
  ## Comprobación del test de normalidad
  sin.outlier = data.frame[-clave.mas.alejado.media,]
  columna.sin.outlier = sin.outlier[, indice.columna]
  shaptest <- shapiro.test(columna.sin.outlier)  
  p.value.test.normalidad <- shaptest$p.value
  es.distrib.norm <- p.value.test.normalidad > alpha
  list(
    nombre.columna=nombre.columna, 
    clave.mas.alejado.media=clave.mas.alejado.media,
    valor.mas.alejado.media=valor.mas.alejado.media,
    nombre.mas.alejado.media=nombre.mas.alejado.media,
    es.outlier=es.outlier,
    p.value=p.value,
    p.value.test.normalidad=p.value.test.normalidad,
    es.distrib.norm=es.distrib.norm
  )
}

test.Grubbs.con.funcion = test_Grubbs(datos.num, indice.columna)
test.Grubbs.con.funcion

## 3.3 Trabajando con varias columnas
claves.outliers.IQR.en.alguna.columna =
  claves_outliers_IQR_en_alguna_columna(datos.num, 1.5)

claves.outliers.IQR.en.alguna.columna
## [1] 20 31 15 16 17  9
# Podrían haber duplicados así que debemos eliminarlos
claves.outliers.IQR.en.mas.de.una.columna = 
  unique(
    claves.outliers.IQR.en.alguna.columna[
      duplicated(claves.outliers.IQR.en.alguna.columna)])
claves.outliers.IQR.en.alguna.columna = 
  unique (claves.outliers.IQR.en.alguna.columna)

claves.outliers.IQR.en.mas.de.una.columna
## integer(0)

claves.outliers.IQR.en.alguna.columna 
## [1] 20 31 15 16 17  9

nombres_filas(datos.num, claves.outliers.IQR.en.mas.de.una.columna)
## character(0)

nombres_filas(datos.num, claves.outliers.IQR.en.alguna.columna)
## [1] "Toyota Corolla"      "Maserati Bora"       "Cadillac Fleetwood"  "Lincoln Continental"
## [5] "Chrysler Imperial"   "Merc 230"

diag_caja_juntos(
  datos.num, 
  titulo = "Visualizar outliers de alguna columna",
  claves.a.mostrar = claves.outliers.IQR.en.alguna.columna
)

## Test de Hipótesis (OPCIONAL)
par(mfrow=c(2,3))
hist <- sapply(
  c(1:ncol(datos.num)), 
  function(x){
    ajusteNormal = fitdist(datos.num[, x], "norm")
    denscomp(ajusteNormal,  xlab = names(datos.num)[x])
})
par(mfrow=c(1,1))

columnas.no.normales <- c(2)
datos.num.var.norm <- datos.num
datos.num.var.norm[, columnas.no.normales] <- NULL
head(datos.num.var.norm)
sapply(
  c(1:ncol(datos.num.var.norm)),
  function (x){
    test_Grubbs(datos.num.var.norm, x)
  }
)


## 4 Outliers Multivariantes.
# Como no se desvían mucho podemos asumir normalidad.
cqplot(datos.num.var.norm , method = "classical")
## 4.1.2 Test de hipótesis
test.MVN = mvn(datos.num.var.norm, mvnTest = "energy")
test.MVN$multivariateNormality["MVN"]
##   MVN
## 1  NO
test.MVN$multivariateNormality["p value"]
##   p value
## 0.002
# El test rechaza la hipótesis nula por lo que no podemos asumir que tengamos una distribución normal multivariante
## 4.1.3 Usando cuantiles.
dist.mah.clas = Mahalanobis(datos.num.var.norm, method = "classical")
# Recordemos que si los datos siguen una distribución normal multivariante, las distancias de Mahalanobis siguen una distribución χ2
umbral.mah <- quantile(dist.mah.clas, 0.975)
son.outliers.mah.clas <- dist.mah.clas > umbral.mah
claves.outliers.mah.clas <- which(son.outliers.mah.clas)
df.outliers.mah.clas <- datos.num[claves.outliers.mah.clas, ]
nombres.outliers.mah.clas <- row.names(df.outliers.mah.clas)
valores.outliers.mah.clas <- df.outliers.mah.clas[, indice.columna]
claves.outliers.mah.clas
nombres_filas(datos.num, claves.outliers.mah.clas)

## 4.1.4 Test de hipótesis para detectar outliers.
# Establecemos la semilla
set.seed(2)

#test individual
test.individual.Cerioli = cerioli2010.fsrmcd.test(datos.num.var.norm, signif.alpha = 0.05)
son.posibles.outliers.individual.Cerioli = test.individual.Cerioli$outliers
claves.test.individual = which (son.posibles.outliers.individual.Cerioli == TRUE)
nombres.test.individual = nombres_filas(datos.num.var.norm, claves.test.individual)

# test interseccion
n = nrow(datos.num.var.norm)
signif.interseccion = 1. - ((1. - 0.05)^(1./n))
test.interseccion.Cerioli = cerioli2010.fsrmcd.test(datos.num.var.norm, signif.alpha = signif.interseccion)  
son.posibles.outliers.Cerioli.interseccion = test.interseccion.Cerioli$outliers
claves.test.interseccion = which (son.posibles.outliers.Cerioli.interseccion == TRUE)
nombres.test.interseccion = nombres_filas(datos.num.var.norm, claves.test.interseccion)
claves.test.individual
## [1]  7  9 29 31
nombres.test.individual
## [1] "Duster 360"     "Merc 230"       "Ford Pantera L" "Maserati Bora"
claves.test.interseccion
## [1] 31
nombres.test.interseccion
## [1] "Maserati Bora"

## 4.2 Visualización de datos con biplot
biplot.outliers.IQR = biplot_2_colores(
  datos.num, 
  claves.outliers.IQR.en.alguna.columna, 
  titulo.grupo.a.mostrar = "Outliers IQR",
  titulo ="Biplot Outliers IQR"
)
biplot.outliers.IQR

## 4.3 Métodos basados en distancias LOF.
num.vecinos.lof = 5
lof.scores = LOF(dataset = datos.num.zscore, k = num.vecinos.lof)
lof.scores.sorted = sort(lof.scores, decreasing = TRUE)
plot(1:length(lof.scores), lof.scores.sorted)

num.outliers = 3
claves.outliers.lof <- which(lof.scores >= lof.scores.sorted[num.outliers])
nombres.outliers.lof <- rownames(datos.num.zscore)[claves.outliers.lof]
claves.outliers.lof
nombres.outliers.lof 
datos.num.zscore[claves.outliers.lof, ]

clave.max.outlier.lof = claves.outliers.lof[1]

colores = rep("black", times = nrow(datos.num.zscore))
colores[clave.max.outlier.lof] = "red"
pairs(datos.num.zscore, pch = 19,  cex = 0.5, col = colores, lower.panel = NULL)

biplot.max.outlier.lof = biplot_2_colores(datos.num.zscore, clave.max.outlier.lof, titulo = "Mayor outlier LOF")
biplot.max.outlier.lof

## 4.4 Métodos basados en clustering
num.outliers = 5
num.clusters = 3
set.seed(2)

modelo.kmeans <- kmeans(
  datos.num.zscore, centers = num.clusters
)

asignaciones.clustering.kmeans <- modelo.kmeans$cluster
centroides.normalizados <- modelo.kmeans$centers

head(asignaciones.clustering.kmeans)
centroides.normalizados

centroides.desnormalizados <- desnormaliza(datos.num, centroides.normalizados)
centroides.desnormalizados

distancia.outliers.centroides <- top_clustering_outliers(
  datos.num.zscore, 
  asignaciones.clustering.kmeans, 
  centroides.normalizados, 
  30
)
distancia.outliers <- distancia.outliers.centroides$distancias
plot(1:length(distancia.outliers), distancia.outliers)

claves.outliers.kmeans <- distancia.outliers.centroides$claves[1:5]
biplot_outliers_clustering(
  datos.num, 
  titulo = "Outliers k-means",
  asignaciones.clustering = asignaciones.clustering.kmeans,
  claves.outliers = claves.outliers.kmeans
)

diag_caja_juntos(datos.num, "Outliers k-means", claves.outliers.kmeans)

## 4.4 Pam

## 4.5 Análisis de outliers multivariantes puros
claves.outliers.lof.no.IQR <- setdiff(claves.outliers.lof, claves.outliers.IQR.en.alguna.columna)
nombres.outliers.lof.no.IQR <- rownames(datos.num)[claves.outliers.lof.no.IQR]
claves.outliers.lof.no.IQR
nombres.outliers.lof.no.IQR


