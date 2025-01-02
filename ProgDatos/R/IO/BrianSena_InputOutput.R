## Ejercicios input/output
# 1. Pida al usuario que introduzca con el teclado una cadena de caracteres s y un número n
# imprima en pantalla n veces la cadena s (sin espacios entre palabras) tal como se ve en el ejemplo
# >s="hola", n=3<br/>
# holaholahola
cat("Ejercicio 1. Introducir cadena y número.\n")
cat("Imprimir N veces la cadena sin espacios\n")
valid <- FALSE
# Dado que ya hemos visto functions and programming pensé en intentar hacer
# Una especie de validador de entrada con lo que hemos visto hasta ahora.
while (!valid){
cat("Introduzca la cadena y un número separado por espacio\n")
  data <- readline()
  data <- unlist(strsplit(data, split = " "))
  s <- data[1]
  n <- data[2]
  valid <- length(grep("^[0-9]+$", n)) == 1 & n >= 0
  if (!valid) {
    cat("The inputs are not valid. Example: text 3\n")
  }
}
cat(paste(paste(rep(s, n), sep = "", collapse = ""), "\n"))
# 2. Crea tres ficheros llamados `dos.txt`, `tres.txt` y `cinco.txt` que contengan la tabla de 2, 3 y 5 respectivamente (sólo incluye los 10 primeros valores de cada uno, un número en una línea separada, SOLO el número, nada más).
secuence <- 1:9
write.table(
  rep(2, 9) * secuence,
  file = "dos.txt",
  row.names = FALSE,
  col.names = FALSE
)
write.table(
  rep(3, 9) * secuence,
  file = "tres.txt",
  row.names = FALSE,
  col.names = FALSE
)
write.table(
  rep(5, 9) * secuence,
  file = "cinco.txt",
  row.names = FALSE,
  col.names = FALSE
)
# 3. Escribe las cinco primeras filas de la matriz creada en el último ejercicio en un nuevo fichero llamado `prime.txt` y las cinco últimas en otro fichero llamado `fin.txt`. Ambos ficheros deben tener los datos separados por comas.
dos <- read.table("dos.txt")
tres <- read.table("tres.txt")
cinco <- read.table("cinco.txt")
matrix <- cbind(dos, tres, cinco)
write.table(
  matrix[1:5, ],
  file = "prime.txt",
  row.names = FALSE,
  col.names = FALSE, 
  sep = ",",
)
write.table(
  matrix[6:nrow(matrix), ],
  file = "fin.txt",
  row.names = FALSE,
  col.names = FALSE,
  sep = ","
)
# 4. Dados dos números, f y c (dados por el usuario mediante el teclado), cree una figura cuadrada de f filas y c columnas con el carácter "x" (sin espacios). Vea a continuación un ejemplo para f=4 y c=3 (notar que no hay espacios en blanco ni [1,] ni cosas raras...):
# > xxx<br/>
#  xxx<br/>
#  xxx<br/>
#  xxx<br/>
valid = FALSE
while (!valid) {
  cat("Introduzca dos numeros enteros separados por espacio\n")
  data <- readline()
  data <- unlist(strsplit(data, split = " "))
  f <- data[1]
  c <- data[2]
  valid_integer <- function(x) length(grep("^[0-9]+$", x))
  valid <- valid_integer(f) & valid_integer(c)
  if (!valid) {
    cat("Los valores introducidos no son válidos\n")
  }
}
cat(
  paste(
    rep(
      paste(
        rep("x", c),
        sep = "",
        collapse = ""),
      f), 
    "\n",
    sep = "",
    collapse = "" 
    )
  )
# 5. Cargue la primer y tercera hojas del fichero `resultados.xls` y muestre un gráfico que compare, para los dos datasets, el resultado en entrenamiento y test a medida que aumenta la cantidad de bits utilizados.
setwd("Master/ProgDatos/R")
library(tidyverse)
library(readxl)
sheet1 <- read_excel("IO/files/results.xlsx", skip = 1, sheet = 1)
sheet3 <- read_excel("IO/files/results.xlsx", skip = 1, sheet = 3)
data <- rbind(sheet1, sheet3)
print(data)
colnames(data)[1:3] <- c("Dataset", "Algorithm", "Bits")
data %>%
  pivot_longer(cols = c(Train, Test)) %>%
  ggplot(aes(x = Bits, y = value, color = name)) +
  geom_line() + 
  facet_wrap(~ Dataset) +
  labs(
    title = "Rendimiento según cantidad de Bits",
    y = "Accuracy",
    color = "Partición",
  ) +
  theme(legend.position = "top")
