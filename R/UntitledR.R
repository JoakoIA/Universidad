#Escriba un programa en R para crear una secuencia de números del 20 al 50 y encuentre la media de los números del 20 al 60 y la suma de los números del 51 al 91.

lo_basico.1 <- function(){
  secuencia <- 20:50
  media_20_60 <- mean(20:60)
  suma_51_91 <- sum(51:91)
  return(list(secuencia = secuencia, media_20_60 = media_20_60, suma_51_91 = suma_51_91))
}
lo_basico.1()

#Escriba un programa R para imprimir los números del 1 al 100 e imprime “Fizz” para múltiplos de 3, imprime “Buzz” para múltiplos de 5 e imprime “FizzBuzz” para múltiplos de ambos.
lo_basico.2 <- function(n) {
  for (i in 1:n) {
    if (i %% 3 == 0 && i %% 5 == 0) {
      print("FizzBuzz")
    } else if (i %% 3 == 0) {
      print("Fizz")
    } else if (i %% 5 == 0) {
      print("Buzz")
    } else {
      print(i)
    }
  }
}
lo_basico.2(100)

#Escriba un programa R para obtener los primeros 10 números de Fibonacci.
lo_basico.3 <- function(n) {
  fibonacci <- numeric(n)
  fibonacci[1] <- 0
  fibonacci[2] <- 1
  
  for (i in 3:n) {
    fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2]
  }
  return(fibonacci)
}

lo_basico.3(30)

#Escriba un programa en R para obtener todos los números primos hasta un número dado (basado en la criba de Eratóstenes).
lo_basico.4 <- function(n) {
  sieve <- rep(TRUE, n)
  sieve[1] <- FALSE
  
  for (i in 2:sqrt(n)) {
    if (sieve[i]) {
      sieve[i * i:n] <- FALSE
    }
  }
  
  return(which(sieve))
}

lo_basico.4(40)

#Escriba un programa en R para crear una curva de campana de una distribución normal aleatoria
lo_basico.5 <- function(media, std, tamano) {
  random_values <- rnorm(tamano, mean = media, sd = std)
  hist(random_values, breaks = 30, probability = TRUE)
  curve(dnorm(x, mean = media, sd = std), add = TRUE, col = "red")
}

lo_basico.5(0, 1, 1000)

#Escriba un programa R para crear una lista de elementos usando vectores, matrices y funciones. Imprime el contenido de la lista.
lo_basico.6 <- function(){
  
  
}

#Escriba un programa en R para crear una matriz de 5 x 4, una matriz de 3 x 3 con etiquetas y rellene la matriz por filas y una matriz de 2 x 2 con etiquetas y rellene la matriz por columnas.
lo_basico.7 <- function(rows, cols) {
  matriz <- matrix(nrow = rows, ncol = cols)
  value <- 1
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      matriz[i, j] <- value
      value <- value + 1
    }
  }
  
  return(matriz)
}

lo_basico.7(5,4)
#Escriba un programa en R para crear una matriz, pasando un vector de valores y un vector de dimensiones. También proporcione nombres para cada dimensión.
lo_basico.8 <- function(){
  
  
}
#Escriba un programa R para crear una matriz con tres columnas, tres filas y dos “matrices”, tomando dos vectores como entrada para la matriz. Imprime la matriz.
lo_basico.9 <- function(vec1, vec2) {
  matriz <- matrix(nrow = 3, ncol = 3)
  
  matriz[, 1] <- vec1
  matriz[, 2] <- vec2
  
  return(matriz)
}

lo_basico.9(c(1, 2, 3), c(1, 2, 3))
print(combined_matrix)
#Escriba un programa R para crear un data.frame que contenga detalles (genero, edad, rut, dirección, profesión) de 5 empleados y muestre un resumen de los datos.
lo_basico.10 <- function(){
  genero <- c("Male", "Female", "Male", "Female", "Male")
  edad <- c(30, 25, 40, 35, 28)
  rut <- c("1234567-8", "9876543-2", "5678901-5", "3456789-0", "2109876-5")
  direccion <- c("Street A", "Street B", "Street C", "Street D", "Street E")
  profesion <- c("Engineer", "Doctor", "Teacher", "Artist", "Programmer")
  
  employees_df <- data.frame(Gender = genero, Age = edad, RUT = rut, Address = direccion, Profession = profesion)
  return(employees_df)
}
lo_basico.10()


#Vectores.....................

#Escriba un programa en R para crear un vector de un tipo y una longitud específicos. Cree vectores de tipos numéricos, complejos, lógicos y de caracteres de longitud 6.
vector_1 <- function() {
  vector_numerico <- numeric(6)
  vector_complejo <- complex(real = 1:6, imaginary = 6:1)
  vector_logico <- logical(6)
  vector_caracter <- letters[1:6]
  
  return(list(numerico = vector_numerico, complejo = vector_complejo, logico = vector_logico, caracter = vector_caracter))
}

resultado_1 <- vector_1()
print(resultado_1$numerico)
print(resultado_1$complejo)
print(resultado_1$logico)
print(resultado_1$caracter)

#Escriba un programa en R para sumar dos vectores de tipo entero y longitud 3.
vector_2 <- function() {
  vector1 <- c(1, 2, 3)
  vector2 <- c(4, 5, 6)
  resultado <- vector1 + vector2
  return(resultado)
}

resultado_2 <- vector_2()
print(resultado_2)

#Escriba un programa R para agregar valores a un vector vacío dado.
vector_3 <- function() {
  vector_vacio <- numeric(0)
  vector_nuevo <- c(vector_vacio, 7, 8, 9)
  return(vector_nuevo)
}

resultado_3 <- vector_3()
print(resultado_3)

#Escriba un programa en R para encontrar la suma, la media y el producto de un vector.
vector_4 <- function() {
  vector <- c(2, 4, 6, 8, 10)
  suma <- sum(vector)
  media <- mean(vector)
  producto <- prod(vector)
  
  return(list(suma = suma, media = media, producto = producto))
}

resultado_4 <- vector_4()
print(resultado_4$suma)
print(resultado_4$media)
print(resultado_4$producto)


#Escriba un programa R para encontrar la suma, la media y el producto de un vector, ignore elementos como NA o NaN.
vector_5 <- function() {
  vector <- c(2, 4, NA, 8, 10)
  suma <- sum(vector, na.rm = TRUE)
  media <- mean(vector, na.rm = TRUE)
  producto <- prod(vector, na.rm = TRUE)
  
  return(list(suma = suma, media = media, producto = producto))
}

resultado_5 <- vector_5()
print(resultado_5$suma)
print(resultado_5$media)
print(resultado_5$producto)

#Escriba un programa en R para ordenar un Vector en orden ascendente y descendente.
vector_6 <- function() {
  vector <- c(5, 1, 9, 3, 7)
  orden_ascendente <- sort(vector)
  orden_descendente <- sort(vector, decreasing = TRUE)
  
  return(list(ascendente = orden_ascendente, descendente = orden_descendente))
}

resultado_6 <- vector_6()
print(resultado_6$ascendente)
print(resultado_6$descendente)

#Escriba un programa R para probar si un vector dado contiene un elemento específico.
vector_7 <- function(elemento_buscar) {
  vector <- c(3, 6, 9, 12, 15)
  contiene_elemento <- elemento_buscar %in% vector
  return(contiene_elemento)
}

elemento_a_buscar <- 9
resultado_7 <- vector_7(elemento_a_buscar)
print(resultado_7)

#Escriba un programa R para encontrar el segundo valor más alto en un vector dado. Haga clic en mí para ver la solución de muestra
vector_8 <- function() {
  vector <- c(7, 2, 10, 5, 9)
  segundo_maximo <- max(vector[vector != max(vector)])
  return(segundo_maximo)
}

resultado_8 <- vector_8()
print(resultado_8)

#Escriba un programa en R para encontrar el enésimo valor más alto en un vector dado.
vector_9 <- function(n) {
  vector <- c(7, 2, 10, 5, 9)
  if (n > length(vector)) {
    return("N es mayor que la longitud del vector")
  }
  
  valores_ordenados <- sort(vector, decreasing = TRUE)
  enesimo_maximo <- valores_ordenados[n]
  
  return(enesimo_maximo)
}

n_valor <- 3
resultado_9 <- vector_9(n_valor)
print(resultado_9)

#Escriba un programa en R para convertir la(s) columna(s) dada(s) de un data.frame en un vector.
vector_10 <- function(dataframe, columnas) {
  vector_resultado <- unlist(dataframe[, columnas])
  return(vector_resultado)
}

dataframe_ejemplo <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
columnas_a_convertir <- c("A", "B")
resultado_10 <- vector_10(dataframe_ejemplo, columnas_a_convertir)
print(resultado_10)

#Escriba un programa R para encontrar los elementos de un vector dado que no están en otro vector dado.
vector_11 <- function(vector1, vector2) {
  elementos_faltantes <- setdiff(vector1, vector2)
  return(elementos_faltantes)
}

vector1 <- c(1, 2, 3, 4, 5)
vector2 <- c(3, 4, 5, 6, 7)
resultado_11 <- vector_11(vector1, vector2)
print(resultado_11)

#Escriba un programa en R para invertir el orden del vector dado. Haga clic en mí para ver la solución de muestra
vector_12 <- function(vector) {
  vector_invertido <- rev(vector)
  return(vector_invertido)
}

vector_original <- c(10, 20, 30, 40, 50)
resultado_12 <- vector_12(vector_original)
print(resultado_12)

#Escriba un programa en R para crear un vector y encuentre la longitud y la dimensión del vector.
vector_13 <- function() {
  vector1 <- 1:10
  vector2 <- seq(from = 1, to = 10, by = 2)
  return(list(op_colon = vector1, funcion_seq = vector2))
}

resultado_13 <- vector_13()
print(resultado_13$op_colon)
print(resultado_13$funcion_seq)

#Escriba un programa R para probar si el valor del elemento de un vector dado es mayor que 10 o no. Devuelve VERDADERO o FALSO.
vector_14 <- function(vector) {
  resultado <- vector > 10
  return(resultado)
}

vector_ejemplo <- c(5, 12, 8, 15, 6)
resultado_14 <- vector_14(vector_ejemplo)
print(resultado_14)


#Escriba un programa en R para sumar 3 a cada elemento en un vector dado. Imprime el vector original y el nuevo.
vector_15 <- function(vector) {
  vector_sumado <- vector + 3
  return(list(original = vector, sumado = vector_sumado))
}

vector_ejemplo <- c(2, 5, 8, 11)
resultado_15 <- vector_15(vector_ejemplo)
print(resultado_15$original)
print(resultado_15$sumado)

#Escriba un programa en R para crear un vector usando el operador : y la función seq().
vector_15 <- function(vector) {
  vector_sumado <- vector + 3
  return(list(original = vector, sumado = vector_sumado))
}

vector_ejemplo <- c(2, 5, 8, 11)
resultado_15 <- vector_15(vector_ejemplo)
print(resultado_15$original)
print(resultado_15$sumado)

#Matrices...............

