#==========================================================================#
#                      Networks: Primer Acercamiento                       #
#                       Redes Sociales de Conflicto                        #          
#==========================================================================#

#=================================#
#### 1. Instalación de paquete ####
#=================================#

# install.packages("igraph")
library(igraph)

#============================#
#### 2. Definición de red ####
#============================#

# Estudio empírico llevado a cabo el día 11/02 de 2024 
# sobre interacciones conflictivas en un círculo familiar

# Id - Personajes:
## 1 - Matriarca
## 2 - Hija 1 (26 años)
## 3 - Hijo 2 (23 años)
## 4 - Hija 3 (21 años)
## 5 - Hijo 4 (10 años)
## 6 - Hijo 5 (5 años)
## 7 - Hija política 1 (23 años)
## 8 - Hija política 2 (23 años)
## 9 - Hijo político 3 (21 años)

## Considerando quién inició la interacción conflictiva y quién la recibió,
## se establece una relación dirigida (dígrafo) en el análisis.

# Definir red
dg <- graph_from_literal(# Incluir todos los individuos
                        1, 2, 3, 4, 5, 6, 7, 8, 9, 
                        # Registrar interacciones 
                        1-+ 2, 1-+ 2, 1-+ 2,
                        1 -+ 4, 1 -+ 4,
                        1 -+ 5, 1 -+ 5, 1 -+ 5, 1 -+ 5, 1 -+ 5,
                        1 -+ 6, 1 -+ 6, 1 -+ 6, 1 -+ 6,
                        1 -+ 9, 1 -+ 9,
                        2 -+ 1, 2 -+ 6, 2 -+ 7, 2 -+ 7, 2 -+ 8, 2 -+ 8, 
                        9 -+ 2, simplify = FALSE)

## Clase
class(dg)

## Identificador
graph_id(dg)

## Vértices
V(dg)

## Orden
vcount(dg)

## Aristas
E(dg)

## Tamaño
ecount(dg)

## ¿Ponderada?
is_weighted(dg)

## ¿Simple?
is_simple(dg)

#===========================#
#### 3. Características ####
#===========================#
# Agregar ponderación
E(dg)$weight <- 1
dg <- simplify(dg, edge.attr.comb = list(weight = "sum"))

# Etiquetas
nombres <- c("Matriarca", "Hija 1", "Hijo 2", "Hija 3", "Hijo 4", "Hijo 5",
             "Hija política 1", "Hija política 2", "Hijo político 3")
V(dg)$name <- nombres

# Atributos
## Sexo
V(dg)$sexo <- c("F", "F", "M", "F", "M", "M",
                "F", "F", "M")

## Edad
V(dg)$edad <- c(46, 26, 23, 21, 10, 5,
                23, 23, 21)

## ¿presente en el momento de la toma de datos?
V(dg)$presente <- c("Sí", "Sí", "No", "Sí", "Sí", "Sí",
                    "Sí", "No", "Sí")

#=====================#
####  4. Gráfica  ####
#====================#
set.seed(12345)
plot(dg, main = "Redes Sociales de Conflicto \n2 Horas de Interacción",
     vertex.size = 30, vertex.color = ifelse(V(dg)$presente == "Sí", "lightgreen", "lightsalmon"),
     edge.width = E(dg)$weight, edge.color = "burlywood")

#============================#
####  5. Matriz Adyacente ####
#============================#
# Convertir a Matriz - Red no dirigida
A <- matrix(data = c(0, 4, 0, 2, 5, 4, 0, 0, 2,
                4, 0, 0, 0, 0, 1, 2, 2, 1,
                rep(0, 9),
                2, rep(0, 8),
                5, rep(0, 8),
                4, 1, rep(0, 7),
                0, 2, rep(0,7),
                0, 2, rep(0,7),
                2, 1, rep(0,7)), nrow = 9, ncol = 9)
row.names(A) <- nombres
colnames(A) <- nombres

print(A)

# Función vectorización de la triangular inferior
vectorizacion <- function(matriz) {
  n <- dim(matriz)[1]
  vector <- c()
  # Contador
  c <- 1
  for (i in 1:(n-1)) {
   vector <- c(vector, as.vector(matriz[(c+1):n, c]))
   c <- c + 1
  }
  return(vector)
}

v <- vectorizacion(A)

# Verificación 
length(v) == (dim(A)*(dim(A)-1)/2)[1]


# Función de vector a triangular inferior
a_matriz <- function(vector, n) {
  matriz <- matrix(data = 0, nrow = n, ncol = n)
  # Contador
  c <- 1
  for (i in 1:(n-1)) {
    num <- length(matriz[(c+1):n, c])
    matriz[(c+1):n, c] <- vector[1:num]
    vector <- vector[(num+1):length(vector)]
    c <- c + 1
  }
  return(matriz)
}

M <- a_matriz(v, n = 9)

# Verificación
A[upper.tri(A)] <- 0
any(M == A)
