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
dg <- as.undirected(dg)

# Crear matriz adyacente
A <- get.adjacency(dg,
                   sparse = FALSE) # Objeto matriz base
print(A)
class(A)

# ¿Simétrica?
isSymmetric(A)

# Versión vectorizada
(V <- A[lower.tri(A)])

#===============================#
####  6. Funciones Ubicación ####
#===============================#
# n = nodos
# i = fila
# j = columna
# k = index en el vector


## FUNCIÓN 1: Ubicación Vector a partir de Matriz

UbiVector <- function(n, i, j) {
  
  if ((i > n) | (j > n)){
    k <- "Operación inválida: i o j supera a n"
    }else{
    
      if (i != j){
      # Longitud del vector
      nK <- n * (n - 1) / 2
      
      # Crear matriz nxn
      matriz <- matrix(data = 0, nrow = n, ncol = n)
      # Crear vector
      vector <- 1:nK
      
      ## TRIANGULAR INFERIOR
      matriz[lower.tri(matriz, diag = FALSE)] <- vector
      ## TRIANGULAR SUPERIOR
      matriz[upper.tri(matriz, diag = FALSE)] <- vector
      
      # Valor en matriz
      k <- matriz[i, j]}else{
        k <- "Operación inválida: No se admiten reflexibidades"
      }
    }
  
  return(k)
}

# Verificación
UbiVector(n = 6, i = 7, j = 3)
UbiVector(n = 6, i = 4, j = 3)
UbiVector(n = 6, i = 3, j = 6)
UbiVector(n = 6, i = 5, j = 5)


## FUNCIÓN 2: Ubicación Matriz a partir de Vector

UbiMatriz <- function(n, k, type = c("both", "upper", "lower")) {
  nK <- n * (n - 1) / 2
  
  if (k > nK)
    return("Operación inválida: k supera la longitud del vector")
  
  if (k <= nK)
    # Crear matriz nxn
    matriz <- matrix(data = 0, nrow = n, ncol = n)
    # Crear vector
    vector <- 1:nK
    
    ## TRIANGULAR INFERIOR
    matriz[lower.tri(matriz, diag = FALSE)] <- vector
    
    ## TRIANGULAR SUPERIOR
    matriz[upper.tri(matriz, diag = FALSE)] <- vector
    
    ## Valores
    ij <- as.data.frame(which(matriz == k, arr.ind = TRUE))
    row.names(ij) <- c("Triangular inferior", "Triangular superior")
    colnames(ij) <- c("i", "j")
    
    ## Return
    if (type == "both")
      return(ij)
    
    if (type == "lower")
      return(ij[1,])
    
    if (type == "upper")
      return(ij[2,])
}

# Verificación
UbiMatriz(n = 6, k = 16, type = "both")
UbiMatriz(n = 6, k = 11, type = "both")
UbiMatriz(n = 6, k = 4, type = "lower")



