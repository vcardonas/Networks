#==========================================================================#
#             Análisis Estadístico de Redes Sociales: Taller #3            #
#                         Valentina Cardona Saldaña                       #          
#==========================================================================#

#==================================#
#### 0. Instalación de paquetes ####
#==================================#
#update.packages()
library(intergraph)
library(igraph)
library(devtools)
#install_github("DougLuke/UserNetR")
library(UserNetR)
library(networkD3)
library(visNetwork)
library(htmlwidgets)
library(statnetWeb)
#install_github("gastonstat/arcdiagram")
library(arcdiagram)
library(statnet)
library(circlize)
library(sna)
library(ggplot2)
library(Hmisc)

#=====================================#
####  1. Capitulo 6 (Luke, 2015)   ####
#=====================================#
# Sintetizar y replicar el Capítulo 6 de Luke (2015).

## 6.1.1 Simple Interactive Networks in igraph
data(Bali)
iBali <- asIgraph(Bali)

set.seed(202424)
## Nota (Intel Macs): el uso de tcltk requiere XQuartz (versión 2.8.5 o posterior).
## Vuelva siempre a instalar XQuartz cuando actualice su macOS a una nueva versión principal.
## https://www.xquartz.org/
Coord <- tkplot(iBali, vertex.size=3,
                vertex.label=V(iBali)$role,
                vertex.color="darkgreen")
# Edit plot in Tk graphics window before
# running next two commands.
MCoords <- tkplot.getcoords(Coord)
plot(iBali, layout=MCoords, vertex.size=5,
     vertex.label=NA, vertex.color="lightblue")


## 6.1.2 Publishing Web-Based Interactive Network Diagrams
src <- c("A","A","B","B","C","E")
target <- c("B","C","C","D","B","C")
net_edge <- data.frame(src, target)
simpleNetwork(net_edge)
net_D3 <- simpleNetwork(net_edge)
saveNetwork(net_D3, file = 'Net_test1.html',
            selfcontained = TRUE)

iBali_edge <- get.edgelist(iBali)
iBali_edge <- iBali_edge - 1
iBali_edge <- data.frame(iBali_edge)
iBali_nodes <- data.frame(NodeID=as.numeric(V(iBali)-1),
                          Group=V(iBali)$role,
                          Nodesize=(igraph::degree(iBali)))
forceNetwork(Links = iBali_edge, Nodes = iBali_nodes,
             Source = "X1", Target = "X2",
             NodeID = "NodeID",Nodesize = "Nodesize",
             radiusCalculation="Math.sqrt(d.nodesize)*3",
             Group = "Group", opacity = 0.8,
             legend=TRUE)
net_D3 <- forceNetwork(Links = iBali_edge,
                       Nodes = iBali_nodes,
                       Source = "X1", Target = "X2",
                       NodeID = "NodeID",Nodesize = "Nodesize",
                       radiusCalculation="Math.sqrt(d.nodesize)*3",
                       Group = "Group", opacity = 0.8,
                       legend=TRUE)
saveNetwork(net_D3,file = 'Net_test2.html',
            selfcontained=TRUE)

iBali_edge <- get.edgelist(iBali)
iBali_edge <- data.frame(from = iBali_edge[,1],
                         to = iBali_edge[,2])
iBali_nodes <- data.frame(id = as.numeric(V(iBali)))
visNetwork(iBali_nodes, iBali_edge, width = "100%")
iBali_nodes$group <- V(iBali)$role
iBali_nodes$value <- degree(iBali)
net <- visNetwork(iBali_nodes, iBali_edge,
                  width = "100%",visLegend=TRUE)
visOptions(net,highlightNearest = TRUE)

net <- visNetwork(iBali_nodes, iBali_edge,
                  width = "100%",visLegend=TRUE)
net <- visOptions(net,highlightNearest = TRUE)
net <- visInteraction(net,navigationButtons = TRUE)
saveWidget(net, "Net_test3.html")

## 6.1.3 Statnet Web: Interactive statnet with shiny
run_sw()

## 6.2 Specialized Network Diagrams

### 6.2.1 Arc Diagrams
data(Simpsons)
iSimp <- asIgraph(Simpsons)
simp_edge <- get.edgelist(iSimp)
arcplot(simp_edge)

s_grp <- V(iSimp)$group
s_col = c("#a6611a", "#dfc27d","#80cdc1","#018571")
cols = s_col[s_grp]
node_deg <- igraph::degree(iSimp)
arcplot(simp_edge, lwd.arcs=2, cex.nodes=node_deg/2,
        labels=V(iSimp)$vertex.names,
        col.labels="darkgreen",font=1,
        pch.nodes=21,line=1,col.nodes = cols,
        bg.nodes = cols, show.nodes = TRUE)

### 6.2.2 Chord Diagrams
data(FIFA_Nether)
FIFAm <- as.sociomatrix(FIFA_Nether,attrname='passes')
names <- c("GK1","DF3","DF4","DF5","MF6",
           "FW7","FW9","MF10","FW11","DF2","MF8")
rownames(FIFAm) = names
colnames(FIFAm) = names
FIFAm

FIFAm[FIFAm < 10] <- 0
FIFAm

chordDiagram(FIFAm)
grid.col <- c("#AA3939",rep("#AA6C39",4),
              rep("#2D882D",3),rep("#226666",3))
chordDiagram(FIFAm,directional = TRUE,
             grid.col = grid.col,
             order=c("GK1","DF2","DF3","DF4","DF5",
                     "MF6","MF8","MF10","FW7",
                     "FW9","FW11"))

### 6.2.3 Heatmaps for Network Data
data(FIFA_Nether)
FIFAm <- as.sociomatrix(FIFA_Nether,attrname='passes')
colnames(FIFAm) <- c("GK1","DF3","DF4","DF5",
                     "MF6","FW7","FW9","MF10",
                     "FW11","DF2","MF8")
rownames(FIFAm) <- c("GK1","DF3","DF4","DF5",
                     "MF6","FW7","FW9","MF10",
                     "FW11","DF2","MF8")
palf <- colorRampPalette(c("#669999", "#003333"))
heatmap(FIFAm[,11:1],Rowv = NA,Colv = NA,col = palf(60),
        scale="none", margins=c(11,11) )

## 6.3 Creating Network Diagrams with Other R Packages

### 6.3.1 Network Diagrams with ggplot2

edgeMaker <- function(whichRow,len=100, curved = TRUE){
  fromC <- layoutCoordinates[adjacencyList[whichRow,1],]
  toC <- layoutCoordinates[adjacencyList[whichRow,2],]
  graphCenter <- colMeans(layoutCoordinates)
  bezierMid <- c(fromC[1], toC[2])
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1],
                                      fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
  }
  bezierMid <- (fromC + toC + bezierMid) / 3
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1],
                              toC[1]),
                            c(fromC[2], bezierMid[2],
                              toC[2]),
                            evaluation = len))
  edge$Sequence <- 1:len
  edge$Group <- paste(adjacencyList[whichRow, 1:2],
                      collapse = ">")
  return(edge)
}

data(FIFA_Nether)
fifa <- FIFA_Nether
fifa.edge <- as.edgelist.sna(fifa,attrname="passes")
fifa.edge <- data.frame(fifa.edge)
names(fifa.edge)[3] <- "value"
fifa.edge <- fifa.edge[fifa.edge$value > 9,]
adjacencyList <- fifa.edge

layoutCoordinates <- gplot(network(fifa.edge))
allEdges <- lapply(1:nrow(fifa.edge),
                   edgeMaker, len = 500, curved = TRUE)
allEdges <- do.call(rbind, allEdges)

new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0,0,-1,-1),
                                         unit = "lines",
                                         valid.unit = 3L,
                                         class = "unit")

zp1 <- ggplot(allEdges)
zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group,
                           colour=Sequence, linewidth=-Sequence))
zp1 <- zp1 + geom_point(data =
                          data.frame(layoutCoordinates),
                        aes(x = x, y = y),
                        size = 4, pch = 21,
                        colour = "black", fill = "gray")
zp1 <- zp1 + scale_colour_gradient(low = gray(0),
                                   high = gray(9/10),
                                   guide = "none")
zp1 <- zp1 + scale_size(range = c(1/10, 1.5),
                        guide = "none")
zp1 <- zp1 + new_theme_empty
print(zp1)

#=====================================#
####  2. Capitulo 9 (Luke, 2015)   ####
#=====================================#
# Sintetizar y replicar el Capítulo 9 de Luke (2015).

## 9.1 Defining Affiliation Networks

### 9.1.1 Affiliations as 2-Mode Networks
C1 <- c(1,1,1,0,0,0)
C2 <- c(0,1,1,1,0,0)
C3 <- c(0,0,1,1,1,0)
C4 <- c(0,0,0,0,1,1)
aff.df <- data.frame(C1,C2,C3,C4)
row.names(aff.df) <- c("S1","S2","S3","S4","S5","S6")

### 9.1.2 Bipartite Graphs
bn <- graph_from_biadjacency_matrix(aff.df)

plt.x <- c(rep(2,6),rep(4,4))
plt.y <- c(7:2,6:3)
lay <- as.matrix(cbind(plt.x,plt.y))

shapes <- c("circle","square")
colors <- c("blue","red")
set.seed(202424)
plot(bn,vertex.color=colors[V(bn)$type+1],
     vertex.shape=shapes[V(bn)$type+1],
     vertex.size=10,vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=0.9,
     layout=lay)

## 9.2 Affiliation Network Basics

### 9.2.1 Creating Affiliation Networks from Incidence Matrices

bn <- graph.incidence(aff.df)
bn

as_biadjacency_matrix(bn)
V(bn)$type
V(bn)$name

### 9.2.2 Creating Affiliation Networks from Edge Lists

el.df <- data.frame(rbind(c("S1","C1"),
                          c("S2","C1"),
                          c("S2","C2"),
                          c("S3","C1"),
                          c("S3","C2"),
                          c("S3","C3"),
                          c("S4","C2"),
                          c("S4","C3"),
                          c("S5","C3"),
                          c("S5","C4"),
                          c("S6","C4")))
el.df

bn2 <- graph_from_data_frame(el.df,directed=FALSE)
bn2

V(bn2)$type <- V(bn2)$name %in% el.df[,1]
bn2

edge_density(bn)==edge_density(bn2)

### 9.2.3 Plotting Affiliation Networks
set.seed(202424)
shapes <- c("circle","square")
colors <- c("blue","red")
plot(bn,vertex.color=colors[V(bn)$type+1],
     vertex.shape=shapes[V(bn)$type+1],
     vertex.size=10,vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=0.9)


### 9.2.4 Projections
bn.pr <- bipartite_projection(bn)
bn.pr

graph.density(bn.pr$proj1)
bn.student <- bn.pr$proj1
bn.class <- bn.pr$proj2
graph.density(bn.student)

get.adjacency(bn.student,sparse=FALSE,attr="weight")
get.adjacency(bn.class,sparse=FALSE,attr="weight")

shapes <- c("circle","square")
colors <- c("blue","red")
op <- par(mfrow=c(1,2))
set.seed(202424)
plot(bn.student,vertex.color="blue",
     vertex.shape="circle",main="Students",
     edge.width=E(bn.student)$weight*2,
     vertex.size=15,vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=1)
set.seed(202424)
plot(bn.class,vertex.color="red",
     vertex.shape="square",main="Classes",
     edge.width=E(bn.student)$weight*2,
     vertex.size=15,vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=1)
par(op)

## 9.3 Example: Hollywood Actors as an Affiliation Network

data(hwd)
h1 <- upgrade_graph(hwd)
h1

V(h1)$name[1:10]

V(h1)$type[1:10]

V(h1)$IMDBrating[1:10]

V(h1)$name[155:165]

V(h1)$shape <- ifelse(V(h1)$type==TRUE,
                      "square","circle")
V(h1)$shape[1:10]

V(h1)$color <- ifelse(V(h1)$type==TRUE,
                      "red","lightblue")

h2 <- subgraph.edges(h1, E(h1)[.inc(V(h1)[name %in%
                                           c("The Wolf of Wall Street", "Gangs of New York",
                                             "The Departed")])])
set.seed(202424)
plot(h2, layout = layout_with_kk)

graph.density(h1)

table(igraph::degree(h1,v=V(h1)[type==FALSE]))

mean(igraph::degree(h1,v=V(h1)[type==FALSE]))

V(h1)$deg <- igraph::degree(h1)
V(h1)[type==FALSE & deg > 4]$name

busy_actor <- data.frame(cbind(
  Actor = V(h1)[type==FALSE & deg > 4]$name,
  Movies = V(h1)[type==FALSE & deg > 4]$deg
))
busy_actor[order(busy_actor$Movies,decreasing=TRUE),]

for (i in 161:1365) {
  V(h1)[i]$totrating <- sum(V(h1)[nei(i)]$IMDBrating)
}

max(V(h1)$totrating,na.rm=TRUE)

pop_actor <- data.frame(cbind(
  Actor = V(h1)[type==FALSE & totrating > 40]$name,
  Popularity = V(h1)[type==FALSE &
                       totrating > 40]$totrating))
pop_actor[order(pop_actor$Popularity,decreasing=TRUE),]

for (i in 161:1365) {
  V(h1)[i]$avgrating <- mean(V(h1)[nei(i)]$IMDBrating)
}
num <- V(h1)[type==FALSE]$deg
avgpop <- V(h1)[type==FALSE]$avgrating
summary(lm(avgpop ~ num))

scatter.smooth(num,avgpop,col="lightblue",
               ylim=c(2,10),span=.8,
               xlab="Number of Movies",
               ylab="Avg. Popularity")

### 9.3.2 Analysis of the Actor and Movie Projections

h1.pr <- bipartite.projection(h1)
h1.act <- h1.pr$proj1
h1.mov <- h1.pr$proj2
h1.act
h1.mov

op <- par(mar = rep(0, 4))
set.seed(202424)
plot(h1.mov,vertex.color="red",
     vertex.shape="circle",
     vertex.size=(V(h1.mov)$IMDBrating)-3,
     vertex.label=NA)
par(op)


graph.density(h1.mov)
count_components(h1.mov)
igraph::components(h1.mov)$csize
table(E(h1.mov)$weight)

h2.mov <- induced_subgraph(h1.mov,
                           vids=clusters(h1.mov)$membership==1)
set.seed(202424)
plot(h2.mov,vertex.color="red",
     edge.width=sqrt(E(h1.mov)$weight),
     vertex.shape="circle",
     vertex.size=(V(h2.mov)$IMDBrating)-3,
     vertex.label=NA)

table(coreness(h2.mov))

h3.mov <- induced.subgraph(h2.mov,
                           vids=graph.coreness(h2.mov)>4)
h3.mov
set.seed(202424)
plot(h3.mov,vertex.color="red",
     vertex.shape="circle",
     edge.width=sqrt(E(h1.mov)$weight),
     vertex.label.cex=0.7,vertex.label.color="darkgreen",
     vertex.label.dist=0.3,
     vertex.size=(V(h3.mov)$IMDBrating)-3)

#===========================================#
####  3. Base considerada por mi misma   ####
#===========================================#
# Eliminar objetos del entorno
rm(list = ls())

# Librerías
library(dplyr)
library(ggplot2)
library(igraph)
library(viridis)

# Datos seleccionados
df_raw <- read.csv("Datos/reforma_tributaria_04_11_2022_5pm_mixed_spanish.csv")
df <- df_raw %>% filter(tweet_type == "retweet") %>% 
  select(from_user_id, to_user_id, from_user_screen_name,
         to_user_screen_name, from_user_botscore, to_user_botscore)

G1 <- graph_from_data_frame(select(df, from_user_screen_name, to_user_screen_name), directed = TRUE)
G1 <- simplify(G1)

class(G1)
ecount(G1)
vcount(G1)
is_connected(G1)

# Componente gigante
comp <- components(G1)
comp$csize[1]/vcount(G1) 

G <- subgraph(G1, which(comp$membership == 1))
df_gc <- df_raw[comp$membership == 1, ]

ecount(G)
vcount(G)
is_connected(G)

####  3.1 Caracterizar la centralidad de los nodos ####

# out-degree
out_dg <- igraph::degree(graph = G, mode = "out", normalized = TRUE)
head(sort(out_dg, decreasing = TRUE), n = 5)

# closeness centraliy normalizada
D <- distances(G)
n <-  vcount(G)
cc <- (n - 1)/apply(X = D, MARGIN = 1, FUN = sum)
head(sort(cc, decreasing = TRUE), n = 5)

# betweenness centrality normalizada
bc <- igraph::betweenness(graph = G, directed = TRUE, normalized = TRUE)
head(sort(bc, decreasing = TRUE), n = 5)

# eigen centraliy normalizada
Y <- as_adjacency_matrix(G, sparse = FALSE)
g <- graph_from_adjacency_matrix(Y)
evd <- eigen(Y)
ec <- eigen_centrality(graph = g, scale = TRUE)$vector
head(sort(ec, decreasing = TRUE), n = 5)

####  3.2 Visualizar la red ####
## con un diseño adecuado teniendo en cuenta la centralidad de los nodos. 

# visualizacion
set.seed(202424)
l <- layout_with_fr(G)

# grado de salida
colores <- rep("gray", length(out_dg)) # Todos los vértices
colores[which.max(out_dg)] <- "red"     # Vértice máximo de out_dg
plot(G, layout = l, vertex.size = 15*sqrt(out_dg), 
     vertex.frame.color = "black", vertex.label = NA,
     edge.width = 0.5, edge.color = "#a0a0a060", edge.arrow.size = 0.2,
     vertex.color = colores)

# cercanía
colores <- rep("gray", length(cc)) # Todos los vértices
colores[which.max(cc)] <- "red"     # Vértice máximo de cc
plot(G, layout = l, vertex.size = 10*sqrt(cc),
     vertex.frame.color = "black", vertex.label = NA,
     edge.width = 0.5, edge.color = "#a0a0a060", edge.arrow.size = 0.2,
     vertex.color = colores)

# valores propios
colores <- rep("gray", length(ec)) # Todos los vértices
colores[which.max(ec)] <- "red"     # Vértice máximo de out_dg
plot(G, layout = l, vertex.size = 15*sqrt(ec),
     vertex.frame.color = "black", vertex.label = NA,
     edge.width = 0.5, edge.color = "#a0a0a060", edge.arrow.size = 0.2,
     vertex.color = colores)
dev.off()

####  3.3 Identificar los puntos de articulación, los puntos aislados y las componentes ####

# red conectada?
is_connected(G)

# puntos aislados
V(G)[igraph::degree(G) == 0]
V(G1)[igraph::degree(G1) == 0]

# componentes
componentes <- decompose(G1)
length(componentes)
table(sapply(X = componentes, FUN = vcount))

# k-conectividad
vertex_connectivity(G)

# puntos de articulación
G_cv <- articulation_points(G)
length(G_cv)

# % de los vértices que son puntos de articulación.
length(G_cv)/vcount(G)

# visualización
colores <- rep("gray", vcount(G)) # Todos los vértices
colores[match(names(V(G)), names(G_cv))] <- "red"     # Vértices de puntos de articulación
plot(G, layout = l, vertex.size = 15*sqrt(out_dg),
     vertex.frame.color = "black", vertex.label = NA,
     edge.width = 0.5, edge.color = "#a0a0a060", edge.arrow.size = 0.2,
     vertex.color = colores)


####  3.4 Hacer la distribución de las distancia geodésica ####

# diámetro
diameter(G, directed = TRUE)
diameter(G, directed = FALSE)

# distancia geodésica promedio
mean_distance(G, directed = TRUE)
mean_distance(G, directed = FALSE)

# distribución de las distancias
distance_table(G, directed = TRUE)
distance_table(G, directed = FALSE)

# visualización
(senderos1 <- distance_table(G, directed = TRUE)$res)
(senderos2 <- distance_table(G, directed = FALSE)$res)

(names(senderos1) <- 1:length(senderos1))
(names(senderos2) <- 1:length(senderos2))

mis_colores <- viridis_pal()(length(prop.table(senderos1)))
barplot(prop.table(senderos1),
        xlab = "Distancia geodésica", ylab = "F. Relativa",
        border = "grey", col = mis_colores,
        ylim = c(0, max(prop.table(senderos1)) * 1.2),
        space = 0.2,
        cex.axis = 1.2, 
        cex.names = 1.2,
        cex.lab = 1.2,
        las = 2)

mis_colores <- viridis_pal()(length(prop.table(senderos2)))
barplot(prop.table(senderos2),
        xlab = "Distancia geodésica", ylab = "F. Relativa",
        border = "grey", col = mis_colores,
        ylim = c(0, max(prop.table(senderos2)) * 1.2),
        space = 0.2,
        cex.axis = 1.2, 
        cex.names = 1.2,
        cex.lab = 1.2,
        las = 2)
dev.off()

####  3.5 Determinar si la red es libre de escala ####

# Grado de entrada
d <- degree(G, mode = "in")
# distribución de grado de entrada
dd <- degree_distribution(G, mode = "in")
# Obtener el rango máximo de densidad en ambos gráficos
max_density <- max(dd[dd != 0])

# visualización
par(mfrow = c(1,2))
plot((0:max(d))[dd != 0], dd[dd != 0], log = "xy", pch = 16,
     col = adjustcolor("tomato3", 0.5), xlab = "Log-grado", ylab = "Log-densidad",
     main = "Distribución grado de entrada\n(log-log)")

# Grado de salida
d <- degree(G, mode = "out")
# distribución de grado de salida
dd <- degree_distribution(G, mode = "out")
min_density <- min(dd[dd != 0])

# Crear el mismo rango en el eje Y
plot((0:max(d))[dd != 0], dd[dd != 0], log = "xy", pch = 16,
     col = adjustcolor("tomato3", 0.5), xlab = "Log-grado", ylab = "Log-densidad",
     main = "Distribución grado de salida\n(log-log)", ylim = c(min_density, max_density))

dev.off()

####  3.6 Hacer un censo de los clanes y calcular el número clan ####

# Frecuencias de clanes
table(sapply(X = cliques(graph = as.undirected(G), min = 1, max = 100), FUN = length))

# Clanes máximos
largest_cliques(graph = G)

# Número clan
clique_num(graph = G)


####  3.7 Calcular la densidad junto con el coeficiente de agrupamiento de la red ####

# Densidad del dígrafo
ecount(G)/(vcount(G)*(vcount(G)-1))

# Transitividad / Coeficiente de agrupamiento
transitivity(graph = G, type = "global")


####  3.8 Particionar la red usando tres métodos de agrupamiento de su elección ####
## Visualizar los resultados obtenidos.

set.seed(202424)
c1 <- cluster_edge_betweenness  (G)
c2 <- cluster_walktrap          (G)
c3 <- cluster_label_prop        (G)

igraph_options(vertex.size = 3, vertex.frame.color = "black")
plot(G, vertex.label = NA, layout = l, vertex.color = c1$membership)
plot(G, vertex.label = NA, layout = l, vertex.color = c2$membership)
plot(G, vertex.label = NA, layout = l, vertex.color = c3$membership)

####  3.9 Hacer un análisis de asortatividad de la red ####

# Generar aleatoriamente un atributo
## 1: Masculino
## 2: Femenino
set.seed(202424)
sexo <- sample(c(1, 2), vcount(G), replace = TRUE)
# Asigna los atributos a los nodos
V(G)$sexo <- sexo

# Tabla
table(V(G)$sexo)

# Asortatividad nominal
v.types <- V(G)$sexo
assortativity_nominal(graph = G, types = v.types, directed = FALSE)

# Asortatividad grado
assortativity_degree(G)

# visualización
par(mfrow = c(1, 1), mar = c(4, 3, 3, 1))
set.seed(202424)
plot(G, vertex.label = NA, layout = l, vertex.color = ifelse(V(G)$sexo == 1, "sienna1", "khaki1"))

#=========================#
####  4. Referencias   ####
#=========================#

# Luke, D. A. (2015). A user’s guide to network analysis in R, volume 72. Springer












