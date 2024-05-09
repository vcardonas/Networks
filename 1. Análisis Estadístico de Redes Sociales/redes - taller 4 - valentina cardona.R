#==========================================================================#
#             Análisis Estadístico de Redes Sociales: Taller 4            #
#                         Valentina Cardona Saldaña                       #          
#==========================================================================#

#==============================================#
#### 1. Instalación de paquetes y funciones ####
#==============================================#
#update.packages()
library(readr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(wordcloud)
library(reshape2)
library(stringr)
library(corrr)
library(igraph)
library(ggraph)
library(graphlayouts)

# Función para asignar el valor correspondiente a la nueva columna
match_subcasos <- function(titulo) {
  if (grepl("casanare", titulo, ignore.case = TRUE)) {
    return("Casanare")
  } else if (grepl("huila", titulo, ignore.case = TRUE)) {
    return("Huila")
  } else if (grepl("antioquia", titulo, ignore.case = TRUE)) {
    return("Antioquia")
  } else if (grepl("dabeiba", titulo, ignore.case = TRUE)) {
    return("Antioquia")
  } else if (grepl("costa caribe", titulo, ignore.case = TRUE)) {
    return("Costa Caribe")
  } else if (grepl("arhuacos", titulo, ignore.case = TRUE)) {
    return("Costa Caribe")
  } else if (grepl("barranquilla", titulo, ignore.case = TRUE)) {
    return("Costa Caribe")
  } else if (grepl("norte de santander", titulo, ignore.case = TRUE)) {
    return("Norte de Santander")
  } else {
    return("Otros")}
}

# Función para crear un grafo a partir de un dataframe
create_graph <- function(df_group) {
  # Umbral
  df <- df_group %>% select(word1, word2, weight) %>% filter(weight > 2)
  # Grafo
  g <- df %>% graph_from_data_frame(directed = FALSE)
  g <- simplify(g)
  # Componente conexa
  V(g)$cluster <- components(graph = g)$membership
  g <- induced_subgraph(graph = g, vids = which(V(g)$cluster == which.max(components(graph = g)$csize)))
  return(g)
}

#====================================#
#### 2. Relaciones entre palabras ####
#====================================#

#### 2.1 Directorio ####
mypath <- "/Users/valentinacardona/Documents/Code Nerd/GitHub/Networks/1. Ejercicios varios/Datos"
setwd(mypath)

#### 2.2 Importación datos ####
# Transcripciones
datos <- read_csv("Caso03_Transcripciones.csv", show_col_types = FALSE)
datos

# Stopwords
### de https://github.com/stopwords-iso/stopwords-es
stop_words_es <- tibble(word = unlist(c(read.table("stopwords-es.txt", quote="\"", comment.char=""))), lexicon = "custom")
dim(stop_words_es)

# Diccionarios
### de https://www.kaggle.com/datasets/rtatman/sentiment-lexicons-for-81-languages
positive_words <- read_csv("positive_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Positivo")
negative_words <- read_csv("negative_words_es.txt", col_names = "word", show_col_types = FALSE) %>%
  mutate(sentiment = "Negativo")
sentiment_words <- bind_rows(positive_words, negative_words)

#### 2.3 Procesamiento de datos ####
# Todo minúsculas
datos$Títulos <- tolower(datos$Títulos)
# Extraer subcasos
datos %<>%
  rowwise() %>%
  mutate(Subcaso = match_subcasos(Títulos)) %>%
  ungroup()
# Excluidos
## Audiencias excluidas
aud_excl <- datos$Títulos[(datos$Subcaso == "Otros") & (grepl("audiencia", datos$Títulos)
                     | grepl("sargento|soldado|teniente|coronel|capitán|mayor|subintendente", datos$Títulos))
                     & !grepl("¿|trailer|prensa",datos$Títulos)]
length(aud_excl)
## Otros
length(datos$Títulos[(datos$Subcaso == "Otros") & !datos$Títulos %in% aud_excl])
## Total
table(datos$Subcaso)

subcasos <- datos %>%
  select(ID, Subcaso, Transcripciones) %>% 
  filter((Subcaso != "Otros") & (Subcaso != "Norte de Santander"))
nrow(subcasos)

#### 2.4 Tokenización ####
tokens <- subcasos %>%
  unnest_tokens(input = Transcripciones, output = word) %>%
  filter(!is.na(word)) 

class(tokens)
dim(tokens)
head(tokens)

#### 2.5 Normalización tokens ####

# Remover texto con números
tokens %<>% 
  filter(!grepl(pattern = '[0-9]', x = word))
dim(tokens)
# Remover stop-words
tokens %<>% 
  anti_join(x = ., y = stop_words_es)
dim(tokens)
# Remover acentos
replacement_list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')
tokens %<>% 
  mutate(word = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                       new = replacement_list %>% str_c(collapse = ''),
                       x = word))
# Remover "eh"
tokens %<>% filter(word != "eh" & word != "digamos")
dim(tokens)

# Conteo
table(tokens$Subcaso)

#### 2.6 Descriptivos ####

# Tokens mas frecuentes
tokens %>% 
  count(word, sort = TRUE) %>%
  head(n = 10)
# Paleta de colores para cada subcaso
palette <- c("magenta4", "cyan3", "chartreuse3", "orchid3")

# Barras
(p <- tokens %>%
  count(Subcaso, word, sort = TRUE) %>%
  group_by(Subcaso) %>%
  slice_max(n, n = 10) %>%
  mutate(word = reorder_within(word, n, Subcaso)) %>% 
  ggplot(aes(x = word, y = n, label = n, fill = Subcaso)) + # Añade fill = Subcaso aquí
  theme_light() + 
  geom_col(alpha = 0.8) +  # Elimina fill de geom_col
  geom_text(aes(x = word, y = n, label = n), hjust = -0.3, color = "black", size = 3) +  
  scale_x_reordered() +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  facet_wrap(~Subcaso, scales = "free_y") +
  scale_fill_manual(values = palette) +  # Aquí se especifica la paleta de colores
  theme(text = element_text(family = "Helvetica", size = 15),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(face = "bold", color = "white"),
        legend.position = "none"))
#ggsave("Top10_Subcaso.png", plot = p, width = 17, height = 8, units = "in", dpi = 600)

# Nube de palabras
## -- Antioquia --
png("WC_Antioquia.png", width = 6, height = 6, units = "in", res = 600)
set.seed(2024)
tokens %>%
  filter(Subcaso == "Antioquia") %>% 
  count(word, sort = TRUE) %>%
  with(wordcloud(words = word, freq = n, max.words = 100, rot.per = .5, colors = palette[1]))
title(main = "Antioquia")
dev.off()
## -- Casanare --
png("WC_Casanare.png", width = 6, height = 6, units = "in", res = 600)
set.seed(2024)
tokens %>%
        filter(Subcaso == "Casanare") %>% 
        count(word, sort = TRUE) %>%
        with(wordcloud(words = word, freq = n, max.words = 100, rot.per = .5, colors = palette[2]))
title(main = "Casanare")
dev.off()
## -- Costa Caribe --
png("WC_Costa_Caribe.png", width = 6, height = 6, units = "in", res = 600)
set.seed(2024)
tokens %>%
        filter(Subcaso == "Costa Caribe") %>% 
        count(word, sort = TRUE) %>%
        with(wordcloud(words = word, freq = n, max.words = 100, rot.per = .5, colors = palette[3]))
title(main = "Costa Caribe")
dev.off()
## -- Huila --
png("WC_Huila.png", width = 6, height = 6, units = "in", res = 600)
set.seed(2024)
tokens %>%
        filter(Subcaso == "Huila") %>% 
        count(word, sort = TRUE) %>%
        with(wordcloud(words = word, freq = n, max.words = 100, rot.per = .5, colors = palette[4]))
title(main = "Huila")
dev.off()

dev.new()

# Proporción palabras en comun
frec <- tokens %>% 
  count(Subcaso, word) %>%
  group_by(Subcaso) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(Subcaso, proportion, fill = 0)
frec_comun <- frec %>% 
  filter(Antioquia != 0, Casanare != 0, `Costa Caribe` != 0, Huila != 0) %>% 
  arrange(desc(Antioquia), desc(Casanare), desc(`Costa Caribe`), desc(Huila))
dim(frec_comun)[1]/dim(frec)[1]

# Correlación de las frecuencias
frec %>% correlate()

#### 2.7 Análisis de sentimiento ####
# Barras
tokens %>%
  filter(!word %in% c("señor", "tipo", "mire")) %>% 
  inner_join(sentiment_words) %>%
  count(Subcaso, word, sentiment, sort = TRUE) %>%
  group_by(Subcaso, sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(n = ifelse(sentiment == "Negativo", -n, n)) %>%
  mutate(word = reorder_within(word, n, sentiment)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  scale_fill_manual(values = brewer.pal(8,'Dark2')[c(2,5)]) +
  scale_x_reordered() +
  coord_flip() +
  labs(y = NULL, x = NULL) +
  facet_wrap(~Subcaso, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica", size = 15),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(face = "bold", color = "white"),
        legend.position = "none")

#### 2.8 Redes ####

# Crear bigramas
bigramas <- subcasos
bigramas %<>%
  group_by(Subcaso) %>% 
  unnest_tokens(tbl = ., input = Transcripciones, output = bigram, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  ungroup()
dim(bigramas)

bigramas %<>%
  # Normalización de bigramas
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!grepl('[0-9]', word1) & 
           !grepl('[0-9]', word2) &
           !word1 %in% stop_words_es$word &
           !word2 %in% stop_words_es$word) %>%
  mutate(word1 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word1)) %>%
  mutate(word2 = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                        new = replacement_list %>% str_c(collapse = ''),
                        x = word2)) %>%
  na.omit() %>%
  # Contar los bigramas
  count(Subcaso, word1, word2, sort = TRUE) %>%
  rename(weight = n) %>% 
  filter(word1 != word2 &
           word1 != "eh" &
           word2 != "eh" &
           word1 != "digamos" &
           word2 != "digamos")
dim(bigramas)
head(bigramas, n = 10)

# Crear gráfos
graphs <- lapply(split(bigramas, bigramas$Subcaso), create_graph)
print(graphs)

## -- Antioquia --
set.seed(2024)
ly <- layout_with_dh(graphs$Antioquia)
top_vertices <- names(sort(betweenness(graph = graphs$Antioquia, normalized = TRUE), decreasing = TRUE)[1:10])
png("red_Antioquia.png", width = 8, height = 8, units = "in", res = 600)
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(graphs$Antioquia, layout = ly,
     vertex.color = adjustcolor(palette[1], 0.1),
     vertex.frame.color = palette[1],
     vertex.size = 30*sqrt(betweenness(graph = graphs$Antioquia, normalized = TRUE)),
     vertex.label = ifelse(V(graphs$Antioquia)$name %in% top_vertices, V(graphs$Antioquia)$name, NA),
     vertex.label.color = "black",
     vertex.label.font = 2)  # '2' para negrilla
title(main = "Antioquia", outer = TRUE, line = -1)
dev.off()

## -- Casanare --
bc <- betweenness(graphs$Casanare, normalized = TRUE)
top_vertices <- names(sort(bc, decreasing = TRUE)[1:10])
set.seed(123)
png("red_Casanare.png", width = 8, height = 10, units = "in", res = 600)
ggraph(graphs$Casanare, layout = "nicely") +
  geom_edge_link0(edge_color = "lightgrey", edge_width = 0.2) +
  geom_node_point(fill = adjustcolor(palette[2], alpha.f = 0.5), shape = 21, size = 50*sqrt(bc), color = palette[2]) +
  geom_node_text(aes(label = ifelse(name %in% top_vertices, name, "")), color = "black", fontface = "bold") +
  theme_graph() +
  labs(title = "Casanare") +
  theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0.001, 0.001, 0.001, 0.001), "lines"))
dev.off()

## -- Costa Caribe --
bc <- betweenness(graphs$`Costa Caribe`, normalized = TRUE)
top_vertices <- names(sort(bc, decreasing = TRUE)[1:10])
set.seed(12)
png("red_Costa_Caribe.png", width = 10, height = 10, units = "in", res = 600)
ggraph(graphs$`Costa Caribe`, layout = "sparse_stress", pivots = 100) +
  geom_edge_link0(edge_color = "lightgrey", edge_width = 0.2) +
  geom_node_point(fill = adjustcolor(palette[3], alpha.f = 0.5), shape = 21, size = 50*sqrt(bc), color = palette[3]) +
  geom_node_text(aes(label = ifelse(name %in% top_vertices, name, "")), color = "black", fontface = "bold") +
  theme_graph() +
  labs(title = "Costa Caribe") +
  theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0.001, 0.001, 0.001, 0.001), "lines"))
dev.off()

## -- Huila --
bc <- betweenness(graphs$Huila, normalized = TRUE)
top_vertices <- names(sort(bc, decreasing = TRUE)[1:10])
set.seed(123)
png("red_Huila.png", width = 8, height = 10, units = "in", res = 600)
ggraph(graphs$Huila, layout = "nicely") +
  geom_edge_link0(edge_color = "lightgrey", edge_width = 0.2) +
  geom_node_point(fill = adjustcolor(palette[4], alpha.f = 0.5), shape = 21, size = 50*sqrt(bc), color = palette[4]) +
  geom_node_text(aes(label = ifelse(name %in% top_vertices, name, "")), color = "black", fontface = "bold") +
  theme_graph() +
  labs(title = "Huila") +
  theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0.001, 0.001, 0.001, 0.001), "lines"))
dev.off()


