#==========================================================================#
#             Análisis Estadístico de Redes Sociales: Taller 4            #
#                         Valentina Cardona Saldaña                       #          
#==========================================================================#

#==================================#
#### 1. Instalación de paquetes ####
#==================================#
#update.packages()
library(readr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(wordcloud)
library(reshape2)

#====================================#
#### 2. Relaciones entre palabras ####
#====================================#

#### 2.1 Directorio ####
mypath <- "/Users/valentinacardona/Documents/Code Nerd/GitHub/Networks/1. Ejercicios varios/Datos"
setwd(mypath)

#### 2.2 Importación datos ####
# Transcripciones
datos <- read_csv("Caso03_Transcripcionesv1.csv")
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


#### 2.3 Tokenización ####
tokens <- datos %>%
  unnest_tokens(input = Transcripciones, output = word) %>%
  filter(!is.na(word)) 
class(tokens)
dim(tokens)

head(tokens)

tokens %>% 
  filter(grepl(pattern = '[0-9]', x = word)) %>% 
  count(word, sort = TRUE)

#### 2.4 Normalización tokens ####

## Remover texto con números
tokens %<>% 
  filter(!grepl(pattern = '[0-9]', x = word))
dim(tokens)
## Remover stop-words
tokens %<>% 
  anti_join(x = ., y = stop_words_es)
dim(tokens)
## Remover acentos
replacement_list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')
tokens %<>% 
  mutate(word = chartr(old = names(replacement_list) %>% str_c(collapse = ''), 
                       new = replacement_list %>% str_c(collapse = ''),
                       x = word))

#### 2.5 Descriptivos ####

# Tokens mas frecuentes
tokens %>% 
  count(word, sort = TRUE) %>%
  head(n = 10)

#set.seed(202404)
tokens %>% 
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'darkolivegreen4', alpha = 0.8) +
  xlab(NULL) +
  ylab("Frecuencia") +
  coord_flip() +
  ggtitle(label = 'Petro: Conteo de palabras')

tokens %>% 
  count(word, sort = TRUE) %>%
  with(wordcloud(words = word, freq = n, max.words = 50, colors = 'darkolivegreen4'))
title(main = "Nube de palabras")

# Frecuencias relativas de las palabras
# bind_rows(mutate(.data = text_petro, author = "petro"),
#           mutate(.data = text_duque, author = "duque")) %>%
#   count(author, word) %>%
#   group_by(author) %>%
#   mutate(proportion = n/sum(n)) %>%
#   select(-n) %>%
#   spread(author, proportion, fill = 0) -> frec  # importante!
# frec %<>%
#   select(word, petro, duque)
# dim(frec)
# head(frec, n = 10)

##### top 10 palabras en comun
# orden anidado respecto a petro y duque
# frec %>%
#   filter(petro !=0, duque != 0) %>%
#   arrange(desc(petro), desc(duque)) -> frec_comun
# dim(frec_comun)
# head(frec_comun, n = 10)

###### proporcion palabras en comun
# dim(frec_comun)[1]/dim(frec)[1]

##### correlacion de las frecuencias
# cuidado con los supuestos de la prueba
# es posible usar Bootstrap como alternativa
# cor.test(x = frec$duque, y = frec$petro)

# cor.test(x = frec_comun$duque, y = frec_comun$petro)

#### 2.6 Análisis de sentimiento ####

tokens %>%
  inner_join(sentiment_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(n > 1500) %>%
  mutate(n = ifelse(sentiment == "Negativo", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  scale_fill_manual(values = brewer.pal(8,'Dark2')[c(2,5)]) +
  coord_flip() +
  labs(y = "Frecuencia",
       x = NULL,
       title = "Conteo por sentimento") +
  theme_minimal()

#set.seed(202404)
tokens %>%
  inner_join(sentiment_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(8,'Dark2')[c(2,5)], 
                   max.words = 50, title.size = 1.5)
title(main = "Nube")



#### 2.7 Bigramas ####

bigramas <- datos
text_petro %>%
  unnest_tokens(tbl = ., input = text, output = bigram, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) -> text_petro_bi  # importante!
dim(text_petro_bi)













