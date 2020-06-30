# Análisis de preguntas del proceso de Rendición de Cuentas del año 2019

# Librerías ----

library(RColorBrewer)
library(tidytext)
library(wordcloud)
library(NLP)
library(tm)
library(tidyverse)
library(readr)

# Inicio

Fuentes <- read.delim("Data/Preguntas.txt", header = TRUE)

Fuentes$Medio <- as.character(Fuentes$Medio)
Fuentes$Pregunta <- as.character(Fuentes$Pregunta)

# Encoding(Fuentes)  <- "UTF-8"

docs <- Corpus(VectorSource(Fuentes$Pregunta))



# Convertir letras a minúsculas
docs <- tm_map(docs, content_transformer(tolower))

# Eliminar artículos, preposiciones, etc... del español
docs <- tm_map(docs, removeWords, stopwords("spanish"))

# Remover signos de puntuación
docs <- tm_map(docs, removePunctuation)

# Eliminar caracteres especiales

removeSpecialChars <- function(x) str_replace_all(x, "[[:punct:]]", " ")
docs <- tm_map(docs, removeSpecialChars)

# Eliminar números
docs <- tm_map(docs, removeNumbers)

# Remover palabras de manera manual

docs <- tm_map(docs, removeWords, c("pej", "i", "ii", "iii", "iv", "v", "vi", 
                                    "unc", "universidad", "nacional", "colombia",
                                    "cuáles", "cuales", "cuándo", "cómo", "cuál", "hacer",
                                    "saber", "gracias", "unal", "cuenta", "quisiera", "van",
                                    "launalcuenta", "tan", "pregunta", "gustaría", "así", "respecto")) 

# Eliminar espacios en blanco
# 
# docs <- tm_map(docs, stripWhitespace)
# 
# docs <- tm_map(docs, PlainTextDocument)

# Matriz

docs <- TermDocumentMatrix(docs)
m <- as.matrix(docs)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#Exportar

nn <- as.character(docs[["content"]]$content)
write.csv(nn, file = "MyData.csv",  row.names = FALSE)
