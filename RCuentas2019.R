# Análisis de preguntas del proceso de Rendición de Cuentas del año 2019

install.packages("knitr")

# Librerías ----

library(RColorBrewer)
library(tidytext)
library(wordcloud)
library(NLP)
library(tm)
library(tidyverse)
library(readr)
library(knitr)

# Inicio

Fuentes <- read.delim("Data/Preguntas.txt", header = TRUE)

Fuentes$Medio <- as.character(Fuentes$Medio)
Fuentes$Pregunta <- as.character(Fuentes$Pregunta)

# PREGUNTAS REDES SOCIALES - DIA AUDIENCIA

Redes <- Fuentes %>% filter(Fuentes != 'Página')


barplot(table(Fuentes$Medio))

ggplot(data=Fuentes, aes(Fuentes$Medio)) +
  geom_bar(fill = "#a6bddb")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, size = 5)+
  ggtitle("Cantidad de preguntas recibidas según medio de recepción", 
          subtitle = "Proceso de Rendición de Cuentas - 2019 \n") +
  xlab("\n Medio de recepción de preguntas") + ylab("Total de preguntas \n") +
  theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x =  element_text(size = 12, color = "red"),
    legend.position="top"
  )
  

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
                                    "launalcuenta", "tan", "pregunta", "gustaría", "así", "respecto",
                                    "cada", "puede", "puedo", "quiero", "hace", "parte", "realizar",
                                    "manera")) 

# Matriz

docs <- TermDocumentMatrix(docs)
m <- as.matrix(docs)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

write.csv(d, file = "Data/Frecuencias.csv",  row.names = FALSE)

# Eliminar espacios en blanco

docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
cc <- docs[["content"]][["content"]] 

write.csv(cc, file = "Data/General.csv",  row.names = FALSE)



# ANALISIS PREGUNTAS REDES SOCIALES

# Facebook
# Twitter
# Youtube






# Encoding(Fuentes)  <- "UTF-8"

docs <- Corpus(VectorSource(Redes$Pregunta))



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
                                    "launalcuenta", "tan", "pregunta", "gustaría", "así", "respecto",
                                    "cada", "puede", "puedo", "quiero", "hace", "parte", "realizar",
                                    "manera")) 

# Matriz

docs <- TermDocumentMatrix(docs)
m <- as.matrix(docs)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

write.csv(d, file = "Data/FrecRedes.csv",  row.names = FALSE)


# NUBES DE PALABRAS


pal <- brewer.pal(4,"Dark2")
pal1 <- brewer.pal(4,"Set1")

set.seed(6987452)

# Nubes de palabras general

Gen_30 <- wordcloud(NuevaCSV1$word, freq = NuevaCSV1$freq, min.freq=1, max.words=30,  color = pal, rot.per=0.35, scale=c(3,0.8))
Gen_50 <- wordcloud(NuevaCSV1$word, freq = NuevaCSV1$freq, min.freq=1, max.words=50,  color = pal, rot.per=0.35, scale=c(3,0.8))

# Nubes de palabras redes sociales

Red_20 <- wordcloud(Redes$word, freq = Redes$freq, min.freq=1, max.words=20,  color = pal1, rot.per=0.35, scale=c(3,0.8))



