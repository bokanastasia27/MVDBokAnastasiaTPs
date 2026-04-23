message("metrics")
install.packages('tm')
library(here)
library(tidyverse)
library(tidytext)
library(tm)
library(ggplot2)

# Leer texto procesado
input_texto_procesado <- here("TP2", "output", "processed_text.rds")
message("Leyendo archivo: ", input_texto_procesado)

processed_text <- readRDS(input_texto_procesado)

#Matriz de frec. de terminos 
dtm <- processed_text %>%
  count(doc_id, lemma) %>%
  cast_dtm(document = doc_id, term = lemma, value = n)


#frec. de terminos elegidos:
terminos <- c("violencia", "democracia", "libertad", "venezuela", "soberanía")
#check tema tildes 
frecuencia_terminos <- processed_text %>%
  filter(lemma %in% terminos) %>%
  count(lemma, sort = TRUE)

frecuencia_terminos

#grafico 2 
grafico <- ggplot(frecuencia_terminos, aes(x = reorder(lemma, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Frecuencia de términos relevantes",
    x = "Término",
    y = "Frecuencia"
  )

grafico

#exportar resultados 
ggsave(
  filename = here("TP2", "output", "grafico_frecuencia_terminos.png"),
  plot = grafico,
  width = 8,
  height = 6
)
