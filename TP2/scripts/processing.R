##extraer, ordenar y limpiar el texto resultante del web scraping
message('processing')

install.packages('udpipe')
install.packages('stopwords')

message("instalando librerias")
library(here)
library(tidyverse)
library(udpipe)  #Paquete de NLP
library(stopwords)  #Paquete de NLP
library(stringi)
message("librerias instaladas correctamente")


# Crear carpeta de salida si no existe
output_dir <- here("TP2", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Se creó la carpeta: ", output_dir)
} else {
  message("La carpeta ya existe: ", output_dir)
}

##Leer datos del scraping
lectura_datos <- here("TP2", "data", "tabla_oea.rds")
message("Leyendo archivo: ", lectura_datos)
tabla_noticias <- readRDS(lectura_datos)
message('datos del scraping leidos')

##limpieza inicial: saca caracteres especiales, signos de puntuación y espacios
tabla_noticias_limpia <- tabla_noticias %>%
  mutate(
    cuerpo_limpio = cuerpo %>%
      stringi::stri_trans_general("Latin-ASCII") %>%  #saca tildes 
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", " ") %>%   # saca puntuación
      str_replace_all("[[:digit:]]", " ") %>%   # saca números
      str_replace_all("[^[:alpha:]\\s]", " ") %>% # saca caracteres especiales
      str_squish()                              # ordena espacios
  )
message('limpieza inicial concluida')


#segunda limpieza, lematizacion en español 
# Descargar modelo (solo la primera vez)
modelo <- udpipe_download_model(language = "spanish")
# Cargar modelo
modelo_path <- here("spanish-gsd-ud-2.5-191206.udpipe")
modelo_ud <- udpipe_load_model(file = modelo_path)
anotado_df <- udpipe(
  x = data.frame(
    doc_id = tabla_noticias_limpia$id,
    text = tabla_noticias_limpia$cuerpo_limpio
  ),
  object = modelo_path
)
message('lematizacion en esp. concluida')


#3, me quedo solo con adjetivos verbos y sustantivos, y saco stopwords 
# Stopwords en español
stop_esp <- stopwords("es")
message('stopwords removidos')
anotado_filtrado <- anotado_df %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>%
  mutate(lemma = str_to_lower(lemma)) %>%
  filter(!lemma %in% stop_esp) %>%
  filter(!is.na(lemma), lemma != "")
message('tabla solo contiene verbos, adjetivos y sustantivos')


#guardado, solo doc id y filas de lemas
processed_text <- anotado_filtrado %>%
  select(doc_id, lemma)

output_texto_procesado <- here("TP2", "output", "processed_text.rds")
saveRDS(processed_text, output_texto_procesado)

message("Archivo guardado en: ", output_texto_procesado)


