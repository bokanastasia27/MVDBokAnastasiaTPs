### este script hace web scraping de la página de la OEA
## y convierte los datos a formato tabular

message("abriendo librerias")
library(tidyverse)
library(rvest)
library(here)
library(xml2)
message("librerias instaladas correctamente")

message("Leyendo página de la OEA")
url <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=4&nAnio=2026"
pagina <- read_html(url)
message("pagina leida")

links <- pagina %>%
  html_elements(".itemmenulink") %>%
  html_attr("href")
head(links)
message("links extraidos de la pagina")

base_url <- "https://www.oas.org/es/centro_noticias/"
message("base url definida")

extraer_links_mes <- function(url_mes) {
  pagina <- read_html(url_mes)
  links <- pagina %>%
    html_elements(".itemmenulink") %>%
    html_attr("href")
  base_url <- "https://www.oas.org/es/centro_noticias/"
  links_completos <- paste0(base_url, links)
  links_filtrados <- links_completos %>%
    str_subset("comunicado_prensa")
  return(links_filtrados)
}
message("funcion extraer_links_mes creada")

#para scrappear solo los meses 
meses <- 1:4
urls_meses <- paste0(
  "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
  meses,
  "&nAnio=2026"
)
message("urls de meses generadas")

links_todos <- map(urls_meses, extraer_links_mes) %>%
  unlist()
message('links extraidos')
message("links de todos los meses extraidos")


#funcion 
extraer_noticia <- function(url) {
  pagina <- read_html(url)
  titulo <- pagina %>%
    html_elements("h4") %>%
    html_text() %>%
    .[nchar(.) > 10] %>%
    .[1]
  cuerpo <- pagina %>%
    html_elements("p") %>%
    html_text() %>%
    paste(collapse = " ")
  id_noticia <- url
  tibble(
    id = id_noticia,
    titulo = titulo,
    cuerpo = cuerpo
  )
}
message("funcion extraer_noticia creada")

tabla_noticias <- map_dfr(links_todos, extraer_noticia)
message("tabla de noticias creada")
saveRDS(tabla_noticias, "TP2/data/tabla_oea.rds")
message("tabla guardada como rds")


#html
#primero busco fechas 
pagina_noticia %>%
  html_elements(".headlinelink") %>%
  html_text()


guardar_html <- function(url) {
  pagina <- read_html(url)
  nombre <- url %>%
    str_extract("sCodigo=.*") %>%
    str_replace("/", "_")
  write_html(pagina, paste0("TP2/data/", nombre, ".html"))
}

walk(links_todos, guardar_html)
  #FALTA TERMINAR ESTA PARTE Y SUMAR MESSAGES 

