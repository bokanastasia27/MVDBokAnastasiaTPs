### este script hace web scraping de la página de la OEA
## y convierte los datos a formato tabular

message("scraping")
install.packages("rvest", type = "binary", dependencies = TRUE)

library(tidyverse)
library(rvest)
library(here)
library(xml2)

message("Leyendo página de la OEA")

url <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=4&nAnio=2026"
pagina <- read_html(url)

links <- pagina %>%
  html_elements(".itemmenulink") %>%
  html_attr("href")
head(links)

base_url <- "https://www.oas.org/es/centro_noticias/"

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

extraer_links_mes(urls_meses[1])

links_todos <- map(urls_meses, extraer_links_mes) %>%
  unlist()
length(links_todos)
head(links_todos)


#para scrappear solo los meses 
meses <- 1:4
urls_meses <- paste0(
  "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
  meses,
  "&nAnio=2026"
)
urls_meses


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


tabla_noticias <- map_dfr(links_todos, extraer_noticia)
glimpse(tabla_noticias)
saveRDS(tabla_noticias, "TP2/data/tabla_oea.rds")

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

#testeo
extraer_noticia(links_filtrados[1])

tabla_noticias <- map_dfr(links_filtrados, extraer_noticia)
head(tabla_noticias)



##primer codigo, solo probar sacar links 
links_completos <- paste0(base_url, links)
head(links_completos)
links_filtrados <- links_completos %>%
  str_subset("comunicado_prensa")
head(links_filtrados)


###PRUEBA 
link_ejemplo <- links_filtrados[1]
pagina_noticia <- read_html(link_ejemplo)
browseURL(link_ejemplo)
titulo <- pagina_noticia %>%
  html_elements("h4") %>%
  html_text() %>%
  .[nchar(.) > 10]
titulo

cuerpo <- pagina_noticia %>%
  html_elements("b") %>%
  html_text()
cuerpo

titulo <- titulo[1]
cuerpo <- cuerpo %>%
  paste(collapse = " ")

titulo
cuerpo
