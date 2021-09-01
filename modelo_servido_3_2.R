args <- commandArgs(trailingOnly = TRUE)
#' Genera los indicadores de tono emocional para AQUACAR
#' 
#' @title Modelo de tono emocional para PQRS de ACUACAR
#' 
#' @description Lee el documento  y produce un archivo donde se reportan los
#' indicadores de tono emocional del documento en el mismo formato que los
#' resultados de un modelo (csv)
#'  
#' binario de r (rds) que contiene el texto preprocesado (minúsculas, solo texto,
#' sin NA, etc.) y con los componentes principales del análisis de sentimiento 
#' para ser usados en los modelos de predicción.  Los archivos se guardan
#' en la carpeta de datos. Los archivos en cuestion en esta primera version son:
#' 
#'
#' @param datos_file nombre del archivo .csv con los datos a predecir (metadatos)
#' @param text_file nombre del archivo .feather donde se encuentran los textos
#' @param resultados_file nombre del archivo .csv con los indicadores de tono
#' @param lexicon_file nombre del archivo .rds con el lexicon. Por defecto, 
#' lexicon_español.rds
#' @param vacias_file: listado de palabras vacias. Por defecto, vacias.txt 

#'
#' @return se escribe en la carpeta datos el archivo todo_textos.rds


datos_file = "modelo_3_2/muestra_3_2.csv" 
# text_file <- "datos/as400.feather" 
# resultados_file <- "resultado_mod_3_2.csv"
# lexicon_file = "lexicon_español.rds"
# vacias_file = "vacias.txt"

tono_textos <- function(datos_file,text_file,resultados_file,
                           lexicon_file = "lexicon_español.rds",
                           vacias_file = "vacias.txt"
                           ) {  

library(dplyr, warn.conflicts = FALSE)  
library(janitor)
#library(readxl)
library(readr)
library(tidyr)
#library(lubridate)
library(stringr)
library(tidytext)
library(stopwords)
library(purrr)
library(jsonlite)
library(qdapRegex)
library(forcats)
#library(tictoc)
library(here)
#library(tidymodels)

   

# library(doParallel)
# all_cores <- parallel::detectCores(logical = FALSE)
# registerDoParallel(cores = all_cores - 2)

#print("Aqui")  
#nombre_vacias <- paste0("datos/",vacias_file)
#print(nombre_vacias)
#otras_vacias <- read_lines(nombre_vacias)
otras_vacias <- read_lines(here("datos",vacias_file))
  
#otras_vacias <- read_lines(vacias_file)
vacias <- c(stopwords("es"), otras_vacias)


print("Lee textos")


textos <- arrow::read_feather(text_file) %>%
  select(name,text)

# nrow(textos) = 67823

print("Lee metadatos")

crudos <- read_csv(datos_file) %>% 
  clean_names() %>% 
  select(radicado_fecha, recepcion_via, motivo_pqr,motivo_tipo_pqr,area,
         descripcion_corta,tipo_solucion_cod,inspeccion,dpto_transferido,
         ruta_combinada)


# Extrae el nombre de ruta_combinada:

# Cuenta cuantos "\\" existen
n_dir <- str_count(crudos$ruta_combinada,coll("\\")) + 1

# Divide el texto
division <- map2(crudos$ruta_combinada,n_dir,~str_split_fixed(.x,coll("\\"),.y))

# El último elemento de la lista es el que tiene el nombre:
nombres_ultimo <- map_chr(division,last)

# Quita la extensión y se queda con el nombre
nombres_vector  <- str_split_fixed(nombres_ultimo,coll("."),n = 2)[,1]

datos <- crudos %>%
  mutate(
    recepcion_via = factor(recepcion_via),
    motivo_pqr = factor(motivo_pqr),
    inspeccion = ifelse(inspeccion == "SI",TRUE,FALSE),
    name = nombres_vector
  ) %>%
  rename(tipo_solucion = tipo_solucion_cod)


print("Une todo")
todo <- left_join(datos,textos, by = "name")


todo_textos <- todo %>%
  select(radicado_fecha,
         recepcion_via,
         motivo_pqr,
         motivo_tipo_pqr,
         dpto_transferido,
         tipo_solucion,
         area,
         name,
         inspeccion,
         text) %>%
  mutate(
    #convierte todo a minúscula
    text = str_to_lower(text),
    # reemplaza cualquier caracter que no sea alfabético por " "
    text = str_replace_all(text, "[^[:alpha:]]", " "),
    # Quita espacios en blanco múltiples
    text = str_replace_all(text, "\\s+", " "),
    # Convierte palabras con acento a sin acento
    text = iconv(text,from = "UTF-8",to = "ASCII//TRANSLIT"),
    # remueve cadenas de longitud 1
    text = rm_nchar_words(text, "1"),
    text = ifelse(text == "",NA,text)) %>%
  drop_na(text)


print("Descompone en palabras")

palabras <- todo_textos %>%
  select(name,text) %>%
  unnest_tokens(word,text) %>%
  filter(!word %in% vacias)



print("Calcula frecuencia de cada palabra por documento")
palabras_todo <- palabras %>%
  group_by(name,word) %>%
  tally() %>%
  ungroup()


print("Calcula el total de palabras por documento")

palabras_documento <-  palabras %>%
  group_by(name) %>%
  tally(name = "total")


palabras_todo <- left_join(palabras_todo,palabras_documento)



print("Une sentimientos")

# nombre_lexicon <- paste0("datos/",lexicon_file)
# lexicon_sentimientos <- readRDS(nombre_lexicon)

#lexicon_sentimientos <- readRDS(lexicon_file)

lexicon_sentimientos <- readRDS(here("datos",lexicon_file))

negativo <- palabras_todo %>%
  left_join(lexicon_sentimientos) %>%
  replace_na(list(anger = 0,
                  disgust = 0,
                  negative = 0,
                  positive = 0,
                  anticipation = 0,
                  fear = 0,
                  joy = 0,
                  sadness = 0,
                  surprise = 0,
                  trust = 0)
  )


con_ponderacion <- negativo %>%
  group_by(name) %>%
  summarize(rabia = sum(anger),
            disgusto = sum(disgust),
            negativo = sum(negative),
            positivo = sum(positive),
            anticipacion = sum(anticipation),
            miedo = sum(fear),
            tristeza = sum(sadness),
            sorpresa = sum(surprise),
            confianza = sum(trust)) %>%
  left_join(palabras_documento) %>%
  mutate(rabia_p = rabia/total,
         disgusto_p = disgusto/total,
         negativo_p = negativo/total,
         positivo_p = positivo/total,
         anticipacion_p = anticipacion/total,
         miedo_p = miedo/total,
         tristeza_p = tristeza/total,
         sorpresa_p = sorpresa/total,
         confianza_p = confianza/total,
         balance_p = (positivo - negativo)/total
  ) %>% 
  select(name,ends_with("_p"))


print("Une texto y sentimientos")

todo_textos <- todo_textos %>%
  left_join(con_ponderacion)


# Calculando ACP

print("Calculando ACP sentimientos")

#nombre_acp <- paste0("datos/","acp.rds")
acp <- readRDS(here("datos","acp.rds")) 

acp_preparacion <- acp$steps 

min_PC1 <- acp$template %>% 
  slice_min(PC1) %>% 
  slice_head() %>% 
  select(PC1) %>% 
  pull()

max_PC1 <- acp$template %>% 
  slice_max(PC1) %>% 
  slice_head() %>% 
  select(PC1) %>% 
  pull()

min_PC2 <- acp$template %>% 
  slice_min(PC2) %>% 
  slice_head() %>% 
  select(PC2) %>% 
  pull()

max_PC2 <- acp$template %>% 
  slice_max(PC2) %>% 
  slice_head() %>% 
  select(PC2) %>% 
  pull()

medias <- acp_preparacion[[1]]$means
desviaciones <- acp_preparacion[[1]]$sds



# Extraer la matriz de ponderaciones

todo_sentimientos <- todo_textos %>% 
  select(name,motivo_pqr,ends_with("_p")) %>% 
  select(-c(balance_p)) 

todo_escalado <- (todo_sentimientos[,3:11] - medias)/desviaciones

# Y finalmente se calculan los scores del PCA usando:
rotacion <- t(acp_preparacion[[2]]$res[[2]])

todo_sentimientos[,3:11] <- t(rotacion %*% as.matrix(t(todo_escalado)))
names(todo_sentimientos)[3:11] <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9") 


textos_acp <- todo_textos %>% 
#  left_join(acp_baked) %>% 
  left_join(todo_sentimientos) %>% 
  select(-c(ends_with("_p"), "PC3","PC4","PC5"))

indicador_tono <- textos_acp %>%
  select(name,PC1,PC2) %>% 
  mutate(PC1_s = scale(PC1,center=min_PC1,scale= (max_PC1 - min_PC1))[,1], 
         PC2_s = scale(PC2,center=min_PC2,scale=(max_PC2 - min_PC2))[,1]) %>% 
  select(name,PC1_s,PC2_s) %>% 
  rename(.pred_class = name,
         .pred_carga = PC1_s,
         .pred_negativo = PC2_s)


write_csv(indicador_tono,resultados_file)
}

### LLamar a la funcion con el nombre de los parametros dados ------------------

# text_file = "datos/as400.feather"
# lexicon_file = "lexicon_español.rds"
# vacias_file = 
# resultados_file = 

#entrada_meta <- "/home/magdiel/github/acuacar/modelo_3_2/muestra_3_2.csv"
entrada_meta <- args[1]

#entrada_text <- "/home/magdiel/github/acuacar/datos/as400.feather"
entrada_text <- args[2]

salida <- args[3]
#salida <- "resultados_3_2.csv"

lexicon_es <- "lexicon_español.rds"
#lexicon_es <- 

vacias_cli <- "vacias.txt"
#vacias_cli <- 

tono_textos(entrada_meta,entrada_text,salida,lexicon_es,vacias_cli)
