#'Lee los textos correspondientes a ACUACAR.
#' 
#' @title Lee el texto correspondiente a ACUACAR
#' 
#' @description Lee los textos correspondientes a ACUACAR y produce un archivo
#' binario de r (rds) que contiene el texto preprocesado (minúsculas, solo texto,
#' sin NA, etc.) y con los componentes principales del análisis de sentimiento 
#' para ser usados en los modelos de predicción.  Los archivos se guardan
#' en la carpeta de datos. Los archivos en cuestión en esta primera version son:
#' 
#'  - as400.feather: el archivo con los textos generados por el pipeline
#'  - PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx: el archivo con los metadatos. Es
#'  necesario para poder conocer el tipo asociado al documento  
#'  - lexicon_español.rds: lo que resulta de correr el script generar_lexicon_espanol.R
#'  Es necesario haber corrido este script previamente. También se almacena en 
#'  la carpeta datos
#'  - vacias.txt:listado de palabras vacías particulares a acuacar
#'
#' Es necesario ejecutar este script antes de correr el modelo de predicción de tipo
#' pqr. Toma un tiempo considerable ejecutarlo
#'    
#' @param datos_file nombre del archivo .csv con los metadatos usados en entrenamiento
#' @param text_file nombre del archivo .feather donde se encuentran los textos 
#' @param lexicon_file nombre del archivo .rds con el lexicon. Por defecto, 
#' lexicon_español.rds
#' @param vacias_file: listado de palabras vacias. Por defecto, vacias.txt 
#' @param procesados_file nombre del archivo .rds con los datos de entrada procesados
#'
#' @return se escribe en la carpeta datos el archivo todo_textos.rds y el archivo
#' acp.rds (del análisis de componentes principales)

# datos_file = "PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx"
# text_file = "as400.feather"
# lexicon_file = "lexicon_español.rds"
# vacias_file = "vacias.txt"
# procesados_file = "todo_textos.rds"

ingerir_textos <- function(datos_file,text_file,
                           lexicon_file = "lexicon_español.rds",
                           vacias_file = "vacias.txt",
                           procesados_file = "todo_textos.rds") {  

library(dplyr, warn.conflicts = FALSE)  
library(janitor)
library(readxl)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(tidytext)
library(stopwords)
library(purrr)
library(jsonlite)
library(qdapRegex)
library(forcats)
library(tictoc)
library(here)
library(tidymodels)


#print("I am here!")

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores - 2)

otras_vacias <- read_lines(here("datos",vacias_file))
vacias <- c(stopwords("es"), otras_vacias)


print("Lee textos")
textos <- arrow::read_feather(here("datos",text_file)) %>%
 select(name,text)


# nrow(textos) = 67823

print("Lee metadatos")
crudos <- read_excel(here("datos",datos_file),
                     sheet = "PQRS TOTAL",
                     col_types = c("skip", "skip", "date",
                                   "skip", "skip", "skip", "text", "text",
                                   "skip", "text", "numeric", "text",
                                   "skip", "skip", "skip", "numeric",
                                   "skip", "skip", "skip", "skip", "skip",
                                   "skip", "skip", "skip", "text", "skip",
                                   "text", "skip", "skip", "text", "skip",
                                   "skip", "skip", "skip", "skip", "skip",
                                   "skip", "skip", "skip", "skip", "skip",
                                   "skip", "skip", "skip", "skip", "text",
                                   "skip")) %>%
  clean_names()


# nrow(crudos) = 69445

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

# nrow(datos) = 69445

# borrar <- c("crudos","n_dir","division","nombres_ultimo","nombres_vector")
# rm(list = borrar)
# gc()

print("Une todo")

todo <- left_join(datos,textos, by = "name")

# nrow(todo) = 69445

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

# nrow(todo_textos) = 64845, la diferencia es texto en blanco

print("Descompone en palabras")

palabras <- todo_textos %>%
  select(name,text) %>%
  unnest_tokens(word,text) %>%
  filter(!word %in% vacias)

# nrow(palabras) = 21.725.658 palabras en todos los documentos

print("Calcula frecuencia de cada palabra por documento")
palabras_todo <- palabras %>%
  group_by(name,word) %>%
  tally() %>%
  ungroup()

# nrow(palabras_todo) = 14.249.640 palabras únicas
# documentos_tipo <- todo_textos %>%
#   select(name,tipo_solucion)

print("Calcula el total de palabras por documento")

palabras_documento <-  palabras %>%
  group_by(name) %>%
  tally(name = "total")

# nrow(palabras_documento) = 63300. Por que esta reducción?

palabras_todo <- left_join(palabras_todo,palabras_documento)

# nrow(palabras_todo) = 14.249.640

print("Une sentimientos")

lexicon_sentimientos <- readRDS(here("datos",lexicon_file))

#lexicon_sentimientos <- readRDS(lexicon_file)

# negativo_lexicon <- lexicon_sentimientos %>%
#   select(english_en_1,word,anger,disgust,negative, positive)

# Hay muchas palabras repetidas. Ver por ejemplo
# negativo_lexicon %>% group_by(word) %>% tally() %>% arrange(desc(n))
# Por ejemplo:
# negativo_lexicon %>% filter(word == "mayor")


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

# nrow(negativo) = 16.253.970
# La razón por la que aumentan los registros es las diferentes valencias del
# AS (las variables repetidas). Por ejemplo:
# negativo %>% filter(word == "base") %>% group_by(name) %>%  tally()

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

# nrow(con_ponderacion) = 63300
print("Une texto y sentimientos")

todo_textos <- todo_textos %>%
  left_join(con_ponderacion)

#nrow(todo_textos) = 64845

# Calculando ACP

print("Calculando ACP sentimientos")

datos_acp <- todo_textos %>% 
  select(name,motivo_pqr,ends_with("_p")) %>% 
  select(-c(balance_p)) 

acp_receta <- recipe(~.,data = datos_acp) %>% 
  update_role(name,motivo_pqr, 
              new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors())

acp_preparacion <- prep(acp_receta)

acp_pasos <- acp_preparacion$steps


acp_tidy <- tidy(acp_preparacion,2)
acp_baked <- bake(acp_preparacion,new_data = NULL)

textos_acp <- todo_textos %>% 
  left_join(acp_baked) %>% 
  select(-c(ends_with("_p"), "PC3","PC4","PC5"))


saveRDS(acp_preparacion,file = here("datos","acp.rds"))
saveRDS(textos_acp, file = here("datos",procesados_file))

}