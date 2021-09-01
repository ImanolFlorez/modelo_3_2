#' @title Genera el modelo de tono emocional de  PQRS para Acuacar. Para que la
#' interfaz sea similar se genera también la ficha con las medidas de rendimiento 
#' del modelo y la matriz de confusión 
#' 
#' @description ESte modelo es el codificado como 3.2. Para su ejecución requiere
#' las mismas entradas que el modelo 2.2 en la carpeta datos: 
#' - El archivo de metadatos ("PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx")
#' - El archivo con los textos ("as400.feather")
#' - El archivo con las palabras vacías ("vacias.txt")
#' - El archivo con el lexicon de sentimientos en español("lexicon_español.rds")
#'
#'  Este modelo es un paso del preprocesamiento y no requiere entrenamiento. Si
#'  el modelo 2.2 se ejecuto antes, debe existir el archivo todo_textos.rds
#'  donde se encuentra la información necesaria. Si no, se hace el proceso de 
#'  ingestion de datos y se obtiene. 
#'  
#' 
#' 

## ----setup, include=FALSE--------------------------------------------------------------

library(dplyr)
library(readr)
# library(tidyr)
# library(tidymodels)
# library(tidytext)
# library(stopwords)
# library(textrecipes)
# library(themis)
# library(tictoc)
library(here)
# library(ranger)
#library(doParallel)
#library(forcats)
library(fs)

# all_cores <- parallel::detectCores(logical = FALSE)
# registerDoParallel(cores = all_cores - 2)

# otras_vacias <- read_lines(here("datos","vacias.txt"))
# vacias <- c(stopwords("es"), otras_vacias)


semilla <- 9835
set.seed(semilla)

source("R/ingestion.R")

## --------------------------------------------------------------------------------------

if (!file_exists(here("datos","todo_textos.rds"))) {
  print("Ingestión de textos..")
  ingerir_textos("PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx","as400.feather",
                 "lexicon_español.rds","vacias.txt","todo_textos.rds")
}

todo_textos <- readRDS(here("datos","todo_textos.rds")) %>% 
  mutate(tipo_solucion = as.character(tipo_solucion))


## --------------------------------------------------------------------------------------

# Simule ficha del modelo
final_res_metrics <- tibble(.metric = NA,
                            .estimator = NA,
                            .estimate = NA)


# Simule matriz de confusión
matriz_confusion <-matrix(0, nrow = 2, ncol = 2) 

# Escriba indicadores 
write.table(matriz_confusion,"matriz_confusion_mod_3_2.csv", sep = ",")

write_csv(final_res_metrics,"ficha_mod_3_2.csv")



## ---- Guarde el "modelo"---------------------------------------------------------
# No es necesario
#saveRDS(todo_textos, file = "modelo_3_2.rds")

