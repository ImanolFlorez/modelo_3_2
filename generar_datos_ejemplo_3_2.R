#' @title Genera datos de ejemplo de textos para ACUACAR
#' 
#' @description Utilizando como base los datos con los que se entreno el modelo,
#' toma una muestra de datos que sirven como ejemplo de un nuevo conjunto de 
#' datos con el cual se van a generar predicciones. Asume que los datos se
#' encuentran aquí: 
#' metadatos: "datos/PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx"
#' No utiliza los archivos de texto (datos/as400.feather)
#' Este script es idéntico al del modelo 2_2
#'  
#' @param numero es el número de muestras a generar
#'

args <- commandArgs(trailingOnly = TRUE)

numero <- as.numeric(args[1])

library(readxl)
library(readr)
library(dplyr)
#library(openxlsx)
library(here)
options(warn=-1)


# datos <- arrow::read_feather(text_file) %>%
#   select(name,text) %>% 
#   slice_sample(n = numero) 

datos <- read_excel(here("datos","PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx"),
                    sheet = "PQRS TOTAL") %>%
  slice_sample(n = numero) 



datos %>%
  write_csv("muestra_3_2.csv")

