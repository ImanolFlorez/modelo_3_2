
# Modelo 3.2:Tono emocional PQRS

## ACUACAR

Los datos con los cuales se entreno el modelo tienen la siguiente
estructura:

### Entrada

#### Metadatos

-   RadicadoCOD,
-   En\_Abox?,
-   RadicadoFecha,
-   ExpedienteCOD,
-   ReclamoID,
-   Poliza,
-   RecepcionVia,
-   MotivoPQR
-   TipoPQR,
-   MotivoTipoPQR,
-   Area,
-   DESCRIPCION\_CORTA,
-   OBSERVACION\_01,
-   OBSERVACION\_02,
-   OBSERVACION\_03,
-   PERIODO\_FACTURADO,
-   AÑO\_FACTURADO,
-   NUMERO\_REGISTRO,
-   FECHA\_REGISTRO,
-   FECHA\_VENCIMTO
-   IMPORTE,
-   USUARIO\_RECEPCION
-   FECHA\_RECEPCION
-   HORA\_RECEPCION
-   DPTO\_TRANSFERIDO,
-   FECHA\_INSPECCION
-   INSPECCION?
-   Imperceptible
-   Estado\_ult\_novedad
-   Tipo\_Solucion\_COD,
-   Fecha\_Solucion,
-   Fecha\_Notificacion,
-   Fecha\_Vencimiento,
-   Fecha\_Venc\_interno,
-   Registro\_Historico,
-   DPTO\_SOLUCION,
-   TIPO\_SOLUCION,
-   CLAVE\_SOLUCION,
-   VIA\_SOLUCION,
-   OBSER\_SOLUCION\_1,
-   OBSER\_SOLUCION\_2,
-   OBSER\_SOLUCION\_3,
-   USUARIO\_SOLUCION,
-   Archivo,
-   NUMERO,
-   RUTA COMBINADA,
-   RUTA2

De estos, los campos imprescindibles son:

-   Tipo\_Solucion\_COD
-   RUTA COMBINADA

#### Texto

-   parser
-   name
-   text
-   pages

De estos, los campos imprescindibles son:

-   text
-   name

Adicionalmente se requiren los siguientes archivos (almacenados en una
carpeta datos del directorio de trabajo):

#### Lexicon de sentimientos:

*lexicon\_español.rds*

#### Palabras vacías:

*vacias.txt*. Este archivo hay que configurarlo para cada cliente

### Salidas

El modelo produce para emular el comportamiento de los otros modelos los
siguientes archivos:

-   ficha\_mod\_\_2.csv: La ficha con las métricas del modelo. En este
    caso, solo NA

-   matriz\_confusion\_mod\_3\_2.csv: El archivo con la matriz de
    confusión. Vacío en este caso

-   archivo de resultados (.csv), con el nombre dado. Tiene los
    siguientes campos:

    -   .pred\_class: el nombre del documento
    -   .pred\_carga: el componente principal 1, escalado de 0 a 1, se
        interpreta como la carga emocional del documento. Mientras más
        pequeño menor es la carga emocional del documento
    -   .pred\_negativo: el componente principal 2, escalado de 0 a 1,
        se interpreta como la negatividad del documento. Mientras más
        pequeño mayor es la carga negativa del documento

------------------------------------------------------------------------

# PASOS

**1. Restaurar el entorno del modelo**

Este paso es necesario siempre.

    Rscript -e 'renv::restore()'

**2. Generar el binario del modelo**

Este paso es necesario solo si es la **primera vez** que el modelo de
corre en este computador. Es el paso de entrenar el modelo y obtener las
métricas relevantes. Por tanto, requiere que en el mismo directorio en
donde se encuentra el script estén los datos en una carpeta así:

-   “datos/PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx”
-   “datos/as400.feather”
-   “datos/lexicon\_español.rds”
-   “datos/vacias.txt”

<!-- -->

    Rscript modelo_3_2.R

Como resultado se obtendrán dos nuevos archivos en la carpeta datos:

-   Un archivo con el resultado de la ingestión de textos
    (todo\_textos.rds)
-   Un archivo con el resultado del análisis de componentes principales
    (acp.rds)

De estos, en este modelo solo se usa los resultados del análisis de
componentes principales

**3. Generar datos de prueba**

Este paso es necesario solo en las pruebas. Los datos de ejemplo son
sustituidos por nuevos datos en producción

    Rscript generar_datos_ejemplo_3_2.R 1

**4. Ejecutar el modelo con nuevos datos**

Este es el comando principal para correr nuevos datos con el modelo que
debe ser llamado desde el servicio

    Rscript modelo_servido_3_2.R <archivo csv metadatos nuevos datos> <archivo feather nuevos textos> <archivo csv resultados> 

El archivo de lexicon y el archivo de vacías se presumen almacenados en
la carpeta datos del directorio desde donde se llama el script

Ejemplo:

    Rscript modelo_servido_3_2.R muestra_3_2.csv datos/as400.feather resultado_mod_3_2.csv
