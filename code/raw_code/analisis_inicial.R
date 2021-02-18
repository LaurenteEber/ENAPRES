#########################################################################################
#     scrip de carga de bases de datos y análisis descriptivo
#     Eber Laurebte    
#     Fecha: 21/01/21
#
#########################################################################################

# Carga de librerias a utilizar
library(tidyverse)
library(haven)
# Cargando la base datos del módulo 100

mod100 <- read_sav("data/raw_data/CAP_100_URBANO_RURAL_2019.sav")

# Seleccionando solo las variables referentes a saneamiento

saneamiento = mod100 %>% select(PER:RESFIN, P129A:P141_7_O, REGIONNATU:FACTOR)

# Indicador 01:
# PERÚ: PORCENTAJE DE HOGARES QUE TIENE ACCESO AL SERVICIO DE AGUA POTABLE, 2010-2012

#########################################################################################
####################### Análisis descriptivo ############################################

### Paso 1. Estructura

dim(saneamiento)
colnames(saneamiento)
str(saneamiento)
glimpse(saneamiento)

### Paso 2. Missing value

# Para calcular la cantidad de NAs en cada variable 
sapply(saneamiento, function(x) sum(is.na(x)))

# Para calcular la proporción de missingness por cada variable
sapply(saneamiento, function(x) sum(is.na(x)))/nrow(saneamiento)

# visualización gráfica de los missing con el paquete naniar
library(naniar)
#library(visdat)

saneamiento %>% select(P129A:P141_7_O) %>% vis_miss(warn_large_data = FALSE)

saneamiento %>% select(P129A:P141_7_O) %>% gg_miss_var() + theme_dark()

### Paso 3. Distribución

saneamiento %>% ggplot(aes(P129A)) + geom_density()

# Ouleirs

saneamiento %>% ggplot(aes(CCDD, P129A)) + geom_boxplot()

# Tendencia central

# Moda
saneamiento %>% ggplot(aes(P129A)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5))


# librería skimr para resumir 
skimr::skim(saneamiento)



