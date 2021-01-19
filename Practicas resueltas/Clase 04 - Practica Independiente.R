## ----echo=TRUE------------------------------------------------------------------------
library(readxl)
library(tidyverse)
Regiones <- read_excel("../Fuentes/Regiones.xlsx")
base <- read.table("../Fuentes/usu_individual_t117.txt",header = T,sep = ";",dec = ",")


## ----echo=TRUE------------------------------------------------------------------------
# Crear un dataframe con las siguientes estimaciones:   
#   
# - Total de perceptores de ingresos 
# - Perceptores de ingresos con ingreso individual total menor a $8.000
# - Perceptores de ingresos con ingreso individual total entre  $8.000 y $15.000
# - Perceptores de ingresos con ingreso individual total mayor a $15.000

resolucion.1<- base %>% 
  filter(P47T >0) %>% 
  summarise(perceptores          =sum(PONDII),
            percep.menos.8000    =sum(PONDII[P47T<8000]),
            percep.8000.a.15.000 =sum(PONDII[P47T %in% 8000:15000]),
            percep.mas.8000      =sum(PONDII[P47T>15000]))
## ----echo=TRUE------------------------------------------------------------------------
#Estimar cuantas personas habitan en hogares con ingresos totales menores a $15.000
base %>% 
  summarise(Casos_Ponderados =sum(PONDIH[ITF<15000]))


## ----echo=TRUE------------------------------------------------------------------------
base_con_identificador <- base %>% 
  group_by(CODUSU,NRO_HOGAR) %>% 
  mutate(Personas_en_hogar = n())


## ----echo=TRUE------------------------------------------------------------------------
# A partir del punto anterior:
#  - ¿Cuantas personas habitan en hogares de 7 o más integrantes?
#  - ¿Cual es el ingreso familiar per capita en estos hogares?
  
  
base_con_identificador %>% 
  ungroup() %>% 
  filter(Personas_en_hogar>=7) %>% 
  mutate(ITF.pc = ITF/Personas_en_hogar) %>% 
  summarise(PERSONAS = sum(PONDERA),
            ITF.pc.promedio = weighted.mean(ITF.pc,PONDIH))





