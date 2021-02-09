## ---- warning=FALSE,message=FALSE---------------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(scales)



## -----------------------------------------------------------------------------------------------
list.files("../Fuentes/")


## -----------------------------------------------------------------------------------------------
Individual_t117 <-
  read.table("../Fuentes/usu_individual_t117.txt",
  sep = ";",
  dec = ",",
  header = TRUE,
  fill = TRUE )
  
  
Aglom <- read.xlsx("../Fuentes/Aglomerados EPH.xlsx")


## -----------------------------------------------------------------------------------------------
Datos  <- Individual_t117[c("AGLOMERADO","MAS_500","CH04","CH06","P47T","PONDERA","PONDII")]



## -----------------------------------------------------------------------------------------------
summary(Datos) 


## -----------------------------------------------------------------------------------------------
unique(Datos$AGLOMERADO)


## -----------------------------------------------------------------------------------------------
sample_n(tbl = Datos,size = 9)


## -----------------------------------------------------------------------------------------------
table(Datos$MAS_500,Datos$CH04) 


## -----------------------------------------------------------------------------------------------
pepito <- Datos %>% 
  filter(CH04==1 , CH06>=50)



## -----------------------------------------------------------------------------------------------
Datos %>% 
    filter(CH04==1| CH06>=50)



## -----------------------------------------------------------------------------------------------
#Ejercicio 1


## -----------------------------------------------------------------------------------------------
Datos <- Datos %>% 
  rename(EDAD = CH06)
Datos



## -----------------------------------------------------------------------------------------------
Datos <- Datos %>% 
  mutate(Edad_cuadrado=EDAD^2,
         Edad_cubo =EDAD^3) 

Datos


## -----------------------------------------------------------------------------------------------
Datos <- Datos %>% 
  mutate(Grupos_Etarios = case_when(EDAD  < 18   ~ "Menores",
                                 EDAD  %in%  18:65   ~ "Adultos",
                                 EDAD  > 65 ~ "Adultos Mayores"))
Datos


## -----------------------------------------------------------------------------------------------
#Ejercicio 2


## -----------------------------------------------------------------------------------------------
#Conservo solo 2 variables
Datos %>% 
  select(CH04,PONDERA)

#Conservo todas las variables desde la 3era
Datos %>% 
  select(3:ncol(.))



## -----------------------------------------------------------------------------------------------
Datos <- Datos %>% 
  arrange(CH04,EDAD)
Datos


## -----------------------------------------------------------------------------------------------
#Recuerden que los menores de un año están clasificados con el valor -1
Datos <- Datos %>% 
  mutate(edad.corregida=ifelse(EDAD == -1,yes = 0,no = EDAD))

#R BASE#
mean(Datos$edad.corregida,na.rm = T) #sin ponderar
weighted.mean(Datos$edad.corregida,Datos$PONDERA) #ponderado

#Tidyverse

Datos %>%      
 summarise(Edad_prom = mean(edad.corregida),  #sin ponderar
           Edad_prom_pond = weighted.mean(x = edad.corregida,w = PONDERA)) #ponderado



## -----------------------------------------------------------------------------------------------
Datos %>% 
  group_by(CH04) %>%
  summarise(Edad_Prom = weighted.mean(EDAD,PONDERA))


## -----------------------------------------------------------------------------------------------
Encadenado <- Datos %>% 
  filter(Grupos_Etarios == "Adultos") %>% 
  mutate(Sexo = case_when(CH04 == 1 ~ "Varon",
                          CH04 == 2 ~ "Mujer")) %>% 
  select(-Edad_cuadrado)
  
Encadenado


## -----------------------------------------------------------------------------------------------
#Ejercicio 3


## -----------------------------------------------------------------------------------------------
Aglom


## -----------------------------------------------------------------------------------------------
Datos_join <- Datos %>% 
  left_join(.,Aglom, by = "AGLOMERADO")
Datos_join

Poblacion_Aglomerados <- Datos_join %>% 
  group_by(Nom_Aglo) %>% 
  summarise(Menores = sum(PONDERA[Grupos_Etarios=="Menores"]),
            Adultos = sum(PONDERA[Grupos_Etarios=="Adultos"]),
            Adultos_Mayores = sum(PONDERA[Grupos_Etarios=="Adultos Mayores"]))

Poblacion_Aglomerados



## -----------------------------------------------------------------------------------------------
pob.aglo.long <- Poblacion_Aglomerados %>% 
  pivot_longer(cols = 2:4,names_to = "Grupo_Etario",values_to = "Poblacion")

pob.aglo.long


## -----------------------------------------------------------------------------------------------
pob.aglo.long %>% 
  pivot_wider(names_from = "Grupo_Etario",values_from = "Poblacion")
  


## -----------------------------------------------------------------------------------------------
Poblacion_ocupados <- Individual_t117 %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]))

Poblacion_ocupados


## -----------------------------------------------------------------------------------------------
Empleo <- Individual_t117 %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Tasa_Empleo    = Ocupados/Poblacion)

Empleo


## -----------------------------------------------------------------------------------------------
Empleo %>% 
  mutate(Tasa_Empleo_Porc = scales::percent(Tasa_Empleo))


## ----eval=FALSE, warining = FALSE---------------------------------------------------------------
## Lista_a_exportar <- list("Tasa de Empleo"  = Empleo,
##                          "Poblacion Aglos" = Poblacion_Aglomerados)
## 
## write.xlsx(x = Lista_a_exportar,file = "../Resultados/ejercicioclase3.xlsx")
## 

