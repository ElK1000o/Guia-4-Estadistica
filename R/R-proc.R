#Guia 4 Estadistica ------------------------------------------------------------

#Camilo Riquelme - Jaime Salinas - Isidora Toledo

#Carga de Paquetes

pacman::p_load(haven, tidyverse, sjmisc, sjPlot, ggrepel, kableExtra, modeest)

#Carga de datos

data = read_dta("input/data/base_85.dta")

#Exploracion de variables ------------------------------------------------------

data_proc = data%>% 
  mutate(iden_pol_2 = case_when(
    iden_pol_2 >= 1 & iden_pol_2 <= 2 ~"Izquierda",
    iden_pol_2 >= 3 & iden_pol_2 <= 4 ~"Centro Izquierda",
    iden_pol_2 >= 5 & iden_pol_2 <= 6 ~"Centro",
    iden_pol_2 >= 7 & iden_pol_2 <= 8 ~"Centro Derecha",
    iden_pol_2 >= 9 & iden_pol_2 <= 10 ~"Derecha"),
    percepcion_4 = case_when(percepcion_4 == 1 ~"progresando", 
    percepcion_4 <= 2 & percepcion_4 <=3 ~"estancado/en decadencia"),
    edad = as.numeric(.$edad))%>%
  select(sexo, edad, region, nivel = esc_nivel_1, iden_pol = iden_pol_2, 
         percepcion = percepcion_4)%>%
  mutate_if(is.labelled, ~(forcats::as_factor(.)))

datos_proc$iden_pol = factor(datos_proc$iden_pol, levels = c(
  "Izquierda", "Centro Izquierda", "Centro", "Centro Derecha", "Derecha"))

#Procesamiento de datos --------------------------------------------------------



#Revision de procesamiento -----------------------------------------------------



#Guardar datos procesados ------------------------------------------------------

saveRDS(data_proc, file = "output/data/datos_proc.rds")