#Guia 4 Estadistica ------------------------------------------------------------

#Camilo Riquelme - Jaime Salinas - Isidora Toledo

#Carga de Paquetes

pacman::p_load(haven, tidyverse, sjmisc, sjPlot, ggrepel, kableExtra, modeest)

#Fijación de directorio
#setwd()

#Carga de datos

data = read_dta("input/data/base_85.dta")

#Exploracion de variables ------------------------------------------------------

frq(data$iden_pol_2)
frq(data$confianza_6_j)
frq(data$percepcion_4)
frq(data$sexo)
frq(data$esc_nivel_1)
frq(data$edad)

#Procesamiento de datos --------------------------------------------------------

data_proc = data%>% 
  mutate(iden_pol = case_when(iden_pol_2 >= 1 & iden_pol_2 <= 2 ~"Izquierda",
                              iden_pol_2 >= 3 & iden_pol_2 <= 4 ~"Centro Izquierda",
                              iden_pol_2 >= 5 & iden_pol_2 <= 6 ~"Centro",
                              iden_pol_2 >= 7 & iden_pol_2 <= 8 ~"Centro Derecha",
                              iden_pol_2 >= 9 & iden_pol_2 <= 10 ~"Derecha",
                              TRUE~NA_character_),
         confianza = case_when(confianza_6_j==1 ~"Mucha Confianza", 
                               confianza_6_j==2 ~"Bastante Confianza",
                               confianza_6_j==3 ~"Poca Confianza",
                               confianza_6_j==4 ~"Nada de Confianza",
                               TRUE~NA_character_),
         edad = case_when(edad<=29 ~"Jovenes",
                          edad>=30 & edad<=59 ~"Adultos/as",
                          edad>=60 ~"Adulto/a Mayor"),
         sexo = case_when(sexo==1 ~"Hombre", sexo==2 ~"Mujer"))%>%
  select(sexo, edad, iden_pol, confianza)

data_proc$iden_pol = factor(data_proc$iden_pol, levels = c(
  "Izquierda", "Centro Izquierda", "Centro", "Centro Derecha", "Derecha"))

#Revision de procesamiento -----------------------------------------------------

frq(data_proc$sexo)
frq(data_proc$edad)
frq(data_proc$confianza)
frq(data_proc$iden_pol)

#Guardar datos procesados ------------------------------------------------------

saveRDS(data_proc, file = "output/data/datos_proc.rds")
