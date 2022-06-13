#Guia 4 Estadistica ------------------------------------------------------------

#Camilo Riquelme - Jaime Salinas - Isidora Toledo

#Carga de Paquetes

pacman::p_load(haven, tidyverse, sjmisc, sjPlot, ggrepel, kableExtra, modeest)

#Carga de datos

data = read_dta("input/data/base_85.dta")

#Exploracion de variables ------------------------------------------------------

#Procesamiento de datos --------------------------------------------------------

#Revision de procesamiento -----------------------------------------------------

#Guardar datos procesados ------------------------------------------------------

saveRDS(data_proc, file = "output/data/datos_proc.rds")