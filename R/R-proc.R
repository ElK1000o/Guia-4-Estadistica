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
         conf_dummy = case_when(confianza_6_j >=1 & confianza_6_j<=2 ~"Confianza",
                                confianza_6_j >=3 & confianza_6_j<=4 ~"Desconfianza"),
         edad_tr = case_when(edad<=29 ~"Jovenes",
                          edad>=30 & edad<=59 ~"Adultos/as",
                          edad>=60 ~"Adulto/a Mayor",
                          TRUE~NA_character_),
         sexo = case_when(sexo==1 ~"Hombre", sexo==2 ~"Mujer"))%>%
  select(sexo, edad, edad_tr, iden_pol, confianza, conf_dummy)

data_proc$iden_pol = factor(data_proc$iden_pol, levels = c(
  "Izquierda", "Centro Izquierda", "Centro", "Centro Derecha", "Derecha"))
data_proc$confianza = factor(data_proc$confianza, levels = c(
  "Mucha Confianza", "Bastante Confianza", "Poca Confianza", "Nada de Confianza"))

#Revision de procesamiento -----------------------------------------------------

frq(data_proc$sexo)
frq(data_proc$edad)
frq(data_proc$confianza)
frq(data_proc$iden_pol)
frq(data_proc$conf_dummy)

#MTC edad

summary(data_proc$edad)
mean(data_proc$edad, na.rm = TRUE)
mean(data_proc$edad, na.rm = T, trim = 0.025)
median(data_proc$edad)
mlv(data_proc$edad, method = "mfv", na.rm = T)

#DE edad

sd(data_proc$edad, na.rm = T)

#Graficos ----------------------------------------------------------------------

# H. univariada
plot_frq(data_proc$conf_dummy,
         title = "Distribucion Niveles de Confianza", geom.colors = "purple")
        
# H. bivariada
plot_xtab(data_proc$confianza, data_proc$iden_pol, 
          title = "Grado de Confianza de cada Identificacion Politica", 
          show.n = F, coord.flip = T)

plot_xtab(data_proc$conf_dummy, data_proc$iden_pol, 
          title = "Confianza/Desconfianza de cada Identificacion Politica", 
          geom.colors = "pink", 
          show.n = F, 
          coord.flip = T)
#-------------------------------------------------------------------------------
plot_xtab(data_proc$conf_dummy, data_proc$sexo,
          title = "Grado de Confianza según sexo",
          geom.colors = "purple",
          show.n = F, 
          coord.flip = T,
          show.total = F)

plot_xtab(data_proc$edad_tr, data_proc$confianza,
          title = "Grado de Confianza según Edad",
          show.n = F, 
          coord.flip = T,
          show.total = F)

plot_grpfrq(data_proc$iden_pol, data_proc$confianza,
            title = "Distribucion de niveles de Confianza según Identificacion Politica",
            show.n = F, 
            coord.flip = T)

#Tablas
sjt.xtab(data_proc$confianza, data_proc$iden_pol, 
         title = "Tabla de Contingencia: Grado confianza según identificacion politica",
         show.col.prc = T, 
         show.row.prc = T)

data_proc %>% 
  select(edad) %>%
  descr(show = c("label", "n", "mean", "sd", "md", "range")) %>% 
  kable(format = "latex",
        caption = "MTC Satisfacion con la vida/Clima Social Escolar",
        col.names = c("Variable", "Etiqueta", "n", "Media", "D. estandar", "Mediana", "Rango"),
        position = "center") %>%
  kable_classic(full_width = F,
                html_font = "cambria") %>% 
  footnote("Elaboración propia en base a CEP 85",
           general_title = "Fuente: ")

#Guardar datos procesados ------------------------------------------------------

saveRDS(data_proc, file = "output/data/datos_proc.rds")
