####Punto 1####

#Guia 2 Estadistica I
#Jaime Salinas y Camilo Riquelme

pacman::p_load(haven, tidyverse, dplyr, sjmisc, sjPlot, modeest, forcats, magrittr)

datos = read_dta("input/data/base_85.dta")

view(datos)
dim(datos)
names(datos)

class(datos$esc_nivel_1)
frq(datos$esc_nivel_1)
frq(datos$iden_pol_2)
frq(datos$percepcion_4)

####FILTRAR DATOS####
datos_proc = datos%>% 
  mutate(iden_pol_2 = case_when(
    iden_pol_2 >= 1 & iden_pol_2 <= 2 ~"Izquierda",
    iden_pol_2 >= 3 & iden_pol_2 <= 4 ~"Centro Izquierda",
    iden_pol_2 >= 5 & iden_pol_2 <= 6 ~"Centro",
    iden_pol_2 >= 7 & iden_pol_2 <= 8 ~"Centro Derecha",
    iden_pol_2 >= 9 & iden_pol_2 <= 10 ~"Derecha"))%>%
  mutate(percepcion_4 = case_when(
    percepcion_4 == 1 ~"progresando", 
    percepcion_4 >= 2 & percepcion_4 <=3 ~"estancado/en decadencia"))%>%
  mutate(edad = as.numeric(.$edad))%>%
  select(iden_pol = iden_pol_2, percepcion = percepcion_4, 
         sexo, edad, nivel = esc_nivel_1)%>%
  mutate_if(is.labelled, ~(forcats::as_factor(.)))

datos_proc$iden_pol = factor(datos_proc$iden_pol, levels = c(
  "Izquierda", "Centro Izquierda", "Centro", "Centro Derecha", "Derecha"))

####ASIGNAR NA####

datos_proc$nivel = na_if(datos_proc$nivel, "99.- No contesta")

####REVISION DATOS####

view(datos_proc)
dim(datos_proc)
names(datos_proc)

####TABLAS DE PROPORCION####

prop.table(table(datos_proc$iden_pol, exclude = F))*100
prop.table(table(datos_proc$iden_pol))*100
table(datos_proc$iden_pol, exclude = F)
table(datos_proc$iden_pol)

prop.table(table(datos_proc$nivel, exclude = F))*100
prop.table(table(datos_proc$nivel))*100
table(datos_proc$nivel, exclude = F)
table(datos_proc$nivel)

prop.table(table(datos_proc$percepcion, exclude = F))*100
prop.table(table(datos_proc$percepcion))*100
table(datos_proc$percepcion, exclude = F)
table(datos_proc$percepcion)

prop.table(table(datos_proc$sexo))*100
table(datos_proc$sexo)

prop.table(table(datos_proc$edad))*100
table(datos_proc$edad)

####Punto 2####

# Determinar en que nivel porcentual las percepciones  se postulan a favor del progeso 
#segun su identidad politica 

####Punto 3####

#Identificacion politica: nominal
#sexo: nominal
#edad: nominal
#nivel educacional:ordinal
#percepcion: nominal
#

####Punto 4####

#GRAFICOS DE EDAD#

ggplot(datos_proc, aes(x=edad)) +
  geom_histogram(breaks=seq(18, 100),
                 col="plum2",
                 fill="orange")+
  labs(title="Histograma Edad", x="Edad", y="Frecuencia", show.summary = TRUE)

#GRAFICOS SEXO/NIVEL EDUCACIONAL#

plot_xtab(datos_proc$sexo, datos_proc$nivel, margin = "row", 
          bar.pos = "stack",
          title = "SEXO/NIVEL",
          show.summary = TRUE, coord.flip = TRUE)

plot_grpfrq(datos_proc$sexo, datos_proc$nivel,
            type = "bar", title = "SEXO/NIVEL1", show)

plot_grpfrq(datos_proc$nivel, datos_proc$sexo,
            type = "bar", title = "NIVEL/SEXO")

plot_xtab(datos_proc$sexo, datos_proc$nivel, title = "SEXO/NIVEL2")

plot_xtab(datos_proc$nivel, datos_proc$sexo, title = "NIVEL/SEXO1")

#TABLAS SEXO/NIVEL EDUCACIONAL#

plot_frq(datos_proc, nivel,
         title = "Nivel Educacional",
         type = "bar")

sjt.xtab(datos_proc$nivel,datos_proc$sexo, encoding = "UTF-8")
sjt.xtab(datos_proc$nivel,datos_proc$sexo, show.row.prc = TRUE, encoding = "UTF-8")
sjt.xtab(datos_proc$nivel,datos_proc$sexo, show.col.prc = TRUE, encoding = "UTF-8")

#GRAFICO DISTRIBUCION NIVEL EDUCACIONAL#

plot_frq(datos_proc, nivel,
         title = "Nivel Educacional1",
         type = "bar")

#GRAFICO DISTRIBUCION SEXO#
plot_frq(datos_proc, sexo,
         title = "SEXO",
         type = "bar")

#MEDIA-MEDIANA-MODA EDAD#

mean(datos_proc$edad, na.rm = TRUE) #Media 
mean(datos_proc$edad, na.rm = T, trim = 0.025) #media truncada
median(datos_proc$edad, na.rm = TRUE) #mediana
mlv(datos_proc$edad, method = "mfv", na.rm = T) #moda

#CUANTILES EDAD#

quantile(datos_proc$edad, na.rm = T) #cuartiles
quantile(datos_proc$edad, probs = seq(0, 1, .2), na.rm = T) #quintil
quantile(datos_proc$edad, probs = seq(0, 1, .1), na.rm = T) #decil
quantile(datos_proc$edad, probs = seq(0, 1, .01), na.rm = T) #percentil

range(datos_proc$edad, na.rm = TRUE) #Rango (m?nimo y m?ximo)
max(datos_proc$edad, na.rm = TRUE) - min(datos_proc$edad, na.rm = TRUE) #Rango

IQR(datos_proc$edad, na.rm = TRUE) #Rango intercuartil 

#  EL total de mujeres encuestadas es de 905 personas representando casi el doble de la cantidad de hombres encuestados (cantidad de hombres encuestados)534 
#  En la edad, la media, la media truncada y la mediana son similares entre si ( media truncada:49,3 media: 49,5 mediana: 49) la moda por otra parte se ve representada con la edad de 56 años   )
#  La tendencia general en la variable nivel educacional es de un 29,2% representados en la variable "media cientifico/humanista/ tecnico profesional completa) 
#  Respecto al porcentaje de nivel educacional, en relacion al sexo ambos mantienen porcentajes similares respecto al nivel educacional 
#  El primer cuartil representa desde los 18 a los 34 años, el segundo cuartil representa desde los 35 a los 49, el tercer cuartil representa desde los 50 a los 64 y el ultimo cuartil representa desde los 65 a los 95 años de edad.
#  El rango etario de la encuesta es a partir de los 18 a los 95 años de edad. 

####Punto 5####

# Optamos por unir las variables "estancado/en decadencia" ya que la variable progresando 
# representa un valor excluyente debido su significado, en contraste con las variables "estancado y en decadencia" 
# que representan un valor mucho mas similar entre simismos 
#Los Na representan aquellos encuestados que optan por las variables "no sabe" o "no contesta", cuya etiqueta dice "no leer" 
#
####Punto 6####

plot_frq(datos_proc, percepcion,
         title = "Distribucion Percepcion",
         type = "bar")

# La percepcion domiante es la variable "estancado/en decadencia" con un 77,8% sobre un 22,2% de la persepcion "progresando"

####Punto 7####

# Los valores perdidos representan un 36,5% del total de la encuesta significando 
#una suma de 527 casos de la muestra recopilada en esta investigacion, equivalente a 1.443 personas encuestadas. 
# Las personas encuestadas que no contestan o no que responden "no saben" toman el valor de NA

####Punto 8####

plot_frq(datos_proc, iden_pol,
         title = "Distribucion Identificacion Politica",
         type = "bar")

table(datos_proc$iden_pol, exclude = F)
prop.table(table(datos_proc$iden_pol, exclude = F))*100


#  Izquierda: El 10,7 % del total de los encuestados se identifica con la "izquierda" representando un total de 98 casos
#  Centro Izquierda: El 13,7 % del total de los encuestados se identifica con la "centro izquierda" representando un total de 137 casos
#  Centro: El 53,2 % del total de los encuestados se identifica con el "centro" representando un total de 487 casos
#  Centro derecha: El 13 % del total de los encuestados se identifica con la "centro derecha" representando un total de 119 casos
#  Dereccha: El 8,2 % del total de los encuestados se identifica con la "Derecha" representando un total de 75 casos

####Punto 9####
#GRAFICOS DE PERCEPCION/IDENTIFICACION POLITICA#

prop.table(table(datos_proc$iden_pol))*100

prop.table(table(datos_proc$percepcion))*100

plot_xtab(datos_proc$iden_pol, datos_proc$percepcion, margin = "row", 
          bar.pos = "stack",
          title = "Identificacion Politica/Percepcion",
          show.summary = TRUE, coord.flip = TRUE)

plot_xtab(datos_proc$percepcion, datos_proc$iden_pol, margin = "row", 
          bar.pos = "stack",
          title = "Percepcion/Identificacion Politica",
          show.summary = TRUE, coord.flip = TRUE)

plot_xtab(datos_proc$percepcion, datos_proc$iden_pol, title = "Percepcion/Identificacion Politica1")

# En base a la recodificacion realizada se precisa que un 77,83% de la muestra opina que el pais esta estancado o en decadencia, mientras que un 22,17% de los encuestados opina que chile se encuentra en situacion de progreso 
# Al recodificar la variable decidimos unir aquellas con nombre "estancado" y "en decadencia" puesto que ambas representan una percepcion negativa
#frente a alguna "identidad" politica ( izquierda, centro izquierda, centro, centro derecha, derecha) diferencia de la variable "progresando". 
#                                            Variable "Estancado/en decadencia"
# Izquierda: Un 80,6% del sector encuestado que se representa con la identidad politica "izquierda" percibe un estado de "decadencia o estancamiento" en Chile
# Centro izquierda: Un 83,3% del sector encuestado que se representa con la identidad politica "Centro izquierda" percibe un estado de "decadencia o estancamiento" en Chile
# Centro: Un 77,5% del sector encuestado que se representa con la identidad politica "centro" percibe un estado de "decadencia o estancamiento" en Chile
# Centro derecha: Un 60% del sector encuestado que se representa con la identidad politica "centro" percibe un estado de "decadencia o estancamiento" en Chile
# Derecha: Un 72,7% del sector encuestado que se representa con la identidad politica "Derecha" percibe un estado de "decadencia o estancamiento" en Chile
#                                            Variable #progresando"
# Izquierda: Un 19,4% del sector encuestado que se representa con la identidad politica "izquierda" percibe un estado de "progreso" en Chile
# Centro izquierda:Un 16,2% del sector encuestado que se representa con la identidad politica "centro izquierda" percibe un estado de "progreso" en Chile
# Centro:Un 22,5% del sector encuestado que se representa con la identidad politica "centro izquierda" percibe un estado de "progreso" en Chile
# Centro derecha:Un 40% del sector encuestado que se representa con la identidad politica "centro izquierda" percibe un estado de "progreso" en Chile
# Derecha:Un 27,3% del sector encuestado que se representa con la identidad politica "centro izquierda" percibe un estado de "progreso" en Chile
# 
# La tendencia por parte del mayor sector se ve representado en la variable "estancado/en decadencia" con un total de los 550 votos, representado un 76,2% de la encuesta total con las variables ("estancado/en decadencia y progresando)
#
#Afirmamos segun los datos obtenidos que ninguna ideadtidad politica considera que el pais se encuentra progresando 
