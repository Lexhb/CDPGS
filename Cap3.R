atencion_ciudadano <- read.csv("http://bitsandbricks.github.io/data/gcba_suaci_barrios.csv")

install.packages("tidyverse")

library(tidyverse)

str(atencion_ciudadano)

summary(atencion_ciudadano)

levels(atencion_ciudadano$BARRIO)

library(tidyverse)

barrios_comunas <- read.csv("http://bitsandbricks.github.io/data/barrios_comunas.csv")

atencion_ciudadano <- left_join(atencion_ciudadano, barrios_comunas)

head(atencion_ciudadano)

write.csv(atencion_ciudadano, "atencion_ciudadano.csv", row.names = FALSE)

atencion_ciudadano <- read.csv("atencion_ciudadano.csv")

getwd()

seleccion <- select(atencion_ciudadano, PERIODO, total)

head(seleccion)

seleccion <- select(atencion_ciudadano, RUBRO:BARRIO)

head(seleccion)

seleccion <- select(atencion_ciudadano, -RUBRO)

seleccion <- select(atencion_ciudadano, -(TIPO_PRESTACION:total))

seleccion <- select(atencion_ciudadano, -RUBRO, -BARRIO)

seleccion <- filter(atencion_ciudadano, BARRIO == "RETIRO", PERIODO == 201401)

head(seleccion)

str(seleccion)

dim(seleccion)

seleccion <- filter(atencion_ciudadano, total>100)

head(seleccion)

seleccion <- filter(atencion_ciudadano, PERIODO==201508, RUBRO=="SALUD")

head(seleccion)

seleccion <- filter(atencion_ciudadano, BARRIO == "RETIRO" | BARRIO == "PALERMO")

head(seleccion)

seleccion <- filter(atencion_ciudadano, 
                    TIPO_PRESTACION == "TRAMITE" & !(RUBRO == "REGISTRO CIVIL"))

head(seleccion)

seleccion <- filter(atencion_ciudadano, 
                    !(TIPO_PRESTACION == "DENUNCIA" & RUBRO == "SEGURIDAD E HIGIENE"))

head(seleccion)
