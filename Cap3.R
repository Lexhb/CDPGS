atencion_ciudadano <- read.csv("http://bitsandbricks.github.io/data/gcba_suaci_barrios.csv")

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
