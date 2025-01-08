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

ordenado <- arrange(atencion_ciudadano, total)

head(ordenado)

ordenado <- arrange(atencion_ciudadano, total, BARRIO)

ordenado <- arrange(atencion_ciudadano, desc(total))

## MUTATE

circulos <- data.frame(nombre = c("Círculo 1", "Círculo 2", "Círculo 3"),
                       tamaño = c("Pequeño", "Mediano", "Grande"),
                       radio = c(1, 3, 5))
circulos

mutate(circulos, area = 3.1416 * radio^2) 

atencion_ciudadano <- mutate(atencion_ciudadano,
                             AÑO = substr(PERIODO, 1, 4),
                             MES = substr(PERIODO, 5, 6))

head(atencion_ciudadano)

summarise(atencion_ciudadano, promedio = mean(total))

agrupado <- group_by(atencion_ciudadano, AÑO)

head(agrupado)

summarise(agrupado, promedio_totales = mean(total))

agrupado <- group_by(atencion_ciudadano, AÑO, MES)

sumario <- summarise(agrupado, promedio = mean(total))

head(sumario)

agrupado <- group_by(atencion_ciudadano, AÑO, MES, BARRIO)

sumario <- summarise(agrupado, promedio = mean(total))

head(sumario)

## PIPE

solo2014 <- filter(atencion_ciudadano, AÑO == 2014)

head(solo2014)

solo2014_agrupado_barrio <- group_by(solo2014, BARRIO)

total_por_barrio_2014 <- summarise(solo2014_agrupado_barrio, total = sum(total))

head(total_por_barrio_2014)

total_por_barrio_2014_ordenado <- arrange(total_por_barrio_2014, desc(total))

head(total_por_barrio_2014_ordenado, 5)

atencion_ciudadano %>%
  filter(AÑO == 2014) %>%
  group_by(BARRIO) %>%
  summarise(total = sum(total)) %>%
  arrange(desc(total)) %>%
  head(5)
