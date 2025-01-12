library(tidyverse)

contactos_por_comuna <- atencion_ciudadano %>%
  group_by(COMUNA) %>%
  summarise(miles_contactos = sum(total) / 1000 )

contactos_por_comuna

habitantes <- read.csv("http://bitsandbricks.github.io/data/gcba_pob_comunas_17.csv")

contactos_por_comuna <- left_join(contactos_por_comuna, habitantes)

#Similar

contactos_por_comuna <- contactos_por_comuna  %>% left_join(habitantes)

head(contactos_por_comuna)

ggplot(contactos_por_comuna) + geom_point(aes(x=POBLACION, y= miles_contactos))

ggplot(contactos_por_comuna) +
  geom_point(aes(x = POBLACION, y = miles_contactos, color = factor(COMUNA)))

ggplot(contactos_por_comuna) +
  geom_label(aes(x = POBLACION, y = miles_contactos, label = factor(COMUNA)))

ggplot(contactos_por_comuna) +
  geom_point(aes(x = POBLACION, y = miles_contactos, size = miles_contactos))

ggplot(contactos_por_comuna) +
  geom_point(aes(x = POBLACION, y = miles_contactos, shape = factor(COMUNA)))

ggplot(contactos_por_comuna) +
  geom_point(aes(x = POBLACION, y = miles_contactos), color = "blue")

ggplot(contactos_por_comuna) +
  geom_point(aes(x = POBLACION, y = miles_contactos), color = "darkolivegreen4")

ggplot(contactos_por_comuna) +
  geom_point(aes(x = POBLACION, y = miles_contactos), size = 5)

ggplot(contactos_por_comuna) +
  geom_point(aes(x = POBLACION, y = miles_contactos),
             size = 9, color = "chocolate3", shape = 0)

summary(atencion_ciudadano)

contactos_por_comuna_y_tipo <- atencion_ciudadano %>%
  group_by(COMUNA, TIPO_PRESTACION) %>%
  summarise(miles_contactos = sum(total) / 1000 ) %>%
  left_join(habitantes)

head(contactos_por_comuna_y_tipo)

ggplot(contactos_por_comuna_y_tipo) + 
  geom_point(aes(x = POBLACION, y= miles_contactos)) +
  facet_wrap(~TIPO_PRESTACION)
