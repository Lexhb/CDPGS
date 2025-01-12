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

str(atencion_ciudadano)

levels(atencion_ciudadano$TIPO_PRESTACION)

contactos_por_comuna_y_tipo <- atencion_ciudadano %>%
  group_by(COMUNA, TIPO_PRESTACION) %>%
  summarise(miles_contactos = sum(total) / 1000 ) %>%
  left_join(habitantes)

head(contactos_por_comuna_y_tipo)

ggplot(contactos_por_comuna_y_tipo) + 
  geom_point(aes(x = POBLACION, y= miles_contactos)) +
  facet_wrap(~TIPO_PRESTACION)

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = BARRIO, weight = total))

options(scipen = 999)

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = BARRIO, weight = total)) +
  coord_flip()

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = BARRIO, weight = total, fill = TIPO_PRESTACION)) +
  coord_flip()

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = BARRIO, weight = total, color = TIPO_PRESTACION)) +
  coord_flip()

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = TIPO_PRESTACION, weight = total))

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = TIPO_PRESTACION, weight = total, fill = BARRIO))

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = TIPO_PRESTACION, weight = total)) +
  facet_wrap(~BARRIO)

contactos_por_mes <- atencion_ciudadano %>%
  group_by(PERIODO) %>%
  summarise(gran_total = sum(total))

head(contactos_por_mes)

ggplot(contactos_por_mes) +
  geom_histogram(aes(x = gran_total))

contactos_por_mes_y_tipo <- atencion_ciudadano %>%
  group_by(PERIODO, TIPO_PRESTACION) %>%
  summarise(gran_total = sum(total))

head(contactos_por_mes_y_tipo)

ggplot(contactos_por_mes_y_tipo) +
  geom_histogram(aes(x = gran_total)) +
  facet_wrap(~TIPO_PRESTACION)

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = BARRIO, weight = total, fill = TIPO_PRESTACION)) +
  coord_flip()

ggplot(atencion_ciudadano) +
  geom_bar(aes(x = BARRIO, weight = total, fill = TIPO_PRESTACION)) +
  coord_flip() +
  labs(title = "Contactos realizados al Sistema Único de Atención Ciudadana",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2013 - 2015",
       caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
       x = "barrio",
       y = "cantidad",
       fill = "Motivo del contacto")
