library(tidyverse)

library(sf)

radios <- st_read("https://bitsandbricks.github.io/data/CABA_rc.geojson")

summary(radios)

ggplot() + geom_sf(data = radios)

ggplot() + geom_sf(data = radios, aes(fill = VIVIENDAS))

ggplot() + geom_sf(data = radios, aes(fill = POBLACION), color = NA)

ggplot() + geom_sf(data = radios, aes(fill = BARRIO), color = NA)

ggplot() + geom_histogram(data = radios, aes(x = AREA_KM2))

filtrados <- radios %>%
  filter(AREA_KM2 > 2)

ggplot() +
  geom_sf(data = filtrados, aes(fill = BARRIO)) +
  labs(title = "Radios censales de mayo tamaño")

ggplot() + geom_sf(data = radios, aes(fill = VIVIENDAS/HOGARES), color = NA)

radios %>%
  mutate(viv_vs_hogares = VIVIENDAS / HOGARES) %>%
  arrange(desc(viv_vs_hogares)) %>%
  head()

ggplot() +
  geom_sf(data = radios, aes(fill = POBLACION/AREA_KM2), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Densidad de población",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       fill = "hab/km2")

names(atencion_ciudadano)

names(radios)

barrios_geo <- radios %>%
  group_by(BARRIO) %>%
  summarise(POBLACION = sum(POBLACION),
            VIVIENDAS = sum(VIVIENDAS),
            HOGARES = sum(HOGARES),
            HOGARES_NBI = sum(HOGARES_NBI),
            AREA_KM2 = sum(AREA_KM2))

ggplot() + geom_sf(data = barrios_geo)

atencion_por_barrio <- atencion_ciudadano %>%
  group_by(BARRIO) %>%
  summarise(total = sum(total))

head(atencion_por_barrio)

barrios_geo <- barrios_geo %>% left_join(atencion_por_barrio)

ggplot() + geom_sf(data = barrios_geo, aes(fill = total))

ggplot() +
  geom_sf(data = barrios_geo, aes(fill = total/POBLACION)) +
  labs(title = "Contactos a atención ciudadana per cápita",
       subtitle = "Barrios de Ciudad Autónoma de Buenos Aires",
       fill = "contactos/habitante")

atencion_por_barrio_principal_rubro <- atencion_ciudadano %>%
  group_by(BARRIO, RUBRO) %>%
  summarise(contactos = sum(total)) %>%
  filter(contactos == max(contactos))

head(atencion_por_barrio_principal_rubro)

barrios_geo <- barrios_geo %>% left_join(atencion_por_barrio_principal_rubro)

ggplot() +
  geom_sf(data = barrios_geo, aes(fill = RUBRO)) +
  labs(title = "Principal categoría de las solicitudes/reclamos")

subte_lineas <- st_read("http://bitsandbricks.github.io/data/subte_lineas.geojson")

subte_estaciones <- st_read("http://bitsandbricks.github.io/data/subte_estaciones.geojson")

ggplot() +
  geom_sf(data = barrios_geo) +
  geom_sf(data = subte_lineas, color = "blue") +
  geom_sf(data = subte_estaciones, color = "orange") +
  labs(title = "Sistema de transporte subterráneo (SUBTE)",
       subtitle = "Ciudad de Buenos Aires")

emergencias_en_subte <- atencion_ciudadano %>%
  filter(RUBRO == "EMERGENCIAS EN SUBTE") %>%
  group_by(BARRIO) %>%
  summarise(emergencias = sum(total))

barrios_geo <- barrios_geo %>% left_join(emergencias_en_subte)

ggplot() +
  geom_sf(data = barrios_geo, aes(fill = emergencias)) +
  geom_sf(data = subte_lineas, color = "yellow") +
  geom_sf(data = subte_estaciones, color = "orange") +
  labs(title = "Emergencias relacionadas con el SUBTE",
       subtitle = "Registros de atención al ciudadano, Ciudad de Buenos Aires (2015 - 2017)",
       fill = "emergencias reportadas")
