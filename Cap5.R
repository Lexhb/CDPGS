library(tidyverse)

data_mundial <- read.csv("https://bitsandbricks.github.io/data/gapminder.csv")

summary(data_mundial)

data_arg <- data_mundial %>%
  filter(pais == "Argentina")

data_arg

ggplot(data_arg) + geom_point(aes(x= anio, y= expVida)) +
  labs(title = "Correlación entre tiempo y expectativa de vida",
       subtitle = "Argentina",
       y = "expectativa de vida")

rename(data_arg, anio=año)

cor(data_arg$anio, data_arg$expVida)

rename(data_arg, año = anio)

names(data_arg)

modelo_exp <- lm(expVida ~ anio, data = data_arg)

modelo_exp

ggplot(data = data_arg) +
  geom_point(aes(x = anio, y = expVida)) +
  labs(title = "Correlación entre tiempo y expectativa de vida",
       subtitle = "Argentina",
       y = "expectativa de vida",
       caption = "con línea de regresión") +
  geom_abline(aes(intercept = -389.6063, slope = 0.2317), color = "blue")

ggplot(data = data_arg) +
  geom_point(aes(x = anio, y = expVida)) +
  labs(title = "Correlación entre tiempo y expectativa de vida",
       subtitle = "Argentina",
       y = "expectativa de vida",
       caption = "con línea de regresión") +
  geom_abline(aes(intercept = -389.6063, slope = 0.2317), color = "blue") +
  xlim(c(1950, 2030)) +
  ylim(c(60, 85))

ggplot(data = data_arg) +
  geom_point(aes(x = anio, y = expVida)) +
  labs(title = "Correlación entre tiempo y expectativa de vida",
       subtitle = "Argentina",
       y = "expectativa de vida",
       caption = "con línea de regresión vía geom_smooth()") +
  geom_smooth(aes(x = anio, y = expVida), method = "lm")

ggplot(data = data_arg) +
  geom_point(aes(x = anio, y = PBI_PC)) +
  labs(title = "Correlación entre PBI y expectativa de vida",
       subtitle = "Argentina",
       y = "PBI per cápita") +
  geom_smooth(aes(x = anio, y = PBI_PC), method = "lm")

modelo_PBI <- lm(PBI_PC ~ anio, data = data_arg)

modelo_PBI

residuos <- residuals(modelo_PBI)

residuos

ggplot(data_arg) +
  geom_point(aes(x = anio, y = residuos)) +
  geom_hline(yintercept = 0, col = "blue") +
  labs(x = "anio", y = "residuo del modelo lineal")

ggplot(data_arg) +
  geom_line(aes(x = anio, y = PBI_PC)) +
  geom_vline(aes(xintercept = 2001), color = "red") +
  labs(title = "Evolución del PBI en la Argentina",
       y = "PBI per cápita",
       caption = "La línea roja indica la ocurrencia de la crisis del 2001")
