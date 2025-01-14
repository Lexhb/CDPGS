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

data_mundial_2007 <- data_mundial %>% filter(anio == 2007)

head(data_mundial_2007)

rename(data_mundial_2007, año = anio)

data_mundial_2007 <- data_mundial %>% filter(anio == 2007)

ggplot(data = data_mundial_2007) +
  geom_point(aes(x = continente, y = expVida, color = continente)) +
  labs(title = "Expectativa de vida por continente",
       y = "expectativa de vida")

ggplot(data = data_mundial_2007) +
  geom_jitter(aes(x = continente, y = expVida, color = continente)) +
  labs(title = "Expectativa de vida por continente",
       y = "expectativa de vida")

ggplot(data = data_mundial_2007) +
  geom_histogram(aes(x = expVida, fill = continente)) +
  facet_wrap(~continente) +
  labs(title = "Expectativa de vida por continente",
       subtitle = "histogramas",
       x = "expectativa de vida",
       y = "cantidad")

modelo_exp_continente <- lm(expVida ~ continente, data = data_mundial_2007)

modelo_exp_continente

data_mundial_2007 <- data_mundial_2007 %>%
  mutate(residuo_ml = residuals(modelo_exp_continente))

ggplot(data_mundial_2007) +
  geom_jitter(aes(x = continente, y = residuo_ml), width = 0.1) +
  geom_hline(yintercept = 0, col = "blue") +
  labs(x = "año", y = "residuo del modelo lineal")

data_mundial_2007 %>%
  filter(continente == "Asia") %>%
  arrange(expVida) %>%
  head()

data_afganistan <- data_mundial %>% filter(pais == "Afghanistan")

ggplot(data_afganistan) +
  geom_line(aes(x = anio, y = expVida)) +
  labs(title = "Expectativa de vida en Afganistán",
       y = "expectativa de vida")

##Regresion Múltiple

modelo_exp_multiple <- lm(expVida ~ pobl + PBI_PC, data = data_mundial_2007)

modelo_exp_multiple

data_mundial_2007 %>%
  arrange(desc(expVida)) %>%
  head(n = 10)

data_mundial_2007 %>%
  arrange(desc(pobl)) %>%
  head(n = 10)

cor(data_mundial_2007$expVida, data_mundial_2007$pobl)

options(scipen = 999)

summary(modelo_exp_multiple)

modelo_exp_multiple <- lm(expVida ~ pobl + PBI_PC + continente, data = data_mundial_2007)

summary(modelo_exp_multiple)
