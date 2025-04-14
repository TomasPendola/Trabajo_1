library(pacman)
pacman::p_load(tidyverse,   # manipulacion datos
               sjPlot,      # tablas
               confintr,    # IC
               gginference, # visualizacion 
               rempsyc,     # reporte
               broom,       # varios
               sjmisc,      # para descriptivos 
               knitr,
               haven,
               kableExtra,
               stargazer,
               janitor)            

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo



ELSOC_Long_2016_2023 <- read_dta("C:/Users/Tomás/OneDrive/Documentos/Trabajo_1/input/ELSOC_Long_2016_2023.dta")

ELSOC_2022_Limitada <- ELSOC_Long_2016_2023 %>% select(r13_ideol_01, # orientacion politica
                                                       c05_12, # Confianza en los medios
                                                       t10) # percecion de Seguridad del barrio



ELSOC_2022_Limitada <- ELSOC_2022_Limitada %>%
  mutate(r13_ideol_01 = na_if(r13_ideol_01, -999),
         r13_ideol_01 = na_if(r13_ideol_01, -888),
         r13_ideol_01 = na_if(r13_ideol_01, -777),
         r13_ideol_01 = na_if(r13_ideol_01, -666))


ELSOC_2022_Limitada$r13_ideol_01 <- car::recode(ELSOC_2022_Limitada$r13_ideol_01, "c(1,2)=1; c(3)=2; c(4,5)=3; c(6)=4") #Se recodifica para hacer mas facil analisis estableciendo agrupando Centro derecha y derecha como uno solo, lo mismo para Izquierda, dejadno solos centro y ninguno


ELSOC_2022_Limitada$r13_ideol_01 <- factor(ELSOC_2022_Limitada$r13_ideol_01,
                                           labels=c( "Derecha",
                                                     "Centro",
                                                     "Izquierda",
                                                     "Ninguno"),
                                           levels=c(1,2,3,4))



tabla <- ELSOC_2022_Limitada %>%
  count(r13_ideol_01) %>%
  mutate(Porcentaje = round(100 * n / sum(n), 2))

tabla %>%
  kable("html", caption = "Tabla de Frecuencias de r13_ideol_01") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




ELSOC_2022_Limitada <- ELSOC_2022_Limitada %>%
  mutate(c05_12 = na_if(c05_12, -999),
         c05_12 = na_if(c05_12, -888),
         c05_12 = na_if(c05_12, -777),
         c05_12 = na_if(c05_12, -666))

frq(ELSOC_2022_Limitada$t10)



LSOC_2022_Limitada <- ELSOC_2022_Limitada %>%
  mutate(t10 = na_if(t10, -999),
         t10 = na_if(t10, -888),
         t10 = na_if(t10, -777),
         t10 = na_if(t10, -666))

frq(ELSOC_2022_Limitada$t10)



# Tabla de frecuencia para c05_12
tabla_freq <- ELSOC_2022_Limitada %>%
  tabyl(c05_12) %>%
  adorn_pct_formatting(digits = 2)

# Mostrar tabla con formato limpio
kable(tabla_freq, caption = "Tabla de Frecuencia de Confianza en los Medios de Comunicacion")


# Tabla de frecuencia para t10
tabla_freq2 <- ELSOC_2022_Limitada %>%
  tabyl(t10) %>%
  adorn_pct_formatting(digits = 2)

# Mostrar tabla con formato limpio
kable(tabla_freq2, caption = "Tabla de Frecuencia de Percepcion de Seguridad en el barrio")




graph1 <- ELSOC_2022_Limitada %>% ggplot(aes(x = r13_ideol_01)) + 
  geom_bar(fill = "springgreen4")+
  labs(title = "Confianza en instituciones",
       x = "Confianza en instituciones",
       y = "Frecuencia") +
  theme_bw()

graph1



graph2 <- ELSOC_2022_Limitada %>% ggplot(aes(x = c05_12)) + 
  geom_bar(fill = "#CD0000")+
  labs(title = "Confianza en Medios de comunicacion tradicionales",
       x = "Confianza en Medios de comunicacion tradicionales",
       y = "Frecuencia") +
  theme_bw()

graph2



graph3 <- ELSOC_2022_Limitada %>% ggplot(aes(x = t10)) + 
  geom_bar(fill = "#008B8B")+
  labs(title = "Percepcion de seguridad en el barrio",
       x = "Percepcion de seguridad en el barrio",
       y = "Frecuencia") +
  theme_bw()

graph3



ggplot(ELSOC_2022_Limitada, aes(x = as.factor(r13_ideol_01), y = c05_12)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    x = "Ideología política",
    y = "Cofianza en los medios de comunicacion tradiconales",
    title = "Distribución de Ideologia politica según Cofianza en los medios de comunicacion"
  ) +
  theme_minimal()



ggplot(ELSOC_2022_Limitada, aes(x = c05_12, y = t10)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Cofianza en los medios de comunicacion tradiconales",
    y = "Percepcion de seguridad en el barrio",
    title = "Relación entre Confianza en los medios y Percepcion de seguiridad en el barrio"
  ) +
  theme_minimal()


