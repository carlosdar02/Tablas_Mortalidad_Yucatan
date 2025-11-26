
#******************************#
#******************************#
#
#                        Trabajo Final del curso de Demografia
#                                     2026-1
#                             Facultad de ciencias UNAM
#                         Tabla de Mortalidad Yucatan 2010, 2020
#                                      APV
#
#         Creado por:               De Anda Ruiz Carlos 
#                                   Ruiz Mora Jeremy Axel
#         Fecha de creación:        04/11/2025
#         Actualizado por:          De Anda Ruiz Carlos 
#                                   Ruiz Mora Jeremy Axel
#         Fecha de actualización:   25/12/2025
#         Contacto:                 carlosdar@ciencias.unam.mx
#                                   jeremy@ciencias.unam.mx
#                                   
#
#******************************#
#******************************#


#Carga de paquetes y funciones ----
source("script/funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

#Carga de tabla de datos ----
censos_pro <- fread("data/censos_pro.csv")


# Cálculo de años persona vividos (población a mitad de año) 2010----
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2010.5)

apv2010 <- censos_pro[year==2010, .(age, sex, N)]
apv2010[ , year:= 2010]


##Grafica de APV 2010 ----

# Crear grupos quinquenales de edad
apv2010_quinquenal <- copy(apv2010)

# Definir grupos quinquenales (0-4, 5-9, 10-14, ..., 85+)
apv2010_quinquenal[, age_group := cut(age, 
                                      breaks = c(0, seq(5, 85, by = 5), Inf),
                                      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                                "25-29", "30-34", "35-39", "40-44", "45-49",
                                                "50-54", "55-59", "60-64", "65-69", "70-74",
                                                "75-79", "80-84", "85+"),
                                      right = FALSE)]

# Sumar la población por grupo quinquenal y sexo
apv2010_quinquenal <- apv2010_quinquenal[, .(N = sum(N, na.rm = TRUE)), by = .(age_group, sex)]

# Mantener el orden natural de los grupos de edad (sin rev())
apv2010_quinquenal$age_group <- factor(apv2010_quinquenal$age_group, 
                                       levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                                "25-29", "30-34", "35-39", "40-44", "45-49",
                                                "50-54", "55-59", "60-64", "65-69", "70-74",
                                                "75-79", "80-84", "85+"))

## Gráfica de APV 2010 con edades quinquenales ----
ggplot(apv2010_quinquenal, aes(x = age_group, y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional de Yucatán 2010",
    subtitle = "Distribución por grupos quinquenales de edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",    
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )
ggsave("images/apv/apv_2010.png", width = 10, height = 6, dpi = 300)


# Cálculo de años persona vividos (población a mitad de año) 2019 ----
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2019.5)
apv2019 <- censos_pro[year==2020, .(age, sex, N)]
apv2019[ , year:= 2019]


##Grafica de APV 2019 ----
# Crear grupos quinquenales de edad
apv2019_quinquenal <- copy(apv2019)

# Definir grupos quinquenales (0-4, 5-9, 10-14, ..., 85+)
apv2019_quinquenal[, age_group := cut(age, 
                                      breaks = c(0, seq(5, 85, by = 5), Inf),
                                      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                                 "25-29", "30-34", "35-39", "40-44", "45-49",
                                                 "50-54", "55-59", "60-64", "65-69", "70-74",
                                                 "75-79", "80-84", "85+"),
                                      right = FALSE)]

# Sumar la población por grupo quinquenal y sexo
apv2019_quinquenal <- apv2019_quinquenal[, .(N = sum(N, na.rm = TRUE)), by = .(age_group, sex)]

# Mantener el orden natural de los grupos de edad (sin rev())
apv2019_quinquenal$age_group <- factor(apv2019_quinquenal$age_group, 
                                       levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                                  "25-29", "30-34", "35-39", "40-44", "45-49",
                                                  "50-54", "55-59", "60-64", "65-69", "70-74",
                                                  "75-79", "80-84", "85+"))

## Gráfica de APV 2019 con edades quinquenales ----
ggplot(apv2019_quinquenal, aes(x = age_group, y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional de Yucatán 2019",
    subtitle = "Distribución por grupos quinquenales de edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",    
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )
ggsave("images/apv/apv_2019.png", width = 10, height = 6, dpi = 300)

# Cálculo de años persona vividos (población a mitad de año) 2021----
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2021.5)

apv2021 <- censos_pro[year==2020, .(age, sex, N)]
apv2021[ , year:= 2021]

##Grafica de APV 2021 ----
# Crear grupos quinquenales de edad
apv2021_quinquenal <- copy(apv2021)

# Definir grupos quinquenales (0-4, 5-9, 10-14, ..., 85+)
apv2021_quinquenal[, age_group := cut(age, 
                                      breaks = c(0, seq(5, 85, by = 5), Inf),
                                      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                                 "25-29", "30-34", "35-39", "40-44", "45-49",
                                                 "50-54", "55-59", "60-64", "65-69", "70-74",
                                                 "75-79", "80-84", "85+"),
                                      right = FALSE)]

# Sumar la población por grupo quinquenal y sexo
apv2021_quinquenal <- apv2021_quinquenal[, .(N = sum(N, na.rm = TRUE)), by = .(age_group, sex)]

# Mantener el orden natural de los grupos de edad (sin rev())
apv2021_quinquenal$age_group <- factor(apv2021_quinquenal$age_group, 
                                       levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                                  "25-29", "30-34", "35-39", "40-44", "45-49",
                                                  "50-54", "55-59", "60-64", "65-69", "70-74",
                                                  "75-79", "80-84", "85+"))

## Gráfica de APV 2019 con edades quinquenales ----
ggplot(apv2021_quinquenal, aes(x = age_group, y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional de Yucatán 2021",
    subtitle = "Distribución por grupos quinquenales de edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",    
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )
ggsave("images/apv/apv_2021.png", width = 10, height = 6, dpi = 300)



#Consolidar tablas 2010 y 2020 ----
apv <- rbind(apv2010, apv2019, apv2021)


#Guardar tabla de APV
write.csv(apv, "data/apv.csv")



