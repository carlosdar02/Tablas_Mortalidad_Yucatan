#******************************#
#******************************#
#
#                        Trabajo Final del curso de Demografia
#                                     2026-1
#                             Facultad de ciencias UNAM
#                         Tabla de Mortalidad Yucatan 2010, 2020
#                             Descomposición de e_0
#
#         Creado por:               De Anda Ruiz Carlos 
#                                   Ruiz Mora Jeremy Axel
#         Fecha de creación:        04/11/2025
#         Actualizado por:          De Anda Ruiz Carlos 
#                                   Ruiz Mora Jeremy Axel
#         Fecha de actualización:   25/11/2025
#         Contacto:                 carlosdar@ciencias.unam.mx
#                                   jeremy@ciencias.unam.mx
#
#
#******************************#
#******************************#

# Preámbulo ----

## Limpieza de gráficas ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

# Carga de paquetes----
library(data.table)
library(ggplot2)
library(openxlsx)
library(readxl)
library(reshape2)
library(lubridate)
library(data.table)
library(dplyr)

# Carga de funciones
source("script/funciones.R")

# Carga de tablas de vida
lt_output <- fread("data/lt_yuc.csv")

# Aplicar descomposición por sexo y períodos
descomposicion_resultados <- list()

# Período 2010-2019
for (sexo in c("m", "f")) {
  lt_2010 <- lt_output[year == 2010 & sex == sexo]
  lt_2019 <- lt_output[year == 2019 & sex == sexo]
  
  descomp <- descomposicion_arriaga(lt_2010, lt_2019)
  descomp$tabla_contribuciones[, `:=`(
    sexo = sexo,
    periodo = "2010-2019"
  )]
  descomposicion_resultados[[paste0("2010_2019_", sexo)]] <- descomp
}

# Período 2019-2021
for (sexo in c("m", "f")) {
  lt_2019 <- lt_output[year == 2019 & sex == sexo]
  lt_2021 <- lt_output[year == 2021 & sex == sexo]
  
  descomp <- descomposicion_arriaga(lt_2019, lt_2021)
  descomp$tabla_contribuciones[, `:=`(
    sexo = sexo,
    periodo = "2019-2021"
  )]
  descomposicion_resultados[[paste0("2019_2021_", sexo)]] <- descomp
}

# Consolidar resultados
tabla_descomposicion <- rbindlist(
  lapply(descomposicion_resultados, function(x) x$tabla_contribuciones)
)

# Gráficas de descomposición
ggplot(tabla_descomposicion, aes(x = edad, y = contribucion, fill = contribucion > 0)) +
  geom_col(width = 2.5, alpha = 0.8) +  # Aumenté el width para mejor visualización
  facet_grid(sexo ~ periodo, labeller = as_labeller(c(
    "m" = "Hombres", 
    "f" = "Mujeres",
    "2010-2019" = "2010-2019",
    "2019-2021" = "2019-2021"
  ))) +
  scale_fill_manual(
    values = c("TRUE" = "#1f77b4", "FALSE" = "#d62728"),
    labels = c("TRUE" = "Aumento e₀", "FALSE" = "Disminución e₀"),
    name = "Contribución"
  ) +
  scale_x_continuous(
    breaks = seq(0, 85, by = 10),  # Etiquetas cada 10 años
    limits = c(0, 85)
  ) +
  labs(
    title = "Contribución por Edad a los Cambios en Esperanza de Vida",
    subtitle = "Descomposición de Arriaga - Yucatán",
    x = "Edad",
    y = "Contribución (años)",
    caption = "Fuente: Elaboración propia con datos INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5)
ggsave("images/ev/cambios_de_ev.png", width = 10, height = 6, dpi = 300)

# Guardar resultados
write.xlsx(
  list(
    descomposicion = tabla_descomposicion,
    resumen = data.table(
      periodo = rep(c("2010-2019", "2019-2021"), each = 2),
      sexo = rep(c("m", "f"), 2),
      dif_e0 = sapply(descomposicion_resultados, function(x) x$diferencia_e0)
    )
  ),
  "output/descomposicion_resultados.xlsx"
)











