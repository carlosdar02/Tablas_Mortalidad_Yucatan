#******************************#
#******************************#
#
#                        Trabajo Final del curso de Demografia
#                                     2026-1
#                             Facultad de ciencias UNAM
#                         Tabla de Mortalidad Yucatan 2010, 2020
#                             Causa Eliminada - Homicidios
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

## Carga de paquetes y funciones ----
source("script/funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)
library(openxlsx)

# Carga de datos ----

def_homicidios <- fread("data/def_pro_hom.csv")
def_totales <- fread("data/def_pro.csv")
apv <- fread("data/apv.csv")
lt_output <- fread("data/lt_yuc.csv")

# Verificar y corregir estructura de datos ----
cat("Estructura de lt_output:\n")
print(names(lt_output))
cat("\n")

# Si lt_output tiene columna 'x' en lugar de 'age', renombrar
if ("x" %in% names(lt_output) & !"age" %in% names(lt_output)) {
  lt_output[, age := x]
  lt_output[, x := NULL]
}

# Procesamiento para causa eliminada ----

## Filtrar años de interés para homicidios ----
def_homicidios <- def_homicidios[year %in% c(2009, 2010, 2011, 2018, 2019, 2021)]

## Crear años de referencia (igual que en el procesamiento original) ----
def_homicidios[, year_new := ifelse(year %in% 2009:2011, 2010,
                                    ifelse(year %in% 2018:2019, 2019, 2021))]

## Promediar homicidios por año de referencia ----
homicidios_promedio <- def_homicidios[, .(deaths_homicidios = mean(deaths)), 
                                      by = .(year = year_new, sex, age)]

## Unir homicidios con defunciones totales ----
def_completa <- merge(def_totales, homicidios_promedio, 
                      by = c("year", "sex", "age"), 
                      all.x = TRUE)

## Si no hay datos de homicidios para alguna observación, asumir 0 ----
def_completa[is.na(deaths_homicidios), deaths_homicidios := 0]

## Calcular defunciones por otras causas (excluyendo homicidios) ----
def_completa[, deaths_otras := deaths - deaths_homicidios]

## Calcular proporción de homicidios ----
def_completa[, prop_homicidios := deaths_homicidios / deaths]
def_completa[is.infinite(prop_homicidios) | is.na(prop_homicidios), prop_homicidios := 0]

# Unir con población para calcular tasas ----

## Unir defunciones completas con población ----
lt_input_homicidios <- merge(apv, def_completa, by = c("year", "sex", "age"), all.x = TRUE)

## Calcular tasas de mortalidad ----
# mx totales (todas las causas)
lt_input_homicidios[, mx_todas_causas := deaths / N]

# mx sin homicidios
lt_input_homicidios[, mx_sin_homicidios := deaths_otras / N]

# Asegurar que no hay valores negativos
lt_input_homicidios[mx_sin_homicidios < 0, mx_sin_homicidios := 0]

# Convertir sexo a formato corto
lt_input_homicidios[, sex := ifelse(sex == "male", "m", "f")]

# Construcción de tablas de vida sin homicidios ----

lt_sin_homicidios <- data.table()

for (s in c('m', 'f')) {
  for (y in c(2010, 2019, 2021)) {
    
    cat("Procesando:", s, "-", y, "\n")
    
    # Filtrar datos para el sexo y año
    temp_dt <- lt_input_homicidios[sex == s & year == y]
    
    # Construir tabla de vida sin homicidios
    temp_lt <- lt_abr(
      x = temp_dt$age,
      mx = temp_dt$mx_sin_homicidios,
      sex = s
    )
    
    # Convertir a data.table y agregar columnas
    temp_lt <- as.data.table(temp_lt)
    temp_lt[, year := y]
    temp_lt[, sex := s]
    temp_lt[, causa := "sin_homicidios"]
    
    # Renombrar columna 'x' a 'age' para consistencia
    if ("x" %in% names(temp_lt)) {
      temp_lt[, age := x]
      temp_lt[, x := NULL]
    }
    
    lt_sin_homicidios <- rbind(lt_sin_homicidios, temp_lt, fill = TRUE)
  }
}

# Preparar datos para comparación ----

## Agregar identificador de causa a las tablas originales ----
lt_output[, causa := "todas_causas"]

## Verificar columnas disponibles ----
cat("Columnas en lt_output:", names(lt_output), "\n")
cat("Columnas en lt_sin_homicidios:", names(lt_sin_homicidios), "\n")

## Seleccionar columnas comunes para comparación ----
columnas_comunes <- intersect(names(lt_output), names(lt_sin_homicidios))
columnas_comunes <- columnas_comunes[columnas_comunes %in% 
                                       c("year", "sex", "age", "mx", "qx", "lx", "dx", "Lx", "Tx", "ex", "causa")]

cat("Columnas comunes para comparación:", columnas_comunes, "\n")

## Combinar tablas ----
lt_comparacion <- rbind(
  lt_output[, ..columnas_comunes],
  lt_sin_homicidios[, ..columnas_comunes],
  fill = TRUE
)

# Análisis de resultados ----

## Esperanza de vida al nacer comparativa ----
e0_comparacion <- lt_comparacion[age == 0, .(year, sex, ex, causa)]

## Diferencia en esperanza de vida debido a homicidios ----
e0_todas <- e0_comparacion[causa == "todas_causas", .(year, sex, ex_todas = ex)]
e0_sin <- e0_comparacion[causa == "sin_homicidios", .(year, sex, ex_sin = ex)]

e0_dif <- merge(e0_todas, e0_sin, by = c("year", "sex"))
e0_dif[, dif_homicidios := ex_sin - ex_todas]  # Cambio aquí: ex_sin - ex_todas

cat("Diferencia en esperanza de vida debido a homicidios:\n")
print(e0_dif)

# Gráficas comparativas ----

## Crear directorio para imágenes si no existe ----
if (!dir.exists("images")) dir.create("images", recursive = TRUE)
if (!dir.exists("images/homicidios")) dir.create("images/homicidios", recursive = TRUE)

## 1. Esperanza de vida al nacer con y sin homicidios ----
p1_horizontal <- ggplot(e0_comparacion, aes(x = causa, y = ex, fill = causa)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", ex)), 
            vjust = -0.5, 
            size = 4,
            fontface = "bold") +
  facet_grid(sex ~ year, 
             labeller = labeller(
               sex = c("m" = "Hombres", "f" = "Mujeres"),
               year = as_labeller(function(x) paste("Año", x))
             )) +
  scale_fill_manual(
    values = c("todas_causas" = "#1f77b4", "sin_homicidios" = "#ff7f0e"),
    labels = c("Todas causas", "Sin homicidios")
  ) +
  scale_y_continuous(
    limits = c(0, max(e0_comparacion$ex) * 1.15),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "ESPERANZA DE VIDA: IMPACTO DE HOMICIDIOS",
    subtitle = "Yucatán - Análisis por año y sexo",
    x = NULL,
    y = "Esperanza de Vida (años)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
    legend.position = "none",  # Quitamos leyenda por redundancia
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2)
  )

print(p1_horizontal)
ggsave("images/homicidios/e0_con_sin_homicidios.png", width = 10, height = 6, dpi = 300)

## 2. Probabilidades de muerte (qx) ----
# Filtrar para un año específico para mejor visualización
qx_2021 <- lt_comparacion[year == 2021 & age <= 85]

#opcion 1
p2 <- ggplot(qx_2021, aes(x = age, y = qx, color = causa)) +
  geom_line(size = 1) +
  scale_y_log10() +
  facet_grid(. ~ sex, 
             labeller = labeller(
               sex = c("m" = "Hombres", "f" = "Mujeres")
             )) +
  scale_color_manual(
    values = c("todas_causas" = "#1f77b4", "sin_homicidios" = "#ff7f0e"),
    labels = c("Todas las causas", "Sin homicidios")
  ) +
  labs(
    title = "PROBABILIDADES DE MUERTE: IMPACTO DE HOMICIDIOS",
    subtitle = "Yucatán 2021 - Escala logarítmica",
    x = "Edad",
    y = "qx (escala logarítmica)",
    color = NULL,
    caption = "Fuente: Elaboración propia con datos INEGI"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

print(p2)

#opcion 2
p2 <- ggplot(qx_2021, aes(x = age, y = qx, color = causa, linetype = sex)) +
  geom_line(size = 1) +
  scale_y_log10() +
  scale_color_manual(
    values = c("todas_causas" = "#1f77b4", "sin_homicidios" = "#ff7f0e"),
    labels = c("Todas las causas", "Sin homicidios")
  ) +
  scale_linetype_manual(
    values = c("m" = "solid", "f" = "dashed"),
    labels = c("Hombres", "Mujeres")
  ) +
  labs(
    title = "PROBABILIDADES DE MUERTE: IMPACTO DE HOMICIDIOS",
    subtitle = "Yucatán 2021 - Escala logarítmica",
    x = "Edad",
    y = "qx (escala logarítmica)",
    color = NULL,
    linetype = NULL,
    caption = "Fuente: Elaboración propia con datos INEGI"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
    legend.position = "top",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.5, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )

print(p2)


ggsave("images/homicidios/qx_con_sin_homicidios_2021.png", width = 12, height = 6, dpi = 300)

## 3. Diferencia en esperanza de vida por homicidios ----
p3 <- ggplot(e0_dif, aes(x = factor(year), y = dif_homicidios, fill = sex)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", dif_homicidios), 
                y = dif_homicidios + ifelse(dif_homicidios >= 0, 0.03, -0.08)),
            position = position_dodge(0.8),
            size = 3.5,  # Reducido de 4.5 a 3.5
            fontface = "bold",
            color = ifelse(e0_dif$dif_homicidios >= 0, "black", "darkred")) +
  scale_fill_manual(
    values = c("m" = "#1f77b4", "f" = "#ff7f0e"),
    labels = c("Hombres", "Mujeres")
  ) +
  scale_y_continuous(
    limits = c(min(e0_dif$dif_homicidios) - 0.1, max(e0_dif$dif_homicidios) + 0.15),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Pérdida de Esperanza de Vida por Homicidios",
    subtitle = "Yucatán 2010, 2019, 2021",
    x = NULL,
    y = "Pérdida de Esperanza de Vida (años)",
    fill = NULL,
    caption = "Fuente: Elaboración propia con datos INEGI"
  ) +
  theme_minimal(base_size = 12) +  # Reducido de 14 a 12
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 8)),  # Reducido
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12, margin = margin(b = 15)),  # Reducido
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 11, face = "bold"),  # Reducido
    legend.margin = margin(b = 10),
    legend.spacing.x = unit(0.7, "cm"),  # Reducido
    axis.text.x = element_text(size = 11, face = "bold", color = "black"),  # Reducido
    axis.text.y = element_text(size = 10, color = "black"),  # Reducido
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 8)),  # Reducido
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 0),  # Reducido
    plot.margin = margin(15, 15, 15, 15)  # Reducido
  ) +
  guides(fill = guide_legend(
    direction = "horizontal",
    keywidth = unit(1.2, "cm"),  # Reducido
    keyheight = unit(0.3, "cm")   # Reducido
  ))

print(p3)
ggsave("images/homicidios/perdida_e0_homicidios.png", width = 10, height = 6, dpi = 300)

# Exportar resultados ----

## Crear directorio de salida si no existe ----
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

## 1. Exportar tablas de vida sin homicidios a Excel ----
wb <- createWorkbook()

for (sexo in c("m", "f")) {
  for (anio in c(2010, 2019, 2021)) {
    hoja_nombre <- paste0(ifelse(sexo == "m", "Hombres_", "Mujeres_"), anio)
    
    # Seleccionar datos y asegurar nombres correctos
    datos_hoja <- lt_sin_homicidios[sex == sexo & year == anio]
    
    # Renombrar columnas si es necesario
    if ("x" %in% names(datos_hoja)) {
      datos_hoja[, age := x]
      datos_hoja[, x := NULL]
    }
    
    # Seleccionar columnas finales
    columnas_finales <- c("age", "n", "mx", "ax", "qx", "lx", "dx", "Lx", "Tx", "ex")
    columnas_finales <- columnas_finales[columnas_finales %in% names(datos_hoja)]
    
    datos_hoja <- datos_hoja[, ..columnas_finales]
    
    addWorksheet(wb, hoja_nombre)
    writeData(wb, hoja_nombre, datos_hoja)
  }
}

# Agregar hoja de resumen
addWorksheet(wb, "Resumen_e0")
writeData(wb, "Resumen_e0", e0_dif)

saveWorkbook(wb, "output/tablas_vida_sin_homicidios.xlsx", overwrite = TRUE)

## 2. Exportar datos de comparación ----
fwrite(lt_comparacion, "output/comparacion_causa_eliminada.csv")
fwrite(e0_dif, "output/resumen_e0_homicidios.csv")

## 3. Exportar datos de homicidios procesados ----
fwrite(def_completa, "output/defunciones_con_homicidios.csv")

# Resumen ejecutivo ----
cat("\n", strrep("=", 60), "\n", sep = "")
cat("RESUMEN EJECUTIVO - CAUSA ELIMINADA (HOMICIDIOS)\n")
cat(strrep("=", 60), "\n\n", sep = "")

cat("Años analizados: 2010, 2019, 2021\n")
cat("Entidad: Yucatán\n\n")

cat("Esperanza de vida al nacer:\n")
for (anio in c(2010, 2019, 2021)) {
  for (sexo in c("m", "f")) {
    e0_todas <- e0_comparacion[year == anio & sex == sexo & causa == "todas_causas", ex]
    e0_sin <- e0_comparacion[year == anio & sex == sexo & causa == "sin_homicidios", ex]
    dif <- e0_todas - e0_sin
    
    cat(sprintf("%s %s: Todas causas = %.2f, Sin homicidios = %.2f, Diferencia = %.3f años\n",
                ifelse(sexo == "m", "Hombres", "Mujeres"), anio, e0_todas, e0_sin, dif))
  }
}

cat("\nArchivos generados:\n")
cat("- output/tablas_vida_sin_homicidios.xlsx: Tablas completas sin homicidios\n")
cat("- output/comparacion_causa_eliminada.csv: Datos completos de comparación\n")
cat("- output/resumen_e0_homicidios.csv: Resumen de esperanzas de vida\n")
cat("- images/homicidios/: Gráficas comparativas\n")

cat(strrep("=", 60), "\n", sep = "")

cat("✅ Script de causa eliminada ejecutado exitosamente\n")

# -------- FIN ----------*