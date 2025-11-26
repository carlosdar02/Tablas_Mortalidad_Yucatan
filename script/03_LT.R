
#******************************#
#******************************#
#
#                        Trabajo Final del curso de Demografia
#                                     2026-1
#                             Facultad de ciencias UNAM
#                         Tabla de Mortalidad Yucatan 2010, 2020
#                                  Tabla de Vida
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


# Preámbulo ----

## Limpieza de gráficas ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones----
source("script/funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

# Carga de tablas de datos ----
def <- fread("data/def.csv")
apv <- fread("data/apv.csv") 

# Unión de tablas de Años Persona Vividos y Defunciones ----
lt_input <- setDT(left_join(apv, def, by = c("year", "sex", "age")))

# Cálculo de mx ----
lt_input[ , mx := deaths/N]
lt_input[ , sex := if_else(sex=="male", "m", "f")]

## Gráfica - mx por año y sexo ----
ggplot(lt_input, aes(x = age, y = log(mx), color = sex, group = sex)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ year, ncol = 2) +
  scale_color_manual(
    values = c("m" = "steelblue", "f" = "lightcoral"),
    labels = c("m" = "Hombres", "f" = "Mujeres")
  ) +
  labs(
    title = "Tasa de mortalidad de Yucatan por año y sexo",
    x = "Edad",
    y = "log(mx)",
    color = "Sexo"
  ) +
  theme_minimal() 
ggsave("images/defunciones/tasa_mx_por_añoysexo.png", width = 10, height = 6, dpi = 300)


## Gráfica - mx por sexo y año ----
ggplot(lt_input, aes(x = age, y = log(mx), color = factor(year), group = year)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  facet_grid(. ~ sex, labeller = as_labeller(c("m" = "Hombres", "f" = "Mujeres"))) +
  scale_color_manual(
    values = c("2010" = "blue", "2019" = "orange", "2021" = "purple"),
    name = "Año"
  ) +
  labs(
    title = "Evolución de la tasa de mortalidad de Yucatan",
    #    subtitle = "Yucatan",
    x = "Edad",
    y = "log(mx)"
  ) +
  theme_minimal()
ggsave("images/defunciones/evolucion_mx.png", width = 10, height = 6, dpi = 300)



# Tablas de mortalidad nacional - eevv + censales 2010, 2019 ----

lt_output <- data.table()

for( s in c( 'm', 'f' ) ){
  for( y in unique( lt_input$year ) ){
    
    temp_dt <- lt_input[ sex == s & year == y ]
    
    temp_lt <-
      lt_abr(x = temp_dt$age, 
             mx = temp_dt$mx, 
             sex = s) %>%
      setDT %>%
      .[ , year := y ] %>%
      .[ , sex := s ]
    
    lt_output <- 
      rbind(
        lt_output,
        temp_lt[ , .( lt_desc = 'LT VR/Census, MEX',
                      year = y, 
                      sex,
                      age = x, 
                      mx = round( mx, 6 ), 
                      qx = round( qx, 6 ),
                      ax = round( ax, 2 ), 
                      lx = round( lx, 0 ), 
                      dx = round( dx, 0 ), 
                      Lx = round( Lx, 0 ), 
                      Tx = round( Tx, 0 ), 
                      ex = round( ex, 2 )) ]
      )
    
  }
  
}

## Esperanzas de vida al nacer ----
lt_output[ age == 0 ] %>% dcast( year ~ sex, value.var = 'ex' )

## Mortalidad infantil ----
lt_output[ age == 0 ] %>% dcast( year ~ sex, value.var = 'qx' )

## Gráfica - qx por sexo y año ---- 
lt_output %>%
  ggplot( ) +
  geom_line( aes( x = age, y = qx, color = factor( year ), group = factor(year ) ), size = 1 ) +
  scale_y_log10() +
  scale_color_manual(
    values = c(
      "2010" = "red",  # Pinkish-purple
      "2019" = "blue",  # Turquoise
      "2021" = "orange",    #Orange
      "Other Years"      = "gray70"    # Light gray for other years
    ),
    name = "Años"
  ) +
  facet_wrap( ~ sex, ) +
  labs(color='año') +   
  # theme_bw()
  theme_classic() +
  ylab("Probabilidad de muerte (qx)") +
  xlab("Edad") +
  labs(colour = "Años")
ggsave("images/defunciones/qx_por_añoysexo.png", width = 10, height = 6, dpi = 300)



##Guardar archivo
write.csv(lt_output,"data/lt_yuc.csv", row.names = F )


# -------- FIN ----------* 
