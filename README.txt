# Trabajo Final del curso de Demografía 2026-1
# Facultad de Ciencias UNAM
# Tabla de Mortalidad Yucatán 2010, 2020, 2021

## Integrantes
- De Anda Ruiz Carlos
- Ruiz Mora Jeremy Axel

## Descripción
Este proyecto tiene como objetivo construir las tablas de mortalidad para el estado de Yucatán para los años 2010, 2020 y 2021. Se utilizan datos del INEGI (censos y defunciones) y se aplican técnicas de preprocesamiento, prorrateo y ajuste para la construcción de las tablas.

## Estructura del proyecto
- `data/`: Contiene los datos brutos y procesados.
- `script/`: Contiene los scripts de R para el preprocesamiento y análisis.
- `README.md`: Este archivo.
- `.gitignore`: Archivo para ignorar ciertos archivos en el repositorio.

## Preprocesamiento
El preprocesamiento se realiza en el script `00_pre_process.R` que se encuentra en la carpeta `script/`. Este script:
1. Carga y limpia los datos censales (2010 y 2020).
2. Realiza el prorrateo de valores missing en edad y sexo.
3. Procesa las defunciones generales y por homicidio (1990-2024).
4. Guarda los datos procesados en la carpeta `data/`.

## Resultados
Los datos procesados (censos_pro.csv, def_pro.csv, def_pro_hom.csv) se utilizan para el cálculo de las tablas de mortalidad.

## Contacto
- carlosdar@ciencias.unam.mx
- jeremy@ciencias.unam.mx
