# SEE Proyecto Final — Data Science

Proyecto final del curso de Data Science de la SEE (Sociedad de Estudiantes de Estadística). Análisis del **WiDS Datathon 2020**: predicción de mortalidad hospitalaria en pacientes de UCI (Unidad de Cuidados Intensivos).

## Descripción

Se desarrolló un modelo predictivo de mortalidad hospitalaria utilizando variables demográficas, signos vitales, resultados de laboratorio y comorbilidades de ~91,000 pacientes de UCI. El modelo final (regresión logística con tidymodels) alcanzó un AUC-ROC de 0.868 y fue desplegado como una aplicación Shiny interactiva.

## Estructura del proyecto

```
see-proyecto-final-data-science/
├── scripts/                         # Pipeline de análisis
│   ├── 01-limpieza-de-datos.R       # Importación y limpieza de datos
│   ├── 02-preprocesamiento.R        # EDA y transformación de variables
│   ├── 03-mortalidad-hospitalaria.R # Análisis de mortalidad por hospital
│   ├── 04-modelamiento-shiny-app.R  # Entrenamiento de modelos ML
│   ├── 05-datathon.R                # Script exploratorio inicial
│   └── report.Rmd                   # Reporte Rmarkdown
├── shiny/                           # Aplicación Shiny
│   ├── global.R                     # Carga de datos y preprocesamiento
│   ├── server.R                     # Lógica del servidor
│   ├── ui.R                         # Interfaz de usuario
│   └── assets/                      # Imágenes de la app
├── data/                            # Datos (no incluidos en el repo)
└── results/                         # Resultados generados (no incluidos)
```

## Datos

Los datos provienen del [WiDS Datathon 2020](https://www.kaggle.com/c/widsdatathon2020) de Kaggle. Para reproducir el análisis, descarga los archivos y colócalos en `data/`:

- `training_v2.csv` — Datos de entrenamiento (~91,000 registros)
- `unlabeled.csv` — Datos sin etiqueta

## Modelos evaluados

| Modelo | AUC-ROC | Balanced Accuracy |
|:---|:---|:---|
| Regresión logística | **0.868** | **0.785** |
| MARS | 0.850 | 0.768 |
| Light GBM | 0.859 | 0.600 |
| Naive Bayes | 0.829 | 0.668 |

Se seleccionó la **regresión logística** por su balance entre rendimiento, interpretabilidad y velocidad de entrenamiento.

## Herramientas

- R / RStudio
- tidymodels (recipes, parsnip, workflows)
- Shiny (app interactiva desplegada en shinyapps.io)
- ggplot2, plotly (visualización)

## Aplicación Shiny

La app "ARQUIMEDEZ" permite a médicos ingresar datos clínicos de un paciente y obtener una predicción de mortalidad. Para ejecutarla localmente:

```r
shiny::runApp("shiny/")
```

## Autores

- Dra. Yalbi Balderas
- Dr. Jaime Soria
- Ing. Belén Escola
