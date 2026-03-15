# =============================================================================
# global.R — Carga de datos y preprocesamiento para la app Shiny
# Proyecto: Predicción de mortalidad en UCI (WiDS Datathon 2020)
# Descripción: Preprocesa los datos de UCI y entrena el modelo de regresión
#              logística que se usa en la app de predicción.
# =============================================================================

# Por último, la función shinyApp crea objetos
# de la aplicación shiny a partir de ui y de server.
# library(shiny)
library(ggplot2)
library(shinythemes)
library(vioplot)
library(rsconnect)
library(stats)
library(tidyverse) # Manipulación de datos y ggplot2
library(magrittr) # pipe
library(dplyr)
#library(sm)
#library(zoo)
library(rsample)
library(tidymodels) # Facilita modelamiento - new way
library(themis) #para balancear datos con up
library(parsnip) #para regresion logistica y random forest
library(DescTools) #para calcular psudo r2

# Importar base de datos --------------------------------------------------

training_v2 <- read.csv(file = "../data/training_v2.csv")

# Limpieza de base de datos -----------------------------------------------
data_demo <- training_v2
data_demo$encounter_id <- NULL #se retira porque es un identificador
data_demo$patient_id <- NULL #se retira porque es un identificador
data_demo$icu_id <- NULL #se retira porque es un identificador
data_demo$readmission_status <- NULL # se retira porque todos son 0
data_demo$icu_admit_type <- NULL # se retira porque no tiene elementos
data_demo$ethnicity # No se usa porque parece que esta variable no influye, hispanos son 3.600 y nativos 770, son los que tienen mayor prevalencia de mortalidad pero es muy baja
data_demo$height # se retira porque parece ser que no influye
data_demo$weight # se retira porque su influencia parece ya estar recogida en BMI
data_demo$pre_icu_los_days <- abs(data_demo$pre_icu_los_days) #se transforman los numeros negativos en absolutos porque asumo error al digitar y en algunos centros lo reportaron como negativo
data_demo$pre_icu_los_days <- round(data_demo$pre_icu_los_days, digits = 0) #se le quita los decimales al los dias de hospitalizacion
data_demo$hospital_death <- factor(
  data_demo$hospital_death,
  levels = c(0, 1),
  labels = c("Sobrevivió", "Falleció")
)

data_demo$elective_surgery <- factor(
  data_demo$elective_surgery,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$apache_post_operative <- factor(
  data_demo$apache_post_operative,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo[data_demo$gender == "", ] <- NA #transformar los espacios vacios en NA

data_demo$intubated_apache <- factor(
  data_demo$intubated_apache,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$ventilated_apache <- factor(
  data_demo$ventilated_apache,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$arf_apache <- factor(
  data_demo$arf_apache,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$aids <- factor(
  data_demo$aids,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$cirrhosis <- factor(
  data_demo$cirrhosis,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$diabetes_mellitus <- factor(
  data_demo$diabetes_mellitus,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$hepatic_failure <- factor(
  data_demo$hepatic_failure,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$immunosuppression <- factor(
  data_demo$immunosuppression,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$leukemia <- factor(
  data_demo$leukemia,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$lymphoma <- factor(
  data_demo$lymphoma,
  levels = c(0, 1),
  labels = c("No", "Si")
)

data_demo$solid_tumor_with_metastasis <- factor(
  data_demo$solid_tumor_with_metastasis,
  levels = c(0, 1),
  labels = c("No", "Si")
)

#Se transforma las variables de la escala de glasgow de variable continua a variable ordinal
data_demo$gcs_eyes_apache <- factor(data_demo$gcs_eyes_apache, ordered = TRUE)
data_demo$gcs_motor_apache <- factor(data_demo$gcs_motor_apache, ordered = TRUE)
data_demo$gcs_verbal_apache <- factor(
  data_demo$gcs_verbal_apache,
  ordered = TRUE
)

# Base de datos para entrenar el modelo -----------------------------------
data_UCI_noCat <- data_demo %>%
  filter(!is.na(hospital_death)) %>% #elimina a 25 paciente sin datos de mortalidad ni de genero
  dplyr::select(
    hospital_death, #NA = 0
    #demograficas
    age, #NA = 4216
    gender, #NA = 25
    bmi,
    #bmi_cat,
    ethnicity, #NA = 0
    elective_surgery, #NA = 0
    #hospital_admit_source, #NA = 0
    icu_admit_source, #NA = 0
    icu_stay_type, #NA = 0
    pre_icu_los_days, #NA = 0
    icu_type, #NA = 0
    #      signos vitales
    temp_apache,
    #temp_cat,
    map_apache,
    #map_cat,
    heart_rate_apache,
    #heart_rate_cat,
    resprate_apache,
    #resprate_cat,
    #      renales
    bun_apache,
    creatinine_apache,
    arf_apache,
    sodium_apache,
    #sodium_cat,
    urineoutput_apache,
    #hepaticas
    albumin_apache,
    bilirubin_apache,
    #      metabulicas
    glucose_apache,
    #glucose_cat,
    hematocrit_apache,
    #hematocrit_cat,
    wbc_apache,
    #wbc_cat,
    #     neurologicas
    gcs_eyes_apache,
    gcs_motor_apache,
    gcs_verbal_apache,
    #     respiratoria
    ventilated_apache,
    #     Comorbilidades
    aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
    cirrhosis, # NA = 715
    diabetes_mellitus, # NA = 715
    hepatic_failure, # NA = 715
    immunosuppression, # NA = 715
    leukemia, # NA = 715
    lymphoma, # NA = 715
    solid_tumor_with_metastasis
  ) %>% # NA = 715
  filter(!is.na(aids)) %>% #quito los 715 NA de: ventilated_apache aids cirrhosis diabetes_mellitus, hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
  filter(!is.na(gcs_motor_apache)) %>% #quita los 1183 de glasgow que faltaba
  filter(!is.na(resprate_apache)) %>% # se quitaron 448 que eran NA
  filter(!is.na(map_apache)) %>% # se quitaron 131 por ser NA
  filter(!is.na(heart_rate_apache)) %>% #se quitaron 13 por ser NA
  mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros

# Entrenar el modelo ------------------------------------------------------

# crear la base de datos --------------------------------------------------
fpe <- data_demo

xyclick <- function(e) {
  e$x
  e$y
}
