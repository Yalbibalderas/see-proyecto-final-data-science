# =============================================================================
# 01 — Limpieza de datos
# Proyecto: Predicción de mortalidad en UCI (WiDS Datathon 2020)
# Autores: Yalbi Balderas, Jaime Soria, Belén Escola
# Descripción: Importación, limpieza y transformación de variables clínicas
#              para el análisis de mortalidad hospitalaria en UCI.
# =============================================================================

library(openxlsx)
library(tidyverse)
library(magrittr)
library(pROC)

# Importar base de datos --------------------------------------------------

training_v2 <- read.csv(file = "data/training_v2.csv")
unlabeled <- read.csv(file = "data/unlabeled.csv")

# Limpieza de la base de datos --------------------------------------------

#elective_surgery= The common national or cultural tradition which the person belongs to

data_demo <- training_v2
data_demo$encounter_id <- NULL #se retira porque es un identificador
data_demo$patient_id <- NULL #se retira porque es un identificador
data_demo$icu_id <- NULL #se retira porque es un identificador
data_demo$readmission_status <- NULL # se retira porque todos son 0
data_demo$icu_admit_type <- NULL # se retira porque no tiene elementos
data_demo$ethnicity <- NULL # se retira porque no influye significativamente
data_demo$height <- NULL    # se retira porque no influye
data_demo$weight <- NULL    # se retira porque su influencia ya está recogida en BMI
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

# Glasgow -----------------------------------------------------------------

data_demo <- mutate(
  data_demo,
  glasgow_apache = gcs_eyes_apache +
    gcs_motor_apache +
    gcs_verbal_apache
)

# Se transforman las variables de la escala de Glasgow de variable continua a variable ordinal
data_demo$gcs_eyes_apache <- factor(data_demo$gcs_eyes_apache, ordered = TRUE)
data_demo$gcs_motor_apache <- factor(data_demo$gcs_motor_apache, ordered = TRUE)
data_demo$gcs_verbal_apache <- factor(
  data_demo$gcs_verbal_apache,
  ordered = TRUE
)

data_demo2 <- data_demo %>% filter(!is.na(ventilated_apache))
data_demo2

# Ejemplo de gráfico
ggplot(data_demo2, aes(log(glasgow_apache))) +
  geom_histogram(aes(fill = hospital_death), binwidth = 1, position = 'fill') +
  facet_wrap(~ventilated_apache) +
  labs(
    x = 'Glasgow (log)',
    y = 'Porcentaje',
    title = 'Histograma del porcentaje de personas que fallecen
        en UCI según la variable ventilated_apache',
    fill = "Muerte hospitalaria"
  ) +
  scale_fill_manual(values = c("#8dd3c7", "#e41a1c")) # verde, rojo

# Transformar variables con puntos de corte -------------------------------

# Edad --------------------------------------------------------------------

data_demo %>%
  group_by(age = 1 * (age %/% 1)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(age, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_smooth(method = "lm", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Días previos de hospitalización ----
data_demo2 <- data_demo %>% filter(!is.na(hospital_death))
data_demo2

ggplot(data_demo2, aes(log2(pre_icu_los_days))) +
  geom_boxplot(fill = c("#8dd3c7", "#e41a1c")) +
  facet_grid(data_demo2$hospital_death ~ .)

plot(roc(data_demo2$hospital_death, data_demo2$pre_icu_los_days))
auc_pre_icu_los_days <- auc(roc(
  data_demo2$hospital_death,
  data_demo2$pre_icu_los_days
))
auc_pre_icu_los_days
#Area under the curve: 0.5378 # no tan buena variable, pero mas cercano

#plot(table(data_demo$pre_icu_los_days, data_demo$hospital_death))
#plot(prop.table(table(data_demo$pre_icu_los_days, data_demo$hospital_death), margin = 1))

data_demo <- mutate(
  data_demo,
  preIcuLosDays_cat = cut(
    data_demo$pre_icu_los_days,
    breaks = c(-Inf, 0, 1, 7, Inf),
    labels = c(
      "Un día",
      "De 1 día a 2 días",
      "De 2 a una semana",
      "Mas de una semana"
    ),
    ordered_result = TRUE
  )
)

plot(table(data_demo$preIcuLosDays_cat, data_demo$hospital_death))
#                   Sobrevivió Falleció   Cuanto riesgo aumentar
# Menos de 2 días        73182     6321   #0
# De 2 a una semana       9051     1211   #1
# Mas de una semana       1548      375   #2

plot(prop.table(
  table(data_demo$preIcuLosDays_cat, data_demo$hospital_death),
  margin = 1
))
#                   Sobrevivió   Falleció
# Menos de 2 días   0.92049357 0.07950643   #0.75
# De 2 a una semana 0.88199181 0.11800819   #1.25
# Mas de una semana 0.80499220 0.19500780   #2

# Temperatura -------------------------------------------------------------

# Se crean las variables dependiendo de la pendiente de mortalidad
data_demo <- mutate(
  data_demo,
  temp_apache_hipo = ifelse(
    data_demo$temp_apache <= 36.5,
    data_demo$temp_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  temp_apache_hiper = ifelse(
    data_demo$temp_apache > 36.5,
    data_demo$temp_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  temp_cat = cut(
    data_demo$temp_apache,
    breaks = c(-Inf, 36.5, Inf),
    labels = c("Hipotermia", "Fiebre"),
    ordered_result = TRUE
  )
)

# Funciona con el método gam mejor que loess
data_demo %>%
  group_by(temp_apache = 0.5 * (temp_apache %/% 0.5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(temp_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "gam", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Presión arterial --------------------------------------------------------

# Presión areterial media se crean las variables dependiendo de la pendiente de mortalidad
data_demo <- mutate(
  data_demo,
  map_apache_hipo = ifelse(data_demo$map_apache <= 95, data_demo$map_apache, NA)
)
data_demo <- mutate(
  data_demo,
  map_apache_hiper = ifelse(data_demo$map_apache > 95, data_demo$map_apache, NA)
)
data_demo <- mutate(
  data_demo,
  map_cat = cut(
    data_demo$map_apache,
    breaks = c(-Inf, 95, Inf),
    labels = c("Hipotensión", "Hipertensión"),
    ordered_result = TRUE
  )
)

# Funciona con el método gam mejor que loess
data_demo %>%
  group_by(map_apache = 5 * (map_apache %/% 5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(map_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "gam", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Frecuencia cardiaca -----------------------------------------------------

# Se crean las variables dependiendo de la pendiente de mortalidad
data_demo <- mutate(
  data_demo,
  heart_rate_apache_hipo = ifelse(
    data_demo$heart_rate_apache <= 50,
    data_demo$heart_rate_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  heart_rate_apache_norm = ifelse(
    data_demo$heart_rate_apache > 50 & data_demo$heart_rate_apache < 100,
    data_demo$heart_rate_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  heart_rate_apache_hiper = ifelse(
    data_demo$heart_rate_apache >= 100,
    data_demo$heart_rate_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  heart_rate_cat = cut(
    data_demo$heart_rate_apache,
    breaks = c(-Inf, 50, 100, Inf),
    labels = c("Bradicardia", "Normocardia", "Taquicardia"),
    ordered_result = TRUE
  )
)

data_demo2 <- data_demo %>% filter(!is.na(heart_rate_apache))
data_demo2

# Solo funciona con loess
data_demo2 %>%
  group_by(heart_rate_apache = 20 * (heart_rate_apache %/% 20)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(heart_rate_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Frecuencia respiratoria -------------------------------------------------

# Se crean las variables dependiendo de la pendiente de mortalidad
data_demo <- mutate(
  data_demo,
  resprate_apache_hipo = ifelse(
    data_demo$resprate_apache <= 10,
    data_demo$resprate_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  resprate_apache_norm = ifelse(
    data_demo$resprate_apache > 10 & data_demo$resprate_apache <= 35,
    data_demo$resprate_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  resprate_apache_hiper = ifelse(
    data_demo$resprate_apache > 35,
    data_demo$resprate_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  resprate_cat = cut(
    data_demo$resprate_apache,
    breaks = c(-Inf, 10, 35, Inf),
    labels = c("Bradicardia", "Normal", "Taquipnea"),
    ordered_result = TRUE
  )
)

# Solo funciona con loess
data_demo %>%
  group_by(resprate_apache = 8 * (resprate_apache %/% 8)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(resprate_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))


# BMI Indice de Masa Corporal ---------------------------------------------
#Frecuencia BMI se crean las variables dependiendo de la pendiente de mortalidad
data_demo <- mutate(
  data_demo,
  bmi_hipo = ifelse(data_demo$bmi <= 30, data_demo$bmi, NA)
)
data_demo <- mutate(
  data_demo,
  bmi_norm = ifelse(data_demo$bmi > 30 & data_demo$bmi <= 45, data_demo$bmi, NA)
)
data_demo <- mutate(
  data_demo,
  bmi_hiper = ifelse(data_demo$bmi > 45, data_demo$bmi, NA)
)
# data_demo <- mutate(data_demo,
#                     bmi_cat = cut(data_demo$bmi, breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
#                                        levels(c) <- c("Bajo peso",
#                                                       "Normal",
#                                                       "Sobrepeso",
#                                                       "Obesidad grado I",
#                                                       "Obbesidad grado II",
#                                                       "Obesidad grado III"),
#                                        ordered_result = TRUE)
# )
data_demo <- mutate(
  data_demo,
  bmi_cat = cut(
    data_demo$bmi,
    breaks = c(-Inf, 18.5, 30, Inf),
    labels = c("Bajo peso", "Normal", "Obesidad"),
    ordered_result = TRUE
  )
)

prop.table(table(data_demo$bmi, data_demo$bmi_cat), margin = 1)

# con loess se ve el efecto
data_demo %>%
  group_by(bmi = 5 * (bmi %/% 5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(bmi, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# BUN nitrogeno urico -----------------------------------------------------

data_demo %>%
  group_by(bun_apache = 5 * (bun_apache %/% 5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(bun_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Arf apache --------------------------------------------------------------

data_demo2 <- data_demo %>% filter(!is.na(arf_apache))
data_demo2

ggplot(data_demo2, aes(log(glasgow_apache))) +
  geom_histogram(aes(fill = hospital_death), binwidth = 1, position = 'fill') +
  facet_wrap(~arf_apache) +
  labs(
    x = 'Glasgow (log)',
    y = 'Porcentaje',
    title = 'Histograma del porcentaje de personas que fallecen
        en UCI según la variable arf_apache',
    fill = "Muerte hospitalaria"
  ) +
  scale_fill_manual(values = c("#8dd3c7", "#e41a1c")) # verde, rojo

# Creatinine apache --------------------------------------------------------------

data_demo %>%
  group_by(creatinine_apache = 1.5 * (creatinine_apache %/% 1.5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(creatinine_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Urine output apache --------------------------------------------------------------

data_demo %>%
  group_by(urineoutput_apache = 1400 * (urineoutput_apache %/% 1400)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(urineoutput_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Glucosa -----------------------------------------------------------------

data_demo <- mutate(
  data_demo,
  glucose_apache_hipo = ifelse(
    data_demo$glucose_apache <= 90,
    data_demo$glucose_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  glucose_apache_hiper = ifelse(
    data_demo$glucose_apache > 90,
    data_demo$glucose_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  glucose_cat = cut(
    data_demo$glucose_apache,
    breaks = c(-Inf, 90, Inf),
    labels = c("Hipoglicemia", "Hiperglicemia"),
    ordered_result = TRUE
  )
)

data_demo %>%
  group_by(glucose_apache = 5 * (glucose_apache %/% 5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(glucose_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Diabetes mellitus -------------------------------------------------------

data_demo2 <- data_demo %>% filter(!is.na(bun_apache))
data_demo2

ggplot(data_demo2, aes(glucose_apache)) +
  geom_histogram(aes(fill = hospital_death), binwidth = 10, position = 'fill') +
  facet_wrap(~diabetes_mellitus) +
  labs(
    x = 'bun_apache',
    y = 'Porcentaje',
    title = 'Histograma del porcentaje de personas que
que fallecen según la variable diabetes mellitus en UCI'
  ) +
  scale_fill_manual(values = c("#8dd3c7", "#e41a1c")) # verde, rojo

# Sodio -------------------------------------------------------------------

data_demo <- mutate(
  data_demo,
  sodium_apache_hipo = ifelse(
    data_demo$sodium_apache <= 140,
    data_demo$sodium_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  sodium_apache_hiper = ifelse(
    data_demo$sodium_apache > 140,
    data_demo$sodium_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  sodium_cat = cut(
    data_demo$sodium_apache,
    breaks = c(-Inf, 140, Inf),
    labels = c("Hiponatremia", "Hipernatremia"),
    ordered_result = TRUE
  )
)

data_demo %>%
  group_by(sodium_apache = 2 * (sodium_apache %/% 2)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(sodium_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))


# Hematocrito -------------------------------------------------------------

# hiper hacerlo polipidal

data_demo <- mutate(
  data_demo,
  hematocrit_apache_hipo = ifelse(
    data_demo$hematocrit_apache <= 40,
    data_demo$hematocrit_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  hematocrit_apache_hiper = ifelse(
    data_demo$hematocrit_apache > 40,
    data_demo$hematocrit_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  hematocrit_cat = cut(
    data_demo$hematocrit_apache,
    breaks = c(-Inf, 40, Inf),
    labels = c("Anemia", "Policitemia"),
    ordered_result = TRUE
  )
)

prop.table(
  table(data_demo$hematocrit_apache, data_demo$hematocrit_cat),
  margin = 1
)

data_demo %>%
  group_by(hematocrit_apache = 5 * (hematocrit_apache %/% 5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(hematocrit_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Globulos blancos --------------------------------------------------------

# hiper hacerlo polipidal
data_demo <- mutate(
  data_demo,
  wbc_apache_hipo = ifelse(data_demo$wbc_apache <= 6, data_demo$wbc_apache, NA)
)
data_demo <- mutate(
  data_demo,
  wbc_apache_hiper = ifelse(data_demo$wbc_apache > 6, data_demo$wbc_apache, NA)
)
data_demo <- mutate(
  data_demo,
  wbc_cat = cut(
    data_demo$wbc_apache,
    breaks = c(-Inf, 6, Inf),
    labels = c("Neutropenia", "Leucocitosis"),
    ordered_result = TRUE
  )
)

prop.table(table(data_demo$wbc_apache, data_demo$wbc_cat), margin = 1)

data_demo %>%
  group_by(wbc_apache = 2 * (wbc_apache %/% 2)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(wbc_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# Bilirubin_apache --------------------------------------------------------

# se recomienda quitar los 6 pacientes con bilirrubina igual o mayor a 40 porque se comportan como outliers
# bilirrubina no necesita cat porque tiene una relación lineal con mortalidad

hist(data_demo$bilirubin_apache)
table(data_demo$bilirubin_apache, data_demo$hospital_death)

data_demo <- data_demo %>%
  filter(bilirubin_apache < 40 | is.na(bilirubin_apache))

data_demo %>%
  group_by(bilirubin_apache = 6 * (bilirubin_apache %/% 6)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(bilirubin_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "lm", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# albumin_apache ----------------------------------------------------------
#Punto de corte en >= 3.4 es un valor normal

hist(data_demo$albumin_apache)
prop.table(
  table(data_demo$albumin_apache, data_demo$hospital_death),
  margin = 1
)

data_demo <- mutate(
  data_demo,
  albumin_apache_hipo = ifelse(
    data_demo$albumin_apache < 3.4,
    data_demo$albumin_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  albumin_apache_hiper = ifelse(
    data_demo$albumin_apache >= 3.4,
    data_demo$albumin_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  albumin_cat = cut(
    data_demo$albumin_apache,
    breaks = c(-Inf, 3.3, Inf),
    labels = c("Hipoalbuminemia", "Normoalbuminemia"),
    ordered_result = TRUE
  )
)

prop.table(table(data_demo$albumin_apache, data_demo$albumin_cat), margin = 1)

data_demo %>%
  group_by(albumin_apache = 0.5 * (albumin_apache %/% 0.5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(albumin_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "lm", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# fio2_apache -------------------------------------------------------------
# hay relación lineal entre FIO2 y mortalidad
# el FIO2 interactúa con PaOS

hist(data_demo$fio2_apache)

data_demo %>%
  group_by(fio2_apache = 0.1 * (fio2_apache %/% 0.1)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(fio2_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "lm", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# paco2_apache ------------------------------------------------------------

# se colocó como punto de corte de 45
# Se debe interactuar con el pH

#hist(data_demo$paco2_apache)
#table(data_demo$paco2_apache, data_demo$hospital_death)

data_demo <- mutate(
  data_demo,
  paco2_apache_hipo = ifelse(
    data_demo$paco2_apache <= 45,
    data_demo$paco2_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  paco2_apache_hiper = ifelse(
    data_demo$paco2_apache > 45,
    data_demo$paco2_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  paco2_cat = cut(
    data_demo$paco2_apache,
    breaks = c(-Inf, 45, Inf),
    labels = c("Hipocapnia", "Hipercapnia"),
    ordered_result = TRUE
  )
)

data_demo %>%
  group_by(paco2_apache = 5 * (paco2_apache %/% 5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(paco2_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# paco2_for_ph_apache -----------------------------------------------------
# el exactamente el mismo valor que el paco2_apache

#hist(data_demo$paco2_for_ph_apache)
#plot(data_demo$paco2_for_ph_apache, data_demo$paco2_apache)

data_demo %>%
  group_by(paco2_for_ph_apache = 5 * (paco2_for_ph_apache %/% 5)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(paco2_for_ph_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# pao2_apache -------------------------------------------------------------

# punto de corte 100
#hist(data_demo$pao2_apache)

data_demo <- mutate(
  data_demo,
  pao2_apache_hipo = ifelse(
    data_demo$pao2_apache <= 100,
    data_demo$pao2_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  pao2_apache_hiper = ifelse(
    data_demo$pao2_apache > 100,
    data_demo$pao2_apache,
    NA
  )
)
data_demo <- mutate(
  data_demo,
  pao2_cat = cut(
    data_demo$pao2_apache,
    breaks = c(-Inf, 100, Inf),
    labels = c("Hipoxia", "Hiperoxia"),
    ordered_result = TRUE
  )
)

data_demo %>%
  group_by(pao2_apache = 25 * (pao2_apache %/% 25)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(pao2_apache, hospital_death)) +
  geom_line(color = "#386cb0", linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))

# ph_apache ---------------------------------------------------------------

# hist(data_demo$ph_apache)
# punto de corte en 7.34
data_demo <- mutate(
  data_demo,
  ph_apache_hipo = ifelse(data_demo$ph_apache <= 7.33, data_demo$ph_apache, NA)
)
data_demo <- mutate(
  data_demo,
  ph_apache_hiper = ifelse(data_demo$ph_apache > 7.33, data_demo$ph_apache, NA)
)
data_demo <- mutate(
  data_demo,
  ph_cat = cut(
    data_demo$ph_apache,
    breaks = c(-Inf, 7.33, Inf),
    labels = c("Alcalisis", "Acidosis"),
    ordered_result = TRUE
  )
)

data_demo %>%
  group_by(ph_apache = 0.04 * (ph_apache %/% 0.04)) %>%
  summarise(
    hospital_death = (mean(as.numeric(hospital_death), na.rm = F)) - 1
  ) %>%
  ggplot(aes(ph_apache, hospital_death)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  stat_smooth(method = "loess", alpha = .2, colour = "red") +
  theme_classic() +
  theme(text = element_text(size = 14))
