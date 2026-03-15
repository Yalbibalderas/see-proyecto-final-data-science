
# Importar librerias generales --------------------------------------------

library(openxlsx) # Importar y exportar a excel
library(tidyverse) # Manipulación de datos
library(magrittr) # Pipe %>%, %$%
library(ggplot2) # Gráficos estáticos
library(DataExplorer)
library(tibble)
library(dplyr)
library(mlogit)# para regresion loggistica
library(tidyr)


# Importar base de datos --------------------------------------------------

training_v2 <- read.csv(file = "data/training_v2.csv")
unlabeled <- read.csv(file = "data/unlabeled.csv")


# Limpiesa de la base de datos --------------------------------------------

#elective_surgery= The common national or cultural tradition which the person belongs to

attach(data) #para no tener que siempre estar poniendo data antes de las variables

data <- training_v2
data$encounter_id <- NULL
data$patient_id <- NULL

attach(data) #para no tener que siempre estar poniendo data antes de las variables

mutar_caracter = function(x) {
  data <- data %>% mutate(x= as.character(x))
}

mutar_caracter(hospital_death)
mutar_caracter(elective_surgery)

#data <- data %>% mutate(hospital_death= as.numeric(hospital_death))
#data <- data %>% mutate(elective_surgery= as.character(elective_surgery))



data <- relevel(hospital_death, 0)




# Estadistica descriptiva -------------------------------------------------



summary(data$hospital_admit_source)

table(data$elective_surgery)
barplot(table(data$hospital_death))



View(training_v2)

create_report(training_v2)
glimpse(training_v2)

table(data$elective_surgery)
barplot(table(data$hospital_death))

# regresion logistica -----------------------------------------------------

mortalidad_Model1 = glm(formula = hospital_death ~ age, data = data, family = binomial())
mortalidad_Model2 = glm(formula = hospital_death ~ age + elective_surgery, data = data, family = binomial())


summary(mortalidad_Model2)                        
                  