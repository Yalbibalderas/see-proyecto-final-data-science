library(tidyverse) # Manipulación de datos
library(magrittr) # Pipe %>%, %$%
library(ggplot2) # Gráficos estáticos
library(tibble)
library(dplyr)
library(mlogit)# para regresion loggistica
library(tidyr)
library(psych)
library(car) #para ver grafico de residuos
library(lmtest) # para ver residuos, esta la preuba de bptest
library(DescTools) #para calcular psudo r2
library(scales) #para poner porcentajes en ggplot

library(stringr) # Manipulacion de texto
library(cowplot) # graficos ggplot
library(plotly) # graficos js
library(corrplot) # Grafico Correlacion
library(skimr) # Descriptivas



data_demo %>%  
  count(Contar_NA=(is.na(hospital_death)))
# Contar_NA     n
# 1     FALSE 91688
# 2      TRUE    25

# Muerte por edad
data_demo %>%
  count(age = 5* (age %/% 5),
        hospital_death) %>%
  pivot_wider(names_from = hospital_death, 
              values_from= n)


data_demo %>%
  filter(!is.na(hospital_id)) %>%
  mutate(hospital_id= fct_lump(as.character(hospital_id), prop = 0.03)) %>%
  count(hospital_id,hospital_death) %>%
  group_by(hospital_id) %>%
  mutate(porcentaje = n/ sum(n)) %>%
  ggplot(aes(hospital_id, porcentaje, fill = hospital_id)) +
  geom_col() +
  facet_wrap(~hospital_death, scales = "free")

# Grafico de mortalidad por edad
data_demo %>%
  group_by(age = 5* (age %/% 5)) %>%
  summarise(hospital_death = (mean(as.numeric(hospital_death), na.rm = F))-1) %>%
  ggplot(aes(age, hospital_death))+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())

#histograma de edad y mortalidad
ggplot(data_demo, aes(age)) +
  geom_histogram(aes(fill = hospital_death), binwidth = 10) +
  labs( x= 'Edad', y= 'Porcentaje',
        title= 'Histograma de la cantidad de personas que
que fallecen según la edad en UCI')

#histograma ponderado de edad y mortalidad
ggplot(data_demo, aes(age)) +
  geom_histogram(aes(fill = hospital_death), binwidth = 10, 
                 position = 'fill') +
  labs( x= 'Edad', y= 'Porcentaje',
        title= 'Histograma de la cantidad de personas que
que fallecen según la edad en UCI')


# Muerte por frecuencia cardiaca
#contar NA
data_demo %>%  
  count(Contar_NA=(is.na(heart_rate_apache)))

data_demo %>%
  count(age = 5* (heart_rate_apache %/% 5),
        hospital_death) %>%
  pivot_wider(names_from = hospital_death, 
              values_from= n)


data_demo %>%
  filter(!is.na(heart_rate_apache)) %>%
  mutate(heart_rate_apache= fct_lump(as.character(heart_rate_apache), prop = 0.03)) %>%
  count(heart_rate_apache,hospital_death) %>%
  group_by(heart_rate_apache) %>%
  mutate(porcentaje = n/ sum(n)) %>%
  ggplot(aes(heart_rate_apache, porcentaje, fill = heart_rate_apache)) +
  geom_col() +
  facet_wrap(~hospital_death, scales = "free")

# Grafico de mortalidad por frecuencia cardiaca
data_demo %>%
  group_by(heart_rate_apache = 5* (heart_rate_apache %/% 5)) %>%
  summarise(hospital_death = (mean(as.numeric(hospital_death), na.rm = F))-1) %>%
  ggplot(aes(heart_rate_apache, hospital_death))+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())

#histograma de edad y mortalidad
ggplot(data_demo, aes(heart_rate_apache)) +
  geom_histogram(aes(fill = hospital_death), binwidth = 10) +
  labs( x= 'Edad', y= 'Porcentaje',
        title= 'Histograma de la cantidad de personas que
que fallecen según la edad en UCI')

#histograma ponderado de edad y mortalidad
ggplot(data_demo, aes(heart_rate_apache)) +
  geom_histogram(aes(fill = hospital_death), binwidth = 10, 
                 position = 'fill') +
  labs( x= 'Edad', y= 'Porcentaje',
        title= 'Histograma de la cantidad de personas que
que fallecen según la edad en UCI')

# Muerte por precion arterial
#contar NA
data_demo %>%  
  count(Contar_NA=(is.na(map_apache)))

data_demo %>%
  count(map_apache = 5* (map_apache %/% 5),
        hospital_death) %>%
  pivot_wider(names_from = hospital_death, 
              values_from= n)


data_demo %>%
  filter(!is.na(heart_rate_apache)) %>%
  mutate(heart_rate_apache= fct_lump(as.character(heart_rate_apache), prop = 0.03)) %>%
  count(heart_rate_apache,hospital_death) %>%
  group_by(heart_rate_apache) %>%
  mutate(porcentaje = n/ sum(n)) %>%
  ggplot(aes(heart_rate_apache, porcentaje, fill = heart_rate_apache)) +
  geom_col() +
  facet_wrap(~hospital_death, scales = "free")

# Grafico de mortalidad por presion arterail
data_demo %>%
  group_by(map_apache = 5* (map_apache %/% 5)) %>%
  summarise(hospital_death = (mean(as.numeric(hospital_death), na.rm = F))-1) %>%
  ggplot(aes(map_apache, hospital_death))+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())

# Grafico de mortalidad por temperatura
data_demo %>%
  group_by(temp_apache = 0.5* (temp_apache %/% 0.5)) %>%
  summarise(hospital_death = (mean(as.numeric(hospital_death), na.rm = F))-1) %>%
  ggplot(aes(temp_apache, hospital_death))+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())

#histograma de edad y mortalidad
ggplot(data_demo, aes(map_apache)) +
  geom_histogram(aes(fill = hospital_death), binwidth = 10) +
  labs( x= 'Edad', y= 'Porcentaje',
        title= 'Histograma de la cantidad de personas que
que fallecen según la edad en UCI')

#histograma ponderado de edad y mortalidad
ggplot(data_demo, aes(map_apache)) +
  geom_histogram(aes(fill = hospital_death), binwidth = 10, 
                 position = 'fill') +
  labs( x= 'Edad', y= 'Porcentaje',
        title= 'Histograma de la cantidad de personas que
que fallecen según la edad en UCI')

#Frecuencia cardiaca y PA
ggplot(data_demo, aes(temp_apache_hipo, map_apache_hipo, color = hospital_death))+
  geom_point()

# Grafico de mortalidad por edad
ggplot(data_demo, aes(temp_apache_hipo, map_apache_hipo, z=round(heart_rate_apache), color = hospital_death))+
  geom_contour()

#color = hospital_death
#geom_contour()
#binwidth = 3


#revisar missing data


# Light Gradient Boosting Machine -----------------------------------------
trainm <- sparse.model.matrix(hospital_death ~ 
                                temp_apache +  
                                map_apache +
                                heart_rate_apache +
                                resprate_apache, data = data_demo)

train_label <- data_demo[,"hospital_death"]

train_matrix = lgb.Dataset(data = as.matrix(trainm), label = train_label)

# model parameters
params <- list(max_bin = 5,
               learning_rate = 0.001,
               objective = "binary",
               metric = 'binary_logloss')

#model training
UCI_lgbm = lightgbm(params = params, train_matrix, valid, nrounds = 1000)

# GAM ---------------------------------------------------------------------

UCI_gam<-gam(hospital_death ~ 
               temp_apache +  
               map_apache +
               heart_rate_apache +
               resprate_apache, 
             method = "REML", 
             data = data_demo, 
             family = "binomial"
)

summary(UCI_gam)
# Modulacion 1 ------------------------------------------------------------
#signos vitales
data_UCI_Vital <- data_demo %>%
  filter(!is.na(hospital_death))%>%
  dplyr::select(hospital_death, #NA = 0
                age, #NA = 4216
                gender, #NA = 25
                bmi_hipo,
                bmi_norm,
                bmi_hiper,
                temp_apache_hipo,
                temp_apache_hiper,
                map_apache_hipo, 
                map_apache_hiper, 
                heart_rate_apache_hipo, 
                heart_rate_apache_norm,
                heart_rate_apache_hiper,
                resprate_apache_hipo,
                resprate_apache_norm,
                resprate_apache_hiper,
                elective_surgery, #NA = 0
                ethnicity, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
                aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
                cirrhosis, # NA = 715
                diabetes_mellitus, # NA = 715
                hepatic_failure, # NA = 715
                immunosuppression, # NA = 715
                leukemia, # NA = 715
                lymphoma, # NA = 715
                solid_tumor_with_metastasis) %>% # NA = 715
  filter(!is.na(gender) | !is.na(gender))%>%
  mutate_if(is.character, factor)

# se crea la nueva base de datos con variables demograficas
data_UCI_segmentado <- data_demo %>%
  filter(!is.na(hospital_death))%>%
  dplyr::select(hospital_death, #NA = 0
                age, #NA = 4216
                gender, #NA = 25
                bmi_hipo,
                bmi_norm,
                bmi_hiper,
                temp_apache_hipo,
                temp_apache_hiper,
                map_apache_hipo, 
                map_apache_hiper, 
                heart_rate_apache_hipo, 
                heart_rate_apache_norm,
                heart_rate_apache_hiper,
                resprate_apache_hipo,
                resprate_apache_norm,
                resprate_apache_hiper,
                elective_surgery, #NA = 0
                ethnicity, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
                bun_apache,
                arf_apache,
                glucose_apache_hipo,
                glucose_apache_hiper,
                hematocrit_apache_hipo,
                hematocrit_apache_hiper,
                wbc_apache_hipo,
                wbc_apache_hiper,
                sodium_apache_hipo,
                sodium_apache_hipo,
                gcs_eyes_apache,
                gcs_motor_apache,
                gcs_verbal_apache,
                ventilated_apache,
                aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
                cirrhosis, # NA = 715
                diabetes_mellitus, # NA = 715
                hepatic_failure, # NA = 715
                immunosuppression, # NA = 715
                leukemia, # NA = 715
                lymphoma, # NA = 715
                solid_tumor_with_metastasis) %>% # NA = 715
  filter(!is.na(gender) | !is.na(gender))%>%
  mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros




# Se filtra la base de datos ----------------------------------------------
data_UCI_noCat <- data_demo %>%
  filter(!is.na(hospital_death))%>% #elimina a 25 paciente sin datos de mortalidad ni de genero
  dplyr::select(hospital_death, #NA = 0
                #demograficas
                age, #NA = 4216
                gender, #NA = 25
                bmi,
                #bmi_cat,
                ethnicity, #NA = 0
                elective_surgery, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
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
                solid_tumor_with_metastasis) %>% # NA = 715
  filter(!is.na(aids))%>% #quito los 715 NA de: ventilated_apache aids cirrhosis diabetes_mellitus, hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
  filter(!is.na(gcs_motor_apache))%>% #quita los 1183 de glasgow que faltaba
  filter(!is.na(resprate_apache))%>% # se quitaron 448 que eran NA
  filter(!is.na(map_apache))%>% # se quitaron 131 por ser NA
  filter(!is.na(heart_rate_apache))%>% #se quitaron 13 por ser NA
  mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros
dim(data_UCI_noCat)
#89198    36

data_UCI_all <- data_demo %>%
  filter(!is.na(hospital_death))%>% #elimina a 25 paciente sin datos de mortalidad ni de genero
  dplyr::select(hospital_death, #NA = 0
                #demograficas
                age, #NA = 4216
                gender, #NA = 25
                bmi,
                bmi_cat,
                ethnicity, #NA = 0
                elective_surgery, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
                #signos vitales
                temp_apache,
                temp_cat,
                map_apache, 
                map_cat, 
                heart_rate_apache, 
                heart_rate_cat,
                resprate_apache,
                resprate_cat,
                #renales
                bun_apache,
                creatinine_apache,
                arf_apache,
                sodium_apache,
                sodium_cat,
                urineoutput_apache,
                #hepaticas
                albumin_apache,
                bilirubin_apache,
                #metabulicas
                glucose_apache,
                glucose_cat,
                hematocrit_apache,
                hematocrit_cat,
                wbc_apache,
                wbc_cat,
                #neurologicas
                gcs_eyes_apache,
                gcs_motor_apache,
                gcs_verbal_apache,
                #respiratoria
                ventilated_apache,
                #Comorbilidades
                aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
                cirrhosis, # NA = 715
                diabetes_mellitus, # NA = 715
                hepatic_failure, # NA = 715
                immunosuppression, # NA = 715
                leukemia, # NA = 715
                lymphoma, # NA = 715
                solid_tumor_with_metastasis) %>% # NA = 715
  filter(!is.na(aids))%>% #quito los 715 NA de: ventilated_apache aids cirrhosis diabetes_mellitus, hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
  filter(!is.na(gcs_motor_apache))%>% #quita los 1183 de glasgow que faltaba
  filter(!is.na(resprate_apache))%>% # se quitaron 448 que eran NA
  filter(!is.na(map_apache))%>% # se quitaron 131 por ser NA
  filter(!is.na(heart_rate_apache))%>% #se quitaron 13 por ser NA
  mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros

dim(data_demo)
dim(data_UCI)
#missing data
#PREVIO A QUITAR LOS NA
#dim(data_UCI)
# 91688    41


data_UCI <- data_demo %>%
  filter(!is.na(hospital_death))%>% #elimina a 25 paciente sin datos de mortalidad ni de genero
  dplyr::select(hospital_death, #NA = 0
                age, #NA = 4216
                gender, #NA = 25
                bmi,
                bmi_cat,
                temp_apache,
                temp_cat,
                map_apache, 
                map_cat, 
                heart_rate_apache, 
                heart_rate_cat,
                resprate_apache,
                resprate_cat,
                elective_surgery, #NA = 0
                ethnicity, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
                bun_apache,
                arf_apache,
                glucose_apache,
                glucose_cat,
                hematocrit_apache,
                hematocrit_cat,
                wbc_apache,
                wbc_cat,
                sodium_apache,
                sodium_cat,
                gcs_eyes_apache,
                gcs_motor_apache,
                gcs_verbal_apache,
                ventilated_apache,
                aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
                cirrhosis, # NA = 715
                diabetes_mellitus, # NA = 715
                hepatic_failure, # NA = 715
                immunosuppression, # NA = 715
                leukemia, # NA = 715
                lymphoma, # NA = 715
                solid_tumor_with_metastasis) %>% # NA = 715
  filter(!is.na(aids))%>% #quito los 715 NA de: ventilated_apache aids cirrhosis diabetes_mellitus, hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
  filter(!is.na(gcs_motor_apache))%>% #quita los 1183 de glasgow que faltaba
  filter(!is.na(resprate_apache))%>% # se quitaron 448 que eran NA
  filter(!is.na(map_apache))%>% # se quitaron 131 por ser NA
  filter(!is.na(heart_rate_apache))%>% #se quitaron 13 por ser NA
  mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros

dim(data_demo)
dim(data_UCI)
#missing data
#PREVIO A QUITAR LOS NA
#dim(data_UCI)
# 91688    41

data_UCI %>% summarise_all(funs(sum(is.na(.))))

# hospital_death  age gender  bmi bmi_cat temp_apache temp_cat map_apache map_cat heart_rate_apache heart_rate_cat
#               0 4216      0 3422    3422        4103     4103        993     993               877            877
# resprate_apache resprate_cat elective_surgery ethnicity icu_admit_source icu_stay_type pre_icu_los_days icu_type
#             1233         1233                0         0                0             0                0        0
# bun_apache arf_apache glucose_apache glucose_cat hematocrit_apache hematocrit_cat wbc_apache wbc_cat sodium_apache
#       19250        715          11024       11024             19865          19865      21999   21999         18588
# sodium_cat gcs_eyes_apache gcs_motor_apache gcs_verbal_apache ventilated_apache aids cirrhosis diabetes_mellitus
#       18588            1898             1898              1898               715  715       715               715
# hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
#              715               715      715      715                         715


#DESPUES DE QUITAR LOS NA
dim(data_UCI)
#89198    41

#View(md.pattern(data_UCI))

data_UCI %>% summarise_all(funs(sum(is.na(.))))
# hospital_death  age gender  bmi bmi_cat temp_apache temp_cat map_apache map_cat
#               0 3451      0 3253    3253        3032     3032          0       0
# heart_rate_apache heart_rate_cat resprate_apache resprate_cat elective_surgery
#                  0              0               0            0                0
# ethnicity icu_admit_source icu_stay_type pre_icu_los_days icu_type bun_apache
#          0                0             0                0        0      18041
# arf_apache glucose_apache glucose_cat hematocrit_apache hematocrit_cat wbc_apache
#           0           9970        9970             18648          18648      20743
# wbc_cat sodium_apache sodium_cat gcs_eyes_apache gcs_motor_apache
#    20743         17397      17397               0                0
# gcs_verbal_apache ventilated_apache aids cirrhosis diabetes_mellitus
#                  0                 0    0         0                 0
# hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
#                0                 0        0        0                           0
#se crea reporte de la base de datos


# Crear reporte de data_UCI -----------------------------------------------
library(DataExplorer)
#create_report(data_UCI)

#Manejo de missing data con MICE
# BMI
prop.table(table(is.na(data_UCI$bmi), data_UCI$hospital_death), margin = 1)
# se puede usar mediana para los datos faltantes
#       Sobrevivió   Falleció
# FALSE 0.91613241 0.08386759
# TRUE  0.89794036 0.10205964

# temp_apache
prop.table(table(is.na(data_UCI$temp_apache), data_UCI$hospital_death), margin = 1)
# la mediana infravaloraria el resultado
#       Sobrevivió   Falleció
# FALSE 0.91713669 0.08286331
# TRUE  0.86807388 0.13192612

# bun_apache
prop.table(table(is.na(data_UCI$bun_apache), data_UCI$hospital_death), margin = 1)
# se puede usar mediana, similar la mortalidad
#      Sobrevivió   Falleció
# FALSE 0.91212390 0.08787610
# TRUE  0.92866249 0.07133751

# glucose_apache
prop.table(table(is.na(data_UCI$glucose_apache), data_UCI$hospital_death), margin = 1)
# Similar la glucosa de los NA, se puede usar mediana
#       Sobrevivió   Falleció
# FALSE 0.91364164 0.08635836
# TRUE  0.92998997 0.07001003

# hematocrit_apache
prop.table(table(is.na(data_UCI$hematocrit_apache), data_UCI$hospital_death), margin = 1)
# similar el hematovrito, se podria usar mediana
#      Sobrevivió   Falleció
# FALSE 0.91318214 0.08681786
# TRUE  0.92412055 0.07587945

# wbc_apache
prop.table(table(is.na(data_UCI$wbc_apache), data_UCI$hospital_death), margin = 1)
# si se podria usar la mediana
#       Sobrevivió   Falleció
# FALSE 0.91318384 0.08681616
# TRUE  0.92301017 0.07698983

# sodium_apache
prop.table(table(is.na(data_UCI$sodium_apache), data_UCI$hospital_death), margin = 1)
# tiene una menor tasa de mortalidad
#       Sobrevivió   Falleció
# FALSE 0.91193716 0.08806284
# TRUE  0.93004541 0.06995459

data_UCI_clean <- data_demo %>%
  filter(!is.na(hospital_death))%>% #elimina a 25 paciente sin datos de mortalidad ni de genero
  dplyr::select(hospital_death, #NA = 0
                age, #NA = 4216
                gender, #NA = 25
                bmi,
                bmi_cat,
                temp_apache,
                temp_cat,
                map_apache, 
                map_cat, 
                heart_rate_apache, 
                heart_rate_cat,
                resprate_apache,
                resprate_cat,
                elective_surgery, #NA = 0
                ethnicity, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
                bun_apache,
                arf_apache,
                glucose_apache,
                glucose_cat,
                hematocrit_apache,
                hematocrit_cat,
                wbc_apache,
                wbc_cat,
                sodium_apache,
                sodium_cat,
                gcs_eyes_apache,
                gcs_motor_apache,
                gcs_verbal_apache,
                ventilated_apache,
                aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
                cirrhosis, # NA = 715
                diabetes_mellitus, # NA = 715
                hepatic_failure, # NA = 715
                immunosuppression, # NA = 715
                leukemia, # NA = 715
                lymphoma, # NA = 715
                solid_tumor_with_metastasis) %>% # NA = 715
  filter(!is.na(aids))%>% #quito los 715 NA de: ventilated_apache aids cirrhosis diabetes_mellitus, hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
  filter(!is.na(gcs_motor_apache))%>% #quita los 1183 de glasgow que faltaba
  filter(!is.na(resprate_apache))%>% # se quitaron 448 que eran NA
  filter(!is.na(map_apache))%>% # se quitaron 131 por ser NA
  filter(!is.na(heart_rate_apache))%>% #se quitaron 13 por ser NA
  filter(!is.na(age))%>%
  filter(!is.na(bmi))%>%
  filter(!is.na(temp_apache))%>%
  filter(!is.na(bun_apache))%>%
  filter(!is.na(glucose_apache))%>%
  filter(!is.na(hematocrit_apache))%>%
  filter(!is.na(wbc_apache))%>%
  filter(!is.na(sodium_apache))%>%
  mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros

data_UCI_clean %>% summarise_all(funs(sum(is.na(.))))
dim(data_UCI_clean)
#58644    41

data_UCI_gas <- data_demo %>%
  filter(!is.na(hospital_death))%>% #elimina a 25 paciente sin datos de mortalidad ni de genero
  dplyr::select(hospital_death, #NA = 0
                age, #NA = 4216
                gender, #NA = 25
                bmi,
                #bmi_cat,
                temp_apache,
                #temp_cat,
                map_apache, 
                #map_cat, 
                heart_rate_apache, 
                #heart_rate_cat,
                resprate_apache,
                #resprate_cat,
                elective_surgery, #NA = 0
                ethnicity, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
                bun_apache,
                arf_apache,
                glucose_apache,
                #glucose_cat,
                hematocrit_apache,
                #hematocrit_cat,
                wbc_apache,
                #wbc_cat,
                sodium_apache,
                #sodium_cat,
                gcs_eyes_apache,
                gcs_motor_apache,
                gcs_verbal_apache,
                ventilated_apache,
                aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
                cirrhosis, # NA = 715
                diabetes_mellitus, # NA = 715
                hepatic_failure, # NA = 715
                immunosuppression, # NA = 715
                leukemia, # NA = 715
                lymphoma, # NA = 715
                solid_tumor_with_metastasis,
                creatinine_apache,
                bilirubin_apache,
                albumin_apache,
                intubated_apache,
                fio2_apache,
                paco2_apache,
                paco2_for_ph_apache,
                pao2_apache,
                ph_apache) %>% # NA = 715
  #filter(!is.na(aids))%>% #quito los 715 NA de: ventilated_apache aids cirrhosis diabetes_mellitus, hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
  #filter(!is.na(gcs_motor_apache))%>% #quita los 1183 de glasgow que faltaba
  #filter(!is.na(resprate_apache))%>% # se quitaron 448 que eran NA
  #filter(!is.na(map_apache))%>% # se quitaron 131 por ser NA
  #filter(!is.na(heart_rate_apache))%>% #se quitaron 13 por ser NA
  #filter(!is.na(age))%>%
  #filter(!is.na(bmi))%>%
  #filter(!is.na(temp_apache))%>%
  #filter(!is.na(bun_apache))%>%
  #filter(!is.na(glucose_apache))%>%
  #filter(!is.na(hematocrit_apache))%>%
  #filter(!is.na(wbc_apache))%>%
  #filter(!is.na(sodium_apache))%>%
  mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros

dim(data_UCI_gas)
#91688    41
#ver cunatos missing data hay en cada variable
data_UCI_gas %>% summarise_all(funs(sum(is.na(.))))



prop.table(table(is.na(data_UCI_gas$fio2_apache), data_UCI_gas$hospital_death), margin = 1)

methods(mice)

data_UCI_gas_clean <- data_demo %>%
  filter(!is.na(hospital_death))%>% #elimina a 25 paciente sin datos de mortalidad ni de genero
  dplyr::select(hospital_death, #NA = 0
                age, #NA = 4216
                gender, #NA = 25
                bmi,
                #bmi_cat,
                temp_apache,
                #temp_cat,
                map_apache, 
                #map_cat, 
                heart_rate_apache, 
                #heart_rate_cat,
                resprate_apache,
                #resprate_cat,
                elective_surgery, #NA = 0
                ethnicity, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
                bun_apache,
                arf_apache,
                glucose_apache,
                #glucose_cat,
                hematocrit_apache,
                #hematocrit_cat,
                wbc_apache,
                #wbc_cat,
                sodium_apache,
                #sodium_cat,
                gcs_eyes_apache,
                gcs_motor_apache,
                gcs_verbal_apache,
                ventilated_apache,
                aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
                cirrhosis, # NA = 715
                diabetes_mellitus, # NA = 715
                hepatic_failure, # NA = 715
                immunosuppression, # NA = 715
                leukemia, # NA = 715
                lymphoma, # NA = 715
                solid_tumor_with_metastasis,
                creatinine_apache,
                bilirubin_apache,
                albumin_apache,
                intubated_apache,
                fio2_apache,
                paco2_apache,
                paco2_for_ph_apache,
                pao2_apache,
                ph_apache) %>% # NA = 715
  filter(!is.na(aids))%>% #quito los 715 NA de: ventilated_apache aids cirrhosis diabetes_mellitus, hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
  filter(!is.na(gcs_motor_apache))%>% #quita los 1183 de glasgow que faltaba
  filter(!is.na(resprate_apache))%>% # se quitaron 448 que eran NA
  filter(!is.na(map_apache))%>% # se quitaron 131 por ser NA
  filter(!is.na(heart_rate_apache))%>% #se quitaron 13 por ser NA
  filter(!is.na(age))%>%
  filter(!is.na(bmi))%>%
  filter(!is.na(temp_apache))%>%
  filter(!is.na(bun_apache))%>%
  filter(!is.na(glucose_apache))%>%
  filter(!is.na(hematocrit_apache))%>%
  filter(!is.na(wbc_apache))%>%
  filter(!is.na(sodium_apache))%>%
  filter(!is.na(creatinine_apache))%>% #NA son 191, porque el comparte como 95% con bun
  filter(!is.na(bilirubin_apache))%>% # NA wa igual a 39203
  filter(!is.na(albumin_apache))%>% # NA sib 75 porque bilirrubina tiene eo 95% los otros NA
  filter(!is.na(fio2_apache))%>% # tiene 20096 NA, que son los mismos para todas las gasometrias
mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros

data_UCI_gas_clean %>% summarise_all(funs(sum(is.na(.))))
dim(data_UCI_gas_clean)
#8031   41

data_UCI_gas_cat_clean <- data_demo %>%
  filter(!is.na(hospital_death))%>% #elimina a 25 paciente sin datos de mortalidad ni de genero
  dplyr::select(hospital_death, #NA = 0
                age, #NA = 4216
                gender, #NA = 25
                bmi,
                bmi_cat,
                temp_apache,
                temp_cat,
                map_apache, 
                map_cat, 
                heart_rate_apache, 
                heart_rate_cat,
                resprate_apache,
                resprate_cat,
                elective_surgery, #NA = 0
                #ethnicity, #NA = 0
                #hospital_admit_source, #NA = 0
                icu_admit_source,#NA = 0
                #icu_stay_type,#NA = 0
                pre_icu_los_days,#NA = 0
                icu_type,#NA = 0
                bun_apache,
                arf_apache,
                creatinine_apache,
                glucose_apache,
                glucose_cat,
                hematocrit_apache,
                hematocrit_cat,
                wbc_apache,
                wbc_cat,
                sodium_apache,
                sodium_cat,
                gcs_eyes_apache,
                gcs_motor_apache,
                gcs_verbal_apache,
                aids, # NA = 715, estos no tienen examenes de lab, ni signos vitales, para este analisis se los transforma a todos en cero, de este grupo 630 sobrevivieron y 85 falecieron
                cirrhosis, # NA = 715
                diabetes_mellitus, # NA = 715
                hepatic_failure, # NA = 715
                immunosuppression, # NA = 715
                leukemia, # NA = 715
                lymphoma, # NA = 715
                solid_tumor_with_metastasis,
                bilirubin_apache,
                albumin_apache,
                albumin_cat,
                ventilated_apache,
                intubated_apache,
                fio2_apache,
                paco2_apache,
                paco2_cat,
                pao2_apache,
                pao2_cat,
                ph_apache,
                ph_cat) %>% # NA = 715
  filter(!is.na(aids))%>% #quito los 715 NA de: ventilated_apache aids cirrhosis diabetes_mellitus, hepatic_failure immunosuppression leukemia lymphoma solid_tumor_with_metastasis
  filter(!is.na(gcs_motor_apache))%>% #quita los 1183 de glasgow que faltaba
  filter(!is.na(resprate_apache))%>% # se quitaron 448 que eran NA
  filter(!is.na(map_apache))%>% # se quitaron 131 por ser NA
  filter(!is.na(heart_rate_apache))%>% #se quitaron 13 por ser NA
  filter(!is.na(age))%>%
  filter(!is.na(bmi))%>%
  filter(!is.na(temp_apache))%>%
  filter(!is.na(bun_apache))%>%
  filter(!is.na(glucose_apache))%>%
  filter(!is.na(hematocrit_apache))%>%
  filter(!is.na(wbc_apache))%>%
  filter(!is.na(sodium_apache))%>%
  filter(!is.na(creatinine_apache))%>% #NA son 191, porque el comparte como 95% con bun
  filter(!is.na(bilirubin_apache))%>% # NA wa igual a 39203
  filter(!is.na(albumin_apache))%>% # NA sib 75 porque bilirrubina tiene eo 95% los otros NA
  filter(!is.na(fio2_apache))%>% # tiene 20096 NA, que son los mismos para todas las gasometrias
  mutate_if(is.character, factor) #%>%
#mutate_if(is.logical, integer) #los logicos los tranforma en numeros enteros

data_UCI_gas_cat_clean %>% summarise_all(funs(sum(is.na(.))))
dim(data_UCI_gas_cat_clean)
# 8031   54