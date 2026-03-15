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
library(tidymodels) # Facilita modelamiento - new way
library(glmnet) # Regularizacion L2/L1/elastic net models\
library(glmnetUtils)
library(earth) # MARS
library(discrim) # Naive Bayes desde tidymodels
library(klaR) # Naive Bayes
library(naivebayes) # Naive Bayes
library(vip) # for variable importance plots
library(doParallel) # Computacion en paralelo
library(themis) #para balancear datos con up
library(gamlss)
library(mgcv)
library(tune)
library(parsnip) #para random forest
library(ranger) #para hacer set_engine de ranger
library(knitr) # con la funcion kablre permite hacer tablas hacerlas mas bonitas en la consola
library(lightgbm) #Light Gradient Boosting Machine
library(tidypredict) # transforma el modelo de tidyverse a un objeto que pueda ser copiado
library(yaml) #copia el modelo previamente trasformado por tidypredict

#se divide un prueba y test
set.seed(123)
UCI_noCat_split <-initial_split(data_UCI_noCat, strata = hospital_death)
UCI_noCat_training <- training(UCI_noCat_split)
UCI_noCat_test <- testing(UCI_noCat_split)

#Tamaño de la muestra del entrenamiento
dim(UCI_noCat_training)
#66894    36

# se crean las carpetas
set.seed(123)
UCI_noCat_folds <- vfold_cv(UCI_noCat_training)

# para renombrar a las varibles dommy
# nodash_names <- function(var, lvl, ordinal) {
#   dummy_names(var = var, lvl = lvl, ordinal = ordinal, sep = "_")
# }

#preprocesamiento de los datos, se crea el recipiente
UCI_noCat_rec <- recipe(hospital_death ~ . , data = UCI_noCat_training) %>%
  step_meanimpute(map_apache, heart_rate_apache, resprate_apache, sodium_apache, urineoutput_apache, hematocrit_apache) %>%
  step_medianimpute(age,bmi, pre_icu_los_days, temp_apache, bun_apache, creatinine_apache, wbc_apache,
                    albumin_apache, bilirubin_apache, glucose_apache) %>%
  step_modeimpute(gender, ethnicity, elective_surgery, icu_admit_source, icu_stay_type, icu_type, arf_apache, gcs_eyes_apache, gcs_motor_apache, gcs_verbal_apache, 
                  ventilated_apache, aids, cirrhosis, diabetes_mellitus, hepatic_failure, immunosuppression, leukemia, lymphoma, solid_tumor_with_metastasis) %>%
  step_mutate(temp_cat = temp_apache) %>%
  step_cut(temp_cat, breaks = c(-Inf, 36.5, Inf)) %>%
  step_mutate(bmi_cat = bmi) %>%
  step_cut(bmi_cat, breaks = c(-Inf, 18.5, 30, Inf)) %>%
  step_mutate(map_cat = map_apache) %>%
  step_cut(map_cat, breaks = c(-Inf, 95, Inf)) %>%
  step_mutate(heart_rate_cat = heart_rate_apache) %>%
  step_cut(heart_rate_cat, breaks = c(-Inf, 50, 100, Inf)) %>%
  step_mutate(resprate_cat = resprate_apache) %>%
  step_cut(resprate_cat, breaks = c(-Inf, 10, 35, Inf)) %>%
  step_mutate(hematocrit_cat = hematocrit_apache) %>%
  step_cut(hematocrit_cat, breaks = c(-Inf, 40, Inf)) %>%
  step_mutate(wbc_cat = wbc_apache) %>%
  step_cut(wbc_cat, breaks = c(-Inf, 6, Inf)) %>%
  step_mutate(sodium_cat = sodium_apache) %>%
  step_cut(sodium_cat, breaks = c(-Inf, 140, Inf)) %>%
  step_mutate(glucose_cat = glucose_apache) %>%
  step_cut(glucose_cat, breaks = c(-Inf, 90, Inf)) %>%
  step_mutate(albumin_cat = albumin_apache) %>%
  step_cut(albumin_cat, breaks = c(-Inf, 3.3, Inf)) %>%
  step_mutate(bun_cat = bun_apache) %>%
  step_cut(bun_cat, breaks = c(-Inf, 40, Inf)) %>%
  step_mutate(creatinine_cat = creatinine_apache) %>%
  step_cut(creatinine_cat, breaks = c(-Inf, 3, Inf)) %>%
  step_mutate(urineoutput_cat = urineoutput_apache) %>%
  step_cut(urineoutput_cat, breaks = c(-Inf, 1400, Inf)) %>%
  #step_medianimpute(all_numeric(), -all_outcomes()) %>% #imputar todos los NA con la mediana de los datos
  step_normalize(all_numeric(), -all_outcomes()) %>% # Normalizacion
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% #crea variables dymmy para las categoricas
  step_other(all_nominal(), -all_outcomes() ) %>% #tienen menos de un 5%
  step_interact(terms = ~ bun_apache:arf_apache_Si) %>% 
  step_interact(terms = ~ bmi:(bmi_cat_X..Inf.18.5.+ bmi_cat_X.18.5.30.)) %>% #solo el bajo peso y peso normal influyen
  step_interact(terms = ~ temp_apache:temp_cat_X..Inf.36.5.) %>% 
  step_interact(terms = ~ map_apache:map_cat_X..Inf.95.) %>% 
  step_interact(terms = ~ heart_rate_apache:(heart_rate_cat_X..Inf.50. + heart_rate_cat_X.50.100.)) %>% #heart_rate_cat_2 parece que no interactua
  step_interact(terms = ~ resprate_apache:(resprate_cat_X..Inf.10. + resprate_cat_X.10.35.)) %>% 
  step_interact(terms = ~ hematocrit_apache:hematocrit_cat_X..Inf.40.) %>% #interaccion entre variables
  step_interact(terms = ~ wbc_apache:wbc_cat_X..Inf.6.) %>% 
  step_interact(terms = ~ sodium_apache:sodium_cat_X..Inf.140.) %>% 
  step_interact(terms = ~ glucose_apache:glucose_cat_X..Inf.90.) %>% #no hay interaccion
  step_interact(terms = ~ albumin_apache:albumin_cat_X..Inf.3.3.) %>% 
  step_interact(terms = ~ bun_apache:bun_cat_X..Inf.40.) %>% 
  step_interact(terms = ~ creatinine_apache:creatinine_cat_X..Inf.3.) %>% 
  step_interact(terms = ~ urineoutput_apache:urineoutput_cat_X..Inf.1.4e.03.) %>% 
  step_upsample(hospital_death)  #Se usa upsalmple porque le va una centesima mejor que a los downsalple


#step_knnimpute(all_predictors(), neighbors = 3) %>%
#step_bagimpute(all_nominal(), -all_outcomes()) %>%
#step_dummy_hcai(all_nominal(), -all_outcomes()) %>% 
#step_unknown(all_nominal(), -all_outcomes()) %>%
#step_dummy(all_nominal(), -all_outcomes()) %>%
# step_mutate(map_apache_hipo_pol2 = map_apache_hipo^2) %>% #hacer esta variable polinomio de 2
# step_mutate(heart_rate_apache_hipo_pol2 = heart_rate_apache_hipo^2) %>% 
# step_mutate(resprate_hipo_pol2 = resprate_apache_hipo^2) %>% 
#step_interact(terms = ~ temp_apache:map_apache) %>% #parece ser que no hay interaccion
#step_interact(terms = ~ diabetes_mellitus:glucose_cat_1) %>%  no hay interaccion
#step_interact(terms = ~ bmi:(bmi_cat_1 + bmi_cat_2 + bmi_cat_3 + bmi_cat_4 + bmi_cat_5)) %>% #la que importa es la los que estan desnutridos
#step_interact(terms = ~ glucose_apache:glucose_cat_1) %>% #no hay interaccion
#pendiente calcular interaccion entre frecuencia cardiaca y Presion arterial



# se crea Workflow

UCI_noCat_wf <- workflow() %>%
  add_recipe(UCI_noCat_rec)


#perparar la receta
UCI_noCat_rec %>% #cogemos el recipiente
  prep() %>% # se prepara la receta, sirve para ejecutar lo que dice la receta
  bake(new_data= NULL) %>% # sirve para observar lo que se preparo, y se escoge si es con nueva data o con la data de la receta
  count(hospital_death) #se cuenta la cantidad muertes con los datos sinteticos (tanto el sample up de kmean y imputacion de datos con mediana)

# Modelos que se usaran
UCI_noCat_lg <- logistic_reg()%>%
  set_engine("glm")  %>%
  set_mode("classification")

UCI_noCat_lgnet <-
  logistic_reg() %>%
  set_engine("glmnet", family = "binomial") %>%
  set_mode("classification")

UCI_noCat_rf <- rand_forest(trees = 1000) %>%
  set_engine('ranger') %>%
  set_mode("classification")

UCI_noCat_lgbm <- boost_tree(trees = 1000) %>%
  set_engine("lightgbm") %>%
  set_mode("classification")

# Usanedo MARS
UCI_noCat_mars <-
  mars() %>%
  set_engine("earth") %>%
  set_mode("classification")

#usando 
UCI_noCat_nbay <-
  naive_Bayes() %>%
  set_engine("klaR") %>%
  set_mode("classification")


# unir el workflow al modelo
doParallel::registerDoParallel()

UCI_noCat_lg_res <- UCI_noCat_wf %>%
  add_model(UCI_noCat_lg) %>%
  fit_resamples(
    resamples = UCI_noCat_folds,
    metrics = metric_set(roc_auc, accuracy, sensitivity, specificity, bal_accuracy),
    control = control_resamples(save_pred = TRUE)
  )
# es el mas rapido, y tiene un buen balance
# .metric      .estimator  mean     n std_err .config             
# <chr>        <chr>      <dbl> <int>   <dbl> <chr>               
# 1 accuracy     binary     0.794    10 0.00203 Preprocessor1_Model1
# 2 bal_accuracy binary     0.785    10 0.00399 Preprocessor1_Model1
# 3 roc_auc      binary     0.868    10 0.00313 Preprocessor1_Model1
# 4 sens         binary     0.796    10 0.00203 Preprocessor1_Model1
# 5 spec         binary     0.774    10 0.00763 Preprocessor1_Model1

#ElasticNet
UCI_lgnet_res <- UCI_wf %>%
  add_model(UCI_lgnet) %>%
  fit_resamples(
    resamples = UCI_folds,
    metrics = metric_set(roc_auc, accuracy, sensitivity, specificity, bal_accuracy),
    control = control_resamples(save_pred = TRUE)
  )

#Random forest
UCI_rf_res <- UCI_wf %>%
  add_model(UCI_lgbm ) %>%
  fit_resamples(
    resamples = UCI_folds,
    metrics = metric_set(roc_auc, accuracy, sensitivity, specificity, bal_accuracy),
    control = control_resamples(save_pred = TRUE)
  )

#light gbm
UCI_lgbm_res <- UCI_wf %>%
  add_model(UCI_rf) %>%
  fit_resamples(
    resamples = UCI_folds,
    metrics = metric_set(roc_auc, accuracy, sensitivity, specificity, bal_accuracy),
    control = control_resamples(save_pred = TRUE)
  )
# este analisis tardo casi una hora en realizarse
# el lgbm no balancea la data y da falsos valore de accuraci
# .metric      .estimator  mean     n  std_err .config             
# <chr>        <chr>      <dbl> <int>    <dbl> <chr>               
# 1 accuracy     binary     0.919    10 0.000674 Preprocessor1_Model1
# 2 bal_accuracy binary     0.600    10 0.00177  Preprocessor1_Model1
# 3 roc_auc      binary     0.859    10 0.00266  Preprocessor1_Model1
# 4 sens         binary     0.986    10 0.000355 Preprocessor1_Model1
# 5 spec         binary     0.214    10 0.00357  Preprocessor1_Model1

#Mars
UCI_noCat_mars_res <- UCI_noCat_wf %>%
  add_model(UCI_noCat_mars) %>%
  fit_resamples(
    resamples = UCI_noCat_folds,
    metrics = metric_set(roc_auc, accuracy, sensitivity, specificity, bal_accuracy),
    control = control_resamples(save_pred = TRUE)
  )
# este fue rapido y dio un vuen resultado
# .metric      .estimator  mean     n std_err .config             
# <chr>        <chr>      <dbl> <int>   <dbl> <chr>               
#   1 accuracy     binary     0.772    10 0.00164 Preprocessor1_Model1
# 2 bal_accuracy binary     0.768    10 0.00339 Preprocessor1_Model1
# 3 roc_auc      binary     0.850    10 0.00261 Preprocessor1_Model1
# 4 sens         binary     0.773    10 0.00162 Preprocessor1_Model1
# 5 spec         binary     0.762    10 0.00642 Preprocessor1_Model1

#Bayes
UCI_nbay_res <- UCI_wf %>%
  add_model(UCI_nbay) %>%
  fit_resamples(
    resamples = UCI_folds,
    metrics = metric_set(roc_auc, accuracy, sensitivity, specificity, bal_accuracy),
    control = control_resamples(save_pred = TRUE)
  )
# al modelo bayes tambien le fue mal
# .metric      .estimator  mean     n std_err .config             
# <chr>        <chr>      <dbl> <int>   <dbl> <chr>               
# 1 accuracy     binary     0.901    10 0.00113 Preprocessor1_Model1
# 2 bal_accuracy binary     0.668    10 0.00269 Preprocessor1_Model1
# 3 roc_auc      binary     0.829    10 0.00274 Preprocessor1_Model1
# 4 sens         binary     0.950    10 0.00133 Preprocessor1_Model1
# 5 spec         binary     0.386    10 0.00556 Preprocessor1_Model1

# para ver como les fue en las metricas
collect_metrics(UCI_noCat_lg_res)
collect_metrics(UCI_lgnet_res)
collect_metrics(UCI_rf_res)
collect_metrics(UCI_lgbm_res)
collect_metrics(UCI_noCat_mars_res)
collect_metrics(UCI_nbay_res)

# hacer tabla de confusion para ver verdaderos positivos y falos positivos
UCI_noCat_lg_res %>% 
  conf_mat_resampled()
# Prediction Truth       Freq
# <fct>      <fct>      <dbl>
#   1 Sobrevivió Sobrevivió 4849.
# 2 Sobrevivió Falleció    130.
# 3 Falleció   Sobrevivió 1279.
# 4 Falleció   Falleció    432 

#tabla de contigencia
UCI_noCat_lg_res %>% 
  conf_mat_resampled(tidy = FALSE) %>%
  mosaicplot()
# Truth       Prediction Prediction
#             Sobrevivió Falleció
# Sobrevivió     4848.9   1278.9
# Falleció        130.1    432.0

UCI_rf_res %>% 
  conf_mat_resampled(tidy = FALSE) %>%
  mosaicplot()

UCI_lgbm_res %>% 
  conf_mat_resampled(tidy = FALSE) %>%
  mosaicplot()

UCI_mars_res %>% 
  conf_mat_resampled(tidy = FALSE) %>%
  mosaicplot()

UCI_nbay_res %>% 
  conf_mat_resampled(tidy = FALSE) %>%
  mosaicplot()

#hacer curva roc
UCI_noCat_lg_res %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(hospital_death, `.pred_Sobrevivió`)%>% #revisar en UCI_lg_res$.predictions la forma en que sale
  autoplot()

UCI_lgbm_res %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(hospital_death, `.pred_Sobrevivi?`)%>% #revisar en UCI_lg_res$.predictions la forma en que sale
  autoplot()



#modelo final, entrenamiento y test
#modelo regresion logistica
UCI_noCat_lg_final <- UCI_noCat_wf %>%
  add_model(UCI_noCat_lg) %>%
  last_fit(UCI_noCat_split)

UCI_noCat_lg_final %>% 
  conf_mat_resampled()

UCI_noCat_lg_fit <- UCI_noCat_wf %>%
  add_model(UCI_noCat_lg) %>%
  fit(UCI_noCat_training)

# Copiar el modelo

parsed_UCI_noCat_lg_fit <- parse_model(UCI_noCat_lg_fit$fit$fit)

#otros modelos
UCI_rf_final <- UCI_wf %>%
  add_model(UCI_rf) %>%
  last_fit(UCI_split)

UCI_lgbm_final <- UCI_wf %>%
  add_model(UCI_lgbm ) %>%
  last_fit(UCI_split)

collect_metrics(UCI_noCat_lg_final)
collect_metrics(UCI_noCat_lg_fit)
UCI_noCat_test %>%
  predict(UCI_noCat_lg_fit, new_data = . ) %>%
  mutate(Real= UCI_noCat_test$hospital_death) %>%
  conf_mat(truth = Real, estimate = .pred_class ) %>%
  summary

collect_metrics(UCI_rf_final)
collect_metrics(UCI_lgbm_final)

collect_predictions(UCI_lg_final) %>%
  conf_mat(hospital_death, .pred_class)

collect_predictions(UCI_rf_final) %>%
  conf_mat(hospital_death, .pred_class)

#ver el beta
UCI_noCat_lg_final %>%
  pull(.workflow) %>% #trae el workflow del modelo UCI_lg_final
  pluck(1) %>% #para darle estructura a los datos que se traen
  tidy(exponentiate =TRUE) %>% #al expoenciar el estimate (beta) en una regresion, se obtiene el odds ratio
  arrange(desc(estimate)) %>% #ponerlo en orden de impacto del estimador
  kable(digits=3) # le da formato a la tabla, y digits para ver cuantos decimales se desea


UCI_noCat_lg_final %>%
  pull(.workflow) %>% #trae el workflow del modelo UCI_lg_final
  pluck(1) %>% #para darle estructura a los datos que se traen
  tidy() %>%
  filter(term != "hospital_admit_source_Observation", #
         term != "hospital_admit_source_Other",
         term != "icu_admit_source_Other.ICU", #one hot variable
         term != "icu_stay_type_transfer", #one hot variable
         term != "icu_type_SICU", #one hot variable
         term != "(Intercept)", #se quita el interseptor
         term != "gender_M", #one hot variable
         term != "solid_tumor_with_metastasis_Si", #one hot variable
         term != "aids_Si", #one hot variable
         term != "cirrhosis_Si", #one hot variable
         term != "diabetes_mellitus_Si", #one hot variable
         term != "hepatic_failure_Si", #one hot variable
         term != "immunosuppression_Si",#one hot variable
         term != "leukemia_Si", #one hot variable
         term != "lymphoma_Si", #one hot variable
         term != "gcs_eyes_apache_4", #one hot variable
         term != "gcs_motor_apache_6", #one hot variable
         term != "gcs_verbal_apache_5", #one hot variable
         term != "bmi_cat_6", #one hot variable
         term != "temp_cat_2", #one hot variable
         term != "map_cat_2", #one hot variable
         term != "heart_rate_cat_3", #one hot variable
         term != "resprate_cat_3", #one hot variable
         term != "elective_surgery_Si", #one hot variable
         term != "ethnicity_Other.Unknown", #one hot variable
         term != "glucose_cat_2", #one hot variable
         term != "hematocrit_cat_2", #one hot variable
         term != "wbc_cat_2", #one hot variable
         term != "sodium_cat_2", #one hot variable
         term != "ventilated_apache_Si", #one hot variable
         term != "arf_apache_Si", #one hot variable
         term != "icu_admit_source_Other.Hospital", #no hay asociacion estadistica
         term != "icu_admit_source_X", #no hay asociacion estadistica
         term != "icu_admit_source_Floor",
         term != "icu_admit_source_Floor",
         term != "icu_type_Cardiac.ICU",
         term != "icu_type_CCU.CTICU",
         term != "icu_type_CTICU",
         term != "icu_stay_type_readmit",
         term != "ethnicity_Native.American",
         term != "ethnicity_Caucasian",
         term != "ethnicity_X",
         term != "ethnicity_African.American",
         term != "ethnicity_Asian", 
         term != "bmi_cat_2",
         term != "bmi_cat_4",
         term != "bmi_cat_5",
         term != "bmi_cat_3",
         term != "gcs_verbal_apache_3",
         term != "gcs_verbal_apache_3",
         term != "hematocrit_cat_1", 
         term != "resprate_cat_2", 
  ) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate)))+
  geom_vline(xintercept = 0, color= "gray50", lty= 2, size =1)+
  geom_point() +
  geom_errorbar(aes(xmin = estimate - 2*std.error,
                    xmax = estimate + 2*std.error),
                with = 0.2, alpha = 0.7) 

#gráfico de variables demográficas
UCI_lg_final %>%
  pull(.workflow) %>% #trae el workflow del modelo UCI_lg_final
  pluck(1) %>% #para darle estructura a los datos que se traen
  tidy() %>%
  filter(term == "age" |
           term == "gender_F" | 
           term == "pre_icu_los_days" |
           term == "elective_surgery_No" |
           term == "ethnicity_Hispanic" |
           term == "ethnicity_Native.American" |
           term == "ethnicity_Caucasian" |
           term == "ethnicity_X" |
           term == "icu_admit_source_Other.Hospital" |
           term == "icu_type_Neuro.ICU" |
           term == "bmi_cat_2" |
           term == "bmi" |
           term == "bmi_x_bmi_cat_2" |
           term == "bmi_cat_1" |
           term == "bmi_x_bmi_cat_1" 
  ) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate)))+
  geom_vline(xintercept = 0, color= "gray50", lty= 2, size =1)+
  geom_point() +
  geom_errorbar(aes(xmin = estimate - 2*std.error,
                    xmax = estimate + 2*std.error),
                with = 0.2, alpha = 0.7) 

#gráfico de variables cardiacas
UCI_lg_final %>%
  pull(.workflow) %>% #trae el workflow del modelo UCI_lg_final
  pluck(1) %>% #para darle estructura a los datos que se traen
  tidy() %>%
  filter(term == "heart_rate_apache" |
           term == "map_apache" | 
           term == "heart_rate_cat_2" |
           term == "map_cat_1" |
           term == "map_apache_x_map_cat_1" |
           term == "heart_rate_apache_x_heart_rate_cat_1" |
           term == "heart_rate_cat_1" 
  ) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate)))+
  geom_vline(xintercept = 0, color= "gray50", lty= 2, size =1)+
  geom_point() +
  geom_errorbar(aes(xmin = estimate - 2*std.error,
                    xmax = estimate + 2*std.error),
                with = 0.2, alpha = 0.7) 

#gráfico de variables neurologicas
UCI_lg_final %>%
  pull(.workflow) %>% #trae el workflow del modelo UCI_lg_final
  pluck(1) %>% #para darle estructura a los datos que se traen
  tidy() %>%
  filter(term == "gcs_motor_apache_1" |
           term == "gcs_motor_apache_2" | 
           term == "gcs_motor_apache_3" |
           term == "gcs_motor_apache_4" |
           term == "gcs_motor_apache_5" |
           term == "ggcs_eyes_apache_1" |
           term == "gcs_eyes_apache_2" |
           term == "gcs_eyes_apache_3" |
           term == "gcs_verbal_apache_1" |
           term == "gcs_verbal_apache_2" |
           term == "gcs_verbal_apache_3" |
           term == "gcs_verbal_apache_4" 
  ) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate)))+
  geom_vline(xintercept = 0, color= "gray50", lty= 2, size =1)+
  geom_point() +
  geom_errorbar(aes(xmin = estimate - 2*std.error,
                    xmax = estimate + 2*std.error),
                with = 0.2, alpha = 0.7)   

#gráfico de variables metabólicas (temperatura, hematocrito, glucosa, globulos blancos)
UCI_lg_final %>%
  pull(.workflow) %>% #trae el workflow del modelo UCI_lg_final
  pluck(1) %>% #para darle estructura a los datos que se traen
  tidy() %>%
  filter(term == "temp_apache" |
           term == "temp_cat_1" |
           term == "temp_apache_x_temp_cat_1" |
           term == "wbc_apache" |
           term == "wbc_cat_1" |
           term == "wbc_apache_x_wbc_cat_1" |
           term == "hematocrit_apache" | 
           term == "hematocrit_cat_1" |
           term == "hematocrit_apache_x_hematocrit_cat_1" |
           term == "glucose_apache" |
           term == "glucose_cat_1" |
           term == "glucose_apache_x_glucose_cat_1" 
  ) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate)))+
  geom_vline(xintercept = 0, color= "gray50", lty= 2, size =1)+
  geom_point() +
  geom_errorbar(aes(xmin = estimate - 2*std.error,
                    xmax = estimate + 2*std.error),
                with = 0.2, alpha = 0.7) 

#gráfico de variables renal y hepatica
UCI_lg_final %>%
  pull(.workflow) %>% #trae el workflow del modelo UCI_lg_final
  pluck(1) %>% #para darle estructura a los datos que se traen
  tidy() %>%
  filter(term == "bun_apache" |
           term == "sodium_cat_1" | 
           term == "sodium_apache" |
           term == "sodium_apache_x_sodium_cat_1" |
           term == "bun_apache_x_arf_apache_Si" |
           term == "bilirubin_apache" |
           term == "albumin_apache" 
  ) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate)))+
  geom_vline(xintercept = 0, color= "gray50", lty= 2, size =1)+
  geom_point() +
  geom_errorbar(aes(xmin = estimate - 2*std.error,
                    xmax = estimate + 2*std.error),
                with = 0.2, alpha = 0.7) 

#gráfico de variables Comorbilidades
UCI_lg_final %>%
  pull(.workflow) %>% #trae el workflow del modelo UCI_lg_final
  pluck(1) %>% #para darle estructura a los datos que se traen
  tidy() %>%
  filter(term == "arf_apache_No " | #falla renal
           term == "ventilated_apache_No" | #necesidad de ventilador
           term == "aids_No" |
           term == "solid_tumor_with_metastasis_No" |
           term == "immunosuppression_No" |
           term == "cirrhosis_No" |
           term == "hepatic_failure_No" |
           term == "leukemia_No" |
           term == "lymphoma_No" |
           term == "diabetes_mellitus_No" 
  ) %>%
  ggplot(aes(estimate, fct_reorder(term, estimate)))+
  geom_vline(xintercept = 0, color= "gray50", lty= 2, size =1)+
  geom_point() +
  geom_errorbar(aes(xmin = estimate - 2*std.error,
                    xmax = estimate + 2*std.error),
                with = 0.2, alpha = 0.7) 

#remuestreo para escoger hiperparametros
set.seed(1234)
cv_data_demo <- vfold_cv(data_demo, v = 10, repeats = 1, strata = hospital_death)
cv_data_demo

metricas <- metric_set(roc_auc, accuracy, sens, spec, bal_accuracy)



#preparar recipiente
rct_edad <- data_demo %>% recipe(hospital_death ~ age ) %>%
  step_normalize( all_numeric(), -all_outcomes()) %>% # Normalizacion
  step_other(all_nominal(), -all_outcomes() ) %>% #tienen menos de un 5%
  step_dummy(all_nominal(), -all_outcomes() ) %>% # Dummy
  step_nzv(all_predictors()) %>%
  themis::step_upsample(hospital_death, over_ratio = 0.9, skip= TRUE, seed= 123)

##  
table(data_demo$hospital_death)
recetaoreoarada <- prep(rct_edad, training = data_demo)
trainpreprocesado <- bake(recetaoreoarada, new_data = NULL)
table(trainpreprocesado$hospital_death)

# regresion logistica -----------------------------------------------------
#especificacion del modelo
reglog_edad <-
  logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") #No Necesario

# Creación del workflow 
reglog_wflow_edad <-
  workflow() %>%
  add_recipe(rct_edad) %>%
  add_model(reglog_edad)

# regresión logística podemos pasar a ajustar directo
reglog_fitted <- fit(reglog_wflow_edad, data = data_demo)


reglog_fitted %>% tidy


data_demo %>%
  predict(reglog_fitted, new_data = . ) %>%
  mutate(Real= data_demo$hospital_death) %>%
  conf_mat(truth = Real, estimate = .pred_class ) %>%
  summary


# Elastik Net -------------------------------------------------------------

#Remuestreo con vfold
set.seed(1234)
cv_edad <- vfold_cv(data_demo, v = 5, repeats = 1, strata = hospital_death)
cv_edad

#metricas
metricas <- metric_set(roc_auc, accuracy, sens, spec, bal_accuracy)

#modelo ElasticNet
eNet_edad <-
  logistic_reg(penalty = tune(), mixture = tune() ) %>%
  set_engine("glmnet", family = "binomial") %>%
  set_mode("classification")

# crea la malla
set.seed(123)
eNet_edad_grid <- enet_edad %>%
  parameters() %>%
  grid_latin_hypercube(size = 10)

# ElasticNet workflow
eNet_wflow_edad <-
  workflow() %>%
  add_recipe(rct_edad) %>%
  add_model(eNet_edad)

# integra workflow, remuestreo, parámetros a probar y métricas.
set.seed(123)
enet_tuned <- tune_grid(
  object =  eNet_wflow_edad,
  resamples= cv_edad,
  grid = eNet_edad_grid,
  metrics = metricas, 
  control = control_grid(allow_par = T)
)
