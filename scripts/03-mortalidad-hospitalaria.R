library(psych)
#Existe diferencia en incidencia de mortalidad
# en diferentes UCI

#Test

#Crear data frame de la tabla con la info de hopsital_id y mortalidad
#opcion una para crear mortalidad: Mortalidad_hospitalaria <- as.data.frame.matrix(table(fpe$hospital_id, fpe$hospital_death))

Mortalidad_hospitalaria <- fpe %>% 
  group_by(hospital_id, hospital_death) %>% 
  summarise(Cant_sobrevivio = n()) %>%
  pivot_wider(names_from = hospital_death, 
              values_from= Cant_sobrevivio)


# Crear data frame con apacue_4a_icu_death_prob para ver riesgo de mortalidad
Apache_mortalidad <- fpe %>% 
  group_by(hospital_id) %>% 
  summarise(Mediana_Apache_4a = median(apache_4a_icu_death_prob, na.rm = TRUE))


# aumentar la columna con la incidencia de mortalidad en porcentaje
Mortalidad_hospitalaria <- round(mutate(Mortalidad_hospitalaria, 
                                        porcentaje_mortalidad = (Falleció * 100)/(Falleció + Sobrevivió)), 
                                 digits = 2)


# Unir mortalidad hospitalaria y apache_mortalidad

Mortalidad_hospitalaria_apache_4a <- merge(x =  Mortalidad_hospitalaria, y = Apache_mortalidad, by.x = "hospital_id", by.y = "hospital_id")



# Se eliminan a los UCI que atendieron menos de 25 pacientes
# se los elimina porque muchos de estas UCI tenian no teneian mortalidad (porque eran pocos pacientes)
# mientras que otros veian dos pacientes, uno fallecia y ya tenian mortalidad del 50%
Mortalidad_hospitalaria_apache_4a <-filter(Mortalidad_hospitalaria_apache_4a, Sobrevivió >= 25)


#xiste correlacion entre la probabilidad de fallcer y el reisgo de fallecer medido por APACHE 4a en los diferentes hospitales
  # si existe esta correlacion y es de un r= 68
pairs.panels(Mortalidad_hospitalaria_apache_4a)

plot(Mortalidad_hospitalaria_apache_4a$porcentaje_mortalidad, Mortalidad_hospitalaria_apache_4a$Mediana_Apache_4a)

#Se crea grafico de densidad para encontrar la Bimodalidad en la curva
# Algunas UCI tienen mortalidad de un 6.5% 
# mientras que otras tienen mortalidad del 11.5%
ggplot(Mortalidad_hospitalaria_apache_4a)+ 
  aes(porcentaje_mortalidad)+
  geom_density()