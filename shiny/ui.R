# El objeto de interfaz de usuario (ui) 
# controla el diseño y la apariencia de la aplicación.

shinyUI(navbarPage(theme = shinytheme("cerulean"), #cerulean es el Fondo de la pantalla
  # titulo de la App  ----
  "ARQUIMEDEZ",
  
  # Diseño de la barra lateral con definiciones de entrada
  # y salida ----
  tabPanel( "Predicción de mortalidad",
  sidebarLayout(
    # Panel de la barra lateral para entradas ----
    sidebarPanel(h4("Equipo de científicos de datos"),
                 
                 img(src='https://553801.smushcdn.com/1938437/wp-content/uploads/2020/08/Jaime_Soria_Viteri-400x-300x400.jpg?lossy=1&strip=1&webp=1', height = 100, width = 75),
                 p("Dr. Jaime Soria"),
                 img(src='https://pbs.twimg.com/profile_images/1010320061192552450/Kw7s7nTu_400x400.jpg', height = 100, width = 75),
                 p("Ing. Yalbi Balderas"),
                 img(src='', height = 100, width = 75),
                 p("Ing. Belen Escola"),
                 hr(),
      
      # Entrada: deslizador para el número de entradas ----
      # Entrada: Seleccionador para elegir un elemento
      # de la base de datos ----
     
    ),
    
    # Panel principal para mostrar resultados ----
    mainPanel(
      # resultado: resumen de los datos ----
      h2("Bienvenido al oráculo de la SEE"),
      p("Cuando un paciente es trasladado a la Unidad de Cuidados Intensivos (UCI) 
      es porque su salud podría deteriorarse rápidamente. Sin embargo, podemos usar sus datos de demográficos, signos vitales, 
      resultados de laboratorio y comorbilidades para predecir su probabilidad de mortalidad."),
      br(),
      p("Con esto en mente, se usó la base de datos del Datathon 2020 disponible en Kaggle, y se desarrollo una applicación 
      que ayude a los médicos a predecir el riesgo de mortalidad de cada paciente. De esta forma se puedan anticipar a 
      las amenazas y pueda proporcionar personalizadamente las formas más avanzadas de cuidados posibles para disminuir 
      su mortalidad."),
      
      br(),

# Modelamiento ------------------------------------------------------------
fluidRow(h2("Predicción de mortalidad:"), 
         tableOutput("disptPlot"),
         # h4("Falla neurológica:"),
         # textOutput("glasgow_pred"),
         
         h2("Seleccione las caracteristicas del paciente"),
         h3("Características demográficas"),
         column(2,
                radioButtons("gender", label = h4("Sexo"),
                             choices = list("Femenino" = "F", "Masculino" = "M"),
                             selected = "F")),
         
         column(2,
                numericInput("age",
                             label = h4("Edad"),
                             value = 65)),
         
         # column(2,
         #        numericInput("weight",
         #                     label = h4("Ingrese peso en Kg"),
         #                     value = 75)),
         # 
         # column(2,
         #        numericInput("height",
         #                     label = h4("Ingrese talla en m"),
         #                     value = 175)),
         
         column(2,
                numericInput("bmi",
                             label = h4("Indice de Masa Corporal"),
                             value = 27)),
         
         column(2,
                radioButtons("ethnicity", label = h4("Escoja etnia del paciente:"),
                             choices = list("Hispano" = "Hispanic",
                                            "Caucasico" = "Caucasian",
                                            "Afroamericano" = "African American",
                                            "Asiático" = "Asian",
                                            "Nativo Americano"= "Native American",
                                            "Otro/Desconocido" = "Other/Unknown"),
                             selected = "Hispanic")),
),
         
fluidRow(h3("Comorbilidades"),
         column(3,
                radioButtons("diabetes_mellitus", label = h4("Diabetes mellitus"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(3,
                radioButtons("aids", label = h4("HIV o SIDA"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(3,
                radioButtons("immunosuppression", label = h4("Inmunosupresión"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(3,
                radioButtons("cirrhosis", label = h4("Cirrocis"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(3,
                radioButtons("hepatic_failure", label = h4("Falla hepática"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(3,
                radioButtons("leukemia", label = h4("Leucemia"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(3,
                radioButtons("lymphoma", label = h4("Linfoma"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(3,
                radioButtons("solid_tumor_with_metastasis", label = h4("Tumor sólido"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
        
         
         
),
fluidRow(h3("Características hospitalarias"),
         column(3,
                radioButtons("icu_admit_source", label = h4("Procedencia de la admisión:"),
                             choices = list("Área de Emergencia" = "Accident & Emergency",
                                            "Piso" = "Floor",
                                            "Postoperatorio" = "Operating Room / Recovery",
                                            "Derivado de otro Hospital"  = "Other Hospital",
                                            "Derivado de otra ICU" = "Other ICU",
                                            "No aplica" = NA),
                             selected = "Accident & Emergency")),
         
         column(2,
                radioButtons("icu_stay_type", label = h4("Tipo de admición:"),
                             choices = list("Admitido" = "admit",
                                            "Readmintido" = "readmit",
                                            "Transferido"  = "transfer",
                                            "No aplica" = NA),
                             selected = "admit")),
         
         column(2,
                radioButtons("icu_type", label = h4("Tipo de atención en UCI:"),
                             choices = list("CTICU" = "CTICU",
                                            "Med-Surg ICU" = "Med-Surg ICU",
                                            "CCU-CTICU" = "CCU-CTICU",
                                            "Neuro ICU"="Neuro ICU",
                                            "MICU"= "MICU",
                                            "SICU" ="SICU",
                                            "Cardiac ICU" ="Cardiac ICU",
                                            "CSICU" ="CSICU",
                                            "No aplica" = NA),
                             selected = "CTICU")),
         column(3,
                numericInput("pre_icu_los_days",
                             label = h4("Ingrese días de hospitalizacion previo al traslado a UCI"),
                             value = 0)),
         
),

fluidRow( h3("Signos vitales"),
          column(3,
                 numericInput("heart_rate_apache",
                              label = h4("Frecuencia cardiaca por minuto"),
                              value = 100)),
          column(3,
                 numericInput("map_apache",
                              label = h4("Presión arteria media"),
                              value = 88)),
          column(3,
                 numericInput("resprate_apache",
                              label = h4("Frecuencia respiratoria por minuto"),
                              value = 25)),
          column(3,
                 numericInput("temp_apache",
                              label = h4("Temperatura °C"),
                              value = 36.5))
),

fluidRow( h3("Resultados de laboratorio"),
          column(3,
                 numericInput("hematocrit_apache",
                              label = h4("Hematocrito"),
                              value = 33)),
          column(3,
                 numericInput("wbc_apache",
                              label = h4("Globulos blancos en miles"),
                              value = 6.5)),
          column(3,
                 numericInput("glucose_apache",
                              label = h4("Glucosa"),
                              value = 134)),
          column(3,
                 numericInput("bun_apache",
                              label = h4("Urea"),
                              value = 19)),
          column(3,
                 numericInput("creatinine_apache",
                              label = h4("Creatinina"),
                              value = 0.9)),
          column(3,
                 numericInput("urineoutput_apache",
                              label = h4("Volumen de orina en 24h"),
                              value = 1700)),
          column(3,
                 numericInput("sodium_apache",
                              label = h4("Sodio"),
                              value = 138)),
          column(3,
                 numericInput("albumin_apache",
                              label = h4("Albumina"),
                              value = 2.9)),
          column(3,
                 numericInput("bilirubin_apache",
                              label = h4("Bilirrubina"),
                              value = 0.6)),
),
      
fluidRow(h3("Situaciones especiales"),
         column(4,
                radioButtons("elective_surgery", label = h4("Cirugía electiva"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(4,
                radioButtons("arf_apache", label = h4("Diálisis"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
         column(4,
                radioButtons("ventilated_apache", label = h4("Ventilación mecánica"),
                             choices = list("Si" = "Si", "No" = "No", "Desconocido" = "No"),
                             selected = "No")),
),

fluidRow(h3("Escala de Glasglow"),
         column(4,
                radioButtons("gcs_motor_apache", label = h4("Respuesta motora"),
                             choices = list("Obedece comandos" = 6,
                                            "Localiza el dolor" = 5,
                                            "Retira al dolor" = 4,
                                            "Flexión al dolor" = 3,
                                            "Extensión al dolor"= 2,
                                            "No responde a dolor" = 1,
                                            "No explorado"= 6),
                             selected = 6)),
         column(4,
                radioButtons("gcs_verbal_apache", label = h4("Respuesta verbal"),
                             choices = list("Orientado" = 5,
                                            "Confundido / desorientado" = 4,
                                            "Palabras innapropiadas" = 3,
                                            "Sonidos incomprencibles"= 2,
                                            "No responde verbalmente" = 1,
                                            "No explorado"= 5),
                             selected = 5)),
         column(4,
                radioButtons("gcs_eyes_apache", label = h4("Respuesta ocular"),
                             choices = list("Espontáneamente abiertos" = 4,
                                            "Abiertos a comando verbal" = 3,
                                            "Abiertos al dolor"= 2,
                                            "No abre los ojos" = 1,
                                            "No explorado"= 4),
                             selected = 4)),
         
),      
      
      
      
    ))),

# Analisis univariado -----------------------------------------------------

  navbarMenu("Análisis exploratorio",# sirve para generar varias pestañas en una sola
  tabPanel("Análisis univariado",
                  
           
           h3("Análisis estadistico descriptivo univariado"),
           
           p("A continuacion presentamos de forma dinamica el analisis estadistico 
        desciptivo univariado de todas las variables de la base de datos"), 
           br(),
           selectInput(inputId = "datos",
                       label = "Escoja una variable:",
                       choices = names(fpe)),
           
           # Input: Numeric entry for number of obs to view ----
           numericInput(inputId = "obs",
                        label = "Numero de observaciones a mostrar:",
                        value = 5),# en que valor aparecerá al comienzo
           
           strong("Se presenta la tabla con las caracteristicas descriptivas de la variable:"),
           verbatimTextOutput("summary"),
           
           # Salida: tabla HTML con el número 
           # solicitado de observaciones ----
           tableOutput("view"),
           
           # Resultado: bar ----
           plotOutput(outputId = "bar"), # tipo de resultado
           # outputId muestra el nombre del objeto creado en server
           # del que se va a imprimir el resultado      
           
           # Resultado: digarama de caja ----
           plotOutput(outputId = "caja"), # tipo de resultado
           # outputId muestra el nombre del objeto creado en server
           # del que se va a imprimir el resultado
           
           
           
           

),
 
tabPanel("Análisis bivariado",
         # Salida: Tablas / graficos, summarys ----
         tabsetPanel(type = "tabs",
                     tabPanel("Variables continuas", #Ajuste
                              selectInput(inputId = "datos2",
                                          label = "Escoja una variable continua para comparar con ajuste:",
                                          choices = names(fpe)),
                              strong("Al realizar la prueba de Wilcoxon dio un valor p de:"),
                              verbatimTextOutput("analisis1"),
                              plotOutput("dispersion1", click = "plot_click"),
                              plotOutput("dispersion", click = "plot_click"),
                              #verbatimTextOutput("info"),
                              #label = ,
                              # strong("Al realizar la prueba de Wilcoxon dio un valor p de:"),
                              # verbatimTextOutput("analisis1"),
                              # plotOutput("dispersion1", click = "plot_click"),
                              # plotOutput("dispersion", click = "plot_click")
                     ),
                     
                     tabPanel("Variables categoricas",
                              selectInput(inputId = "datos3",
                                          label = "Escoja una variable categórica para comparar con cambio:",
                                          choices = names(fpe)),
                              strong("Al realizar la prueba de Chi2 dio un valor p de:"),
                              verbatimTextOutput("analisis3"),
                              verbatimTextOutput("tablaFrecCategorica"),
                              plotOutput("dispersion2"),
                              verbatimTextOutput("tablaPorcCategorica"),
                              plotOutput("dispersion3")),
                     
                     tabPanel("Correlaciones", 
                              selectInput(inputId = "datos4",
                                          label = "Escoja una variable para comparar con producto:",
                                          choices = c(names(fpe)[1],names(fpe)[3])),
                              
                              plotOutput("dispersion4"))
                     
         )
)

)

))