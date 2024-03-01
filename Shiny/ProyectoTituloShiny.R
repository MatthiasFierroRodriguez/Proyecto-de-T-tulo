rm(list = ls())

#### Librerías ####

# install.packages("shiny")
# install.packages("DT")
# install.packages("dygraphs")
# install.packages("xts")
# install.packages("leaflet")
# install.packages("sf")
# install.packages("tidyverse")
# install.packages("shinythemes")
# install.packages("htmltools")
library(shiny)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(sf)
library(tidyverse)
library(shinythemes)
library(htmltools)

#### Directorios ####

# setwd(choose.dir())
addResourcePath("www", getwd())

#### Datos ####

load("BaseShinyDef.Rdata")
load("Geom_Santiago2.Rdata")
load("CoefEspEstr.Rdata")
load("CoefEspNoEstr.Rdata")
load("CoefTempEstr.Rdata")
load("CoefTempNoEstr.Rdata")
load("CoefInt.Rdata")

data = BaseShiny |> mutate(Codigo = as.character(Codigo))

colnames(data) = c("Codigo", "Comuna", 
                   "Número de Muertes por Cáncer de Pulmón",
                   "Número de Muertes Esperadas por Cáncer de Pulmón",
                   "Año", "Mes", 
                   "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori",
                   "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Mediana a Posteriori",
                   "Límite Inferior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (2.5%)",
                   "Límite Superior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (97.5%)",
                   "Probabilidad de Sobremortalidad por Cáncer de Pulmón",
                   "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Observada")

map = santiago.sf2

#### User-Interface ####

ui = fluidPage(
  
  #### Tema por Defecto ####
  
  theme = shinytheme(theme = "flatly"),
  
  #### Tema del Fondo y Configuración Leaflet ####
  
  tags$head(
    tags$style(HTML("
            body {
                background-color: white;
                color: black;
            }
            .leaflet-top {z-index:999!important;}
            .leaflet-bottom {z-index:999!important;}
        "))
  ),
  
  #### Título y Autor ####
  
  titlePanel(title = p(strong("Proyecto de Título"), style = "color: #00aae4"),
             windowTitle = "Proyecto de Título"),
  
  h4("Matthias Alejandro Fierro Rodríguez", style = "color: #00aae4"),
  
  tabsetPanel(
    
    #### Información: User-Interface ####
    
    tabPanel(title = "Información",
             hr(),
             h4(strong("Descripción del Proyecto"), style = "color: #00aae4"),
             hr(),
             p("Este proyecto titulado 'Un análisis espacio-temporal de la mortalidad por cáncer de pulmón en la provincia de Santiago, Chile (2012-2023): Un enfoque basado en INLA' tiene por objetivo analizar mediante un modelo de mapeo de enfermedades espacio-temporal las razones de mortalidad estandarizadas (SMR por sus siglas en inglés) por cáncer de pulmón de cada una de las comunas pertenecientes a la provincia de Santiago de Chile, a nivel mensual desde enero del año 2012 hasta diciembre del año 2023. Por otro lado, esta plataforma fue construida con la finalidad de desplegar tanto los datos como los resultados de este proyecto de forma dinámica para que los usuarios puedan interactuar con dicha información."),
             hr(),
             h4(strong("Información de Interés"),  style = "color: #00aae4"),
             hr(),
             p("Todos los archivos relacionados a este proyecto pueden encontrarse en", a("GitHub", href = "https://github.com/MatthiasFierroRodriguez/Proyecto-de-T-tulo", target = "_blank"), "."),
             p("Email de Contacto: mafierro2019@udec.cl"),
             hr(),
             h4(strong("Descripción de las Pestañas"),  style = "color: #00aae4"),
             hr(),
             p(strong("Análisis Exploratorio:"), "Distintas pestañas asociadas al análisis exploratorio de los datos."),
             p(strong("Modelo:"), "Distintas pestañas asociadas a los resultados del modelo de mapeo de enfermedades espacio-temporal."),
             p(strong("Mapas:"), "Muestra dos mapas interactivos, cada uno con una variable de interés elegida por el usuario, donde se muestran los valores de dicha variable para cada una de las comunas de la provincia de Santiago de Chile en el año y mes elegido por el usuario. Además, se puede visualizar la evolución temporal de las variables elegidas a través de una animación que irá avanzando de mes en mes automáticamente."),
             p(strong("Evolución Temporal:"), "Contiene una serie de tiempo interactiva donde se muestra la evolución temporal de alguna variable de interés elegida por el usuario."),
             p(strong("Datos:"), "Se despliega una tabla con los datos utilizados en el proyecto y de los resultados del modelo. Estos datos pueden ser filtrados y descargados por el usuario."),
             p(strong("Configuración:"), "Pestaña destinada a la configuración de distintos aspectos de la plataforma por parte del usuario."),
             icon = icon(name = "circle-info", lib = "font-awesome")),
    
    #### Análisis Exploratorio de Datos: User-Interface ####
    
    navbarMenu(title = "Análisis Exploratorio",
               icon = icon(name = "magnifying-glass-chart", lib = "font-awesome"),
               
               tabPanel(title = "Ranking de muertes por enfermedades oncológicas",
                        hr(),
                        h4(strong("Ranking de Muertes por Enfermedades Oncológicas"), style = "color: #00aae4"),
                        hr(),
                        p("Ranking de las 10 de enfermedades oncológicas con mayor número de muertes en la provincia de Santiago de Chile entre enero del año 2012 y diciembre del año 2023, con su respectivo código CIE-10, número de muertos por dicha enfermedad y a que porcentaje del total de muertes por enfermedades oncológicas corresponde en dicho periodo."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "topcancer"))),
               
               tabPanel(title = "Muertes por cáncer de pulmón por edad",
                        hr(),
                        h4(strong("Muertes por Cáncer de Pulmón por Edad"), style = "color: #00aae4"),
                        hr(),
                        p("Número de muertos por cáncer de pulmón por edad (en años) en la provincia de Santiago de Chile entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        h4(strong("Nota:"), style = "color: #00aae4"),
                        hr(),
                        p("Para este proyecto se agruparon las edades en edades quinquenales, es decir, en intervalos de 5 años, por ejemplo, de 0 a 4 años, luego de 5 a 9 años, etc. Sin embargo, debido a que las estimaciones y proyecciones de la población dadas por el Instituto Nacional de Estadísticas de Chile solo llegan hasta el grupo '80 años y más', para estas edades quinquenales el último grupo considerado también corresponderá a '80 años y más'."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "edades2"))),
               
               tabPanel(title = "Muertes por cáncer de pulmón por edad quinquenal",
                        hr(),
                        h4(strong("Muertes por Cáncer de Pulmón por Edad Quinquenal"), style = "color: #00aae4"),
                        hr(),
                        p("Número de muertes por cáncer de pulmón en cada grupo de edad quinquenal considerada en el proyecto, dentro de la provincia de Santiago de Chile, desde enero del año 2012 hasta diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "edades"))),
               
               tabPanel(title = "Muertes por cáncer de pulmón por sexo",
                        hr(),
                        h4(strong("Muertes por Cáncer de Pulmón por Sexo"), style = "color: #00aae4"),
                        hr(),
                        p("Número de muertes por cáncer de pulmón por sexo en la provincia de Santiago de Chile entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "muertesporsexo"))),
               
               tabPanel(title = "Muertes mensuales por cáncer de pulmón",
                        hr(),
                        h4(strong("Muertes Mensuales por Cáncer de Pulmón"), style = "color: #00aae4"),
                        hr(),
                        p("Número de muertes a nivel mensual por cáncer de pulmón en la provincia de Santiago de Chile entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "muertesmes"))),
               
               tabPanel(title = "Muertes mensuales por cáncer de pulmón por sexo",
                        hr(),
                        h4(strong("Muertes Mensuales por Cáncer de Pulmón por Sexo"), style = "color: #00aae4"),
                        hr(),
                        p("Número de muertes a nivel mensual por cáncer de pulmón segmentado por sexo en la provincia de Santiago de Chile entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "muertesyearsex"))),
               
               tabPanel(title = "Razones de mortalidad estandarizada por cáncer de pulmón por comuna",
                        hr(),
                        h4(strong("Razones de Mortalidad Estandarizada por Cáncer de Pulmón por Comuna"), style = "color: #00aae4"),
                        hr(),
                        p("Histogramas de las razones de mortalidad estandarizada por cáncer de pulmón entre enero del año 2012 y diciembre del año 2023 para cada una de las comunas pertenecientes a la provincia de Santiago de Chile."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "histsmrcomuna"))),
               
               tabPanel(title = "Evolución temporal de las razones de mortalidad estandarizada por cáncer de pulmón por comuna",
                        hr(),
                        h4(strong("Evolución Temporal de las Razones de Mortalidad Estandarizada por Cáncer de Pulmón por Comuna"), style = "color: #00aae4"),
                        hr(),
                        p("Gráficos con las razones de mortalidad estandarizada por cáncer de pulmón a través del periodo en estudio, para cada una de las comunas pertenecientes a la provincia de Santiago de Chile."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "smrcomunatemp"))),
               
               tabPanel(title = "Boxplots de las razones de mortalidad estandarizada por cáncer de pulmón por comuna",
                        hr(),
                        h4(strong("Boxplots de las Razones de Mortalidad Estandarizada por Cáncer de Pulmón por Comuna"), style = "color: #00aae4"),
                        hr(),
                        p("Boxplots de las razones de mortalidad estandarizada por cáncer de pulmón para cada una de las comunas de la provincia de Santiago de Chile entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "boxplotsmr")))
               
    ),
    
    #### Modelo: User-Interface ####
    
    navbarMenu(title = "Modelo", icon = icon(name = "lungs-virus", lib = "font-awesome"),
               
               tabPanel(title = "Descripción del Modelo",
                        hr(),
                        h4(strong("Descripción del Modelo"), style = "color: #00aae4"),
                        hr(),
                        p("Los modelos espacio-temporales de mapeo de enfermedades, suelen ser utilizados en estudios de vigilancia de enfermedades cuando el interés se basa en identificar el patrón espacial y temporal de una enfermedad. En lo que respecta a este proyecto se busca suavizar las razones de mortalidad estandarizadas por cáncer de pulmón a través de este tipo de modelos con el fin de identificar aquellas comunas que presentan sobremortalidad por dicha enfermedad y en que periodos ocurre dicho fenomeno. Con ello la especificación teórica de estos modelos, considerando una tendencia temporal no paramétrica y un efecto aleatorio relacionado a la interacción espacio-temporal se tiene:"),
                        withMathJax(),
                        p("$$y_{it} \\sim Poisson(\\lambda_{it} = E_{it} \\rho_{it}),$$"),
                        p("$$\\log(\\rho_{it}) = b_{0} + \\frac{1}{\\sqrt{\\tau}} (\\sqrt{\\phi} \\  u_{i}^{*} + \\sqrt{1 - \\phi} \\ v_{i}^{*}) + \\gamma_{t}^{*} + \\psi_{t} + \\delta_{it}.$$"),
                        p("Donde, en el contexto de este proyecto:"),
                        p("\\(y_{it}\\) representa el número de muertes en la i-ésima comuna y en el t-ésimo mes, además se asume que sigue una distribución Poisson con parámetro \\(\\lambda_{it}\\), el cual es reescrito como \\(\\lambda_{it} = \\rho_{it}E_{it} \\), así"),
                        p("\\(\\rho_{it}\\) representa la razón de mortalidad estandarizada por cáncer de pulmón en la i-ésima comuna y en el t-ésimo mes, y"),
                        p("\\(E_{it}\\) representa el número de muertes esperadas por cáncer de pulmón en la i-ésima comuna y en el t-ésimo mes."),
                        p(strong("Nota:"), "El número esperado de muertes así como las razones de mortalidad estandarizadas por cáncer de pulmón fueron obtenidas a través de la estandarización indirecta ajustando las tasas de mortalidad por edades quinquenales y sexo de los fallecidos."),
                        p("Por otro lado con lo que respecta al predictor lineal se tiene:"),
                        p("\\(b_0\\) representa el intercepto del modelo, el cual a su vez corresponde a la razón de mortalidad estandarizada por cáncer de pulmón promedio de la provincia de Santiago de Chile"),
                        p("\\(\\tau\\) corresponde a un parámetro de precisión (lo cual implica que \\(\\tau\\) > 0) compartido por los efectos aleatorios espaciales descritos a continuación:"),
                        p("\\(u_{i}^{*}\\) corresponde al efecto aleatorio espacial estructurado de la i-ésima comuna,"),
                        p("\\(v_{i}^{*}\\) corresponde al efecto aleatorio espacial no estructurado de la i-ésima comuna."),
                        p(strong("Nota:"), "Aquí * representa que dicho efecto aleatorio fue escalado para que tenga varianza igual a 1, además ambos efectos aleatorios espaciales fueron modelados a través de un modelo BYM2."),
                        p("\\(\\phi\\) representa la proporción de variabilidad espacial explicada por el efecto aleatorio estructurado, con \\(\\phi \\in [0,1]\\)."),
                        p("\\(\\gamma_{t}^{*}\\) corresponde al efecto aleatorio temporal estructurado del t-ésimo mes, modelado a través de un camino aleatorio de orden uno."),
                        p("\\(\\psi_{t}\\) corresponde al efecto aleatorio temporal no estructurado del t-ésimo mes."),
                        p("\\(\\delta_{it}\\) corresponde al efecto aleatorio asociado a la interacción espacio-temporal, modelado como una interacción espacio-temporal de tipo IV.")
               ),
               
               tabPanel(title = "Análisis de Sensibilidad de las Distribuciones a Priori",
                        hr(),
                        h4(strong("Análisis de Sensibilidad de las Distribuciones a Priori"), style = "color: #00aae4"),
                        hr(),
                        p("Este proyecto fue realizado bajo un paradigma Bayesiano realizado a través de la metodologia INLA, por ello la elección de las distribuciones a priori cumple un papel fundamental a la hora de realizar inferencias Bayesianas, ya que las conclusiones que se pueden obtener dependen de esta elección, así el análisis de sensibilidad busca medir la robustez de los resultados con respecto a la elección de las distribuciones a priori, donde estos pueden ser más o menos dependientes de esta elección."),
                        p("Dentro del proyecto se eligieron las siguientes distribuciones para los parámetros e hiperparámetros del modelo de mapeo de enfermedades espacio-temporal:"),
                        p("$$b_0 \\sim Normal(0,1000),$$"),
                        p("$$\\log(\\tau) \\sim PC(U = 0.75, \\alpha = 0.05),$$"),
                        p("$$logit(\\phi) \\sim PC(U = 1/2, \\alpha = 2/3),$$"),
                        p("$$\\boldsymbol{u^{*}}|\\tau \\sim MVNormal(\\boldsymbol{0}, (\\boldsymbol{R} \\tau)^{-1}),$$"),
                        p("$$u_{i}^{*}|\\boldsymbol{u}^{*}_{-i},\\tau \\sim Normal\\left(\\frac{1}{N_i} \\displaystyle\\sum_{j:i \\sim j} u_{j}^{*}, (\\tau N_{i})^{-1}\\right),$$"),
                        p("$$\\boldsymbol{v^{*}}|\\tau \\sim MVNormal(\\boldsymbol{0}, \\boldsymbol{I} \\tau^{-1}),$$"),
                        p("$$\\boldsymbol{\\gamma}|\\tau_{\\gamma} \\sim MVNormal(\\boldsymbol{0}, (\\tau_{\\gamma} \\boldsymbol{K})^{-1}),$$"),
                        p("$$\\log(\\tau_{\\gamma}) \\sim PC(U = 1, \\alpha = 0.01),$$"),
                        p("$$\\boldsymbol{\\psi}|\\tau_{\\psi} \\sim MVNormal(\\boldsymbol{0}, \\boldsymbol{I} \\tau^{-1}_{\\psi}),$$"),
                        p("$$\\log(\\tau_{\\psi}) \\sim loggamma(1,5 \\cdot 10^{-05}),$$"),
                        p("$$\\boldsymbol{\\delta}|\\tau_{\\delta} \\sim MVNormal(\\boldsymbol{0}, (\\boldsymbol{R}_{\\delta} \\tau_{\\delta})^{-1}),$$"),
                        p("$$\\log(\\tau_{\\delta}) \\sim loggamma(1,5 \\cdot 10^{-05}).$$"),
                        p("Con ello a continuación se despliegan los análisis de sensibilidad realizados, los cuales se basaron en cambios dentro de los parámetros de las distribuciones a priori elegidas:"),
                        hr(),
                        h4(strong("Análisis de Sensibilidad: \\(b_0\\)"), style = "color: #00aae4"),
                        hr(),
                        wellPanel(htmlOutput(outputId = "sensibilidad1")),
                        hr(),
                        h4(strong("Análisis de Sensibilidad: \\(\\phi\\)"), style = "color: #00aae4"),
                        hr(),
                        wellPanel(htmlOutput(outputId = "sensibilidad2")),
                        hr(),
                        h4(strong("Análisis de Sensibilidad: \\(\\tau\\)"), style = "color: #00aae4"),
                        hr(),
                        wellPanel(htmlOutput(outputId = "sensibilidad3")),
                        hr(),
                        h4(strong("Análisis de Sensibilidad: \\(\\tau_\\gamma\\)"), style = "color: #00aae4"),
                        hr(),
                        wellPanel(htmlOutput(outputId = "sensibilidad4")),
                        hr(),
                        h4(strong("Análisis de Sensibilidad: \\(\\tau_\\psi\\)"), style = "color: #00aae4"),
                        hr(),
                        wellPanel(htmlOutput(outputId = "sensibilidad5")),
                        hr(),
                        h4(strong("Análisis de Sensibilidad: \\(\\tau_\\delta\\)"), style = "color: #00aae4"),
                        hr(),
                        wellPanel(htmlOutput(outputId = "sensibilidad6"))
               ),
               
               tabPanel(title = "Mapa de Calor: Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori",
                        hr(),
                        h4(strong("Mapa de Calor: Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori"), style = "color: #00aae4"),
                        hr(),
                        p("Mapa de calor con las razones de mortalidad estandarizada por cáncer de pulmón promedio a posteriori obtenidas desde el modelo de mapeo de enfermedades espacio-temporal para cada una las comunas de la provincia de Santiago de Chile y para cada mes desde enero del año 2012 hasta diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "heatmapsmr"))),
               
               tabPanel(title = "Mapa de Calor: Probabilidad de Sobremortalidad a Posteriori por Cáncer de Pulmón",
                        hr(),
                        h4(strong("Mapa de Calor: Probabilidad de Sobremortalidad a Posteriori por Cáncer de Pulmón"), style = "color: #00aae4"),
                        hr(),
                        p("Mapa de calor con la probabilidad a posteriori de que una comuna de la provincia de Santiago de Chile en un mes en particular entre enero del año 2012 y diciembre del año 2023 presente sobremortalidad por cáncer de pulmón, es decir, la probabilidad a posteriori de que hayan más muertos que los que se esperaba."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "heatmapprob"))),
               
               tabPanel(title = "Mapa de Calor: Sobremortalidad por Cáncer de Pulmón",
                        hr(),
                        h4(strong("Mapa de Calor: Sobremortalidad por Cáncer de Pulmón"), style = "color: #00aae4"),
                        hr(),
                        p("Mapa de calor que indica (en color rojo) si una comuna de la provincia de Santiago de Chile presenta sobremortalidad en algún mes entre enero del año 2012 y diciembre del año 2023, aquí se considera que existe sobremortalidad por cáncer de pulmón si su probabilidad a posteriori supera el umbral de 0.8."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "heatmapsobre"))),
               
               tabPanel(title = "Mapa: Efecto Espacial Estructurado",
                        hr(),
                        h4(strong("Mapa: Efecto Espacial Estructurado"), style = "color: #00aae4"),
                        hr(),
                        p("Mapa con el efecto aleatorio espacial estructurado promedio a posteriori asociado a cada una de las comunas pertenecientes a la provincia de Santiago de Chile."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "mapadinamicoestructurado"))),
               
               tabPanel(title = "Mapa: Efecto Espacial no Estructurado",
                        hr(),
                        h4(strong("Mapa: Efecto Espacial no Estructurado"), style = "color: #00aae4"),
                        hr(),
                        p("Mapa con el efecto aleatorio espacial no estructurado promedio a posteriori asociado a cada una de las comunas pertenecientes a la provincia de Santiago de Chile."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "mapadinamiconoestructurado"))),
               
               tabPanel(title = "Gráfico: Efecto Temporal Estructurado",
                        hr(),
                        h4(strong("Gráfico: Efecto Temporal Estructurado"), style = "color: #00aae4"),
                        hr(),
                        p("Gráfico con el efecto aleatorio temporal estructurado promedio a posteriori y su respectivo intervalo de credibilidad del 95% (de cuantiles) a posteriori desde enero del año 2012 hasta diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "temporalestr"))),
               
               tabPanel(title = "Gráfico: Efecto Temporal no Estructurado",
                        hr(),
                        h4(strong("Gráfico: Efecto Temporal no Estructurado"), style = "color: #00aae4"),
                        hr(),
                        p("Gráfico con el efecto aleatorio temporal no estructurado promedio a posteriori y su respectivo intervalo de credibilidad del 95% (de cuantiles) a posteriori desde enero del año 2012 hasta diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "temporalnoestr"))),
               
               tabPanel(title = "Gráfico: Interacción Espacio-Temporal",
                        hr(),
                        h4(strong("Gráfico: Interacción Espacio-Temporal"), style = "color: #00aae4"),
                        hr(),
                        p("Gráfico con el efecto aleatorio de interacción espacio-temporal de tipo IV promedio a posteriori y su respectivo intervalo de credibilidad del 95% (de cuantiles) a posteriori para cada una de las comunas de la provincia de Santiago de Chile y para cada mes entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "interaccion"))),
               
               tabPanel(title = "Gráfico: Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori por Comuna",
                        hr(),
                        h4(strong("Gráfico: Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori por Comuna"), style = "color: #00aae4"),
                        hr(),
                        p("Gráfico con las razones de mortalidad estandarizada por cáncer de pulmón promedio a posteriori suavizadas por el modelo de mapeo de enfermedades espacio-temporal para cada una de las comunas de la provincia de Santiago de Chile entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "smrcomunatemp2"))),
               
               tabPanel(title = "Gráfico: Probabilidad de Sobremortalidad a Posteriori por Comuna",
                        hr(),
                        h4(strong("Gráfico: Probabilidad de Sobremortalidad a Posteriori por Comuna"), style = "color: #00aae4"),
                        hr(),
                        p("Gráfico con las probabilidades a posteriori de que una comuna de la provincia de Santiago de Chile presente sobremortalidad por cáncer de pulmón entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        wellPanel(htmlOutput(outputId = "probsobreporcomuna"))),
               
               tabPanel(title = "Gráfico: Resultados del Modelo por Comuna",
                        hr(),
                        h4(strong("Gráfico: Resultados del Modelo por Comuna"), style = "color: #00aae4"),
                        hr(),
                        p("Gráfico con las razones de mortalidad estandarizadas por cáncer de pulmón promedio a posteriori y observadas, así como sus respectivos intervalos de credibilidad del 95% (de cuantiles) de cada una de las comunas de la provincia de Santiago de Chile entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        wellPanel(img(src = "www/RRProbSobreTotal.png", height = "720px", width = "100%"))),
               
               tabPanel(title = "Tabla: Efecto Espacial Estructurado",
                        hr(),
                        h4(strong("Tabla: Efecto Espacial Estructurado"), style = "color: #00aae4"),
                        hr(),
                        p("Tabla con estadísticos a posteriori de los efectos aleatorios espaciales estructurados de cada una de las comunas de la provincia de Santiago de Chile."),
                        hr(),
                        DTOutput(outputId = "tespestr")),
               
               tabPanel(title = "Tabla: Efecto Espacial no Estructurado",
                        hr(),
                        h4(strong("Tabla: Efecto Espacial no Estructurado"), style = "color: #00aae4"),
                        hr(),
                        p("Tabla con estadísticos a posteriori de los efectos aleatorios espaciales no estructurados de cada una de las comunas de la provincia de Santiago de Chile."),
                        hr(),
                        DTOutput(outputId = "tespnoestr")),
               
               tabPanel(title = "Tabla: Efecto Temporal Estructurado",
                        hr(),
                        h4(strong("Tabla: Efecto Temporal Estructurado"), style = "color: #00aae4"),
                        hr(),
                        p("Tabla con estadísticos a posteriori de los efectos aleatorios temporales estructurados para cada mes entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        DTOutput(outputId = "ttempestr")),
               
               tabPanel(title = "Tabla: Efecto Temporal no Estructurado",
                        hr(),
                        h4(strong("Tabla: Efecto Temporal no Estructurado"), style = "color: #00aae4"),
                        hr(),
                        p("Tabla con estadísticos a posteriori de los efectos aleatorios temporales no estructurados para cada mes entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        DTOutput(outputId = "ttempnoestr")),
               
               tabPanel(title = "Tabla: Interacción Espacio-Temporal",
                        hr(),
                        h4(strong("Tabla: Interacción Espacio-Temporal"), style = "color: #00aae4"),
                        hr(),
                        p("Tabla con estadísticos a posteriori de los efectos aleatorios de interacción espacio-temporal para cada una de las comunas de la provincia de Santiago de Chile y para cada mes entre enero del año 2012 y diciembre del año 2023."),
                        hr(),
                        DTOutput(outputId = "tint"))
               
    ),
    
    #### Mapas: User-Interface ####
    
    tabPanel(title = "Mapas",
             icon = icon(name = "map", lib = "font-awesome"),
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(
                   inputId = "variableselected",
                   label = "Seleccionar (Izquierda)",
                   choices = c("Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori",
                               "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Mediana a Posteriori",
                               "Límite Inferior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (2.5%)",
                               "Límite Superior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (97.5%)",
                               "Probabilidad de Sobremortalidad por Cáncer de Pulmón",
                               "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Observada",
                               "Número de Muertes por Cáncer de Pulmón",
                               "Número de Muertes Esperadas por Cáncer de Pulmón")
                 ),
                 
                 selectInput(
                   inputId = "variableselected2",
                   label = "Seleccionar (Derecha)",
                   choices = c("Probabilidad de Sobremortalidad por Cáncer de Pulmón",
                               "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori",
                               "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Mediana a Posteriori",
                               "Límite Inferior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (2.5%)",
                               "Límite Superior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (97.5%)",
                               "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Observada",
                               "Número de Muertes por Cáncer de Pulmón",
                               "Número de Muertes Esperadas por Cáncer de Pulmón")
                 ),
                 
                 sliderInput(
                   inputId = "yearselected",
                   label = "Seleccionar Año",
                   min = 2012,
                   max = 2023,
                   value = 2012,
                   step = 1,
                   ticks = T,
                   sep = ""
                 ),
                 
                 sliderInput(
                   inputId = "monthselected",
                   label = "Seleccionar Mes",
                   min = 1,
                   max = 12,
                   value = 1,
                   step = 1,
                   ticks = T
                 ),
                 
                 actionButton(
                   inputId = "gif",
                   label = "Iniciar Animación", 
                   icon = icon("play"),
                 )
                 
               ),
               
               mainPanel(
                 splitLayout(
                   
                   leafletOutput(outputId = "map", height = 400, width = 450),
                   leafletOutput(outputId = "map2", height = 400, width = 450)
                   
                 )
               )
             )
    ),
    
    #### Serie de Tiempo: User-Interface ####
    
    tabPanel(title = "Evolución Temporal", 
             icon = icon(name = "chart-line", lib = "font-awesome"),
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(
                   inputId = "variableselected3",
                   label = "Seleccionar",
                   choices = c("Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori",
                               "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Mediana a Posteriori",
                               "Límite Inferior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (2.5%)",
                               "Límite Superior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (97.5%)",
                               "Probabilidad de Sobremortalidad por Cáncer de Pulmón",
                               "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Observada",
                               "Número de Muertes por Cáncer de Pulmón",
                               "Número de Muertes Esperadas por Cáncer de Pulmón")
                 ),
               ),
               
               mainPanel(
                 splitLayout(
                   
                   dygraphOutput(outputId = "timetrend")
                   
                 )
               )
             )
    ),
    
    #### Base de Datos: User-Interface ####
    
    tabPanel(title = "Datos", icon = icon(name = "table", lib = "font-awesome"),
             
             fluidRow(
               
               column(width = 2, selectInput("comuna", "Comuna", BaseShiny$Comuna |> unique(), multiple = T)),
               column(width = 2, selectInput("año", "Año", 2012:2023, multiple = T)),
               column(width = 2, selectInput("mes", "Mes", 1:12, multiple = T)),
               
               DTOutput(outputId = "table")
               
             )
    ),
    
    #### Configuración: User-Interface ####
    
    tabPanel(title = "Configuración",
             
             div(style = "width: 30%; 
                 margin: auto;",
                 
                 wellPanel(
                   
                   selectInput(inputId = "seleccionartema", 
                               label = "Elegir Tema", 
                               choices = c("flatly",
                                           "cosmo",
                                           "cerulean",
                                           "journal",
                                           "readable",
                                           "simplex",
                                           "spacelab",
                                           "united"
                               )
                   ),
                   
                   sliderInput(inputId = "velocidadgif",
                               label = "Retardado de la Animación (en segundos)", 
                               min = 1,
                               max = 10, 
                               value = 2,
                               step = 1, 
                               ticks = T),
                   
                   uiOutput(outputId = "tema")
                   
                 )
             ), 
             
             icon = icon(name = "gear", lib = "font-awesome"))
    
  )
)

#### Servidor ####

server = function(input, output, session){
  
  #### Configuración: Tema ####
  
  output$tema = renderUI({
    
    theme = shinytheme(theme = input$seleccionartema)
    
    fluidPage(
      theme = theme
    )
    
  })
  
  #### Boton: Animación ####
  
  etiqueta = reactiveVal(T)
  
  observeEvent(input$gif, {
    
    if(etiqueta()){
      updateActionButton(session, "gif",
                         label = "Pausar Animación",
                         icon = icon(name = "pause", lib = "font-awesome"))
      etiqueta(F)
    }else{
      updateActionButton(session, "gif", 
                         label = "Iniciar Animación",
                         icon = icon(name = "play", lib = "font-awesome"))
      etiqueta(T)
    }
  })
  
  observeEvent(input$gif, {
    
    observe({
      
      if(!etiqueta()){
        
        invalidateLater(millis = 1000 * input$velocidadgif, session = session)
        currentYear = isolate(expr = input$yearselected)
        currentMonth = isolate(expr = input$monthselected)
        
        if(currentMonth < 12){
          
          invalidateLater(millis = 1000 * input$velocidadgif, session = session)
          updateSliderInput(session = session, 
                            inputId = "monthselected", 
                            value = currentMonth + 1)
          
        }else{
          
          if(currentYear < 2023){
            
            invalidateLater(millis = 1000 * input$velocidadgif, session = session)
            updateSliderInput(session = session, 
                              inputId = "yearselected", 
                              value = currentYear + 1)
            updateSliderInput(session = session, 
                              inputId = "monthselected", 
                              value = 1)
            
          }else{
            
            updateSliderInput(session = session,
                              inputId = "yearselected",
                              value = 1)
            updateSliderInput(session = session, 
                              inputId = "monthselected",
                              value = 1)
            # updateActionButton(session = session,
            #                    inputId = "gif", 
            #                    label = "Iniciar Animación",
            #                    icon = icon(name = "play", lib = "font-awesome"))
            # etiqueta(T)
            
          }
        }
      }
    })
  })
  
  #### Tablas Efectos Aleatorios ####
  
  output$tespestr = renderDT(Estr, 
                             class = "cell-border stripe",
                             rownames = NULL,
                             options = list(
                               pageLength = nrow(Estr), 
                               autoWidth = T,
                               dom = "Blfrt",
                               orderClasses = T,
                               searchHighlight = T,
                               buttons = list("copy", "colvis",
                                              list(
                                                extend = "collection",
                                                buttons = c("csv", "excel"),
                                                text = "Descargar"
                                              )
                               ),
                               fixedHeader = T,
                               language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                 "}")
                             ),
                             extensions = c("Buttons", "FixedHeader")
  )
  
  
  output$tespnoestr = renderDT(NoEstr, 
                               class = "cell-border stripe",
                               rownames = NULL,
                               options = list(
                                 pageLength = nrow(NoEstr), 
                                 autoWidth = T,
                                 dom = "Blfrt",
                                 orderClasses = T,
                                 searchHighlight = T,
                                 buttons = list("copy", "colvis",
                                                list(
                                                  extend = "collection",
                                                  buttons = c("csv", "excel"),
                                                  text = "Descargar"
                                                )
                                 ),
                                 fixedHeader = T,
                                 language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                   "}")
                               ),
                               extensions = c("Buttons", "FixedHeader")
  )
  
  output$ttempestr = renderDT(EstrTemp, 
                              class = "cell-border stripe",
                              rownames = NULL,
                              options = list(
                                pageLength = nrow(EstrTemp), 
                                autoWidth = T,
                                dom = "Blfrt",
                                orderClasses = T,
                                searchHighlight = T,
                                buttons = list("copy", "colvis",
                                               list(
                                                 extend = "collection",
                                                 buttons = c("csv", "excel"),
                                                 text = "Descargar"
                                               )
                                ),
                                fixedHeader = T,
                                language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}")
                              ),
                              extensions = c("Buttons", "FixedHeader")
  )
  
  output$ttempnoestr = renderDT(NoEstrTemp, 
                                class = "cell-border stripe",
                                rownames = NULL,
                                options = list(
                                  pageLength = nrow(NoEstrTemp), 
                                  autoWidth = T,
                                  dom = "Blfrt",
                                  orderClasses = T,
                                  searchHighlight = T,
                                  buttons = list("copy", "colvis",
                                                 list(
                                                   extend = "collection",
                                                   buttons = c("csv", "excel"),
                                                   text = "Descargar"
                                                 )
                                  ),
                                  fixedHeader = T,
                                  language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
                                  initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                    "}")
                                ),
                                extensions = c("Buttons", "FixedHeader")
  )
  
  output$tint = renderDT(Int |> separate(Fecha, into = c("Año", "Mes"), sep = "-"), 
                         class = "cell-border stripe",
                         rownames = NULL,
                         options = list(
                           pageLength = nrow(Int), 
                           autoWidth = T,
                           dom = "Blfrt",
                           orderClasses = T,
                           searchHighlight = T,
                           buttons = list("copy", "colvis",
                                          list(
                                            extend = "collection",
                                            buttons = c("csv", "excel"),
                                            text = "Descargar"
                                          )
                           ),
                           fixedHeader = T,
                           language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                             "}")
                         ),
                         extensions = c("Buttons", "FixedHeader")
  )
  
  #### Base de Datos: Servidor ####
  
  output$table = renderDT(data |> filter(is.null(input$comuna) | Comuna %in% input$comuna,
                                         is.null(input$año) | Año %in% input$año,
                                         is.null(input$mes) | Mes %in% input$mes), 
                          class = "cell-border stripe",
                          rownames = NULL,
                          options = list(
                            pageLength = nrow(data), 
                            autoWidth = T,
                            dom = "Blfrt",
                            orderClasses = T,
                            searchHighlight = T,
                            lengthMenu = c(nrow(data), seq(100, nrow(data), by = 100)),
                            buttons = list("copy", "colvis",
                                           list(
                                             extend = "collection",
                                             buttons = c("csv", "excel"),
                                             text = "Descargar"
                                           )
                            ),
                            fixedHeader = T,
                            language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                              "}")
                          ),
                          extensions = c("Buttons", "FixedHeader"),
                          colnames = c("Código",
                                       "Comuna",
                                       "Número de Muertes por Cáncer de Pulmón",
                                       "Número de Muertes Esperadas por Cáncer de Pulmón",
                                       "Año", 
                                       "Mes", 
                                       "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Promedio a Posteriori",
                                       "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Mediana a Posteriori",
                                       "Límite Inferior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (2.5%)", 
                                       "Límite Superior del Intervalo de Credibilidad de la Razón de Mortalidad Estandarizada por Cáncer de Pulmón a Posteriori (97.5%)",
                                       "Probabilidad de Sobremortalidad por Cáncer de Pulmón a Posteriori",
                                       "Razón de Mortalidad Estandarizada por Cáncer de Pulmón Observada")
  )
  
  
  #### Serie de Tiempo: Servidor ####
  
  output$timetrend = renderDygraph({
    
    dataxts = NULL
    counties = unique(data$Comuna)
    
    for (l in 1:length(counties)){
      
      datacounty = data[data$Comuna == counties[l], ]
      
      dd = xts(
        datacounty[, input$variableselected3],
        as.Date(paste0(datacounty$Año, "-",
                       ifelse(datacounty$Mes < 10, 
                              paste0(0, datacounty$Mes),
                              datacounty$Mes), "-01")
        )
      )
      
      dataxts = cbind(dataxts, dd)
      
    }
    
    colnames(dataxts) = counties
    
    d1 = dygraph(data = dataxts) |> 
      dyRangeSelector() |> 
      dyHighlight(highlightSeriesBackgroundAlpha = 0.2, 
                  highlightSeriesOpts = list(strokeWidth = 2)) |> 
      #dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))
      dyUnzoom()
    
    
    d1$x$css = "
 .dygraph-legend > span {display:none;}
 .dygraph-legend > span.highlight { display: inline; }
 "
    d1
    
  })
  
  #### Mapas: Servidor ####
  
  datos = reactive({
    
    datafiltered = data |> filter(Año == input$yearselected,
                                  Mes == input$monthselected)
    
    map = map |> left_join(datafiltered, by = "Codigo")
    
    map$variableplot = map[, input$variableselected] |>
      as.data.frame() |> 
      select(-geometry) |>
      unlist() |>
      as.numeric()
    
    map
    
  })
  
  label = reactive({
    
    map = datos()
    
    label = sprintf("%s: %g",
                    map$Comuna,
                    map$variableplot) |> 
      lapply(FUN = HTML)
    
    label
    
  })
  
  datos2 = reactive({
    
    datafiltered = data |> filter(Año == input$yearselected,
                                  Mes == input$monthselected)
    
    map = map |> left_join(datafiltered, by = "Codigo")
    
    map$variableplot = map[, input$variableselected2] |>
      as.data.frame() |> 
      select(-geometry) |>
      unlist() |>
      as.numeric()
    
    map
    
  })
  
  label2 = reactive({
    
    map = datos2()
    
    label = sprintf("%s: %g",
                    map$Comuna,
                    map$variableplot) |> 
      lapply(FUN = HTML)
    
    label
    
  })
  
  output$map = renderLeaflet({
    
    pal = colorBin(palette = "RdYlGn",
                   domain = data[, input$variableselected] |> as.numeric(),
                   bins = 7,
                   pretty = T,
                   reverse = T)
    
    l = leaflet() |> 
      
      addTiles() |> 
      
      addProviderTiles(provider = providers$OpenStreetMap, group = "OpenStreetMap") |>
      addProviderTiles(provider = providers$CartoDB, group = "CartoDB") |>
      addProviderTiles(provider = providers$Esri, group = "Esri") |>
      
      addLayersControl(
        baseGroups = c("OpenStreetMap",
                       "CartoDB",
                       "Esri"),
        options = layersControlOptions(collapsed = F)
      ) |> 
      
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(imperial = F, metric = T)
      ) |> 
      
      fitBounds(-70.80840 + 0.16, -33.62743, -70.43015, -33.31069) |> 
      
      addMiniMap(
        toggleDisplay = T,
        width = 120, 
        height = 120, 
        strings = list(hideText = "Minimizar Minimapa", 
                       showText = "Mostrar Minimapa"), 
        zoomAnimation = T
      ) |> 
      
      addLegend(
        pal = pal,
        values = data[, input$variableselected] |> as.numeric(),
        opacity = 0.7,
        title = NULL
      )  
  })
  
  observe({
    
    map = datos()
    labels = label()
    
    pal = colorBin(palette = "RdYlGn",
                   domain = data[, input$variableselected] |> as.numeric(),
                   bins = 7,
                   pretty = T,
                   reverse = T)
    
    leafletProxy(mapId = "map", data = map, session = session) |>
      
      clearShapes() |> 
      
      addPolygons(
        data = map,
        fillColor = ~pal(variableplot),
        color = "black",
        dashArray = "1",
        weight = 1.5,
        fillOpacity = 0.5,
        opacity = 1,
        label = labels,
        highlightOptions = highlightOptions(color = "black", 
                                            weight = 3,
                                            bringToFront = T)
      )
  })
  
  output$map2 = renderLeaflet({
    
    pal = colorBin(palette = "RdYlGn",
                   domain = data[, input$variableselected2] |> as.numeric(),
                   bins = 7,
                   pretty = T,
                   reverse = T)
    
    l = leaflet() |> 
      
      addTiles() |> 
      
      addProviderTiles(provider = providers$OpenStreetMap, group = "OpenStreetMap") |>
      addProviderTiles(provider = providers$CartoDB, group = "CartoDB") |>
      addProviderTiles(provider = providers$Esri, group = "Esri") |>
      
      addLayersControl(
        baseGroups = c("OpenStreetMap",
                       "CartoDB",
                       "Esri"),
        options = layersControlOptions(collapsed = F)
      ) |> 
      
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(imperial = F, metric = T)
      ) |> 
      
      fitBounds(-70.80840 + 0.16, -33.62743, -70.43015, -33.31069) |> 
      
      addMiniMap(
        toggleDisplay = T,
        width = 120, 
        height = 120, 
        strings = list(hideText = "Minimizar Minimapa", 
                       showText = "Mostrar Minimapa"), 
        zoomAnimation = T
      ) |> 
      
      addLegend(
        pal = pal,
        values = data[, input$variableselected2] |> as.numeric(),
        opacity = 0.7,
        title = NULL
      )  
    
  })
  
  observe({
    
    map = datos2()
    labels = label2()
    
    pal = colorBin(palette = "RdYlGn",
                   domain = data[, input$variableselected2] |> as.numeric(),
                   bins = 7,
                   pretty = T,
                   reverse = T)
    
    leafletProxy(mapId = "map2", data = map, session = session) |> 
      
      clearShapes() |> 
      
      addPolygons(
        data = map,
        fillColor = ~pal(variableplot),
        color = "black",
        dashArray = "1",
        weight = 1.5,
        fillOpacity = 0.5,
        opacity = 1,
        label = labels,
        highlightOptions = highlightOptions(color = "black", 
                                            weight = 3,
                                            bringToFront = T)
      )
    
  })
  
  outputOptions(output, "map", suspendWhenHidden = F)
  outputOptions(output, "map2", suspendWhenHidden = F)
  
  #### Análisis Exploratorio de Datos: Servidor ####
  
  output$edades = renderUI({
    tags$iframe(src = "www/Edades.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$edades2 = renderUI({
    tags$iframe(src = "www/Edades2.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$topcancer = renderUI({
    tags$iframe(src = "www/TopCancer.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$muertesporsexo = renderUI({
    tags$iframe(src = "www/MuertesSexo.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$muertesmes = renderUI({
    tags$iframe(src = "www/MuertesMes.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$muertesyearsex = renderUI({
    tags$iframe(src = "www/MuertesYearSex.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$histsmrcomuna = renderUI({
    tags$iframe(src = "www/HistSMRComuna.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$boxplotsmr = renderUI({
    tags$iframe(src = "www/BoxplotSMR.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$smrcomunatemp = renderUI({
    tags$iframe(src = "www/SMRComunaTemp.html", height = "630", width = "100%", frameborder = "0")
  })
  
  #### Modelo: Servidor ####
  
  output$heatmapsmr = renderUI({
    tags$iframe(src = "www/HeatmapSMR.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$heatmapprob = renderUI({
    tags$iframe(src = "www/HeatmapProb.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$heatmapsobre = renderUI({
    tags$iframe(src = "www/HeatmapSobre.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$temporalestr = renderUI({
    tags$iframe(src = "www/TemporalEstr.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$temporalnoestr = renderUI({
    tags$iframe(src = "www/TemporalNoEstr.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$interaccion = renderUI({
    tags$iframe(src = "www/Interaccion.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$mapadinamicoestructurado = renderUI({
    tags$iframe(src = "www/MapaDinamicoEstructurado.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$mapadinamiconoestructurado = renderUI({
    tags$iframe(src = "www/MapaDinamicoNoEstructurado.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$smrcomunatemp2 = renderUI({
    tags$iframe(src = "www/SMRComunaTemp2.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$probsobreporcomuna = renderUI({
    tags$iframe(src = "www/ProbSobrePorComuna.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$sensibilidad1 = renderUI({
    tags$iframe(src = "www/SensibilidadB0.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$sensibilidad2 = renderUI({
    tags$iframe(src = "www/SensibilidadPhi.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$sensibilidad3 = renderUI({
    tags$iframe(src = "www/SensibilidadTau.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$sensibilidad4 = renderUI({
    tags$iframe(src = "www/SensibilidadTauGamma.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$sensibilidad5 = renderUI({
    tags$iframe(src = "www/SensibilidadTauPsi.html", height = "630", width = "100%", frameborder = "0")
  })
  
  output$sensibilidad6 = renderUI({
    tags$iframe(src = "www/SensibilidadTauDelta.html", height = "630", width = "100%", frameborder = "0")
  })
  
}

shinyApp(ui = ui, server = server)
