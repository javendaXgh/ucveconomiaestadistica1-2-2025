# aplicacion con shinymobile para recolectar datos:
# peso
# altura
# edad
# cantidad de hermanos
# cantidad de hijos
# codigo postal
# sexo
# cantidad de materias cursadas
# trabaja si, no
# medio de transporte (bus, caminando, bicicleta, metro, carro, otro)
# distancia de su casa a la Universidad en km
# red social preferida (instagram, facebook, tiktok, twitter, otra)
# horas de estudio promedio a la semana
# promedio académico
# día de nacimiento
# consume o ha consumido drogas


# los datos son guardados en la carpetas `datos_recolectados` en formato .csv

library(tidyverse)
library(shiny)
library(shinyMobile)
library(googlesheets4)


if(!dir.exists("datos_recolectados")){
  dir.create("datos_recolectados")
}
ui <- f7Page(
  title = "UCV-Escuela de Economía. Estadística I",
  f7SingleLayout(
    navbar = f7Navbar(
      title = "Recolector de datos",
      hairline = TRUE,
      shadow = TRUE,
      backButton = FALSE
    ),
    f7Card(
      f7Text("edad", "Edad (años)"),
      f7Select(
        inputId = "sexo",
        label = "¿Cuál es su sexo biológico?",
        choices = c("Femenino", "Masculino", "Otro")
      ),
      f7Select(
        inputId = "hermanos",
        label = "¿Cuántos hermanos y hermanas (vivos) tiene en total?",
        choices = 0:12,
        selected = 0
      ),
      f7Text("codigo_postal", "Código Postal"),
      f7Select(
        inputId = "dia_nacimiento",
        label = "¿En qué día del mes nació?",
        choices = 1:31
      ),
      f7Select(
        inputId = "hijos",
        label = "¿Cuántos hijos/hijas tiene?",
        choices = 0:10,
        selected = 0
      ),
      f7Text("distancia_universidad", "Distancia a la Universidad (km)"),
      f7Select(
        inputId = "transporte",
        label = "¿Cuál es su principal medio de transporte para llegar a la Universidad?",
        choices = c("Bus", "Caminando", "Bicicleta", "Metro", "Carro particular", "Otro")
      ),
      f7Select(
        inputId = "trabaja",
        label = "¿Actualmente trabaja a tiempo parcial o completo?",
        choices = c("Sí", "No")
      ),
      f7Select(
        inputId = "materias_cursadas",
        label = "¿Cuántas asignaturas está cursando este semestre?",
        choices = 1:10
      ),
      f7Text("horas_estudio", "Horas de estudio promedio a la semana"),
      f7Text("promedio_academico", "Promedio académico actual"),
      f7Text("altura", "Altura (m)"),
      f7Text("peso", "Peso (kg)"),
      f7Select(
        inputId = "red_social",
        label = "¿Cuál es su red social preferida?",
        choices = c("Instagram", "Facebook", "TikTok", "X (antes Twitter)", "Otra")
      ),
      f7Select(
        inputId = "consumo_drogas",
        label = "¿Consume o ha consumido drogas ilícitas recreacionales en el último año?",
        choices = c("Sí", "No", "Prefiero no responder"),
        selected = "No"
      ),
      f7Button("submit", "Enviar datos")
    )
  )
)

# Cargar el paquete googlesheets4
library(googlesheets4)

# Autenticación no interactiva con credenciales de servicio
gs4_auth(path = "creden.json")  # Asegúrate de que el archivo esté en el mismo directorio que la aplicación

# ID o URL de la Google Sheet donde se almacenarán los datos
sheet_id <- "https://docs.google.com/spreadsheets/d/1aj4vZmvJtIiNy4ET1F0kMJXsqaYeyuEIFQ0tuHKI3YU/edit?usp=sharing"  # Reemplaza con el ID o URL de tu hoja de cálculo

server <- function(input, output, session) {  
  observeEvent(input$submit, {
    # Crear un data frame con los datos recolectados
    datos <- data.frame(
      peso = as.numeric(input$peso),
      altura = as.numeric(input$altura),
      edad = as.numeric(input$edad),
      hermanos = as.numeric(input$hermanos),
      codigo_postal = input$codigo_postal,
      sexo = as.character(input$sexo[1]),
      dia_nacimiento = as.numeric(input$dia_nacimiento),
      hijos = as.numeric(input$hijos),
      distancia_universidad = as.numeric(input$distancia_universidad),
      transporte = as.character(input$transporte),
      trabaja = as.character(input$trabaja),
      materias_cursadas = as.numeric(input$materias_cursadas),
      horas_estudio = as.numeric(input$horas_estudio),
      promedio_academico = as.numeric(input$promedio_academico),
      red_social = as.character(input$red_social),
      consumo_drogas = as.character(input$consumo_drogas),
      fecha_hora = Sys.time()
    )

    # Enviar los datos a la Google Sheet
    sheet_append(ss = sheet_id, data = datos)

    # Mostrar un mensaje de confirmación
    f7Toast("Datos enviados correctamente a Google Sheets!", position = "bottom")
  })
}

shinyApp(ui = ui, server = server, options = list(port = 8080))
