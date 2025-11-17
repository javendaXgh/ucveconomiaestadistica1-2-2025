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
      f7Text("promedio_academico", "Promedio académico actual (GPA)"),
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
        choices = c("Sí", "No", "Prefiero no responder")
      ),
      f7Button("submit", "Enviar datos")
    )
  )
)

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
      fecha_hora = Sys.time()
    )
    
    # Generar un nombre de archivo único
    nombre_archivo <- paste0("datos_recolectados/datos_", 
                             format(Sys.time(), "%Y%m%d_%H%M%S"), 
                             ".csv")
    
    # Guardar los datos en un archivo CSV
    write.csv(datos, nombre_archivo, row.names = FALSE)
    
    # Mostrar un mensaje de confirmación
    f7Toast("Datos enviados correctamente!", position = "bottom")
    
    # Close the app 5 seconds after sending the data
    # shiny::observe({
    #   invalidateLater(5000, session)
    #   session$close()
    # })
    
    # Limpiar los campos del formulario
    # updateF7Text(session, "peso", value = "")
    # updateF7Text(session, "altura", value = "")
    # updateF7Text(session, "edad", value = "")
    # updateF7Text(session, "hermanos", value = "")
    # updateF7Text(session, "codigo_postal", value = "")
    # 
    # # Debugging: Print input values to check their structure
    # # print(str(input$sexo))
    # # print(str(session))
    # 
    # # Simplify and ensure proper reset of `sexo`
    # updateF7Select(session, "sexo")
  })
}

shinyApp(ui = ui, server = server, options = list(port = 8080))

