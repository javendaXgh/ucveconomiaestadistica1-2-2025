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
  title = "Recolección de datos",
  f7SingleLayout(
    navbar = f7Navbar(
      title = "Recolector de datos",
      hairline = TRUE,
      shadow = TRUE,
      backButton = FALSE
    ),
    f7Card(
      f7Text("peso", "Peso (kg)"),
      f7Text("altura", "Altura (cm)"),
      f7Text("edad", "Edad (años)"),
      f7Select(inputId ="hermanos",
               label="Cantidad de hermanos",
               choices=0:12,
               selected=0),
      f7Text("codigo_postal", "Código Postal"),
      f7Select(
        inputId = "sexo",
        label = "Sexo",
        choices = c( "Femenino","Masculino", "Otro")
      ),
      f7Select(inputId= 'dia_nacimiento',
             label= 'Día de Nacimiento:',
             choices=1:31,
             selected=NULL,
             # multiple=FALSE#,
             # position = c("left")
             ),
      f7Select(
        inputId= 'consumidor',
        label='¿Consume o ha consumido drogas? (Sí/No):',
        choices = c('Sí', 'No','Prefiero no responder'),
        selected = NULL,
        # position = c("left"),
        # style = list(inset = FALSE, outline = FALSE, dividers = FALSE, strong = FALSE)
      ),
      
      # f7Select(inputId= 'consumidor', 
      #        label='¿Consume o ha consumido drogas? (Sí/No):',
      #        ),
  
  # Ref7Select
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

