library(shiny)
library(bslib)

# ==============================================================================
# Simulación Tablero de Galton: Visualización de Probabilidades (Pascal)
# ==============================================================================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f4f4; }
      #canvas-wrapper {
        display: flex;
        justify-content: center;
        background-color: #2c3e50;
        border-radius: 8px;
        padding: 10px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
      }
      canvas {
        background-color: #2c3e50;
        border-radius: 4px;
      }
      .control-panel {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
      }
    ")),
    tags$script(HTML("
      let canvas, ctx;
      let particles = [];
      let pegs = [];
      let bins = [];
      let animationId;
      let isRunning = false;
      
      // Variables para visualización de probabilidades
      let showProbabilities = false;
      let targetRowIndex = 0; // Fila seleccionada por el usuario (base 0)
      let rowProbabilities = []; // Textos de probabilidad
      
      // --- FÍSICA ---
      const gravity = 0.4;        
      const friction = 0.96;      
      const restitution = 0.6;    
      const pegRadius = 4;
      const particleRadius = 4.5;
      
      // Dimensiones
      let rows = 12;
      let spacingX = 40;
      let spacingY = 35;
      let startX, startY;
      
      $(document).on('shiny:connected', function() {
        canvas = document.getElementById('galtonCanvas');
        ctx = canvas.getContext('2d');
        
        let dpr = window.devicePixelRatio || 1;
        canvas.width = 800 * dpr;
        canvas.height = 600 * dpr;
        ctx.scale(dpr, dpr);
        canvas.style.width = '800px';
        canvas.style.height = '600px';
        
        resetBoard();
      });

      Shiny.addCustomMessageHandler('control_sim', function(msg) {
        if (msg.action === 'start') {
          rows = msg.rows;
          // Si cambia el número de filas o no hay partículas, reiniciamos
          if (!isRunning || particles.length > 0 || pegs.length === 0) resetBoard();
          isRunning = true;
          spawnParticles(msg.count);
          animate();
        } else if (msg.action === 'reset') {
          isRunning = false;
          cancelAnimationFrame(animationId);
          resetBoard();
        } else if (msg.action === 'update_probs') {
          // Actualizar solo la visualización de probabilidades
          showProbabilities = msg.show;
          targetRowIndex = msg.row_index; 
          rowProbabilities = msg.values;
          
          // Si la animación está parada, forzamos un redibujado para ver los números
          if (!isRunning) draw();
        }
      });

      function resetBoard() {
        particles = [];
        pegs = [];
        bins = new Array(rows + 1).fill(0);
        
        startX = 800 / 2;
        startY = 60;

        // Crear Clavos y guardarlos con su índice de fila/columna
        for (let r = 0; r < rows; r++) {
          for (let c = 0; c <= r; c++) {
            let x = startX + (c - r/2) * spacingX;
            let y = startY + r * spacingY;
            pegs.push({x: x, y: y, r: r, c: c});
          }
        }
        draw();
      }

      function spawnParticles(total) {
        let spawned = 0;
        let interval = setInterval(() => {
          if (!isRunning) { clearInterval(interval); return; }
          
          let x_jitter = (Math.random() - 0.5) * 2; 
          
          particles.push({
            x: startX + x_jitter,
            y: 10,
            vx: 0,
            vy: 0,
            stopped: false,
            color: `hsl(${Math.random() * 360}, 70%, 65%)`
          });
          
          spawned++;
          if (spawned >= total) clearInterval(interval);
        }, 15); 
      }

      function update() {
        let floorY = startY + rows * spacingY + 30;

        for (let p of particles) {
          if (p.stopped) continue;

          p.vy += gravity;
          p.vx *= friction; 
          p.x += p.vx;
          p.y += p.vy;

          // Colisión con Clavos
          for (let peg of pegs) {
            let dx = p.x - peg.x;
            let dy = p.y - peg.y;
            let distSq = dx*dx + dy*dy;
            let minDist = pegRadius + particleRadius;
            
            if (distSq < minDist * minDist) {
              let angle = Math.atan2(dy, dx);
              let dist = Math.sqrt(distSq);
              let overlap = minDist - dist;
              p.x += Math.cos(angle) * overlap;
              p.y += Math.sin(angle) * overlap;
              
              let direction = Math.random() < 0.5 ? -1 : 1;
              p.vy = -Math.abs(p.vy) * restitution; 
              p.vx = direction * (1.5 + Math.random()); 
            }
          }

          // Suelo
          if (p.y >= floorY) {
            p.y = floorY;
            p.stopped = true;
            let relativeX = p.x - startX;
            let binIndex = Math.round((relativeX / spacingX) + (rows / 2));
            if (binIndex < 0) binIndex = 0;
            if (binIndex > rows) binIndex = rows;
            bins[binIndex]++;
          }
        }
      }

      function draw() {
        ctx.clearRect(0, 0, 800, 600);

        // 1. Dibujar Clavos
        for (let i = 0; i < pegs.length; i++) {
          let peg = pegs[i];
          ctx.beginPath();
          ctx.arc(peg.x, peg.y, pegRadius, 0, Math.PI * 2);
          
          // Lógica de resaltado
          if (showProbabilities && peg.r === targetRowIndex) {
            ctx.fillStyle = '#f1c40f'; // Amarillo
            ctx.shadowBlur = 10;
            ctx.shadowColor = '#f1c40f';
          } else {
            ctx.fillStyle = 'rgba(255,255,255,0.3)';
            ctx.shadowBlur = 0;
          }
          ctx.fill();
          ctx.shadowBlur = 0; 
          
          // Dibujar probabilidades (Texto)
          if (showProbabilities && peg.r === targetRowIndex && rowProbabilities[peg.c]) {
             ctx.fillStyle = '#f1c40f';
             ctx.font = 'bold 14px Arial';
             ctx.textAlign = 'center';
             ctx.fillText(rowProbabilities[peg.c], peg.x, peg.y - 12);
          }
        }

        // 2. Dibujar Histograma
        let floorY = startY + rows * spacingY + 30;
        let binWidth = spacingX - 4;
        
        for (let i = 0; i < bins.length; i++) {
            if (bins[i] === 0) continue;
            let h = bins[i] * 5; 
            let bx = startX + (i - rows/2) * spacingX;
            
            ctx.fillStyle = 'rgba(52, 152, 219, 0.6)';
            ctx.fillRect(bx - binWidth/2, floorY - h, binWidth, h);
            ctx.strokeStyle = '#3498db';
            ctx.strokeRect(bx - binWidth/2, floorY - h, binWidth, h);
            
            ctx.fillStyle = '#fff';
            ctx.font = '12px Arial';
            ctx.textAlign = 'center';
            ctx.fillText(bins[i], bx, floorY - h - 5);
        }

        // 3. Dibujar Partículas
        for (let p of particles) {
          ctx.beginPath();
          ctx.arc(p.x, p.y, particleRadius, 0, Math.PI * 2);
          if (p.stopped) {
             ctx.fillStyle = p.color; 
             ctx.globalAlpha = 0.4;
          } else {
             ctx.fillStyle = p.color;
             ctx.globalAlpha = 1.0;
          }
          ctx.fill();
          ctx.globalAlpha = 1.0;
        }
      }

      function animate() {
        if (!isRunning) return;
        update();
        draw();
        animationId = requestAnimationFrame(animate);
      }
    "))
  ),
  
  titlePanel("Tablero de Galton: Simulación + Teoría"),
  
  sidebarLayout(
    sidebarPanel(
      class = "control-panel",
      width = 3,
      h4("Simulación"),
      sliderInput("rows", "Total de Niveles (N):", min=8, max=16, value=12, step=1),
      sliderInput("count", "Cantidad de Bolas:", min=50, max=1000, value=300, step=50),
      actionButton("btn_start", "Lanzar Bolas", class="btn-success w-100", icon=icon("play")),
      br(), br(),
      actionButton("btn_reset", "Limpiar", class="btn-secondary w-100", icon=icon("eraser")),
      
      hr(),
      
      h4("Análisis Teórico"),
      p("Visualiza la probabilidad (Triángulo de Pascal) en cada nivel."),
      
      # Checkbox simple para evitar problemas de CSS externos
      checkboxInput("show_probs", "Mostrar Probabilidades", value = FALSE),
      
      # Slider dinámico (aparece cuando hay filas definidas)
      uiOutput("prob_slider_ui")
    ),
    
    mainPanel(
      width = 9,
      div(id = "canvas-wrapper",
          tags$canvas(id = "galtonCanvas")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Slider para seleccionar fila a inspeccionar
  output$prob_slider_ui <- renderUI({
    req(input$rows)
    sliderInput("target_row", 
                "Inspeccionar Fila #:", 
                min = 1, 
                max = input$rows, 
                value = 1, 
                step = 1,
                animate = animationOptions(interval = 800, loop = FALSE))
  })
  
  # Control Simulación
  observeEvent(input$btn_start, {
    session$sendCustomMessage("control_sim", list(
      action = "start",
      rows = input$rows,
      count = input$count
    ))
  })
  
  observeEvent(input$btn_reset, {
    session$sendCustomMessage("control_sim", list(
      action = "reset",
      rows = input$rows
    ))
  })
  
  # Control Probabilidades (Teoría)
  observe({
    req(input$rows, input$target_row)
    
    # Índice base 0 para JS
    r_idx <- input$target_row - 1 
    
    # Cálculo de probabilidad binomial
    probs <- dbinom(0:r_idx, size = r_idx, prob = 0.5)
    probs_txt <- paste0(round(probs * 100, 1), "%")
    
    session$sendCustomMessage("control_sim", list(
      action = "update_probs",
      show = isTRUE(input$show_probs),
      row_index = r_idx,
      values = probs_txt
    ))
  })
}

shinyApp(ui, server)