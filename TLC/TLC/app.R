library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Interfaz de Usuario
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Simulación del Teorema del Límite Central (TLC)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Seleccione la Distribución:",
                  choices = c("Binomial", "Poisson", "Hipergeométrica", "Uniforme", "Exponencial")),
      
      hr(),
      helpText("Configuración de la Simulación:"),
      sliderInput("n", "Tamaño de la muestra (n):", min = 2, max = 100, value = 30),
      sliderInput("reps", "Número de repeticiones (R):", min = 1000, max = 20000, value = 10000, step = 1000),
      actionButton("run", "Simular Ahora", icon = icon("sync"), class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Distribución Original", 
                 plotOutput("plotPob"),
                 helpText("Representación de la población base.")),
        
        tabPanel("Simulación TLC", 
                 plotOutput("plotTLC"),
                 helpText("Distribución de las medias muestrales aproximándose a la Normal."))
      )
    )
  )
)

# Lógica del Servidor
server <- function(input, output) {
  
  # Definición de colores por distribución
  colores <- reactive({
    switch(input$dist,
           "Binomial" = "#90EE90",       # Verde claro (como tu captura)
           "Poisson" = "#87CEEB",        # Azul cielo
           "Hipergeométrica" = "#FFB347", # Naranja pastel
           "Uniforme" = "#BA55D3",       # Púrpura medio
           "Exponencial" = "#20B2AA")    # Verde mar claro
  })
  
  # Datos de la población original
  getPobData <- reactive({
    set.seed(123)
    dist <- input$dist
    if(dist == "Binomial") return(data.frame(x = rbinom(5000, size = 10, prob = 0.4)))
    if(dist == "Poisson") return(data.frame(x = rpois(5000, lambda = 3)))
    if(dist == "Hipergeométrica") return(data.frame(x = rhyper(5000, m = 20, n = 30, k = 10)))
    if(dist == "Uniforme") return(data.frame(x = runif(5000, min = 0, max = 10)))
    if(dist == "Exponencial") return(data.frame(x = rexp(5000, rate = 0.5)))
  })
  
  # Gráfico 1: Población Original
  output$plotPob <- renderPlot({
    df_pob <- getPobData()
    color_fill <- colores()
    
    p <- ggplot(df_pob, aes(x = x)) +
      labs(title = paste("Población Original:", input$dist), x = "Valor", y = "Frecuencia") +
      theme_minimal(base_size = 15)
    
    if(input$dist %in% c("Binomial", "Poisson", "Hipergeométrica")) {
      p + geom_bar(fill = color_fill, color = "white", alpha = 0.8)
    } else {
      p + geom_histogram(fill = color_fill, color = "white", alpha = 0.8, bins = 30)
    }
  })
  
  # Simulación de Medias
  sim_medias <- eventReactive(input$run, {
    dist <- input$dist
    n <- input$n
    R <- input$reps
    
    res <- replicate(R, {
      muestra <- switch(dist,
                        "Binomial" = rbinom(n, size = 10, prob = 0.4),
                        "Poisson" = rpois(n, lambda = 3),
                        "Hipergeométrica" = rhyper(n, m = 20, n = 30, k = 10),
                        "Uniforme" = runif(n, min = 0, max = 10),
                        "Exponencial" = rexp(n, rate = 0.5))
      mean(muestra)
    })
    data.frame(media = res)
  }, ignoreNULL = FALSE)
  
  # Gráfico 2: Simulación TLC
  output$plotTLC <- renderPlot({
    df_sim <- sim_medias()
    color_fill <- colores()
    
    ggplot(df_sim, aes(x = media)) +
      geom_histogram(aes(y = after_stat(density)), bins = 35, 
                     fill = color_fill, color = "white", alpha = 0.7) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(df_sim$media), sd = sd(df_sim$media)), 
                    color = "red", linetype = "dashed", linewidth = 1.2) +
      labs(title = paste("Medias Muestrales (n =", input$n, ")"),
           subtitle = "La curva roja indica la tendencia Normal esperada",
           x = "Media Calculada", y = "Densidad") +
      theme_minimal(base_size = 15)
  })
}

shinyApp(ui = ui, server = server)