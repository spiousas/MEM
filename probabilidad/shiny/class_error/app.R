#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, gt, dplyr, tibble, ggplot2, tidyr, readr, shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
              
  # Application title
  titlePanel("Error de clasificación"),
  # Sidebar ####
  sidebarLayout(
    sidebarPanel(
           sliderInput("N",
                       "Valores posibles de X (n):",
                       min = 1,
                       max = 15,
                       value = 9)
  ),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        # Raw data ####
        "Un g(x)",
  fluidRow(
    h2("La conjunta:"),
    p("Sea un vector aleatorio discreto (X,Y) con la función de probabilidad puntual conjunta dada por:"),
    gt_output(outputId = "tablepxy"),
    p("Y n posibles valores de X mientras que sólo 2 posibles valores de Y (0 y 1)."),
    p("Esta pxy está generada aleatoriamente, si desea generar otra cambie el valor de n."),
    p("Dada una función de clasificación g(x) (con n 0s y 1s), podemos definir el error de clasifiacción como:
      P(g(X) != Y)")
  ),
  fluidRow(
    p("A continuación puede ingresar manualmente un g(x) y se calculará su error (tenga en cuenta que el 
      número de elementos del vector tiene que coincidir con n.")),
  fluidRow(column(width = 8,
                  textInput("g", "Ingrese manualmente un g(x) delimitado por comas",
                            ,"0,1,1,0,1,1,0,1", width = "100%")),
           column(width = 2,
                  actionButton(inputId = "ok", label = "OK!"))),
  # fluidRow(
  #   actionButton("createg", 
  #                label = "O cree un nuevo g(x) al azar")
  # ),
  fluidRow(
    gt_output(outputId = "tableg")
  ),
  fluidRow(column(width = 12,
                  verbatimTextOutput("textError"))
  )),
  tabPanel(
    # Todos los g(s) ####
    "Todos los g(x)",
  fluidRow(
    p("A continuación exploraremos todas las posibles n*n funciones de clasifiación g(x) distintas."),
                  p("El error más chico de clasificación es de: "),
                  verbatimTextOutput("textgs")
  ),
  fluidRow(p("Para el g(x):"),
             gt_output(outputId = "tablegs")),
  fluidRow(
    p("En la siguiente figura se muestra el error de clasificación para las n*n posibles g(x).
      Los errores están ordenados de mayor a menor en de fondo se ve, de forma vertical y de abajo hacia arribam
      la estructura del g(x) en color verde cuando hay un 1 y rojo cuando hay un 0. De esta forma podemos ver 
      cuales son las clasificaciones que más minimizan el error."),
      plotOutput("gsPlot"),
  )
)))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pxy <- reactive({
    pxy <- matrix(runif(n = input$N*2, min = 0, max = 1), 
                  nrow = 2, 
                  ncol =input$N)
    pxy <- pxy/sum(pxy)
    colnames(pxy) <- paste0("X", 1:input$N)
    pxy
  })
  
  pxy_tbl <- reactive({
    as_tibble(pxy()) %>% 
      bind_cols(tibble(Y = c(0,1))) %>%
      relocate(Y, .before = X1) %>%
      gt() %>%
      fmt_number(
        columns = starts_with("X"),
        decimals = 3,
        use_seps = FALSE)
  })
  
  output$tablepxy <- render_gt(expr = pxy_tbl()) 
  
  # g <- eventReactive(input$createg, {
  #   g <- matrix(rbinom(n = input$N, size = 1, prob = .5))
  #   g <- t(g)
  #   colnames(g) <- paste0("X", 1:input$N)
  #   g
  # })

  g <- eventReactive(input$ok, {
    g <-as.numeric(unlist(strsplit(input$g,",")))
    g <- t(g)
    colnames(g) <- paste0("X", 1:input$N)
    g
  })
  
  g_tbl <- reactive({
    as_tibble(g()) %>% 
      gt() %>%
      fmt_number(
        columns = starts_with("X"),
        decimals = 0,
        use_seps = FALSE) %>%
      data_color(
        direction = "row",
        method = "numeric",
        palette = c("#DE3C4B", "#57CC99")
      )
  })
  
  output$tableg <- render_gt(expr = g_tbl()) 
  
  error <- reactive({
    sum(pxy()[1,g()!=0]) + sum(pxy()[2,g()!=1])
  })
  
  output$textError <- renderText({ paste0("El error de clasificación es: ", round(error(), digits =3 ))})
  
  gs <- reactive({
    l <- rep(list(0:1), input$N)
    gs <- expand.grid(l)
    colnames(gs) <- paste0("X", 1:input$N)
    gs
  })

  errors <- reactive({
    errors <- c()
    for (i in 1:2^input$N) {
      errors[i] <- sum(pxy()[1,gs()[i,]!=0]) + sum(pxy()[2,gs()[i,]!=1])
    }
    errors
  })
  
  error_tbl <- reactive({
    as_tibble(gs()) %>%
      bind_cols(tibble(error = errors()))%>%
      arrange(desc(error)) %>%
      mutate(order = row_number())
  })

  gs_tbl <- reactive({
    error_tbl() %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "X",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    mutate(X = (parse_number(X)-1)/(input$N-1)*(max(error)-min(error)) + min(error))
  })

  output$textgs <- renderText({ round(min(errors()), digits =3 )})
  
  gs_table <- reactive({
    error_tbl() %>% 
      filter(order == max(order)) %>%
      select(starts_with("X")) %>%
      gt() %>%
      fmt_number(
        columns = starts_with("X"),
        decimals = 0,
        use_seps = FALSE) %>%
      data_color(
        direction = "row",
        method = "numeric",
        palette = c("#DE3C4B", "#80ED99")
      )
  })
  
  output$tablegs <- render_gt(expr = gs_table()) 
  
  output$gsPlot <- renderPlot({ggplot() +
      geom_tile(data = gs_tbl(),
                aes(x=order, y = X, fill = factor(value)),
                alpha = .4) +
      geom_line(data = error_tbl(),
                aes(x=order, y = error),
                linewidth = 2,
                color = "gray20") +
      scale_fill_manual(values = c("#DE3C4B", "#57CC99")) +
      labs(x = "Orden de mayor a menor error",
           y = "Error de clasificación",
           fill = "Estructura de la g(x)") +
      theme_bw() +
      theme(legend.position = "top")})

}

# Run the application 
shinyApp(ui = ui, server = server)
