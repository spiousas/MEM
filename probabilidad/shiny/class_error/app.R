#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, gt, dplyr, tibble, ggplot2, tidyr, readr)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Error de clasificación"),
  # Sidebar ####
  sidebarLayout(
    sidebarPanel(
           sliderInput("N",
                       "Valores posibles de X:",
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
    gt_output(outputId = "tablepxy")
  ),
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
  fluidRow(column(width = 12,
                  verbatimTextOutput("textgs"))
  ),
  fluidRow(gt_output(outputId = "tablegs")),
  fluidRow(
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
        palette = c("red", "green")
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

  output$textgs <- renderText({ paste0("El error más chico de clasificación es de: ", round(min(errors()), digits =3 ),
                                       "\nPara el g(x):")})
  
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
        palette = c("red", "green")
      )
  })
  
  output$tablegs <- render_gt(expr = gs_table()) 
  
  output$gsPlot <- renderPlot({ggplot() +
      geom_tile(data = gs_tbl(),
                aes(x=order, y = X, fill = factor(value)),
                alpha = .4) +
      geom_line(data = error_tbl(),
                aes(x=order, y = error),
                linewidth = 2) +
      scale_fill_manual(values = c("red", "green")) +
      labs(x = "Orden de mayor a menor error",
           y = "Error de clasificación",
           fill = "Estructura de la g(x)") +
      theme_bw() +
      theme(legend.position = "top")})

}

# Run the application 
shinyApp(ui = ui, server = server)
