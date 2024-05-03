#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)

# Load data
data(spotify)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  spotify <- spotify %>% 
    select(artist, title, popularity) %>% 
    mutate(artist = fct_reorder(artist, popularity, .fun = 'mean'))
  
  artist_means <- spotify %>% 
    group_by(artist) %>% 
    summarize(count = n(), popularity = mean(popularity))
  
  spotify_hierarchical <- stan_glmer(
    popularity ~ (1 | artist), 
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735)
  
  set.seed(84735)
  predictions_hierarchical <- posterior_predict(spotify_hierarchical, 
                                                newdata = artist_means)
  
    output$distPlot <- renderPlot({
      ppc_intervals(artist_means$popularity, yrep = predictions_hierarchical, 
                    prob_outer = 0.80) +
        ggplot2::scale_x_continuous(labels = artist_means$artist, 
                                    breaks = 1:nrow(artist_means)) +
        xaxis_text(angle = 90, hjust = 1) + 
        geom_hline(yintercept = 58.4, linetype = "dashed")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
