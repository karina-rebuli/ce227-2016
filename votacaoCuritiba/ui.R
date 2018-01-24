library(shiny)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage( 
    
    fluidRow(
      
      column(4,
             ## Population
             h4("População")
             , sliderInput("N", label = "Tamanho da população:",
                           min = 2, max = 1e3, value = 1e3, step = 1)
             , sliderInput("Theta", label = "Proporção de votantes no candidato:",
                           min = 0, max = 1e2, value = 30, step = 1)
      )
      , column(4,
               ## Priori 
               h4("Priori")
               , sliderInput("limits.pri", label = "Intervalo de 95% da priori:",
                             min = 0, max = 100, value = c(45, 60) )
               , hr()
               
               ## Amostra
               , h4("Dados")
               , sliderInput("n", label = "Tamanho da amostra:\n",
                             min = 1, max = 1e3, value = 50, step = 1)
      )
      , column(4,
               ## Plot
               h4("Plot")
               , checkboxGroupInput("plots", ""
                                    , c("Priori" = "pri"
                                        , "Verossimilhança" = "lik"
                                        , "Verossimilhança modificada" = "lik2" 
                                        , "Posteriori por aproximação (analítico)" = "app"
                                        , "Posteriori por aproximação (numérico)" = "app.num"
                                        , "Posteriori por amostragem" = "samp" )
                                    , selected = c("pri", "lik") )
      )
      
    )
    , hr()
    , br()
    , plotOutput("distributionsPlot")
    , hr()
    , fluidRow(
      column(12, includeHTML("exVotacaoCuritiba.html"))
    )
    
  )
)