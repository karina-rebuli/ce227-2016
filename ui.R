library(shiny)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    sidebarLayout(
      
      sidebarPanel(
        h4("Parâmetros")
        , hr()
        ## Population
        , "População"
        , sliderInput("N", label = "Tamanho da população:",
                      min = 2, max = 1e3, value = 1e2, step = 1)
        , sliderInput("Theta", label = "Proporção de votantes no candidato:",
                      min = 0, max = 1e2, value = 30, step = 1)
        , hr()
        
        ## Priori 
        , "Priori"
        , sliderInput("limits.pri", label = "Intervalo de 95% da priori:",
                      min = 0, max = 100, value = c(45, 60) )
        , hr()
        
        ## Amostra
        , "Dados"
        , sliderInput("n", label = "Tamanho da amostra:\n",
                      min = 1, max = 1e3, value = 50, step = 1)
      ),
      
      # Create a spot for the barplot
      mainPanel(
        br()
        , br()
        , h4("Atualização da posteriori", align="center")
        , br()
        , br()
        , plotOutput("distributionsPlot")
      )
      
    )
  )
)