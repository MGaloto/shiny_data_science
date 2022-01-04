

library(shiny)
library(shinyWidgets)

# ui ------------------------------------------------------------------

ui <- fluidPage(
    setBackgroundColor("#191970"),
    br(),
    img(src = 'ia.png', width = '100%', height = 150),
    h1('Data Science Panel Control', style = "color:#27a38a", align = 'left'),
    sidebarLayout(
        sidebarPanel(style = "color:#27a38a",
                     tags$style(type = 'text/css','.well {
                       background-color: #00244a;}'),
            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}")),
            
# slider ------------------------------------------------------------------

            sliderInput(inputId = "slider",
                        label = "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),

# select ------------------------------------------------------------------

            selectInput(inputId = "select",
                        label = p('Seleccione la variable: ',  style="color:#27a38a"),
                        choices = c('Grafico 1','Grafico 2','Grafico 3'))
        ),
        mainPanel(
            tabsetPanel(type = 'pills',
                        
                        tabPanel('Primer Tab',
                                 fluidRow(
                                     p('Primero'))),
                        
                        tabPanel('Segundo Tab',
                                 fluidRow(
                                     p('Segundo'))),
                        tabPanel('Tercer Tab',
                                 fluidRow(
                                     p('3'))),
                        
                        tabPanel('Cuarto Tab',
                                 fluidRow(
                                     p('4'))),
                        tabPanel('Quinto Tab',
                                 fluidRow(
                                     p('5'))),
                        
                        tabPanel('Sexto Tab',
                                 fluidRow(
                                     p('6')))
                            

                        )
                    )
        )
  )






server <- function(input, output) {


}





shinyApp(ui = ui, server = server)
