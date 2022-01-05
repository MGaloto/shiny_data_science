



# ui ------------------------------------------------------------------

ui <- fluidPage(
    setBackgroundColor("#191970"),
    br(),
    img(src = 'ia.png', width = '100%', height = 150),
    h1('Financial Panel', style = "color:#27a38a", align = 'left'),
    sidebarLayout(
        sidebarPanel(style = "color:#27a38a",
                     tags$style(type = 'text/css','.well {background-color: #00244a;}'),

            
# slider ------------------------------------------------------------------
            

            sliderTextInput(inputId = "trajectory", 
                            label = "Date Range:", 
                            choices = choices,
                            selected = c(default, max(choices)))


# mainpanel ---------------------------------------------------------------


        ),
        mainPanel(
            tabsetPanel(type = 'pills',
                        

# primer tab ---------------------------------------------------------------

                        
                        tabPanel('Primer Tab',
                                 fluidRow(
                                   br(),
                                   highchartOutput("valuetech",height = "500", width = '1000'))),
                        
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
  
  output$valuetech <- renderHighchart({
    

# primer tab plot ---------------------------------------------------------------
    
    
    MSFTdis = round((MSFT$Close[MSFT$Fecha == input$trajectory[2]] - MSFT$Close[MSFT$Fecha == input$trajectory[1]])  / MSFT$Close[MSFT$Fecha == input$trajectory[1]],4)*100
    GOOGLdis = round((GOOGL$Close[GOOGL$Fecha == input$trajectory[2]] - GOOGL$Close[GOOGL$Fecha == input$trajectory[1]])  / GOOGL$Close[GOOGL$Fecha == input$trajectory[1]],4)*100
    AAPLdis = round((AAPL$Close[AAPL$Fecha == input$trajectory[2]] - AAPL$Close[AAPL$Fecha == input$trajectory[1]])  / AAPL$Close[AAPL$Fecha == input$trajectory[1]],4)*100
    AMZNdis = round((AMZN$Close[AMZN$Fecha == input$trajectory[2]] - AMZN$Close[AMZN$Fecha == input$trajectory[1]])  / AMZN$Close[AMZN$Fecha == input$trajectory[1]],4)*100
    
    
    Acciones = c("GOOGL","MSFT","AAPL","AMZN")
    
    Variacion = c(GOOGLdis,MSFTdis,AAPLdis,AMZNdis)
    
    df = data.frame(Acciones, Variacion) %>% arrange(-Variacion)
    
    
    bar_plot = df %>% 
      arrange(desc(Variacion)) %>% 
      hchart('column', hcaes(x = Acciones, y = Variacion, color = custom)) %>% 
      hc_add_theme(hc_theme_flatdark()) %>% 
      hc_tooltip(pointFormat = '<b>Variacion %: </b> {point.y} <br> ' ) %>% 
      hc_title(text = 'Acciones Tecnologicas',
               style = list(fontSize = '25px', fontWeight = 'bold')) %>% 
      hc_subtitle(text = paste('Variacion % desde:  ',input$trajectory[1], ' hasta:  ',input$trajectory[2]),
                  style = list(fontSize = '16px', color = "#ffffff")) %>% 
      hc_credits(enabled = TRUE, text = '@MGaloto',
                 style = list(fontSize = '16px', color = "#ffffff")) %>% 
      hc_yAxis(labels = list(style = list(color = "#ffffff")),
               tickColor = "#ffffff") %>% 
      hc_xAxis(labels = list(style = list(color = "#ffffff")),
               tickColor = "#ffffff")
    
    bar_plot
    
# primer tab cierre ---------------------------------------------------------------

  })


}




shinyApp(ui = ui, server = server)
