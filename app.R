



# ui ------------------------------------------------------------------

ui <- fluidPage(
    setBackgroundColor("#191970"),
    br(),
    img(src = 'ia.png', width = '100%', height = 150),
    h1('Financial Panel', style = "color:#27a38a", align = 'left'),
    sidebarLayout(
        sidebarPanel(style = "color:#27a38a",
                     width = 3,
                     tags$style(type = 'text/css','.well {background-color: #00244a;}'),

            
# slider ------------------------------------------------------------------
            

            sliderTextInput(inputId = "trajectory", 
                            label = "Date Range:", 
                            choices = choices,
                            selected = c(default - 5000, max(choices)))


# mainpanel ---------------------------------------------------------------


        ),
        mainPanel(
            tabsetPanel(type = 'pills',
                        

# primer tab ---------------------------------------------------------------

                        
                        tabPanel('Primer Tab',
                                 fluidRow(
                                   br(),
                                   splitLayout(cellWidths = c("50%", "50%"), 
                                               highchartOutput("valuetech",height = "500", width = '600'), 
                                               highchartOutput("valuetechpie",height = "500", width = '600')))),
                        
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
    

# primer tab plot 1 ---------------------------------------------------------------
    
    
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
    


  })
  
# primer tab plot 2 ---------------------------------------------------------------
  
  output$valuetechpie <- renderHighchart({
    
    
    
    MSFTvol = (subset(MSFT, Fecha >= input$trajectory[1] & Fecha <= input$trajectory[2]) %>% summarise(volumen = sum(Volumen)))$volumen
    GOOGLvol = (subset(GOOGL, Fecha >= input$trajectory[1] & Fecha <= input$trajectory[2]) %>% summarise(volumen = sum(Volumen)))$volumen
    AAPLvol = (subset(AAPL, Fecha >= input$trajectory[1] & Fecha <= input$trajectory[2]) %>% summarise(volumen = sum(Volumen)))$volumen
    AMZNvol = (subset(AMZN, Fecha >= input$trajectory[1] & Fecha <= input$trajectory[2]) %>% summarise(volumen = sum(Volumen)))$volumen
    
    
    
    Acciones_vol = c("GOOGL Volumen", "MSFT Volumen","AAPL Volumen","AMZN Volumen")
    
    Acumulado_vol = c(GOOGLvol,MSFTvol,AAPLvol,AMZNvol)
    
    df_vol = data.frame(Acciones_vol, Acumulado_vol) %>% arrange(-Acumulado_vol)
    

    
    pie_chart = df_vol %>% 
      hchart('pie', hcaes(x = Acciones_vol, y = Acumulado_vol, color = viridis::rocket(n = 4))) %>% 
      hc_add_theme(hc_theme_flatdark()) %>% 
      hc_tooltip(pointFormat = '<b> Sumatoria de Volumen Operado en USD: </b>  {point.y}') %>% 
      hc_title(text = 'Pie Chart participacion Volumen Operado',
               style = list(fontSize = '15px', fontWeight = 'bold')) %>% 
      hc_credits(enabled = TRUE, text = '@MGaloto',
                 style = list(fontSize = '16px', color = "#ffffff")) %>% 
      hc_subtitle(text = paste('Volumen acumulado desde:  ',input$trajectory[1], ' hasta:  ',input$trajectory[2]),
                  style = list(fontSize = '16px', color = "#ffffff"))
    
    
    pie_chart
    
    
    
    
  })

# primer tab cierre ---------------------------------------------------------------
  
  
  
}




shinyApp(ui = ui, server = server)
