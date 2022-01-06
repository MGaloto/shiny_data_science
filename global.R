
# Librerias

library(corrplot) 
library(quantmod)
library(highcharter)
library(tidyr)
library(ggplot2)
library(readr)  
library(dplyr) 
library(plotly)
library(crayon) 
library(modeest)
library(readxl)
library(plotly)
library(ggthemes)
library(reshape)
library(viridisLite)
library(viridis)
library(shinyWidgets)
library(lubridate)

# --------------------------------------------------------------------------------------- #

# Importar acciones

AAPL=as.data.frame(getSymbols("AAPL" , src = 'yahoo', auto.assign = F, from = "2004-08-19", to = Sys.Date(), periodicity = "daily"))
AMZN=as.data.frame(getSymbols("AMZN" , src = 'yahoo', auto.assign = F, from = "2004-08-19", to = Sys.Date(), periodicity = "daily"))
MSFT=as.data.frame(getSymbols("MSFT" , src = 'yahoo', auto.assign = F, from = "2004-08-19", to = Sys.Date(), periodicity = "daily"))
GOOGL=as.data.frame(getSymbols("GOOGL" , src = 'yahoo', auto.assign = F, from = "2004-08-19", to = Sys.Date(), periodicity = "daily"))


# --------------------------------------------------------------------------------------- #

# Se agregan las Fechas como variables

AAPL$Fecha <- row.names(AAPL)
AMZN$Fecha <- row.names(AMZN)
MSFT$Fecha <- row.names(MSFT)
GOOGL$Fecha <- row.names(GOOGL)


# --------------------------------------------------------------------------------------- #

# Se mofidican las columnas

colnames(AAPL) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
colnames(AMZN) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
colnames(MSFT) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
colnames(GOOGL) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")

# --------------------------------------------------------------------------------------- #

# Se numeran las filas

rownames(AAPL) = 1:nrow(AAPL)
rownames(AMZN) = 1:nrow(AMZN)
rownames(MSFT) = 1:nrow(MSFT)
rownames(GOOGL) = 1:nrow(GOOGL)


# --------------------------------------------------------------------------------------- #

# Fechas formatos


GOOGL$Fecha = as.Date(GOOGL$Fecha)
AAPL$Fecha = as.Date(AAPL$Fecha)
AMZN$Fecha = as.Date(AMZN$Fecha)
MSFT$Fecha = as.Date(MSFT$Fecha)


# --------------------------------------------------------------------------------------- #

# Variables de prueba



MSFTdis = round((MSFT$Close[MSFT$Fecha == '2022-01-04'] - MSFT$Close[MSFT$Fecha == '2020-09-24'])  / MSFT$Close[MSFT$Fecha == '2020-09-24'],4)*100
GOOGLdis = round((GOOGL$Close[GOOGL$Fecha == '2022-01-04'] - GOOGL$Close[GOOGL$Fecha == '2020-09-24'])  / GOOGL$Close[GOOGL$Fecha == '2020-09-24'],4)*100
AAPLdis = round((AAPL$Close[AAPL$Fecha == '2022-01-04'] - AAPL$Close[AAPL$Fecha == '2020-09-24'])  / AAPL$Close[AAPL$Fecha == '2020-09-24'],4)*100
AMZNdis = round((AMZN$Close[AMZN$Fecha == '2022-01-04'] - AMZN$Close[AMZN$Fecha == '2020-09-24'])  / AMZN$Close[AMZN$Fecha == '2020-09-24'],4)*100




Acciones = c(
  "GOOGL",
  "MSFT",
  "AAPL",
  "AMZN")


Variacion = c(
  GOOGLdis,
  MSFTdis,
  AAPLdis,
  AMZNdis)

df = data.frame(Acciones, Variacion) %>% arrange(-Variacion)


# --------------------------------------------------------------------------------------- #

# Grafico de barras de prueba 

custom = viridis::mako(n = 4)

bar_plot = df %>% 
  arrange(desc(Variacion)) %>% 
  hchart('column', hcaes(x = Acciones, y = Variacion, color = custom)) %>% 
  hc_add_theme(hc_theme_flatdark()) %>% 
  hc_tooltip(pointFormat = '<b>Variacion %: </b> {point.y} <br> ' ) %>% 
  hc_title(text = 'Acciones Tecnologicas',
           style = list(fontSize = '25px', fontWeight = 'bold')) %>% 
  hc_subtitle(text = 'Dinamica',
              style = list(fontSize = '16px', color = "#ffffff")) %>% 
  hc_credits(enabled = TRUE, text = '@MGaloto',
             style = list(fontSize = '16px', color = "#ffffff")) %>% 
  hc_yAxis(labels = list(style = list(color = "#ffffff")),
           tickColor = "#ffffff") %>% 
  hc_xAxis(labels = list(style = list(color = "#ffffff")),
           tickColor = "#ffffff")
  


# --------------------------------------------------------------------------------------- #

# Editando las fechas 



choices <- seq.Date(as.Date("2004-08-19"), today() - 1, by = 1)

choices <- choices[!wday(choices) %in% c(1, 7)] 

default <- seq.Date(today() - 182, today() - 180, by = 1) 

default <- default[!wday(default) %in% c(1, 7)]

default <- max(default) 



# --------------------------------------------------------------------------------------- #

# Volumen acumulado




MSFTvol = (subset(MSFT, Fecha >= "2022-01-03" & Fecha <= "2022-01-05") %>% summarise(volumen = sum(Volumen)))$volumen

GOOGLvol = (subset(GOOGL, Fecha >= "2022-01-03" & Fecha <= "2022-01-05") %>% summarise(volumen = sum(Volumen)))$volumen

AAPLvol = (subset(AAPL, Fecha >= "2022-01-03" & Fecha <= "2022-01-05") %>% summarise(volumen = sum(Volumen)))$volumen

AMZNvol = (subset(AMZN, Fecha >= "2022-01-03" & Fecha <= "2022-01-05") %>% summarise(volumen = sum(Volumen)))$volumen



Acciones_vol = c(
  "GOOGL Volumen",
  "MSFT Volumen",
  "AAPL Volumen",
  "AMZN Volumen")


Acumulado_vol = c(
  GOOGLvol,
  MSFTvol,
  AAPLvol,
  AMZNvol)

df_vol = data.frame(Acciones_vol, Acumulado_vol) %>% arrange(-Acumulado_vol)



# --------------------------------------------------------------------------------------- #

# Pie Chart


pie_chart = df_vol %>% 
  hchart('pie', hcaes(x = Acciones_vol, y = Acumulado_vol, color = viridis::rocket(n = 4))) %>% 
  hc_add_theme(hc_theme_flatdark()) %>% 
  hc_tooltip(pointFormat = '<b> Sumatoria de Volumen Operado en USD: </b>  {point.y}') %>% 
  hc_title(text = 'Pie Chart participacion Volumen Operado',
           style = list(fontSize = '15px', fontWeight = 'bold')) %>% 
  hc_subtitle(text = 'Volumen',
              style = list(fontSize = '16px', color = "#ffffff")) %>% 
  hc_credits(enabled = TRUE, text = '@MGaloto',
             style = list(fontSize = '16px', color = "#ffffff"))




