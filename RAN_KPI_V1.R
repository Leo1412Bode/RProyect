#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(RODBC)
library(lubridate)
library(dygraphs)
library(xts)
library(DT)
library(ggplot2)
library(ggiraph)
library(xlsx)
library(foreach)
library(doParallel)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Cubacel RAN KPI"),
  dashboardSidebar(
    
    #textInput("idate","Date(dd/mm/yyyy):",value = format(Sys.Date()-1, format="%d-%m-%Y") ),
    dateInput("idate","Incio :",value = Sys.Date()-1, format = "yyyy-mm-dd"),
    dateInput("fdate","Final :",value = Sys.Date(), format ="yyyy-mm-dd"),
    selectInput("gra", label = "Granularidad:", choices = list("Dia" = "yyyy-mm-dd", "Horaria" = "yyyy-mm-dd hh", "Cuartil" = "yyyy-mm-dd hh:MM"), selected = "yyyy-mm-dd", width = 180),
    
    actionButton("query","Ejecutar Query"),
    #selectInput("hour", "Seleccione Hora",
    #            choices = c("All","00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21"
    #                        ,"22","23"),width = 120),
    
    
    sliderInput("hour", "Hora:", 0, 24, 24,step = 1),
    
    actionButton("selectHora","Ejecutar Hora",colorspaces = 'red'),
    
    
    sidebarMenu(
      menuItem("Tablero de Mando", tabName = "Mando", icon = icon("tb"),
               menuSubItem("2G Graficos", tabName = "Tab2Gfraph"),
               menuSubItem("3G Graficos", tabName = "Tab3Gfraph"),
               menuSubItem("2G", tabName = "Tab2G"),
               menuSubItem("3G", tabName = "Tab3G"),
               downloadButton("tableroWeek","Descargar")
               
      ),
      menuItem("HUAWEI", tabName = "HUAWEI", icon = icon("tf"),
               menuSubItem("2G", tabName = "Huawei2G"),
               menuSubItem("3G", tabName = "Huawei3G"),
               menuSubItem("Provincias 2G", tabName = "Graf2G"),
               menuSubItem("Provincias 3G", tabName = "Graf3G"),
               downloadButton("huaDownload","Descargar")
               
      ),
      menuItem("ERICSSON", tabName = "ERICSSON", icon = icon("tb"),
               menuSubItem("2G", tabName = "Ericsson2G"),
               menuSubItem("3G", tabName = "Ericsson3G"),
               menuSubItem("4G", tabName = "Ericsson4G"),
               menuSubItem("Provincias 2G", tabName = "Graf2GE"),
               menuSubItem("Provincias 3G", tabName = "Graf3GE"),
               menuSubItem("Provincias 4G", tabName = "Graf4GE"),
               downloadButton("eriDownload","Descargar")
      ),
      menuItem("NOKIA", tabName = "NOKIA", icon = icon("tb"),
               menuSubItem("2G", tabName = "Nokia2G"),
               menuSubItem("3G", tabName = "Nokia3G"),
               menuSubItem("Provincias 2G", tabName = "Graf2GN"),
               menuSubItem("Provincias 3G", tabName = "Graf3GN")
      ),
      
      downloadButton("EstadoRed","Descargar")
     
    )
  ),
  
  ###ERICSSON CELL#####
  dashboardBody(
    tabItems(
      tabItem(tabName = "Ericsson2G",
              fluidRow(
                tabBox(width = 12,
                       title = "Ericsson 2G KPI",
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabsetEricsson2G", height = "250px",
                       tabPanel("KPI por Provincia",dataTableOutput("Ericsson2GKPI_PROVINCIA")),
                       tabPanel("ACC",dataTableOutput("Ericsson2G_ACC_Celdas")),
                       tabPanel("RET",dataTableOutput("Ericsson2G_RET_Celdas")),
                       tabPanel("SER",dataTableOutput("Ericsson2G_SER_Celdas")),
                       tabPanel("TRAFICO_TCH",dataTableOutput("Ericsson2G_TCH_Celdas")),
                       tabPanel("TRAFICO_SDCCH",dataTableOutput("Ericsson2G_SDCCH_Celdas")),
                       tabPanel("Cong_SDCCH",dataTableOutput("Ericsson2G_CONG_SDCCH_Celdas")),
                       tabPanel("Cong_TCH",dataTableOutput("Ericsson2G_CONG_TCH_Celdas")),
                       tabPanel("Pagings",dataTableOutput("Ericsson2G_Paging_Celdas")),
                       tabPanel("Call_Drop",dataTableOutput("Ericsson2G_Drop")),
                       tabPanel("ICMBAND",dataTableOutput("Ericsson2G_ICM_Celdas"))
                )
              )
      ),
      tabItem(tabName = "Ericsson3G",
              fluidRow(
                tabBox(width = 12,
                       title = "Ericsson 3G KPI",
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabsetEricsson3G", height = "250px",
                       tabPanel("KPI por Provincia",dataTableOutput("Ericsson3GKPI_PROVINCIA")),
                       tabPanel("ACC_CS",dataTableOutput("Ericsson_ACC_CS")),
                       tabPanel("ACC_PS",dataTableOutput("Ericsson_ACC_PS")),
                       tabPanel("RET_CS",dataTableOutput("Ericsson_RET_CS")),
                       tabPanel("RET_PS",dataTableOutput("Ericsson_RET_PS")),
                       tabPanel("DISPONIBILIDAD",dataTableOutput("Ericsson_AVAIL")),
                       tabPanel("CONG_CE_RRC",dataTableOutput("Ericsson_CE_RRC")),
                       tabPanel("CONG_PWR_RRC",dataTableOutput("Ericsson_PWR_RRC")),
                       tabPanel("CONG_CODE_RRC",dataTableOutput("Ericsson_CODE_RRC")),
                       tabPanel("CONG_CE_RAB",dataTableOutput("Ericsson_CE_RAB")),
                       tabPanel("CONG_PWR_RAB",dataTableOutput("Ericsson_PWR_RAB")),
                       tabPanel("CONG_CODE_RAB",dataTableOutput("Ericsson_CODE_RAB")),
                       tabPanel("DROP",dataTableOutput("Ericsson_DROP_CS")),
                       tabPanel("Pagings",dataTableOutput("Ericsson_Paging")),
                       tabPanel("RSSI",dataTableOutput("Ericsson_RSSI"))
                )
              )
      ),
      tabItem(tabName = "Ericsson4G",
              fluidRow(
                tabBox(width = 12,
                       title = "Ericsson 4G KPI",
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabsetEricsson4G", height = "250px",
                       tabPanel("KPI por Provincia",dataTableOutput("Ericsson4GKPI_PROVINCIA")),
                       tabPanel("ACC",dataTableOutput("Ericsson_ACC")),
                       tabPanel("RET",dataTableOutput("Ericsson_RET")),
                       tabPanel("VOLUMEN",dataTableOutput("Ericsson_VOL")),
                       tabPanel("TROUGHPUT",dataTableOutput("Ericsson_TRO")),
                       tabPanel("USER",dataTableOutput("Ericsson_USER")),
                       tabPanel("LATENCIA",dataTableOutput("Ericsson_LAT")),
                       tabPanel("DROP",dataTableOutput("Ericsson_DROP"))
                )
              )
      ),
      
  ###HUAWEI CELL#####
      tabItem(tabName = "Huawei2G",
              fluidRow(
                tabBox(width =12, title = "Huawei 2G KPI", id = "tabsetHuawei2G" , height = "250px",
                       tabPanel("KPI por Provincia",dataTableOutput("Huawei2GKPI_PROVINCIA")),
                       tabPanel("ACC",dataTableOutput("Huawei2G_ACC_Celdas")),
                       tabPanel("RET",dataTableOutput("Huawei2G_RET_Celdas")),
                       tabPanel("SER",dataTableOutput("Huawei2G_SER_Celdas")),
                       tabPanel("TRAFICO_TCH",dataTableOutput("Huawei2G_TCH_Celdas")),
                       tabPanel("TRAFICO_SDCCH",dataTableOutput("Huawei2G_SDCCH_Celdas")),
                       tabPanel("Cong_SDCCH",dataTableOutput("Huawei2G_CONG_SDCCH_Celdas")),
                       tabPanel("Cong_TCH",dataTableOutput("Huawei2G_CONG_TCH_Celdas")),
                       tabPanel("Pagings",dataTableOutput("Huawei2G_Pagings")),
                       tabPanel("Call_Drop",dataTableOutput("Huawei2G_Drop"))
                       
                      
                       )
              )
        
      ),
      tabItem(tabName = "Huawei3G",
              fluidRow(
                tabBox(width =12, title = "Huawei 2G KPI", id = "tabsetHuawei3G" , height = "250px",
                       tabPanel("KPI por Provincia",dataTableOutput("Huawei3GKPI_PROVINCIA")),
                       tabPanel("ACC_CS",dataTableOutput("Huawei_ACC_CS")),
                       tabPanel("ACC_PS",dataTableOutput("Huawei_ACC_PS")),
                       tabPanel("RET_CS",dataTableOutput("Huawei_RET_CS")),
                       tabPanel("RET_PS",dataTableOutput("Huawei_RET_PS")),
                       tabPanel("CONG_CE_RRC",dataTableOutput("Huawei_CE_RRC")),
                       tabPanel("CONG_PWR_RRC",dataTableOutput("Huawei_PWR_RRC")),
                       tabPanel("CONG_CODE_RRC",dataTableOutput("Huawei_CODE_RRC")),
                       tabPanel("CONG_CE_RAB",dataTableOutput("Huawei_CE_RAB")),
                       tabPanel("CONG_PWR_RAB",dataTableOutput("Huawei_PWR_RAB")),
                       tabPanel("CONG_CODE_RAB",dataTableOutput("Huawei_CODE_RAB")),
                       tabPanel("DROP",dataTableOutput("Huawei_DROP_CS")),
                       tabPanel("THROUGHPUT_HSDPA",dataTableOutput("Huawei_THROUGHPUT_HSDPA")),
                       tabPanel("USER_HSDPA",dataTableOutput("Huawei_USER_HSDPA")),
                       tabPanel("Pagings",dataTableOutput("Huawei_PAGING")),
                       tabPanel("RTWP",dataTableOutput("Huawei_RTWP"))
                       
                )
              )
              
      ),
      
  ###NOKIA CELL#####
      tabItem(tabName = "Nokia2G",
              fluidRow(
                tabBox(width =12, title = "Nokia 2G KPI", id = "tabsetNokia2G" , height = "250px",
                       tabPanel("KPI por Provincia",dataTableOutput("Nokia2GKPI_PROVINCIA")),
                       tabPanel("ACC",dataTableOutput("Nokia2G_ACC_Celdas")),
                       tabPanel("RET",dataTableOutput("Nokia2G_SER_Celdas")),
                       tabPanel("SER",dataTableOutput("Nokia2G_RET_Celdas")),
                       tabPanel("TRAFICO_TCH",dataTableOutput("Nokia2G_TCH_Celdas")),
                       tabPanel("TRAFICO_SDCCH",dataTableOutput("Nokia2G_SDCCH_Celdas")),
                       tabPanel("Cong_SDCCH",dataTableOutput("Nokia2G_CONG_SDCCH_Celdas")),
                       tabPanel("Cong_TCH",dataTableOutput("Nokia2G_CONG_TCH_Celdas")),
                       tabPanel("Pagings",dataTableOutput("Nokia2G_Pagings")),
                       tabPanel("Call_Drop",dataTableOutput("Nokia2G_Drop")),
                       tabPanel("ICMBAND",dataTableOutput("Nokia2G_ICM_Celdas"))
                       
                       
                )
              )
              
      ),
      
      tabItem(tabName = "Nokia3G",
              fluidRow(
                tabBox(width =12, title = "Nokia 2G KPI", id = "tabsetNokia3G" , height = "250px",
                       tabPanel("KPI por Provincia",dataTableOutput("Nokia3GKPI_PROVINCIA")),
                       tabPanel("ACC_CS",dataTableOutput("Nokia_ACC_CS")),
                       tabPanel("ACC_PS",dataTableOutput("Nokia_ACC_PS")),
                       tabPanel("RET_CS",dataTableOutput("Nokia_RET_CS")),
                       tabPanel("RET_PS",dataTableOutput("Nokia_RET_PS")),
                       tabPanel("DISPONIBILIDAD",dataTableOutput("Nokia_DISPONIBILIDAD")),
                       tabPanel("CONG_CE_RRC",dataTableOutput("Nokia_CE_RRC")),
                       tabPanel("CONG_PWR_RRC",dataTableOutput("Nokia_PWR_RRC")),
                       tabPanel("CONG_CODE_RRC",dataTableOutput("Nokia_CODE_RRC")),
                       tabPanel("CONG_CE_RAB",dataTableOutput("Nokia_CE_RAB")),
                       tabPanel("CONG_PWR_RAB",dataTableOutput("Nokia_PWR_RAB")),
                       tabPanel("CONG_CODE_RAB",dataTableOutput("Nokia_CODE_RAB")),
                       tabPanel("DROP",dataTableOutput("Nokia_DROP_CS")),
                       tabPanel("THROUGHPUT_HSDPA",dataTableOutput("Nokia_THROUGHPUT_HSDPA")),
                       tabPanel("USER_HSDPA",dataTableOutput("Nokia_USER_HSDPA")),
                       tabPanel("RTWP",dataTableOutput("Nokia_RTWP"))
                       
                )
              )
              
      ),
      
  ###GRAFICOS 2G#####
      tabItem(tabName = "Graf2G",
              fluidRow(
                tabBox(width =12, title = "Huawei 2G KPI", id = "tabsetHuawei2G" , height = "250px",
                       tabPanel("ACC",dygraphOutput("ACC_PROV_H")),
                       tabPanel("RET",dygraphOutput("RET_PROV_H")),
                       tabPanel("SER",dygraphOutput("SER_PROV_H")),
                       tabPanel("TRAFICO_TCH",dygraphOutput("TCH_H")),
                       tabPanel("TRAFICO_SDCCH",dygraphOutput("SDCCH_H")),
                       tabPanel("CONG_SDCCH",dygraphOutput("CONG_SDCCH_H")),
                       tabPanel("CONG_TCH",dygraphOutput("CONG_TCH_H")),
                       tabPanel("DROP_TCH",dygraphOutput("DROP_TCH_H")),
                       tabPanel("DROP_SDCCH",dygraphOutput("DROP_SDCCH_H")),
                       tabPanel("PAGINGS",dygraphOutput("PAGINGS_H"))
               )
              )
              
      ),
      
      tabItem(tabName = "Graf2GE",
              fluidRow(
                tabBox(width =12, title = "ERICSSON 2G KPI", id = "tabsetEricsson2G" , height = "250px",
                       tabPanel("ACC",dygraphOutput("ACC_PROV_E")),
                       tabPanel("RET",dygraphOutput("RET_PROV_E")),
                       tabPanel("SER",dygraphOutput("SER_PROV_E")),
                       tabPanel("TRAFICO_TCH",dygraphOutput("TCH_E")),
                       tabPanel("TRAFICO_SDCCH",dygraphOutput("SDCCH_E")),
                       tabPanel("CONG_SDCCH",dygraphOutput("CONG_SDCCH_E")),
                       tabPanel("CONG_TCH",dygraphOutput("CONG_TCH_E")),
                       tabPanel("DROP_TCH",dygraphOutput("DROP_TCH_E")),
                       tabPanel("DROP_SDCCH",dygraphOutput("DROP_SDCCH_E")),
                       tabPanel("PAGINGS",dygraphOutput("PAGINGS_E"))
                )
              )
              
      ),
  ###GRAFICOS 3G#####
      tabItem(tabName = "Graf3GE",
              fluidRow(
                tabBox(width =12, title = "ERICSSON 3G KPI", id = "tabsetEricsson3G" , height = "250px",
                       tabPanel("ACC_CS",dygraphOutput("ACC_CS_PROV_E")),
                       tabPanel("ACC_PS",dygraphOutput("ACC_PS_PROV_E")),
                       tabPanel("RET_CS",dygraphOutput("RET_CS_PROV_E")),
                       tabPanel("RET_PS",dygraphOutput("RET_PS_PROV_E")),
                       tabPanel("TRAFICO_CS",dygraphOutput("TRAFF_CS_PROV_E")),
                       tabPanel("VOLUMEN_DATOS",dygraphOutput("VOL_PROV_E")),
                       tabPanel("CONG_CE_RRC",dygraphOutput("CONG_CE_RRC_E")),
                       tabPanel("CONG_PWR_RRC",dygraphOutput("CONG_PWR_RRC_E")),
                       tabPanel("CONG_CODE_RRC",dygraphOutput("CONG_CODE_RRC_E")),
                       tabPanel("CONG_CE_RAB",dygraphOutput("CONG_CE_RAB_E")),
                       tabPanel("CONG_PWR_RAB",dygraphOutput("CONG_PWR_RAB_E")),
                       tabPanel("CONG_CODE_RAB",dygraphOutput("CONG_CODE_RAB_E")),
                       tabPanel("DROP_CS",dygraphOutput("DROP_CS_PROV_E")),
                       tabPanel("DROP_PS",dygraphOutput("DROP_PS_PROV_E")),
                       tabPanel("RSSI",dygraphOutput("RSSI_PROV_E"))
                       
                      )
              )
              
      ),
      
      tabItem(tabName = "Graf3G",
              fluidRow(
                tabBox(width =12, title = "HUAWEI 3G KPI", id = "tabsethuawei3G" , height = "250px",
                       tabPanel("ACC_CS",dygraphOutput("ACC_CS_PROV_H")),
                       tabPanel("ACC_PS",dygraphOutput("ACC_PS_PROV_H")),
                       tabPanel("RET_CS",dygraphOutput("RET_CS_PROV_H")),
                       tabPanel("RET_PS",dygraphOutput("RET_PS_PROV_H")),
                       tabPanel("TRAFICO_CS",dygraphOutput("TRAFF_CS_PROV_H")),
                       tabPanel("VOLUMEN_DATOS",dygraphOutput("VOL_PROV_H")),
                       tabPanel("CONG_CE_RRC",dygraphOutput("CONG_CE_RRC_H")),
                       tabPanel("CONG_PWR_RRC",dygraphOutput("CONG_PWR_RRC_H")),
                       tabPanel("CONG_CODE_RRC",dygraphOutput("CONG_CODE_RRC_H")),
                       tabPanel("CONG_CE_RAB",dygraphOutput("CONG_CE_RAB_H")),
                       tabPanel("CONG_PWR_RAB",dygraphOutput("CONG_PWR_RAB_H")),
                       tabPanel("CONG_CODE_RAB",dygraphOutput("CONG_CODE_RAB_H")),
                       tabPanel("DROP_CS",dygraphOutput("DROP_CS_PROV_H")),
                       tabPanel("DROP_PS",dygraphOutput("DROP_PS_PROV_H")),
                       tabPanel("RTWP",dygraphOutput("RTWP_PROV_H"))
               
                )
              )
              
      ),
      
  ###TABLERO#####
      tabItem(tabName = "Tab2G",
              fluidRow(
                tabBox(width =12, title = "Tablero 2G", id = "tab2G" , height = "250px",
                       tabPanel("Huawei",dataTableOutput("tablero2GH")),
                       tabPanel("Ericsson",dataTableOutput("tablero2GE")),
                       tabPanel("Nokia",dataTableOutput("tablero2GN")),
                       tabPanel("Red",dataTableOutput("tablero2GRed"))
                )
              )
              
      ),
      tabItem(tabName = "Tab2Gfraph",
              fluidRow(
                tabBox(width =12, title = "Tablero 2G", id = "tab2Ggraph" , height = "250px",
                       tabPanel("Huawei",dygraphOutput("tablero2GraphH"),dataTableOutput("tablero2GraphHTotal")),
                       tabPanel("Ericsson",dygraphOutput("tablero2GraphE"),dataTableOutput("tablero2GraphETotal")),
                       tabPanel("Nokia",dygraphOutput("tablero2GraphN"),dataTableOutput("tablero2GraphNTotal")),
                       tabPanel("Red",dygraphOutput("tablero2GraphRed"),dataTableOutput("tablero2GraphRedTotal"))
                       
                )
              )
              
      ),
      tabItem(tabName = "Tab3G",
              fluidRow(
                tabBox(width =12, title = "Tablero 3G", id = "tab3G" , height = "250px",
                       tabPanel("Huawei",dataTableOutput("tablero3GH")),
                       tabPanel("Ericsson",dataTableOutput("tablero3GE")),
                       tabPanel("Nokia",dataTableOutput("tablero3GN")),
                       tabPanel("Red",dataTableOutput("tablero3GRed"))
                )
              )
              
      ),
      tabItem(tabName = "Tab3Gfraph",
              fluidRow(
                tabBox(width =12, title = "Tablero 3G", id = "tab3Ggraph" , height = "250px",
                       tabPanel("Huawei",dygraphOutput("tablero3GraphH"),dataTableOutput("tablero3GraphHTotal")),
                       tabPanel("Ericsson",dygraphOutput("tablero3GraphE"),dataTableOutput("tablero3GraphETotal")),
                       tabPanel("Nokia",dygraphOutput("tablero3GraphN"),dataTableOutput("tablero3GraphNTotal")),
                       tabPanel("Red",dygraphOutput("tablero3GraphRed"),dataTableOutput("tablero3GraphRedTotal"))
                )
              )
              
      )
      
      
      
      
    )

  )
)

sigestop <- function(){
  con <- odbcConnect('SIGESTOP',uid="ADMINRED",pwd="adm!nr3d",believeNRows= FALSE)
  
  qry <-"SELECT CELL_ID AS CELDA,PROVINCE as PROVINCIA, CELDA as SITIO from GSM_CELL_ID"
  
  datos <- sqlQuery(con,qry)
  
  datos$PROVINCIA <- as.character(datos$PROVINCIA)
  
  datos$PROVINCIA = ifelse(datos$PROVINCIA == "Habana", "La Habana", datos$PROVINCIA)
  datos$PROVINCIA = ifelse(datos$PROVINCIA == "Isla Juventud", "Isla de la Juventud", datos$PROVINCIA)
  
  odbcClose(con)
  
  return (datos)
}

queryERICSSON <- function(idate,fdate,datos,granul){
  con <- odbcConnect('dwhdb',uid = 'dcbo' , pwd = 'dcbo',believeNRows= FALSE)
  options(dec =",")
  
  qry<- "
  SELECT  DATEFORMAT(DATETIME_ID,'granul')  AS DIA, CELL_NAME as CELDA, 
  
  --ACC---
  Sum(CLSDCCH_CCONGS) as CCONGS,
  Sum(CLSDCCH_CCALLS) as CCALLS,
  SUM(CELLCCHDR_CDISSS) as CDISSS,
  SUM(CELLCCHDR_CDISQA) as CDISQA,
  SUM(CELLCCHDR_CDISTA) as CDISTA,
  Sum(CLSDCCH_CMSESTAB) as CMSESTAB,
  SUM(CELTCHF_TFCASSALL) as TFCASSALL,
  SUM(CELTCHF_TFCASSALLSUB) as TFCASSALLSUB,
  SUM(CELTCHH_THCASSALL) as THCASSALL,
  SUM(CELTCHH_THCASSALLSUB) as THCASSALLSUB,
  SUM(CLTCH_TASSALL) as TASSALL,
  SUM(CLTCH_TCASSALL) as TCASSALL,
  SUM(CLSDCCH_CTRALACC) as  CTRALACC,
  SUM(CLSDCCH_CNSCAN)  as CNSCAN,
  SUM(CELTCHF_TFTRALACC)  as TFTRALACC, 
  SUM(CELTCHF_TFNSCAN)  as TFNSCAN, 
  SUM(CELTCHH_THTRALACC)  as THTRALACC,
  SUM(CELTCHH_THNSCAN) as THNSCAN,
  SUM(CLSDCCH_CNDROP) as DROP_SDCCH,
  SUM (CELTCHF_TFNDROP + CELTCHH_THNDROP + CELTCHF_TFNDROPSUB + CELTCHH_THNDROPSUB) as DROP_TCH,
  
  --RET---
  SUM(CELTCHF_TFNDROP) as TFNDROP,
  SUM(CELTCHH_THNDROP) as THNDROP,
  SUM(CELTCHF_TFNDROPSUB) as TFNDROPSUB,
  SUM(CELTCHH_THNDROPSUB) as THNDROPSUB,
  SUM(CELTCHF_TFMSESTB) as TFMSESTB,
  SUM(CELTCHH_THMSESTB) as THMSESTB,

  --ICM BAND---
  Sum(IDLEOTCHF_ITFOSIB4) as ITFOSIB4,
  Sum(IDLEOTCHF_ITFOSIB5) as ITFOSIB5,
  Sum(IDLEUTCHF_ITFUSIB4) as ITFUSIB4,
  Sum(IDLEUTCHF_ITFUSIB5) as ITFUSIB5,
  
  Sum(IDLEOTCHF_ITFOSIB1) as ITFOSIB1,
  Sum(IDLEOTCHF_ITFOSIB2) as ITFOSIB2,
  Sum(IDLEOTCHF_ITFOSIB3) as ITFOSIB3,
  Sum(IDLEUTCHF_ITFUSIB1) as ITFUSIB1,
  Sum(IDLEUTCHF_ITFUSIB2) as ITFUSIB2,
  Sum(IDLEUTCHF_ITFUSIB3) as ITFUSIB3,
  
  
  --Congestion TCH---
  SUM(CLSDCCH_CNRELCONG) as CNRELCONG,
  SUM(CELTCHF_TFNRELCONG) as TFNRELCONG,
  SUM(CELTCHH_THNRELCONG) as THNRELCONG,
  SUM(CELTCHF_TFNRELCONGSUB) as TFNRELCONGSUB,
  SUM(CELTCHH_THNRELCONGSUB) as THNRELCONGSUB,
  
  --Pagings---
  SUM(CELLPAG_PAGETOOOLD) as PAGETOOOLD,
  SUM(CELLPAG_PAGPCHCONG) as PAGPCHCONG,
  SUM(CELLPAG_PAGESRECCS) as PAGESRECCS,
  SUM(CELLPAG_PAGESRECPS) as PAGESRECPS
  
  
  FROM dc.DC_E_BSS_CELL_CS_RAW
  
  WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
  GROUP BY DATEFORMAT(DATETIME_ID,'granul') , CELDA
  ORDER BY DATEFORMAT(DATETIME_ID,'granul') , CELDA"
  
  qry1<- "
   SELECT  DATEFORMAT(DATETIME_ID,'granul') AS DIA, UtranCell as CELDA, 
  
  --PS DROP---
  Sum(pmNoSystemRabReleasePacket) as PmSysRAB,
  Sum(pmNoNormalRabReleasePacket) as PmNoNormalRAB,

  --CS DROP---
  SUM(pmNoSystemRabReleaseSpeech ) as PmSysRABSpeech,
  SUM(pmNoSystemRabReleaseCs64) as PmSysRABCS64,
  SUM(pmNoNormalRabReleaseSpeech) as PmSysNormalSpeech,
  SUM(pmNoNormalRabReleaseCs64) as PmSysNormalCS64,

  ---PS_RRC_Rate---
  SUM(pmTotNoRrcConnectReqPsSucc) as PmToConnectPsSucc,
  SUM(pmTotNoRrcConnectReqPs) as PmToConnectPs,
  SUM(pmNoLoadSharingRrcConnPs) as PmLoadSharing,
  
  ---PS_RAB_Rate---
  SUM(pmNoRabEstablishSuccessPacketInteractive) as PmNoRABSuccPack,
  SUM(pmNoRabEstablishAttemptPacketInteractive) as PmNoRABAttPack,


  ---CS_RRC_Rate---
  SUM(pmTotNoRrcConnectReqCsSucc) as PmNoRRCSucc,
  SUM(pmTotNoRrcConnectReqCs) as PmNoRRC,
  SUM(pmNoLoadSharingRrcConnCs) as PmLoadShar,
  
  ---CS_RAB_Rate---
  SUM(pmNoRabEstablishSuccessSpeech) as PmNoRABSuccSpeech,
  SUM(pmNoRabEstablishSuccessCs64) as PmNoRAB64,
  SUM(pmRabEstablishEcSuccess) as PmEstaSucc,
  SUM(pmRabEstablishEcAttempt) as PmEstaAtt,
  SUM(pmNoRabEstablishAttemptSpeech) as PmNoRABAttSpeech,
  SUM(pmNoRabEstablishAttemptCs64) as PmRABAttCs64,
  SUM(pmNoDirRetryAtt) as PmNoDirRetry,
  
  ---Congestion_PWR---
  SUM(pmNoRrcReqDeniedAdmDlPwr) as PmPwrRRC,
  SUM(pmNoFailedRabEstAttemptLackDlPwr) as PmPwrRAB,
  
  ---Congestion_CODES---
  SUM(pmNoRrcReqDeniedAdmDlChnlCode) as PmCodesRRC,
  SUM(pmNoFailedRabEstAttemptLackDlChnlCode) as PmCodesRAB,
  
  ---Congestion_CE---
  SUM(pmNoFailedRabEstAttemptLackDlHw) as PmCEDlRab,
  SUM(pmNoFailedRabEstAttemptLackUlHw) as PmCEUlRab,
  SUM(pmNoRrcReqDeniedAdmDlHw) as PmCEDlRrc,
  SUM(pmNoRrcReqDeniedAdmUlHw) as PmCEUlRrc,
  
  ---Total Eventos Congestion---
  SUM(pmTotNoRrcConnectReq) as RRCAtt,
  SUM(pmNoRabEstablishAttemptSpeech) as RABSpeechAtt,
  SUM(pmNoRabEstablishAttemptPacketInteractive) as RABPckIntAtt,
  SUM(pmNoRabEstablishAttemptPacketStream) as RABPckStrAtt,
  SUM(pmNoRabEstablishAttemptPacketStream128) as RABPckStr128Att,
  SUM(pmDlTrafficVolumePsIntHs) as pmDlTrafficVolumePsIntHs,
  SUM(pmUlTrafficVolumePsIntEul) as pmUlTrafficVolumePsIntEul,
  SUM(pmUlTrafficVolumePs384+pmUlTrafficVolumePs8+pmUlTrafficVolumePs16+pmUlTrafficVolumePs64+pmUlTrafficVolumePs128) as VOLUMEN_R99_UL,
  SUM(pmDlTrafficVolumePs64+pmDlTrafficVolumePs128+pmDlTrafficVolumePs384+pmDlTrafficVolumePs8+pmDlTrafficVolumePs16) as VOLUMEN_R99_DL,


  
  ---Paging---
  SUM(pmNoPagingAttemptUtranRejected) as pmNoPagingAttemptUtranRejected,
  sum(pmNoPagingType1Attempt) AS pmNoPagingType1Attempt,
  sum(pmNoPagingType1AttemptCs) AS pmNoPagingType1AttemptCs,
  sum(pmNoPagingType1AttemptPs) AS pmNoPagingType1AttemptPs,
  sum(pmNoPagingType1ReplaceCs) AS pmNoPagingType1ReplaceCs,
  
  ---RSSI---
  SUM(pmSumUlRssi) as PmSumUlRSSi,
  (CAST(SUM(pmSamplesUlRssi) as int)) as PmsampleUlRSSi,
  SUM(pmSumBestCs12Establish)  as pmSumBestCs12Establish, 
  SUM(pmSamplesBestCs12Establish) as pmSamplesBestCs12Establish,

 ---PS DROP-----

  SUM(pmNoSystemRbReleaseHs) as   PS_DROP_HSDPA ,
  SUM(pmNoSystemRbReleaseEul) as   PS_DROP_HSUPA ,

  ---VER
  SUM(pmNoSystemRabReleasePacketUra) as pmNoSystemRabReleasePacketUra,
  SUM(pmChSwitchAttemptFachUra) as pmChSwitchAttemptFachUra,
  SUM(pmChSwitchSuccFachUra) as pmChSwitchSuccFachUra,
  SUM(pmChSwitchAttemptDchUra) as pmChSwitchAttemptDchUra,
  SUM(pmChSwitchSuccDchUra) as pmChSwitchSuccDchUra,
  SUM(pmChSwitchAttemptHsUra) as pmChSwitchAttemptHsUra,
  SUM(pmChSwitchSuccHsUra) as pmChSwitchSuccHsUra,
  SUM(pmNoNormalRabReleasePacketUra) as pmNoNormalRabReleasePacketUra,
  SUM(pmSumPsHsAdchRabEstablish) as pmSumPsHsAdchRabEstablish,
  SUM(pmSamplesPsHsAdchRabEstablish) as pmSamplesPsHsAdchRabEstablish, 
  SUM(pmSumPsEulRabEstablish) as pmSumPsEulRabEstablish,
  SUM(pmSamplesPsEulRabEstablish) as pmSamplesPsEulRabEstablish,
  SUM(pmCellDowntimeAuto) as pmCellDowntimeAuto,
  SUM(pmCellDowntimeMan) as pmCellDowntimeMan
  
  FROM dc.DC_E_RAN_UCELL_RAW
  
  WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
  GROUP BY DATEFORMAT(DATETIME_ID,'granul'), CELDA
  ORDER BY DATEFORMAT(DATETIME_ID,'granul'), CELDA"
  
  
  qry2<- "
  SELECT  DATEFORMAT(DATETIME_ID,'granul')  AS DIA, EUtranCellFDD as CELDA,
  
  --ACC---
  sum(pmRrcConnEstabSucc) as pmRrcConnEstabSucc,
  sum(pmRrcConnEstabAtt) as pmRrcConnEstabAtt,
  sum(pmRrcConnEstabAttReatt) as pmRrcConnEstabAttReatt,
  sum(pmS1SigConnEstabSucc) as pmS1SigConnEstabSucc,
  sum(pmS1SigConnEstabAtt) as pmS1SigConnEstabAtt,
  sum(pmErabEstabSuccInit) as pmErabEstabSuccInit,
  sum(pmErabEstabAttInit) as pmErabEstabAttInit,
  sum(pmRrcConnEstabFailMmeOvlMos + pmRrcConnEstabFailMmeOvlMod) as rrcmmeovl,
  Sum(pmS1SigConnEstabFailMmeOvlMos) AS pmS1SigConnEstabFailMmeOvlMos,
  --RET---
  sum(pmErabRelAbnormalEnbAct) as pmErabRelAbnormalEnbAct,
  sum(pmErabRelAbnormalMmeAct) as pmErabRelAbnormalMmeAct,
  sum(pmErabRelAbnormalEnb) as pmErabRelAbnormalEnb,
  sum(pmErabRelNormalEnb) as pmErabRelNormalEnb,
  sum(pmErabRelMme) as pmErabRelMme,
  
  --PAYLOAD---
  sum(pmPdcpVolDlDrb) as pmPdcpVolDlDrb,
  sum(pmPdcpVolDlSrb) as pmPdcpVolDlSrb,
  
  sum(pmPdcpVolUlDrb + pmPdcpVolUlSrb)  as VOL_UL,
  
  --THRP---
  
  sum(pmPdcpVolDlDrbLastTTI) as pmPdcpVolDlDrbLastTTI,
  sum(pmUeThpTimeDl) as pmUeThpTimeDl,
  sum(pmUeThpVolUl) as pmUeThpVolUl,
  sum(pmUeThpTimeUl) as pmUeThpTimeUl,
  
  --DROP_ERAB---
  SUM(pmErabRelAbnormalEnbActCdt) AS pmErabRelAbnormalEnbActCdt,
  SUM(pmErabRelAbnormalEnbActHo) AS pmErabRelAbnormalEnbActHo,
  SUM(pmErabRelAbnormalEnbActHpr) AS pmErabRelAbnormalEnbActHpr,
  SUM(pmErabRelAbnormalEnbActPe) AS pmErabRelAbnormalEnbActPe,
  SUM(pmErabRelAbnormalEnbActTnFail) AS  pmErabRelAbnormalEnbActTnFail,
  SUM(pmErabRelAbnormalEnbActUeLost) AS  pmErabRelAbnormalEnbActUeLost,	
  SUM(pmErabRelAbnormalEnbLic) AS  pmErabRelAbnormalEnbLic,
  
  --FAILS_RRC---
  SUM(pmRrcConnEstabAttHpa) AS pmRrcConnEstabAttHpa,
  SUM(pmRrcConnEstabAttMod) AS pmRrcConnEstabAttMod,
  SUM(pmRrcConnEstabAttMos) AS pmRrcConnEstabAttMos,
  SUM(pmRrcConnEstabAttMta) AS pmRrcConnEstabAttMta,
  SUM(pmRrcConnEstabAttDta) AS pmRrcConnEstabAttDta,
  
  ---LATENCY----
  sum(pmPdcpLatTimeDl) as pmPdcpLatTimeDl,
  sum(pmPdcpLatPktTransDl) as pmPdcpLatPktTransDl,
  
  sum(pmActiveUeDlSum) as pmActiveUeDlSum,
  sum(pmschedactivityCellDl) as pmschedactivityCellDl,
  
  sum(pmActiveUeUlSum) as pmActiveUeUlSum,
  sum(pmschedactivityCellUl) as pmschedactivityCellUl,
  
  ---USER-------
  sum(pmActiveUeDlMax) as sum_pmActiveUeDlMax,
  max(pmActiveUeDlMax) as max_pmActiveUeDlMax,
  sum(pmActiveUeUlMax) as sum_pmActiveUeUlMax,
  max(pmActiveUeUlMax) as max_pmActiveUeUlMax,
  SUM(pmCellDowntimeAuto) as pmCellDowntimeAuto,
  SUM(pmCellDowntimeMan) as pmCellDowntimeMan,
  SUM(pmRrcConnLevSum) AS pmRrcConnLevSum,
  SUM(pmRrcConnLevSamp) AS pmRrcConnLevSamp


  FROM dc.DC_E_ERBS_EUTRANCELLFDD_RAW
  
  WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
  GROUP BY DATEFORMAT(DATETIME_ID,'granul'),CELDA
  ORDER BY DATEFORMAT(DATETIME_ID,'granul'),CELDA
  "
  
  datos3G <- sqlQuery(con, gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry1))))
  datos3G <- datos3G %>% inner_join(datos)
  
  datos2G <- sqlQuery(con, gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry))))
  datos2G <- datos2G %>% inner_join(datos) 
  
  datos4G <- sqlQuery(con, gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry2))))
  datos4G <- datos4G %>% inner_join(datos)
  
  datos3G$VOLUMEN_R99_UL <- as.numeric(as.character(datos3G$VOLUMEN_R99_UL))
  datos3G$VOLUMEN_R99_DL <- as.numeric(as.character(datos3G$VOLUMEN_R99_DL))
  datos3G$pmDlTrafficVolumePsIntHs <- as.numeric(as.character(datos3G$pmDlTrafficVolumePsIntHs))
  datos3G$pmUlTrafficVolumePsIntEul <- as.numeric(as.character(datos3G$pmUlTrafficVolumePsIntEul))
  datos3G$pmSamplesBestCs12Establish <- as.numeric(as.character(datos3G$pmSamplesBestCs12Establish))
  datos3G$pmCellDowntimeAuto <- as.numeric(as.character(datos3G$pmCellDowntimeAuto))
  datos3G$pmCellDowntimeMan <- as.numeric(as.character(datos3G$pmCellDowntimeMan))
  datos4G$sum_pmActiveUeDlMax <- as.numeric(as.character(datos4G$sum_pmActiveUeDlMax))
  datos4G$max_pmActiveUeDlMax <- as.numeric(as.character(datos4G$max_pmActiveUeDlMax))
  datos4G$sum_pmActiveUeUlMax <- as.numeric(as.character(datos4G$sum_pmActiveUeUlMax))
  datos4G$max_pmActiveUeUlMax <- as.numeric(as.character(datos4G$max_pmActiveUeUlMax))
  datos4G$pmRrcConnLevSum <- as.numeric(as.character(datos4G$pmRrcConnLevSum))
  datos4G$pmRrcConnLevSamp <- as.numeric(as.character(datos4G$pmRrcConnLevSamp))
  
  datos3G[is.na(datos3G)] <- 0
  datos2G[is.na(datos2G)] <- 0
  datos4G[is.na(datos4G)] <- 0
  
  datos3G$pmDlTrafficVolumePsIntHs = ifelse(datos3G$pmDlTrafficVolumePsIntHs == 0, 1, datos3G$pmDlTrafficVolumePsIntHs)
  datos3G$pmUlTrafficVolumePsIntEul = ifelse(datos3G$pmUlTrafficVolumePsIntEul == 0, 1 , datos3G$pmUlTrafficVolumePsIntEul)
  datos3G$VOLUMEN_R99_UL = ifelse(datos3G$VOLUMEN_R99_UL == 0, 1, datos3G$VOLUMEN_R99_UL)
  datos3G$VOLUMEN_R99_DL = ifelse(datos3G$VOLUMEN_R99_DL == 0, 1 , datos3G$VOLUMEN_R99_DL)
  
  datos2G$TFTRALACC = ifelse(datos2G$TFTRALACC == 0, 1, datos2G$TFTRALACC)
  datos2G$TFNSCAN = ifelse(datos2G$TFNSCAN == 0, 1 , datos2G$TFNSCAN)
  datos2G$THTRALACC = ifelse(datos2G$THTRALACC == 0, 1, datos2G$THTRALACC)
  datos2G$THNSCAN = ifelse(datos2G$THNSCAN == 0, 1 , datos2G$THNSCAN)
  datos2G$CTRALACC = ifelse(datos2G$CTRALACC == 0, 1 , datos2G$CTRALACC)
  datos2G$CNSCAN = ifelse(datos2G$CNSCAN == 0, 1 , datos2G$CNSCAN)
  
  
  odbcClose(con)
  
  return(list("ericsson2G"= datos2G, "ericsson3G"=datos3G,"ericsson4G"=datos4G))
  
}
  
#ERICSSON 2G
queryData2GEricssonP <- function (data,hora){
  
  if(hora != "24"){
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    } 
  
  datosG <- filter(data,substr(DIA,12,13)== hora)
  
  datosP <- datosG %>%group_by(DIA,PROVINCIA)%>%
    summarise(
      ACC = round(100 * (1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                    (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))),3),  
      RET = round(100 * (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB)))),3),
      
      SER = round( 100*  ((1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                            (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))))
                   *
                     (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB))))
                   ,3),
      CONG_TCH =  round( 100 * as.numeric(as.numeric( sum(CNRELCONG) + sum(TFNRELCONG) + sum(THNRELCONG) + sum(TFNRELCONGSUB) + sum(THNRELCONGSUB) )/ as.numeric(sum(TASSALL))),3),
      CONG_SDCCH=  round(100* as.numeric(sum(CCONGS)) / as.numeric(sum(CCALLS)),3),
      TRAFF_TCH = round(as.numeric(sum(TFTRALACC/TFNSCAN)) + as.numeric(sum(THTRALACC/ THNSCAN)),3),
      TRAFF_SDCCH = round(as.numeric(sum(CTRALACC/ CNSCAN)),3),
      CALL_DROP_TCH = sum(DROP_TCH),
      CALL_DROP_SDCCH = sum(DROP_SDCCH),
      PAGINGS = as.numeric(sum(PAGETOOOLD)+sum(PAGPCHCONG) )
      
    )%>%filter(PROVINCIA != 0)
  return(datosP)
  }
  else
  {
    datosN <- data%>%group_by(CELDA)%>%filter(!is.na(CCALLS))
    
    
    datosP <- datosN %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC = round(100 * (1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                      (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))),3),  
        RET = round(100 * (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB)))),3),
        
        SER = round( 100*  ((1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                              (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))))
                     *
                       (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB))))
                     ,3),
        CONG_TCH = 100 * round(  as.numeric(as.numeric( sum(CNRELCONG) + sum(TFNRELCONG) + sum(THNRELCONG) + sum(TFNRELCONGSUB) + sum(THNRELCONGSUB) )/ as.numeric(sum(TASSALL))),3),
        CONG_SDCCH= 100* round( as.numeric(sum(CCONGS)) / as.numeric(sum(CCALLS)),3),
        TRAFF_TCH = round(as.numeric(sum(TFTRALACC/TFNSCAN)) + as.numeric(sum(THTRALACC/ THNSCAN)),3),
        TRAFF_SDCCH = round(as.numeric(sum(CTRALACC/ CNSCAN)),3),
        CALL_DROP_TCH = sum(DROP_TCH),
        CALL_DROP_SDCCH = sum(DROP_SDCCH),
        PAGINGS = as.numeric(sum(PAGETOOOLD)+sum(PAGPCHCONG) )
        
        
      )%>%filter(PROVINCIA != 0)
    return(datosP)
  }
}

queryData2GEricssonAll <- function(data,datos,hora,gra){
  
  if(gra == "yyyy-mm-dd"){ seg <- 24 }else{seg <- 1}
  if(hora != "24"){
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
    
    
    datosG <- filter(data,substr(DIA,12,13)==hora)
  
  datosP <- datosG%>%group_by(CELDA)%>%
    summarise(
      ACC = round(100 * (1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                    (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))),3),  
      RET = round(100 * (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB)))),3),
      
      SER = round( 100*  ((1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                            (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))))
                   *
                     (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB))))
                   ,3),
      CONG_TCH = 100 * round(  as.numeric(as.numeric( sum(CNRELCONG) + sum(TFNRELCONG) + sum(THNRELCONG) + sum(TFNRELCONGSUB) + sum(THNRELCONGSUB) )/ as.numeric(sum(TASSALL))),3),
      CONG_SDCCH= 100* round( as.numeric(sum(CCONGS)) / as.numeric(sum(CCALLS)),3),
      ICMBAND = 100 *round( as.numeric(sum(ITFOSIB4) + sum(ITFOSIB5) + sum(ITFUSIB4) + sum(ITFUSIB5)) / as.numeric(sum(ITFOSIB1)+ sum(ITFOSIB2)+ sum(ITFOSIB3)+ sum(ITFOSIB4)+ sum(ITFOSIB5) + sum(ITFUSIB1)+ sum(ITFUSIB2)+sum(ITFUSIB3)+sum(ITFUSIB4)+ sum(ITFUSIB5) ),3),
      PAGINGS = as.numeric(max(PAGETOOOLD)+max(PAGPCHCONG) ),
      TRAFF_TCH = round(as.numeric(sum(TFTRALACC/TFNSCAN)) + as.numeric(sum(THTRALACC/ THNSCAN)),3),
      TRAFF_SDCCH = round(as.numeric(sum(CTRALACC/ CNSCAN)),3),
      CALL_DROP_TCH = sum(DROP_TCH),
      CALL_DROP_SDCCH = sum(DROP_SDCCH),
      PAGING_RATE = round(100 - 100 * (as.numeric(sum(PAGETOOOLD+PAGPCHCONG))/as.numeric(sum(PAGESRECCS+PAGESRECPS))),2)
    )
  datosG <- datosP %>% inner_join(datos)
  return(datosG)
  
  }
  else{
    datosP <- data%>%group_by(CELDA)%>%
      summarise(
        ACC = round(100 * (1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                      (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))),3),  
        RET = round(100 * (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB)))),3),
        
        SER = round( 100*  ((1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                              (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))))
                     *
                       (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB))))
                     ,3),
        CONG_TCH = 100 * round(  as.numeric(as.numeric( sum(CNRELCONG) + sum(TFNRELCONG) + sum(THNRELCONG) + sum(TFNRELCONGSUB) + sum(THNRELCONGSUB) )/ as.numeric(sum(TASSALL))),3),
        CONG_SDCCH= 100* round( as.numeric(sum(CCONGS)) / as.numeric(sum(CCALLS)),3),
        ICMBAND = 100 *round( as.numeric(sum(ITFOSIB4) + sum(ITFOSIB5) + sum(ITFUSIB4) + sum(ITFUSIB5)) / as.numeric(sum(ITFOSIB1)+ sum(ITFOSIB2)+ sum(ITFOSIB3)+ sum(ITFOSIB4)+ sum(ITFOSIB5) + sum(ITFUSIB1)+ sum(ITFUSIB2)+sum(ITFUSIB3)+sum(ITFUSIB4)+ sum(ITFUSIB5) ),3),
        PAGINGS = as.numeric(max(PAGETOOOLD)+max(PAGPCHCONG) ),
        TRAFF_TCH = round(seg *as.numeric(sum(TFTRALACC/TFNSCAN)) + as.numeric(sum(THTRALACC/ THNSCAN)),3),
        TRAFF_SDCCH = round(seg *as.numeric(sum(CTRALACC/ CNSCAN)),3),
        CALL_DROP_TCH = sum(DROP_TCH),
        CALL_DROP_SDCCH = sum(DROP_SDCCH),
        PAGING_RATE = round(100 - 100 * (as.numeric(sum(PAGETOOOLD + PAGPCHCONG))/as.numeric(sum(PAGESRECCS+PAGESRECPS))),2)
      )
    datosG <- datosP %>% inner_join(datos)
    return(datosG)
  }
}

#ERICSSON 3G

queryData3GEricssonP <- function (data,hora,gra){
  if(gra == "yyyy-mm-dd"){ seg <- 24 
  data <- data %>%  mutate(pmSamplesBestCs12Establish = pmSamplesBestCs12Establish / 24)
  }else{seg <- 1}
  if(hora != "24"){
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
      
  datosG <- filter(data,substr(DIA,12,13)==hora)  
  datos1 <- datosG %>% group_by(CELDA) 
  datosP <- datos1 %>%filter(pmSumBestCs12Establish > 0)%>%group_by(DIA,PROVINCIA)%>%
    summarise(
      ACC_CS = round(100 * as.numeric(as.numeric(sum(PmNoRRCSucc)/(sum(PmNoRRC)-sum(PmLoadShar)))    *    as.numeric((sum(PmEstaSucc)+sum(PmNoRABSuccSpeech)+sum(PmNoRAB64)) 
                                                                                                                     /(sum(PmEstaAtt) + sum(PmNoRABAttSpeech) + sum(PmRABAttCs64) - sum(PmNoDirRetry)))),3),
      
      ACC_PS = round(100 * as.numeric(as.numeric(sum(PmToConnectPsSucc)/(sum(PmToConnectPs) - sum(PmLoadSharing))) * 
                                        as.numeric(sum(PmNoRABSuccPack)/sum(PmNoRABAttPack))) ,3),
      
      RET_CS = 100 -  round(100*(as.numeric(sum(PmSysRABSpeech) + sum(PmSysRABCS64)) / 
                                   as.numeric(sum(PmSysNormalSpeech) + sum(PmSysNormalCS64) + sum(PmSysRABSpeech) + sum(PmSysRABCS64)) ),3),
      
      RET_PS = round(100 - 100 * as.numeric(as.numeric( sum(as.numeric(PmSysRAB )- as.numeric(pmNoSystemRabReleasePacketUra) )  - sum(as.numeric(pmChSwitchAttemptFachUra) -as.numeric( pmChSwitchSuccFachUra) ) - sum(as.numeric(pmChSwitchAttemptDchUra) - as.numeric(pmChSwitchSuccDchUra) ) - sum(as.numeric(pmChSwitchAttemptHsUra) - as.numeric(pmChSwitchSuccHsUra) ))
                                            / as.numeric(sum(as.numeric(PmNoNormalRAB) ) - sum(as.numeric(pmNoNormalRabReleasePacketUra) ) +  sum(as.numeric(PmSysRAB) ) + sum(as.numeric(pmNoSystemRabReleasePacketUra) ) 
                                                         + sum(as.numeric(pmChSwitchSuccFachUra) ) + sum(as.numeric(pmChSwitchSuccDchUra) )  + sum(as.numeric(pmChSwitchSuccHsUra) )) ),3),
      
      CONG_CE_RRC = round (100 * as.numeric(sum(PmCEDlRrc) + sum(PmCEUlRrc))/ as.numeric(sum(RRCAtt)),3),
      
      CONG_PWR_RRC = round (100 * as.numeric(sum(PmPwrRRC))/ as.numeric(sum(RRCAtt)),3),
      
      CONG_CODE_RRC = round (100 * as.numeric(sum(PmCodesRRC))/ as.numeric(sum(RRCAtt)),3),
      
      CONG_CE_RAB = round (100 * as.numeric(sum(PmCEDlRab) + sum(PmCEUlRab))/
                             as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
      #
      CONG_PWR_RAB = round (100 * as.numeric(sum(PmPwrRAB))/
                              as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
      
      CONG_CODE_RAB = round ((100 * sum(PmCodesRAB))/ 
                               as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
      
      TRAFF_CS = round(as.numeric(sum(as.numeric(pmSumBestCs12Establish)/as.numeric( pmSamplesBestCs12Establish))),2),
      
      VOLUMEN_DATOS = round(as.numeric(sum(pmDlTrafficVolumePsIntHs) + sum(pmUlTrafficVolumePsIntEul) + sum(VOLUMEN_R99_UL) + sum(VOLUMEN_R99_DL))/(8*1000000),3),

      DROP_CS = as.numeric(sum( PmSysRABSpeech + PmSysRABCS64)),
      
      DROP_PS = as.numeric(sum(PmSysRAB)),
      
      PAGINGS = as.numeric(mean(pmNoPagingAttemptUtranRejected)),
      
      RSSI = -112 +  round (0.1 * as.numeric( sum(PmSumUlRSSi) / sum(PmsampleUlRSSi) ) ,3),
      AVG_HS_THP = ((sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                       ((sum(as.numeric(pmSumPsHsAdchRabEstablish))/sum(as.numeric(pmSamplesPsHsAdchRabEstablish)))+
                          (sum(as.numeric(pmSumPsEulRabEstablish))/sum(as.numeric(pmSamplesPsEulRabEstablish))))) /
                      (sum(as.numeric(pmSamplesBestCs12Establish))*5)),
      AVG_HS_THP1 = (sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                       (sum(as.numeric(pmSamplesBestCs12Establish))*5))
      ) 
  
 
  return(datosP)
  }
  
  else{
    datos1 <- data %>% group_by(CELDA) 
      
    datosP <- data %>%filter(pmSumBestCs12Establish > 0)%>%group_by(DIA,PROVINCIA)%>%
      summarise(
        
        ACC_CS = round(100 * as.numeric(as.numeric(sum(PmNoRRCSucc)/(sum(PmNoRRC)-sum(PmLoadShar)))    *    as.numeric((sum(PmEstaSucc)+sum(PmNoRABSuccSpeech)+sum(PmNoRAB64)) 
                                /(sum(PmEstaAtt) + sum(PmNoRABAttSpeech) + sum(PmRABAttCs64) - sum(PmNoDirRetry)))),3),
        
        ACC_PS = round(100 * as.numeric(as.numeric(sum(PmToConnectPsSucc)/(sum(PmToConnectPs) - sum(PmLoadSharing))) * 
                              as.numeric(sum(PmNoRABSuccPack)/sum(PmNoRABAttPack))) ,3),
    
        RET_CS = 100 -  round(100*(as.numeric(sum(PmSysRABSpeech) + sum(PmSysRABCS64)) / 
                             as.numeric(sum(PmSysNormalSpeech) + sum(PmSysNormalCS64) + sum(PmSysRABSpeech) + sum(PmSysRABCS64)) ),3),
        
        RET_PS = round(100 - 100 * as.numeric(as.numeric( sum(as.numeric(PmSysRAB )- as.numeric(pmNoSystemRabReleasePacketUra) )  - sum(as.numeric(pmChSwitchAttemptFachUra) -as.numeric( pmChSwitchSuccFachUra) ) - sum(as.numeric(pmChSwitchAttemptDchUra) - as.numeric(pmChSwitchSuccDchUra) ) - sum(as.numeric(pmChSwitchAttemptHsUra) - as.numeric(pmChSwitchSuccHsUra) ))
                                              / as.numeric(sum(as.numeric(PmNoNormalRAB) ) - sum(as.numeric(pmNoNormalRabReleasePacketUra) ) +  sum(as.numeric(PmSysRAB) ) + sum(as.numeric(pmNoSystemRabReleasePacketUra) ) 
                                                           + sum(as.numeric(pmChSwitchSuccFachUra) ) + sum(as.numeric(pmChSwitchSuccDchUra) )  + sum(as.numeric(pmChSwitchSuccHsUra) )) ),3),
        
        CONG_CE_RRC = round (100 * as.numeric(sum(PmCEDlRrc) + sum(PmCEUlRrc))/ as.numeric(sum(RRCAtt)),3),
        
        CONG_PWR_RRC = round (100 * as.numeric(sum(PmPwrRRC))/ as.numeric(sum(RRCAtt)),3),
        
        CONG_CODE_RRC = round (100 * as.numeric(sum(PmCodesRRC))/ as.numeric(sum(RRCAtt)),3),
        
        CONG_CE_RAB = round (100 * as.numeric(sum(PmCEDlRab) + sum(PmCEUlRab))/
                               as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
        #
        CONG_PWR_RAB = round (100 * as.numeric(sum(PmPwrRAB))/
                                as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
        
        CONG_CODE_RAB = round ((100 * sum(PmCodesRAB))/ 
                                 as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
       
        TRAFF_CS = round(as.numeric(sum(as.numeric(pmSumBestCs12Establish)/as.numeric( pmSamplesBestCs12Establish))),2),
        
        VOLUMEN_DATOS = round(as.numeric(sum(pmDlTrafficVolumePsIntHs) + sum(pmUlTrafficVolumePsIntEul) + sum(VOLUMEN_R99_UL) + sum(VOLUMEN_R99_DL)  )/ (8*1000000),3),
  
        DROP_CS = as.numeric(sum( PmSysRABSpeech + PmSysRABCS64)),
        
        DROP_PS = as.numeric(sum(PmSysRAB)),
        
        PAGINGS = as.numeric(mean(pmNoPagingAttemptUtranRejected)),
        
        RSSI = -112 +  round (0.1 * as.numeric( sum(PmSumUlRSSi) / sum(PmsampleUlRSSi) ) ,3),
        
        AVG_HS_THP = ((sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                         ((sum(as.numeric(pmSumPsHsAdchRabEstablish))/sum(as.numeric(pmSamplesPsHsAdchRabEstablish)))+
                            (sum(as.numeric(pmSumPsEulRabEstablish))/sum(as.numeric(pmSamplesPsEulRabEstablish))))) /
                        (sum(as.numeric(pmSamplesBestCs12Establish))*5)),
        AVG_HS_THP1 = (sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                         (sum(as.numeric(pmSamplesBestCs12Establish))*5))
              )
    return(datosP)
  }
}

queryData3GEricssonAll <- function(data,datosS,hora,gra){
   if(gra == "yyyy-mm-dd"){ seg <- 24
   data <- data %>%  mutate(pmSamplesBestCs12Establish = pmSamplesBestCs12Establish / 24)
   seg1<-86400
   }else{seg <- 1
   seg1<-3600}
   if(hora != "24"){
  
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }  
  datosG <- filter(data,substr(DIA,12,13)==hora)  
  
  datosP <- datosG%>%group_by(CELDA)%>%
    summarise(
      ACC_CS = round(100 * as.numeric(as.numeric(sum(PmNoRRCSucc)/(sum(PmNoRRC)-sum(PmLoadShar)))    *    as.numeric((sum(PmEstaSucc)+sum(PmNoRABSuccSpeech)+sum(PmNoRAB64)) 
                                                                                                                     /(sum(PmEstaAtt) + sum(PmNoRABAttSpeech) + sum(PmRABAttCs64) - sum(PmNoDirRetry)))),3),
      
      ACC_PS = round(100 * as.numeric(as.numeric(sum(PmToConnectPsSucc)/(sum(PmToConnectPs) - sum(PmLoadSharing))) * 
                                        as.numeric(sum(PmNoRABSuccPack)/sum(PmNoRABAttPack))) ,3),
      
      RET_CS = 100 -  round(100*(as.numeric(sum(PmSysRABSpeech) + sum(PmSysRABCS64)) / 
                                   as.numeric(sum(PmSysNormalSpeech) + sum(PmSysNormalCS64) + sum(PmSysRABSpeech) + sum(PmSysRABCS64)) ),3),
      
      RET_PS = round(100 - 100 * as.numeric(as.numeric( sum(as.numeric(PmSysRAB )- as.numeric(pmNoSystemRabReleasePacketUra) )  - sum(as.numeric(pmChSwitchAttemptFachUra) -as.numeric( pmChSwitchSuccFachUra) ) - sum(as.numeric(pmChSwitchAttemptDchUra) - as.numeric(pmChSwitchSuccDchUra) ) - sum(as.numeric(pmChSwitchAttemptHsUra) - as.numeric(pmChSwitchSuccHsUra) ))
                                            / as.numeric(sum(as.numeric(PmNoNormalRAB) ) - sum(as.numeric(pmNoNormalRabReleasePacketUra) ) +  sum(as.numeric(PmSysRAB) ) + sum(as.numeric(pmNoSystemRabReleasePacketUra) ) 
                                                         + sum(as.numeric(pmChSwitchSuccFachUra) ) + sum(as.numeric(pmChSwitchSuccDchUra) )  + sum(as.numeric(pmChSwitchSuccHsUra) )) ),3),
      
      CONG_CE_RRC = round (100 * as.numeric(sum(PmCEDlRrc) + sum(PmCEUlRrc))/ as.numeric(sum(RRCAtt)),3),
      
      CONG_PWR_RRC = round (100 * as.numeric(sum(PmPwrRRC))/ as.numeric(sum(RRCAtt)),3),
      
      CONG_CODE_RRC = round (100 * as.numeric(sum(PmCodesRRC))/ as.numeric(sum(RRCAtt)),3),
      
      CONG_CE_RAB = round (100 * as.numeric(sum(PmCEDlRab) + sum(PmCEUlRab))/
                             as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
      #
      CONG_PWR_RAB = round (100 * as.numeric(sum(PmPwrRAB))/
                              as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
      
      CONG_CODE_RAB = round ((100 * sum(PmCodesRAB))/ 
                               as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
      RSSI = -112 +  round (0.1 * as.numeric( sum(PmSumUlRSSi) / sum(PmsampleUlRSSi) ) ,3),
      
      PAGINGS = as.numeric(mean(pmNoPagingAttemptUtranRejected)),
      
      TRAFF_CS = round(seg * as.numeric(sum(as.numeric(pmSumBestCs12Establish)/as.numeric( pmSamplesBestCs12Establish))),3),
      
      VOLUMEN_DATOS = round(as.numeric(sum(pmDlTrafficVolumePsIntHs) + sum(pmUlTrafficVolumePsIntEul) + sum(VOLUMEN_R99_UL) + sum(VOLUMEN_R99_DL)  )/ (8*1000000),3),
      DROP_CS = as.numeric(sum( PmSysRABSpeech + PmSysRABCS64)),
      DROP_PS = as.numeric(sum(PmSysRAB)),
      AVG_HS_THP = ((sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                       ((sum(as.numeric(pmSumPsHsAdchRabEstablish))/sum(as.numeric(pmSamplesPsHsAdchRabEstablish)))+
                          (sum(as.numeric(pmSumPsEulRabEstablish))/sum(as.numeric(pmSamplesPsEulRabEstablish))))) /
                      (sum(as.numeric(pmSamplesBestCs12Establish))*5)),
      AVG_HS_THP1 = (sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                       (sum(as.numeric(pmSamplesBestCs12Establish))*5)),
      DISPONIBILIDAD =  100 * round( 1 - as.numeric(sum(pmCellDowntimeAuto) + sum(pmCellDowntimeMan))/seg1,3),
      
      PAGING_RATE =  round(100 - 100 * as.numeric(sum(pmNoPagingAttemptUtranRejected))/as.numeric(sum(pmNoPagingType1AttemptCs+pmNoPagingType1AttemptPs)),2)
    ) 
  datosF <- datosP%>% inner_join(datosS)
  return(datosF)
  }
  else
  {
    
    datosP <- data%>%group_by(CELDA)%>%
      summarise(
        ACC_CS = round(100 * as.numeric(as.numeric(sum(PmNoRRCSucc)/(sum(PmNoRRC)-sum(PmLoadShar)))    *    as.numeric((sum(PmEstaSucc)+sum(PmNoRABSuccSpeech)+sum(PmNoRAB64)) 
                                                                                                                       /(sum(PmEstaAtt) + sum(PmNoRABAttSpeech) + sum(PmRABAttCs64) - sum(PmNoDirRetry)))),3),
        
        ACC_PS = round(100 * as.numeric(as.numeric(sum(PmToConnectPsSucc)/(sum(PmToConnectPs) - sum(PmLoadSharing))) * 
                                          as.numeric(sum(PmNoRABSuccPack)/sum(PmNoRABAttPack))) ,3),
        
        RET_CS = 100 -  round(100*(as.numeric(sum(PmSysRABSpeech) + sum(PmSysRABCS64)) / 
                                     as.numeric(sum(PmSysNormalSpeech) + sum(PmSysNormalCS64) + sum(PmSysRABSpeech) + sum(PmSysRABCS64)) ),3),
        
        RET_PS = round(100 - 100 * as.numeric(as.numeric( sum(as.numeric(PmSysRAB )- as.numeric(pmNoSystemRabReleasePacketUra) )  - sum(as.numeric(pmChSwitchAttemptFachUra) -as.numeric( pmChSwitchSuccFachUra) ) - sum(as.numeric(pmChSwitchAttemptDchUra) - as.numeric(pmChSwitchSuccDchUra) ) - sum(as.numeric(pmChSwitchAttemptHsUra) - as.numeric(pmChSwitchSuccHsUra) ))
                                              / as.numeric(sum(as.numeric(PmNoNormalRAB) ) - sum(as.numeric(pmNoNormalRabReleasePacketUra) ) +  sum(as.numeric(PmSysRAB) ) + sum(as.numeric(pmNoSystemRabReleasePacketUra) ) 
                                                           + sum(as.numeric(pmChSwitchSuccFachUra) ) + sum(as.numeric(pmChSwitchSuccDchUra) )  + sum(as.numeric(pmChSwitchSuccHsUra) )) ),3),
        
        CONG_CE_RRC = round (100 * as.numeric(sum(PmCEDlRrc) + sum(PmCEUlRrc))/ as.numeric(sum(RRCAtt)),3),
        
        CONG_PWR_RRC = round (100 * as.numeric(sum(PmPwrRRC))/ as.numeric(sum(RRCAtt)),3),
        
        CONG_CODE_RRC = round (100 * as.numeric(sum(PmCodesRRC))/ as.numeric(sum(RRCAtt)),3),
        
        CONG_CE_RAB = round (100 * as.numeric(sum(PmCEDlRab) + sum(PmCEUlRab))/
                               as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
        #
        CONG_PWR_RAB = round (100 * as.numeric(sum(PmPwrRAB))/
                                as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
        
        CONG_CODE_RAB = round ((100 * sum(PmCodesRAB))/ 
                                 as.numeric(sum(RABSpeechAtt) + sum(RABPckIntAtt) + sum(RABPckStrAtt)  + sum(RABPckStr128Att)),3),
        RSSI = -112 +  round (0.1 * as.numeric( sum(PmSumUlRSSi) / sum(PmsampleUlRSSi) ) ,3),
        
        PAGINGS = as.numeric(mean(pmNoPagingAttemptUtranRejected)),
        
        TRAFF_CS = round(seg *as.numeric(sum(as.numeric(pmSumBestCs12Establish)/as.numeric( pmSamplesBestCs12Establish))),3),

        VOLUMEN_DATOS = round(as.numeric(sum(pmDlTrafficVolumePsIntHs) + sum(pmUlTrafficVolumePsIntEul) + sum(VOLUMEN_R99_UL) + sum(VOLUMEN_R99_DL)  )/ (8*1000000),3),
        DROP_CS = as.numeric(sum( PmSysRABSpeech + PmSysRABCS64)),
        DROP_PS = as.numeric(sum(PmSysRAB)),
        AVG_HS_THP = ((sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                         ((sum(as.numeric(pmSumPsHsAdchRabEstablish))/sum(as.numeric(pmSamplesPsHsAdchRabEstablish)))+
                            (sum(as.numeric(pmSumPsEulRabEstablish))/sum(as.numeric(pmSamplesPsEulRabEstablish))))) /
                        (sum(as.numeric(pmSamplesBestCs12Establish))*5)),
        AVG_HS_THP1 = (sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                         (sum(as.numeric(pmSamplesBestCs12Establish))*5)),
        DISPONIBILIDAD =  100 * round( 1 - as.numeric(sum(pmCellDowntimeAuto) + sum(pmCellDowntimeMan))/seg1,3),
        
        PAGING_RATE =  round(100 - 100 * as.numeric(sum(pmNoPagingAttemptUtranRejected))/as.numeric(sum(pmNoPagingType1AttemptCs+pmNoPagingType1AttemptPs)),2)
        ) 
    datosF <- datosP%>% inner_join(datosS)
    return(datosF)
  }
}

#ERICSSON 4G

queryData4GEricssonP <- function(data,hora,gra){
  if(gra == "yyyy-mm-dd"){ seg <- 24 }else{seg <- 1}
  if(hora != "24"){
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    } 
    
    datosG <- filter(data,substr(DIA,12,13)==hora)  
    datosP <- datosG %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC = round(100* as.numeric(sum(as.numeric(pmRrcConnEstabSucc))/ (sum(as.numeric(pmRrcConnEstabAtt)) - sum(as.numeric(pmRrcConnEstabAttReatt))- sum(as.numeric(rrcmmeovl)) ) )
                    * as.numeric(sum(as.numeric(pmS1SigConnEstabSucc))/ (sum(as.numeric(pmS1SigConnEstabAtt))-sum(as.numeric(pmS1SigConnEstabFailMmeOvlMos))) ) * as.numeric(sum(as.numeric(pmErabEstabSuccInit))/sum(as.numeric(pmErabEstabAttInit))) ,3),
        
        RET = round(100 - 100 * as.numeric(sum(as.numeric(pmErabRelAbnormalEnbAct))+ sum(as.numeric(pmErabRelAbnormalMmeAct)))/as.numeric(sum(as.numeric(pmErabRelAbnormalEnb)) + sum(as.numeric(pmErabRelNormalEnb)) + sum(as.numeric(pmErabRelMme)) ),3),
        
        PAYLOAD_DL = round(as.numeric((sum(as.numeric(pmPdcpVolDlDrb))+sum(as.numeric(pmPdcpVolDlSrb) )) /(8*1000000)) ,3),
        PAYLOAD_UL = round(as.numeric(sum(as.numeric(VOL_UL) )/(8*1000000)) ,3),
        
        THRPTDL = round(as.numeric(sum(as.numeric(pmPdcpVolDlDrb))-sum(as.numeric(pmPdcpVolDlDrbLastTTI) ) )/as.numeric(sum(as.numeric(pmUeThpTimeDl))) ,3),
        
        THRPTUL = round(as.numeric(sum(as.numeric(pmUeThpVolUl)))/as.numeric(sum(as.numeric(pmUeThpTimeUl))),3),
        
        DROP_ERAB = as.numeric(sum(as.numeric(pmErabRelAbnormalEnbActCdt)) + sum(as.numeric(pmErabRelAbnormalEnbActHo) )+ sum(as.numeric(pmErabRelAbnormalEnbActHpr)) + sum(as.numeric(pmErabRelAbnormalEnbActPe)) + sum(as.numeric(pmErabRelAbnormalEnbActTnFail)) + sum(as.numeric(pmErabRelAbnormalEnbActUeLost)) + sum(as.numeric(pmErabRelAbnormalEnbLic))),
        
        LAT = round(as.numeric(sum(as.numeric(pmPdcpLatTimeDl)))/as.numeric(sum(as.numeric(pmPdcpLatPktTransDl))),3),
        
        USER_DL = as.numeric(sum(sum_pmActiveUeDlMax)),
        
        USER_UL = as.numeric(sum(sum_pmActiveUeUlMax)),
        
        USER_MAX_UL = as.numeric(max(max_pmActiveUeUlMax)),
        
        USER_MAX_DL = as.numeric(max(max_pmActiveUeDlMax)),
        
        RRC_USER =   round(sum(as.numeric(pmRrcConnLevSum)/as.numeric(pmRrcConnLevSamp)),3)
        
      ) 
    return(datosP)
    
  }
  else{
    datosP <- data %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC = round(100* as.numeric(sum(as.numeric(pmRrcConnEstabSucc))/ (sum(as.numeric(pmRrcConnEstabAtt)) - sum(as.numeric(pmRrcConnEstabAttReatt))- sum(as.numeric(rrcmmeovl)) ) )
                    * as.numeric(sum(as.numeric(pmS1SigConnEstabSucc))/ (sum(as.numeric(pmS1SigConnEstabAtt))-sum(as.numeric(pmS1SigConnEstabFailMmeOvlMos))) ) * as.numeric(sum(as.numeric(pmErabEstabSuccInit))/sum(as.numeric(pmErabEstabAttInit))) ,3),
        
        RET = round(100 - 100 * as.numeric(sum(as.numeric(pmErabRelAbnormalEnbAct))+ sum(as.numeric(pmErabRelAbnormalMmeAct)))/as.numeric(sum(as.numeric(pmErabRelAbnormalEnb)) + sum(as.numeric(pmErabRelNormalEnb)) + sum(as.numeric(pmErabRelMme)) ),3),
        
        PAYLOAD_DL = round(as.numeric((sum(as.numeric(pmPdcpVolDlDrb))+sum(as.numeric(pmPdcpVolDlSrb) )) /(8*1000000)) ,3),
        PAYLOAD_UL = round(as.numeric(sum(as.numeric(VOL_UL) )/(8*1000000)) ,3),
        
        THRPTDL = round(as.numeric(sum(as.numeric(pmPdcpVolDlDrb))-sum(as.numeric(pmPdcpVolDlDrbLastTTI) ) )/as.numeric(sum(as.numeric(pmUeThpTimeDl))) ,3),
        
        THRPTUL = round(as.numeric(sum(as.numeric(pmUeThpVolUl)))/as.numeric(sum(as.numeric(pmUeThpTimeUl))),3),
        
        DROP_ERAB = as.numeric(sum(as.numeric(pmErabRelAbnormalEnbActCdt)) + sum(as.numeric(pmErabRelAbnormalEnbActHo) )+ sum(as.numeric(pmErabRelAbnormalEnbActHpr)) + sum(as.numeric(pmErabRelAbnormalEnbActPe)) + sum(as.numeric(pmErabRelAbnormalEnbActTnFail)) + sum(as.numeric(pmErabRelAbnormalEnbActUeLost)) + sum(as.numeric(pmErabRelAbnormalEnbLic))),
        
        LAT = round(as.numeric(sum(as.numeric(pmPdcpLatTimeDl)))/as.numeric(sum(as.numeric(pmPdcpLatPktTransDl))),3),
        
        USER_DL = as.numeric(sum(sum_pmActiveUeDlMax)),
        
        USER_UL = as.numeric(sum(sum_pmActiveUeUlMax)),
        
        USER_MAX_UL = as.numeric(max(max_pmActiveUeUlMax)),
        
        USER_MAX_DL = as.numeric(max(max_pmActiveUeDlMax)),
        
        RRC_USER =   round(sum(as.numeric(pmRrcConnLevSum)/as.numeric(pmRrcConnLevSamp)),3)
      ) 
    return(datosP)
  }
}

queryData4GEricssonAll <- function(data,datosS,hora,gra){
  
  if(gra == "yyyy-mm-dd"){ seg <- 24 }else{seg <- 1}
  if(hora != "24"){
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    } 
    
    datosG <- filter(data,substr(DIA,12,13)==hora)  
    datosP <- datosG %>%group_by(CELDA)%>%
      summarise(
        ACC = round(100* as.numeric(sum(as.numeric(pmRrcConnEstabSucc))/ (sum(as.numeric(pmRrcConnEstabAtt)) - sum(as.numeric(pmRrcConnEstabAttReatt))- sum(as.numeric(rrcmmeovl)) ) )
                    * as.numeric(sum(as.numeric(pmS1SigConnEstabSucc))/ (sum(as.numeric(pmS1SigConnEstabAtt))-sum(as.numeric(pmS1SigConnEstabFailMmeOvlMos))) ) * as.numeric(sum(as.numeric(pmErabEstabSuccInit))/sum(as.numeric(pmErabEstabAttInit))) ,3),
        
        RET = round(100 - 100 * as.numeric(sum(as.numeric(pmErabRelAbnormalEnbAct))+ sum(as.numeric(pmErabRelAbnormalMmeAct)))/as.numeric(sum(as.numeric(pmErabRelAbnormalEnb)) + sum(as.numeric(pmErabRelNormalEnb)) + sum(as.numeric(pmErabRelMme)) ),3),
        
        PAYLOAD_DL = round(as.numeric((sum(as.numeric(pmPdcpVolDlDrb))+sum(as.numeric(pmPdcpVolDlSrb) )) /(8*1000000)) ,3),
        PAYLOAD_UL = round(as.numeric(sum(as.numeric(VOL_UL) )/(8*1000000)) ,3),
        
        THRPTDL = round(as.numeric(sum(as.numeric(pmPdcpVolDlDrb))-sum(as.numeric(pmPdcpVolDlDrbLastTTI) ) )/as.numeric(sum(as.numeric(pmUeThpTimeDl))) ,3),
        
        THRPTUL = round(as.numeric(sum(as.numeric(pmUeThpVolUl)))/as.numeric(sum(as.numeric(pmUeThpTimeUl))),3),
        
        DROP_ERAB = as.numeric(sum(as.numeric(pmErabRelAbnormalEnbActCdt)) + sum(as.numeric(pmErabRelAbnormalEnbActHo) )+ sum(as.numeric(pmErabRelAbnormalEnbActHpr)) + sum(as.numeric(pmErabRelAbnormalEnbActPe)) + sum(as.numeric(pmErabRelAbnormalEnbActTnFail)) + sum(as.numeric(pmErabRelAbnormalEnbActUeLost)) + sum(as.numeric(pmErabRelAbnormalEnbLic))),
        
        LAT = round(as.numeric(sum(as.numeric(pmPdcpLatTimeDl)))/as.numeric(sum(as.numeric(pmPdcpLatPktTransDl))),3),
        
        USER_DL = as.numeric(sum(sum_pmActiveUeDlMax)),
        
        USER_UL = as.numeric(sum(sum_pmActiveUeUlMax)),
        
        USER_MAX_UL = as.numeric(max(max_pmActiveUeUlMax)),
        
        USER_MAX_DL = as.numeric(max(max_pmActiveUeDlMax)),
        
        RRC_USER =   round(sum(as.numeric(pmRrcConnLevSum)/as.numeric(pmRrcConnLevSamp)),3)
      ) 
    datosF <- datosP%>% inner_join(datosS)
    return(datosF)
  }
  else{
    datosP <- data %>%group_by(CELDA)%>%
      summarise(
        ACC = round(100* as.numeric(sum(as.numeric(pmRrcConnEstabSucc))/ (sum(as.numeric(pmRrcConnEstabAtt)) - sum(as.numeric(pmRrcConnEstabAttReatt))- sum(as.numeric(rrcmmeovl)) ) )
                    * as.numeric(sum(as.numeric(pmS1SigConnEstabSucc))/ (sum(as.numeric(pmS1SigConnEstabAtt))-sum(as.numeric(pmS1SigConnEstabFailMmeOvlMos))) ) * as.numeric(sum(as.numeric(pmErabEstabSuccInit))/sum(as.numeric(pmErabEstabAttInit))) ,3),
        
        RET = round(100 - 100 * as.numeric(sum(as.numeric(pmErabRelAbnormalEnbAct))+ sum(as.numeric(pmErabRelAbnormalMmeAct)))/as.numeric(sum(as.numeric(pmErabRelAbnormalEnb)) + sum(as.numeric(pmErabRelNormalEnb)) + sum(as.numeric(pmErabRelMme)) ),3),
        
        PAYLOAD_DL = round(as.numeric((sum(as.numeric(pmPdcpVolDlDrb))+sum(as.numeric(pmPdcpVolDlSrb) )) /(8*1000000)) ,3),
        PAYLOAD_UL = round(as.numeric(sum(as.numeric(VOL_UL) )/(8*1000000)) ,3),
        
        THRPTDL = round(as.numeric(sum(as.numeric(pmPdcpVolDlDrb))-sum(as.numeric(pmPdcpVolDlDrbLastTTI) ) )/as.numeric(sum(as.numeric(pmUeThpTimeDl))) ,3),
        
        THRPTUL = round(as.numeric(sum(as.numeric(pmUeThpVolUl)))/as.numeric(sum(as.numeric(pmUeThpTimeUl))),3),
        
        DROP_ERAB = as.numeric(sum(as.numeric(pmErabRelAbnormalEnbActCdt)) + sum(as.numeric(pmErabRelAbnormalEnbActHo) )+ sum(as.numeric(pmErabRelAbnormalEnbActHpr)) + sum(as.numeric(pmErabRelAbnormalEnbActPe)) + sum(as.numeric(pmErabRelAbnormalEnbActTnFail)) + sum(as.numeric(pmErabRelAbnormalEnbActUeLost)) + sum(as.numeric(pmErabRelAbnormalEnbLic))),
        
        LAT = round(as.numeric(sum(as.numeric(pmPdcpLatTimeDl)))/as.numeric(sum(as.numeric(pmPdcpLatPktTransDl))),3),
        
        USER_DL = as.numeric(sum(as.numeric(sum_pmActiveUeDlMax))),
        
        USER_UL = as.numeric(sum(as.numeric(sum_pmActiveUeUlMax))),
        
        USER_MAX_UL = as.numeric(max(as.numeric(max_pmActiveUeUlMax))),
        
        USER_MAX_DL = as.numeric(max(as.numeric(max_pmActiveUeDlMax))),
        
        RRC_USER =   round(sum(as.numeric(pmRrcConnLevSum)/as.numeric(pmRrcConnLevSamp)),3)
      ) 
    
    datosF <- datosP%>% inner_join(datosS)
    return(datosF)
  }
  
}

#NOKIA 2G
queryData2GNokia <- function(idate,fdate,datos,granul){

  if(granul == 'yyyy-mm-dd hh' ){
    granul <- 'YYYY-MM-DD HH24'
  }
  
  if(granul == 'yyyy-mm-dd' ){
    granul <- 'YYYY-MM-DD'
  }
  
  qry1 <- "select to_char(period_start_time, 'granul') as DIA,
  CO_NAME as CELDA,
  
  ----ACC----
  sum(TCH_REQUESTS_CALL_ATTEMPT) as TCH_REQUESTS_CALL_ATTEMPT,
  sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) as SUCC_TCH_SEIZ_CALL_ATTEMPT,
  sum(SDCCH_DROP_CALL_AND_HO) as SDCCH_DROP_CALL_AND_HO,
  
  ----RET----
  sum(DROP_AFTER_TCH_ASSIGN) as DROP_AFTER_TCH_ASSIGN,
  
  ----SDCCH CONG
  sum(SDCCH_BUSY_ATT) as SDCCH_BUSY_ATT,
  sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) as TCH_SEIZ_ATT_DUE_SDCCH_CON,
  sum(SDCCH_SEIZ_ATT) as SDCCH_SEIZ_ATT,
  
  sum(SDCCH_RADIO_FAIL + SDCCH_RF_OLD_HO  +SDCCH_BCSU_RESET+SDCCH_NETW_ACT+SDCCH_RADIO_FAIL+
  + SDCCH_ABIS_FAIL_CALL+SDCCH_ABIS_FAIL_OLD+SDCCH_BTS_FAIL+SDCCH_LAPD_FAIL+
  SDCCH_A_IF_FAIL_CALL+SDCCH_A_IF_FAIL_OLD
  ) as DROP_SDCCH
  
  
  from BSC_PS_TRAFFIC_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (CO_GID = bts_gid ) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME
  "
  
  qry2 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  ----ACC----
  SUM(SDCCH_ALLOC_FOR_VOICE_CALL) AS SDCCH_ALLOC_FOR_VOICE_CALL,
  SUM(RES_AV_DENOM15) AS RES_AV_DENOM15,
  SUM(AVE_BUSY_SDCCH) AS AVE_BUSY_SDCCH,
  SUM(AVE_BUSY_TCH) AS AVE_BUSY_TCH,
  SUM(RES_AV_DENOM14) AS RES_AV_DENOM14,
  SUM(AVE_TCH_BUSY_FULL) AS AVE_TCH_BUSY_FULL,
  SUM(AVE_BUSY_DHR_TCH) AS AVE_BUSY_DHR_TCH,
  SUM(AVE_TCH_BUSY_HALF) AS AVE_TCH_BUSY_HALF,
  SUM(AVE_BUSY_DFR_TCH) AS AVE_BUSY_DFR_TCH,
  SUM(AVE_BUSY_DHR_TCH_DENOM) AS AVE_BUSY_DHR_TCH_DENOM,
  sum(AVE_IDLE_F_TCH_1) as AVE_IDLE_F_TCH_1,
  sum(AVE_IDLE_F_TCH_2) as AVE_IDLE_F_TCH_2,
  sum(AVE_IDLE_F_TCH_3) as AVE_IDLE_F_TCH_3,
  sum(AVE_IDLE_F_TCH_4) as AVE_IDLE_F_TCH_4,
  sum(AVE_IDLE_F_TCH_5) as AVE_IDLE_F_TCH_5,
  sum(RES_AV_DENOM4) as RES_AV_DENOM4,
  sum(RES_AV_DENOM5) as RES_AV_DENOM5,
  sum(RES_AV_DENOM6) as RES_AV_DENOM6,
  sum(RES_AV_DENOM7) as RES_AV_DENOM7,
  sum(RES_AV_DENOM8) as RES_AV_DENOM8
  
  
  from BSC_PS_RESAVAIL_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry3 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(TCH_NEW_CALL_ASSIGN) AS TCH_NEW_CALL_ASSIGN,
  SUM(TCH_RE_EST_ASSIGN) AS TCH_RE_EST_ASSIGN,
  SUM(TCH_HO_ASSIGN) AS TCH_HO_ASSIGN,
  SUM(T3101_EXPIRED) AS T3101_EXPIRED,
  SUM(TCH_HO_RELEASE) AS TCH_HO_RELEASE
  
  from BSC_PS_SERVICE_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry4 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(CELL_TCH_TCH) AS CELL_TCH_TCH
  
  from BSC_PS_HO_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry5 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(DELETE_PAGING_COMMAND) AS DELETE_PAGING_COMMAND,
  SUM(PAGING_MSG_SENT) as PAGING_MSG_SENT
  
  from BSC_PS_RESACC_TTP2_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry6 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(RLC_DATA_BLOCKS_UL_CS1) AS RLC_DATA_BLOCKS_UL_CS1,
  SUM(RLC_DATA_BLOCKS_UL_CS2) AS RLC_DATA_BLOCKS_UL_CS2,
  SUM(RLC_DATA_BLOCKS_DL_CS1) AS RLC_DATA_BLOCKS_DL_CS1,
  SUM(RLC_DATA_BLOCKS_DL_CS2) AS RLC_DATA_BLOCKS_DL_CS2

  from BSC_PS_PCU_TTP_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry7 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(UL_RLC_BLOCKS_IN_ACK_MODE + UL_RLC_BLOCKS_IN_UNACK_MODE) as  UL_RLC_BLOCK,
  SUM(DL_RLC_BLOCKS_IN_ACK_MODE + DL_RLC_BLOCKS_IN_UNACK_MODE) as  DL_RLC_BLOCK
  
  from BSC_PS_CODINGSC_CS1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  paramtoqry <- function(qry,granul,fdate,idate){
  gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry)))
  }
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 7
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    #Connecting to database through RODBC
    con <- odbcConnect('NOKIA',uid="pmr",pwd="pmr")
    options(dec=",")
    #Test connection
    #odbcGetInfo(ch)
    result<- sqlQuery(con, qrys[i])
    #    result<-sqlQuery(ch,paste('SELECT * FROM ',databases[i]))  
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datanok <- outputPar[[1]]
  for (i in 2:length(outputPar)){datanok <- datanok %>% full_join(outputPar[[i]])}
  
  datanok <- filter(datanok, !is.na(CELDA))
  datanok$AVE_TCH_BUSY_HALF <- as.numeric(as.character(datanok$AVE_TCH_BUSY_HALF))
  datanok$AVE_TCH_BUSY_FULL <- as.numeric(as.character(datanok$AVE_TCH_BUSY_FULL))
  
  datanok[is.na(datanok)] <- 0
  str(datanok)
  return(datanok)
}

queryData2GnokiaP <- function (data,hora,gra){
  data$PROVINCIA <- "Cienfuegos"
  if(gra == "yyyy-mm-dd"){ seg <- 24 }else{seg <- 1}
  
  
  if(hora != "24"){
    
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
    
    datosG <- filter(data,substr(DIA,12,13)==hora)
    
    datosP <- datosG %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC = round((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                      (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  )) *
                      (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000,3),
    
        RET = round(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))),3),
        
        SER = round(as.numeric((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                                 (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  ))*
                                 (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000)
                    *as.numeric(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))) )/100,3),
        
        TRAFF_TCH =round(seg * sum( as.numeric(AVE_BUSY_TCH)/as.numeric(RES_AV_DENOM14)),3),
        
        TRAFF_TCH_FR = round(as.numeric(sum(AVE_TCH_BUSY_FULL)) + (2*as.numeric(sum(AVE_BUSY_DFR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
        
        TRAFF_TCH_HR = round(as.numeric(sum(AVE_TCH_BUSY_HALF)) + (2*as.numeric(sum(AVE_BUSY_DHR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
        
        TRAFF_SDCCH = round(seg * as.numeric(sum(AVE_BUSY_SDCCH))/as.numeric(sum(RES_AV_DENOM15) ),3),
        
        CONG_SDCCH = round(100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)),2),
        
        CONG_TCH = round(100 * as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)- sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) )/as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)),2),
        
        DROP_TCH = as.numeric(sum(DROP_AFTER_TCH_ASSIGN)),
        
        DROP_SDCCH = as.numeric(sum(DROP_SDCCH) - sum(T3101_EXPIRED)),
        
        PAGINGS = as.numeric(sum(DELETE_PAGING_COMMAND)),
        
        INT4_5P = round(100 * as.numeric(as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+ as.numeric(sum(AVE_IDLE_F_TCH_5)/sum(RES_AV_DENOM8)) ) / as.numeric(  as.numeric(sum(AVE_IDLE_F_TCH_1)/(sum(RES_AV_DENOM4)) )+ 
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_2)/(sum(RES_AV_DENOM5)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_3)/(sum(RES_AV_DENOM6)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_5)/(sum(RES_AV_DENOM8)) )),3)
        
        )
    return(datosP)
  }
  
  else{
    
    datosP <- data %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC = round((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                      (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  )) *
                      (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000,3),
        RET = round(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))),3),
        
        SER = round(as.numeric((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                                 (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  ))*
                                 (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000)
                    *as.numeric(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))) )/100,3),
        
        TRAFF_TCH =round(seg * sum( as.numeric(AVE_BUSY_TCH)/as.numeric(RES_AV_DENOM14)),3),
        
        TRAFF_TCH_FR = round(as.numeric(sum(AVE_TCH_BUSY_FULL)) + (2*as.numeric(sum(AVE_BUSY_DFR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
        
        TRAFF_TCH_HR = round(as.numeric(sum(AVE_TCH_BUSY_HALF)) + (2*as.numeric(sum(AVE_BUSY_DHR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
        
        TRAFF_SDCCH = round(seg * as.numeric(sum(AVE_BUSY_SDCCH))/as.numeric(sum(RES_AV_DENOM15) ),3),
        
        CONG_SDCCH = round(100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)),2),
        
        CONG_TCH = round(100 * as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)- sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) )/as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)),2),
        
        DROP_TCH = as.numeric(sum(DROP_AFTER_TCH_ASSIGN)),
        
        DROP_SDCCH = as.numeric(sum(DROP_SDCCH) - sum(T3101_EXPIRED)),
        
        PAGINGS = as.numeric(sum(DELETE_PAGING_COMMAND)),
        
        INT4_5P = round(100 * as.numeric(as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+ as.numeric(sum(AVE_IDLE_F_TCH_5)/sum(RES_AV_DENOM8)) ) / as.numeric(  as.numeric(sum(AVE_IDLE_F_TCH_1)/(sum(RES_AV_DENOM4)) )+ 
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_2)/(sum(RES_AV_DENOM5)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_3)/(sum(RES_AV_DENOM6)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_5)/(sum(RES_AV_DENOM8)) )),3)
      )
    return(datosP)
  }
  
}

queryData2GnokiaALL <- function (data,datos,hora,gra){
  data$PROVINCIA <- "Cienfuegos"
  if(gra == "yyyy-mm-dd"){ seg <- 24 }else{seg <- 1}
  
  if(hora != "24"){
    
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
    datosG <- filter(data,substr(DIA,12,13)==hora)
    
    datosP <- datosG %>%group_by(CELDA)%>%
      summarise(
        ACC = round((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                      (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  )) *
                      (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000,3),
        RET = round(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))),3),
        
        SER = round(as.numeric((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                                 (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  ))*
                                 (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000)
                    *as.numeric(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))) )/100,3),
        
        TRAFF_TCH =round(seg * sum( as.numeric(AVE_BUSY_TCH)/as.numeric(RES_AV_DENOM14)),3),
        
        TRAFF_TCH_FR = round(as.numeric(sum(AVE_TCH_BUSY_FULL)) + (2*as.numeric(sum(AVE_BUSY_DFR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
        
        TRAFF_TCH_HR = round(as.numeric(sum(AVE_TCH_BUSY_HALF)) + (2*as.numeric(sum(AVE_BUSY_DHR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
        
        TRAFF_SDCCH = round(seg * as.numeric(sum(AVE_BUSY_SDCCH))/as.numeric(sum(RES_AV_DENOM15) ),3),
        
        CONG_SDCCH = round(100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)),2),
        
        CONG_TCH = round(100 * as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)- sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) )/as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)),2),
        
        DROP_TCH = as.numeric(sum(DROP_AFTER_TCH_ASSIGN)),
        
        DROP_SDCCH = as.numeric(sum(DROP_SDCCH) - sum(T3101_EXPIRED)),
        
        PAGINGS = as.numeric(sum(DELETE_PAGING_COMMAND)),
        
        INT4_5P = round(100 * as.numeric(as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+ as.numeric(sum(AVE_IDLE_F_TCH_5)/sum(RES_AV_DENOM8)) ) / as.numeric(  as.numeric(sum(AVE_IDLE_F_TCH_1)/(sum(RES_AV_DENOM4)) )+ 
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_2)/(sum(RES_AV_DENOM5)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_3)/(sum(RES_AV_DENOM6)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_5)/(sum(RES_AV_DENOM8)) )),3),
        PAGING_RATE = round(100 - 100 * as.numeric(sum(DELETE_PAGING_COMMAND))/as.numeric(sum(PAGING_MSG_SENT)),2)
        )
    
    datosP <- datosP %>% inner_join(datos)
    return(datosP)
  }
  
  else{
  
    datosP <- data %>%group_by(CELDA)%>%
      summarise(
        ACC = round((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                      (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  )) *
                      (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000,3),
        RET = round(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))),3),
        
        SER = round(as.numeric((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                                 (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  ))*
                                 (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000)
                    *as.numeric(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))) )/100,3),
        
        TRAFF_TCH =round(seg * sum( as.numeric(AVE_BUSY_TCH)/as.numeric(RES_AV_DENOM14)),3),
        
        TRAFF_TCH_FR = round(as.numeric(sum(AVE_TCH_BUSY_FULL)) + (2*as.numeric(sum(AVE_BUSY_DFR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
        
        TRAFF_TCH_HR = round(as.numeric(sum(AVE_TCH_BUSY_HALF)) + (2*as.numeric(sum(AVE_BUSY_DHR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
        
        TRAFF_SDCCH = round(seg * as.numeric(sum(AVE_BUSY_SDCCH))/as.numeric(sum(RES_AV_DENOM15) ),3),
        
        CONG_SDCCH = round(100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)),2),
        
        CONG_TCH = round(100 * as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)- sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) )/as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)),2),
        
        DROP_TCH = as.numeric(sum(DROP_AFTER_TCH_ASSIGN)),
        
        DROP_SDCCH = as.numeric(sum(DROP_SDCCH) - sum(T3101_EXPIRED)),
        
        PAGINGS = as.numeric(sum(DELETE_PAGING_COMMAND)),
        
        INT4_5P = round(100 * as.numeric(as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+ as.numeric(sum(AVE_IDLE_F_TCH_5)/sum(RES_AV_DENOM8)) ) / as.numeric(  as.numeric(sum(AVE_IDLE_F_TCH_1)/(sum(RES_AV_DENOM4)) )+ 
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_2)/(sum(RES_AV_DENOM5)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_3)/(sum(RES_AV_DENOM6)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+
                                                                                                                                                                          as.numeric(sum(AVE_IDLE_F_TCH_5)/(sum(RES_AV_DENOM8)) )),3),
        PAGING_RATE = round(100 - 100 * as.numeric(sum(DELETE_PAGING_COMMAND))/as.numeric(sum(PAGING_MSG_SENT)),2)
        )
    datosP <- datosP %>% inner_join(datos)
    return(datosP)
    
  } 
}


#NOKIA 3G
queryData3GNokia <- function(idate,fdate,datos,granul){

  if(granul == 'yyyy-mm-dd hh' ){
    granul <- 'YYYY-MM-DD HH24'
  }
  
  if(granul == 'yyyy-mm-dd' ){
    granul <- 'YYYY-MM-DD'
  }
  
  qry1 <- "
  select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----TRAFF_CS-----
  sum(AVG_RAB_HLD_TM_CS_VOICE + avg_rab_hld_tm_cs_conv ) as AVG_RAB_HLD_TM_CS_VOICE,
  
  
  
  ----ACC_CS----NUM
  SUM(moc_conv_call_atts - moc_conv_call_fails + mtc_conv_call_atts - mtc_conv_call_fails + emergency_call_atts 
  - emergency_call_fails - rrc_acc_rel_emergency - rrc_acc_rel_mo_conv - rrc_acc_rel_mt_conv 
  ) as ACC_CS_N,
  
  sum(moc_conv_call_atts + mtc_conv_call_atts + emergency_call_atts - rrc_att_rep_mo_conv - rrc_att_rep_mt_conv 
  - rrc_att_rep_emergency - rrc_acc_rel_emergency - rrc_acc_rel_mo_conv - rrc_acc_rel_mt_conv -
  rrc_conn_stp_rej_emerg_call ) as ACC_CS_D,
  
  SUM(RAB_ACC_COMP_CS_VOICE)as RAB_CS_VOICE, 
  
  sum(RAB_STP_ATT_CS_VOICE) as RAB_STP_VOICE,
  
  --ACC_PS
  ---NUM---
  sum( moc_inter_call_atts - moc_inter_call_fails + moc_backg_call_atts - moc_backg_call_fails 
  + mtc_inter_call_atts - mtc_inter_call_fails + mtc_backg_call_atts - mtc_backg_call_fails + moc_high_prior_sign_atts 
  - moc_high_prior_sign_fails + mtc_high_prior_sign_atts - mtc_high_prior_sign_fails - rrc_acc_rel_interactive - rrc_acc_rel_mo_background 
  - rrc_acc_rel_mo_high_pr_sign - rrc_acc_rel_mo_interactive - rrc_acc_rel_mt_background - rrc_acc_rel_mt_high_pr_sign ) as NUM_ACC_PS,
  
  ---DEN----
  sum (
  moc_inter_call_atts + moc_backg_call_atts + moc_high_prior_sign_atts + mtc_inter_call_atts + mtc_backg_call_atts + 
  mtc_high_prior_sign_atts - rrc_att_rep_interactive - rrc_att_rep_mo_interactive - rrc_att_rep_mo_high_pr_sign - rrc_att_rep_mo_background - 
  rrc_att_rep_mt_background - rrc_att_rep_mt_high_pr_sign - rrc_acc_rel_interactive - rrc_acc_rel_mo_background - rrc_acc_rel_mo_high_pr_sign 
  - rrc_acc_rel_mo_interactive - rrc_acc_rel_mt_background - rrc_acc_rel_mt_high_pr_sign) as DEN_ACC_PS,
  
  ---MULT_NUM----
  sum(rab_acc_comp_ps_inter + rab_acc_comp_ps_backg) as MULT_NUM1,
  sum(rab_stp_att_ps_inter + rab_stp_att_ps_backg) as MULT_NUM2,
  
  ---RET_CS
  ----NUM-----
  sum(rab_act_rel_cs_voice_p_emp + rab_act_fail_cs_voice_iu + rab_act_fail_cs_voice_radio + rab_act_fail_cs_voice_bts
  + rab_act_fail_cs_voice_iur + rab_act_fail_cs_voice_rnc + rab_act_fail_cs_voice_ue + rab_act_fail_cs_voice_trans ) as NUM_RET_CS,
  ----DEN----
  sum( rab_act_comp_cs_voice + rab_act_rel_cs_voice_srnc +  rab_act_rel_cs_voice_p_emp +  rab_act_rel_cs_voice_hho + rab_act_rel_cs_voice_isho + 
  rab_act_rel_cs_voice_ganho + rab_act_rel_cs_voice_ganho + rab_act_fail_cs_voice_iu + rab_act_fail_cs_voice_radio + rab_act_fail_cs_voice_bts +
  rab_act_fail_cs_voice_iur + rab_act_fail_cs_voice_rnc + rab_act_fail_cs_voice_ue + rab_act_fail_cs_voice_trans) as DEN_RET_CS,
  
  ----CONG_CE---------
  sum(RRC_CONN_STP_FAIL_BTS) as RRC_CONN_STP_FAIL_BTS,
  sum(RAB_STP_FAIL_CS_VOICE_BTS) as RAB_STP_FAIL_CS_VOICE_BTS,
  
  ----CONG_CODE---------
  sum(RAB_STP_FAIL_CS_VOICE_AC_COD) as RAB_STP_FAIL_CS_VOICE_AC_COD,
  sum(RRC_CONN_STP_FAIL_AC_COD) as RRC_CONN_STP_FAIL_AC_COD,
  
  
  ----CONG_PWR---------
  
  sum(RAB_STP_FAIL_CS_VOICE_AC_UL) as RAB_STP_FAIL_CS_VOICE_AC_UL,
  sum(RAB_STP_FAIL_CS_VOICE_AC_DL) as RAB_STP_FAIL_CS_VOICE_AC_DL,
  sum(RRC_CONN_STP_FAIL_AC_UL) as RRC_CONN_STP_FAIL_AC_UL,
  sum(RRC_CONN_STP_FAIL_AC_DL) as RRC_CONN_STP_FAIL_AC_DL,
  sum(RAB_STP_FAIL_PS_STREA_BTS) as RAB_STP_FAIL_PS_STREA_BTS,
  sum(RRC_CONN_STP_ATT) as RRC_CONN_STP_ATT,
  sum(RAB_STP_COMP_CS_VOICE)as RAB_STP_COMP_CS_VOICE
  
  from NOKRWW_PS_SERVLEV_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD')) 
  AND (wcel_id = CO_GID) 
  GROUP BY to_char(period_start_time,'granul'),CO_NAME
  "
  
  qry2 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  ----ACC----
  sum(CELL_UPDATE_SUCC_CS_CALL) as CELL_UPDATE_SUCC_CS_CALL,
  
  sum(CELL_UPDATE_ATT_CS_CALL) as CELL_UPDATE_ATT_CS_CALL,
  
  sum(FAIL_PAG_NO_RESP_CELL_PCH) as FAIL_PAG_NO_RESP_CELL_PCH,
  
  sum(FAIL_PAG_NO_RESP_URA_PCH) as FAIL_PAG_NO_RESP_URA_PCH,
  
  sum(PAGING_OCCASION_URA_PCH) as PAGING_OCCASION_URA_PCH,
  
  sum(PAGING_OCCASION_CELL_PCH) as PAGING_OCCASION_CELL_PCH,
  
  sum(ATT_HS_DSCH_TO_FACH) as ATT_HS_DSCH_TO_FACH,
  
  sum(ATT_FACH_TO_HS_DSCH) as ATT_FACH_TO_HS_DSCH, 
  
  sum(SUCC_HS_DSCH_TO_FACH) as SUCC_HS_DSCH_TO_FACH,
  
  sum(SUCC_FACH_TO_HS_DSCH) as SUCC_FACH_TO_HS_DSCH
  
  
  from NOKRWW_PS_RRC_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME "
  
  
  qry3 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  ---RET_PS----
  ---NUM----
  sum(ps_rel_rl_fail_hs_e_int + ps_rel_rl_fail_hs_e_bgr + ps_rel_rl_fail_hs_d_int + ps_rel_rl_fail_hs_d_bgr + ps_rel_rl_fail_d_d_int +
  ps_rel_rl_fail_d_d_bgr + ps_rel_oth_fail_hs_e_int + ps_rel_oth_fail_hs_e_bgr + ps_rel_oth_fail_hs_d_int + ps_rel_oth_fail_hs_d_bgr +
  ps_rel_oth_fail_d_d_int + ps_rel_oth_fail_d_d_bgr + ps_rel_rl_fail_hs_e_stre + ps_rel_rl_fail_hs_d_stre + ps_rel_rl_fail_d_d_stre  +
  ps_rel_oth_fail_hs_e_stre + ps_rel_oth_fail_hs_d_stre + ps_rel_oth_fail_d_d_stre ) as NUM_RET_PS,
  
  ---DEN----
  sum(ps_rel_rl_fail_hs_e_int + ps_rel_rl_fail_hs_e_bgr + ps_rel_rl_fail_hs_d_int + ps_rel_rl_fail_hs_d_bgr + ps_rel_rl_fail_d_d_int +
  ps_rel_rl_fail_d_d_bgr + ps_rel_oth_fail_hs_e_int + ps_rel_oth_fail_hs_e_bgr + ps_rel_oth_fail_hs_d_int + ps_rel_oth_fail_hs_d_bgr +
  ps_rel_oth_fail_d_d_int + ps_rel_oth_fail_d_d_bgr + ps_rel_rl_fail_hs_e_stre + ps_rel_rl_fail_hs_d_stre + ps_rel_rl_fail_d_d_stre  +
  ps_rel_oth_fail_hs_e_stre + ps_rel_oth_fail_hs_d_stre + ps_rel_oth_fail_d_d_stre + ps_rel_norm_hs_e_stre + ps_rel_norm_hs_d_stre +
  ps_rel_norm_d_d_stre + ps_rel_norm_hs_e_int + ps_rel_norm_hs_e_bgr + ps_rel_norm_hs_d_int + ps_rel_norm_hs_d_bgr + ps_rel_norm_d_d_int + ps_rel_norm_d_d_bgr ) as DEN_RET_PS,
  sum(PS_SETUP_FAIL_AC_COD_NRT) as PS_SETUP_FAIL_AC_COD_NRT,
  sum(PS_SETUP_FAIL_AC_DL_NRT) as PS_SETUP_FAIL_AC_DL_NRT,
  sum(PS_SETUP_FAIL_AC_UL_NRT) as PS_SETUP_FAIL_AC_UL_NRT,
  SUM(PS_ATT_HSDSCH_EDCH_INT + PS_ATT_HSDSCH_EDCH_BGR + PS_ATT_HSDSCH_DCH_INT + PS_ATT_HSDSCH_DCH_BGR + PS_ATT_DCH_DCH_INT + PS_ATT_DCH_DCH_BGR) as PS_RAB_ATT 

  from NOKRWW_PS_PKTCALL_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  GROUP BY to_char(period_start_time,'granul'),CO_NAME "
  
  qry4 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(REJ_DCH_DUE_POWER_INT_DL) as REJ_DCH_DUE_POWER_INT_DL,
  SUM(REJ_DCH_DUE_POWER_BGR_DL) as REJ_DCH_DUE_POWER_BGR_DL,
  SUM(REJ_DCH_REC_DUE_PWR_INT_DL) as REJ_DCH_REC_DUE_PWR_INT_DL,
  SUM(REJ_DCH_REC_DUE_PWR_BGR_DL) as REJ_DCH_REC_DUE_PWR_BGR_DL,
  SUM(REJ_DCH_DUE_POWER_INT_UL) as REJ_DCH_DUE_POWER_INT_UL,
  SUM(REJ_DCH_DUE_POWER_BGR_UL) as REJ_DCH_DUE_POWER_BGR_UL,
  SUM(REJ_DCH_REC_DUE_PWR_INT_UL) as REJ_DCH_REC_DUE_PWR_INT_UL, 
  SUM(REJ_DCH_REC_DUE_PWR_BGR_UL) as REJ_DCH_REC_DUE_PWR_BGR_UL,
  sum(REJ_DCH_DUE_CODES_INT_DL) as REJ_DCH_DUE_CODES_INT_DL,
  sum(REJ_DCH_DUE_CODES_BGR_DL) as REJ_DCH_DUE_CODES_BGR_DL,
  SUM(dch_sel_max_hsdpa_users_int + dch_sel_max_hsdpa_users_bgr + dch_sel_max_hsdpa_users_str) as HSDPA_TO_DCH_NUM,
  SUM(allo_hs_dsch_flow_int + allo_hs_dsch_flow_bgr + allo_hs_dsch_flow_str + dch_sel_max_hsdpa_users_int + dch_sel_max_hsdpa_users_bgr + dch_sel_max_hsdpa_users_str 
  + dl_dch_sel_hsdpa_power_int + dl_dch_sel_hsdpa_power_bgr +dl_dch_sel_hsdpa_power_str + rej_hs_dsch_ret_int + rej_hs_dsch_ret_bgr + rej_hs_dsch_ret_str 
  + setup_fail_rnc_hs_dsch_int + setup_fail_rnc_hs_dsch_bgr + setup_fail_rnc_hs_dsch_str + setup_fail_ue_hs_dsch_int + setup_fail_ue_hs_dsch_bgr + setup_fail_ue_hs_dsch_str 
  + setup_fail_bts_hs_dsch_int +setup_fail_bts_hs_dsch_bgr + setup_fail_bts_hs_dsch_str + setup_fail_iub_hs_total_int + setup_fail_iub_hs_total_bgr + setup_fail_iub_hs_total_str ) as HSDPA_TO_DCH_DEN,
  SUM(ul_dch_sel_max_hsupa_usr_int + ul_dch_sel_max_hsupa_usr_bgr + ul_dch_sel_max_hsupa_usr_str) as HSUPA_TO_DCH_NUM,
  SUM(allo_success_edch_int + allo_success_edch_bgr + allo_success_edch_str + edch_allo_canc_na_as_bgr + edch_allo_canc_na_as_int + edch_allo_canc_na_as_str 
  + ul_dch_sel_max_hsupa_usr_bgr + ul_dch_sel_max_hsupa_usr_int + ul_dch_sel_max_hsupa_usr_str + ul_dch_sel_bts_hw_int + ul_dch_sel_bts_hw_bgr + ul_dch_sel_bts_hw_str 
  + setup_fail_edch_bts_bgr + setup_fail_edch_bts_int + setup_fail_edch_bts_str + setup_fail_edch_other_bgr + setup_fail_edch_other_int + setup_fail_edch_other_str 
  + setup_fail_edch_trans_bgr + setup_fail_edch_trans_int + setup_fail_edch_trans_str + setup_fail_edch_ue_bgr + setup_fail_edch_ue_int + setup_fail_edch_ue_str 
  + setup_rej_edch_ac_int + setup_rej_edch_ac_bgr) as HSUPA_TO_DCH_DEN
  
  from NOKRWW_PS_TRAFFIC_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME "
  
  qry5 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(RB_RELEASE_DUE_DYLO_RL_POWER) as RB_RELEASE_DUE_DYLO_RL_POWER,
  SUM(RB_RELEASE_DUE_PBS_BTS) as RB_RELEASE_DUE_PBS_BTS,
  SUM(RB_RELEASE_DUE_PBS_INTERF) as RB_RELEASE_DUE_PBS_INTERF,
  SUM(RB_RELEASE_DUE_PBS_SPREAD) as RB_RELEASE_DUE_PBS_SPREAD,
  
  SUM(RB_RELEASE_DUE_PRE_EMP_BTS) as RB_RELEASE_DUE_PRE_EMP_BTS,
  SUM(RB_RELEASE_DUE_PRE_EMP_INTF)  as RB_RELEASE_DUE_PRE_EMP_INTF,
  SUM(RB_RELEASE_DUE_PRE_EMP_SPREA)  as RB_RELEASE_DUE_PRE_EMP_SPREA,
  
  SUM(HSPA_NRT_O_NRT_DUE_USR) as HSPA_NRT_O_NRT_DUE_USR,
  SUM(HSPA_NRT_O_NRT_DUE_BTS_HW) as HSPA_NRT_O_NRT_DUE_BTS_HW,
  SUM(HSPA_NRT_O_NRT_DUE_SC_SLOT) as HSPA_NRT_O_NRT_DUE_SC_SLOT,
  SUM(HSPA_NRT_O_NRT_DUE_TRANS) as HSPA_NRT_O_NRT_DUE_TRANS,
  SUM(HSPA_RT_O_NRT_DUE_SC) as HSPA_RT_O_NRT_DUE_SC,
  MAX(MAX_HSUPA_USERS_IN_CELL) as MAX_HSUPA_USERS_IN_CELL,
  MAX(MAX_HSDPA_USERS_IN_CELL) as MAX_HSDPA_USERS_IN_CELL,
  SUM(AVE_CE_USED_AMR ) as AVE_CE_USED_AMR,
  SUM(ave_ce_used_ps_bgr_8_ul +  ave_ce_used_ps_bgr_16_ul + ave_ce_used_ps_bgr_32_ul + ave_ce_used_ps_bgr_64_ul +
  ave_ce_used_ps_bgr_128_ul + ave_ce_used_ps_bgr_256_ul + ave_ce_used_ps_bgr_384_ul
  ) as AVE_CE_USED_BGR_UL,
  SUM(ave_ce_used_ps_bgr_8_dl +  ave_ce_used_ps_bgr_16_dl + ave_ce_used_ps_bgr_32_dl + ave_ce_used_ps_bgr_64_dl +
  ave_ce_used_ps_bgr_128_dl + ave_ce_used_ps_bgr_256_dl + ave_ce_used_ps_bgr_384_dl
  ) as AVE_CE_USED_BGR_DL,
  SUM(ave_ce_used_ps_int_8_ul +  ave_ce_used_ps_int_16_ul + ave_ce_used_ps_int_32_ul + ave_ce_used_ps_int_64_ul +
  ave_ce_used_ps_int_128_ul + ave_ce_used_ps_int_256_ul + ave_ce_used_ps_int_384_ul
  ) as AVE_CE_USED_INT_UL,
  SUM(ave_ce_used_ps_int_8_dl +  ave_ce_used_ps_int_16_dl + ave_ce_used_ps_int_32_dl + ave_ce_used_ps_int_64_dl +
  ave_ce_used_ps_int_128_dl + ave_ce_used_ps_int_256_dl + ave_ce_used_ps_int_384_dl
  ) as AVE_CE_USED_INT_DL,
  sum(RTWP_CLASS_0 ) as RTWP_CLASS_0 ,
  sum(RTWP_CLASS_1 ) as RTWP_CLASS_1 ,
  sum(RTWP_CLASS_2 ) as RTWP_CLASS_2 ,
  sum(RTWP_CLASS_3 ) as RTWP_CLASS_3 ,
  sum(RTWP_CLASS_4 ) as RTWP_CLASS_4 ,
  sum(RTWP_CLASS_5 ) as RTWP_CLASS_5 ,
  sum(RTWP_CLASS_6 ) as RTWP_CLASS_6 ,
  sum(RTWP_CLASS_7 ) as RTWP_CLASS_7 ,
  sum(RTWP_CLASS_8 ) as RTWP_CLASS_8 ,
  sum(RTWP_CLASS_9 ) as RTWP_CLASS_9 ,
  sum(RTWP_CLASS_10) as RTWP_CLASS_10,
  sum(RTWP_CLASS_11) as RTWP_CLASS_11,
  sum(RTWP_CLASS_12) as RTWP_CLASS_12,
  sum(RTWP_CLASS_13) as RTWP_CLASS_13,
  sum(RTWP_CLASS_14) as RTWP_CLASS_14,
  sum(RTWP_CLASS_15) as RTWP_CLASS_15,
  sum(RTWP_CLASS_16) as RTWP_CLASS_16,
  sum(RTWP_CLASS_17) as RTWP_CLASS_17,
  sum(RTWP_CLASS_18) as RTWP_CLASS_18,
  sum(RTWP_CLASS_19) as RTWP_CLASS_19,
  sum(RTWP_CLASS_20) as RTWP_CLASS_20,
  sum(RTWP_CLASS_21) as RTWP_CLASS_21,
  sum(AVAIL_WCELL_IN_WO_STATE) as AVAIL_WCELL_IN_WO_STATE,
  sum(AVAIL_WCELL_EXISTS_IN_RNW_DB)as AVAIL_WCELL_EXISTS_IN_RNW_DB,
  sum(AVAIL_WCELL_BLOCKED_BY_USER)as AVAIL_WCELL_BLOCKED_BY_USER,
  SUM(CE_SAMPLE_AMOUNT) as CE_SAMPLE_AMOUNT

  from NOKRWW_PS_CELLRES_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME "
  
  qry6 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(NRT_EDCH_UL_DATA_VOL) as NRT_EDCH_UL_DATA_VOL,
  SUM(HS_DSCH_DATA_VOL) as HS_DSCH_DATA_VOL,
  SUM(ps_strea_dl_data + intera_dl_data + bgr_dl_data) as VOL_DL,
  SUM(ps_strea_ul_data  + intera_ul_data + bgr_ul_data) as VOL_UL,
  sum(NRT_DCH_UL_DATA_VOL) as NRT_DCH_UL_DATA_VOL,
  sum(NRT_DCH_DL_DATA_VOL) as NRT_DCH_DL_DATA_VOL
  
  from NOKRWW_PS_CELLTP_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry7 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(HSDPA_ORIG_DATA) as HSDPA_ORIG_DATA
  
  from NOKRWW_PS_CELTPW_MNC1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  
  qry8 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(HSDPA_BUFF_WITH_DATA_PER_TTI) as HSDPA_BUFF_WITH_DATA_PER_TTI,
  SUM(MACE_PDU_DATA_2MS_TTI) as MACE_PDU_DATA_2MS_TTI,
  SUM(MACE_PDU_DATA_10MS_TTI) as MACE_PDU_DATA_10MS_TTI,
  SUM(MACE_PDUS_2MS_TTI) as MACE_PDUS_2MS_TTI,
  SUM(MACE_PDUS_10MS_TTI) as MACE_PDUS_10MS_TTI
  
  from NOKRWW_PS_HSDPAW_MNC1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry)))
  }
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 8
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    #Connecting to database through RODBC
    con <- odbcConnect('NOKIA',uid="pmr",pwd="pmr")
    options(dec=",")
    #Test connection
    #odbcGetInfo(ch)
    result<- sqlQuery(con, qrys[i])
    #    result<-sqlQuery(ch,paste('SELECT * FROM ',databases[i]))  
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datanok <- outputPar[[1]]
  for (i in 2:length(outputPar)){datanok <- datanok %>% full_join(outputPar[[i]])}
  
  datanok <- filter(datanok, !is.na(CELDA))
  datanok <- filter(datanok,CELDA != "Nokia31" )
  
  datanok$NRT_EDCH_UL_DATA_VOL <- as.numeric(as.character(datanok$NRT_EDCH_UL_DATA_VOL))
  datanok$HS_DSCH_DATA_VOL <- as.numeric(as.character(datanok$HS_DSCH_DATA_VOL))
  
  datanok[is.na(datanok)] <- 0
  
  return(datanok)
  
}

queryData3GnokiaP <- function (data,hora){
  data$PROVINCIA <- "Cienfuegos"
  
  if(hora != "24"){
    
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
    
    datosG <- filter(data,substr(DIA,12,13)==hora)
    
    datosP <- datosG %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC_CS = round(100* as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) /as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)) * as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),3),
        
        ACC_PS = round(100 * as.numeric(sum(NUM_ACC_PS)/sum(DEN_ACC_PS))*as.numeric(sum(MULT_NUM1)/sum(MULT_NUM2)),3),
        
        RET_CS = round(100 -  as.numeric(100 * as.numeric(sum(NUM_RET_CS))/as.numeric(sum(DEN_RET_CS))),3),
        
        RET_PS = round(100 - as.numeric(100 * as.numeric(sum(NUM_RET_PS))/as.numeric(sum(DEN_RET_PS))),3),
        
        COD_RRC = round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_COD)/sum(RRC_CONN_STP_ATT)),3),
        
        COD_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_COD)+ sum(PS_SETUP_FAIL_AC_COD_NRT)) / as.numeric(sum(RAB_STP_COMP_CS_VOICE)+ sum(PS_RAB_ATT)),3),
        
        PWR_RRC = round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_UL)+sum(RRC_CONN_STP_FAIL_AC_DL) )/as.numeric(sum(RRC_CONN_STP_ATT)),3),
        
        PWR_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_UL)+sum(RAB_STP_FAIL_CS_VOICE_AC_DL) + sum(PS_SETUP_FAIL_AC_DL_NRT) + sum(PS_SETUP_FAIL_AC_UL_NRT))/as.numeric(sum(RAB_STP_COMP_CS_VOICE) + sum(PS_RAB_ATT)) ,3),
        
        CE_RRC = round(100* as.numeric(sum(RRC_CONN_STP_FAIL_BTS))/as.numeric(sum(RRC_CONN_STP_ATT)),3),
        
        CE_RAB = round(100* as.numeric(sum(RAB_STP_FAIL_CS_VOICE_BTS))/as.numeric(sum(RAB_STP_COMP_CS_VOICE)),3),
        
        TRAFF_CS = round(as.numeric(sum(AVG_RAB_HLD_TM_CS_VOICE))/360000,3),
        
        VOL = round(as.numeric(sum(VOL_DL)+sum(VOL_UL))/1000000000,3),
        
        USER_HSDPA = as.numeric(mean(MAX_HSDPA_USERS_IN_CELL)),
        
        USER_HSUPA = as.numeric(mean(MAX_HSUPA_USERS_IN_CELL)),
        
        DROP_CS = round(as.numeric(sum(NUM_RET_CS)),3),
        
        DROP_PS = round(as.numeric(sum(NUM_RET_PS)),3),
        
        AVAIL = round(100 * sum(AVAIL_WCELL_IN_WO_STATE)/as.numeric(sum(AVAIL_WCELL_EXISTS_IN_RNW_DB)-sum(AVAIL_WCELL_BLOCKED_BY_USER)),2)
      )
    return(datosP)
  }
  
  else{
    
    datosP <- data %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC_CS = round(100* as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) /as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)) * as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),3),
        
        ACC_PS = round(100 * as.numeric(sum(NUM_ACC_PS)/sum(DEN_ACC_PS))*as.numeric(sum(MULT_NUM1)/sum(MULT_NUM2)),3),
        
        RET_CS = round(100 -  as.numeric(100 * as.numeric(sum(NUM_RET_CS))/as.numeric(sum(DEN_RET_CS))),3),
        
        RET_PS = round(100 - as.numeric(100 * as.numeric(sum(NUM_RET_PS))/as.numeric(sum(DEN_RET_PS))),3),
        
        COD_RRC = round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_COD)/sum(RRC_CONN_STP_ATT)),3),
        
        COD_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_COD)+ sum(PS_SETUP_FAIL_AC_COD_NRT)) / as.numeric(sum(RAB_STP_COMP_CS_VOICE)++ sum(PS_RAB_ATT)),3),
        
        PWR_RRC = round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_UL)+sum(RRC_CONN_STP_FAIL_AC_DL) )/as.numeric(sum(RRC_CONN_STP_ATT)),3),
        
        PWR_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_UL)+sum(RAB_STP_FAIL_CS_VOICE_AC_DL) + sum(PS_SETUP_FAIL_AC_DL_NRT) + sum(PS_SETUP_FAIL_AC_UL_NRT))/as.numeric(sum(RAB_STP_COMP_CS_VOICE) + sum(PS_RAB_ATT)) ,3),
        
        CE_RRC = round(100* as.numeric(sum(RRC_CONN_STP_FAIL_BTS))/as.numeric(sum(RRC_CONN_STP_ATT)),3),
        
        CE_RAB = round(100* as.numeric(sum(RAB_STP_FAIL_CS_VOICE_BTS))/as.numeric(sum(RAB_STP_COMP_CS_VOICE)),3),
        
        TRAFF_CS = round(as.numeric(sum(AVG_RAB_HLD_TM_CS_VOICE))/360000,3),
        
        VOL = round(as.numeric(sum(VOL_DL)+sum(VOL_UL))/1000000000,3),
        
        USER_HSDPA = as.numeric(mean(MAX_HSDPA_USERS_IN_CELL)),
        
        USER_HSUPA = as.numeric(mean(MAX_HSUPA_USERS_IN_CELL)),
        
        DROP_CS = round(as.numeric(sum(NUM_RET_CS)),3),
        
        DROP_PS = round(as.numeric(sum(NUM_RET_PS)),3),
        
        AVAIL = round(100 * sum(AVAIL_WCELL_IN_WO_STATE)/as.numeric(sum(AVAIL_WCELL_EXISTS_IN_RNW_DB)-sum(AVAIL_WCELL_BLOCKED_BY_USER)),2)
        
        )
    return(datosP)
  }
  
}

queryData3GnokiaALL <- function (data,datos,hora){
  data$PROVINCIA <- "Cienfuegos"
  data$CELDA <- substr(data$CELDA,1,7)
  if(hora != "24"){
    
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
    datosG <- filter(data,substr(DIA,12,13)==hora)
    
    datosP <- datosG %>%group_by(CELDA)%>%
      summarise(
        ACC_CS = round(100* as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) /as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)) * as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),3),
        
        ACC_PS = round(100 * as.numeric(sum(NUM_ACC_PS)/sum(DEN_ACC_PS))*as.numeric(sum(MULT_NUM1)/sum(MULT_NUM2)),3),
        
        RET_CS = round(100 -  as.numeric(100 * as.numeric(sum(NUM_RET_CS))/as.numeric(sum(DEN_RET_CS))),3),
        
        RET_PS = round(100 - as.numeric(100 * as.numeric(sum(NUM_RET_PS))/as.numeric(sum(DEN_RET_PS))),3),
        
        COD_RRC = round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_COD)/sum(RRC_CONN_STP_ATT)),3),
        
        COD_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_COD)+ sum(PS_SETUP_FAIL_AC_COD_NRT)) / as.numeric(sum(RAB_STP_COMP_CS_VOICE)++ sum(PS_RAB_ATT)),3),
        
        PWR_RRC = round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_UL)+sum(RRC_CONN_STP_FAIL_AC_DL) )/as.numeric(sum(RRC_CONN_STP_ATT)),3),
        
        PWR_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_UL)+sum(RAB_STP_FAIL_CS_VOICE_AC_DL) + sum(PS_SETUP_FAIL_AC_DL_NRT) + sum(PS_SETUP_FAIL_AC_UL_NRT))/as.numeric(sum(RAB_STP_COMP_CS_VOICE) + sum(PS_RAB_ATT)) ,3),
        
        CE_RRC = round(100* as.numeric(sum(RRC_CONN_STP_FAIL_BTS))/as.numeric(sum(RRC_CONN_STP_ATT)),3),
        
        CE_RAB = round(100* as.numeric(sum(RAB_STP_FAIL_CS_VOICE_BTS))/as.numeric(sum(RAB_STP_COMP_CS_VOICE)),3),
        
        TRAFF_CS = round(as.numeric(sum(AVG_RAB_HLD_TM_CS_VOICE))/360000,3),
        
        VOL = round(as.numeric(sum(VOL_DL)+sum(VOL_UL))/1000000000,3),
        
        USER_HSDPA = as.numeric(mean(MAX_HSDPA_USERS_IN_CELL)),
        
        USER_HSUPA = as.numeric(mean(MAX_HSUPA_USERS_IN_CELL)),
        
        DROP_CS = round(as.numeric(sum(NUM_RET_CS)),3),
        
        DROP_PS = round(as.numeric(sum(NUM_RET_PS)),3),
        
        TROUGH_HSDPA =  round(as.numeric(sum(HSDPA_ORIG_DATA)*8*500) / as.numeric(sum(HSDPA_BUFF_WITH_DATA_PER_TTI) ) ,3),
        
        TROUGH_HSUPA =  round((as.numeric(sum(MACE_PDU_DATA_2MS_TTI) + sum(MACE_PDU_DATA_10MS_TTI))*8)/as.numeric(as.numeric(sum(MACE_PDUS_2MS_TTI) /500) + as.numeric(sum(MACE_PDUS_10MS_TTI) /100)),3),
        
        AVAIL = round(100 * sum(AVAIL_WCELL_IN_WO_STATE)/as.numeric(sum(AVAIL_WCELL_EXISTS_IN_RNW_DB)-sum(AVAIL_WCELL_BLOCKED_BY_USER)),2),
        
        RTWP = round(10 * log10(as.numeric(as.numeric(10^(-11)*sum(RTWP_CLASS_0)) + as.numeric(10^(-10.75)*sum(RTWP_CLASS_1)) +as.numeric(10^(-10.65)*sum(RTWP_CLASS_2)) +as.numeric(10^(-10.55)*sum(RTWP_CLASS_3)) +as.numeric(10^(-10.45)*sum(RTWP_CLASS_4))
                                           +as.numeric(10^(-10.25)*sum(RTWP_CLASS_5)) + as.numeric(10^(-10.25)*sum(RTWP_CLASS_6)) +as.numeric(10^(-10.15)*sum(RTWP_CLASS_7)) +as.numeric(10^(-10.05)*sum(RTWP_CLASS_8)) +as.numeric(10^(-9.95)*sum(RTWP_CLASS_9)) 
                                           +as.numeric(10^(-9.85)*sum(RTWP_CLASS_10)) + as.numeric(10^(-9.70)*sum(RTWP_CLASS_11)) +as.numeric(10^(-9.50)*sum(RTWP_CLASS_12)) +as.numeric(10^(-9.30)*sum(RTWP_CLASS_13)) +as.numeric(10^(-9.05)*sum(RTWP_CLASS_14)) 
                                           +as.numeric(10^(-8.75)*sum(RTWP_CLASS_15)) + as.numeric(10^(-8.45)*sum(RTWP_CLASS_16)) +as.numeric(10^(-8.15)*sum(RTWP_CLASS_17)) +as.numeric(10^(-7.75)*sum(RTWP_CLASS_18)) +as.numeric(10^(-7.25)*sum(RTWP_CLASS_19))
                                           +as.numeric(10^(-6.75)*sum(RTWP_CLASS_20)) + as.numeric(10^(-6.50)*sum(RTWP_CLASS_21)) )/
                                  as.numeric( sum(RTWP_CLASS_0) +sum(RTWP_CLASS_1)+sum(RTWP_CLASS_2)+sum(RTWP_CLASS_3)+sum(RTWP_CLASS_4)+sum(RTWP_CLASS_5)+sum(RTWP_CLASS_6)+sum(RTWP_CLASS_7)+sum(RTWP_CLASS_8)+sum(RTWP_CLASS_9)+sum(RTWP_CLASS_10)+
                                                +sum(RTWP_CLASS_11)+sum(RTWP_CLASS_12)+sum(RTWP_CLASS_13)+sum(RTWP_CLASS_14)+sum(RTWP_CLASS_15)+sum(RTWP_CLASS_16)+sum(RTWP_CLASS_17)+sum(RTWP_CLASS_18)+sum(RTWP_CLASS_19)+sum(RTWP_CLASS_20)+sum(RTWP_CLASS_21))
        ),2),
        
    
        PAGING_RATE = round(100 - 100 * as.numeric(sum(FAIL_PAG_NO_RESP_URA_PCH + FAIL_PAG_NO_RESP_CELL_PCH))/as.numeric(sum(PAGING_OCCASION_CELL_PCH + PAGING_OCCASION_URA_PCH)),2)
        
        )
    
    datosP <- datosP %>% inner_join(datos)
    return(datosP)
  }
  
  else{
    
    datosP <- data %>%group_by(CELDA)%>%
      summarise(
        ACC_CS = round(100* as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) /as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)) * as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),3),
        
        ACC_PS = round(100 * as.numeric(sum(NUM_ACC_PS)/sum(DEN_ACC_PS))*as.numeric(sum(MULT_NUM1)/sum(MULT_NUM2)),3),
        
        RET_CS = round(100 -  as.numeric(100 * as.numeric(sum(NUM_RET_CS))/as.numeric(sum(DEN_RET_CS))),3),
        
        RET_PS = round(100 - as.numeric(100 * as.numeric(sum(NUM_RET_PS))/as.numeric(sum(DEN_RET_PS))),3),
        
        COD_RRC = round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_COD)/sum(RRC_CONN_STP_ATT)),3),
        
        COD_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_COD)+ sum(PS_SETUP_FAIL_AC_COD_NRT)) / as.numeric(sum(RAB_STP_COMP_CS_VOICE)+ sum(PS_RAB_ATT)),3),
   
        PWR_RRC = round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_UL)+sum(RRC_CONN_STP_FAIL_AC_DL) )/as.numeric(sum(RRC_CONN_STP_ATT)),3),
        
        PWR_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_UL)+sum(RAB_STP_FAIL_CS_VOICE_AC_DL) + sum(PS_SETUP_FAIL_AC_DL_NRT) + sum(PS_SETUP_FAIL_AC_UL_NRT))/as.numeric(sum(RAB_STP_COMP_CS_VOICE) + sum(PS_RAB_ATT)) ,3),
        
        CE_RRC = round(100* as.numeric(sum(RRC_CONN_STP_FAIL_BTS))/as.numeric(sum(RRC_CONN_STP_ATT)),3),
        
        CE_RAB = round(100* as.numeric(sum(RAB_STP_FAIL_CS_VOICE_BTS))/as.numeric(sum(RAB_STP_COMP_CS_VOICE)),3),
        
        TRAFF_CS = round(as.numeric(sum(AVG_RAB_HLD_TM_CS_VOICE))/360000,3),
        
        VOL = round(as.numeric(sum(VOL_DL)+sum(VOL_UL))/1000000000,3),
        
        USER_HSDPA = as.numeric(mean(MAX_HSDPA_USERS_IN_CELL)),
        
        USER_HSUPA = as.numeric(mean(MAX_HSUPA_USERS_IN_CELL)),
        
        DROP_CS = round(as.numeric(sum(NUM_RET_CS)),3),
        
        DROP_PS = round(as.numeric(sum(NUM_RET_PS)),3),
        
        TROUGH_HSDPA =  round(as.numeric(sum(HSDPA_ORIG_DATA)*8*500) / as.numeric(sum(HSDPA_BUFF_WITH_DATA_PER_TTI) ) ,3),
        
        TROUGH_HSUPA =  round((as.numeric(sum(MACE_PDU_DATA_2MS_TTI) + sum(MACE_PDU_DATA_10MS_TTI))*8)/as.numeric(as.numeric(sum(MACE_PDUS_2MS_TTI) /500) + as.numeric(sum(MACE_PDUS_10MS_TTI) /100)),3),
        
        AVAIL = round(100 * sum(AVAIL_WCELL_IN_WO_STATE)/as.numeric(sum(AVAIL_WCELL_EXISTS_IN_RNW_DB)-sum(AVAIL_WCELL_BLOCKED_BY_USER)),2),
        RTWP = round(10 * log10(as.numeric(as.numeric(10^(-11)*sum(RTWP_CLASS_0)) + as.numeric(10^(-10.75)*sum(RTWP_CLASS_1)) +as.numeric(10^(-10.65)*sum(RTWP_CLASS_2)) +as.numeric(10^(-10.55)*sum(RTWP_CLASS_3)) +as.numeric(10^(-10.45)*sum(RTWP_CLASS_4))
                                           +as.numeric(10^(-10.25)*sum(RTWP_CLASS_5)) + as.numeric(10^(-10.25)*sum(RTWP_CLASS_6)) +as.numeric(10^(-10.15)*sum(RTWP_CLASS_7)) +as.numeric(10^(-10.05)*sum(RTWP_CLASS_8)) +as.numeric(10^(-9.95)*sum(RTWP_CLASS_9)) 
                                           +as.numeric(10^(-9.85)*sum(RTWP_CLASS_10)) + as.numeric(10^(-9.70)*sum(RTWP_CLASS_11)) +as.numeric(10^(-9.50)*sum(RTWP_CLASS_12)) +as.numeric(10^(-9.30)*sum(RTWP_CLASS_13)) +as.numeric(10^(-9.05)*sum(RTWP_CLASS_14)) 
                                           +as.numeric(10^(-8.75)*sum(RTWP_CLASS_15)) + as.numeric(10^(-8.45)*sum(RTWP_CLASS_16)) +as.numeric(10^(-8.15)*sum(RTWP_CLASS_17)) +as.numeric(10^(-7.75)*sum(RTWP_CLASS_18)) +as.numeric(10^(-7.25)*sum(RTWP_CLASS_19))
                                           +as.numeric(10^(-6.75)*sum(RTWP_CLASS_20)) + as.numeric(10^(-6.50)*sum(RTWP_CLASS_21)) )/
                                  as.numeric( sum(RTWP_CLASS_0) +sum(RTWP_CLASS_1)+sum(RTWP_CLASS_2)+sum(RTWP_CLASS_3)+sum(RTWP_CLASS_4)+sum(RTWP_CLASS_5)+sum(RTWP_CLASS_6)+sum(RTWP_CLASS_7)+sum(RTWP_CLASS_8)+sum(RTWP_CLASS_9)+sum(RTWP_CLASS_10)+
                                                +sum(RTWP_CLASS_11)+sum(RTWP_CLASS_12)+sum(RTWP_CLASS_13)+sum(RTWP_CLASS_14)+sum(RTWP_CLASS_15)+sum(RTWP_CLASS_16)+sum(RTWP_CLASS_17)+sum(RTWP_CLASS_18)+sum(RTWP_CLASS_19)+sum(RTWP_CLASS_20)+sum(RTWP_CLASS_21))
        ),2),
        
        PAGING_RATE = round(100 - 100 * as.numeric(sum(FAIL_PAG_NO_RESP_URA_PCH + FAIL_PAG_NO_RESP_CELL_PCH))/as.numeric(sum(PAGING_OCCASION_CELL_PCH+PAGING_OCCASION_URA_PCH)),2)
        
        )
    datosP <- datosP %>% inner_join(datos)
    return(datosP)
    
  } 
}


#Huawei 2G
queryData2GHUAWEI <- function(idate,fdate,datos,granul){

  if(granul == 'yyyy-mm-dd hh' ){
    granul <- 'YYYY-MM-DD HH24'
  }
  
  if(granul == 'yyyy-mm-dd' ){
    granul <- 'YYYY-MM-DD'
  }
  qry1 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(K3000) AS K3000,
  SUM(K3003) AS K3003,
  SUM(K3001) AS K3001,
  SUM(K3010A) AS K3010A,
  SUM(K3011A) AS K3011A,
  SUM(K3013A) AS K3013A,
  SUM(K3040) AS K3040,
  MAX(K3045) AS K3045,
  SUM(K3014)  AS K3014,
  SUM(K3004)  AS K3004
  FROM STATDBA.OSS_HUA_KPICEL_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  
  qry2 <-"SELECT  OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(CM30) AS CM30,
  SUM(CM33) AS CM33
  FROM  STATDBA.OSS_HUA_CDROP_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  
  qry3 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(CH363) AS CH363
  FROM STATDBA.OSS_HUA_IINTERRAT_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  
  qry4<-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(CH303) AS CH303
  FROM STATDBA.OSS_HUA_INTRAHCEL_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  qry5 <-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(CH333) AS CH333
  FROM STATDBA.OSS_HUA_OEINTERHC_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  
  qry6 <-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(CH353) AS CH353
  FROM STATDBA.OSS_HUA_OINTERRAT_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  
  qry7 <-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(CH313) AS CH313
  FROM STATDBA.OSS_HUA_OIINTERHC_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  qry8 <-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(A330) AS A330,
  SUM(A331) AS A331,
  SUM(A337) AS A337,
  SUM(A338) AS A338,
  SUM(A339) AS A339,
  SUM(A340) AS A340
  FROM STATDBA.OSS_HUA_PAGING_ABIS
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"

  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry)))
  }             
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 8
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    
    con <- odbcConnect('SIGESTOP',uid="felix",pwd="F3l!x2k10")
    options(dec=",")
   
    result<- sqlQuery(con, qrys[i])
 
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datahua <- outputPar[[1]]
  for (i in 2:length(outputPar)){datahua <- datahua %>% full_join(outputPar[[i]])}

  datahua$CELDA<-substr(datahua$CELDA,7,stop=unlist(lapply(gregexpr(',',datahua$CELDA), `[[`, 1))-1)
  
  datahua <- datahua %>% inner_join(datos) %>%group_by(CELDA)
  
  datahua$K3014 <- as.numeric(as.character(datahua$K3014))
  datahua$K3004 <- as.numeric(as.character(datahua$K3004))
  
  datahua[is.na(datahua)] <- 0
 
  return(datahua)
}

queryData2GHUAWEIP <- function(data,hora){
 
  if(hora != "24"){
    
    value <- as.integer(hora)
    if(value < 10){
     hora <- paste0("0",hora,collapse =" ")
    }
    
    datosG <- filter(data,substr(DIA,12,13)==hora,!is.na(K3000))
    
    
    
    datosP <- datosG %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC = round(100* as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                     *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                     *(sum(K3013A) / sum(K3010A))
        ),3),
        
        RET = round(100 * (as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))/
                             as.numeric(sum(K3040) + sum(CH363) - sum(CH303)- sum(CH313) - sum(CH333) - sum(CH353))),3),
        
        SER = round( 100 * ((as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                         *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                         *(sum(K3013A) / sum(K3010A))
        ))
        *(as.numeric(sum(K3040) + sum(CH363) -  sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))
          / as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333)- sum(CH353)))),3),
        
        CONG_TCH =   round(100 * as.numeric(sum(K3011A)/ sum(K3010A)),3),
        CONG_SDCCH =  round(100 * as.numeric(sum(K3001)/ sum(K3000)),3),
        TRAFF_TCH = round(as.numeric(sum(K3014)),3),
        TRAFF_SDCCH = round(as.numeric(sum(K3004)),3),
        CALL_DROP_TCH = sum(CM30),
        CALL_DROP_SDCCH = sum(CM33),
        PAGINGS = as.numeric(sum(A338))
      )
    return(datosP)
  }
  else
  {
    datosN <- data%>%group_by(CELDA)%>%filter(!is.na(K3000))
    
    
    datosP <- datosN %>%group_by(DIA,PROVINCIA)%>%
      summarise(
        ACC = round(100* as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                     *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                     *(sum(K3013A) / sum(K3010A))
        ),3),
        
        RET = round(100 * (as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))/
                             as.numeric(sum(K3040) + sum(CH363) - sum(CH303)- sum(CH313) - sum(CH333) - sum(CH353))),3),
        
        SER = round( 100 * ((as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                         *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                         *(sum(K3013A) / sum(K3010A))
        ))
        *(as.numeric(sum(K3040) + sum(CH363) -  sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))
          / as.numeric(sum(K3040) + sum(CH363)- sum(CH303)  - sum(CH313) - sum(CH333)- sum(CH353)))),3),
        
        CONG_TCH =   round(100 * as.numeric(sum(K3011A)/ sum(K3010A)),3),
        CONG_SDCCH =  round(100 * as.numeric(sum(K3001)/ sum(K3000)),3),
        TRAFF_TCH = round(as.numeric(sum(K3014)),3),
        TRAFF_SDCCH = round(as.numeric(sum(K3004)),3),
        CALL_DROP_TCH = sum(CM33),
        CALL_DROP_SDCCH = sum(CM30),
        PAGINGS = as.numeric(sum(A338))
      )
    return(datosP)
  }
  
}

queryData2GHUAWEIAll <- function(data,datos,hora){
  if(hora != "24"){
    
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
      
  datosG <- filter(data,substr(DIA,12,13)==hora)
  datosP <- datosG%>% group_by(CELDA)%>%
    summarise(
      ACC = round(100* as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                   *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                   *(sum(K3013A) / sum(K3010A))
      ),3),
      
      RET = round(100 * (as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))/
                           as.numeric(sum(K3040) + sum(CH363) - sum(CH303)- sum(CH313) - sum(CH333) - sum(CH353))),3),
      
      SER = round( 100 * ((as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                       *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                       *(sum(K3013A) / sum(K3010A))
      ))
      *(as.numeric(sum(K3040) + sum(CH363) -  sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))
        / as.numeric(sum(K3040) + sum(CH363)  - sum(CH313) - sum(CH333)- sum(CH303)- sum(CH353)))),3),
      
      CONG_TCH =   round(100 * as.numeric(sum(K3011A)/ sum(K3010A)),3),
      CONG_SDCCH = round(100 * as.numeric(sum(K3001)/ sum(K3000)),3),
      PAGINGS = as.numeric(mean(A338 + A339)),
      TRAFF_TCH = round(as.numeric(sum(K3014)),3),
      TRAFF_SDCCH = round(as.numeric(sum(K3004)),3),
      CALL_DROP_TCH = sum(CM33),
      CALL_DROP_SDCCH = sum(CM30),
      PAGING_RATE = round(100 - 100 * as.numeric(sum(A338 + A339))/as.numeric(sum(A330 + A331)),3)
      
    )
  
  datosT <- datosP %>% inner_join(datos)
  
  return(datosT)
  }
  else{
    datosP <- data%>% group_by(CELDA)%>%
      summarise(
        ACC = round(100* as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                     *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                     *(sum(K3013A) / sum(K3010A))
        ),3),
        
        RET = round(100 * (as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))/
                             as.numeric(sum(K3040) + sum(CH363) - sum(CH303)- sum(CH313) - sum(CH333) - sum(CH353))),3),
        
        SER = round( 100 * ((as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                         *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                         *(sum(K3013A) / sum(K3010A))
        ))
        *(as.numeric(sum(K3040) + sum(CH363) -  sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))
          / as.numeric(sum(K3040) + sum(CH363)  - sum(CH313) - sum(CH333)- sum(CH303)- sum(CH353)))),3),
        
        CONG_TCH =   round(100 * as.numeric(sum(K3011A)/ sum(K3010A)),3),
        CONG_SDCCH = round(100 * as.numeric(sum(K3001)/ sum(K3000)),3),
        PAGINGS = as.numeric(mean(A338 + A339)),
        TRAFF_TCH = round(as.numeric(sum(K3014)),3),
        TRAFF_SDCCH = round(as.numeric(sum(K3004)),3),
        CALL_DROP_TCH = sum(CM33),
        CALL_DROP_SDCCH = sum(CM30),
        PAGING_RATE = round(100 - 100 * as.numeric(sum(A338 + A339))/as.numeric(sum(A330 + A331)),3)
      )
    
    datosT <- datosP %>% inner_join(datos)
    
    return(datosT)
  }
  
  
}

#Huawei 3G
queryData3GHuawei <- function(idate,fdate,datos,granul){
  
  
  if(granul == 'yyyy-mm-dd hh' ){
    granul <- 'YYYY-MM-DD HH24'
  }
  
  qry1<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(C1) AS RRC_CONN_SETUP,
  SUM(C2) AS RRC_CONN_ATT,
  SUM(C3) AS RRC_CONN_FAIL,
  SUM(C4) AS SetupConnEstab,
  SUM(C5) AS PS_DROP_FD_DEN,
  SUM(C6) AS PS_DROP_FD_NUM,
  SUM(C7) AS DEN_AMR_Call_DROP,
  SUM(C8) AS DEN_AMR_RAB,
  SUM(C9) AS DEN_CS_RRC,
  SUM(C10) AS DEN_PS_CALL,
  SUM(C11) AS DEN_PS_RAB,
  SUM(C12) AS DEN_PS_RRC,
  SUM(C13) AS NUM_AMR_Call_DROP,
  SUM(C14) AS NUM_AMR_RAB,
  SUM(C15) AS NUM_CS_RRC,
  SUM(C16) AS NUM_PS_CALL,
  SUM(C17) AS NUM_PS_RAB,
  SUM(C18) AS NUM_PS_RRC
  FROM STATDBA.OSS_HUA_RRC_SETUP
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  qry2<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(C1) AS FailConnEstb_Cong,
  SUM(C2) AS CODE_CONG_RRC,
  SUM (C3 + C6) as CE_CONG_RRC,
  SUM (C5 + C8) as PWR_CONG_RRC,
  SUM (C4 + C7) as IUB_CONG_RRC 
  FROM STATDBA.OSS_HUA_RRC_REJECT
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"

  qry3<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(C3) AS Cong_PS_RAB,
  SUM(C5 + C6 + C7 + C10) as PS_RAB_ATT
  FROM STATDBA.OSS_HUA_PS_RAB_SETUP
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD '))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  qry4<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(C1) as CODE_PS_CONG_RAB,
  SUM(C2) as CELL_PS_CONG_RAB,
  SUM (C3 + C8 ) as CE_PS_CONG_RAB,
  SUM (C4 + C9 ) as IUB_PS_CONG_RAB,
  SUM (C5 + C10 ) as PWR_PS_CONG_RAB
  FROM STATDBA.OSS_HUA_PS_RAB_FAIL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  qry5 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(C2) AS RAB_CS_ATT
  
  FROM STATDBA.OSS_HUA_CS_RAB_SETUP
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ "
  
  qry6 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(C1) as CODE_CS_CONG_RAB,
  SUM(C2) as CELL_CS_CONG_RAB,
  SUM (C3 + C6 ) as CE_CS_CONG_RAB,
  SUM (C4 + C7 ) as IUB_CS_CONG_RAB,
  SUM (C5 + C8 ) as PWR_CS_CONG_RAB
  FROM STATDBA.OSS_HUA_CS_RAB_FAIL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ "
  
  qry7 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(C4)  AS  TRAFICO_CS
  FROM STATDBA.OSS_HUA_RADIO_BEARER
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ "
  
  qry8 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  MAX(C3) AS MAX_USER_HSDPA,
  MAX (C4)  AS MAX_TROUGHPUT_HSDPA,
  AVG (C4)  AS AVG_TROUGHPUT_HSDPA,
  SUM (C7) AS TOTAL_VOLUMEN_HSDPA
  FROM STATDBA.OSS_HUA_HSDPA_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ "
  
  qry9 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM (C6) AS TOTAL_VOLUMEN_HSUPA
  FROM STATDBA.OSS_HUA_HSUPA_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ "
  
  
  qry10 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  SUM(C1)	AS CS_DROP_A,				
  SUM(C2)	AS CS_DROP_N,				
  SUM(C3)	AS PS_DROP_A,				
  SUM(C4)	AS PS_DROP_N				
  FROM STATDBA.OSS_HUA_RAB_REL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ "
  
  qry11 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  AVG(C11) as RTWP
  FROM STATDBA.OSS_HUA_RTWP_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  qry12 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,
  
  SUM(C8) as PAGING_DISCARD,
  SUM(C9) as PAGING_SENT
  FROM STATDBA.OSS_HUA_PAGING_3G
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ"
  
  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry)))
  }             
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8,qry9,qry10,qry11,qry12)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 12
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    
    con <- odbcConnect('SIGESTOP',uid="felix",pwd="F3l!x2k10")
    options(dec=",")
    
    result<- sqlQuery(con, qrys[i])
    
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datahua <- outputPar[[1]]
  for (i in 2:length(outputPar)){datahua <- datahua %>% full_join(outputPar[[i]])}
  
  datahua$CONG_PS_RAB <- as.numeric(as.character(datahua$CONG_PS_RAB))
  datahua$TRAFICO_CS <- as.numeric(as.character(datahua$TRAFICO_CS))
  datahua$MAX_TROUGHPUT_HSDPA <- as.numeric(as.character(datahua$MAX_TROUGHPUT_HSDPA))
  datahua$AVG_TROUGHPUT_HSDPA <- as.numeric(as.character(datahua$AVG_TROUGHPUT_HSDPA))
  datahua$TOTAL_VOLUMEN_HSDPA <- as.numeric(as.character(datahua$TOTAL_VOLUMEN_HSDPA))
  datahua$TOTAL_VOLUMEN_HSUPA <- as.numeric(as.character(datahua$TOTAL_VOLUMEN_HSUPA))
  datahua$RTWP <- as.numeric(as.character(datahua$RTWP))
 
  datahua[is.na(datahua)] <- 0
  
  datahua <- datahua %>% inner_join(datos)
  
  datahua$TOTAL_VOLUMEN_HSUPA = ifelse(datahua$TOTAL_VOLUMEN_HSUPA == 0, 1, datahua$TOTAL_VOLUMEN_HSUPA)
  datahua$TOTAL_VOLUMEN_HSDPA = ifelse(datahua$TOTAL_VOLUMEN_HSDPA == 0, 1 , datahua$TOTAL_VOLUMEN_HSDPA)
  
  return(datahua)
}

queryData3GHUAWEIP <- function(data,hora){
  if(hora != "24"){
    
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
    
    datosG <- filter(data,substr(DIA,12,13)==hora)  
    datosP <- datosG %>% group_by(DIA,PROVINCIA)%>%
      summarise(
        
        ACC_CS =  round( 100  *(as.numeric(as.numeric(sum(NUM_AMR_RAB)) * as.numeric(sum(NUM_CS_RRC)))  ) / 
                                (as.numeric(as.numeric(sum(DEN_CS_RRC)) * as.numeric(sum(DEN_AMR_RAB)))) ,3),
        
        ACC_PS = round( 100  *(as.numeric(as.numeric(sum(NUM_PS_RAB)) * as.numeric(sum(NUM_PS_RRC))))  / 
                               (as.numeric(as.numeric(sum(DEN_PS_RAB)) * as.numeric(sum(DEN_PS_RRC)))),3),
        
        RET_CS = 100 - round(100 * (as.numeric(as.numeric(sum(NUM_AMR_CALL_DROP)) / as.numeric(sum(DEN_AMR_CALL_DROP)))) ,3 ),
        
        RET_PS = 100 -  100 * round((as.numeric(as.numeric(sum(PS_DROP_FD_NUM))/as.numeric(sum(PS_DROP_FD_DEN)))),3),
        
        CONG_CE_RRC = round (100 * as.numeric(sum(CE_CONG_RRC) )/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_PWR_RRC = round (100 * as.numeric(sum(PWR_CONG_RRC))/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_CODE_RRC = round (100 * as.numeric(sum(CODE_CONG_RRC))/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_CE_RAB = round (100 * as.numeric(sum(CE_PS_CONG_RAB) + sum(CE_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        #
        CONG_PWR_RAB = round (100 * as.numeric(sum(PWR_PS_CONG_RAB) + sum(PWR_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        #
        CONG_CODE_RAB = round (100 * as.numeric(sum(CODE_PS_CONG_RAB) + sum(CODE_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        
        TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS)),3),
        
        VOLUMEN_DATOS = round(as.numeric(sum(TOTAL_VOLUMEN_HSUPA) + sum(TOTAL_VOLUMEN_HSDPA)  )/1000000,3),
        
        DROP_CS = as.numeric(sum(CS_DROP_A)),
        
        DROP_PS = as.numeric(sum(PS_DROP_A)),
        
        RTWP = round(as.numeric(sum(RTWP)),3),
        
        AVG_TROUGHPUT = round(as.numeric(mean(AVG_TROUGHPUT_HSDPA)),3),
        
        PAGING = round(as.numeric(sum(PAGING_DISCARD)),2),
        
        PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_DISCARD))/as.numeric(sum(PAGING_SENT)),2)
         
      )
    
    
    return(datosP)
  }
  
  else{
    
    datosP <- data %>% group_by(DIA,PROVINCIA)%>%
        summarise(
          
          ACC_CS =  round(100  *(as.numeric(as.numeric(sum(NUM_AMR_RAB)) * as.numeric(sum(NUM_CS_RRC)))  ) / 
                                  (as.numeric(as.numeric(sum(DEN_CS_RRC)) * as.numeric(sum(DEN_AMR_RAB)))) ,3),
          
          ACC_PS =  round(100 *(as.numeric(as.numeric(sum(NUM_PS_RAB)) * as.numeric(sum(NUM_PS_RRC))))  / 
                                 (as.numeric(as.numeric(sum(DEN_PS_RAB)) * as.numeric(sum(DEN_PS_RRC)))),3),
          
          RET_CS = 100 - round(100 * (as.numeric(as.numeric(sum(NUM_AMR_CALL_DROP)) / as.numeric(sum(DEN_AMR_CALL_DROP)))) ,2 ),
          
          RET_PS = 100 -  100 * round((as.numeric(as.numeric(sum(PS_DROP_FD_NUM))/as.numeric(sum(PS_DROP_FD_DEN)))),3),
          
          CONG_CE_RRC = round (100 * as.numeric(sum(CE_CONG_RRC) )/ as.numeric(sum(RRC_CONN_SETUP)),3),
          
          CONG_PWR_RRC = round (100 * as.numeric(sum(PWR_CONG_RRC))/ as.numeric(sum(RRC_CONN_SETUP)),3),
          
          CONG_CODE_RRC = round (100 * as.numeric(sum(CODE_CONG_RRC))/ as.numeric(sum(RRC_CONN_SETUP)),3),
          
          CONG_CE_RAB = round (100 * as.numeric(sum(CE_PS_CONG_RAB) + sum(CE_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
          #
          CONG_PWR_RAB = round (100 * as.numeric(sum(PWR_PS_CONG_RAB) + sum(PWR_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
          #
          CONG_CODE_RAB = round (100 * as.numeric(sum(CODE_PS_CONG_RAB) + sum(CODE_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
          
          TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS)),3),
          
          VOLUMEN_DATOS = round(as.numeric(sum(TOTAL_VOLUMEN_HSUPA) + sum(TOTAL_VOLUMEN_HSDPA)  )/1000000000,3),
          
          DROP_CS = as.numeric(sum(CS_DROP_A)),
          
          DROP_PS = as.numeric(sum(PS_DROP_A)),
          
          RTWP = round(as.numeric(mean(RTWP)),3),
          
          AVG_TROUGHPUT = round(as.numeric(mean(AVG_TROUGHPUT_HSDPA)),3),
          
          PAGING = round(as.numeric(sum(PAGING_DISCARD)),2),
          
          PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_DISCARD))/as.numeric(sum(PAGING_SENT)),2)
          
        )
    return(datosP)
  }
}

queryData3GHuaweiAll <- function(data,datos,hora){
  if(hora != "24"){
    
    
    
    value <- as.integer(hora)
    if(value < 10){
      hora <- paste0("0",hora,collapse =" ")
    }
    datosG <- filter(data,substr(DIA,12,13)==hora)  
    datos1 <- datosG %>%group_by(CELDA)%>%
      summarise(
        ACC_CS =  round( 100  *(as.numeric(as.numeric(sum(NUM_AMR_RAB)) * as.numeric(sum(NUM_CS_RRC)))  ) / 
                           (as.numeric(as.numeric(sum(DEN_CS_RRC)) * as.numeric(sum(DEN_AMR_RAB)))) ,3),
        
        ACC_PS = round( 100  *(as.numeric(as.numeric(sum(NUM_PS_RAB)) * as.numeric(sum(NUM_PS_RRC))))  / 
                          (as.numeric(as.numeric(sum(DEN_PS_RAB)) * as.numeric(sum(DEN_PS_RRC)))),3),
        
        RET_CS = 100 - round(100 * (as.numeric(as.numeric(sum(NUM_AMR_CALL_DROP)) / as.numeric(sum(DEN_AMR_CALL_DROP)))) ,3 ),
        
        RET_PS = 100 -  100 * round((as.numeric(as.numeric(sum(PS_DROP_FD_NUM))/as.numeric(sum(PS_DROP_FD_DEN)))),3),
        
        CONG_CE_RRC = round (100 * as.numeric(sum(CE_CONG_RRC) )/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_PWR_RRC = round (100 * as.numeric(sum(PWR_CONG_RRC))/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_CODE_RRC = round (100 * as.numeric(sum(CODE_CONG_RRC))/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_CE_RAB = round (100 * as.numeric(sum(CE_PS_CONG_RAB) + sum(CE_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        #
        CONG_PWR_RAB = round (100 * as.numeric(sum(PWR_PS_CONG_RAB) + sum(PWR_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        #
        CONG_CODE_RAB = round (100 * as.numeric(sum(CODE_PS_CONG_RAB) + sum(CODE_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        
        MAX_TROUGHPUT = round(as.numeric(sum(MAX_TROUGHPUT_HSDPA)),3),
        
        AVG_TROUGHPUT = round(as.numeric(mean(AVG_TROUGHPUT_HSDPA)),3),
        
        #
        MAX_USER_HSDPA = round(as.numeric(max(MAX_USER_HSDPA)),3),
        
        VOLUMEN_DATOS = round(as.numeric(sum(TOTAL_VOLUMEN_HSUPA) + sum(TOTAL_VOLUMEN_HSDPA))/1000000,3),
        
        DROP_CS = as.numeric(sum(CS_DROP_A)),
        
        DROP_PS = as.numeric(sum(PS_DROP_A)),
        
        RTWP = round(as.numeric(mean(RTWP)),3),
        
        PAGING = round(as.numeric(sum(PAGING_DISCARD)),0),
        
        PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_DISCARD))/as.numeric(sum(PAGING_SENT)),2)
        
        #TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS)),3)
        
      )
    
    datosF <- datos1%>% inner_join(datos)
    return(datosF)
  }
  else
  {
    datos1 <- data %>%group_by(CELDA)%>% 
      summarise(
        ACC_CS =  round( 100  *(as.numeric(as.numeric(sum(NUM_AMR_RAB)) * as.numeric(sum(NUM_CS_RRC)))  ) / 
                           (as.numeric(as.numeric(sum(DEN_CS_RRC)) * as.numeric(sum(DEN_AMR_RAB)))) ,3),
        
        ACC_PS = round( 100  *(as.numeric(as.numeric(sum(NUM_PS_RAB)) * as.numeric(sum(NUM_PS_RRC))))  / 
                          (as.numeric(as.numeric(sum(DEN_PS_RAB)) * as.numeric(sum(DEN_PS_RRC)))),3),
        
        RET_CS = 100 - round(100 * (as.numeric(as.numeric(sum(NUM_AMR_CALL_DROP)) / as.numeric(sum(DEN_AMR_CALL_DROP)))) ,3 ),
        
        RET_PS = 100 -  100 * round((as.numeric(as.numeric(sum(PS_DROP_FD_NUM))/as.numeric(sum(PS_DROP_FD_DEN)))),3),
        
        CONG_CE_RRC = round (100 * as.numeric(sum(CE_CONG_RRC) )/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_PWR_RRC = round (100 * as.numeric(sum(PWR_CONG_RRC))/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_CODE_RRC = round (100 * as.numeric(sum(CODE_CONG_RRC))/ as.numeric(sum(RRC_CONN_SETUP)),3),
        
        CONG_CE_RAB = round (100 * as.numeric(sum(CE_PS_CONG_RAB) + sum(CE_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        #
        CONG_PWR_RAB = round (100 * as.numeric(sum(PWR_PS_CONG_RAB) + sum(PWR_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        #
        CONG_CODE_RAB = round (100 * as.numeric(sum(CODE_PS_CONG_RAB) + sum(CODE_CS_CONG_RAB))/ as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),3),
        MAX_TROUGHPUT = round(as.numeric(max(MAX_TROUGHPUT_HSDPA)),3),
        
        AVG_TROUGHPUT = round(as.numeric(mean(AVG_TROUGHPUT_HSDPA)),3),
        
        MAX_USER_HSDPA = round(as.numeric(max(MAX_USER_HSDPA)),3),
        
        VOLUMEN_DATOS = round(as.numeric(sum(TOTAL_VOLUMEN_HSUPA) + sum(TOTAL_VOLUMEN_HSDPA)  )/1000000,3),
        
        DROP_CS = as.numeric(sum(CS_DROP_A)),
        
        DROP_PS = as.numeric(sum(PS_DROP_A)),
        
        RTWP = round(as.numeric(mean(RTWP)),3),
        
        PAGING = round(as.numeric(sum(PAGING_DISCARD)),0),
        
        PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_DISCARD))/as.numeric(sum(PAGING_SENT)),2)
        
        #TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS)),3)
        
      )
    
    datosF <- datos1%>% inner_join(datos)
    return(datosF)
  }
}

#Tablero
data2GTablero <-function(dataH,dataE,dataN){
  
  datosHua <- dataH  %>% group_by(DIA)%>% 
    summarise(
      
      ACC_NUM_H = as.numeric(sum(K3000)- sum(K3001))*as.numeric(sum(K3003)-sum(CM30))*as.numeric(sum(K3013A)),
      
      ACC_DEN_H = as.numeric(sum(K3000))*as.numeric(sum(K3003)) * as.numeric(sum(K3010A)),
      
      RET_NUM_H = as.numeric( sum(K3040)+ sum(CH363) - sum(CH303)- sum(CH313)- sum(CH333)- sum(CH353)- sum(CM33)),
      
      RET_DEN_H = as.numeric(sum(K3040)+sum(CH363)- sum(CH313)-sum(CH333)- sum(CH353))
      
    )
  
  datosEri <- dataE %>% group_by(DIA)%>%
    summarise(
      ACC_NUM_E = as.numeric(sum(CCALLS) - sum(CCONGS))* as.numeric(sum(CMSESTAB)- sum(CDISSS) + sum(CDISQA) + sum(CDISTA) )
      * as.numeric(sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB)),
      
      ACC_DEN_E = as.numeric(as.numeric(sum(CCALLS)) * as.numeric(sum(CMSESTAB)) * as.numeric(sum(TASSALL))),
      
      RET_NUM_E = as.numeric(sum(TFMSESTB) + sum(THMSESTB) )-as.numeric( sum(TFNDROP) + sum(THNDROP) + sum(TFNDROPSUB) + sum(THNDROPSUB)),
      
      RET_DEN_E = as.numeric(sum(TFMSESTB) + sum(THMSESTB))
      )
  
  datosNok <- dataN %>% group_by(DIA)%>%
    summarise(
      ACC_NUM_N = as.numeric(sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)) * as.numeric(sum(SDCCH_DROP_CALL_AND_HO) ) ,
     
      ACC_DEN_N = as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) ) * as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  ),
      
      RET_NUM_N = as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN)) + as.numeric(sum(TCH_HO_ASSIGN) ) -  as.numeric(sum(DROP_AFTER_TCH_ASSIGN)) - as.numeric(sum(TCH_HO_RELEASE)) ,
      
      RET_DEN_N = (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE)))
    )

  datosRed <- datosHua  %>%  inner_join(datosEri)%>% inner_join(datosNok)%>%group_by(DIA)
  
  return(list("datosHua"= datosHua, "datosEri"=datosEri,"datosRed"= datosRed,"datosNok"=datosNok ))
  
}
data3GTablero <-function(dataE,dataH,dataN){
  
  datosHua <- dataH  %>% group_by(DIA)%>% 
  summarise(
        ACC_CS_NUM_H = as.numeric(as.numeric(sum(NUM_AMR_RAB)) * as.numeric(sum(NUM_CS_RRC))),
        
        ACC_CS_DEN_H = as.numeric(as.numeric(sum(DEN_CS_RRC)) * as.numeric(sum(DEN_AMR_RAB))),
        
        ACC_PS_NUM_H = as.numeric(as.numeric(sum(NUM_PS_RAB)) * as.numeric(sum(NUM_PS_RRC))),
        
        ACC_PS_DEN_H = as.numeric(as.numeric(sum(DEN_PS_RAB)) * as.numeric(sum(DEN_PS_RRC))),
        
        RET_PS_NUM_H = as.numeric(sum(PS_DROP_FD_NUM)),
        
        RET_PS_DEN_H = as.numeric(sum(PS_DROP_FD_DEN)),
        
        RET_CS_NUM_H = as.numeric(sum(NUM_AMR_CALL_DROP)),
        
        RET_CS_DEN_H = as.numeric(sum(DEN_AMR_CALL_DROP))
    )
  
  
  datosEri <- dataE %>% group_by(DIA)%>%
    summarise(
      ACC_CS_NUM_E = as.numeric(sum(PmNoRRCSucc))*as.numeric(sum(PmNoRABSuccSpeech) + sum(PmNoRAB64) + sum(PmEstaSucc)),
      
      ACC_CS_DEN_E = as.numeric(sum(PmNoRRC) - sum(PmLoadShar)) * as.numeric(sum(PmEstaAtt) + sum(PmNoRABAttSpeech) + sum(PmRABAttCs64) - sum(PmNoDirRetry)),
      
      ACC_PS_NUM_E = as.numeric(sum(PmToConnectPsSucc))*as.numeric(sum(PmNoRABSuccPack)),
      
      ACC_PS_DEN_E = as.numeric(sum(PmToConnectPs) - sum(PmLoadSharing)) * as.numeric(sum(PmNoRABAttPack)),
      
      RET_CS_NUM_E = as.numeric(sum(PmSysRABSpeech) + sum(PmSysRABCS64)),
      
      RET_CS_DEN_E = as.numeric(sum(PmSysNormalSpeech) + sum(PmSysNormalCS64) + sum(PmSysRABSpeech) + sum(PmSysRABCS64)),
      
      RET_PS_NUM_E = as.numeric( sum(as.numeric(PmSysRAB )- as.numeric(pmNoSystemRabReleasePacketUra) )  - sum(as.numeric(pmChSwitchAttemptFachUra) -as.numeric( pmChSwitchSuccFachUra) ) - sum(as.numeric(pmChSwitchAttemptDchUra) - as.numeric(pmChSwitchSuccDchUra) ) - sum(as.numeric(pmChSwitchAttemptHsUra) - as.numeric(pmChSwitchSuccHsUra) )),
      
      RET_PS_DEN_E = as.numeric(sum(as.numeric(PmNoNormalRAB) ) - sum(as.numeric(pmNoNormalRabReleasePacketUra) ) +  sum(as.numeric(PmSysRAB) ) + sum(as.numeric(pmNoSystemRabReleasePacketUra) ) 
                                + sum(as.numeric(pmChSwitchSuccFachUra) ) + sum(as.numeric(pmChSwitchSuccDchUra) )  + sum(as.numeric(pmChSwitchSuccHsUra) ))
    )
  
  
  datosNok <- dataN %>% group_by(DIA)%>%
    summarise(
      ACC_CS_NUM_N = as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) * as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),
      
      ACC_CS_DEN_N = as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)),
      
      ACC_PS_NUM_N = as.numeric(as.numeric(sum(NUM_ACC_PS))* as.numeric(sum(MULT_NUM1))),
      
      ACC_PS_DEN_N = as.numeric(as.numeric(sum(DEN_ACC_PS))* as.numeric(sum(MULT_NUM2))),
      
      RET_CS_NUM_N = as.numeric(sum(NUM_RET_CS)),
      
      RET_CS_DEN_N = as.numeric(sum(DEN_RET_CS)),
      
      RET_PS_NUM_N = as.numeric(sum(NUM_RET_PS)) ,
      
      RET_PS_DEN_N = as.numeric(sum(DEN_RET_PS))
    )
  
  datosRed <- datosHua %>% inner_join(datosEri)%>% inner_join(datosNok)%>%group_by(DIA)
  
  return(list("datosHua"= datosHua, "datosEri"=datosEri, "datosRed"= datosRed ,"datosNok"= datosNok))
}

data2GTableroOK <-function(data,value,valueF){
  dat <- data
  
  data$RED <- "Total"
  
  if(valueF == "F"){
    if(value == "T"){
      datos <- data %>% group_by(DIA)%>%
        summarise(
          
          ACC = round(100* as.numeric((sum(ACC_NUM_H)+sum(ACC_NUM_E) + sum(ACC_NUM_N))/(sum(ACC_DEN_H)+sum(ACC_DEN_E) + sum(ACC_DEN_N))),2),
          
          RET = round(100 * as.numeric((sum(RET_NUM_H)+sum(RET_NUM_E) + sum(RET_NUM_N))/(sum(RET_DEN_H)+sum(RET_DEN_E) + sum(RET_DEN_N) )),2),
          
          SER = round(100* as.numeric(((sum(ACC_NUM_H)+sum(ACC_NUM_E)+sum(ACC_NUM_N))/(sum(ACC_DEN_H)+sum(ACC_DEN_E)+sum(ACC_DEN_N)))* ((sum(RET_NUM_H)+sum(RET_NUM_E)+sum(RET_NUM_N))/(sum(RET_DEN_H)+sum(RET_DEN_E)+sum(RET_DEN_N)))),2)
          
          
        )  }
    if(value == "H"){
      datos <- data %>% group_by(DIA)%>%
        summarise(
          ACC = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))),2),
          
          RET = round(100 *as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
          
          SER = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))) * as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2)
        )  
    }
    
    if(value == "E"){
      datos <- data %>% group_by(DIA)%>%
        summarise(
          
          ACC = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))),2),
          
          RET = round(100 *as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))),2),
          
          SER = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))) * as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))) ,2)
        )  
    }
    
    if(value == "N"){
      datos <- data %>% group_by(DIA)%>%
        summarise(
          
          ACC = round(100 - 100* as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N))),2),
          
          RET = round(100 *as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N))),2),
          
          SER = round(  (100 - 100*as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N)))) * (100*as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N)))) /100 ,2)
        )  
    }
    
    return(datos) 
    
  }
  else{
    if(value == "T"){
      datos <- data %>% group_by(RED)%>%
        summarise(
          
          ACC = round(100* as.numeric((sum(ACC_NUM_H)+sum(ACC_NUM_E) + sum(ACC_NUM_N))/(sum(ACC_DEN_H)+sum(ACC_DEN_E) + sum(ACC_DEN_N))),2),
          
          RET = round(100 * as.numeric((sum(RET_NUM_H)+sum(RET_NUM_E) + sum(RET_NUM_N))/(sum(RET_DEN_H)+sum(RET_DEN_E) + sum(RET_DEN_N) )),2),
          
          SER = round(100* as.numeric(((sum(ACC_NUM_H)+sum(ACC_NUM_E)+sum(ACC_NUM_N))/(sum(ACC_DEN_H)+sum(ACC_DEN_E)+sum(ACC_DEN_N)))* ((sum(RET_NUM_H)+sum(RET_NUM_E)+sum(RET_NUM_N))/(sum(RET_DEN_H)+sum(RET_DEN_E)+sum(RET_DEN_N)))),2)
          
        )  }
    if(value == "H"){
      datos <- data %>% group_by(RED)%>%
        summarise(
          ACC = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))),2),
          
          RET = round(100 *as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
          
          SER = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))) * as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2)
        )  
    }
    
    if(value == "E"){
      datos <- data %>% group_by(RED)%>%
        summarise(
          
          ACC = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))),2),
          
          RET = round(100 *as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))),2),
          
          SER = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))) * as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))) ,2)
        )  
    }
    
    if(value == "N"){
      datos <- data %>% group_by(RED)%>%
        summarise(
          
          ACC = round(100 - 100* as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N))),2),
          
          RET = round(100 *as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N))),2),
          
          SER = round(  (100 - 100*as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N)))) * ( 100*as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N)))) /100 ,2)
        )  
    }
    
    return(datos) 
  }

}

data3GTableroOK <-function(data,value,valueF){
  data$RED <- "Total"
  
  if(valueF == "F"){
    if(value == "T"){
      datos <- data %>% group_by(DIA)%>%
        summarise(
          ACC_CS = round(100* as.numeric((sum(ACC_CS_NUM_H)+sum(ACC_CS_NUM_E)+sum(ACC_CS_NUM_N))/(sum(ACC_CS_DEN_H)+sum(ACC_CS_DEN_E)+sum(ACC_CS_DEN_N))),2),
          
          ACC_PS = round(100* as.numeric((sum(ACC_PS_NUM_H)+sum(ACC_PS_NUM_E)+sum(ACC_PS_NUM_N))/(sum(ACC_PS_DEN_H)+sum(ACC_PS_DEN_E)+sum(ACC_PS_DEN_N))),2),
          
          RET_CS = 100 - round(100* as.numeric((sum(RET_CS_NUM_H) + sum(RET_CS_NUM_E) + sum(RET_CS_NUM_N))/(sum(RET_CS_DEN_H)+sum(RET_CS_DEN_E) + +sum(RET_CS_DEN_N))),2),
          
          RET_PS = 100 - round(100* as.numeric((sum(RET_PS_NUM_H) + sum(RET_PS_NUM_E) + sum(RET_PS_NUM_N))/(sum(RET_PS_DEN_H)+sum(RET_PS_DEN_E) + +sum(RET_PS_DEN_N))),2)
          
        )  }
    if(value == "H"){
      datos <- data %>% group_by(DIA)%>%
        summarise(
          ACC_CS = round(100* as.numeric((sum(ACC_CS_NUM_H))/(sum(ACC_CS_DEN_H))),2),
          
          ACC_PS = round(100* as.numeric((sum(ACC_PS_NUM_H))/(sum(ACC_PS_DEN_H))),2),
          
          RET_CS = 100-round(100* as.numeric((sum(RET_CS_NUM_H))/(sum(RET_CS_DEN_H))),2),
          
          RET_PS = 100-round(100* as.numeric((sum(RET_PS_NUM_H))/(sum(RET_PS_DEN_H))),2)
        )  
    }
    
    if(value == "E"){
      datos <- data %>% group_by(DIA)%>%
        summarise(
          
          ACC_CS = round(100* as.numeric((sum(ACC_CS_NUM_E))/(sum(ACC_CS_DEN_E))),2),
            
          ACC_PS = round(100* as.numeric((sum(ACC_PS_NUM_E))/(sum(ACC_PS_DEN_E))),2),
            
          RET_CS = 100-round(100* as.numeric((sum(RET_CS_NUM_E))/(sum(RET_CS_DEN_E))),2),
            
          RET_PS = 100-round(100* as.numeric((sum(RET_PS_NUM_E))/(sum(RET_PS_DEN_E))),2)
        
        )  
    }
    if(value == "N"){
      datos <- data %>% group_by(DIA)%>%
        summarise(
          ACC_CS = round(100* as.numeric((sum(ACC_CS_NUM_N))/(sum(ACC_CS_DEN_N))),2),
          
          ACC_PS = round(100* as.numeric((sum(ACC_PS_NUM_N))/(sum(ACC_PS_DEN_N))),2),
          
          RET_CS = 100-round(100* as.numeric((sum(RET_CS_NUM_N))/(sum(RET_CS_DEN_N))),2),
          
          RET_PS = 100-round(100* as.numeric((sum(RET_PS_NUM_N))/(sum(RET_PS_DEN_N))),2)
        )  
    }
    
    return(datos) 
    
  }
  else{
    if(value == "T"){
      datos <- data %>% group_by(RED)%>%
        summarise(
          ACC_CS = round(100* as.numeric((sum(ACC_CS_NUM_H)+sum(ACC_CS_NUM_E)+sum(ACC_CS_NUM_N))/(sum(ACC_CS_DEN_H)+sum(ACC_CS_DEN_E)+sum(ACC_CS_DEN_N))),2),
          
          ACC_PS = round(100* as.numeric((sum(ACC_PS_NUM_H)+sum(ACC_PS_NUM_E)+sum(ACC_PS_NUM_N))/(sum(ACC_PS_DEN_H)+sum(ACC_PS_DEN_E)+sum(ACC_PS_DEN_N))),2),
          
          RET_CS = 100 - round(100* as.numeric((sum(RET_CS_NUM_H) + sum(RET_CS_NUM_E) + sum(RET_CS_NUM_N))/(sum(RET_CS_DEN_H)+sum(RET_CS_DEN_E) +sum(RET_CS_DEN_N))),2),
          
          RET_PS = 100 - round(100* as.numeric((sum(RET_PS_NUM_H) + sum(RET_PS_NUM_E) + sum(RET_PS_NUM_N))/(sum(RET_PS_DEN_H)+sum(RET_PS_DEN_E) +sum(RET_PS_DEN_N))),2)
          
                  )  }
    if(value == "H"){
      datos <- data %>% group_by(RED)%>%
        summarise(
          ACC_CS = round(100* as.numeric((sum(ACC_CS_NUM_H))/(sum(ACC_CS_DEN_H))),2),
          
          ACC_PS = round(100* as.numeric((sum(ACC_PS_NUM_H))/(sum(ACC_PS_DEN_H))),2),
          
          RET_CS = 100-round(100* as.numeric((sum(RET_CS_NUM_H))/(sum(RET_CS_DEN_H))),2),
          
          RET_PS = 100-round(100* as.numeric((sum(RET_PS_NUM_H))/(sum(RET_PS_DEN_H))),2)
        )  
    }
    
    if(value == "E"){
      datos <- data %>% group_by(RED)%>%
        summarise(
          
          ACC_CS = round(100* as.numeric((sum(ACC_CS_NUM_E))/(sum(ACC_CS_DEN_E))),2),
          
          ACC_PS = round(100* as.numeric((sum(ACC_PS_NUM_E))/(sum(ACC_PS_DEN_E))),2),
          
          RET_CS = 100 - round(100* as.numeric((sum(RET_CS_NUM_E))/(sum(RET_CS_DEN_E))),2),
          
          RET_PS = 100 - round(100* as.numeric((sum(RET_PS_NUM_E))/(sum(RET_PS_DEN_E))),2)
        )  
    }
    
    if(value == "N"){
      datos <- data %>% group_by(RED)%>%
        summarise(
          
          ACC_CS = round(100* as.numeric((sum(ACC_CS_NUM_N))/(sum(ACC_CS_DEN_N))),2),
          
          ACC_PS = round(100* as.numeric((sum(ACC_PS_NUM_N))/(sum(ACC_PS_DEN_N))),2),
          
          RET_CS = 100-round(100* as.numeric((sum(RET_CS_NUM_N))/(sum(RET_CS_DEN_N))),2),
          
          RET_PS = 100-round(100* as.numeric((sum(RET_PS_NUM_N))/(sum(RET_PS_DEN_N))),2)
          
        )  
    }
    
    return(datos) 
  }
  
}


renderGraf <- function(dataP,index,granul){
  g <-granul
  
  if(granul == "yyyy-mm-dd"){
  tformat <- "%Y-%m-%d"
  }
   
  if(granul == "yyyy-mm-dd hh"){
  tformat <- "%Y-%m-%d %H"
  }
  
  cell.vcl <- levels(factor(unlist(dataP[,2]), ordered = TRUE))
  #              mydate <- strptime(levels(factor(unlist(datos[,1]$Date_h), ordered = TRUE)),format = "%Y-%m-%d %H")
  mydate <- strptime(levels(factor(unlist(dataP[,1]$DIA), ordered = TRUE)),format = tformat,tz="EST")
  mydata <- data.frame(matrix(0, nrow = length(mydate), ncol = 0))
  for (i in 1:length(cell.vcl)) {
    x <- as.numeric(dim(dataP[str_detect(unlist(dataP[,2]),cell.vcl[i]),index])[1])
    y <- as.numeric(length(mydate))
    
    data <- dataP
    if (dim(dataP[str_detect(unlist(dataP[,2]),cell.vcl[i]),index])[1] == length(mydate))
    {mydata <- cbind(mydata,dataP[str_detect(unlist(dataP[,2]),cell.vcl[i]),index])} 
    else
    {
      tempdata <- data.frame(matrix(0, nrow = length(mydate), ncol = 1))
      mydata <- cbind(mydata,tempdata)
    }
    colnames(mydata)[i] <- cell.vcl[i]
  }
  
  return(list("mydata"= mydata, "mydate"=mydate))
}

rendermygraphOK <- function (mylist,value,granul){
  
  
  
  if(value == "P"){
    return(renderDygraph({
      d1 <- dygraph(as.xts(mylist$mydata,order.by = mylist$mydate, tz="GMT"))%>%
        dyLegend(show = "always", hideOnMouseOut = TRUE, width = 400) %>%
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2 ,hideOnMouseOut = FALSE) %>%
        dyRangeSelector() 
        

      d1$x$css = "
                  .dygraph-legend > span.highlight { display: inline; }
                  .dygraph-legend > span.highlight { border: 1px solid grey; }
            
                             "
      
      d1
    }))
  }
  if(value == "T"){
    return(renderDygraph({
      
      g <- granul
      
      if(granul == "yyyy-mm-dd"){
        tformat <- "%Y-%m-%d"
      }
      
      if(granul == "yyyy-mm-dd hh"){
        tformat <- "%Y-%m-%d %H"
      }
      
      day <- strptime(levels(factor(unlist(mylist[,1]$DIA), ordered = TRUE)),format = tformat)
      #day<- as.Date(day$DIA)
      mylist <- mylist[,-1]
      
      d1 <- dygraph(as.xts(mylist,order.by = day,tz="GMT"))%>%
        dyLegend(show = "always", hideOnMouseOut = TRUE, width = 400) %>%
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2 ,hideOnMouseOut = FALSE) %>%
        dyRangeSelector()
      
      d1$x$css = "
                  .dygraph-legend > span {display:none;}
      .dygraph-legend > span.highlight { display: inline; }
      .dygraph-legend > span.highlight { border: 1px solid grey; }
      
      "
      
      d1
    }))
  }
  
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #datos.2GEri <- data.frame()
  datos.2GHua <- data.frame()
  datos.3GHua <- data.frame()
  datos.Eri <- data.frame()
  datos.2GNok <- data.frame()
  datos.3GNok <- data.frame()
  datoSigestop <- data.frame()
  
  observeEvent(input$query, {
    ###Eventos################
    datoSigestop <<- sigestop()
    
    withProgress(message ='Ejecutando Consulta: Huawei',value = 0.5,{
      #Event Huawei 2G
      #datos.2GHua <<- queryData2GHUAWEI(as.character(input$idate),format(Sys.Date(), format="%d-%m-%Y"),datoSigestop)
      datos.2GHua <<- queryData2GHUAWEI(as.character(input$idate),as.character(input$fdate),datoSigestop,input$gra)
      datos.2GHuawei_PROVINCIA <- queryData2GHUAWEIP(datos.2GHua,as.character(input$hour))
      datos.2GHuaweiAll <- queryData2GHUAWEIAll(datos.2GHua,datoSigestop,as.character(input$hour))
      
      
      #Event Huawei 3G
      #datos.3GEri <<- queryData3GERICSSON(as.character(input$idate),format(Sys.Date(), format="%d-%m-%Y"),datoSigestop)
      datos.3GHua <<- queryData3GHuawei(as.character(input$idate),as.character(input$fdate),datoSigestop,input$gra)
      datos.3GHuawei_PROVINCIA <- queryData3GHUAWEIP(datos.3GHua,as.character(input$hour))
      datos.3GHuawei_All <- queryData3GHuaweiAll(datos.3GHua,datoSigestop,as.character(input$hour))
    })
    
    withProgress(message ='Ejecutando Consulta: Ericsson',value = 0.5,{
      
      
      datos.Eri <<- queryERICSSON(as.character(input$idate),as.character(input$fdate),datoSigestop,input$gra)
      #Event Ericsson 4G
      datos.4GEricsson_PROVINCIA <- queryData4GEricssonP(datos.Eri$ericsson4G,as.character(input$hour),input$gra)
      datos.4GEricsson_All <- queryData4GEricssonAll(datos.Eri$ericsson4G,datoSigestop,as.character(input$hour),input$gra)
      #Event Ericsson 3G
      datos.3GEricsson_PROVINCIA <- queryData3GEricssonP(datos.Eri$ericsson3G,as.character(input$hour),input$gra)
      datos.3GEricsson_All <- queryData3GEricssonAll(datos.Eri$ericsson3G,datoSigestop,as.character(input$hour),input$gra)
      #Event Ericsson 2G
      datos.2GEricsson_PROVINCIA <- queryData2GEricssonP(datos.Eri$ericsson2G,as.character(input$hour))
      datos.2GEricsson_All <- queryData2GEricssonAll(datos.Eri$ericsson2G,datoSigestop,as.character(input$hour),input$gra)
    })
    
    withProgress(message ='Ejecutando Consulta: Nokia',value = 0.5,{
      #Event NOKIA 3G
      datos.3GNok <<- queryData3GNokia(as.character(input$idate),as.character(input$fdate),datoSigestop,input$gra)
      datos.3GNokia_PROVINCIA <- queryData3GnokiaP(datos.3GNok,as.character(input$hour))
      datos.3GNokiaAll <- queryData3GnokiaALL(datos.3GNok,datoSigestop,as.character(input$hour))
      
      #Event NOKIA 2G
      datos.2GNok <<- queryData2GNokia(as.character(input$idate),as.character(input$fdate),datoSigestop,input$gra)
      datos.2GNokia_PROVINCIA <- queryData2GnokiaP(datos.2GNok,as.character(input$hour),input$gra)
      datos.2GNokiaAll <- queryData2GnokiaALL(datos.2GNok,datoSigestop,as.character(input$hour),input$gra)
    })

    id <- showNotification("Filtrando datos.",duration =NULL)
    
    ###TABLERO####################
    
    data.tablero2G <- data2GTablero(datos.2GHua,datos.Eri$ericsson2G,datos.2GNok)
    
    data.tablero3G <- data3GTablero(datos.Eri$ericsson3G,datos.3GHua,datos.3GNok)
    
    
    ###2G TABLERO####################
    datos.Tablero2GH <- data2GTableroOK(data.tablero2G$datosHua,"H","F")
    
    datos.Tablero2GE <- data2GTableroOK(data.tablero2G$datosEri,"E","F")
    
    datos.Tablero2GN <- data2GTableroOK(data.tablero2G$datosNok,"N","F")
    
    datos.Tablero2GT <- data2GTableroOK(data.tablero2G$datosRed,"T","F")
    
    datos.Tablero2GHG <- data2GTableroOK(data.tablero2G$datosHua,"H","G")
    
    datos.Tablero2GEG <- data2GTableroOK(data.tablero2G$datosEri,"E","G")
    
    datos.Tablero2GNG <- data2GTableroOK(data.tablero2G$datosNok,"N","G")
    
    datos.Tablero2GTG <- data2GTableroOK(data.tablero2G$datosRed,"T","G")
    
   
    ###3G TABLERO####################
    datos.Tablero3GE <- data3GTableroOK(data.tablero3G$datosEri,"E","F")
    
    datos.Tablero3GH <- data3GTableroOK(data.tablero3G$datosHua,"H","F")
    
    datos.Tablero3GN <- data3GTableroOK(data.tablero3G$datosNok,"N","F")
    
    datos.Tablero3GT <- data3GTableroOK(data.tablero3G$datosRed,"T","F")
    
    datos.Tablero3GEG <- data3GTableroOK(data.tablero3G$datosEri,"E","G")
    
    datos.Tablero3GHG <- data3GTableroOK(data.tablero3G$datosHua,"H","G")
    
    datos.Tablero3GNG <- data3GTableroOK(data.tablero3G$datosNok,"N","G")
    
    datos.Tablero3GTG <- data3GTableroOK(data.tablero3G$datosRed,"T","G")
    
    ###Tablero RENDER 2G####################
    output$tablero2GH <- renderDataTable(datatable(datos.Tablero2GH, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero2GE <- renderDataTable(datatable(datos.Tablero2GE, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero2GN <- renderDataTable(datatable(datos.Tablero2GN, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero2GRed <- renderDataTable(datatable(datos.Tablero2GT, options = list(pageLength = 30, scrollX = TRUE)))
    
    output$tablero2GraphH <- rendermygraphOK(datos.Tablero2GH,"T",input$gra)
    output$tablero2GraphE <- rendermygraphOK(datos.Tablero2GE,"T",input$gra)
    output$tablero2GraphN <- rendermygraphOK(datos.Tablero2GN,"T",input$gra)
    output$tablero2GraphRed <- rendermygraphOK(datos.Tablero2GT,"T",input$gra)
    
    output$tablero2GraphHTotal <- renderDataTable(datatable(datos.Tablero2GHG, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero2GraphETotal <- renderDataTable(datatable(datos.Tablero2GEG, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero2GraphNTotal <- renderDataTable(datatable(datos.Tablero2GNG, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero2GraphRedTotal <- renderDataTable(datatable(datos.Tablero2GTG, options = list(pageLength = 30, scrollX = TRUE)))
    
    ###Tablero RENDER 3G####################
    output$tablero3GE <- renderDataTable(datatable(datos.Tablero3GE, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero3GH <- renderDataTable(datatable(datos.Tablero3GH, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero3GN <- renderDataTable(datatable(datos.Tablero3GN, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero3GRed <- renderDataTable(datatable(datos.Tablero3GT, options = list(pageLength = 30, scrollX = TRUE)))
    
    
    output$tablero3GraphHTotal <- renderDataTable(datatable(datos.Tablero3GHG, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero3GraphETotal <- renderDataTable(datatable(datos.Tablero3GEG, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero3GraphNTotal <- renderDataTable(datatable(datos.Tablero3GNG, options = list(pageLength = 30, scrollX = TRUE)))
    output$tablero3GraphRedTotal <- renderDataTable(datatable(datos.Tablero3GTG, options = list(pageLength = 30, scrollX = TRUE)))
    
    output$tablero3GraphE <- rendermygraphOK(datos.Tablero3GE,"T",input$gra)
    output$tablero3GraphH <- rendermygraphOK(datos.Tablero3GH,"T",input$gra)
    output$tablero3GraphN <- rendermygraphOK(datos.Tablero3GN,"T",input$gra)
    output$tablero3GraphRed <- rendermygraphOK(datos.Tablero3GT,"T",input$gra)                                         
    
    ###NOKIA 2G####################
    output$Nokia2GKPI_PROVINCIA <- renderDataTable(datatable(datos.2GNokia_PROVINCIA, options = list(10)))
    output$Nokia2G_ACC_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC")]%>% arrange(ACC), options = list(pageLength = 10)))
    output$Nokia2G_RET_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","RET")]%>% arrange(RET), options = list(pageLength = 10)))
    output$Nokia2G_SER_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","SER")]%>% arrange(SER), options = list(pageLength = 10)))
    output$Nokia2G_CONG_SDCCH_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","CONG_SDCCH")] %>% arrange(desc(CONG_SDCCH)), options = list(10)))
    output$Nokia2G_CONG_TCH_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","CONG_TCH")] %>% arrange(desc(CONG_TCH)), options = list(10)))
    output$Nokia2G_TCH_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","TRAFF_TCH","TRAFF_TCH_FR","TRAFF_TCH_HR")]%>% arrange(desc(TRAFF_TCH)), options = list(pageLength = 10)))
    output$Nokia2G_SDCCH_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","TRAFF_SDCCH")]%>% arrange(desc(TRAFF_SDCCH)), options = list(pageLength = 10)))
    output$Nokia2G_Pagings <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","PAGINGS")] %>% arrange(desc(PAGINGS)), options = list(10)))
    output$Nokia2G_Drop <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","DROP_TCH","DROP_SDCCH")]%>% arrange(desc(DROP_TCH)), options = list(pageLength = 10)))
    output$Nokia2G_ICM_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","INT4_5P")] %>% arrange(desc(INT4_5P)), options = list(10)))
   
    ###NOKIA 3G#####################
    output$Nokia3GKPI_PROVINCIA <- renderDataTable(datatable(datos.3GNokia_PROVINCIA, options = list(10)))
    output$Nokia_ACC_CS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC_CS")] %>% arrange(ACC_CS), options = list(10)))
    output$Nokia_ACC_PS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC_PS")] %>% arrange(ACC_PS), options = list(10)))
    output$Nokia_RET_CS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","RET_CS")] %>% arrange(RET_CS), options = list(10)))
    output$Nokia_RET_PS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","RET_PS")] %>% arrange(RET_PS), options = list(10)))
    output$Nokia_CODE_RRC <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","COD_RRC")] %>% arrange(desc(COD_RRC) ), options = list(10)))
    output$Nokia_CODE_RAB <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","COD_RAB")] %>% arrange(desc(COD_RAB)), options = list(10)))
    output$Nokia_CE_RRC <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","CE_RRC")] %>% arrange(desc(CE_RRC)), options = list(10)))
    output$Nokia_CE_RAB <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","CE_RAB")] %>% arrange(desc(CE_RAB)), options = list(10)))
    output$Nokia_PWR_RRC <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","PWR_RRC")] %>% arrange(desc(PWR_RRC)), options = list(10)))
    output$Nokia_PWR_RAB <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","PWR_RAB")] %>% arrange(desc(PWR_RAB)), options = list(10)))
    output$Nokia_USER_HSDPA <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","USER_HSDPA")] %>% arrange(desc(USER_HSDPA)), options = list(10)))
    output$Nokia_DROP_CS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","DROP_CS","DROP_PS")] %>% arrange(desc(DROP_CS)), options = list(10)))
    output$Nokia_THROUGHPUT_HSDPA <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","TROUGH_HSDPA")] %>% arrange(TROUGH_HSDPA), options = list(10)))
    output$Nokia_DISPONIBILIDAD <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","AVAIL")] %>% arrange(AVAIL), options = list(10)))
    output$Nokia_RTWP <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","RTWP")] %>% arrange(RTWP), options = list(10)))
    
    ###HUAWEI 2G####################
    output$Huawei2Gtable <- renderDataTable(datatable(datos.2GHUawei, options = list(pageLength = 10, scrollX = TRUE)))
    output$Huawei2GKPI_PROVINCIA <- renderDataTable(datatable(datos.2GHuawei_PROVINCIA, options = list(10)))
    output$Huawei2G_ACC_Celdas <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","ACC")]%>% arrange(ACC), options = list(pageLength = 10)))
    output$Huawei2G_RET_Celdas <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","RET")]%>% arrange(RET), options = list(pageLength = 10)))
    output$Huawei2G_SER_Celdas <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","SER")]%>% arrange(SER), options = list(pageLength = 10)))
    output$Huawei2G_CONG_TCH_Celdas <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","CONG_TCH")]%>% arrange(desc(CONG_TCH)), options = list(pageLength = 10)))
    output$Huawei2G_CONG_SDCCH_Celdas <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","CONG_SDCCH")]%>% arrange(desc(CONG_SDCCH)), options = list(pageLength = 10)))
    output$Huawei2G_TCH_Celdas <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","TRAFF_TCH")]%>% arrange(desc(TRAFF_TCH)), options = list(pageLength = 10)))
    output$Huawei2G_SDCCH_Celdas <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","TRAFF_SDCCH")]%>% arrange(desc(TRAFF_SDCCH)), options = list(pageLength = 10)))
    output$Huawei2G_Pagings <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","PAGINGS")]%>% arrange(desc(PAGINGS)), options = list(pageLength = 10)))
    output$Huawei2G_Drop <- renderDataTable(datatable(datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","CALL_DROP_TCH","CALL_DROP_SDCCH")]%>% arrange(desc(CALL_DROP_TCH)), options = list(pageLength = 10)))
    
    #Graficos
    output$ACC_PROV_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,3,input$gra),"P",input$gra)
    output$RET_PROV_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,4,input$gra),"P",input$gra)
    output$SER_PROV_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,5,input$gra),"P",input$gra)
    output$CONG_SDCCH_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,6,input$gra),"P",input$gra)
    output$CONG_TCH_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,7,input$gra),"P",input$gra)
    output$TCH_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,8,input$gra),"P",input$gra)
    output$SDCCH_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,9,input$gra),"P",input$gra)
    output$DROP_TCH_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,10,input$gra),"P",input$gra)
    output$DROP_SDCCH_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,11,input$gra),"P",input$gra)
    output$PAGINGS_H <- rendermygraphOK(renderGraf(datos.2GHuawei_PROVINCIA,12,input$gra),"P",input$gra)
    
    ###HUAWEI 3G####################
    output$Huawei3GKPI_PROVINCIA <- renderDataTable(datatable(datos.3GHuawei_PROVINCIA, options = list(10)))
    output$Huawei_ACC_CS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC_CS")] %>% arrange(ACC_CS), options = list(10)))
    output$Huawei_ACC_PS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC_PS")] %>% arrange(ACC_PS), options = list(10)))
    output$Huawei_RET_CS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","RET_CS")] %>% arrange(RET_CS), options = list(10)))
    output$Huawei_RET_PS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","RET_PS")] %>% arrange(RET_PS), options = list(10)))
    output$Huawei_CE_RRC <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_CE_RRC")] %>% arrange(desc(CONG_CE_RRC)), options = list(10)))
    output$Huawei_PWR_RRC <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_PWR_RRC")] %>% arrange(desc(CONG_PWR_RRC)), options = list(10)))
    output$Huawei_CODE_RRC <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_CODE_RRC")] %>% arrange(desc(CONG_CODE_RRC)), options = list(10)))
    output$Huawei_CE_RAB <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_CE_RAB")] %>% arrange(desc(CONG_CE_RAB)), options = list(10)))
    output$Huawei_PWR_RAB <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_PWR_RAB")] %>% arrange(desc(CONG_PWR_RAB)), options = list(10)))
    output$Huawei_CODE_RAB <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_CODE_RAB")] %>% arrange(desc(CONG_CODE_RAB)), options = list(10)))
    output$Huawei_THROUGHPUT_HSDPA <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","MAX_TROUGHPUT","AVG_TROUGHPUT")] %>% arrange(desc(MAX_TROUGHPUT)), options = list(10)))
    output$Huawei_USER_HSDPA <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","MAX_USER_HSDPA")] %>% arrange(desc(MAX_USER_HSDPA)), options = list(10)))
    output$Huawei_DROP_CS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","DROP_CS","DROP_PS")] %>% arrange(desc(DROP_CS)), options = list(10)))
    output$Huawei_RTWP <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","RTWP")] %>% arrange(desc(RTWP)), options = list(10)))
    output$Huawei_PAGING <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","PAGING")] %>% arrange(desc(PAGING)), options = list(10)))
    
    #Graficos
    output$ACC_CS_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,3,input$gra),"P",input$gra)
    output$ACC_PS_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,4,input$gra),"P",input$gra)
    output$RET_CS_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,5,input$gra),"P",input$gra)
    output$RET_PS_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,6,input$gra),"P",input$gra)
    
    output$CONG_CE_RRC_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,7,input$gra),"P",input$gra)
    output$CONG_PWR_RRC_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,8,input$gra),"P",input$gra)
    output$CONG_CODE_RRC_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,9,input$gra),"P",input$gra)
    output$CONG_CE_RAB_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,10,input$gra),"P",input$gra)
    output$CONG_PWR_RAB_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,11,input$gra),"P",input$gra)
    output$CONG_CODE_RAB_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,12,input$gra),"P",input$gra)
    output$TRAFF_CS_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,13,input$gra),"P",input$gra)
    output$VOL_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,14,input$gra),"P",input$gra)
    output$DROP_CS_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,15,input$gra),"P",input$gra)
    output$DROP_PS_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,16,input$gra),"P",input$gra)
    output$RTWP_PROV_H <- rendermygraphOK(renderGraf(datos.3GHuawei_PROVINCIA,17,input$gra),"P",input$gra)
   
    ###Ericsson 2G################
    output$Ericsson2GKPI_PROVINCIA <- renderDataTable(datatable(datos.2GEricsson_PROVINCIA, options = list(10)))
    output$Ericsson2G_ACC_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC")] %>% arrange(ACC), options = list(10)))
    output$Ericsson2G_RET_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","RET")] %>% arrange(RET), options = list(10)))
    output$Ericsson2G_SER_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","SER")] %>% arrange(SER), options = list(10)))
    output$Ericsson2G_CONG_SDCCH_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_SDCCH")] %>% arrange(desc(CONG_SDCCH)), options = list(10)))
    output$Ericsson2G_CONG_TCH_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_TCH")] %>% arrange(desc(CONG_TCH)), options = list(10)))
    output$Ericsson2G_TCH_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","TRAFF_TCH")]%>% arrange(desc(TRAFF_TCH)), options = list(pageLength = 10)))
    output$Ericsson2G_SDCCH_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","TRAFF_SDCCH")]%>% arrange(desc(TRAFF_SDCCH)), options = list(pageLength = 10)))
    output$Ericsson2G_Paging_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","PAGINGS")] %>% arrange(desc(PAGINGS)), options = list(10)))
    output$Ericsson2G_Drop <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","CALL_DROP_TCH","CALL_DROP_SDCCH")]%>% arrange(desc(CALL_DROP_TCH)), options = list(pageLength = 10)))
    output$Ericsson2G_ICM_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","ICMBAND")] %>% arrange(desc(ICMBAND)), options = list(10)))
    

    #Graficos
    output$ACC_PROV_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,3,input$gra),"P",input$gra)
    output$RET_PROV_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,4,input$gra),"P",input$gra)
    output$SER_PROV_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,5,input$gra),"P",input$gra)
    output$CONG_SDCCH_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,6,input$gra),"P",input$gra)
    output$CONG_TCH_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,7,input$gra),"P",input$gra)
    output$TCH_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,8,input$gra),"P",input$gra)
    output$SDCCH_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,9,input$gra),"P",input$gra)
    output$DROP_TCH_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,10,input$gra),"P",input$gra)
    output$DROP_SDCCH_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,11,input$gra),"P",input$gra)
    output$PAGINGS_E <- rendermygraphOK(renderGraf(datos.2GEricsson_PROVINCIA,12,input$gra),"P",input$gra)
  
    ###Ericsson 3G##############
    output$Ericsson3GKPI_PROVINCIA <- renderDataTable(datatable(datos.3GEricsson_PROVINCIA, options = list(10)))
    output$Ericsson_ACC_CS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC_CS")] %>% arrange(ACC_CS), options = list(10)))
    output$Ericsson_ACC_PS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC_PS")] %>% arrange(ACC_PS), options = list(10)))
    output$Ericsson_RET_CS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","RET_CS")] %>% arrange(RET_CS), options = list(10)))
    output$Ericsson_RET_PS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","RET_PS")] %>% arrange(RET_PS), options = list(10)))
    output$Ericsson_CE_RRC <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_CE_RRC")] %>% arrange(desc(CONG_CE_RRC)), options = list(10)))
    output$Ericsson_PWR_RRC <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_PWR_RRC")] %>% arrange(desc(CONG_PWR_RRC)), options = list(10)))
    output$Ericsson_CODE_RRC <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_CODE_RRC")] %>% arrange(desc(CONG_CODE_RRC)), options = list(10)))
    output$Ericsson_CE_RAB <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_CE_RAB")] %>% arrange(desc(CONG_CE_RAB)), options = list(10)))
    output$Ericsson_PWR_RAB <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_PWR_RAB")] %>% arrange(desc(CONG_PWR_RAB)), options = list(10)))
    output$Ericsson_CODE_RAB <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_CODE_RAB")] %>% arrange(desc(CONG_CODE_RAB)), options = list(10)))
    output$Ericsson_RSSI <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","RSSI")] %>% arrange(desc(RSSI)), options = list(10)))
    output$Ericsson_Paging <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","PAGINGS")] %>% arrange(desc(PAGINGS)), options = list(10)))
    output$Ericsson_DROP_CS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","DROP_CS","DROP_PS")] %>% arrange(desc(DROP_CS)), options = list(10)))
    output$Ericsson_AVAIL <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","DISPONIBILIDAD")] %>% arrange(DISPONIBILIDAD), options = list(10)))
    ##Graficos
    
    output$ACC_CS_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,3,input$gra),"P",input$gra)
    output$ACC_PS_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,4,input$gra),"P",input$gra)
    output$RET_CS_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,5,input$gra),"P",input$gra)
    output$RET_PS_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,6,input$gra),"P",input$gra)
    
    output$CONG_CE_RRC_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,7,input$gra),"P",input$gra)
    output$CONG_PWR_RRC_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,8,input$gra),"P",input$gra)
    output$CONG_CODE_RRC_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,9,input$gra),"P",input$gra)
    output$CONG_CE_RAB_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,10,input$gra),"P",input$gra)
    output$CONG_PWR_RAB_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,11,input$gra),"P",input$gra)
    output$CONG_CODE_RAB_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,12,input$gra),"P",input$gra)
    output$TRAFF_CS_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,13,input$gra),"P",input$gra)
    output$VOL_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,14,input$gra),"P",input$gra)
    output$DROP_CS_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,15,input$gra),"P",input$gra)
    output$DROP_PS_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,16,input$gra),"P",input$gra)
    output$RSSI_PROV_E <- rendermygraphOK(renderGraf(datos.3GEricsson_PROVINCIA,18,input$gra),"P",input$gra)
    ###Ericsson 4G##############
    output$Ericsson4GKPI_PROVINCIA <- renderDataTable(datatable(datos.4GEricsson_PROVINCIA, options = list(10)))
    output$Ericsson_ACC <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC")] %>% arrange(ACC), options = list(10)))
    output$Ericsson_RET <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","RET")] %>% arrange(RET), options = list(10)))
    output$Ericsson_VOL <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","PAYLOAD_DL")] %>% arrange(desc(PAYLOAD_DL)), options = list(10)))
    output$Ericsson_TRO <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","THRPTDL","THRPTUL")] %>% arrange(desc(THRPTDL) ), options = list(10)))
    output$Ericsson_USER <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","USER_DL","USER_UL","RRC_USER")] %>% arrange(desc(USER_DL) ), options = list(10)))
    output$Ericsson_LAT <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","LAT")] %>% arrange(LAT), options = list(10)))
    output$Ericsson_DROP <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","DROP_ERAB")] %>% arrange(desc(DROP_ERAB) ), options = list(10)))
    ###Exportar XLSX##############
    
    #datosH2G <- datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","CONG_TCH","CONG_SDCCH","PAGINGS","CALL_DROP_TCH","CALL_DROP_SDCCH")] %>% arrange(ACC)
    #datosE2G <- datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","CONG_TCH","CONG_SDCCH","PAGINGS","CALL_DROP_TCH","CALL_DROP_SDCCH","TRAFF_TCH","TRAFF_SDCCH")] %>% arrange(ACC)
    #datosE3G <- datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","DISPONIBILIDAD","RET_PS","PAGINGS","CONG_CE_RRC","CONG_PWR_RRC","CONG_CODE_RRC","CONG_CE_RAB","CONG_PWR_RAB","CONG_CODE_RAB","DROP_CS","DROP_PS","RSSI","TRAFF_CS","VOLUMEN_DATOS")] %>% arrange(ACC_PS)
    #datosH3G <- datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","RET_PS","CONG_CE_RRC","CONG_PWR_RRC","CONG_CODE_RRC","CONG_CE_RAB","CONG_PWR_RAB","CONG_CODE_RAB","MAX_TROUGHPUT","AVG_TROUGHPUT","MAX_USER_HSDPA","DROP_CS","DROP_PS","RTWP","PAGING")] %>% arrange(ACC_PS)
    #datosN2G <- datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","TRAFF_TCH","TRAFF_TCH_FR","TRAFF_TCH_HR","TRAFF_SDCCH","CONG_SDCCH","CONG_TCH","DROP_TCH","DROP_SDCCH","PAGINGS")] %>% arrange(ACC)
    #datosN3G <- datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","RET_PS","AVAIL","TRAFF_CS","VOL","COD_RRC","COD_RAB","CE_RRC","CE_RAB","PWR_RRC","PWR_RAB","DROP_CS","DROP_PS","TROUGH_HSDPA","TROUGH_HSUPA","USER_HSDPA","USER_HSUPA","RTWP")] %>% arrange(ACC_PS)
    #datosE4G <- datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC","RET","PAYLOAD_DL","PAYLOAD_UL","THRPTDL","THRPTUL","USER_MAX_UL","USER_MAX_DL","RRC_USER","LAT" ,"DROP_ERAB")] %>% arrange(ACC)
    
    
    datosH2G <- datos.2GHuaweiAll[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","CONG_TCH","CONG_SDCCH","PAGINGS","PAGING_RATE","CALL_DROP_TCH","CALL_DROP_SDCCH")] %>% arrange(ACC)
    datosE2G <- datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","CONG_TCH","CONG_SDCCH","PAGINGS","PAGING_RATE","CALL_DROP_TCH","CALL_DROP_SDCCH")] %>% arrange(ACC)
    datosE3G <- datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","RET_PS","DISPONIBILIDAD","CONG_CE_RRC","CONG_PWR_RRC","CONG_CODE_RRC","CONG_CE_RAB","CONG_PWR_RAB","CONG_CODE_RAB","DROP_CS","DROP_PS","RSSI","PAGING_RATE")] %>% arrange(ACC_CS)
    datosH3G <- datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","RET_PS","CONG_CE_RRC","CONG_PWR_RRC","CONG_CODE_RRC","CONG_CE_RAB","CONG_PWR_RAB","CONG_CODE_RAB","MAX_TROUGHPUT","AVG_TROUGHPUT","MAX_USER_HSDPA","DROP_CS","DROP_PS","RTWP","PAGING_RATE")] %>% arrange(ACC_CS)
    datosN2G <- datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","TRAFF_TCH","TRAFF_TCH_FR","TRAFF_TCH_HR","TRAFF_SDCCH","CONG_SDCCH","CONG_TCH","DROP_TCH","DROP_SDCCH","PAGINGS","PAGING_RATE")] %>% arrange(ACC)
    datosN3G <- datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","RET_PS","AVAIL","TRAFF_CS","VOL","COD_RRC","COD_RAB","CE_RRC","CE_RAB","PWR_RRC","PWR_RAB","DROP_CS","DROP_PS","TROUGH_HSDPA","TROUGH_HSUPA","USER_HSDPA","USER_HSUPA","PAGING_RATE")] %>% arrange(ACC_CS)
    datosE4G <- datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC","RET","PAYLOAD_DL","PAYLOAD_UL","THRPTDL","THRPTUL","USER_MAX_UL","USER_MAX_DL","RRC_USER","LAT" ,"DROP_ERAB")] %>% arrange(ACC)
    
    output$huaDownload <- downloadHandler(
      filename = function() {
        paste('Huawei Red', Sys.Date(),'.xlsx', sep = '')
      },
      content = function(file) {
        
        write.xlsx2(as.data.frame(datos.2GHuawei_PROVINCIA),file,sheetName="2G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(as.data.frame(datos.3GHuawei_PROVINCIA),file,sheetName="3G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
      }
    )
    
    output$eriDownload <- downloadHandler(
      filename = function() {
        paste('Ericsson Red', Sys.Date(), '.xlsx', sep = '')
      },
      content = function(file) {
        write.xlsx2(as.data.frame(datos.2GEricsson_PROVINCIA),file,sheetName="2G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(as.data.frame(datos.3GEricsson_PROVINCIA),file,sheetName="3G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(as.data.frame(datos.4GEricsson_PROVINCIA),file,sheetName="4G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
      }
    )
  
     output$EstadoRed <- downloadHandler(
       
       filename = function() {
         paste('Red Movil', Sys.Date(),'.xlsx', sep = '')
       },
       
       #[1:1000,]
       #[1:1000,]
       #[1:1000,]
       #[1:1000,]
       #[1:1000,]
       #[1:1000,]
       #[1:1000,]

       content = function(file) {
         
         write.xlsx2(datosE2G,file,sheetName="2G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
         write.xlsx2(datosH2G,file,sheetName="2G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
         write.xlsx2(datosN2G,file,sheetName="2G_Nokia",col.names=TRUE, row.names = TRUE ,append = TRUE)
         write.xlsx2(datosE3G,file,sheetName="3G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
         write.xlsx2(datosH3G,file,sheetName="3G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
         write.xlsx2(datosN3G,file,sheetName="3G_Nokia",col.names=TRUE, row.names = TRUE ,append = TRUE)
         write.xlsx2(datosE4G,file,sheetName="4G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
    
       }
    )
    
     
     
     
     
     
     
    output$tableroWeek <- downloadHandler(
      filename = function() {
        paste('Tablero Semanal', Sys.Date(), '.xlsx', sep = '')
      },
      content = function(file) {
        
        names(datos.Tablero2GEG)[1]<-"DIA"
        names(datos.Tablero2GHG)[1]<-"DIA"
        names(datos.Tablero2GNG)[1]<-"DIA"
        names(datos.Tablero3GEG)[1]<-"DIA"
        names(datos.Tablero3GHG)[1]<-"DIA"
        names(datos.Tablero3GNG)[1]<-"DIA"
        
        
        write.xlsx2(rbind(datos.Tablero2GE,datos.Tablero2GEG), file,sheetName="2G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(rbind(datos.Tablero2GH,datos.Tablero2GHG), file,sheetName="2G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(rbind(datos.Tablero2GN,datos.Tablero2GNG), file,sheetName="2G_Nokia",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(rbind(datos.Tablero3GE,datos.Tablero3GEG), file,sheetName="3G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(rbind(datos.Tablero3GH,datos.Tablero3GHG), file,sheetName="3G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(rbind(datos.Tablero3GN,datos.Tablero3GNG), file,sheetName="3G_Nokia",col.names=TRUE, row.names = TRUE ,append = TRUE)
      }
    )
    
    removeNotification(id)
    showNotification("Hecho.",duration =10)
    
    
  })
    
  observeEvent(input$selectHora, {
    
    id <- showNotification("Actualizando Tabla",duration =NULL) 
    
    ####ERICSSON####
    
    datos.2GEricsson_PROVINCIA <- queryData2GEricssonP(datos.Eri$ericsson2G,as.character(input$hour))
    datos.2GEricsson_All <- queryData2GEricssonAll(datos.Eri$ericsson2G,datoSigestop,as.character(input$hour))
    
    output$Ericsson2GKPI_PROVINCIA <- renderDataTable(datatable(datos.2GEricsson_PROVINCIA, options = list(10)))
    output$Ericsson2G_ACC_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC")] %>% arrange(ACC), options = list(10)))
    output$Ericsson2G_RET_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","RET")] %>% arrange(RET), options = list(10)))
    output$Ericsson2G_SER_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","SER")] %>% arrange(SER), options = list(10)))
    output$Ericsson2G_CONG_SDCCH_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_SDCCH")] %>% arrange(desc(CONG_SDCCH)), options = list(10)))
    output$Ericsson2G_CONG_TCH_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_TCH")] %>% arrange(desc(CONG_TCH)), options = list(10)))
    output$Ericsson2G_Paging_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","PAGINGS")] %>% arrange(desc(PAGINGS)), options = list(10)))
    output$Ericsson2G_ICM_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","ICMBAND")] %>% arrange(desc(ICMBAND)), options = list(10)))
    output$Ericsson2G_TCH_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","TRAFF_TCH")]%>% arrange(desc(TRAFF_TCH)), options = list(pageLength = 10)))
    output$Ericsson2G_SDCCH_Celdas <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","TRAFF_SDCCH")]%>% arrange(desc(TRAFF_SDCCH)), options = list(pageLength = 10)))
    output$Ericsson2G_Drop <- renderDataTable(datatable(datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","CALL_DROP_TCH","CALL_DROP_SDCCH")]%>% arrange(desc(CALL_DROP_TCH)), options = list(pageLength = 10)))

    datos.3GEricsson_PROVINCIA <- queryData3GEricssonP(datos.Eri$ericsson3G,as.character(input$hour))
    datos.3GEricsson_All <- queryData3GEricssonAll(datos.Eri$ericsson3G,datoSigestop,as.character(input$hour))
    
    output$Ericsson3GKPI_PROVINCIA <- renderDataTable(datatable(datos.3GEricsson_PROVINCIA, options = list(10)))
    output$Ericsson_ACC_CS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC_CS")] %>% arrange(ACC_CS), options = list(10)))
    output$Ericsson_ACC_PS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC_PS")] %>% arrange(ACC_PS), options = list(10)))
    output$Ericsson_RET_CS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","RET_CS")] %>% arrange(RET_CS), options = list(10)))
    output$Ericsson_RET_PS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","RET_PS")] %>% arrange(RET_PS), options = list(10)))
    output$Ericsson_CE_RRC <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_CE_RRC")] %>% arrange(desc(CONG_CE_RRC)), options = list(10)))
    output$Ericsson_PWR_RRC <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_PWR_RRC")] %>% arrange(desc(CONG_PWR_RRC)), options = list(10)))
    output$Ericsson_CODE_RRC <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_CODE_RRC")] %>% arrange(desc(CONG_CODE_RRC)), options = list(10)))
    output$Ericsson_CE_RAB <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_CE_RAB")] %>% arrange(desc(CONG_CE_RAB)), options = list(10)))
    output$Ericsson_PWR_RAB <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_PWR_RAB")] %>% arrange(desc(CONG_PWR_RAB)), options = list(10)))
    output$Ericsson_CODE_RAB <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","CONG_CODE_RAB")] %>% arrange(desc(CONG_CODE_RAB)), options = list(10)))
    output$Ericsson_RSSI <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","RSSI")] %>% arrange(desc(RSSI)), options = list(10)))
    output$Ericsson_Paging <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","PAGINGS")] %>% arrange(desc(PAGINGS)), options = list(10)))
    output$Ericsson_DROP_CS <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","DROP_CS","DROP_CS")] %>% arrange(desc(DROP_CS)), options = list(10)))
    output$Ericsson_AVAIL <- renderDataTable(datatable(datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","DISPONIBILIDAD")] %>% arrange(DISPONIBILIDAD), options = list(10)))
    
    
    datos.4GEricsson_PROVINCIA <- queryData4GEricssonP(datos.Eri$ericsson4G,as.character(input$hour),input$gra)
    datos.4GEricsson_All <- queryData4GEricssonAll(datos.Eri$ericsson4G,datoSigestop,as.character(input$hour),input$gra)
    
    output$Ericsson4GKPI_PROVINCIA <- renderDataTable(datatable(datos.4GEricsson_PROVINCIA, options = list(10)))
    output$Ericsson_ACC <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC")] %>% arrange(ACC), options = list(10)))
    output$Ericsson_RET <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","RET")] %>% arrange(RET), options = list(10)))
    output$Ericsson_VOL <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","PAYLOAD_DL")] %>% arrange(desc(PAYLOAD_DL)), options = list(10)))
    output$Ericsson_TRO <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","THRPTDL","THRPTUL")] %>% arrange(desc(THRPTDL) ), options = list(10)))
    output$Ericsson_USER <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","USER_DL","USER_UL","RRC_USER")] %>% arrange(desc(USER_DL) ), options = list(10)))
    output$Ericsson_LAT <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","LAT")] %>% arrange(LAT), options = list(10)))
    output$Ericsson_DROP <- renderDataTable(datatable(datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","DROP_ERAB")] %>% arrange(desc(DROP_ERAB) ), options = list(10)))
    
    ####HUAWEI####
    
    datos.2GHuawei_PROVINCIA <- queryData2GHUAWEIP(datos.2GHua,as.character(input$hour))
    datos.2GHuawei_All <- queryData2GHUAWEIAll(datos.2GHua,datoSigestop,as.character(input$hour))
    
    output$Huawei2GKPI_PROVINCIA <- renderDataTable(datatable(datos.2GHuawei_PROVINCIA, options = list(10)))
    output$Huawei2G_ACC_Celdas <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC")]%>% arrange(ACC), options = list(pageLength = 10)))
    output$Huawei2G_RET_Celdas <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","RET")]%>% arrange(RET), options = list(pageLength = 10)))
    output$Huawei2G_SER_Celdas <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","SER")]%>% arrange(SER), options = list(pageLength = 10)))
    output$Huawei2G_CONG_TCH_Celdas <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_TCH")]%>% arrange(desc(CONG_TCH)), options = list(pageLength = 10)))
    output$Huawei2G_CONG_SDCCH_Celdas <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_SDCCH")]%>% arrange(desc(CONG_SDCCH)), options = list(pageLength = 10)))
    output$Huawei2G_Pagings <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","PAGINGS")]%>% arrange(desc(PAGINGS)), options = list(pageLength = 10)))
    output$Huawei2G_TCH_Celdas <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","TRAFF_TCH")]%>% arrange(desc(TRAFF_TCH)), options = list(pageLength = 10)))
    output$Huawei2G_SDCCH_Celdas <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","TRAFF_SDCCH")]%>% arrange(desc(TRAFF_SDCCH)), options = list(pageLength = 10)))
    output$Huawei2G_Drop <- renderDataTable(datatable(datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","CALL_DROP_TCH","CALL_DROP_SDCCH")]%>% arrange(desc(CALL_DROP_TCH)), options = list(pageLength = 10)))
    
    datos.3GHuawei_PROVINCIA <- queryData3GHUAWEIP(datos.3GHua,as.character(input$hour))
    datos.3GHuawei_All <- queryData3GHuaweiAll(datos.3GHua,datoSigestop,as.character(input$hour))
    
    output$Huawei3GKPI_PROVINCIA <- renderDataTable(datatable(datos.3GHuawei_PROVINCIA, options = list(10)))
    output$Huawei_ACC_CS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC_CS")] %>% arrange(ACC_CS), options = list(10)))
    output$Huawei_ACC_PS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC_PS")] %>% arrange(ACC_PS), options = list(10)))
    output$Huawei_RET_CS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","RET_CS")] %>% arrange(RET_CS), options = list(10)))
    output$Huawei_RET_PS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","RET_PS")] %>% arrange(RET_PS), options = list(10)))
    output$Huawei_CE_RRC <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_CE_RRC")] %>% arrange(desc(CONG_CE_RRC)), options = list(10)))
    output$Huawei_PWR_RRC <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_PWR_RRC")] %>% arrange(desc(CONG_PWR_RRC)), options = list(10)))
    output$Huawei_CODE_RRC <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_CODE_RRC")] %>% arrange(desc(CONG_CODE_RRC)), options = list(10)))
    output$Huawei_CE_RAB <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_CE_RAB")] %>% arrange(desc(CONG_CE_RAB)), options = list(10)))
    output$Huawei_PWR_RAB <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_PWR_RAB")] %>% arrange(desc(CONG_PWR_RAB)), options = list(10)))
    output$Huawei_CODE_RAB <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","CONG_CODE_RAB")] %>% arrange(desc(CONG_CODE_RAB)), options = list(10)))
    output$Huawei_THROUGHPUT_HSDPA <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","MAX_TROUGHPUT","AVG_TROUGHPUT")] %>% arrange(desc(MAX_TROUGHPUT)), options = list(10)))
    output$Huawei_USER_HSDPA <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","MAX_USER_HSDPA")] %>% arrange(desc(MAX_USER_HSDPA)), options = list(10)))
    output$Huawei_DROP_CS <- renderDataTable(datatable(datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","DROP_CS","DROP_PS")] %>% arrange(desc(DROP_CS)), options = list(10)))
    
    ####NOKIA####
    
    datos.2GNokia_PROVINCIA <- queryData2GnokiaP(datos.2GNok,as.character(input$hour),input$gra)
    datos.2GNokiaAll <- queryData2GnokiaALL(datos.2GNok,datoSigestop,as.character(input$hour),input$gra)
   
    
    output$Nokia2GKPI_PROVINCIA <- renderDataTable(datatable(datos.2GNokia_PROVINCIA, options = list(10)))
    output$Nokia2G_ACC_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC")]%>% arrange(ACC), options = list(pageLength = 10)))
    output$Nokia2G_RET_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","RET")]%>% arrange(RET), options = list(pageLength = 10)))
    output$Nokia2G_SER_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","SER")]%>% arrange(SER), options = list(pageLength = 10)))
    output$Nokia2G_CONG_SDCCH_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","CONG_SDCCH")] %>% arrange(desc(CONG_SDCCH)), options = list(10)))
    output$Nokia2G_CONG_TCH_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","CONG_TCH")] %>% arrange(desc(CONG_TCH)), options = list(10)))
    output$Nokia2G_TCH_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","TRAFF_TCH","TRAFF_TCH_FR","TRAFF_TCH_HR")]%>% arrange(desc(TRAFF_TCH)), options = list(pageLength = 10)))
    output$Nokia2G_SDCCH_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","TRAFF_SDCCH")]%>% arrange(desc(TRAFF_SDCCH)), options = list(pageLength = 10)))
    output$Nokia2G_Paging <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","PAGINGS")] %>% arrange(desc(PAGINGS)), options = list(10)))
    output$Nokia2G_Drop <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","DROP_TCH","DROP_SDCCH")]%>% arrange(desc(DROP_TCH)), options = list(pageLength = 10)))
    output$Nokia2G_ICM_Celdas <- renderDataTable(datatable(datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","INT4_5P")] %>% arrange(desc(INT4_5P)), options = list(10)))
    
    datos.3GNokia_PROVINCIA <- queryData3GnokiaP(datos.3GNok,as.character(input$hour))
    datos.3GNokiaAll <- queryData3GnokiaALL(datos.3GNok,datoSigestop,as.character(input$hour))
    
    output$Nokia_ACC_CS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC_CS")] %>% arrange(ACC_CS), options = list(10)))
    output$Nokia_ACC_PS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC_PS")] %>% arrange(ACC_PS), options = list(10)))
    output$Nokia_RET_CS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","RET_CS")] %>% arrange(RET_CS), options = list(10)))
    output$Nokia_RET_PS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","RET_PS")] %>% arrange(RET_PS), options = list(10)))
    output$Nokia_CODE_RRC <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","COD_RRC")] %>% arrange(COD_RRC), options = list(10)))
    output$Nokia_CODE_RAB <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","COD_RAB")] %>% arrange(COD_RAB), options = list(10)))
    output$Nokia_PWR_RRC <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","PWR_RRC")] %>% arrange(PWR_RRC), options = list(10)))
    output$Nokia_PWR_RAB <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","PWR_RAB")] %>% arrange(PWR_RAB), options = list(10)))
    output$Nokia_CE_RRC <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","CE_RRC")] %>% arrange(desc(CE_RRC)), options = list(10)))
    output$Nokia_CE_RAB <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","CE_RAB")] %>% arrange(desc(CE_RAB)), options = list(10)))
    output$Nokia_USER_HSDPA <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","USER_HSDPA")] %>% arrange(USER_HSDPA), options = list(10)))
    output$Nokia_DROP_CS <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","DROP_CS","DROP_PS")] %>% arrange(desc(DROP_CS)), options = list(10)))
    output$Nokia_DISPONIBILIDAD <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","AVAIL")] %>% arrange(AVAIL), options = list(10)))
    output$Nokia_RTWP <- renderDataTable(datatable(datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","RTWP")] %>% arrange(RTWP), options = list(10)))
    ####PARA EXPORTAR XLSX#######
    
    datosH2G <- datos.2GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","CONG_TCH","CONG_SDCCH","PAGINGS","PAGING_RATE","CALL_DROP_TCH","CALL_DROP_SDCCH")] %>% arrange(ACC)
    datosE2G <- datos.2GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","CONG_TCH","CONG_SDCCH","PAGINGS","PAGING_RATE","CALL_DROP_TCH","CALL_DROP_SDCCH")] %>% arrange(ACC)
    datosE3G <- datos.3GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","RET_PS","DISPONIBILIDAD","CONG_CE_RRC","CONG_PWR_RRC","CONG_CODE_RRC","CONG_CE_RAB","CONG_PWR_RAB","CONG_CODE_RAB","DROP_CS","DROP_PS","RSSI","PAGING_RATE")] %>% arrange(ACC_CS)
    datosH3G <- datos.3GHuawei_All[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","RET_PS","CONG_CE_RRC","CONG_PWR_RRC","CONG_CODE_RRC","CONG_CE_RAB","CONG_PWR_RAB","CONG_CODE_RAB","MAX_TROUGHPUT","AVG_TROUGHPUT","MAX_USER_HSDPA","DROP_CS","DROP_PS","RTWP","PAGING_RATE")] %>% arrange(ACC_CS)
    datosN2G <- datos.2GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC","RET","SER","TRAFF_TCH","TRAFF_TCH_FR","TRAFF_TCH_HR","TRAFF_SDCCH","CONG_SDCCH","CONG_TCH","DROP_TCH","DROP_SDCCH","PAGINGS","PAGING_RATE")] %>% arrange(ACC)
    datosN3G <- datos.3GNokiaAll[c("CELDA","SITIO","PROVINCIA","ACC_CS","ACC_PS","RET_CS","RET_PS","AVAIL","TRAFF_CS","VOL","COD_RRC","COD_RAB","CE_RRC","CE_RAB","PWR_RRC","PWR_RAB","DROP_CS","DROP_PS","TROUGH_HSDPA","TROUGH_HSUPA","USER_HSDPA","USER_HSUPA","PAGING_RATE",)] %>% arrange(ACC_CS)
    datosE4G <- datos.4GEricsson_All[c("CELDA","SITIO","PROVINCIA","ACC","RET","PAYLOAD_DL","PAYLOAD_UL","THRPTDL","THRPTUL","USER_MAX_UL","USER_MAX_DL","RRC_USER","LAT","DROP_ERAB")] %>% arrange(ACC)
    
    
    
    output$EstadoRed <- downloadHandler(
      filename = function() {
        paste('Red Movil ', Sys.Date(), '.xlsx', sep = '')
      },
      content = function(file) {
        write.xlsx2(datosE2G[1:1000,],file,sheetName="2G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(datosH2G[1:1000,],file,sheetName="2G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(datosN2G[1:1000,],file,sheetName="2G_Nokia",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(datosE3G[1:1000,],file,sheetName="3G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(datosH3G[1:1000,],file,sheetName="3G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(datosN3G[1:1000,],file,sheetName="3G_Nokia",col.names=TRUE, row.names = TRUE ,append = TRUE)
        write.xlsx2(datosE4G[1:1000,],file,sheetName="4G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
    )
    
    removeNotification(id)
    showNotification("Actualizada",duration =1)
    
    
    } )

}

# Run the application 
shinyApp(ui = ui, server = server)

