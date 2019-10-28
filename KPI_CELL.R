

library(shiny)
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
 dashboardHeader(title = "Cubacel CELL KPI"),
 dashboardSidebar(
   
 #dateInput("idate","Incio :",value = Sys.Date()-1, format = "yyyy-mm-dd"),
 #dateInput("fdate","Final :",value = Sys.Date(), format ="yyyy-mm-dd"),
 fluidRow(
   column(width = 8, dateInput("idate","Init :",value = Sys.Date()-1, format = "yyyy-mm-dd")),
   column(width = 8, dateInput("fdate","Final :",value = Sys.Date(), format ="yyyy-mm-dd"))
 ),
 
 fluidRow(
   column(width = 5,checkboxGroupInput("network", label = "Network", 
                                       choices = list("2G" = "2G", "3G" = "3G", "4G" = "4G"),
                                       selected = c("3G"))),
   column(width = 5,checkboxGroupInput("provider", label = "Provider", 
                                       choices = list("ERICSSON" = "ERICSSON", "HUAWEI" = "HUAWEI", "NOKIA"="NOKIA"),
                                       selected = c("NOKIA")))
 ),
 selectInput("gra", label = "Granularidad:", choices = list("D" = "yyyy-mm-dd", "H" = "yyyy-mm-dd hh","C" = "yyyy-mm-dd hh:MM"), selected = "yyyy-mm-dd", width = 180),
 
 fluidRow(
   column(width = 7,textInput("cellcontext", "Name(%):", value = "%")),
   column(width = 3,checkboxGroupInput("type", label = NULL, 
                                       choices = list("CELL" = "CELL", "BTS" = "BTS"),
                                       selected = c("CELL")))
   ),
 
 actionButton("query","Query"),
 sidebarMenu(
   menuItem("2G", tabName = "Graficos2G", icon = icon("tb"),
            menuSubItem("KPI", tabName = "kpi_2G"),
            menuSubItem("Trafico", tabName = "traff_2G"),
            menuSubItem("Volumen", tabName = "volumen_2G"),
            menuSubItem("Congestion", tabName = "congestion_2G"),
            menuSubItem("Drop", tabName = "drop_2G"),
            menuSubItem("Interferencia", tabName = "icm_2G"),
            menuSubItem("Pagings", tabName = "pagings_2G")
          ),
   
   menuItem("3G", tabName = "Graficos3G", icon = icon("tb"),
            menuSubItem("KPI", tabName = "kpi_3G"),
            menuSubItem("Trafico", tabName = "traff_3G"),
            menuSubItem("Volumen", tabName = "volumen_3G"),
            menuSubItem("Congestion", tabName = "congestion_3G"),
            menuSubItem("Drop", tabName = "drop_3G"),
            menuSubItem("Handover", tabName = "handover_3G"),
            menuSubItem("Troughput", tabName = "troughput_3G"),
            menuSubItem("User", tabName = "user_3G"),
            menuSubItem("User_State", tabName = "userState_3G"),
            menuSubItem("Interferencia", tabName = "rtwp_3G"),
            menuSubItem("Paging", tabName = "paging_3G"),
            menuSubItem("Channel_Switch", tabName = "channel_3G"),
            menuSubItem("Channel_Capacity", tabName = "channel_C_3G"),
            menuSubItem("Code_Capacity", tabName = "code_3G"),
            menuSubItem("Nokia_Eval", tabName = "noke_3G"),
            menuSubItem("Feature", tabName = "feat_3G")
   ),
   menuItem("4G", tabName = "Graficos4G", icon = icon("tb"),
            menuSubItem("KPI", tabName = "kpi_4G"),
            menuSubItem("Volumen", tabName = "volumen_4G"),
            menuSubItem("Congestion", tabName = "congestion_4G"),
            menuSubItem("Latencia", tabName = "latencia_4G"),
            menuSubItem("Drop", tabName = "drop_4G"),
            menuSubItem("Troughput", tabName = "troughput_4G"),
            menuSubItem("User", tabName = "user_4G"),
            menuSubItem("Interferencia", tabName = "rtwp_4G")
            ),
   
   menuItem("BTS", tabName = "GraficosBTS", icon = icon("tb"),
            menuSubItem("CE", tabName = "ce"),
            menuSubItem("USER", tabName = "user")
            
   )
 ),
 textInput("cellfilter","Name:",value = ""),
 #actionButton("fbutton", "Filter"),
 
 fluidRow(
   column(width = 3,actionButton("fbutton", "Filtrar") ),
   column(width = 8,selectInput("filter",label = NULL,choices = list("Celda" = "ByCell","Sitio" = "BySite","Provincia" = "ByProvince","Controlador" = "ByRNC","Total"="Total"), selected = "ByProvince", width = 180) )
 ),
 downloadButton("EstadoRed","Descargar")
                      ),
 dashboardBody(
   tabItems(
     
###DATA 2G#####
     
     tabItem(tabName = "kpi_2G",
             fluidRow(
               tabBox(width = 12,
                      title = "Data2G",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "2G_KPI", height = "250px",
                      tabPanel("ACC",dygraphOutput("ACC",width = "100%", height = "350px")),
                      tabPanel("RET",dygraphOutput("RET",width = "100%", height = "350px")),
                      tabPanel("SER",dygraphOutput("SER",width = "100%", height = "350px"))
               )
             )
     ),   
     tabItem(tabName = "traff_2G",
             fluidRow(
               tabBox(width = 12,
                      title = "Data2G",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "TRAF_2G", height = "250px",
                      tabPanel("TRAFICO_TCH",dygraphOutput("TRAF_TCH",width = "100%", height = "350px")),
                      tabPanel("TRAFICO_FR",dygraphOutput("TRAF_FR",width = "100%", height = "350px")),
                      tabPanel("TRAFICO_HR",dygraphOutput("TRAF_HR",width = "100%", height = "350px")),
                      tabPanel("TRAFICO_SDCCH",dygraphOutput("TRAF_SDCCH",width = "100%", height = "350px"))
               )
             )
     ),
     tabItem(tabName = "volumen_2G",
             fluidRow(
               tabBox(width = 12,
                      title = "Data2G",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "VOL_2G", height = "250px",
                      tabPanel("VOL_UL",dygraphOutput("VOL_UL",width = "100%", height = "350px")),
                      tabPanel("VOL_DL",dygraphOutput("VOL_DL",width = "100%", height = "350px"))
               )
             )
     ),
     
     tabItem(tabName = "congestion_2G",
             fluidRow(
               tabBox(width = 12,
                      title = "Data2G",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "CON_2G", height = "250px",
                      tabPanel("CONG_TCH",dygraphOutput("CONG_TCH",width = "100%", height = "350px")),
                      tabPanel("CONG_SDCCH",dygraphOutput("CONG_SDCCH",width = "100%", height = "350px"))
               )
             )
     ),
     tabItem(tabName = "drop_2G",
             fluidRow(
               tabBox(width = 12,
                      title = "Data2G",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "DROP_2G", height = "250px",
                      tabPanel("DROP_TCH",dygraphOutput("DROP_TCH",width = "100%", height = "350px")),
                      tabPanel("DROP_SDCCH",dygraphOutput("DROP_SDCCH",width = "100%", height = "350px"))
               )
             )
     ),
     
     tabItem(tabName = "icm_2G",
             fluidRow(
               tabBox(width = 12,
                      title = "Data2G",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "ICM_2G", height = "250px",
                      tabPanel("Interferencia 4+5",dygraphOutput("INT4_5",width = "100%", height = "350px")),
                      tabPanel("Interferencia 4",dygraphOutput("INT_4",width = "100%", height = "350px")),
                      tabPanel("Interferencia 5",dygraphOutput("INT_5",width = "100%", height = "350px")),
                      tabPanel("Interferencia 4+5%",dygraphOutput("INT4_5P",width = "100%", height = "350px"))
                      
                      
               )
             )
     ),
     tabItem(tabName = "pagings_2G",
             fluidRow(
               tabBox(width = 12,
                      title = "Data2G",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "PAG_2G", height = "250px",
                      tabPanel("DISCARD_CS",dygraphOutput("PAGINGS",width = "100%", height = "350px")),
                      tabPanel("DISCARD_PS",dygraphOutput("PAGINGS_PS",width = "100%", height = "350px"))
               )
             )
     ),
     
     tabItem(tabName = "prueba_2G",
             fluidRow(
               tabBox(width = 12,
                      title = "Data2G",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "prueba_2G", height = "250px",
                      tabPanel("Prueba",dygraphOutput("prueba_2G",width = "100%", height = "350px"))
               )
             )
     ),
     
     
###DATA 3G#####
 
  tabItem(tabName = "kpi_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    id = "3G_KPI", height = "250px",
                    tabPanel("ACC_CS",dygraphOutput("ACC_CS",width = "100%", height = "500px")),
                    tabPanel("ACC_PS",dygraphOutput("ACC_PS",width = "100%", height = "350px")),
                    tabPanel("RET_CS",dygraphOutput("RET_CS",width = "100%", height = "350px")),
                    tabPanel("RET_PS",dygraphOutput("RET_PS",width = "100%", height = "350px")),
                    tabPanel("ACC_RRC_CS",dygraphOutput("ACC_RRC_CS",width = "100%", height = "350px")),
                    tabPanel("ACC_RAB_CS",dygraphOutput("ACC_RAB_CS",width = "100%", height = "350px")),
                    tabPanel("ACC_RRC_PS",dygraphOutput("ACC_RRC_PS",width = "100%", height = "350px")),
                    tabPanel("ACC_RAB_PS",dygraphOutput("ACC_RAB_PS",width = "100%", height = "350px")),
                    tabPanel("ACC_HSDPA",dygraphOutput("HSDPA_ACC",width = "100%", height = "350px")),
                    tabPanel("ACC_HSUPA",dygraphOutput("HSUPA_ACC",width = "100%", height = "350px")),
                    tabPanel("DISPONIBILIDAD",dygraphOutput("AVAIL",width = "100%", height = "350px"))
                   
             )
           )
   ),   
   tabItem(tabName = "traff_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    id = "TRAF_3G", height = "250px",
                    tabPanel("TRAFICO_CS",dygraphOutput("TRAF_CS",width = "100%", height = "350px")),
                    tabPanel("TRAFICO_PS",dygraphOutput("TRAF_PS",width = "100%", height = "350px")),
                    tabPanel("TRAFICO_TOTAL",dygraphOutput("TRAF_TOT",width = "100%", height = "350px"))
                    
             )
           )
   ),
   tabItem(tabName = "volumen_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    id = "VOL_3G", height = "250px",
                    tabPanel("VOLUMEN",dygraphOutput("VOL",width = "100%", height = "350px")),
                    tabPanel("VOL_HSDPA",dygraphOutput("VOL_HSDPA",width = "100%", height = "350px")),
                    tabPanel("VOL_HSUPA",dygraphOutput("VOL_HSUPA",width = "100%", height = "350px")),
                    tabPanel("VOL_R99_DL",dygraphOutput("VOL_R99_DL",width = "100%", height = "350px")),
                    tabPanel("VOL_R99_UL",dygraphOutput("VOL_R99_UL",width = "100%", height = "350px"))
                    
                        
           )
   )),
   
   tabItem(tabName = "congestion_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    id = "CON_3G", height = "250px",
                    tabPanel("CONG_CE_RRC",dygraphOutput("CE_RRC",width = "100%", height = "350px")),
                    tabPanel("CONG_PWR_RRC",dygraphOutput("PWR_RRC",width = "100%", height = "350px")),
                    tabPanel("CONG_PWR_RRC_DL",dygraphOutput("PWR_RRC_DL",width = "100%", height = "350px")),
                    tabPanel("CONG_PWR_RRC_UL",dygraphOutput("PWR_RRC_UL",width = "100%", height = "350px")),
                    tabPanel("CONG_CODE_RRC",dygraphOutput("CODE_RRC",width = "100%", height = "350px")),
                    tabPanel("CONG_CE_RAB",dygraphOutput("CE_RAB",width = "100%", height = "350px")),
                    tabPanel("CONG_PWR_RAB",dygraphOutput("PWR_RAB",width = "100%", height = "350px")),
                    tabPanel("CONG_PWR_RAB_DL",dygraphOutput("PWR_RAB_DL",width = "100%", height = "350px")),
                    tabPanel("CONG_PWR_RAB_UL",dygraphOutput("PWR_RAB_UL",width = "100%", height = "350px")),
                    tabPanel("CONG_CODE_RAB",dygraphOutput("CODE_RAB")),
                    tabPanel("CONG_HSDPA_USER",dygraphOutput("CONG_HSDPA_USER",width = "100%", height = "350px")),
                    tabPanel("CONG_HSUPA_USER",dygraphOutput("CONG_HSUPA_USER",width = "100%", height = "350px")),
                    tabPanel("NO_ACC_HSDPA",dygraphOutput("NO_ACC_HSDPA",width = "100%", height = "350px")),
                    tabPanel("NO_ACC_HSUPA",dygraphOutput("NO_ACC_HSUPA",width = "100%", height = "350px"))
          
             )
           )
   ),
   tabItem(tabName = "drop_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "DROP_3G", height = "250px",
                    tabPanel("DROP_CS",dygraphOutput("DROP_CS",width = "100%", height = "350px")),
                    tabPanel("DROP_PS",dygraphOutput("DROP_PS",width = "100%", height = "350px"))
             )
           )
   ),
   
   tabItem(tabName = "rtwp_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "RTWP_3G", height = "250px",
                    tabPanel("RTWP",dygraphOutput("RTWP",width = "100%", height = "350px"))
                    
             )
           )
   ),
   tabItem(tabName = "troughput_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "TRO_3G", height = "250px",
                    tabPanel("TROUGHPUT_MAX_HSDPA",dygraphOutput("TP_MAX",width = "100%", height = "350px")),
                    tabPanel("TROUGHPUT_AVG_HSDPA",dygraphOutput("TP_AVG",width = "100%", height = "350px")),
                    tabPanel("TROUGHPUT_MAX_HSUPA",dygraphOutput("TP_MAX_U",width = "100%", height = "350px")),
                    tabPanel("TROUGHPUT_AVG_HSUPA",dygraphOutput("TP_AVG_U",width = "100%", height = "350px")),
                    tabPanel("CPICH_PWR",dygraphOutput("CPICH_PWR",width = "100%", height = "350px")),
                    tabPanel("CPICH_PWR_RTWP",dygraphOutput("CPICH_PWR_RTWP",width = "100%", height = "350px")),
                    tabPanel("LDR_PWR_DL",dygraphOutput("LDR_PWR_DL",width = "100%", height = "350px")),
                    tabPanel("LDR_PWR_UL",dygraphOutput("LDR_PWR_UL",width = "100%", height = "350px"))
                    
             )
           )
   ),
   tabItem(tabName = "user_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "User_3G", height = "250px",
                    tabPanel("USER_HSDPA",dygraphOutput("USER_HSDPA",width = "100%", height = "350px")),
                    tabPanel("USER_HSUPA",dygraphOutput("USER_HSUPA",width = "100%", height = "350px")),
                    tabPanel("HSUPA_2MS",dygraphOutput("HSUPA_2MS",width = "100%", height = "350px")),
                    tabPanel("HSUPA_10MS",dygraphOutput("HSUPA_10MS",width = "100%", height = "350px")),
                    tabPanel("USER_DL",dygraphOutput("USER_DL_3G",width = "100%", height = "350px")),
                    tabPanel("AVG_USER_DL",dygraphOutput("MAX_USER_DL_3G",width = "100%", height = "350px")),
                    tabPanel("USER_UL",dygraphOutput("USER_UL_3G",width = "100%", height = "350px")),
                    tabPanel("AVG_USER_UL",dygraphOutput("MAX_USER_UL_3G",width = "100%", height = "350px")),
                    tabPanel("USER_16_QAM",dygraphOutput("USER_16",width = "100%", height = "350px"))
                 
                     )
           )
   ),

  tabItem(tabName = "userState_3G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data3G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "userState_3G", height = "250px",
                 tabPanel("USER_DCH",dygraphOutput("USER_DCH",width = "100%", height = "350px")),
                 tabPanel("USER_PCH",dygraphOutput("USER_PCH",width = "100%", height = "350px")),
                 tabPanel("USER_FACH",dygraphOutput("USER_FACH",width = "100%", height = "350px"))
                )
                 
                 
          )
        
   ),
   tabItem(tabName = "paging_3G",
           fluidRow(
             tabBox(width = 12,
                    title = "Data3G",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "PAG_3G", height = "250px",
                    tabPanel("PAGING_DISCARD",dygraphOutput("PAG_DIS_3G",width = "100%", height = "350px")),
                    tabPanel("PAGING_SENT",dygraphOutput("PAG_SEN_3G",width = "100%", height = "350px")),
                    tabPanel("PAGING_DISCARD_PCH",dygraphOutput("PAG_DIS_PCH_3G",width = "100%", height = "350px")),
                    tabPanel("PAGING_DISCARD_URA",dygraphOutput("PAG_DIS_URA_3G",width = "100%", height = "350px")),
                    tabPanel("PAGING__SENT_URA",dygraphOutput("PAG_SEN_URA_3G",width = "100%", height = "350px")),
                    tabPanel("PAGING__SENT_PCH",dygraphOutput("PAG_SEN_PCH_3G",width = "100%", height = "350px")),
                    tabPanel("PAGING_DISCARD%",dygraphOutput("PAG_DIS_CIENTO",width = "100%", height = "350px"))
             )
           )
           
        ),
   
   tabItem(tabName = "noke_3G",
          
          fluidRow(
          tabBox(width = 12,
                 title = "Data3G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "Noke_3G", height = "250px",
                 tabPanel("SETUP_FAIL_IUB_HS",dygraphOutput("SETUP_FAIL_IUB_HS",width = "100%", height = "350px")),
                 tabPanel("REJ_HS_DSCH ",dygraphOutput("REJ_HS_DSCH",width = "100%", height = "350px")),
                 tabPanel("REJ_E_DCH",dygraphOutput("REJ_E_DCH",width = "100%", height = "350px")),
                 tabPanel("REJ_DCH_DUE_POWER_BGR_DL",dygraphOutput("CONG_PWR_BGR",width = "100%", height = "350px")),
                 tabPanel("REJ_DCH_DUE_POWER_INT_DL",dygraphOutput("CONG_PWR_INT",width = "100%", height = "350px")),
                 tabPanel("REJ_DCH_DUE_CODES_INT_DL",dygraphOutput("REJ_DCH_DUE_CODES_INT_DL",width = "100%", height = "350px")),
                 tabPanel("REJ_DCH_DUE_CODES_BGR_DL",dygraphOutput("REJ_DCH_DUE_CODES_BGR_DL",width = "100%", height = "350px")),
                 tabPanel("PS_SETUP_FAIL_AC_INT",dygraphOutput("PS_SETUP_FAIL_AC_INT",width = "100%", height = "350px")),
                 tabPanel("PS_SETUP_FAIL_AC_BGR",dygraphOutput("PS_SETUP_FAIL_AC_BGR",width = "100%", height = "350px")),
                 tabPanel("PS_SETUP_FAIL_AC_DL_NRT",dygraphOutput("PS_SETUP_FAIL_AC_DL_NRT",width = "100%", height = "350px")),
                 tabPanel("PS_SETUP_FAIL_AC_UL_NRT",dygraphOutput("PS_SETUP_FAIL_AC_UL_NRT",width = "100%", height = "350px")),
                 tabPanel("PS_SETUP_FAIL_AC_COD_NRT",dygraphOutput("PS_SETUP_FAIL_AC_COD_NRT",width = "100%", height = "350px")),
                 tabPanel("RAB_STP_FAIL_PS_STREA_BTS",dygraphOutput("RAB_STP_FAIL_PS_STREA_BTS",width = "100%", height = "350px"))
                
                 
          )
        )
        
    ),

    tabItem(tabName = "handover_3G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data3G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "HANDOVER_3G", height = "250px",
                 tabPanel("UNSUCC_UPDATES_ON_SHO_NRT",dygraphOutput("UNSC_SHO_N",width = "100%", height = "350px")),
                 tabPanel("UNSUCC_UPDATES_ON_SHO_RT",dygraphOutput("UNSC_SHO_R",width = "100%", height = "350px")),
                 tabPanel("SHO_SUCC_RATIO%",dygraphOutput("SUCC_SHO",width = "100%", height = "350px"))
                 
          )
        )
    ),
    
    tabItem(tabName = "channel_3G",
            fluidRow(
              tabBox(width = 12,
                     title = "Data3G",
                     # The id lets us use input$tabset1 on the server to find the current tab
                     id = "CHANNEL_3G", height = "250px",
                     tabPanel("ATT_HS_DSCH_TO_FACH",dygraphOutput("ATT_HS_DSCH_TO_FACH",width = "100%", height = "350px")),
                     tabPanel("ATT_FACH_TO_HS_DSCH",dygraphOutput("ATT_FACH_TO_HS_DSCH",width = "100%", height = "350px")),
                     tabPanel("SUCC_HS_DSCH_TO_FACH",dygraphOutput("SUCC_HS_DSCH_TO_FACH",width = "100%", height = "350px")),
                     tabPanel("SUCC_FACH_TO_HS_DSCH",dygraphOutput("SUCC_FACH_TO_HS_DSCH",width = "100%", height = "350px")),
                     tabPanel("ATT_DCH_TO_PCH",dygraphOutput("ATT_DCH_TO_PCH",width = "100%", height = "350px")),
                     tabPanel("ATT_PCH_TO_DCH",dygraphOutput("ATT_PCH_TO_DCH",width = "100%", height = "350px")),
                     tabPanel("ATT_FACH_TO_PCH",dygraphOutput("ATT_FACH_TO_PCH",width = "100%", height = "350px")),
                     tabPanel("ATT_PCH_TO_FACH",dygraphOutput("ATT_PCH_TO_FACH",width = "100%", height = "350px"))
                     
              )
            )
    ),

    tabItem(tabName = "channel_C_3G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data3G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "CHANNEL_C_3G", height = "250px",
                 
                 tabPanel("SCCPCH_LOAD",dygraphOutput("SCCPCH_LOAD",width = "100%", height = "350px")),
                 tabPanel("FACH_u_LOAD",dygraphOutput("FACH_u_LOAD",width = "100%", height = "350px")),
                 tabPanel("FACH_c_LOAD",dygraphOutput("FACH_c_LOAD",width = "100%", height = "350px")),
                 tabPanel("RACH_u_LOAD",dygraphOutput("RACH_u_LOAD",width = "100%", height = "350px")),
                 tabPanel("RACH_c_LOAD",dygraphOutput("RACH_c_LOAD",width = "100%", height = "350px"))
          )
        )
    ),
    tabItem(tabName = "code_3G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data3G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "CODE_3G", height = "250px",
                 
                 tabPanel("AVG_CODE_TREE",dygraphOutput("AVG_CODE_TREE",width = "100%", height = "350px")),
                 tabPanel("MAX_CODE_TREE",dygraphOutput("MAX_CODE_TREE",width = "100%", height = "350px")),
                 tabPanel("MIN_CODE_TREE",dygraphOutput("MIN_CODE_TREE",width = "100%", height = "350px")),
                 tabPanel("CODE_BLOCK_DL",dygraphOutput("CODE_BLOCK_DL",width = "100%", height = "350px"))
                 
          )
        )
    ),


    tabItem(tabName = "feat_3G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data3G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "CHANNEL_3G", height = "250px",
                 tabPanel("REJ_HSDSCH_VP",dygraphOutput("MEH_HSDSCH",width = "100%", height = "350px")),
                 tabPanel("ADMIT_CS_VOICE_VP",dygraphOutput("MEH_CS_VOICE",width = "100%", height = "350px")),
                 tabPanel("HSPA_RT_O_NRT_GV",dygraphOutput("HSPA_RT_O_NRT",width = "100%", height = "350px"))
                 
          )
        )
    ),


###BTS 3G#####
   tabItem(tabName = "ce",
        fluidRow(
          tabBox(width = 12,
                 title = "Data3G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "CE_3G", height = "250px",
                 tabPanel("AVG_CE_R99_DL%",dygraphOutput("AVG_USED_CE_R99_DL",width = "100%", height = "350px")),
                 tabPanel("AVG_CE_R99_UL%",dygraphOutput("AVG_USED_CE_R99_UL",width = "100%", height = "350px")),
                 tabPanel("AVG_CE_R99%",dygraphOutput("AVG_USED_CE_R99" ,width = "100%", height = "350px")),
                 tabPanel("MAX_CE_R99_DL%",dygraphOutput("MAX_USED_CE_R99_DL"      ,width = "100%", height = "350px")),
                 tabPanel("MAX_CE_R99_UL%",dygraphOutput("MAX_USED_CE_R99_UL",width = "100%", height = "350px")),
                 tabPanel("MAX_CE_R99%",dygraphOutput("MAX_USED_CE_R99",width = "100%", height = "350px")),
                 tabPanel("AVG_HSUPA_SU%",dygraphOutput("AVG_USED_HSUPA_SU",width = "100%", height = "350px")),
                 tabPanel("MAX_HSUPA_SU%",dygraphOutput("MAX_USED_HSUPA_SU",width = "100%", height = "350px")),
                 tabPanel("AVG_SU%",dygraphOutput("AVG_USED_SU",width = "100%", height = "350px")),
                 tabPanel("MAX_SU%",dygraphOutput("MAX_USED_SU",width = "100%", height = "350px"))
                 
                 
           
          )
        )
        
    ),

    tabItem(tabName = "user",
        fluidRow(
          tabBox(width = 12,
                 title = "user_bts",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "USER_3G", height = "250px",
                 tabPanel("AVG_HSDPA_USER%" ,dygraphOutput("AVG_HSDPA_USER" ,width = "100%", height = "350px")),
                 tabPanel("MAX_HSDPA_USER%" ,dygraphOutput("MAX_HSDPA_USER",width = "100%", height = "350px")),
                 tabPanel("AVG_HSUPA_USER%",dygraphOutput("AVG_HSUPA_USER",width = "100%", height = "350px")),
                 tabPanel("MAX_HSUPA_USER%",dygraphOutput("MAX_HSUPA_USER",width = "100%", height = "350px"))
                 
          )
        )
        
    ),
###DATA 4G#####
  tabItem(tabName = "kpi_4G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data4G",
                 id = "4G_KPI", height = "250px",
                 tabPanel("ACC",dygraphOutput("ACC_4G",width = "100%", height = "350px")),
                 tabPanel("RET",dygraphOutput("RET_4G",width = "100%", height = "350px"))
          )
        )
  ),   
  
  tabItem(tabName = "volumen_4G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data4G",
                 id = "VOL_4G", height = "250px",
                 tabPanel("VOLUMEN_DL",dygraphOutput("VOLUMEN_DL",width = "100%", height = "350px")),
                 tabPanel("VOLUMEN_UL",dygraphOutput("VOLUMEN_UL",width = "100%", height = "350px"))
          )
        )),

  tabItem(tabName = "congestion_4G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data4G",
                 id = "CON_4G", height = "250px",
                 tabPanel("CONG_CE",dygraphOutput("CONG_CE",width = "100%", height = "350px")),
                 tabPanel("CONG_PWR",dygraphOutput("CONG_PWR",width = "100%", height = "350px"))
                 
                 
          )
        )
   ),
   tabItem(tabName = "drop_4G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data4G",
                
                 id = "DROP_4G", height = "250px",
                 tabPanel("DROP_ERAB",dygraphOutput("DROP_ERAB",width = "100%", height = "350px"))
          )
        )
   ),

   tabItem(tabName = "rtwp_4G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data4G",
                 id = "RTWP_4G", height = "250px",
                 tabPanel("INT",dygraphOutput("INT",width = "100%", height = "350px"))
                 
          )
        )
   ),
   tabItem(tabName = "troughput_4G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data4G",
                 id = "TRO_4G", height = "250px",
                 tabPanel("TROUGHPUT_DL",dygraphOutput("TP_DL",width = "100%", height = "350px")),
                 tabPanel("TROUGHPUT_UL",dygraphOutput("TP_UL",width = "100%", height = "350px"))
                 
                 
          )
        )
   ),
   tabItem(tabName = "user_4G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data4G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "User_4G", height = "250px",
                 tabPanel("USER_DL",dygraphOutput("USER_DL",width = "100%", height = "350px")),
                 tabPanel("USER_UL",dygraphOutput("USER_UL",width = "100%", height = "350px")),
                 tabPanel("USER_MAX_DL",dygraphOutput("USER_MAX_DL",width = "100%", height = "350px")),
                 tabPanel("USER_MAX_UL",dygraphOutput("USER_MAX_UL",width = "100%", height = "350px"))
          )
        )
    ),

   tabItem(tabName = "latencia_4G",
        fluidRow(
          tabBox(width = 12,
                 title = "Data4G",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "Late_4G", height = "250px",
                 tabPanel("LATENCIA",dygraphOutput("LAT",width = "100%", height = "350px"))
                 
          )
        )
   )
    
 )
)
)

###FUNCTION#####
queryData2GHuawei <- function(idate,fdate,cellcontext,granul){
  cellcontext <- paste0('LABEL=',cellcontext)
  con <- odbcConnect('SIGESTOP',uid="ADMINRED",pwd="adm!nr3d")
  options(dec = ",")
  if(granul == 'yyyy-mm-dd hh' ){
    granul <- 'YYYY-MM-DD HH24'
  }
  
  if(granul == 'yyyy-mm-dd' ){
    granul <- 'YYYY-MM-DD'
  }
  
  qry1 <- "SELECT  to_char(DATETIME, 'granul') AS DIA, OBJ as CELDA, EXCHANGE_ID, 
  SUM(K3000) AS K3000,
  SUM(K3003) AS K3003,
  SUM(K3004) AS K3004,
  SUM(K3001) AS K3001,
  SUM(K3010A) AS K3010A,
  SUM(K3013A) AS K3013A,
  SUM(K3040) AS K3040,
  SUM(K3014) AS K3014,
  MAX(K3045) AS K3045,
  SUM(K3034) AS K3034,
  sum(K3011A) AS K3011A,
  sum(K3011B) AS K3011B,
  SUM(K3014 - K3034) AS K3014MINUSK3034
  FROM STATDBA.OSS_HUA_KPICEL_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ
  ORDER BY to_char(DATETIME, 'granul')"
  
  qry2 <-"SELECT  to_char(DATETIME, 'granul') AS DIA, OBJ as CELDA, EXCHANGE_ID, 
  SUM(CM30) AS CM30,
  SUM(CM33) AS CM33
  FROM  STATDBA.OSS_HUA_CDROP_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ
  ORDER BY to_char(DATETIME, 'granul')"
  
  qry3 <-"SELECT  to_char(DATETIME, 'granul') AS DIA, OBJ as CELDA, EXCHANGE_ID, 
  SUM(A330) AS A330,
  SUM(A331) AS A331,
  SUM(A337) AS A337,
  SUM(A338) AS A338,
  SUM(A339) AS A339,
  SUM(A340) AS A340
  FROM STATDBA.OSS_HUA_PAGING_ABIS
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ
  ORDER BY to_char(DATETIME, 'granul')"
  
  qry4 <- "SELECT  to_char(DATETIME, 'granul') AS DIA, OBJ as CELDA, EXCHANGE_ID, 
  SUM(CH363) AS CH363
  FROM STATDBA.OSS_HUA_IINTERRAT_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ
  ORDER BY to_char(DATETIME, 'granul')"
  
  qry5 <-"SELECT  to_char(DATETIME, 'granul') AS DIA, OBJ as CELDA, EXCHANGE_ID, 
  SUM(CH303) AS CH303
  FROM STATDBA.OSS_HUA_INTRAHCEL_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ
  ORDER BY to_char(DATETIME, 'granul')"
  
  qry6 <-"SELECT  to_char(DATETIME, 'granul') AS DIA, OBJ as CELDA, EXCHANGE_ID, 
  SUM(CH333) AS CH333
  FROM STATDBA.OSS_HUA_OEINTERHC_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ
  ORDER BY to_char(DATETIME, 'granul')"
  
  
  qry7 <-"SELECT  to_char(DATETIME, 'granul') AS DIA, OBJ as CELDA,EXCHANGE_ID, 
  SUM(CH353) AS CH353
  FROM STATDBA.OSS_HUA_OINTERRAT_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ
  ORDER BY to_char(DATETIME, 'granul')"
  
  qry8 <-"SELECT  to_char(DATETIME, 'granul') AS DIA, OBJ as CELDA,EXCHANGE_ID, 
  SUM(CH313) AS CH313
  FROM STATDBA.OSS_HUA_OIINTERHC_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ
  ORDER BY to_char(DATETIME, 'granul')"
  
  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,sub("cellcontext",cellcontext,qry))))
  }             
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 8
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    #Connecting to database through RODBC
    con <- odbcConnect('SIGESTOP',uid="felix",pwd="F3l!x2k10")
    options(dec=",")
    #Test connection
    #odbcGetInfo(ch)
    result<- sqlQuery(con, qrys[i])
    #result<-sqlQuery(ch,paste('SELECT * FROM ',databases[i]))  
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datahua <- outputPar[[1]]
  for (i in 2:length(outputPar)){datahua <- datahua %>% full_join(outputPar[[i]])}
  
  datahua$CELDA<-substr(datahua$CELDA,7,stop=unlist(lapply(gregexpr(',',datahua$CELDA), `[[`,1))-1)
  
  #datahua <- datahua %>% inner_join(datos) %>%group_by(CELDA)
  
  datahua$K3014 <- as.numeric(as.character(datahua$K3014))
  datahua$K3004 <- as.numeric(as.character(datahua$K3004))
  datahua$K3014MINUSK3034 <- as.numeric(as.character(datahua$K3014MINUSK3034))
  datahua$K3034 <- as.numeric(as.character(datahua$K3034))
  
  
  qry <- "SELECT * FROM EXCHANGE_ID"
  EXCHANGE_ID <- sqlQuery(con, qry)
  odbcClose(con)
  
  datahua <- datahua %>% inner_join(EXCHANGE_ID %>% select(ID,CODE) , by = c("EXCHANGE_ID" = "CODE")) %>% mutate(BSC=ID)

  return(datahua)
  
}

queryData3GHuawei <- function(idate,fdate,cellcontext,granul){
  #cellcontext <- paste0('LABEL=',cellcontext)
  
  con <- odbcConnect('SIGESTOP',uid="ADMINRED",pwd="adm!nr3d")
  options(dec = ",")
  
  if(granul == 'yyyy-mm-dd hh' ){
    granul <- 'YYYY-MM-DD HH24'
  }
  
  if(granul == 'yyyy-mm-dd' ){
    granul <- 'YYYY-MM-DD'
  }
  
  
  qry1<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
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
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry2<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1) AS FailConnEstb_Cong,
  SUM(C2) AS CODE_CONG_RRC,
  SUM (C3 + C6) as CE_CONG_RRC,
  SUM (C5 + C8) as PWR_CONG_RRC,
  SUM (C4 + C7) as IUB_CONG_RRC 
  FROM STATDBA.OSS_HUA_RRC_REJECT
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry3<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C3) AS Cong_PS_RAB,
  SUM(C5 + C6 + C7 + C10) as PS_RAB_ATT

  FROM STATDBA.OSS_HUA_PS_RAB_SETUP
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD '))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry4<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1) as CODE_PS_CONG_RAB,
  SUM(C2) as CELL_PS_CONG_RAB,
  SUM (C3 + C8 ) as CE_PS_CONG_RAB,
  SUM (C4 + C9 ) as IUB_PS_CONG_RAB,
  SUM (C5 + C10 ) as PWR_PS_CONG_RAB,
  SUM (C5) as PWR_PS_CONG_RAB_DL,
  SUM (C10) as PWR_PS_CONG_RAB_UL

  FROM STATDBA.OSS_HUA_PS_RAB_FAIL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry5 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C2) AS RAB_CS_ATT
  
  FROM STATDBA.OSS_HUA_CS_RAB_SETUP
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry6 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1) as CODE_CS_CONG_RAB,
  SUM(C2) as CELL_CS_CONG_RAB,
  SUM (C3 + C6 ) as CE_CS_CONG_RAB,
  SUM (C4 + C7 ) as IUB_CS_CONG_RAB,
  SUM (C5 + C8 ) as PWR_CS_CONG_RAB,
  SUM( C5) as PWR_CS_CONG_RAB_DL,
  SUM( C8) as PWR_CS_CONG_RAB_UL

  FROM STATDBA.OSS_HUA_CS_RAB_FAIL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry7 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C4)  AS  TRAFICO_CS
  FROM STATDBA.OSS_HUA_RADIO_BEARER
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry8 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  MAX(C3) AS MAX_USER_HSDPA,
  MAX (C4)  AS MAX_TROUGHPUT_HSDPA,
  AVG (C4)  AS AVG_TROUGHPUT_HSDPA,
  SUM (C7) AS TOTAL_VOLUMEN_HSDPA
  FROM STATDBA.OSS_HUA_HSDPA_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry9 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM (C6) AS TOTAL_VOLUMEN_HSUPA,
  MAX (C2) AS MAX_USER_HSUPA,
  MAX (C3)  AS MAX_TROUGHPUT_HSUPA,
  AVG (C3)  AS AVG_TROUGHPUT_HSUPA

  FROM STATDBA.OSS_HUA_HSUPA_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  
  qry10 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1)	AS CS_DROP_A,				
  SUM(C2)	AS CS_DROP_N,				
  SUM(C3)	AS PS_DROP_A,				
  SUM(C4)	AS PS_DROP_N				
  FROM STATDBA.OSS_HUA_RAB_REL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry11 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  AVG(C11) as RTWP
  FROM STATDBA.OSS_HUA_RTWP_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry12 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C27) as SUM_USER_DL,
  AVG(C27) as MAX_USER_DL,
  SUM(C28) as SUM_USER_UL,
  AVG(C28) as MAX_USER_UL,
  AVG(C1) as CPICH_PWR,
  SUM (C42) as CPICH_BY_RTWP,
  SUM(C10) as  LDR_PWR_DL,
  SUM(C13) as LDR_PWR_UL,
  sum(C112) as SUCC_FAST_DORM,
  sum(C105) as ATT_FAST_DORM


  FROM STATDBA.OSS_HUA_EQV_USER
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry13 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  
  SUM(C8) as PAGING_DISCARD,
  SUM(C9) as PAGING_SENT
  FROM STATDBA.OSS_HUA_PAGING_3G
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry14 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  
  MAX(C1) as MAX_FACH,
  SUM(C2) as CELL_DCH,
  SUM(C3) as CELL_FACH,
  SUM(C4) as CELL_PCH

  FROM STATDBA.OSS_HUA_RRC_STATUS
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  AND (OBJ LIKE 'cellcontext')
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,sub("cellcontext",cellcontext,qry))))
  }             
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8,qry9,qry10,qry11,qry12,qry13,qry14)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 14
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    #Connecting to database through RODBC
    con <- odbcConnect('SIGESTOP',uid="felix",pwd="F3l!x2k10")
    options(dec=",")
    #Test connection
    #odbcGetInfo(ch)
    result<- sqlQuery(con, qrys[i])
    result$EXCHANGE_ID <- as.numeric(as.character(result$EXCHANGE_ID))
    #result<-sqlQuery(ch,paste('SELECT * FROM ',databases[i]))  
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datahua <- outputPar[[1]]
  for (i in 2:length(outputPar)){
    a<-i
    datahua <- datahua %>% full_join(outputPar[[i]])
  }
  
  
  #datahua <- datahua %>% inner_join(datos) %>%group_by(CELDA)
  datahua[is.na(datahua)] <- 0
  datahua<- as.data.frame(datahua)
  
  datahua$CONG_PS_RAB <- as.numeric(as.character(datahua$CONG_PS_RAB))
  datahua$TRAFICO_CS <- as.numeric(as.character(datahua$TRAFICO_CS))
  datahua$MAX_TROUGHPUT_HSDPA <- as.numeric(as.character(datahua$MAX_TROUGHPUT_HSDPA))
  datahua$AVG_TROUGHPUT_HSDPA <- as.numeric(as.character(datahua$AVG_TROUGHPUT_HSDPA))
  datahua$TOTAL_VOLUMEN_HSDPA <- as.numeric(as.character(datahua$TOTAL_VOLUMEN_HSDPA))
  datahua$TOTAL_VOLUMEN_HSUPA <- as.numeric(as.character(datahua$TOTAL_VOLUMEN_HSUPA))
  datahua$MAX_TROUGHPUT_HSUPA <- as.numeric(as.character(datahua$MAX_TROUGHPUT_HSUPA))
  datahua$AVG_TROUGHPUT_HSUPA <- as.numeric(as.character(datahua$AVG_TROUGHPUT_HSUPA))
  datahua$SUM_USER_DL <- as.numeric(as.character(datahua$SUM_USER_DL))
  datahua$MAX_USER_DL <- as.numeric(as.character(datahua$MAX_USER_DL))
  datahua$SUM_USER_UL <- as.numeric(as.character(datahua$SUM_USER_UL))
  datahua$MAX_USER_UL <- as.numeric(as.character(datahua$MAX_USER_UL))
  datahua$RTWP <- as.numeric(as.character(datahua$RTWP))
  
  datahua$CELL_DCH <- as.numeric(as.character(datahua$CELL_DCH))
  datahua$CELL_PCH <- as.numeric(as.character(datahua$CELL_PCH))
  datahua$CELL_FACH <- as.numeric(as.character(datahua$CELL_FACH))
  
  qry <- "SELECT * FROM EXCHANGE_ID "
  con <- odbcConnect('SIGESTOP',uid="felix",pwd="F3l!x2k10")
  options(dec=",")
  EXCHANGE_ID <- sqlQuery(con, qry)
  odbcClose(con)
  
  datahua <- datahua %>% inner_join(EXCHANGE_ID %>% select(ID,CODE) , by = c("EXCHANGE_ID" = "CODE")) %>%
    mutate(RNC=ID)
 
  return(datahua)
  
}

queryData2GEricsson <- function (idate,fdate,cellcontext,granul){
  
  con <- odbcConnect('dwhdb',uid = 'dcbo' , pwd = 'dcbo',believeNRows= FALSE)
  options(dec =",")
  
  qry<- "
  SELECT  DATEFORMAT(DATETIME_ID,'granul')  AS DIA, CELL_NAME as CELDA, BSC,
  
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
  MAX(CELLPAG_PAGETOOOLD) as PAGETOOOLD,
  MAX(CELLPAG_PAGPCHCONG) as PAGPCHCONG,
  MAX(CELLPAG_PAGESRECCS) as PAGESRECCS,
  MAX(CELLPAG_PAGESRECPS) as PAGESRECPS
  
  
  FROM dc.DC_E_BSS_CELL_CS_RAW
  
  WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
  AND (CELL_NAME LIKE 'cellcontext')
  GROUP BY DATEFORMAT(DATETIME_ID,'granul') ,BSC, CELL_NAME
  ORDER BY DATEFORMAT(DATETIME_ID,'granul') ,BSC, CELL_NAME"
  
  dataEri <- sqlQuery(con, gsub("granul",granul,sub("cellcontext",cellcontext,sub("fdate",fdate,sub("idate",idate,qry)))))
  dataEri[is.na(dataEri)] <- 0
  dataEri$TFTRALACC = ifelse(dataEri$TFTRALACC == 0, 1, dataEri$TFTRALACC)
  dataEri$TFNSCAN = ifelse(dataEri$TFNSCAN == 0, 1 , dataEri$TFNSCAN)
  dataEri$THTRALACC = ifelse(dataEri$THTRALACC == 0, 1, dataEri$THTRALACC)
  dataEri$THNSCAN = ifelse(dataEri$THNSCAN == 0, 1 , dataEri$THNSCAN)
  dataEri$CTRALACC = ifelse(dataEri$CTRALACC == 0, 1 , dataEri$CTRALACC)
  dataEri$CNSCAN = ifelse(dataEri$CNSCAN == 0, 1 , dataEri$CNSCAN)
  
  odbcClose(con)
  #dataEri <- dataEri %>% inner_join(datos)
  
  return(dataEri)
}

queryData3GEricsson <- function (idate,fdate,cellcontext,granul){
  
  con <- odbcConnect('dwhdb',uid = 'dcbo' , pwd = 'dcbo',believeNRows= FALSE)
  options(dec =",")
  
  qry<- "
  SELECT  DATEFORMAT(DATETIME_ID,'granul') AS DIA, UtranCell as CELDA, RNC,
  
  --PS DROP---
  
  SUM(pmNoNormalRabReleasePacket) as pmNoNormalRabReleasePacket,
  
  --CS DROP---
  SUM(pmNoSystemRabReleaseSpeech ) as pmNoSystemRabReleaseSpeech,
  SUM(pmNoSystemRabReleaseCs64) as pmNoSystemRabReleaseCs64,
  SUM(pmNoNormalRabReleaseSpeech) as pmNoNormalRabReleaseSpeech,
  SUM(pmNoNormalRabReleaseCs64) as pmNoNormalRabReleaseCs64,
  
  ---PS_RRC_Rate---
  SUM(pmTotNoRrcConnectReqPsSucc) as pmTotNoRrcConnectReqPsSucc,
  SUM(pmTotNoRrcConnectReqPs) as pmTotNoRrcConnectReqPs,
  SUM(pmNoLoadSharingRrcConnPs) as pmNoLoadSharingRrcConnPs,
  
  ---PS_RAB_Rate---
  SUM(pmNoRabEstablishSuccessPacketInteractive) as pmNoRabEstablishSuccessPacketInteractive,
  SUM(pmNoRabEstablishAttemptPacketInteractive) as pmNoRabEstablishAttemptPacketInteractive,
  
  
  ---CS_RRC_Rate---
  SUM(pmTotNoRrcConnectReqCsSucc) as pmTotNoRrcConnectReqCsSucc,
  SUM(pmTotNoRrcConnectReqCs) as pmTotNoRrcConnectReqCs,
  SUM(pmNoLoadSharingRrcConnCs) as pmNoLoadSharingRrcConnCs,
  
  ---CS_RAB_Rate---
  SUM(pmNoRabEstablishSuccessSpeech) as pmNoRabEstablishSuccessSpeech,
  SUM(pmNoRabEstablishSuccessCs64) as pmNoRabEstablishSuccessCs64,
  SUM(pmRabEstablishEcSuccess) as pmRabEstablishEcSuccess,
  SUM(pmRabEstablishEcAttempt) as pmRabEstablishEcAttempt,
  SUM(pmNoRabEstablishAttemptSpeech) as pmNoRabEstablishAttemptSpeech,
  SUM(pmNoRabEstablishAttemptCs64) as pmNoRabEstablishAttemptCs64,
  SUM(pmNoDirRetryAtt) as pmNoDirRetryAtt,
  
  ---Congestion_PWR---
  SUM(pmNoRrcReqDeniedAdmDlPwr) as pmNoRrcReqDeniedAdmDlPwr,
  SUM(pmNoFailedRabEstAttemptLackDlPwr) as pmNoFailedRabEstAttemptLackDlPwr,
  
  ---Congestion_CODES---
  SUM(pmNoRrcReqDeniedAdmDlChnlCode) as pmNoRrcReqDeniedAdmDlChnlCode,
  SUM(pmNoFailedRabEstAttemptLackDlChnlCode) as pmNoFailedRabEstAttemptLackDlChnlCode,
  
  ---Congestion_CE---
  SUM(pmNoFailedRabEstAttemptLackDlHw) as pmNoFailedRabEstAttemptLackDlHw,
  SUM(pmNoFailedRabEstAttemptLackUlHw) as pmNoFailedRabEstAttemptLackUlHw,
  SUM(pmNoRrcReqDeniedAdmDlHw) as pmNoRrcReqDeniedAdmDlHw,
  SUM(pmNoRrcReqDeniedAdmUlHw) as pmNoRrcReqDeniedAdmUlHw,
  
  ---Total Eventos Congestion---
  ---SUM(pmTotNoRrcConnectReq) as RRCAtt,
  ---SUM(pmNoRabEstablishAttemptSpeech) as RABSpeechAtt,
  ---SUM(pmNoRabEstablishAttemptPacketStream) as RABPckStrAtt,
  ---SUM(pmNoRabEstablishAttemptPacketStream128) as RABPckStr128Att,
  SUM(pmDlTrafficVolumePsIntHs) as pmDlTrafficVolumePsIntHs,
  SUM(pmUlTrafficVolumePsIntEul) as pmUlTrafficVolumePsIntEul,
  SUM(pmUlTrafficVolumePs384+pmUlTrafficVolumePs8+pmUlTrafficVolumePs16+pmUlTrafficVolumePs64+pmUlTrafficVolumePs128) as VOLUMEN_R99_UL,
  SUM(pmDlTrafficVolumePs64+pmDlTrafficVolumePs128+pmDlTrafficVolumePs384+pmDlTrafficVolumePs8+pmDlTrafficVolumePs16) as VOLUMEN_R99_DL,
  
  ---Paging---
  SUM(pmNoPagingAttemptUtranRejected) as pmNoPagingAttemptUtranRejected,
  SUM(pmNoPagingType1Attempt) as pmNoPagingType1Attempt,
  
  ---RSSI---
  SUM(pmSumUlRssi) as pmSumUlRssi,
  SUM(pmSamplesUlRssi) as pmSamplesUlRssi,
  SUM(pmSumBestCs12Establish)  as pmSumBestCs12Establish, 
  SUM(pmSamplesBestCs12Establish) as pmSamplesBestCs12Establish,
  
  ---PS DROP-----
  
  
  SUM(pmNoSystemRbReleaseHs) as   pmNoSystemRbReleaseHs,
  SUM(pmNoSystemRbReleaseEul) as   pmNoSystemRbReleaseEul,

  SUM(pmNoSystemRabReleasePacket) as pmNoSystemRabReleasePacket,
  ---VER
  SUM(pmNoSystemRabReleasePacketUra) as pmNoSystemRabReleasePacketUra,
  SUM(pmChSwitchAttemptFachUra) as pmChSwitchAttemptFachUra,
  SUM(pmChSwitchSuccFachUra) as pmChSwitchSuccFachUra,
  SUM(pmChSwitchAttemptDchUra) as pmChSwitchAttemptDchUra,
  SUM(pmChSwitchSuccDchUra) as pmChSwitchSuccDchUra,
  SUM(pmChSwitchAttemptHsUra) as pmChSwitchAttemptHsUra,
  SUM(pmChSwitchSuccHsUra) as pmChSwitchSuccHsUra,
  SUM(pmNoNormalRabReleasePacketUra) as pmNoNormalRabReleasePacketUra,
  ---Erlangs PS----
  SUM(pmSumBestDchPsIntRabEstablish) as pmSumBestDchPsIntRabEstablish,
  SUM(pmSumFachPsIntRabEstablish) as pmSumFachPsIntRabEstablish,
  SUM(pmSumBestPsHsAdchRabEstablish) as pmSumBestPsHsAdchRabEstablish,
  SUM(pmSumBestPsEulRabEstablish) as pmSumBestPsEulRabEstablish,
  SUM(pmCellDowntimeAuto) as pmCellDowntimeAuto,
  SUM(pmCellDowntimeMan) as pmCellDowntimeMan

  FROM dc.DC_E_RAN_UCELL_RAW
  
  WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
  AND (UtranCell LIKE 'cellcontext')
  GROUP BY DATEFORMAT(DATETIME_ID,'granul'),RNC, CELDA
  ORDER BY DATEFORMAT(DATETIME_ID,'granul'),RNC, CELDA
  "
  
  dataEri <- sqlQuery(con, gsub("granul",granul,sub("cellcontext",cellcontext,sub("fdate",fdate,sub("idate",idate,qry)))))
  odbcClose(con)
  
  
  
  #dataEri$pmDlTrafficVolumePsIntHs = ifelse(dataEri$pmDlTrafficVolumePsIntHs == 0, 1, dataEri$pmDlTrafficVolumePsIntHs)
  dataEri$pmUlTrafficVolumePsIntEul = ifelse(dataEri$pmUlTrafficVolumePsIntEul == 0, 1 , dataEri$pmUlTrafficVolumePsIntEul)
  dataEri$VOLUMEN_R99_UL = ifelse(dataEri$VOLUMEN_R99_UL == 0, 1, dataEri$VOLUMEN_R99_UL)
  dataEri$VOLUMEN_R99_DL = ifelse(dataEri$VOLUMEN_R99_DL == 0, 1 , dataEri$VOLUMEN_R99_DL)
  dataEri$pmSamplesBestCs12Establish <- as.numeric(as.character(dataEri$pmSamplesBestCs12Establish))
  dataEri$pmSumBestCs12Establish <- as.numeric(as.character(dataEri$pmSumBestCs12Establish))
  dataEri$pmCellDowntimeAuto <- as.numeric(as.character(dataEri$pmCellDowntimeAuto))
  dataEri$pmCellDowntimeMan <- as.numeric(as.character(dataEri$pmCellDowntimeMan))
  
  dataEri$pmSumUlRssi <- as.numeric(as.character(dataEri$pmSumUlRssi))
  dataEri$pmSamplesUlRssi <- as.numeric(as.character(dataEri$pmSamplesUlRssi))
  
  dataEri[is.na(dataEri)] <- 0
  #dataEri <- dataEri %>% inner_join(datos)
  
  return(dataEri)
}

queryData2GNokia <- function(idate,fdate,granul,cellcontext){
  
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
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME
  "
  
  qry2 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  ----ACC----
  SUM(SDCCH_ALLOC_FOR_VOICE_CALL) AS SDCCH_ALLOC_FOR_VOICE_CALL,
  SUM(RES_AV_DENOM15) AS RES_AV_DENOM15,
  SUM(AVE_BUSY_TCH) AS AVE_BUSY_TCH,
  SUM(AVE_BUSY_SDCCH) AS AVE_BUSY_SDCCH,
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
  AND (CO_NAME LIKE 'cellcontext')
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
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry4 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(CELL_TCH_TCH) AS CELL_TCH_TCH
  
  from BSC_PS_HO_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry5 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(DELETE_PAGING_COMMAND) AS DELETE_PAGING_COMMAND
  
  from BSC_PS_RESACC_TTP2_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
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
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry7 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(UL_RLC_BLOCKS_IN_ACK_MODE + UL_RLC_BLOCKS_IN_UNACK_MODE) as  UL_RLC_BLOCK,
  SUM(DL_RLC_BLOCKS_IN_ACK_MODE + DL_RLC_BLOCKS_IN_UNACK_MODE) as  DL_RLC_BLOCK
 
  from BSC_PS_CODINGSC_CS1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"

  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,sub("cellcontext",cellcontext,qry))))
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
  
  datanok$AVE_TCH_BUSY_HALF <- as.numeric(as.character(datanok$AVE_TCH_BUSY_HALF))
  datanok$AVE_TCH_BUSY_FULL <- as.numeric(as.character(datanok$AVE_TCH_BUSY_FULL))
  
  datanok <- filter(datanok, !is.na(CELDA))
  
  datanok$BSC <- "BSC_NOK"
  
  datanok[is.na(datanok)] <- 0
  
  return(datanok)
}

queryData3GNokia <- function(idate,fdate,granul,cellcontext){
  
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
  sum(RAB_STP_FAIL_PS_STREA_BTS) as RAB_STP_FAIL_PS_STREA_BTS

  from NOKRWW_PS_SERVLEV_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD')) 
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
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

  sum(SUCC_FACH_TO_HS_DSCH) as SUCC_FACH_TO_HS_DSCH,

  sum(ATT_DCH_TO_PCH) as ATT_DCH_TO_PCH,

  sum(ATT_PCH_TO_DCH) as ATT_PCH_TO_DCH,

  sum(ATT_FACH_TO_PCH) as ATT_FACH_TO_PCH,

  sum(ATT_PCH_TO_FACH) as ATT_PCH_TO_FACH,

  sum(ATT_FACH_TO_HS_DSCH) as ATT_FACH_TO_HS_DSCH

  from NOKRWW_PS_RRC_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
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
  SUM(PS_SETUP_FAIL_AC_INT) as PS_SETUP_FAIL_AC_INT,
  SUM(PS_SETUP_FAIL_AC_BGR) as PS_SETUP_FAIL_AC_BGR
  

  from NOKRWW_PS_PKTCALL_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
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

  sum(dl_dch_sel_hsdpa_power_int + dl_dch_sel_hsdpa_power_bgr) as HSDPA_PWR,
 
  
  ---HSDPA ACC---
  SUM(allo_success_edch_int + allo_success_edch_bgr + allo_success_edch_str + edch_allo_canc_na_as_bgr + edch_allo_canc_na_as_int + edch_allo_canc_na_as_str 
  + ul_dch_sel_max_hsupa_usr_bgr + ul_dch_sel_max_hsupa_usr_int + ul_dch_sel_max_hsupa_usr_str + ul_dch_sel_bts_hw_int + ul_dch_sel_bts_hw_bgr + ul_dch_sel_bts_hw_str 
  + setup_fail_edch_bts_bgr + setup_fail_edch_bts_int + setup_fail_edch_bts_str + setup_fail_edch_other_bgr + setup_fail_edch_other_int + setup_fail_edch_other_str 
  + setup_fail_edch_trans_bgr + setup_fail_edch_trans_int + setup_fail_edch_trans_str + setup_fail_edch_ue_bgr + setup_fail_edch_ue_int + setup_fail_edch_ue_str 
  + setup_rej_edch_ac_int + setup_rej_edch_ac_bgr) as HSUPA_TO_DCH_DEN,

  SUM(allo_hs_dsch_flow_int + allo_hs_dsch_flow_bgr + rej_hs_dsch_ret_int + rej_hs_dsch_ret_bgr + setup_fail_rnc_hs_dsch_int + setup_fail_bts_hs_dsch_int + setup_fail_iub_hs_total_int +
  setup_fail_rnc_hs_dsch_bgr + setup_fail_bts_hs_dsch_bgr + setup_fail_iub_hs_total_bgr + setup_fail_ue_hs_dsch_int + setup_fail_ue_hs_dsch_bgr + dch_sel_max_hsdpa_users_int + dch_sel_max_hsdpa_users_bgr) as DEN_HSDPA_ACC,
  sum(allo_hs_dsch_flow_int + allo_hs_dsch_flow_bgr)  as NUM_HSDPA_ACC,
  
  ---HSUPA ACC---
  SUM(allo_success_edch_int + allo_success_edch_bgr + edch_allo_canc_na_as_bgr + edch_allo_canc_na_as_int 
  + ul_dch_sel_max_hsupa_usr_bgr + ul_dch_sel_max_hsupa_usr_int + ul_dch_sel_bts_hw_int + ul_dch_sel_bts_hw_bgr  + setup_fail_edch_bts_bgr + setup_fail_edch_bts_int + setup_fail_edch_other_bgr + setup_fail_edch_other_int 
  + setup_fail_edch_trans_bgr + setup_fail_edch_trans_int + setup_fail_edch_ue_bgr + setup_fail_edch_ue_int + setup_rej_edch_ac_int + setup_rej_edch_ac_bgr) as DEN_HSUPA_ACC,
  SUM(allo_success_edch_int + allo_success_edch_bgr) as NUM_HSUPA_ACC,
  
  SUM(MEH_REJECT_HSDSCH) as MEH_REJECT_HSDSCH,
  SUM(MEH_ADMIT_CS_VOICE) as MEH_ADMIT_CS_VOICE,
  SUM(SETUP_FAIL_IUB_HS_TOTAL_INT) as SETUP_FAIL_IUB_HS_TOTAL_INT,
  SUM(SETUP_FAIL_IUB_HS_TOTAL_BGR) as SETUP_FAIL_IUB_HS_TOTAL_BGR ,
 
  SUM(REJ_HS_DSCH_RET_INT) as REJ_HS_DSCH_RET_INT,
  SUM(REJ_HS_DSCH_RET_BGR) as REJ_HS_DSCH_RET_BGR ,

  SUM(SETUP_REJ_EDCH_AC_INT) as SETUP_REJ_EDCH_AC_INT,
  SUM(SETUP_REJ_EDCH_AC_BGR) as SETUP_REJ_EDCH_AC_BGR 

 
  from NOKRWW_PS_TRAFFIC_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
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
  SUM(CE_SAMPLE_AMOUNT) as CE_SAMPLE_AMOUNT,
  
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
  sum(HSPA_RT_O_NRT_DUE_SC) as HSPA_RT_O_NRT_DUE_SC,
  AVG(AVE_SCCPCH_INC_PCH_LOAD) as AVE_SCCPCH_INC_PCH_LOAD,
  AVG(SCCPCH_LOAD_DENOM_0) as SCCPCH_LOAD_DENOM_0,
  AVG(FACH_U_DATA_TPUT_DENOM_1) as FACH_U_DATA_TPUT_DENOM_1,
  AVG(AVE_FACH_UDATA_TP_SCCPCH) as AVE_FACH_UDATA_TP_SCCPCH,
  AVG(AVE_FACH_USER_TOT_TPUT) as AVE_FACH_USER_TOT_TPUT,
  AVG(FACH_USER_TOT_TPUT_DENOM_1) as FACH_USER_TOT_TPUT_DENOM_1,
  AVG(AVE_RACH_DATA_THROUGHPUT) as AVE_RACH_DATA_THROUGHPUT,
  AVG(RACH_DENOM_3) as RACH_DENOM_3,
  AVG(AVE_RACH_THROUGHPUT) as AVE_RACH_THROUGHPUT,
  AVG(RACH_DENOM_4) as RACH_DENOM_4,
  AVG(CODE_CAPACITY) as CODE_CAPACITY,
  AVG(DENOM_CODE_CAPACITY) as DENOM_CODE_CAPACITY,
  MIN(MIN_CODE_OCCUPANCY_PERCENT) as MIN_CODE_OCCUPANCY_PERCENT,
  MAX(MAX_CODE_OCCUPANCE_PERCENT) as MAX_CODE_OCCUPANCE_PERCENT,
  SUM(NBR_SUCC_CODE_TREE_ALLO) as NBR_SUCC_CODE_TREE_ALLO ,
  SUM(CHAN_CODE_SF4_REQUEST) as CHAN_CODE_SF4_REQUEST,
  SUM(CHAN_CODE_SF8_REQUEST) as CHAN_CODE_SF8_REQUEST,
  SUM(CHAN_CODE_SF16_REQUEST) as CHAN_CODE_SF16_REQUEST,
  SUM(CHAN_CODE_SF32_REQUEST) as CHAN_CODE_SF32_REQUEST,
  SUM(CHAN_CODE_SF64_REQUEST) as CHAN_CODE_SF64_REQUEST,
  SUM(CHAN_CODE_SF128_REQUEST) as CHAN_CODE_SF128_REQUEST,
  SUM(CHAN_CODE_SF256_REQUEST) as CHAN_CODE_SF256_REQUEST
  
  from NOKRWW_PS_CELLRES_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
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
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry7 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(HSDPA_ORIG_DATA) as HSDPA_ORIG_DATA
 
  from NOKRWW_PS_CELTPW_MNC1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  
  qry8 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(HSDPA_BUFF_WITH_DATA_PER_TTI) as HSDPA_BUFF_WITH_DATA_PER_TTI,
  SUM(MACE_PDU_DATA_2MS_TTI) as MACE_PDU_DATA_2MS_TTI,
  SUM(MACE_PDU_DATA_10MS_TTI) as MACE_PDU_DATA_10MS_TTI,
  SUM(MACE_PDUS_2MS_TTI) as MACE_PDUS_2MS_TTI,
  SUM(MACE_PDUS_10MS_TTI) as MACE_PDUS_10MS_TTI,
  AVG(EDCH_16QAM_UE_ACT_SUM) as EDCH_16QAM_UE_ACT_SUM,
  SUM(SUM_HSUPA_USERS_2MS_TTI) as SUM_HSUPA_USERS_2MS_TTI ,
  SUM(SUM_HSUPA_USERS_10MS_TTI) as SUM_HSUPA_USERS_10MS_TTI,
  SUM (DENOM_HSUPA_USERS_10MS_TTI) as DENOM_HSUPA_USERS_10MS_TTI,
  sum(DENOM_HSUPA_USERS_2MS_TTI ) as DENOM_HSUPA_USERS_2MS_TTI


  from NOKRWW_PS_HSDPAW_MNC1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
 
  qry9 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(SUCC_UPDATES_ON_SHO_FOR_RT) as SUCC_UPDATES_ON_SHO_FOR_RT,
  SUM(SUCC_UPDATES_ON_SHO_FOR_NRT) as SUCC_UPDATES_ON_SHO_FOR_NRT,
  SUM(CELL_ADD_REQ_ON_SHO_FOR_RT) as  CELL_ADD_REQ_ON_SHO_FOR_RT,
  SUM(CELL_REPL_REQ_ON_SHO_FOR_RT) as CELL_REPL_REQ_ON_SHO_FOR_RT,
  SUM(CELL_DEL_REQ_ON_SHO_FOR_RT) as CELL_DEL_REQ_ON_SHO_FOR_RT,
  SUM(CELL_ADD_REQ_ON_SHO_FOR_NRT) as CELL_ADD_REQ_ON_SHO_FOR_NRT,
  SUM(CELL_REPL_REQ_ON_SHO_FOR_NRT) as CELL_REPL_REQ_ON_SHO_FOR_NRT,
  SUM(CELL_DEL_REQ_ON_SHO_FOR_NRT) as CELL_DEL_REQ_ON_SHO_FOR_NRT,
  SUM(UNSUCC_UPDATES_ON_SHO_NRT) as UNSUCC_UPDATES_ON_SHO_NRT,
  SUM(UNSUCC_UPDATES_ON_SHO_FOR_RT) as UNSUCC_UPDATES_ON_SHO_FOR_RT,
  SUM(SHO_SUCC_CS_PS_ACT_MRAB) as SHO_SUCC_CS_PS_ACT_MRAB,
  SUM(SHO_FAIL_CS_PS_ACT_MRAB ) as SHO_FAIL_CS_PS_ACT_MRAB 
  
  
  
  from NOKRWW_PS_SOFTHO_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry10 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  SUM(IFHO_SUCC_CS_PS_ACT_MRAB ) as IFHO_SUCC_CS_PS_ACT_MRAB ,
  SUM(IFHO_FAIL_CS_PS_ACT_MRAB ) as IFHO_FAIL_CS_PS_ACT_MRAB
  from NOKRWW_PS_INTSYSHO_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry11 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  SUM(ISHO_SUCC_CS_PS_ACT_MRAB  ) as ISHO_SUCC_CS_PS_ACT_MRAB,
  SUM(ISHO_FAIL_CS_PS_ACT_MRAB ) as ISHO_FAIL_CS_PS_ACT_MRAB

  from NOKRWW_PS_INTERSHO_MNC_RAW ,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  
  #qry12 <- "select to_char(period_start_time, 'granul') as DIA,
  #(CO_NAME) as CELDA,
  #SUM(SHO_ADJ_INTRA_FREQ_SHO_ATT) as SHO_ADJ_INTRA_FREQ_SHO_ATT,
  #SUM(SHO_ADJ_INTRA_FREQ_SHO_COMPL) as SHO_ADJ_INTRA_FREQ_SHO_COMPL,
  #SUM(CPICH_ECNO_SHO_SUM) as CPICH_ECNO_SHO_SUM,
  #SUM(CPICH_ECNO_SHO_DENOM) as CPICH_ECNO_SHO_DENOM,
  #SUM(CPICH_RSCP_SHO_SUM) as CPICH_RSCP_SHO_SUM,
  #SUM(CPICH_RSCP_SHO_DENOM) as CPICH_RSCP_SHO_DENOM
#
  #from NOKRWW_PS_AUTSH2_DMNC_RAW ,UTP_COMMON_OBJECTS
  #WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  #AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  #AND (wcel_id = CO_GID) 
  #AND (CO_NAME LIKE 'cellcontext')
  #GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  #
  #
  #qry13 <- "select to_char(period_start_time, 'granul') as DIA,
  #(CO_NAME) as CELDA,
  #SUM(HHO_ATT_CAUSED_SHO_INCAP_RT + IMMED_HHO_CSD_SHO_INCAP_RT + HHO_ATT_CAUSED_SHO_INCAP_NRT 
  #+ IMMED_HHO_CSD_SHO_INCAP_NRT + INTRA_INTRA_HHO_ATT_RT + INTRA_INTER_HHO_ATT_RT + INTER_HHO_ATT_RT + INTRA_INTRA_HHO_ATT_NRT + INTRA_INTER_HHO_ATT_NRT + INTER_HHO_ATT_NRT) as IFHO_ATT,
#
  #SUM (SUCC_HHO_CAUSED_SHO_INCAP_RT + SUCC_HHO_SHO_INCAP_NRT + SUCC_INTRA_INTRA_HHO_ATT_RT + SUCC_INTRA_INTER_HHO_ATT_RT 
  #+ SUCC_INTER_HHO_ATT_RT + SUCC_INTRA_INTRA_HHO_ATT_NRT + SUCC_INTRA_INTER_HHO_ATT_NRT + SUCC_INTER_HHO_ATT_NRT) as IFHO_SUCC
  #
#
  #from NOKRWW_PS_INTSYSHO_MNC_RAW ,UTP_COMMON_OBJECTS
  #WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  #AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  #AND (wcel_id = CO_GID) 
  #AND (CO_NAME LIKE 'cellcontext')
  #GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  #
  #qry14 <- "select to_char(period_start_time, 'granul') as DIA,
  #(CO_NAME) as CELDA,
  #SUM(CPICH_ECNO_IFHO_SUM) as CPICH_ECNO_IFHO_SUM,
  #SUM(CPICH_ECNO_IFHO_DENOM) as CPICH_ECNO_IFHO_DENOM,
  #SUM(CPICH_RSCP_IFHO_SUM) as CPICH_RSCP_IFHO_SUM,
  #SUM(CPICH_RSCP_IFHO_DENOM) as CPICH_RSCP_IFHO_DENOM
  #
  #from NOKRWW_PS_AUTIF2_DMNC_RAW,UTP_COMMON_OBJECTS
  #WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  #AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  #AND (wcel_id = CO_GID) 
  #AND (CO_NAME LIKE 'cellcontext')
  #GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  #

  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,sub("cellcontext",cellcontext,qry))))
  } 
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8,qry9,qry10,qry11)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 11
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
  
  datanok <- filter(datanok, CELDA != "Nokia31")
 
  datanok$CELDA <- substr(datanok$CELDA,1,7)
  
  datanok$AVE_SCCPCH_INC_PCH_LOAD  <- as.numeric(as.character(datanok$AVE_SCCPCH_INC_PCH_LOAD) )
  datanok$SCCPCH_LOAD_DENOM_0      <- as.numeric(as.character(datanok$SCCPCH_LOAD_DENOM_0 ) )
  datanok$FACH_U_DATA_TPUT_DENOM_1 <- as.numeric(as.character(datanok$FACH_U_DATA_TPUT_DENOM_1 ) )
  datanok$AVE_FACH_UDATA_TP_SCCPCH <- as.numeric(as.character(datanok$AVE_FACH_UDATA_TP_SCCPCH) )
  datanok$AVE_FACH_USER_TOT_TPUT      <- as.numeric(as.character(datanok$AVE_FACH_USER_TOT_TPUT) )
  datanok$AVE_FACH_USER_TOT_TPUT  <- as.numeric(as.character(datanok$AVE_FACH_USER_TOT_TPUT) )
  datanok$FACH_USER_TOT_TPUT_DENOM_1 <- as.numeric(as.character(datanok$FACH_USER_TOT_TPUT_DENOM_1) )
  datanok$AVE_RACH_DATA_THROUGHPUT <- as.numeric(as.character(datanok$AVE_RACH_DATA_THROUGHPUT) )
  datanok$RACH_DENOM_3 <- as.numeric(as.character(datanok$RACH_DENOM_3) )
  datanok$AVE_RACH_THROUGHPUT <- as.numeric(as.character(datanok$AVE_RACH_THROUGHPUT) )
  datanok$RACH_DENOM_4  <- as.numeric(as.character(datanok$RACH_DENOM_4) )
  datanok$CODE_CAPACITY <- as.numeric(as.character(datanok$CODE_CAPACITY) )
  datanok$DENOM_CODE_CAPACITY <- as.numeric(as.character(datanok$DENOM_CODE_CAPACITY) )
  datanok$MIN_CODE_OCCUPANCY_PERCENT <- as.numeric(as.character(datanok$MIN_CODE_OCCUPANCY_PERCENT) )
  datanok$MAX_CODE_OCCUPANCE_PERCENT <- as.numeric(as.character(datanok$MAX_CODE_OCCUPANCE_PERCENT) )
  
  
  
  datanok$NRT_EDCH_UL_DATA_VOL <- as.numeric(as.character(datanok$NRT_EDCH_UL_DATA_VOL))
  datanok$HS_DSCH_DATA_VOL <- as.numeric(as.character(datanok$HS_DSCH_DATA_VOL))
  datanok$SUM_HSUPA_USERS_2MS_TTI <- as.numeric(as.character(datanok$SUM_HSUPA_USERS_2MS_TTI))
  datanok$SUM_HSUPA_USERS_10MS_TTI <- as.numeric(as.character(datanok$SUM_HSUPA_USERS_10MS_TTI))
  datanok$DENOM_HSUPA_USERS_10MS_TTI <- as.numeric(as.character(datanok$DENOM_HSUPA_USERS_10MS_TTI))
  datanok$DENOM_HSUPA_USERS_2MS_TTI <- as.numeric(as.character(datanok$DENOM_HSUPA_USERS_2MS_TTI))
  datanok$EDCH_16QAM_UE_ACT_SUM <- as.numeric(as.character(datanok$EDCH_16QAM_UE_ACT_SUM))
  
  datanok$RNC <- "RNC_NOK"
  
  datanok <- filter(datanok,CELDA != "FCF10B1",CELDA != "FCF10C1",CELDA != "FCF10A1",CELDA != "FCF10D1" )
  
  str(datanok)
  
  datanok[is.na(datanok)] <- 0
  
  return(datanok)
}

queryDataBTS3GNokia <- function (idate,fdate,granul,cellcontext){
  
  if(granul == 'yyyy-mm-dd hh' ){
    granul <- 'YYYY-MM-DD HH24'
  }
  
  if(granul == 'yyyy-mm-dd' ){
    granul <- 'YYYY-MM-DD'
  }
  
  con <- odbcConnect('NOKIA',uid="pmr",pwd="pmr",believeNRows= FALSE)
  options(dec =",")
  
 qry <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as SITIO,
  MIN(MIN_USED_BB_SUBUNITS) as MIN_USED_BB_SUBUNITS ,
  MAX(MAX_HSUPA_SU_USG) as  MAX_HSUPA_SU_USG,
  AVG(AVG_HSUPA_SU_USG) as  AVG_HSUPA_SU_USG,
  MIN(MIN_HSUPA_SU_USG) as  MIN_HSUPA_SU_USG,
  AVG(AVG_USED_CE_DL_WBTS ) as AVG_USED_CE_DL_WBTS ,
  MAX(MAX_USED_CE_DL_WBTS ) as MAX_USED_CE_DL_WBTS ,
  AVG(AVG_USED_CE_UL_WBTS ) as AVG_USED_CE_UL_WBTS ,
  MAX(MAX_USED_CE_UL_WBTS ) as MAX_USED_CE_UL_WBTS ,
  MAX(CONF_PIC_SU_WBTS    ) as  CONF_PIC_SU_WBTS,
  AVG(AVG_USED_CE_R99_DL  ) as  AVG_USED_CE_R99_DL,
  MAX(MAX_USED_CE_R99_DL  ) as  MAX_USED_CE_R99_DL,
  AVG(AVG_USED_CE_R99_UL  ) as  AVG_USED_CE_R99_UL,
  MAX(MAX_USED_CE_R99_UL  ) as  MAX_USED_CE_R99_UL,
  AVG(AVG_BTS_HSUPA_USERS ) as AVG_BTS_HSUPA_USERS,
  MAX(MAX_BTS_HSUPA_USERS ) as MAX_BTS_HSUPA_USERS,
  AVG(AVG_BTS_HSDPA_USERS ) as AVG_BTS_HSDPA_USERS,
  MAX(MAX_BTS_HSDPA_USERS ) as MAX_BTS_HSDPA_USERS,
  AVG(AVG_USED_BB_SUBUNITS) as  AVG_USED_BB_SUBUNITS,
  MAX(MAX_USED_BB_SUBUNITS) as  MAX_USED_BB_SUBUNITS,
  MAX(NUM_BB_SUBUNITS     ) as NUM_BB_SUBUNITS,
  MAX(CONF_HSDPA_USERS    ) as CONF_HSDPA_USERS,
  MAX(CONF_HSUPA_USERS    ) as CONF_HSUPA_USERS,
  MAX(LICENSED_R99CE_WBTS ) as LICENSED_R99CE_WBTS
 

  from NOKRWW_PS_WBTSMON_WBTS_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wbts_id = CO_GID) 
  AND (CO_NAME LIKE 'cellcontext')
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME
 "
 
  dataNokBTS <- sqlQuery(con, gsub("granul",granul,sub("cellcontext",cellcontext,sub("fdate",fdate,sub("idate",idate,qry)))))
  dataNokBTS[is.na(dataNokBTS)] <- 0
  
  str(dataNokBTS)
  
  dataNokBTS$MIN_USED_BB_SUBUNITS <-as.numeric(as.character(dataNokBTS$MIN_USED_BB_SUBUNITS))
  dataNokBTS$MAX_HSUPA_SU_USG     <-as.numeric(as.character(dataNokBTS$MAX_HSUPA_SU_USG))
  dataNokBTS$AVG_HSUPA_SU_USG     <-as.numeric(as.character(dataNokBTS$AVG_HSUPA_SU_USG))
  dataNokBTS$MIN_HSUPA_SU_USG     <-as.numeric(as.character(dataNokBTS$MIN_HSUPA_SU_USG))
  dataNokBTS$AVG_USED_CE_DL_WBTS  <-as.numeric(as.character(dataNokBTS$AVG_USED_CE_DL_WBTS)) 
  dataNokBTS$MAX_USED_CE_DL_WBTS  <-as.numeric(as.character(dataNokBTS$MAX_USED_CE_DL_WBTS)) 
  dataNokBTS$AVG_USED_CE_UL_WBTS  <-as.numeric(as.character(dataNokBTS$AVG_USED_CE_UL_WBTS)) 
  dataNokBTS$MAX_USED_CE_UL_WBTS  <-as.numeric(as.character(dataNokBTS$MAX_USED_CE_UL_WBTS)) 
  dataNokBTS$CONF_PIC_SU_WBTS     <-as.numeric(as.character(dataNokBTS$CONF_PIC_SU_WBTS))
  dataNokBTS$AVG_USED_CE_R99_DL   <-as.numeric(as.character(dataNokBTS$AVG_USED_CE_R99_DL))
  dataNokBTS$MAX_USED_CE_R99_DL   <-as.numeric(as.character(dataNokBTS$MAX_USED_CE_R99_DL))
  dataNokBTS$AVG_USED_CE_R99_UL   <-as.numeric(as.character(dataNokBTS$AVG_USED_CE_R99_UL))
  dataNokBTS$MAX_USED_CE_R99_UL   <-as.numeric(as.character(dataNokBTS$MAX_USED_CE_R99_UL))
  dataNokBTS$AVG_BTS_HSUPA_USERS  <-as.numeric(as.character(dataNokBTS$AVG_BTS_HSUPA_USERS)) 
  dataNokBTS$MAX_BTS_HSUPA_USERS  <-as.numeric(as.character(dataNokBTS$MAX_BTS_HSUPA_USERS)) 
  dataNokBTS$AVG_BTS_HSDPA_USERS  <-as.numeric(as.character(dataNokBTS$AVG_BTS_HSDPA_USERS)) 
  dataNokBTS$MAX_BTS_HSDPA_USERS  <-as.numeric(as.character(dataNokBTS$MAX_BTS_HSDPA_USERS)) 
  dataNokBTS$AVG_USED_BB_SUBUNITS <-as.numeric(as.character(dataNokBTS$AVG_USED_BB_SUBUNITS))
  dataNokBTS$MAX_USED_BB_SUBUNITS <-as.numeric(as.character(dataNokBTS$MAX_USED_BB_SUBUNITS))
  dataNokBTS$NUM_BB_SUBUNITS      <-as.numeric(as.character(dataNokBTS$NUM_BB_SUBUNITS))
  dataNokBTS$CONF_HSDPA_USERS     <-as.numeric(as.character(dataNokBTS$CONF_HSDPA_USERS))
  dataNokBTS$CONF_HSUPA_USERS     <-as.numeric(as.character(dataNokBTS$CONF_HSUPA_USERS))
  dataNokBTS$LICENSED_R99CE_WBTS  <-as.numeric(as.character(dataNokBTS$LICENSED_R99CE_WBTS))
  
  
  
  return(dataNokBTS)
  
}

queryData4GEricsson <- function (idate,fdate,cellcontext,granul){
  con <- odbcConnect('dwhdb',uid = 'dcbo' , pwd = 'dcbo',believeNRows= FALSE)
  options(dec =",")
  gra <- granul
  
  qry<- "
  SELECT  DATEFORMAT(DATETIME_ID,'granul')  AS DIA, EUtranCellFDD as CELDA, ERBS as SITIO,
  
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
  max(pmActiveUeDlMax) as pmActiveUeDlMax,
  max(pmActiveUeUlMax) as pmActiveUeUlMax
  
  FROM dc.DC_E_ERBS_EUTRANCELLFDD_RAW
  
  WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
  AND (EUtranCellFDD LIKE 'cellcontext')
  GROUP BY DATEFORMAT(DATETIME_ID,'granul'),ERBS, CELDA
  ORDER BY DATEFORMAT(DATETIME_ID,'granul'),ERBS, CELDA
  "
  
  dataEri <- sqlQuery(con, gsub("granul",granul,sub("cellcontext",cellcontext,sub("fdate",fdate,sub("idate",idate,qry)))))
  odbcClose(con)
  
  dataEri$pmActiveUeDlMax <- as.numeric(as.character(dataEri$pmActiveUeDlMax))
  dataEri$pmActiveUeUlMax <- as.numeric(as.character(dataEri$pmActiveUeUlMax))
  
  dataEri[is.na(dataEri)] <- 0
  return(dataEri)
}

###FILTER#####
filter2GHua <- function(data,filter,cellfilter){
  if (is.null(data) | (dim(data)[1] == 0)) return(data.frame())
  if(cellfilter != ""){data <- filter(data,str_detect(CELDA,cellfilter))}
  
  switch (filter,
          "Total" = {data$TOTAL <- "Total"
          datosT <- data %>% group_by(DIA,TOTAL)
          },
          "ByProvince" = {datosT <- data %>% group_by(DIA, PROVINCIA = substr(CELDA,1,1))},  
          "BySite" = {datosT <- data %>% group_by(DIA,SITE = substr(CELDA,1,6))},
          "ByRNC" = {datosT <- data %>% group_by(DIA,BSC)},
          "ByCell" = {datosT <- data %>% group_by(DIA,CELDA)}
  )
  datosP <- datosT %>%summarise(
      ACC = round(100* as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                   *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                   *(sum(K3013A) / sum(K3010A))
      ),3),
      
      RET = round(100 * (as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))/
                           as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353))),3),
      
      SER = round( 100 * ((as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                       *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                       *(sum(K3013A) / sum(K3010A))
      ))
      *
        (as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))/
           as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353)))),3),
      CONG_TCH =   round(100 * as.numeric(sum(K3011A)/ sum(K3010A)),3),
      CONG_SDCCH =  round(100 * as.numeric(sum(K3001)/ sum(K3000)),3),
      TRAFF_TCH = round(as.numeric(sum(K3014)),3),
      TRAFF_SDCCH = round(as.numeric(sum(K3004)),3),
      CALL_DROP_TCH = sum(CM30),
      CALL_DROP_SDCCH = sum(CM33),
      PAGINGS = as.numeric(sum(A338)),
      PAGINGS_PS = as.numeric(sum(A340)),
      TRAFF_TCH_FR = as.numeric(sum(K3014MINUSK3034)) ,
      TRAFF_TCH_HR = as.numeric(sum(K3034))
      
    )
  
  datosP[is.na(datosP)] <- 0

  return(datosP)
  
}

filter3GHua <- function(data,filter,cellfilter,gra){
  if (is.null(data) | (dim(data)[1] == 0)) return(data.frame())
  
  if(cellfilter != ""){data <- filter(data,str_detect(CELDA,cellfilter))}
  
  if(gra == "yyyy-mm-dd"){
    seg=86400}
  else{ 
  seg=3600}
  
  switch (filter,
          "Total" = {data$TOTAL <- "Total"
          datosT <- data %>% group_by(DIA,TOTAL)
          },
                         
          "ByProvince" = {datosT <- data %>% group_by(DIA, PROVINCIA = substr(CELDA,1,1))},  
          "BySite" = {datosT <- data %>% group_by(DIA,SITE = substr(CELDA,1,6))},
          "ByCell" = {datosT <- data %>% group_by(DIA,CELDA)},
          "ByRNC" = {datosT <- data %>% group_by(DIA,RNC)}
  )
  datosP <- datosT %>%summarise(
    ACC_CS =  round( 100  *(as.numeric(as.numeric(sum(NUM_AMR_RAB)) * as.numeric(sum(NUM_CS_RRC)))  ) / 
                       (as.numeric(as.numeric(sum(DEN_CS_RRC)) * as.numeric(sum(DEN_AMR_RAB)))) ,3),
    
    ACC_PS = round( 100  *(as.numeric(as.numeric(sum(NUM_PS_RAB)) * as.numeric(sum(NUM_PS_RRC))))  / 
                      (as.numeric(as.numeric(sum(DEN_PS_RAB)) * as.numeric(sum(DEN_PS_RRC)))),3),
    
    RET_CS = 100 - round(100 * (as.numeric(as.numeric(sum(NUM_AMR_CALL_DROP)) / as.numeric(sum(DEN_AMR_CALL_DROP)))) ,3 ),
    
    RET_PS= 100 -  100 * round((as.numeric(as.numeric(sum(PS_DROP_FD_NUM))/as.numeric(sum(PS_DROP_FD_DEN)))),3),
    
    CONG_CE_RRC = round ( as.numeric(sum(CE_CONG_RRC) ),3),
    
    CONG_PWR_RRC = round ( as.numeric(sum(PWR_CONG_RRC)),3),
    
    CONG_CODE_RRC = round (as.numeric(sum(CODE_CONG_RRC)),3),
    
    CONG_CE_RAB = round ( as.numeric(sum(CE_PS_CONG_RAB) + sum(CE_CS_CONG_RAB)),3),
    #
    CONG_PWR_RAB = round (as.numeric(sum(PWR_PS_CONG_RAB) + sum(PWR_CS_CONG_RAB)),3),
    #
    CONG_CODE_RAB = round (as.numeric(sum(CODE_PS_CONG_RAB) + sum(CODE_CS_CONG_RAB)),3),
    
    MAX_TROUGHPUT_HSDPA = round(as.numeric(max(MAX_TROUGHPUT_HSDPA)),3),
    
    AVG_TROUGHPUT_HSDPA = round(as.numeric(mean(AVG_TROUGHPUT_HSDPA)),3),
    
    MAX_USER_HSDPA = round(as.numeric(sum(MAX_USER_HSDPA)),3),
    
    VOLUMEN_DATOS = round(as.numeric(sum(TOTAL_VOLUMEN_HSUPA) + sum(TOTAL_VOLUMEN_HSDPA)  )/1000000000,3),
    
    DROP_CS = as.numeric(sum(CS_DROP_A)),
    
    DROP_PS = as.numeric(sum(PS_DROP_A)),
    
    TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS)),3),
    
    RTWP = round(mean(as.numeric(RTWP),na.rm = TRUE),3),
    
    MAX_USER_HSUPA = round(as.numeric(sum(MAX_USER_HSUPA)),3),
    
    USER_DL = round(as.numeric(sum(SUM_USER_DL)),3),
    
    AVG_USER_DL = round(as.numeric(mean(MAX_USER_DL)),3),
    
    USER_UL = round(as.numeric(sum(SUM_USER_UL)),3),
    
    AVG_USER_UL = round(as.numeric(mean(MAX_USER_UL)),3),
    
    PAGING_DISCARD = round(as.numeric(sum(PAGING_DISCARD)),0),
    
    PAGING_SENT = round(as.numeric(sum(PAGING_SENT)),0),
    
    PAGING_EFI = 100 * round(as.numeric(sum(PAGING_DISCARD)/sum(PAGING_SENT)),2),
    
    ACC_RRC_CS =  round( 100  *(as.numeric( as.numeric(sum(NUM_CS_RRC)))  ) / 
                       (as.numeric(as.numeric(sum(DEN_CS_RRC)))) ,3),
    
    ACC_RAB_CS =  round( 100  *(as.numeric(as.numeric(sum(NUM_AMR_RAB)))) / 
                           (as.numeric(as.numeric(sum(DEN_AMR_RAB)))) ,3),
    
    ACC_RRC_PS = round( 100  *(as.numeric(as.numeric(sum(NUM_PS_RRC))))  / 
                      (as.numeric(as.numeric(sum(DEN_PS_RRC)))),3),
    
    ACC_RAB_PS = round( 100  *(as.numeric(as.numeric(sum(NUM_PS_RAB))))  / 
                          (as.numeric(as.numeric(sum(DEN_PS_RAB)))),3),
    
    VOLUMEN_HSDPA = round(as.numeric(sum(TOTAL_VOLUMEN_HSDPA))/1000000000,3),
    
    VOLUMEN_HSUPA = round(as.numeric(sum(TOTAL_VOLUMEN_HSUPA))/1000000000,3),
    
    CELL_DCH = as.numeric(sum(CELL_DCH)),
    
    CELL_PCH = as.numeric(sum(CELL_PCH)),
    
    CELL_FACH = as.numeric(sum(CELL_FACH)),
    
    PWR_DL_RAB = as.numeric(sum (PWR_CS_CONG_RAB_DL) + sum(PWR_PS_CONG_RAB_DL)),
    
    PWR_UL_RAB = as.numeric(sum (PWR_CS_CONG_RAB_UL) + sum(PWR_PS_CONG_RAB_UL)),
    
    RAB_PS_SUCC = as.numeric(sum(NUM_PS_RAB)),
    
    RAB_PS_ATTEMPS = as.numeric(sum(DEN_PS_RAB)),
    
    RAB_CS_SUCC = as.numeric(sum(NUM_AMR_RAB)),
    
    RAB_CS_ATTEMPS = as.numeric(sum(DEN_AMR_RAB)),
    
    MAX_TROUGHPUT_HSUPA = round(as.numeric(max(MAX_TROUGHPUT_HSUPA)),3),
    
    AVG_TROUGHPUT_HSUPA = round(as.numeric(mean(AVG_TROUGHPUT_HSUPA)),3),
    
    CPICH_PWR = round(as.numeric(mean(CPICH_PWR)),3),
    
    CPICH_PWR_RTWP = round(as.numeric(sum(CPICH_BY_RTWP)),3),
    
    LDR_PWR_DL = round(100 * as.numeric(sum(LDR_PWR_DL))/as.numeric(seg),3),
    
    LDR_PWR_UL = round(100 * as.numeric(sum(LDR_PWR_UL))/as.numeric(seg),3)
    
    #NUM_RAB = round(as.numeric(sum(NUM_PS_RAB)),3),
    #
    #DEN_RAB = round(as.numeric(sum(DEN_PS_RAB)),3),
    
  )
  datosP[is.na(datosP)] <- 0
  return (datosP)
}

filter2GEri <- function(data,filter,cellfilter,gra){
  if (is.null(data) | (dim(data)[1] == 0)) return(data.frame())
  if(cellfilter != ""){data <- filter(data,str_detect(CELDA,cellfilter))}
  if (gra == "yyyy-mm-dd") {data <- data %>% 
    mutate(TFNSCAN = TFNSCAN / 24,THNSCAN = THNSCAN / 24, CNSCAN = CNSCAN / 24)} 
  switch (filter,
          "Total" = {data$TOTAL <- "Total"
          datosT <- data %>% group_by(DIA,TOTAL)
          },
          "ByProvince" = {datosT <- data %>% group_by(DIA, PROVINCIA = substr(CELDA,1,1))},  
          "BySite" = {datosT <- data %>% group_by(DIA,SITE = substr(CELDA,1,6))},
          "ByRNC" = {datosT <- data %>% group_by(DIA,BSC)},
          "ByCell" = {datosT <- data %>% group_by(DIA,CELDA)}
  )
  datosP <- datosT %>%summarise(
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
    TRAFF_TCH = round(sum(as.numeric(TFTRALACC)/as.numeric(TFNSCAN) + as.numeric(THTRALACC)/as.numeric(THNSCAN)),2),
    TRAFF_SDCCH = round(sum(as.numeric(CTRALACC)/as.numeric(CNSCAN)),2),
    CALL_DROP_TCH = sum(DROP_TCH),
    CALL_DROP_SDCCH = sum(DROP_SDCCH)
  )
  datosP[is.na(datosP)] <- 0
  
  return (datosP)
}

filter3GEri <- function(data,filter,cellfilter,gra){
  if (is.null(data) | (dim(data)[1] == 0)) return(data.frame())
   if(gra == "yyyy-mm-dd"){
    seg <- 24 
    seg1=86400}
  else{seg <- 1 
  seg1=3600}
  data <- data %>% filter(pmSamplesBestCs12Establish > 0)
  if(cellfilter != ""){data <- filter(data,str_detect(CELDA,cellfilter))}
  switch (filter,
          "Total" = {data$TOTAL <- "Total"
          datosT <- data %>% group_by(DIA,TOTAL)
          },
          "ByProvince" = {datosT <- data %>% group_by(DIA, PROVINCIA = substr(CELDA,1,1))},  
          "BySite" = {datosT <- data %>% group_by(DIA,SITE = substr(CELDA,1,6))},
          "ByRNC" = {datosT <- data %>% group_by(DIA,RNC)},
          "ByCell" = {datosT <- data %>% group_by(DIA,CELDA)}
  )
  
  datosP <- datosT %>%summarise(
    ACC_CS = round(100 * as.numeric(as.numeric(sum(as.numeric(pmTotNoRrcConnectReqCsSucc)  )/(sum(as.numeric(pmTotNoRrcConnectReqCs)  )-sum(as.numeric(pmNoLoadSharingRrcConnCs) )))    *    as.numeric((sum(as.numeric(pmRabEstablishEcSuccess) )+sum(as.numeric(pmNoRabEstablishSuccessSpeech) )+sum(as.numeric(pmNoRabEstablishSuccessCs64) )) 
                                                                                                                   /(sum(as.numeric(pmRabEstablishEcAttempt) ) + sum(as.numeric(pmNoRabEstablishAttemptSpeech) ) + sum(as.numeric(pmNoRabEstablishAttemptCs64) ) - sum(as.numeric(pmNoDirRetryAtt) )))),3),
    
    ACC_PS = round(100 * as.numeric(as.numeric(sum(as.numeric(pmTotNoRrcConnectReqPsSucc) )/(sum(as.numeric(pmTotNoRrcConnectReqPs) ) - sum(as.numeric(pmNoLoadSharingRrcConnPs) ))) * 
                                      as.numeric(sum(pmNoRabEstablishSuccessPacketInteractive)/sum(pmNoRabEstablishAttemptPacketInteractive))),3),
    
  
    RET_CS = 100 -  round(100*(as.numeric(sum(as.numeric(pmNoSystemRabReleaseSpeech) ) + sum(as.numeric(pmNoSystemRabReleaseCs64) )) / 
                                 as.numeric(sum(as.numeric(pmNoNormalRabReleaseSpeech) ) + sum(as.numeric(pmNoNormalRabReleaseCs64) ) + sum(as.numeric(pmNoSystemRabReleaseSpeech) ) + sum(as.numeric(pmNoSystemRabReleaseCs64) )) ),3),
    
    RET_PS = round(100 - 100 * as.numeric(as.numeric( sum(as.numeric(pmNoSystemRabReleasePacket )- as.numeric(pmNoSystemRabReleasePacketUra) )  - sum(as.numeric(pmChSwitchAttemptFachUra) -as.numeric( pmChSwitchSuccFachUra) ) - sum(as.numeric(pmChSwitchAttemptDchUra) - as.numeric(pmChSwitchSuccDchUra) ) - sum(as.numeric(pmChSwitchAttemptHsUra) - as.numeric(pmChSwitchSuccHsUra) ))
                                              / as.numeric(sum(as.numeric(pmNoNormalRabReleasePacket) ) - sum(as.numeric(pmNoNormalRabReleasePacketUra) ) +  sum(as.numeric(pmNoSystemRabReleasePacket) ) + sum(as.numeric(pmNoSystemRabReleasePacketUra) ) 
                                                           + sum(as.numeric(pmChSwitchSuccFachUra) ) + sum(as.numeric(pmChSwitchSuccDchUra) )  + sum(as.numeric(pmChSwitchSuccHsUra) )) ),3),
    
    CONG_CE_RRC = as.numeric(sum(pmNoRrcReqDeniedAdmDlHw) + sum(pmNoRrcReqDeniedAdmUlHw)),
    
    CONG_PWR_RRC =  as.numeric(sum(pmNoRrcReqDeniedAdmDlPwr)),
    
    CONG_CODE_RRC =  as.numeric(sum(pmNoRrcReqDeniedAdmDlChnlCode)),
    
    CONG_CE_RAB =  as.numeric(sum(pmNoFailedRabEstAttemptLackDlHw) + sum(pmNoFailedRabEstAttemptLackUlHw)),
    
    #
    CONG_PWR_RAB =  as.numeric(sum(pmNoFailedRabEstAttemptLackDlPwr)),
    
    
    CONG_CODE_RAB = as.numeric(sum(pmNoFailedRabEstAttemptLackDlChnlCode)), 
    
    ITF = -112 +  ((0.1)*as.numeric(sum(pmSumUlRssi)/sum(pmSamplesUlRssi))),
    PAGINGS = sum(as.numeric(pmNoPagingAttemptUtranRejected) ),
    TRAFF_CS_ERL = round(seg * as.numeric(sum(pmSumBestCs12Establish/pmSamplesBestCs12Establish)),2),
    
    VOLUMEN_DATOS = round(as.numeric(sum(as.numeric(pmDlTrafficVolumePsIntHs) ) + sum(as.numeric(pmUlTrafficVolumePsIntEul) ) + sum(as.numeric(VOLUMEN_R99_UL) ) + sum(as.numeric(VOLUMEN_R99_DL) )  )/ (8*1000000),3),
    
    DROP_CS = as.numeric(sum(as.numeric( pmNoSystemRabReleaseSpeech )+ as.numeric(pmNoSystemRabReleaseCs64) )),
    
    DROP_PS = as.numeric(sum(as.numeric(pmNoSystemRabReleasePacket))),
  
    VOLUMEN_HSDPA = round(as.numeric(sum(as.numeric(pmDlTrafficVolumePsIntHs) )  )/ (8*1000000),3),
    VOLUMEN_HSUPA = round(as.numeric(sum(as.numeric(pmUlTrafficVolumePsIntEul) )  )/ (8*1000000),3),
    R99_DL = round(as.numeric(sum(as.numeric(VOLUMEN_R99_DL) ))/ (8*1000000),3),
    R99_UL = round(as.numeric(sum(as.numeric(VOLUMEN_R99_UL) ))/ (8*1000000),3),
    
    TRAFF_PS_ERL = round(as.numeric(sum(as.numeric(pmSumBestDchPsIntRabEstablish)) + sum(as.numeric(pmSumFachPsIntRabEstablish)) + sum(as.numeric(pmSumBestPsHsAdchRabEstablish)) + sum(as.numeric(pmSumBestPsEulRabEstablish)) )/720,3),
    
    TRAFF_TOTAL_ERL = round(as.numeric(seg * as.numeric(sum(as.numeric(pmSumBestCs12Establish) /as.numeric(pmSamplesBestCs12Establish) ))) +  as.numeric(as.numeric(sum(as.numeric(pmSumBestDchPsIntRabEstablish)) + sum(as.numeric(pmSumFachPsIntRabEstablish)) + sum(as.numeric(pmSumBestPsHsAdchRabEstablish)) + sum(as.numeric(pmSumBestPsEulRabEstablish)) )/720),3),
    
    ACC_PS_RRC = 100 * as.numeric(sum(as.numeric(pmTotNoRrcConnectReqPsSucc) )/(sum(as.numeric(pmTotNoRrcConnectReqPs) ) )),
    pmTotNoRrcConnectReqPsSucc = sum(pmTotNoRrcConnectReqPsSucc),
    pmTotNoRrcConnectReqPs =sum(pmTotNoRrcConnectReqPs),
    pmNoLoadSharingRrcConnPs = sum(pmNoLoadSharingRrcConnPs),
    DISPONIBILIDAD =  100 * round( 1 - as.numeric(sum(pmCellDowntimeAuto) + sum(pmCellDowntimeMan))/seg1,3),
    
    ACC_PS_NUM_E = as.numeric(sum(pmTotNoRrcConnectReqPsSucc))*as.numeric(sum(pmNoRabEstablishSuccessPacketInteractive)),
    
    ACC_PS_DEN_E = as.numeric(sum(pmTotNoRrcConnectReqPsSucc) - sum(pmNoLoadSharingRrcConnPs)) * as.numeric(sum(pmNoRabEstablishAttemptPacketInteractive))
    
  )
  
  datosP[is.na(datosP)] <- 0
  return (datosP)
  
}

filter2GNok <- function(data,filter,cellfilter,gra){
  if (is.null(data) | (dim(data)[1] == 0)) return(data.frame())
  if(cellfilter != ""){data <- filter(data,str_detect(CELDA,cellfilter))}
  
  switch (filter,
          "Total" = {data$TOTAL <- "Total"
          datosT <- data %>% group_by(DIA,TOTAL)
          },
          "ByProvince" = {datosT <- data %>% group_by(DIA, PROVINCIA = substr(CELDA,1,1))},  
          "BySite" = {datosT <- data %>% group_by(DIA,SITE = substr(CELDA,1,6))},
          "ByRNC" = {datosT <- data %>% group_by(DIA,BSC)},
          "ByCell" = {datosT <- data %>% group_by(DIA,CELDA)}
  )
  
  if (gra == "yyyy-mm-dd") {datosT <- datosT %>% 
    mutate(RES_AV_DENOM14 = RES_AV_DENOM14 / 24,
           RES_AV_DENOM15 = RES_AV_DENOM15 / 24)}
  datosP <- datosT %>%summarise(
    ACC = round((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                  (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  )) *
                  (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000,3),
    RET = round(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))),3),
    
    SER = round(as.numeric((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                             (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  ))*
                             (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000)
                *as.numeric(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))) )/100,3),
    TRAFF_TCH =round(sum( as.numeric(AVE_BUSY_TCH)/as.numeric(RES_AV_DENOM14)),3),
    
    TRAFF_TCH_FR = round(as.numeric(sum(AVE_TCH_BUSY_FULL)) + (2*as.numeric(sum(AVE_BUSY_DFR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
    
    TRAFF_TCH_HR = round(as.numeric(sum(AVE_TCH_BUSY_HALF)) + (2*as.numeric(sum(AVE_BUSY_DHR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
    
    TRAFF_SDCCH = round(sum(as.numeric(AVE_BUSY_SDCCH)/as.numeric(RES_AV_DENOM15)),3),
    
    CONG_SDCCH = round(100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)),2),
    
    CONG_TCH = round(100 * as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)- sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) )/as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)),2),
    
    DROP_TCH = as.numeric(sum(DROP_AFTER_TCH_ASSIGN)),
    
    DROP_SDCCH = as.numeric(sum(DROP_SDCCH) - sum(T3101_EXPIRED)),
    
    PAGINGS = as.numeric(sum(DELETE_PAGING_COMMAND)),
    
    #VOL_UL = round(as.numeric(sum(as.numeric( 20*sum(RLC_DATA_BLOCKS_UL_CS1) + 30*sum(RLC_DATA_BLOCKS_UL_CS2))
    #                          + as.numeric(sum(UL_RLC_BLOCK)*36)
    #                          + as.numeric(sum(UL_RLC_BLOCK)*50))/1024
    #                          +
    #                          as.numeric(
    #                            as.numeric( as.numeric(UL_RLC_BLOCK)  * 22)
    #                          + as.numeric( as.numeric(UL_RLC_BLOCK)  * 28)
    #                          + as.numeric( as.numeric(UL_RLC_BLOCK)  * 37)
    #                          + as.numeric( as.numeric(UL_RLC_BLOCK)  * 44)
    #                          + as.numeric( as.numeric(UL_RLC_BLOCK)  * 56)
    #                          + as.numeric( as.numeric(UL_RLC_BLOCK)  * 74)
    #                          + as.numeric( as.numeric(UL_RLC_BLOCK)/2  * 112)
    #                          + as.numeric( as.numeric(UL_RLC_BLOCK)/2  * 136) 
    #                          + as.numeric( as.numeric(UL_RLC_BLOCK)/2  * 148)
    #                            )
    #                          )/1073741824 ,2),
    #VOL_DL = round(as.numeric((as.numeric( 20*sum(RLC_DATA_BLOCKS_DL_CS1) + 30*sum(RLC_DATA_BLOCKS_DL_CS2))
    #                           + as.numeric( as.numeric(sum(DL_RLC_BLOCK))  * 36)
    #                           + as.numeric( as.numeric(sum(DL_RLC_BLOCK))  * 50))/1024
    #                          +
    #                            as.numeric(
    #                              as.numeric(as.numeric( as.numeric(sum(DL_RLC_BLOCK))  * 22)
    #                              + as.numeric( as.numeric(sum(DL_RLC_BLOCK))  * 28)
    #                              + as.numeric( as.numeric(sum(DL_RLC_BLOCK))  * 37)
    #                              + as.numeric( as.numeric(sum(DL_RLC_BLOCK))  * 44)
    #                              + as.numeric( as.numeric(sum(DL_RLC_BLOCK))  * 56)
    #                              + as.numeric( as.numeric(sum(DL_RLC_BLOCK))  * 74)
    #                              + as.numeric( as.numeric(sum(DL_RLC_BLOCK))/2  * 74)
    #                              + as.numeric( as.numeric(sum(DL_RLC_BLOCK))/2  * 136) 
    #                              + as.numeric( as.numeric(sum(DL_RLC_BLOCK))/2  * 148))/1024
    #                            )
    #)/1073741824 ,2),
   INT4_5 = as.numeric(sum(AVE_IDLE_F_TCH_4)+sum(AVE_IDLE_F_TCH_5)),
   INT4 = as.numeric(sum(AVE_IDLE_F_TCH_4)),
   INT5 = as.numeric(sum(AVE_IDLE_F_TCH_5)),
   INT4_5P = round(100 * as.numeric(as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+ as.numeric(sum(AVE_IDLE_F_TCH_5)/sum(RES_AV_DENOM8)) ) / as.numeric(  as.numeric(sum(AVE_IDLE_F_TCH_1)/(sum(RES_AV_DENOM4)) )+ 
                                                                                                                                                                  as.numeric(sum(AVE_IDLE_F_TCH_2)/(sum(RES_AV_DENOM5)) )+
                                                                                                                                                                   as.numeric(sum(AVE_IDLE_F_TCH_3)/(sum(RES_AV_DENOM6)) )+
                                                                                                                                                                   as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+
                                                                                                                                                                   as.numeric(sum(AVE_IDLE_F_TCH_5)/(sum(RES_AV_DENOM8)) )),3)
  
  )
  datosP[is.na(datosP)] <- 0
  return (datosP)
}

filter3GNok <- function(data,filter,cellfilter){ 
  if (is.null(data) | (dim(data)[1] == 0)) return(data.frame()) 
  if(cellfilter != ""){data <- filter(data,str_detect(CELDA,cellfilter))}
  
  switch (filter,
          "Total" = {data$TOTAL <- "Total"
          datosT <- data %>% group_by(DIA,TOTAL)
          },
          "ByProvince" = {datosT <- data %>% group_by(DIA, PROVINCIA = substr(CELDA,1,1))},  
          "BySite" = {datosT <- data %>% group_by(DIA,SITE = substr(CELDA,1,6))},
          "ByRNC" = {datosT <- data %>% group_by(DIA,RNC)},
          "ByCell" = {datosT <- data %>% group_by(DIA,CELDA)}
  )
  datosP <- datosT %>% summarise(
      ACC_CS = round(100* as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) /as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)) * as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),3),
      
      ACC_PS = round(100 * as.numeric(sum(NUM_ACC_PS)/sum(DEN_ACC_PS))*as.numeric(sum(MULT_NUM1)/sum(MULT_NUM2)),3),
      
      RET_CS = round(100 -  as.numeric(100 * as.numeric(sum(NUM_RET_CS))/as.numeric(sum(DEN_RET_CS))),3),
      
      RET_PS = round(100 - as.numeric(100 * as.numeric(sum(NUM_RET_PS))/as.numeric(sum(DEN_RET_PS))),3),
      
      PWR_RRC = as.numeric(sum(as.numeric(RRC_CONN_STP_FAIL_AC_DL))+sum(as.numeric(RRC_CONN_STP_FAIL_AC_UL) ) ),
      
      PWR_RAB = as.numeric(sum(as.numeric(RAB_STP_FAIL_CS_VOICE_AC_UL) )+sum(as.numeric(RAB_STP_FAIL_CS_VOICE_AC_DL) ) ),
      
      AVG_RAB_HLD_TM_CS_VOICE = round(as.numeric(sum(as.numeric(as.numeric(AVG_RAB_HLD_TM_CS_VOICE) )))/360000,3),
      
      VOL = round(as.numeric(sum(as.numeric(VOL_DL) )+sum(as.numeric(VOL_UL) ))/1000000000,3),
      
      MAX_HSDPA_USERS_IN_CELL = as.numeric(sum(as.numeric(MAX_HSDPA_USERS_IN_CELL) )),
      
      MAX_HSUPA_USERS_IN_CELL = as.numeric(sum(as.numeric(MAX_HSUPA_USERS_IN_CELL) )),
      
      HS_DSCH_DATA_VOL = round(as.numeric(sum(as.numeric(HS_DSCH_DATA_VOL) ))/1000000000,3),
      
      NRT_EDCH_UL_DATA_VOL = round(as.numeric(sum(as.numeric(NRT_EDCH_UL_DATA_VOL) ))/1000000000,3),
      
      REJ_DCH_DUE_POWER_BGR_DL = round(sum(as.numeric(REJ_DCH_DUE_POWER_BGR_DL) )),
      
      REJ_DCH_DUE_POWER_INT_DL = round(sum(as.numeric(REJ_DCH_DUE_POWER_INT_DL) )),
      
      RRC_CONN_STP_FAIL_BTS = round(sum(as.numeric(RRC_CONN_STP_FAIL_BTS) )),
      
      RAB_STP_FAIL_CS_VOICE_BTS = round(sum(as.numeric(RAB_STP_FAIL_CS_VOICE_BTS) )),
      
      RRC_CONN_STP_FAIL_AC_COD = round(sum(as.numeric(RRC_CONN_STP_FAIL_AC_COD) ),3),
      
      RAB_STP_FAIL_CS_VOICE_AC_COD = round(sum(as.numeric(RAB_STP_FAIL_CS_VOICE_AC_COD) ),3),
      
      
      REJ_DCH_DUE_CODES_INT_DL = round(sum(as.numeric(REJ_DCH_DUE_CODES_INT_DL) ),2), 
      REJ_DCH_DUE_CODES_BGR_DL = round(sum(as.numeric(REJ_DCH_DUE_CODES_BGR_DL) ),2),
      PS_SETUP_FAIL_AC_COD_NRT = round(sum(as.numeric(PS_SETUP_FAIL_AC_COD_NRT) ) ,3),
      PS_SETUP_FAIL_AC_DL_NRT = round(sum(as.numeric(PS_SETUP_FAIL_AC_DL_NRT)) ,3),
      PS_SETUP_FAIL_AC_UL_NRT = round(sum(as.numeric(PS_SETUP_FAIL_AC_UL_NRT)) ,3),
      RAB_STP_FAIL_PS_STREA_BTS = round(sum(as.numeric(RAB_STP_FAIL_PS_STREA_BTS) ) ,3),
      
      R99_DL = round((as.numeric(sum(as.numeric(NRT_DCH_DL_DATA_VOL))))/1000000000,5),
  
      R99_UL = round((as.numeric(sum(as.numeric(NRT_DCH_UL_DATA_VOL))))/1000000000,5),
      
      PAG_DISCARD = round(as.numeric(sum(as.numeric(FAIL_PAG_NO_RESP_URA_PCH)) +sum(as.numeric(FAIL_PAG_NO_RESP_CELL_PCH))),3),
      
      FAIL_PAG_NO_RESP_URA_PCH = round(as.numeric(sum(as.numeric(FAIL_PAG_NO_RESP_URA_PCH) )),3),
      
      FAIL_PAG_NO_RESP_CELL_PCH =  round(as.numeric(sum(as.numeric(FAIL_PAG_NO_RESP_CELL_PCH) )),3),
      
      PAG_SENT = round(as.numeric(sum(as.numeric(PAGING_OCCASION_CELL_PCH) )+sum(as.numeric(PAGING_OCCASION_URA_PCH) )),3),
      
      PAGING_OCCASION_URA_PCH = round(as.numeric(sum(as.numeric(PAGING_OCCASION_URA_PCH) )),3),
      
      PAGING_OCCASION_CELL_PCH =  round(as.numeric(sum(as.numeric(PAGING_OCCASION_CELL_PCH) )),3),
      
      ATT_FACH_TO_HS_DSCH = round(as.numeric(sum(as.numeric(ATT_FACH_TO_HS_DSCH) )),3),
      
      ATT_HS_DSCH_TO_FACH = round(as.numeric(sum(as.numeric(ATT_HS_DSCH_TO_FACH) )),3),
      
      SUCC_HS_DSCH_TO_FACH = round(as.numeric(sum(as.numeric(SUCC_HS_DSCH_TO_FACH) )),3),
      
      SUCC_FACH_TO_HS_DSCH = round(as.numeric(sum(as.numeric(SUCC_FACH_TO_HS_DSCH) )),3),
      
      CONG_USER_HSDPA = round(as.numeric(sum(as.numeric(HSDPA_TO_DCH_NUM)) ),3),
      
      CONG_USER_HSUPA = round(as.numeric(sum(as.numeric(HSUPA_TO_DCH_NUM)) ),3),
      
      NO_ACC_HSDPA = round(as.numeric(sum(as.numeric(HSDPA_TO_DCH_DEN)) ),3),
      
      NO_ACC_HSUPA = round(as.numeric(sum(as.numeric(HSUPA_TO_DCH_DEN)) ),3),
      
      RRC_CONN_STP_FAIL_AC_DL = as.numeric(sum(as.numeric(RRC_CONN_STP_FAIL_AC_DL))),
      
      RRC_CONN_STP_FAIL_AC_UL = as.numeric(sum(as.numeric(RRC_CONN_STP_FAIL_AC_UL))),
      
      RAB_STP_FAIL_CS_VOICE_AC_UL = as.numeric(sum(as.numeric(RAB_STP_FAIL_CS_VOICE_AC_UL)) ),
      
      RAB_STP_FAIL_CS_VOICE_AC_DL = as.numeric(sum(as.numeric(RAB_STP_FAIL_CS_VOICE_AC_DL)) ),
      
      TROUGH_HSDPA =  round(as.numeric(sum(as.numeric(HSDPA_ORIG_DATA)) *8*500) / as.numeric(sum(as.numeric(HSDPA_BUFF_WITH_DATA_PER_TTI)) ) ,3),
        
      TROUGH_HSUPA =  round((as.numeric(sum(as.numeric(MACE_PDU_DATA_2MS_TTI) ) + sum(as.numeric(MACE_PDU_DATA_10MS_TTI) ))*8)/as.numeric(as.numeric(sum(as.numeric(MACE_PDUS_2MS_TTI)) /500) + as.numeric(sum(as.numeric(MACE_PDUS_10MS_TTI)   ) /100)),3),
      
      RTWP = round(10 * log10(as.numeric(as.numeric(10^(-11)*sum(RTWP_CLASS_0)) + as.numeric(10^(-10.75)*sum(RTWP_CLASS_1)) +as.numeric(10^(-10.65)*sum(RTWP_CLASS_2)) +as.numeric(10^(-10.55)*sum(RTWP_CLASS_3)) +as.numeric(10^(-10.45)*sum(RTWP_CLASS_4))
                                         +as.numeric(10^(-10.25)*sum(RTWP_CLASS_5)) + as.numeric(10^(-10.25)*sum(RTWP_CLASS_6)) +as.numeric(10^(-10.15)*sum(RTWP_CLASS_7)) +as.numeric(10^(-10.05)*sum(RTWP_CLASS_8)) +as.numeric(10^(-9.95)*sum(RTWP_CLASS_9)) 
                                         +as.numeric(10^(-9.85)*sum(RTWP_CLASS_10)) + as.numeric(10^(-9.70)*sum(RTWP_CLASS_11)) +as.numeric(10^(-9.50)*sum(RTWP_CLASS_12)) +as.numeric(10^(-9.30)*sum(RTWP_CLASS_13)) +as.numeric(10^(-9.05)*sum(RTWP_CLASS_14)) 
                                         +as.numeric(10^(-8.75)*sum(RTWP_CLASS_15)) + as.numeric(10^(-8.45)*sum(RTWP_CLASS_16)) +as.numeric(10^(-8.15)*sum(RTWP_CLASS_17)) +as.numeric(10^(-7.75)*sum(RTWP_CLASS_18)) +as.numeric(10^(-7.25)*sum(RTWP_CLASS_19))
                                         +as.numeric(10^(-6.75)*sum(RTWP_CLASS_20)) + as.numeric(10^(-6.50)*sum(RTWP_CLASS_21)) )/
                                as.numeric( sum(RTWP_CLASS_0) +sum(RTWP_CLASS_1)+sum(RTWP_CLASS_2)+sum(RTWP_CLASS_3)+sum(RTWP_CLASS_4)+sum(RTWP_CLASS_5)+sum(RTWP_CLASS_6)+sum(RTWP_CLASS_7)+sum(RTWP_CLASS_8)+sum(RTWP_CLASS_9)+sum(RTWP_CLASS_10)+
                                              +sum(RTWP_CLASS_11)+sum(RTWP_CLASS_12)+sum(RTWP_CLASS_13)+sum(RTWP_CLASS_14)+sum(RTWP_CLASS_15)+sum(RTWP_CLASS_16)+sum(RTWP_CLASS_17)+sum(RTWP_CLASS_18)+sum(RTWP_CLASS_19)+sum(RTWP_CLASS_20)+sum(RTWP_CLASS_21))
      ),2),
      
      AVAIL = round(100 * sum(AVAIL_WCELL_IN_WO_STATE)/as.numeric(sum(AVAIL_WCELL_EXISTS_IN_RNW_DB)-sum(AVAIL_WCELL_BLOCKED_BY_USER)),2),
      HSDPA_ACC = round(100 * as.numeric(sum(NUM_HSDPA_ACC))/as.numeric(sum(DEN_HSDPA_ACC) ),2),
      HSUPA_ACC = round(100 * as.numeric(sum(NUM_HSUPA_ACC))/as.numeric(sum(DEN_HSUPA_ACC) ),2),
      
      UNSF_SHO_NRT = sum(UNSUCC_UPDATES_ON_SHO_NRT),
      UNSF_SHO_RT = sum(UNSUCC_UPDATES_ON_SHO_FOR_RT),
      SUCC_SHO_RATIO = round(100 * as.numeric(sum(SUCC_UPDATES_ON_SHO_FOR_RT) + sum(SUCC_UPDATES_ON_SHO_FOR_NRT)) /as.numeric(sum(CELL_ADD_REQ_ON_SHO_FOR_RT) + sum(CELL_REPL_REQ_ON_SHO_FOR_RT) + sum(CELL_DEL_REQ_ON_SHO_FOR_RT) + sum(CELL_ADD_REQ_ON_SHO_FOR_NRT) + sum(CELL_REPL_REQ_ON_SHO_FOR_NRT) + sum(CELL_DEL_REQ_ON_SHO_FOR_NRT)) ,2),
      DROP_CS = round(as.numeric(sum(NUM_RET_CS)),3),
      
      DROP_PS = round(as.numeric(sum(NUM_RET_PS)),3),
      
      PS_SETUP_FAIL_AC_INT = sum(as.numeric(PS_SETUP_FAIL_AC_INT)),
      
      PS_SETUP_FAIL_AC_BGR = sum(as.numeric(PS_SETUP_FAIL_AC_BGR)),
      
      CONG_HSDPA_PWR = sum(as.numeric(HSDPA_PWR)),
      
      USER_16_QAM = as.numeric(mean(EDCH_16QAM_UE_ACT_SUM)),
      
      MEH_HSDSCH = as.numeric(sum(MEH_REJECT_HSDSCH)),
      
      MEH_CS_VOICE = as.numeric(sum(MEH_ADMIT_CS_VOICE)),
      
      HSPA_RT_O_NRT_DUE_SC = as.numeric(sum(HSPA_RT_O_NRT_DUE_SC)),
      
      ACC_RRC_CS = round(100* as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) /as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)),3),
      
      ACC_RAB_CS = round(100*  as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),3),
      
      ACC_RRC_PS = round(100 * as.numeric(sum(NUM_ACC_PS)/sum(DEN_ACC_PS)),3),
      
      ACC_RAB_PS = round(100 * as.numeric(sum(MULT_NUM1)/sum(MULT_NUM2)),3),
     
      ATT_DCH_TO_PCH = as.numeric(sum(ATT_DCH_TO_PCH)),
      
      ATT_PCH_TO_DCH = as.numeric(sum(ATT_PCH_TO_DCH)), 
      
      ATT_FACH_TO_PCH = as.numeric(sum(ATT_FACH_TO_PCH)),
      
      ATT_PCH_TO_FACH = as.numeric(sum(ATT_PCH_TO_FACH)),
      
      SUM_HSUPA_USERS_2MS_TTI = as.numeric(sum(SUM_HSUPA_USERS_2MS_TTI/DENOM_HSUPA_USERS_2MS_TTI)),
      
      SUM_HSUPA_USERS_10MS_TTI = as.numeric(sum(SUM_HSUPA_USERS_10MS_TTI/DENOM_HSUPA_USERS_10MS_TTI)),
      
      SETUP_FAIL_IUB = as.numeric(sum(SETUP_FAIL_IUB_HS_TOTAL_INT) + sum(SETUP_FAIL_IUB_HS_TOTAL_BGR)),
      
      REJ_HS_DSCH_FOR_UL = as.numeric(sum(REJ_HS_DSCH_RET_INT) + sum(REJ_HS_DSCH_RET_BGR)),
      
      SETUP_FAIL_EUL = as.numeric(sum(SETUP_REJ_EDCH_AC_INT) + sum(SETUP_REJ_EDCH_AC_BGR)),
      
      SCCPCH_LOAD = round( as.numeric(sum(AVE_SCCPCH_INC_PCH_LOAD) /  sum(SCCPCH_LOAD_DENOM_0) ) ,2), 
      
      FACH_u_LOAD = 100 * as.numeric(sum(AVE_FACH_UDATA_TP_SCCPCH) /sum( FACH_U_DATA_TPUT_DENOM_1) )/ 36000 ,
      
      FACH_c_LOAD = 100 * ((as.numeric(sum(AVE_FACH_USER_TOT_TPUT) / sum(FACH_USER_TOT_TPUT_DENOM_1)) -  as.numeric(sum(AVE_FACH_UDATA_TP_SCCPCH / FACH_U_DATA_TPUT_DENOM_1))) / 33600),
      
      RACH_u_LOAD = 100 * as.numeric(sum(AVE_RACH_DATA_THROUGHPUT) / sum(RACH_DENOM_4 *72000)),
      
      RACH_c_LOAD = 100 * ((as.numeric(sum(AVE_RACH_THROUGHPUT) / sum(RACH_DENOM_3)) -  as.numeric(sum(AVE_RACH_DATA_THROUGHPUT) / sum(RACH_DENOM_4))) / 33600),
      
      AVG_CODE_TREE = round( as.numeric(sum(CODE_CAPACITY)/sum(DENOM_CODE_CAPACITY)) ,2),
      
      MAX_CODE_TREE =  as.numeric(mean(MAX_CODE_OCCUPANCE_PERCENT)) ,
      
      MIN_CODE_TREE =  as.numeric(mean(MIN_CODE_OCCUPANCY_PERCENT)),
     
      CODE_BLOCK_DL = 100 - 100* round(as.numeric(sum(NBR_SUCC_CODE_TREE_ALLO))/as.numeric( sum(CHAN_CODE_SF4_REQUEST + CHAN_CODE_SF8_REQUEST + CHAN_CODE_SF16_REQUEST + CHAN_CODE_SF32_REQUEST + CHAN_CODE_SF64_REQUEST + CHAN_CODE_SF128_REQUEST + CHAN_CODE_SF256_REQUEST)),2),
      
      PAGING_DISCARD = 100 - (100 * as.numeric(sum(as.numeric(FAIL_PAG_NO_RESP_URA_PCH)) +sum(as.numeric(FAIL_PAG_NO_RESP_CELL_PCH))) / as.numeric(sum(as.numeric(PAGING_OCCASION_CELL_PCH))+sum(as.numeric(PAGING_OCCASION_URA_PCH) )))
  )
  datosP[is.na(datosP)] <- 0
  return (datosP)
}

filter3GNokBTS <- function(data,filter,cellfilter){ 
  if (is.null(data) | (dim(data)[1] == 0)) return(data.frame())
  if(cellfilter != ""){data <- filter(data,str_detect(SITIO,cellfilter))}
    datosP <- data %>%group_by(DIA,SITIO)%>%summarise(
    
    
    AVG_CE_R99_DL = round(100 * as.numeric(sum(AVG_USED_CE_R99_DL / LICENSED_R99CE_WBTS)),3),
    AVG_CE_R99_UL = round(100 * as.numeric(sum(AVG_USED_CE_R99_UL / LICENSED_R99CE_WBTS)),3),
    
    AVG_USED_CE_R99 = round(100 * as.numeric(sum(AVG_USED_CE_R99_DL)+sum(AVG_USED_CE_R99_UL)) / as.numeric(sum(LICENSED_R99CE_WBTS)) ,3),
    
    MAX_CE_R99_DL = round(100 * as.numeric(sum(MAX_USED_CE_R99_DL / LICENSED_R99CE_WBTS)),3),
    MAX_CE_R99_UL = round(100 * as.numeric(sum(MAX_USED_CE_R99_UL / LICENSED_R99CE_WBTS)),3),
    
    MAX_USED_CE_R99 = round(100 * as.numeric(sum(MAX_USED_CE_R99_DL)+sum(MAX_USED_CE_R99_UL)) / as.numeric(sum(LICENSED_R99CE_WBTS)) ,3),
   
    AVG_USED_SU = round(100 * as.numeric(sum(AVG_USED_BB_SUBUNITS / NUM_BB_SUBUNITS)),3),
    MAX_USED_SU = round(100 * as.numeric(sum(MAX_USED_BB_SUBUNITS / NUM_BB_SUBUNITS)),3),
    
    AVG_USED_HSUPA_SU = round(100 * as.numeric(sum(AVG_HSUPA_SU_USG/NUM_BB_SUBUNITS)),3),
    MAX_USED_HSUPA_SU = round(100 * as.numeric(sum(MAX_HSUPA_SU_USG/NUM_BB_SUBUNITS)),3),
    
    AVG_HSDPA_USER = as.numeric(sum(AVG_BTS_HSDPA_USERS)),
    MAX_HSUPA_USER = as.numeric(sum(MAX_BTS_HSDPA_USERS)),
    AVG_HSDPA_USER = as.numeric(sum(AVG_BTS_HSUPA_USERS)),
    MAX_HSUPA_USER = as.numeric(sum(MAX_BTS_HSUPA_USERS))

  )
  
    datosP[is.na(datosP)] <- 0
    return (datosP)
}

filter4GEri <- function(data,filter,cellfilter){
  if (is.null(data) | (dim(data)[1] == 0)) return(data.frame())
  if(cellfilter != ""){data <- filter(data,str_detect(CELDA,cellfilter))}
  switch (filter,
          "Total" = {data$TOTAL <- "Total"
          datosT <- data %>% group_by(DIA,TOTAL)
          },
          "ByProvince" = {datosT <- data %>% group_by(DIA, PROVINCIA = substr(CELDA,1,1))},  
          "BySite" = {datosT <- data %>% group_by(DIA,SITIO)},
          "ByCell" = {datosT <- data %>% group_by(DIA,CELDA)}
  )
  
  datosP <- datosT %>%summarise(
    ACC = round(100* as.numeric(sum(pmRrcConnEstabSucc)/ (sum(pmRrcConnEstabAtt) - sum(pmRrcConnEstabAttReatt) - sum(rrcmmeovl) ) )
                * as.numeric(sum(pmS1SigConnEstabSucc)/ (sum(pmS1SigConnEstabAtt)-sum(pmS1SigConnEstabFailMmeOvlMos)) ) * as.numeric(sum(pmErabEstabSuccInit)/sum(pmErabEstabAttInit)) ,3),
    
    RET = round(100 - 100 * as.numeric(sum(pmErabRelAbnormalEnbAct)+ sum(pmErabRelAbnormalMmeAct))/as.numeric(sum(pmErabRelAbnormalEnb) + sum(pmErabRelNormalEnb) + sum(pmErabRelMme) ),3),
    
    PAYLOAD_DL = round(as.numeric((sum(as.numeric(pmPdcpVolDlDrb))+sum(as.numeric(pmPdcpVolDlSrb) )) /(8*1000000)) ,3),
    PAYLOAD_UL = round(as.numeric(sum(as.numeric(VOL_UL) )/(8*1000000)) ,3),
    
    THRPTDL = round(as.numeric(sum(as.numeric(pmPdcpVolDlDrb))-sum(as.numeric(pmPdcpVolDlDrbLastTTI) ) )/as.numeric(sum(as.numeric(pmUeThpTimeDl))) ,3),
    
    THRPTUL = round(as.numeric(sum(pmUeThpVolUl))/as.numeric(sum(pmUeThpTimeUl)),3),
    
    DROP_ERAB = as.numeric(sum(pmErabRelAbnormalEnbActCdt) + sum(pmErabRelAbnormalEnbActHo) + sum(pmErabRelAbnormalEnbActHpr) + sum(pmErabRelAbnormalEnbActPe) + sum(pmErabRelAbnormalEnbActTnFail) + sum(pmErabRelAbnormalEnbActUeLost) + sum(pmErabRelAbnormalEnbLic)),
    
    LAT = round(as.numeric(sum(pmPdcpLatTimeDl))/as.numeric(sum(pmPdcpLatPktTransDl)),3),
    
    USER_DL = as.numeric(sum(pmActiveUeDlMax)),
    
    USER_UL = as.numeric(sum(pmActiveUeUlMax)),
    
    USER_MAX_UL = as.numeric(max(pmActiveUeUlMax)),
    
    USER_MAX_DL = as.numeric(max(pmActiveUeDlMax))
  )
  datosP[is.na(datosP)] <- 0
  return (datosP)
  
}

renderGraf <- function(dataP,index,granul){
  
  if(granul == "yyyy-mm-dd"){
    tformat <- "%Y-%m-%d"
  }
  
  if(granul == "yyyy-mm-dd hh"){
    tformat <- "%Y-%m-%d %H"
  }
  
  cell.vcl <- levels(factor(unlist(dataP[,2]), ordered = TRUE))
  #              mydate <- strptime(levels(factor(unlist(datos[,1]$Date_h), ordered = TRUE)),format = "%Y-%m-%d %H")
  mydate <- strptime(levels(factor(unlist(dataP[,1]), ordered = TRUE)),format = tformat,tz="EST")
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

rendermygraphOK <- function (mylist){
  
    return(renderDygraph({
      d1 <- dygraph(as.xts(mylist$mydata,order.by = mylist$mydate,tz="GMT"))%>%
        dyLegend(show = "always", hideOnMouseOut = TRUE, width = 400) %>%
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2 ,hideOnMouseOut = FALSE) %>%
        dyRangeSelector() 
   #
      
      #.dygraph-legend > span.highlight { display: inline; }
      #.dygraph-legend > span.highlight { border: 1px solid grey; }

      d1$x$css = "
      .dygraph-legend > span {display:none;}
      .dygraph-legend > span.highlight { display: inline; }
      .dygraph-legend > span.highlight { border: 1px solid grey; }
      "
      d1
    }))
}

###SERVER#####
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datos.2GHua <- data.frame()
  datos.3GHua <- data.frame()
  datos.2GEri <- data.frame()
  datos.3GEri <- data.frame()
  datos.2GNok <- data.frame()
  datos.3GNok <- data.frame()
  datos.4GEri <- data.frame()
  hua3G <- data.frame()
  hua2G <- data.frame()
  eri3G <- data.frame()
  eri2G <- data.frame()
  eri4G <- data.frame()
  Nok2G <- data.frame()
  Nok3G <- data.frame()
  
  
  observeEvent(input$query, {
    
    id <- showNotification("Ejecutando Consulta",duration =NULL)
    if( "CELL" %in%  input$type){
    
    if (("2G" %in% input$network) & ("ERICSSON" %in% input$provider)) {
    datos.2GEri <<- queryData2GEricsson (as.character(input$idate),as.character(input$fdate),input$cellcontext,input$gra)
    eri2G <<- filter2GEri(datos.2GEri,input$filter,input$cellfilter,input$gra)

    output$ACC = rendermygraphOK(renderGraf(eri2G,3,input$gra))
    output$RET = rendermygraphOK(renderGraf(eri2G,4,input$gra))
    output$SER = rendermygraphOK(renderGraf(eri2G,5,input$gra))
    output$CONG_TCH = rendermygraphOK(renderGraf(eri2G,6,input$gra))
    output$CONG_SDCCH = rendermygraphOK(renderGraf(eri2G,7,input$gra))
    output$TRAF_TCH = rendermygraphOK(renderGraf(eri2G,10,input$gra))
    output$TRAF_SDCCH = rendermygraphOK(renderGraf(eri2G,11,input$gra))
    output$DROP_TCH= rendermygraphOK(renderGraf(eri2G,12,input$gra))
    output$DROP_SDCCH = rendermygraphOK(renderGraf(eri2G,13,input$gra))
    output$PAGINGS = rendermygraphOK(renderGraf(eri2G,9,input$gra))
    
    
    output$EstadoRed <- downloadHandler(
      filename = function() {
        paste('Red Movil ', Sys.Date(), '.xlsx', sep = '')
      },
      content = function(file) {
        write.xlsx2(as.data.frame(eri2G), file,sheetName="2G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
    )
    
    
    }
    
    if (("2G" %in% input$network) & ("HUAWEI" %in% input$provider)) {
      datos.2GHua <<- queryData2GHuawei(as.character(input$idate),as.character(input$fdate),input$cellcontext,input$gra)
      hua2G <<- filter2GHua(datos.2GHua,input$filter,input$cellfilter)
      output$ACC = rendermygraphOK(renderGraf(hua2G,3,input$gra))
      
      #output$ACC = rendermygraphOK(renderGraf(hua2G,3,input$gra))
      output$RET = rendermygraphOK(renderGraf(hua2G,4,input$gra))
      output$SER = rendermygraphOK(renderGraf(hua2G,5,input$gra))
      output$CONG_TCH = rendermygraphOK(renderGraf(hua2G,6,input$gra))
      output$CONG_SDCCH = rendermygraphOK(renderGraf(hua2G,7,input$gra))
      output$TRAF_TCH = rendermygraphOK(renderGraf(hua2G,8,input$gra))
      output$TRAF_SDCCH = rendermygraphOK(renderGraf(hua2G,9,input$gra))
      output$DROP_TCH= rendermygraphOK(renderGraf(hua2G,10,input$gra))
      output$DROP_SDCCH = rendermygraphOK(renderGraf(hua2G,11,input$gra))
      output$PAGINGS = rendermygraphOK(renderGraf(hua2G,12,input$gra))
      output$PAGINGS_PS = rendermygraphOK(renderGraf(hua2G,13,input$gra))
      output$TRAF_FR = rendermygraphOK(renderGraf(hua2G,14,input$gra))
      output$TRAF_HR = rendermygraphOK(renderGraf(hua2G,15,input$gra))

      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.xlsx', sep = '')
        },
        content = function(file) {
          write.xlsx2(as.data.frame(hua2G), file,sheetName="2G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
      
    }
    
    if (("2G" %in% input$network) & ("NOKIA" %in% input$provider)){
      datos.2GNok <<- queryData2GNokia(as.character(input$idate),as.character(input$fdate),input$gra,input$cellcontext)
      dat <- datos.2GNok
      Nok2G <<- filter2GNok(datos.2GNok,input$filter,input$cellfilter,input$gra)
      
      output$ACC = rendermygraphOK(renderGraf(Nok2G,3,input$gra))
      output$RET = rendermygraphOK(renderGraf(Nok2G,4,input$gra))
      output$SER = rendermygraphOK(renderGraf(Nok2G,5,input$gra))
      output$TRAF_TCH = rendermygraphOK(renderGraf(Nok2G,6,input$gra))
      output$TRAF_FR = rendermygraphOK(renderGraf(Nok2G,7,input$gra))
      output$TRAF_HR = rendermygraphOK(renderGraf(Nok2G,8,input$gra))
      output$TRAF_SDCCH = rendermygraphOK(renderGraf(Nok2G,9,input$gra))
      output$CONG_TCH = rendermygraphOK(renderGraf(Nok2G,11,input$gra))
      output$CONG_SDCCH = rendermygraphOK(renderGraf(Nok2G,10,input$gra))
      output$DROP_TCH= rendermygraphOK(renderGraf(Nok2G,12,input$gra))
      output$DROP_SDCCH = rendermygraphOK(renderGraf(Nok2G,13,input$gra))
      output$PAGINGS = rendermygraphOK(renderGraf(Nok2G,14,input$gra))
      
      output$INT4_5 = rendermygraphOK(renderGraf(Nok2G,15,input$gra))
      output$INT_4 = rendermygraphOK(renderGraf(Nok2G,16,input$gra))
      output$INT_5 = rendermygraphOK(renderGraf(Nok2G,17,input$gra))
      output$INT4_5P = rendermygraphOK(renderGraf(Nok2G,18,input$gra))
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          write.csv(as.data.frame(Nok2G),file,col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
      #output$VOL_UL = rendermygraphOK(renderGraf(Nok2G,15,input$gra))
      #output$VOL_DL = rendermygraphOK(renderGraf(Nok2G,16,input$gra))
      #output$PRUEBA = rendermygraphOK(renderGraf(Nok2G,15,input$gra))
      
    }

    
    if (("3G" %in% input$network) & ("ERICSSON" %in% input$provider)) {
      datos.3GEri <<- queryData3GEricsson (as.character(input$idate),as.character(input$fdate),input$cellcontext,input$gra)
      eri3G <<- filter3GEri(datos.3GEri,input$filter,input$cellfilter,input$gra)
      
      output$ACC_CS = rendermygraphOK(renderGraf(eri3G,3,input$gra))
      output$ACC_PS = rendermygraphOK(renderGraf(eri3G,4,input$gra))
      output$RET_CS = rendermygraphOK(renderGraf(eri3G,5,input$gra))
      output$RET_PS = rendermygraphOK(renderGraf(eri3G,6,input$gra))
      output$CE_RRC = rendermygraphOK(renderGraf(eri3G,7,input$gra))
      output$PWR_RRC = rendermygraphOK(renderGraf(eri3G,8,input$gra))
      output$CODE_RRC = rendermygraphOK(renderGraf(eri3G,9,input$gra))
      output$CE_RAB = rendermygraphOK(renderGraf(eri3G,10,input$gra))
      output$PWR_RAB = rendermygraphOK(renderGraf(eri3G,11,input$gra))
      output$CODE_RAB = rendermygraphOK(renderGraf(eri3G,12,input$gra))
      output$RTWP = rendermygraphOK(renderGraf(eri3G,13,input$gra))
      output$DROP_CS = rendermygraphOK(renderGraf(eri3G,17,input$gra))
      output$DROP_PS = rendermygraphOK(renderGraf(eri3G,18,input$gra))
      output$VOL = rendermygraphOK(renderGraf(eri3G,16,input$gra))
      output$TRAF_CS = rendermygraphOK(renderGraf(eri3G,15,input$gra))
      
      output$VOL_HSDPA = rendermygraphOK(renderGraf(eri3G,19,input$gra))
      output$VOL_HSUPA = rendermygraphOK(renderGraf(eri3G,20,input$gra))
      output$VOL_R99_DL = rendermygraphOK(renderGraf(eri3G,21,input$gra))
      output$VOL_R99_UL = rendermygraphOK(renderGraf(eri3G,22,input$gra))
      output$TRAF_PS = rendermygraphOK(renderGraf(eri3G,23,input$gra))
      output$TRAF_TOT = rendermygraphOK(renderGraf(eri3G,24,input$gra))
      output$ACC_PS_RRC = rendermygraphOK(renderGraf(eri3G,25,input$gra))
      output$C1 = rendermygraphOK(renderGraf(eri3G,26,input$gra))
      output$C2 = rendermygraphOK(renderGraf(eri3G,27,input$gra))
      output$C3 = rendermygraphOK(renderGraf(eri3G,28,input$gra))
      output$AVAIL = rendermygraphOK(renderGraf(eri3G,29,input$gra))
      
    
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          write.csv(as.data.frame(eri3G), file,col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
    
    }
    
    if (("3G" %in% input$network) & ("HUAWEI" %in% input$provider)) {
      datos.3GHua <<- queryData3GHuawei(as.character(input$idate),as.character(input$fdate),input$cellcontext,input$gra)
      hua3G <<- filter3GHua(datos.3GHua,input$filter,input$cellfilter,input$gra)
      
      output$ACC_CS = rendermygraphOK(renderGraf(hua3G,3,input$gra))
      output$ACC_PS = rendermygraphOK(renderGraf(hua3G,4,input$gra))
      output$RET_CS = rendermygraphOK(renderGraf(hua3G,5,input$gra))
      output$RET_PS = rendermygraphOK(renderGraf(hua3G,6,input$gra))
      output$CE_RRC = rendermygraphOK(renderGraf(hua3G,7,input$gra))
      output$PWR_RRC = rendermygraphOK(renderGraf(hua3G,8,input$gra))
      output$CODE_RRC = rendermygraphOK(renderGraf(hua3G,9,input$gra))
      output$CE_RAB = rendermygraphOK(renderGraf(hua3G,10,input$gra))
      output$PWR_RAB = rendermygraphOK(renderGraf(hua3G,11,input$gra))
      output$CODE_RAB = rendermygraphOK(renderGraf(hua3G,12,input$gra))
      output$TP_MAX = rendermygraphOK(renderGraf(hua3G,13,input$gra))
      output$TP_AVG = rendermygraphOK(renderGraf(hua3G,14,input$gra))
      output$VOL = rendermygraphOK(renderGraf(hua3G,16,input$gra))
      output$DROP_CS = rendermygraphOK(renderGraf(hua3G,17,input$gra))
      output$DROP_PS = rendermygraphOK(renderGraf(hua3G,18,input$gra))
      output$USER_HSDPA = rendermygraphOK(renderGraf(hua3G,15,input$gra))
      output$USER_HSUPA = rendermygraphOK(renderGraf(hua3G,21,input$gra))
      output$TRAF_CS = rendermygraphOK(renderGraf(hua3G,19,input$gra))
      output$RTWP = rendermygraphOK(renderGraf(hua3G,20,input$gra))
      
      output$USER_DL_3G = rendermygraphOK(renderGraf(hua3G,22,input$gra))
      output$MAX_USER_DL_3G = rendermygraphOK(renderGraf(hua3G,23,input$gra))
      output$USER_UL_3G = rendermygraphOK(renderGraf(hua3G,24,input$gra))
      output$MAX_USER_UL_3G = rendermygraphOK(renderGraf(hua3G,25,input$gra))
      
      output$PAG_DIS_3G = rendermygraphOK(renderGraf(hua3G,26,input$gra))
      output$PAG_SEN_3G = rendermygraphOK(renderGraf(hua3G,27,input$gra))
      output$PAG_3G = rendermygraphOK(renderGraf(hua3G,28,input$gra))
      
      output$ACC_RRC_CS = rendermygraphOK(renderGraf(hua3G,29,input$gra))
      output$ACC_RAB_CS = rendermygraphOK(renderGraf(hua3G,30,input$gra))
      output$ACC_RRC_PS = rendermygraphOK(renderGraf(hua3G,31,input$gra))
      output$ACC_RAB_PS = rendermygraphOK(renderGraf(hua3G,32,input$gra))
      
      output$VOL_HSDPA = rendermygraphOK(renderGraf(hua3G,33,input$gra))
      output$VOL_HSUPA = rendermygraphOK(renderGraf(hua3G,34,input$gra))
      
      output$USER_DCH = rendermygraphOK(renderGraf(hua3G,35,input$gra))
      output$USER_PCH = rendermygraphOK(renderGraf(hua3G,36,input$gra))
      output$USER_FACH = rendermygraphOK(renderGraf(hua3G,37,input$gra))
      
      
      output$PWR_RAB_DL = rendermygraphOK(renderGraf(hua3G,38,input$gra))
      output$PWR_RAB_UL = rendermygraphOK(renderGraf(hua3G,39,input$gra))
      
      output$TP_MAX_U = rendermygraphOK(renderGraf(hua3G,44,input$gra))
      output$TP_AVG_U = rendermygraphOK(renderGraf(hua3G,45,input$gra))
      
      
      output$CPICH_PWR = rendermygraphOK(renderGraf(hua3G,46,input$gra))
      output$CPICH_PWR_RTWP = rendermygraphOK(renderGraf(hua3G,47,input$gra))
      output$LDR_PWR_DL = rendermygraphOK(renderGraf(hua3G,48,input$gra))
      output$LDR_PWR_UL = rendermygraphOK(renderGraf(hua3G,49,input$gra))

      #output$NUM_RAB = rendermygraphOK(renderGraf(hua3G,50,input$gra))
      #output$DEN_RAB = rendermygraphOK(renderGraf(hua3G,51,input$gra))

      
     
      
      if("ByCell" %in% input$filter){
        hua3G$CARRIER <- 0
        hua3G$SECTOR <- 0
        
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) <= 4, 1 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) >= 5, 2 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) == "I", 3 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) == "J", 3 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) == "K", 3 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) == "L", 3 , hua3G$CARRIER)
        
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 1, 1 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 2, 2 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 3, 3 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 4, 4 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 5, 1 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 6, 2 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 7, 3 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 8, 4 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == "I", 1 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == "J", 2 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == "K", 3 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == "L", 4 , hua3G$SECTOR)
      }
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          write.csv(as.data.frame(hua3G), file,col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
      
    }
    
    if (("3G" %in% input$network) & ("NOKIA" %in% input$provider)){
      datos.3GNok <<- queryData3GNokia(as.character(input$idate),as.character(input$fdate),input$gra,input$cellcontext)
      Nok3G <<- filter3GNok(datos.3GNok,input$filter,input$cellfilter)
      
      output$ACC_CS = rendermygraphOK(renderGraf(Nok3G,3,input$gra))
      output$ACC_PS = rendermygraphOK(renderGraf(Nok3G,4,input$gra))
      output$RET_CS = rendermygraphOK(renderGraf(Nok3G,5,input$gra))
      output$RET_PS = rendermygraphOK(renderGraf(Nok3G,6,input$gra))
      output$CE_RRC = rendermygraphOK(renderGraf(Nok3G,17,input$gra))
      output$PWR_RRC = rendermygraphOK(renderGraf(Nok3G,7,input$gra))
      output$CODE_RRC = rendermygraphOK(renderGraf(Nok3G,19,input$gra))
      output$CE_RAB = rendermygraphOK(renderGraf(Nok3G,18,input$gra))
      output$PWR_RAB = rendermygraphOK(renderGraf(Nok3G,8,input$gra))
      output$TRAF_CS = rendermygraphOK(renderGraf(Nok3G,9,input$gra))
      output$CODE_RAB = rendermygraphOK(renderGraf(Nok3G,20,input$gra))
      #output$TP_MAX = rendermygraphOK(renderGraf(hua3G,13,input$gra))
      #output$TP_AVG = rendermygraphOK(renderGraf(hua3G,14,input$gra))
      output$VOL = rendermygraphOK(renderGraf(Nok3G,10,input$gra))
      #output$DROP_CS = rendermygraphOK(renderGraf(hua3G,17,input$gra))
      #output$DROP_PS = rendermygraphOK(renderGraf(hua3G,18,input$gra))
      output$USER_HSDPA = rendermygraphOK(renderGraf(Nok3G,11,input$gra))
      output$USER_HSUPA = rendermygraphOK(renderGraf(Nok3G,12,input$gra))
      output$VOL_HSDPA = rendermygraphOK(renderGraf(Nok3G,13,input$gra))
      output$VOL_HSUPA = rendermygraphOK(renderGraf(Nok3G,14,input$gra))
      output$CONG_PWR_BGR = rendermygraphOK(renderGraf(Nok3G,15,input$gra))
      output$CONG_PWR_INT = rendermygraphOK(renderGraf(Nok3G,16,input$gra))
      
      output$REJ_DCH_DUE_CODES_INT_DL = rendermygraphOK(renderGraf(Nok3G,21,input$gra))
      output$REJ_DCH_DUE_CODES_BGR_DL = rendermygraphOK(renderGraf(Nok3G,22,input$gra))
      output$PS_SETUP_FAIL_AC_COD_NRT = rendermygraphOK(renderGraf(Nok3G,23,input$gra))
      output$PS_SETUP_FAIL_AC_DL_NRT = rendermygraphOK(renderGraf(Nok3G,24,input$gra))
      output$PS_SETUP_FAIL_AC_UL_NRT = rendermygraphOK(renderGraf(Nok3G,25,input$gra))
      output$RAB_STP_FAIL_PS_STREA_BTS = rendermygraphOK(renderGraf(Nok3G,26,input$gra))
      
      output$VOL_R99_DL = rendermygraphOK(renderGraf(Nok3G,27,input$gra))
      output$VOL_R99_UL = rendermygraphOK(renderGraf(Nok3G,28,input$gra))
      
      output$PAG_DIS_3G = rendermygraphOK(renderGraf(Nok3G,29,input$gra))
      output$PAG_DIS_URA_3G = rendermygraphOK(renderGraf(Nok3G,30,input$gra))
      output$PAG_DIS_PCH_3G = rendermygraphOK(renderGraf(Nok3G,31,input$gra))
      
      output$PAG_SEN_3G = rendermygraphOK(renderGraf(Nok3G,32,input$gra))
      output$PAG_SEN_URA_3G = rendermygraphOK(renderGraf(Nok3G,33,input$gra))
      output$PAG_SEN_PCH_3G = rendermygraphOK(renderGraf(Nok3G,34,input$gra))
      
      output$ATT_FACH_TO_HS_DSCH = rendermygraphOK(renderGraf(Nok3G,35,input$gra))
      output$ATT_HS_DSCH_TO_FACH = rendermygraphOK(renderGraf(Nok3G,36,input$gra))
      
      output$SUCC_HS_DSCH_TO_FACH = rendermygraphOK(renderGraf(Nok3G,37,input$gra))
      output$SUCC_FACH_TO_HS_DSCH = rendermygraphOK(renderGraf(Nok3G,38,input$gra))
      
      output$CONG_HSDPA_USER = rendermygraphOK(renderGraf(Nok3G,39,input$gra))
      output$CONG_HSUPA_USER = rendermygraphOK(renderGraf(Nok3G,40,input$gra))
      
      output$NO_ACC_HSDPA = rendermygraphOK(renderGraf(Nok3G,41,input$gra))
      output$NO_ACC_HSUPA = rendermygraphOK(renderGraf(Nok3G,42,input$gra))
      
      output$TP_MAX = rendermygraphOK(renderGraf(Nok3G,47,input$gra))
      output$TP_AVG = rendermygraphOK(renderGraf(Nok3G,48,input$gra))
      
      output$RTWP = rendermygraphOK(renderGraf(Nok3G,49,input$gra))
      output$AVAIL = rendermygraphOK(renderGraf(Nok3G,50,input$gra))
      output$HSDPA_ACC = rendermygraphOK(renderGraf(Nok3G,51,input$gra))
      output$HSUPA_ACC = rendermygraphOK(renderGraf(Nok3G,52,input$gra))
  
      output$UNSC_SHO_N = rendermygraphOK(renderGraf(Nok3G,53,input$gra))
      output$UNSC_SHO_R = rendermygraphOK(renderGraf(Nok3G,54,input$gra))
      output$SUCC_SHO = rendermygraphOK(renderGraf(Nok3G,55,input$gra))
      output$DROP_CS = rendermygraphOK(renderGraf(Nok3G,56,input$gra))
      output$DROP_PS = rendermygraphOK(renderGraf(Nok3G,57,input$gra))
      output$PS_SETUP_FAIL_AC_INT = rendermygraphOK(renderGraf(Nok3G,58,input$gra))
      output$PS_SETUP_FAIL_AC_BGR = rendermygraphOK(renderGraf(Nok3G,59,input$gra))
      output$USER_16 = rendermygraphOK(renderGraf(Nok3G,61,input$gra))
      output$MEH_HSDSCH = rendermygraphOK(renderGraf(Nok3G,62,input$gra))
      output$MEH_CS_VOICE = rendermygraphOK(renderGraf(Nok3G,63,input$gra))
      output$HSPA_RT_O_NRT = rendermygraphOK(renderGraf(Nok3G,64,input$gra))
      
      output$ACC_RRC_CS = rendermygraphOK(renderGraf(Nok3G,65,input$gra))
      output$ACC_RAB_CS = rendermygraphOK(renderGraf(Nok3G,66,input$gra))
      output$ACC_RRC_PS = rendermygraphOK(renderGraf(Nok3G,67,input$gra))
      output$ACC_RAB_PS = rendermygraphOK(renderGraf(Nok3G,68,input$gra))
      
      output$ATT_DCH_TO_PCH = rendermygraphOK(renderGraf(Nok3G,69,input$gra))
      output$ATT_PCH_TO_DCH = rendermygraphOK(renderGraf(Nok3G,70,input$gra))
      output$ATT_FACH_TO_PCH = rendermygraphOK(renderGraf(Nok3G,71,input$gra))
      output$ATT_PCH_TO_FACH = rendermygraphOK(renderGraf(Nok3G,72,input$gra))
      
      output$HSUPA_2MS = rendermygraphOK(renderGraf(Nok3G,73,input$gra))
      output$HSUPA_10MS = rendermygraphOK(renderGraf(Nok3G,74,input$gra))
      
      output$SETUP_FAIL_IUB_HS = rendermygraphOK(renderGraf(Nok3G,75,input$gra))
      output$REJ_HS_DSCH = rendermygraphOK(renderGraf(Nok3G,76,input$gra))
      output$REJ_E_DCH = rendermygraphOK(renderGraf(Nok3G,77,input$gra))
      
      output$SCCPCH_LOAD = rendermygraphOK(renderGraf(Nok3G,78,input$gra))
      output$FACH_u_LOAD = rendermygraphOK(renderGraf(Nok3G,79,input$gra))
      output$FACH_c_LOAD = rendermygraphOK(renderGraf(Nok3G,80,input$gra))
      output$RACH_u_LOAD = rendermygraphOK(renderGraf(Nok3G,81,input$gra))
      output$RACH_c_LOAD = rendermygraphOK(renderGraf(Nok3G,82,input$gra))
      
      output$AVG_CODE_TREE = rendermygraphOK(renderGraf(Nok3G,83,input$gra))
      output$MAX_CODE_TREE = rendermygraphOK(renderGraf(Nok3G,84,input$gra))
      output$MIN_CODE_TREE = rendermygraphOK(renderGraf(Nok3G,85,input$gra))
      output$CODE_BLOCK_DL = rendermygraphOK(renderGraf(Nok3G,86,input$gra))
      
      output$PAG_DIS_CIENTO = rendermygraphOK(renderGraf(Nok3G,87,input$gra))
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          write.csv(as.data.frame(Nok3G),file,col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
      
    }
      
    if (("4G" %in% input$network) & ("ERICSSON" %in% input$provider)){
      datos.4GEri <<- queryData4GEricsson(as.character(input$idate),as.character(input$fdate),input$cellcontext,input$gra)
      Eri4G <<- filter4GEri(datos.4GEri,input$filter,input$cellfilter)
      output$ACC_4G = rendermygraphOK(renderGraf(Eri4G,3,input$gra))
      output$RET_4G = rendermygraphOK(renderGraf(Eri4G,4,input$gra))
      output$VOLUMEN_DL = rendermygraphOK(renderGraf(Eri4G,5,input$gra))
      output$VOLUMEN_UL = rendermygraphOK(renderGraf(Eri4G,6,input$gra))
      output$TP_DL = rendermygraphOK(renderGraf(Eri4G,7,input$gra))
      output$TP_UL = rendermygraphOK(renderGraf(Eri4G,8,input$gra))
      output$DROP_ERAB = rendermygraphOK(renderGraf(Eri4G,9,input$gra))
      output$LAT = rendermygraphOK(renderGraf(Eri4G,10,input$gra))
      output$USER_DL = rendermygraphOK(renderGraf(Eri4G,11,input$gra)) 
      output$USER_UL = rendermygraphOK(renderGraf(Eri4G,12,input$gra))
      output$USER_MAX_DL = rendermygraphOK(renderGraf(Eri4G,14,input$gra)) 
      output$USER_MAX_UL = rendermygraphOK(renderGraf(Eri4G,13,input$gra)) 
      
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Ericsson', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          write.csv(as.data.frame(Eri4G),file,col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
    }
      
    
    }else{
      if (("3G" %in% input$network) & ("NOKIA" %in% input$provider)){
        datos.3GNokBTS <<- queryDataBTS3GNokia(as.character(input$idate),as.character(input$fdate),input$gra,input$cellcontext)
        Nok3GBTS <<- filter3GNokBTS(datos.3GNokBTS,input$filter,input$cellfilter)
        
        output$AVG_USED_CE_R99_DL = rendermygraphOK(renderGraf(Nok3GBTS,3,input$gra))
        output$AVG_USED_CE_R99_UL = rendermygraphOK(renderGraf(Nok3GBTS,4,input$gra))
        output$AVG_USED_CE_R99 = rendermygraphOK(renderGraf(Nok3GBTS,5,input$gra))
        
        output$MAX_USED_CE_R99_DL = rendermygraphOK(renderGraf(Nok3GBTS,6,input$gra))
        output$MAX_USED_CE_R99_UL = rendermygraphOK(renderGraf(Nok3GBTS,7,input$gra))
        output$MAX_USED_CE_R99 = rendermygraphOK(renderGraf(Nok3GBTS,8,input$gra))
        
        output$AVG_USED_SU = rendermygraphOK(renderGraf(Nok3GBTS,9,input$gra))
        output$MAX_USED_SU = rendermygraphOK(renderGraf(Nok3GBTS,10,input$gra))
        
        output$AVG_USED_HSUPA_SU = rendermygraphOK(renderGraf(Nok3GBTS,11,input$gra))
        output$MAX_USED_HSUPA_SU = rendermygraphOK(renderGraf(Nok3GBTS,12,input$gra))
        
        output$AVG_HSDPA_USER = rendermygraphOK(renderGraf(Nok3GBTS,13,input$gra))
        output$MAX_HSUPA_USER = rendermygraphOK(renderGraf(Nok3GBTS,14,input$gra))
        output$AVG_HSDPA_USER = rendermygraphOK(renderGraf(Nok3GBTS,15,input$gra))
        output$MAX_HSUPA_USER = rendermygraphOK(renderGraf(Nok3GBTS,16,input$gra))
        
     
        
        output$EstadoRed <- downloadHandler(
          filename = function() {
            paste('Red Movil ', Sys.Date(), '.csv', sep = '')
          },
          content = function(file) {
            write.csv(as.data.frame(Nok3GBTS),file,col.names=TRUE, row.names = TRUE ,append = TRUE)
          }
        )
      }
    }
    
    removeNotification(id)
  
  })
  observeEvent(input$fbutton, {
    id <- showNotification("Ejecutando Filtrado",duration =NULL)
    if( "CELL" %in%  input$type){
    
    if (("2G" %in% input$network) & ("ERICSSON" %in% input$provider)) {
      eri2G <<- filter2GEri(datos.2GEri,input$filter,input$cellfilter,input$gra)
      
      output$ACC = rendermygraphOK(renderGraf(eri2G,3,input$gra))
      output$RET = rendermygraphOK(renderGraf(eri2G,4,input$gra))
      output$SER = rendermygraphOK(renderGraf(eri2G,5,input$gra))
      output$CONG_TCH = rendermygraphOK(renderGraf(eri2G,6,input$gra))
      output$CONG_SDCCH = rendermygraphOK(renderGraf(eri2G,7,input$gra))
      output$TRAF_TCH = rendermygraphOK(renderGraf(eri2G,10,input$gra))
      output$TRAF_SDCCH = rendermygraphOK(renderGraf(eri2G,11,input$gra))
      output$DROP_TCH= rendermygraphOK(renderGraf(eri2G,12,input$gra))
      output$DROP_SDCCH = rendermygraphOK(renderGraf(eri2G,13,input$gra))
      output$PAGINGS = rendermygraphOK(renderGraf(eri2G,9,input$gra))
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.xlsx', sep = '')
        },
        content = function(file) {
          write.xlsx2(as.data.frame(eri2G), file,sheetName="2G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
    }
    
    if (("2G" %in% input$network) & ("HUAWEI" %in% input$provider)) {
      hua2G <- filter2GHua(datos.2GHua,input$filter,input$cellfilter)
      
      output$ACC = rendermygraphOK(renderGraf(hua2G,3,input$gra))
      output$RET = rendermygraphOK(renderGraf(hua2G,4,input$gra))
      output$SER = rendermygraphOK(renderGraf(hua2G,5,input$gra))
      output$CONG_TCH = rendermygraphOK(renderGraf(hua2G,6,input$gra))
      output$CONG_SDCCH = rendermygraphOK(renderGraf(hua2G,7,input$gra))
      output$TRAF_TCH = rendermygraphOK(renderGraf(hua2G,8,input$gra))
      output$TRAF_SDCCH = rendermygraphOK(renderGraf(hua2G,9,input$gra))
      output$DROP_TCH= rendermygraphOK(renderGraf(hua2G,10,input$gra))
      output$DROP_SDCCH = rendermygraphOK(renderGraf(hua2G,11,input$gra))
      output$PAGINGS = rendermygraphOK(renderGraf(hua2G,12,input$gra))
      output$PAGINGS_PS = rendermygraphOK(renderGraf(hua2G,13,input$gra))
      output$TRAF_FR = rendermygraphOK(renderGraf(hua2G,14,input$gra))
      output$TRAF_HR = rendermygraphOK(renderGraf(hua2G,15,input$gra))
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.xlsx', sep = '')
        },
        content = function(file) {
          write.xlsx2(as.data.frame(hua2G), file,sheetName="2G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
      
    }
    
    if (("2G" %in% input$network) & ("NOKIA" %in% input$provider)){
        Nok2G <- filter2GNok(datos.2GNok,input$filter,input$cellfilter,input$gra)
        
        output$ACC = rendermygraphOK(renderGraf(Nok2G,3,input$gra))
        output$RET = rendermygraphOK(renderGraf(Nok2G,4,input$gra))
        output$SER = rendermygraphOK(renderGraf(Nok2G,5,input$gra))
        output$TRAF_TCH = rendermygraphOK(renderGraf(Nok2G,6,input$gra))
        output$TRAF_FR = rendermygraphOK(renderGraf(Nok2G,7,input$gra))
        output$TRAF_HR = rendermygraphOK(renderGraf(Nok2G,8,input$gra))
        output$TRAF_SDCCH = rendermygraphOK(renderGraf(Nok2G,9,input$gra))
        output$CONG_TCH = rendermygraphOK(renderGraf(Nok2G,11,input$gra))
        output$CONG_SDCCH = rendermygraphOK(renderGraf(Nok2G,10,input$gra))
        output$DROP_TCH= rendermygraphOK(renderGraf(Nok2G,12,input$gra))
        output$DROP_SDCCH = rendermygraphOK(renderGraf(Nok2G,13,input$gra))
        output$PAGINGS = rendermygraphOK(renderGraf(Nok2G,14,input$gra))

        output$INT4_5 = rendermygraphOK(renderGraf(Nok2G,15,input$gra))
        output$INT_4 = rendermygraphOK(renderGraf(Nok2G,16,input$gra))
        output$INT_5 = rendermygraphOK(renderGraf(Nok2G,17,input$gra))
        output$INT4_5P = rendermygraphOK(renderGraf(Nok2G,18,input$gra))
        
        output$EstadoRed <- downloadHandler(
          filename = function() {
            paste('Red Movil ', Sys.Date(), '.csv', sep = '')
          },
          content = function(file) {
            write.csv(as.data.frame(Nok2G),file,col.names=TRUE, row.names = TRUE ,append = TRUE)
          }
        )
        
    }
    
    if (("3G" %in% input$network) & ("ERICSSON" %in% input$provider)) {
      eri3G <- filter3GEri(datos.3GEri,input$filter,input$cellfilter,input$gra)
      
      output$ACC_CS = rendermygraphOK(renderGraf(eri3G,3,input$gra))
      output$ACC_PS = rendermygraphOK(renderGraf(eri3G,4,input$gra))
      output$RET_CS = rendermygraphOK(renderGraf(eri3G,5,input$gra))
      output$RET_PS = rendermygraphOK(renderGraf(eri3G,6,input$gra))
      output$CE_RRC = rendermygraphOK(renderGraf(eri3G,7,input$gra))
      output$PWR_RRC = rendermygraphOK(renderGraf(eri3G,8,input$gra))
      output$CODE_RRC = rendermygraphOK(renderGraf(eri3G,9,input$gra))
      output$CE_RAB = rendermygraphOK(renderGraf(eri3G,10,input$gra))
      output$PWR_RAB = rendermygraphOK(renderGraf(eri3G,11,input$gra))
      output$CODE_RAB = rendermygraphOK(renderGraf(eri3G,12,input$gra))
      output$RTWP = rendermygraphOK(renderGraf(eri3G,13,input$gra))
      output$DROP_CS = rendermygraphOK(renderGraf(eri3G,17,input$gra))
      output$DROP_PS = rendermygraphOK(renderGraf(eri3G,18,input$gra))
      output$VOL = rendermygraphOK(renderGraf(eri3G,16,input$gra))
      output$TRAF_CS = rendermygraphOK(renderGraf(eri3G,15,input$gra))
      
      output$VOL_HSDPA = rendermygraphOK(renderGraf(eri3G,19,input$gra))
      output$VOL_HSUPA = rendermygraphOK(renderGraf(eri3G,20,input$gra))
      output$VOL_R99_DL = rendermygraphOK(renderGraf(eri3G,21,input$gra))
      output$VOL_R99_UL = rendermygraphOK(renderGraf(eri3G,22,input$gra))
      output$TRAF_PS = rendermygraphOK(renderGraf(eri3G,23,input$gra))
      output$TRAF_TOT = rendermygraphOK(renderGraf(eri3G,24,input$gra))
      output$ACC_PS_RRC = rendermygraphOK(renderGraf(eri3G,25,input$gra))
      output$C1 = rendermygraphOK(renderGraf(eri3G,26,input$gra))
      output$C2 = rendermygraphOK(renderGraf(eri3G,27,input$gra))
      output$C3 = rendermygraphOK(renderGraf(eri3G,28,input$gra))
      output$AVAIL = rendermygraphOK(renderGraf(eri3G,29,input$gra))
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          write.csv(as.data.frame(eri3G), file,col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
      
    }
    
    if (("3G" %in% input$network) & ("HUAWEI" %in% input$provider)) {
      hua3G <- filter3GHua(datos.3GHua,input$filter,input$cellfilter,input$gra)
      str(hua3G)
      output$ACC_CS = rendermygraphOK(renderGraf(hua3G,3,input$gra))
      output$ACC_PS = rendermygraphOK(renderGraf(hua3G,4,input$gra))
      output$RET_CS = rendermygraphOK(renderGraf(hua3G,5,input$gra))
      output$RET_PS = rendermygraphOK(renderGraf(hua3G,6,input$gra))
      output$CE_RRC = rendermygraphOK(renderGraf(hua3G,7,input$gra))
      output$PWR_RRC = rendermygraphOK(renderGraf(hua3G,8,input$gra))
      output$CODE_RRC = rendermygraphOK(renderGraf(hua3G,9,input$gra))
      output$CE_RAB = rendermygraphOK(renderGraf(hua3G,10,input$gra))
      output$PWR_RAB = rendermygraphOK(renderGraf(hua3G,11,input$gra))
      output$CODE_RAB = rendermygraphOK(renderGraf(hua3G,12,input$gra))
      output$TP_MAX = rendermygraphOK(renderGraf(hua3G,13,input$gra))
      output$TP_AVG = rendermygraphOK(renderGraf(hua3G,14,input$gra))
      output$VOL = rendermygraphOK(renderGraf(hua3G,16,input$gra))
      output$DROP_CS = rendermygraphOK(renderGraf(hua3G,17,input$gra))
      output$DROP_PS = rendermygraphOK(renderGraf(hua3G,18,input$gra))
      output$USER_HSDPA = rendermygraphOK(renderGraf(hua3G,15,input$gra))
      output$USER_HSUPA = rendermygraphOK(renderGraf(hua3G,21,input$gra))
      output$TRAF_CS = rendermygraphOK(renderGraf(hua3G,19,input$gra))
      output$RTWP = rendermygraphOK(renderGraf(hua3G,20,input$gra))
      
      output$USER_DL_3G = rendermygraphOK(renderGraf(hua3G,22,input$gra))
      output$MAX_USER_DL_3G = rendermygraphOK(renderGraf(hua3G,23,input$gra))
      output$USER_UL_3G = rendermygraphOK(renderGraf(hua3G,24,input$gra))
      output$MAX_USER_UL_3G = rendermygraphOK(renderGraf(hua3G,25,input$gra))
      
      output$PAG_DIS_3G = rendermygraphOK(renderGraf(hua3G,26,input$gra))
      output$PAG_SEN_3G = rendermygraphOK(renderGraf(hua3G,27,input$gra))
      output$PAG_3G = rendermygraphOK(renderGraf(hua3G,28,input$gra))
      
      output$ACC_RRC_CS = rendermygraphOK(renderGraf(hua3G,29,input$gra))
      output$ACC_RAB_CS = rendermygraphOK(renderGraf(hua3G,30,input$gra))
      output$ACC_RRC_PS = rendermygraphOK(renderGraf(hua3G,31,input$gra))
      output$ACC_RAB_PS = rendermygraphOK(renderGraf(hua3G,32,input$gra))

      output$VOL_HSDPA = rendermygraphOK(renderGraf(hua3G,33,input$gra))
      output$VOL_HSUPA = rendermygraphOK(renderGraf(hua3G,34,input$gra))
      
      output$USER_DCH = rendermygraphOK(renderGraf(hua3G,35,input$gra))
      output$USER_PCH = rendermygraphOK(renderGraf(hua3G,36,input$gra))
      output$USER_FACH = rendermygraphOK(renderGraf(hua3G,37,input$gra))
      
      output$PWR_RAB_DL = rendermygraphOK(renderGraf(hua3G,38,input$gra))
      output$PWR_RAB_UL = rendermygraphOK(renderGraf(hua3G,39,input$gra))
      output$TP_MAX_U = rendermygraphOK(renderGraf(hua3G,44,input$gra))
      output$TP_AVG_U = rendermygraphOK(renderGraf(hua3G,45,input$gra))
      
      output$CPICH_PWR = rendermygraphOK(renderGraf(hua3G,46,input$gra))
      output$CPICH_PWR_RTWP = rendermygraphOK(renderGraf(hua3G,47,input$gra))
      output$LDR_PWR_DL = rendermygraphOK(renderGraf(hua3G,48,input$gra))
      output$LDR_PWR_UL = rendermygraphOK(renderGraf(hua3G,49,input$gra))
      
      #output$NUM_RAB = rendermygraphOK(renderGraf(hua3G,50,input$gra))
      #output$DEN_RAB = rendermygraphOK(renderGraf(hua3G,51,input$gra))
      
       if("ByCell" %in% input$filter){
        hua3G$CARRIER <- 0
        hua3G$SECTOR <- 0
        
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) <= 4, 1 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) >= 5, 2 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) == "I", 3 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) == "J", 3 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) == "K", 3 , hua3G$CARRIER)
        hua3G$CARRIER = ifelse(substr(hua3G$CELDA,7,7) == "L", 3 , hua3G$CARRIER)
        
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 1, 1 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 2, 2 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 3, 3 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 4, 4 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 5, 1 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 6, 2 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 7, 3 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == 8, 4 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == "I", 1 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == "J", 2 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == "K", 3 , hua3G$SECTOR)
        hua3G$SECTOR = ifelse(substr(hua3G$CELDA,7,7) == "L", 4 , hua3G$SECTOR)
      }
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          write.csv(as.data.frame(hua3G), file,col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
    }
    
    if (("3G" %in% input$network) & ("NOKIA" %in% input$provider)){
      Nok3G <<- filter3GNok(datos.3GNok,input$filter,input$cellfilter)
      output$ACC_CS = rendermygraphOK(renderGraf(Nok3G,3,input$gra))
      output$ACC_PS = rendermygraphOK(renderGraf(Nok3G,4,input$gra))
      output$RET_CS = rendermygraphOK(renderGraf(Nok3G,5,input$gra))
      output$RET_PS = rendermygraphOK(renderGraf(Nok3G,6,input$gra))
      output$CE_RRC = rendermygraphOK(renderGraf(Nok3G,17,input$gra))
      output$PWR_RRC = rendermygraphOK(renderGraf(Nok3G,7,input$gra))
      output$CODE_RRC = rendermygraphOK(renderGraf(Nok3G,19,input$gra))
      output$CE_RAB = rendermygraphOK(renderGraf(Nok3G,18,input$gra))
      output$PWR_RAB = rendermygraphOK(renderGraf(Nok3G,8,input$gra))
      output$TRAF_CS = rendermygraphOK(renderGraf(Nok3G,9,input$gra))
      output$CODE_RAB = rendermygraphOK(renderGraf(Nok3G,20,input$gra))
      #output$TP_MAX = rendermygraphOK(renderGraf(hua3G,13,input$gra))
      #output$TP_AVG = rendermygraphOK(renderGraf(hua3G,14,input$gra))
      output$VOL = rendermygraphOK(renderGraf(Nok3G,10,input$gra))
      
      output$USER_HSDPA = rendermygraphOK(renderGraf(Nok3G,11,input$gra))
      output$USER_HSUPA = rendermygraphOK(renderGraf(Nok3G,12,input$gra))
      output$VOL_HSDPA = rendermygraphOK(renderGraf(Nok3G,13,input$gra))
      output$VOL_HSUPA = rendermygraphOK(renderGraf(Nok3G,14,input$gra))
      output$CONG_PWR_BGR = rendermygraphOK(renderGraf(Nok3G,15,input$gra))
      output$CONG_PWR_INT = rendermygraphOK(renderGraf(Nok3G,16,input$gra))
      
      output$REJ_DCH_DUE_CODES_INT_DL = rendermygraphOK(renderGraf(Nok3G,21,input$gra))
      output$REJ_DCH_DUE_CODES_BGR_DL = rendermygraphOK(renderGraf(Nok3G,22,input$gra))
      output$PS_SETUP_FAIL_AC_COD_NRT = rendermygraphOK(renderGraf(Nok3G,23,input$gra))
      output$PS_SETUP_FAIL_AC_DL_NRT = rendermygraphOK(renderGraf(Nok3G,24,input$gra))
      output$PS_SETUP_FAIL_AC_UL_NRT = rendermygraphOK(renderGraf(Nok3G,25,input$gra))
      output$RAB_STP_FAIL_PS_STREA_BTS = rendermygraphOK(renderGraf(Nok3G,26,input$gra))
      
      output$VOL_R99_DL = rendermygraphOK(renderGraf(Nok3G,27,input$gra))
      output$VOL_R99_UL = rendermygraphOK(renderGraf(Nok3G,28,input$gra))
      
      output$PAG_DIS_3G = rendermygraphOK(renderGraf(Nok3G,29,input$gra))
      output$PAG_DIS_URA_3G = rendermygraphOK(renderGraf(Nok3G,30,input$gra))
      output$PAG_DIS_PCH_3G = rendermygraphOK(renderGraf(Nok3G,31,input$gra))
      
      output$PAG_SEN_3G = rendermygraphOK(renderGraf(Nok3G,32,input$gra))
      output$PAG_SEN_URA_3G = rendermygraphOK(renderGraf(Nok3G,33,input$gra))
      output$PAG_SEN_PCH_3G = rendermygraphOK(renderGraf(Nok3G,34,input$gra))
      
      output$ATT_FACH_TO_HS_DSCH = rendermygraphOK(renderGraf(Nok3G,35,input$gra))
      output$ATT_HS_DSCH_TO_FACH = rendermygraphOK(renderGraf(Nok3G,36,input$gra))
      
      
      output$SUCC_HS_DSCH_TO_FACH = rendermygraphOK(renderGraf(Nok3G,37,input$gra))
      output$SUCC_FACH_TO_HS_DSCH = rendermygraphOK(renderGraf(Nok3G,38,input$gra))
      
      output$CONG_HSDPA_USER = rendermygraphOK(renderGraf(Nok3G,39,input$gra))
      output$CONG_HSUPA_USER = rendermygraphOK(renderGraf(Nok3G,40,input$gra))
      
      output$NO_ACC_HSDPA = rendermygraphOK(renderGraf(Nok3G,41,input$gra))
      output$NO_ACC_HSUPA = rendermygraphOK(renderGraf(Nok3G,42,input$gra))
      
      output$TP_MAX = rendermygraphOK(renderGraf(Nok3G,47,input$gra))
      output$TP_AVG = rendermygraphOK(renderGraf(Nok3G,48,input$gra))
      
      output$RTWP = rendermygraphOK(renderGraf(Nok3G,49,input$gra))
      output$AVAIL = rendermygraphOK(renderGraf(Nok3G,50,input$gra))
      output$HSDPA_ACC = rendermygraphOK(renderGraf(Nok3G,51,input$gra))
      output$HSUPA_ACC = rendermygraphOK(renderGraf(Nok3G,52,input$gra))
      
      output$UNSC_SHO_N = rendermygraphOK(renderGraf(Nok3G,53,input$gra))
      output$UNSC_SHO_R = rendermygraphOK(renderGraf(Nok3G,54,input$gra))
      output$SUCC_SHO = rendermygraphOK(renderGraf(Nok3G,55,input$gra))
      
      output$DROP_CS = rendermygraphOK(renderGraf(Nok3G,56,input$gra))
      output$DROP_PS = rendermygraphOK(renderGraf(Nok3G,57,input$gra))
      
      output$PS_SETUP_FAIL_AC_INT = rendermygraphOK(renderGraf(Nok3G,58,input$gra))
      output$PS_SETUP_FAIL_AC_BGR = rendermygraphOK(renderGraf(Nok3G,59,input$gra))
      
      output$USER_16 = rendermygraphOK(renderGraf(Nok3G,61,input$gra))
      output$MEH_HSDSCH = rendermygraphOK(renderGraf(Nok3G,62,input$gra))
      output$MEH_CS_VOICE = rendermygraphOK(renderGraf(Nok3G,63,input$gra))
      output$HSPA_RT_O_NRT = rendermygraphOK(renderGraf(Nok3G,64,input$gra))
      
      output$ACC_RRC_CS = rendermygraphOK(renderGraf(Nok3G,65,input$gra))
      output$ACC_RAB_CS = rendermygraphOK(renderGraf(Nok3G,66,input$gra))
      output$ACC_RRC_PS = rendermygraphOK(renderGraf(Nok3G,67,input$gra))
      output$ACC_RAB_PS = rendermygraphOK(renderGraf(Nok3G,68,input$gra))
      
      output$ATT_DCH_TO_PCH = rendermygraphOK(renderGraf(Nok3G,69,input$gra))
      output$ATT_PCH_TO_DCH = rendermygraphOK(renderGraf(Nok3G,70,input$gra))
      output$ATT_FACH_TO_PCH = rendermygraphOK(renderGraf(Nok3G,71,input$gra))
      output$ATT_PCH_TO_FACH = rendermygraphOK(renderGraf(Nok3G,72,input$gra))
      
      output$HSUPA_2MS = rendermygraphOK(renderGraf(Nok3G,73,input$gra))
      output$HSUPA_10MS = rendermygraphOK(renderGraf(Nok3G,74,input$gra))
      
      output$SETUP_FAIL_IUB_HS = rendermygraphOK(renderGraf(Nok3G,75,input$gra))
      output$REJ_HS_DSCH = rendermygraphOK(renderGraf(Nok3G,76,input$gra))
      output$REJ_E_DCH = rendermygraphOK(renderGraf(Nok3G,77,input$gra))
      
      output$SCCPCH_LOAD = rendermygraphOK(renderGraf(Nok3G,78,input$gra))
      output$FACH_u_LOAD = rendermygraphOK(renderGraf(Nok3G,79,input$gra))
      output$FACH_c_LOAD = rendermygraphOK(renderGraf(Nok3G,80,input$gra))
      output$RACH_u_LOAD = rendermygraphOK(renderGraf(Nok3G,81,input$gra))
      output$RACH_c_LOAD = rendermygraphOK(renderGraf(Nok3G,82,input$gra))
      
      output$AVG_CODE_TREE = rendermygraphOK(renderGraf(Nok3G,83,input$gra))
      output$MAX_CODE_TREE = rendermygraphOK(renderGraf(Nok3G,84,input$gra))
      output$MIN_CODE_TREE = rendermygraphOK(renderGraf(Nok3G,85,input$gra))
      output$CODE_BLOCK_DL = rendermygraphOK(renderGraf(Nok3G,86,input$gra))
      
      output$PAG_DIS_CIENTO = rendermygraphOK(renderGraf(Nok3G,87,input$gra))
      
      
      output$EstadoRed <- downloadHandler(
        filename = function() {
          paste('Red Movil ', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          write.csv(as.data.frame(Nok3G),file,col.names=TRUE, row.names = TRUE ,append = TRUE)
        }
      )
      
    }
      
    if (("4G" %in% input$network) & ("ERICSSON" %in% input$provider)){
     
     Eri4G <<- filter4GEri(datos.4GEri,input$filter,input$cellfilter)
     output$ACC_4G = rendermygraphOK(renderGraf(Eri4G,3,input$gra))
     output$RET_4G = rendermygraphOK(renderGraf(Eri4G,4,input$gra))
     output$VOLUMEN_DL = rendermygraphOK(renderGraf(Eri4G,5,input$gra))
     output$VOLUMEN_UL = rendermygraphOK(renderGraf(Eri4G,6,input$gra))
     output$TP_DL = rendermygraphOK(renderGraf(Eri4G,7,input$gra))
     output$TP_UL = rendermygraphOK(renderGraf(Eri4G,8,input$gra))
     output$DROP_ERAB = rendermygraphOK(renderGraf(Eri4G,9,input$gra))
     output$LAT = rendermygraphOK(renderGraf(Eri4G,10,input$gra))
     output$USER_DL = rendermygraphOK(renderGraf(Eri4G,11,input$gra)) 
     output$USER_UL = rendermygraphOK(renderGraf(Eri4G,12,input$gra)) 
     
     output$EstadoRed <- downloadHandler(
       filename = function() {
         paste('Ericsson', Sys.Date(), '.csv', sep = '')
       },
       content = function(file) {
         write.csv(as.data.frame(Eri4G),file,col.names=TRUE, row.names = TRUE ,append = TRUE)
       }
     )
   }  
    
    
    }else
    {
      if (("3G" %in% input$network) & ("NOKIA" %in% input$provider)){
        Nok3GBTS <<- filter3GNokBTS(datos.3GNokBTS,input$filter,input$cellfilter)
        
        output$AVG_USED_CE_R99_DL = rendermygraphOK(renderGraf(Nok3GBTS,3,input$gra))
        output$AVG_USED_CE_R99_UL = rendermygraphOK(renderGraf(Nok3GBTS,4,input$gra))
        output$AVG_USED_CE_R99 = rendermygraphOK(renderGraf(Nok3GBTS,5,input$gra))
        
        output$MAX_USED_CE_R99_DL = rendermygraphOK(renderGraf(Nok3GBTS,6,input$gra))
        output$MAX_USED_CE_R99_UL = rendermygraphOK(renderGraf(Nok3GBTS,7,input$gra))
        output$MAX_USED_CE_R99 = rendermygraphOK(renderGraf(Nok3GBTS,8,input$gra))
        
        output$AVG_USED_SU = rendermygraphOK(renderGraf(Nok3GBTS,9,input$gra))
        output$MAX_USED_SU = rendermygraphOK(renderGraf(Nok3GBTS,10,input$gra))
        
        output$AVG_HSDPA_USER = rendermygraphOK(renderGraf(Nok3GBTS,13,input$gra))
        output$MAX_HSUPA_USER = rendermygraphOK(renderGraf(Nok3GBTS,14,input$gra))
        output$AVG_HSDPA_USER = rendermygraphOK(renderGraf(Nok3GBTS,15,input$gra))
        output$MAX_HSUPA_USER = rendermygraphOK(renderGraf(Nok3GBTS,16,input$gra))
        
        output$AVG_USED_HSUPA_SU = rendermygraphOK(renderGraf(Nok3GBTS,11,input$gra))
        output$MAX_USED_HSUPA_SU = rendermygraphOK(renderGraf(Nok3GBTS,12,input$gra))
        
        
        output$EstadoRed <- downloadHandler(
          filename = function() {
            paste('Red Movil ', Sys.Date(), '.csv', sep = '')
          },
          content = function(file) {
            write.csv(as.data.frame(Nok3GBTS),file,col.names=TRUE, row.names = TRUE ,append = TRUE)
          }
        )
      }
    }
    
    removeNotification(id)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

