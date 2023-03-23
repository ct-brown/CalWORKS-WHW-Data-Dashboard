
# SETUP
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(dslabs)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(sf)
library(tidyverse)
library(tidyr)
library(leaflet)
library(readxl)

# DATA
ca_dta <- read_excel("~/Library/CloudStorage/Box-Box/Administrative_Data/Data Dashboard/Master Book - 20230216.xlsx")

ca_counties <- read_sf("https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")
ca_counties <- filter(ca_counties, STATE == "06")
ca_counties <- rename(ca_counties, county = NAME)

dta <- merge(ca_dta, ca_counties, by = "county")

dta <- dta %>% 
  mutate(range = case_when(
    month == "July" & year == 2021 ~ 1,
    month == "August" & year == 2021 ~ 2,
    month == "September" & year == 2021 ~ 3,
    month == "October" & year == 2021 ~ 4,
    month == "November" & year == 2021 ~ 5,
    month == "December" & year == 2021 ~ 6,
    month == "January" & year == 2022 ~ 7,
    month == "February" & year == 2022 ~ 8,
    month == "March" & year == 2022 ~ 9,
    month == "April" & year == 2022 ~ 10,
    month == "May" & year == 2022 ~ 11
  ))

dta <- dta %>% 
  mutate(date = case_when(
    range == 1 ~ "July 2022",
    range == 2 ~ "August 2022",
    range == 3 ~ "September 2022",
    range == 4 ~ "October 2022",
    range == 5 ~ "November 2022",
    range == 6 ~ "December 2022",
    range == 7 ~ "January 2023",
    range == 8 ~ "February 2023",
    range == 9 ~ "March 2023",
    range == 10 ~ "April 2023",
    range == 11 ~ "May 2023"
  ))

# UI
ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "CalWORKS WHW"),
                    
                    dashboardSidebar(
                      
                      sidebarMenu(
                        
                        menuItem("Map", tabName = "map"),
                        menuItem("Graphs", tabName = "graph"),
                        menuItem("About", tabName = "about")
                      )
                    ),
                    
                    dashboardBody(
                      
                      tabItems(
                        
                        tabItem(
                          
                          h1("County Map"),
                          
                          tabName = "map",
                          
                          fluidRow(
                            
                            column(
                              
                              selectizeInput(inputId = "var",
                                             label = "Select Variable",
                                             choices = c("Sanctions - Overall" = "overall.sanctions",
                                                         "Sanctions - Two Parent" = "two.parent.sanctions",
                                                         "Sanctions - Single Parent with Child Less than Two Years Old" = "single.parent.0024.sanctions",
                                                         "Sanctions - Single Parent with Child between Two and Six Years Old" = "single.parent.2506.sanctions",
                                                         "Sanctions - Single Parent with Child Older than Six Years Old" = "single.parent.0600.sanctions",
                                                         "Sanctions - Unidentified" = "unidentified.sanctions",
                                                         "Sanctions - Younger than Age 20" = "age.0020.sanctions",
                                                         "Sanctions - 20 to 24 Years Old" = "age.2024.sanctions",
                                                         "Sanctions - 25 to 29 Years Old" = "age.2529.sanctions",
                                                         "Sanctions - 30 to 34 Years Old" = "age.3034.sanctions",
                                                         "Sanctions - 35 to 39 Years Old" = "age.3539.sanctions",
                                                         "Sanctions - 40 to 44 Years Old" = "age.4044.sanctions",
                                                         "Sanctions - 45 Years and Older" = "age.4500.sanctions",
                                                         "Sanctions - Other Language" = "other.language.sanctions",
                                                         "Sanctions - Spanish" = "spanish.sanctions",
                                                         "Sanctions - English" = "english.sanction",
                                                         "Sanctions - Other Race/Ethnicity" = "other.sanctions",
                                                         "Sanctions - White" = "white.sanctions",
                                                         "Sanctions - Hispanic" = "hispanic.sanctions",
                                                         "Sanctions - Black" = "black.sanctions",
                                                         "Sanctions - Asian" = "asian.sanctions",
                                                         "Sanctions - Indigenous" = "indigenous.sanctions",
                                                         "Cases - Overall" = "overall.total.cases",
                                                         "Cases - Two Parent" = "two.parent.total.cases",
                                                         "Cases - Single Parent with Child Less than Two Years Old" = "single.parent.0024.total.cases",
                                                         "Cases - Single Parent with Child between Two and Six Years Old" = "single.parent.2506.total.cases",
                                                         "Cases - Single Parent with Child Older than Six Years Old" = "single.parent.0600.total.cases",
                                                         "Cases - Unidentified" = "unidentified.total.cases",
                                                         "Cases - Younger than Age 20" = "age.0020.total.cases",
                                                         "Cases - 20 to 24 Years Old" = "age.2024.total.cases",
                                                         "Cases - 25 to 29 Years Old" = "age.2529.total.cases",
                                                         "Cases - 30 to 34 Years Old" = "age.3034.total.cases",
                                                         "Cases - 35 to 39 Years Old" = "age.3539.total.cases",
                                                         "Cases - 40 to 44 Years Old" = "age.4044.total.cases",
                                                         "Cases - 45 Years and Older" = "age.4500.total.cases",
                                                         "Cases - Other Language" = "other.language.total.cases",
                                                         "Cases - Spanish" = "spanish.total.cases",
                                                         "Cases - English" = "english.total.cases",
                                                         "Cases - Other Race/Ethnicity" = "other.total.cases",
                                                         "Cases - White" = "white.total.cases",
                                                         "Cases - Hispanic" = "hispanic.total.cases",
                                                         "Cases - Black" = "black.total.cases",
                                                         "Cases - Asian" = "asian.total.cases",
                                                         "Cases - Indigenous" = "indigenous.total.cases",
                                                         "Sanction Rate - Overall" = "overall.sanction.rate",
                                                         "Sanction Rate - Two Parent" = "two.parent.sanction.rate",
                                                         "Sanction Rate - Single Parent with Child Less than Two Years Old" = "single.parent.0024.sanction.rate",
                                                         "Sanction Rate - Single Parent with Child between Two and Six Years Old" = "single.parent.2506.sanction.rate",
                                                         "Sanction Rate - Single Parent with Child Older than Six Years Old" = "single.parent.0600.sanction.rate",
                                                         "Sanction Rate - Unidentified" = "unidentified.sanction.rate",
                                                         "Sanction Rate - Younger than Age 20" = "age.0020.sanction.rate",
                                                         "Sanction Rate - 20 to 24 Years Old" = "age.2024.sanction.rate",
                                                         "Sanction Rate - 25 to 29 Years Old" = "age.2529.sanction.rate",
                                                         "Sanction Rate - 30 to 34 Years Old" = "age.3034.sanction.rate",
                                                         "Sanction Rate - 35 to 39 Years Old" = "age.3539.sanction.rate",
                                                         "Sanction Rate - 40 to 44 Years Old" = "age.4044.sanction.rate",
                                                         "Sanction Rate - 45 Years and Older" = "age.4500.sanction.rate",
                                                         "Sanction Rate - Other Language" = "other.language.sanction.rate",
                                                         "Sanction Rate - Spanish" = "spanish.sanction.rate",
                                                         "Sanction Rate - English" = "english.sanction.rate",
                                                         "Sanction Rate - Other Race/Ethnicity" = "other.sanction.rate",
                                                         "Sanction Rate - White" = "white.sanction.rate",
                                                         "Sanction Rate - Hispanic" = "hispanic.sanction.rate",
                                                         "Sanction Rate - Black" = "black.sanction.rate",
                                                         "Sanction Rate - Asian" = "asian.sanction.rate",
                                                         "Sanction Rate - Indigenous" = "indigenous.sanction.rate",
                                                         "Resolved - Overall" = "overall.resolved",
                                                         "Resolved - Sanction 12" = "sanction.12.resolved",
                                                         "Resolved - Sanction Long" = "sanction.long.resolved",
                                                         "Resolved - Two Parent" = "two.parent.resolved",
                                                         "Resolved - Single Parent with Child Less than Two Years Old" = "single.parent.0024.resolved",
                                                         "Resolved - Single Parent with Child between Two and Six Years Old" = "single.parent.2506.resolved",
                                                         "Resolved - Single Parent with Child Older than Six Years Old" = "single.parent.0600.resolved",
                                                         "Resolved - Unidentified" = "unidentified.resolved",
                                                         "Resolved - Younger than Age 20" = "age.0020.resolved",
                                                         "Resolved - 20 to 24 Years Old" = "age.2024.resolved",
                                                         "Resolved - 25 to 29 Years Old" = "age.2529.resolved",
                                                         "Resolved - 30 to 34 Years Old" = "age.3034.resolved",
                                                         "Resolved - 35 to 39 Years Old" = "age.3539.resolved",
                                                         "Resolved - 40 to 44 Years Old" = "age.4044.resolved",
                                                         "Resolved - 45 Years and Older" = "age.4500.resolved",
                                                         "Resolved - Other Language" = "other.language.resolved",
                                                         "Resolved - Spanish" = "spanish.resolved",
                                                         "Resolved - English" = "english.resolved",
                                                         "Resolved - Other Race/Ethnicity" = "other.resolved",
                                                         "Resolved - White" = "white.resolved",
                                                         "Resolved - Hispanic" = "hispanic.resolved",
                                                         "Resolved - Black" = "black.resolved",
                                                         "Resolved - Asian" = "asian.resolved",
                                                         "Resolved - Indigenous" = "indigenous.resolved",
                                                         "Sanctions Prior Month - Overall" = "overall.sanctions.prior.month",
                                                         "Sanctions Prior Month - Sanction 12" = "sanction.12.sanctions.prior.month",
                                                         "Sanctions Prior Month - Sanction Long" = "sanction.long.sanctions.prior.month",
                                                         "Sanctions Prior Month - Two Parent" = "two.parent.sanctions.prior.month",
                                                         "Sanctions Prior Month - Single Parent with Child Less than Two Years Old" = "single.parent.0024.sanctions.prior.month",
                                                         "Sanctions Prior Month - Single Parent with Child between Two and Six Years Old" = "single.parent.2506.sanctions.prior.month",
                                                         "Sanctions Prior Month - Single Parent with Child Older than Six Years Old" = "single.parent.0600.sanctions.prior.month",
                                                         "Sanctions Prior Month - Unidentified" = "unidentified.sanctions.prior.month",
                                                         "Sanctions Prior Month - Younger than Age 20" = "age.0020.sanctions.prior.month",
                                                         "Sanctions Prior Month - 20 to 24 Years Old" = "age.2024.sanctions.prior.month",
                                                         "Sanctions Prior Month - 25 to 29 Years Old" = "age.2529.sanctions.prior.month",
                                                         "Sanctions Prior Month - 30 to 34 Years Old" = "age.3034.sanctions.prior.month",
                                                         "Sanctions Prior Month - 35 to 39 Years Old" = "age.3539.sanctions.prior.month",
                                                         "Sanctions Prior Month - 40 to 44 Years Old" = "age.4044.sanctions.prior.month",
                                                         "Sanctions Prior Month - 45 Years and Older" = "age.4500.sanctions.prior.month",
                                                         "Sanctions Prior Month - Other Language" = "other.language.sanctions.prior.month",
                                                         "Sanctions Prior Month - Spanish" = "spanish.sanctions.prior.month",
                                                         "Sanctions Prior Month - English" = "english.sanctions.prior.month",
                                                         "Sanctions Prior Month - Other Race/Ethnicity" = "other.sanctions.prior.month",
                                                         "Sanctions Prior Month - White" = "white.sanctions.prior.month",
                                                         "Sanctions Prior Month - Hispanic" = "hispanic.sanctions.prior.month",
                                                         "Sanctions Prior Month - Black" = "black.sanctions.prior.month",
                                                         "Sanctions Prior Month - Asian" = "asian.sanctions.prior.month",
                                                         "Sanctions Prior Month - Indigenous" = "indigenous.sanctions.prior.month",
                                                         "Resolution Rate - Overall" = "overall.resolution.rate",
                                                         "Resolution Rate - Sanction 12" = "sanction.12.resolution.rate",
                                                         "Resolution Rate - Sanction Long" = "sanction.long.resolution.rate",
                                                         "Resolution Rate - Two Parent" = "two.parent.resolution.rate",
                                                         "Resolution Rate - Single Parent with Child Less than Two Years Old" = "single.parent.0024.resolution.rate",
                                                         "Resolution Rate - Single Parent with Child between Two and Six Years Old" = "single.parent.2506.resolution.rate",
                                                         "Resolution Rate - Single Parent with Child Older than Six Years Old" = "single.parent.0600.resolution.rate",
                                                         "Resolution Rate - Unidentified" = "unidentified.resolution.rate",
                                                         "Resolution Rate - Younger than Age 20" = "age.0020.resolution.rate",
                                                         "Resolution Rate - 20 to 24 Years Old" = "age.2024.resolution.rate",
                                                         "Resolution Rate - 25 to 29 Years Old" = "age.2529.resolution.rate",
                                                         "Resolution Rate - 30 to 34 Years Old" = "age.3034.resolution.rate",
                                                         "Resolution Rate - 35 to 39 Years Old" = "age.3539.resolution.rate",
                                                         "Resolution Rate - 40 to 44 Years Old" = "age.4044.resolution.rate",
                                                         "Resolution Rate - 45 Years and Older" = "age.4500.resolution.rate",
                                                         "Resolution Rate - Other Language" = "other.language.resolution.rate",
                                                         "Resolution Rate - Spanish" = "spanish.resolution.rate",
                                                         "Resolution Rate - English" = "english.resolution.rate",
                                                         "Resolution Rate - Other Race/Ethnicity" = "other.resolution.rate",
                                                         "Resolution Rate - White" = "white.resolution.rate",
                                                         "Resolution Rate - Hispanic" = "hispanic.resolution.rate",
                                                         "Resolution Rate - Black" = "black.resolution.rate",
                                                         "Resolution Rate - Asian" = "asian.resolution.rate",
                                                         "Resolution Rate - Indigenous" = "indigenous.resolution.rate"),
                                      multiple = F),
                              width = 4),
                          
                          column(
                            
                            selectizeInput(inputId = "date",
                                           label = "Select Month and Year",
                                           choices = c("July 2022" = "July 2022",
                                                       "August 2022" = "August 2022",
                                                       "September 2022"= "September 2022",
                                                       "October 2022" = "October 2022" ,
                                                       "November 2022" = "November 2022",
                                                       "December 2022" = "December 2022",
                                                       "January 2023" = "January 2023",
                                                       "February 2023" = "February 2023",
                                                       "March 2023" = "March 2023",
                                                       "April 2023" = "April 2023",
                                                       "May 2023" = "May 2023"),
                                      multiple = F),
                            width = 4)
                          
                          ),
                          
                          br(), br(),
                          
                          fluidRow(
                            
                            column(
                              
                              leafletOutput('map', height = 600),
                              width = 12)
                            
                          )
                        ),
                        
                        tabItem(
                          
                          h1('Graphs'),
                          
                          tabName = 'graph',
                          
                          tabsetPanel(
                            
                            tabPanel(
                              
                              'Sanctions',
                              
                              br(),
                              
                              fluidRow(
                                
                                column(
                                  
                                  selectizeInput(inputId = 'sanctions',
                                                 label = 'Select Variable',
                                                 choices = c("Overall" = "overall.sanctions",
                                                             "Two Parent" = "two.parent.sanctions",
                                                             "Single Parent with Child Less than Two Years Old" = "single.parent.0024.sanctions",
                                                             "Single Parent with Child between Two and Six Years Old" = "single.parent.2506.sanctions",
                                                             "Single Parent with Child Older than Six Years Old" = "single.parent.0600.sanctions",
                                                             "Unidentified" = "unidentified.sanctions",
                                                             "Younger than Age 20" = "age.0020.sanctions",
                                                             "20 to 24 Years Old" = "age.2024.sanctions",
                                                             "25 to 29 Years Old" = "age.2529.sanctions",
                                                             "30 to 34 Years Old" = "age.3034.sanctions",
                                                             "35 to 39 Years Old" = "age.3539.sanctions",
                                                             "40 to 44 Years Old" = "age.4044.sanctions",
                                                             "45 Years and Older" = "age.4500.sanctions",
                                                             "Other Language" = "other.language.sanctions",
                                                             "Spanish" = "spanish.sanctions",
                                                             "English" = "english.sanctions",
                                                             "Other Race/Ethnicity" = "other.sanctions",
                                                             "White" = "white.sanctions",
                                                             "Hispanic" = "hispanic.sanctions",
                                                             "Black" = "black.sanctions",
                                                             "Asian" = "asian.sanctions",
                                                             "Indigenous" = "indigenous.sanctions"),
                                                 selected = "overall.sanctions",
                                                 multiple = F),
                                  width = 4),
                                
                                column(
                                  
                                  selectizeInput(inputId = "county.sanctions",
                                                label = "Select County",
                                                choices = c("Alameda" = "Alameda",
                                                            "Alpine" = "Alpine",
                                                            "Amador" = "Amador",
                                                            "Butte" = "Butte",
                                                            "Calaveras" = "Calaveras",
                                                            "Colusa" = "Colusa",
                                                            "Contra Costa" = "Contra Costa",
                                                            "Del Norte" = "Del Norte",
                                                            "El Dorado" = "El Dorado",
                                                            "Fresno" = "Fresno",
                                                            "Glenn" = "Glenn",
                                                            "Humboldt" = "Humboldt",
                                                            "Imperial" = "Imperial",
                                                            "Inyo" = "Inyo",
                                                            "Kern" = "Kern",
                                                            "Kings" = "Kings",
                                                            "Lake" = "Lake",
                                                            "Lassen" = "Lassen",
                                                            "Los Angeles" = "Los Angeles",
                                                            "Madera" = "Madera",
                                                            "Marin" = "Marin",
                                                            "Mariposa" = "Mariposa",
                                                            "Mendocino" = "Mendocino",
                                                            "Merced" = "Merced",
                                                            "Modoc" = "Modoc",
                                                            "Mono" = "Mono",
                                                            "Monterey" = "Monterey",
                                                            "Napa" = "Napa",
                                                            "Nevada" = "Nevada",
                                                            "Orange" = "Orange",
                                                            "Placer" = "Placer",
                                                            "Plumas" = "Plumas",
                                                            "Riverside" = "Riverside",
                                                            "Sacramento" = "Sacramento",
                                                            "San Benito" = "San Benito",
                                                            "San Bernardino" = "San Bernardino",
                                                            "San Diego" = "San Diego",
                                                            "San Francisco" = "San Francisco",
                                                            "San Joaquin" = "San Joaquin",
                                                            "San Luis Obispo" = "San Luis Obispo",
                                                            "San Mateo" = "San Mateo",
                                                            "Santa Barbara" = "Santa Barbara",
                                                            "Santa Clara" = "Santa Clara",
                                                            "Santa Cruz" = "Santa Cruz",
                                                            "Shasta" = "Shasta",
                                                            "Sierra" = "Sierra",
                                                            "Siskiyou" = "Siskiyou",
                                                            "Solano" = "Solano",
                                                            "Sonoma" = "Sonoma",
                                                            "Stanislaus" = "Stanislaus",
                                                            "Sutter" = "Sutter",
                                                            "Tehama" = "Tehama",
                                                            "Trinity" = "Trinity",
                                                            "Tulare" = "Tulare",
                                                            "Tuolumne" = "Tuolumne",
                                                            "Ventura" = "Ventura",
                                                            "Yolo" = "Yolo",
                                                            "Yuba" = "Yuba"),
                                                selected = "Alameda",
                                                multiple = T),
                                  width = 4),
                                
                                column(
                                  
                                  br(),
                                  
                                  actionButton("button1", "Apply"),
                                  width = 4),
                                
                                column(
                                  
                                  plotOutput("sanctionsGraph", height = 600),
                                  width = 12,
                                  align = "center")
                                
                                )
                            ),
                            
                            tabPanel(
                              
                              "Cases",
                              
                              br(),
                              
                              fluidRow(
                                
                                column(
                                  
                                  selectizeInput(inputId = "cases",
                                                 label = "Select Variable",
                                                 choices = c("Overall" = "overall.total.cases",
                                                             "Two Parent" = "two.parent.total.cases",
                                                             "Single Parent with Child Less than Two Years Old" = "single.parent.0024.total.cases",
                                                             "Single Parent with Child between Two and Six Years Old" = "single.parent.2506.total.cases",
                                                             "Single Parent with Child Older than Six Years Old" = "single.parent.0600.total.cases",
                                                             "Unidentified" = "unidentified.total.cases",
                                                             "Younger than Age 20" = "age.0020.total.cases",
                                                             "20 to 24 Years Old" = "age.2024.total.cases",
                                                             "25 to 29 Years Old" = "age.2529.total.cases",
                                                             "30 to 34 Years Old" = "age.3034.total.cases",
                                                             "35 to 39 Years Old" = "age.3539.total.cases",
                                                             "40 to 44 Years Old" = "age.4044.total.cases",
                                                             "45 Years and Older" = "age.4500.total.cases",
                                                             "Other Language" = "other.language.total.cases",
                                                             "Spanish" = "spanish.total.cases",
                                                             "English" = "english.total.cases",
                                                             "Other Race/Ethnicity" = "other.total.cases",
                                                             "White" = "white.total.cases",
                                                             "Hispanic" = "hispanic.total.cases",
                                                             "Black" = "black.total.cases",
                                                             "Asian" = "asian.total.cases",
                                                             "Indigenous" = "indigenous.total.cases"),
                                                 selected = "overall.total.cases",
                                                 multiple = F),
                                  width = 4),
                                
                                column(
                                  
                                  selectizeInput(inputId = "county.cases",
                                                 label = "Select County",
                                                 choices = c("Alameda" = "Alameda",
                                                             "Alpine" = "Alpine",
                                                             "Amador" = "Amador",
                                                             "Butte" = "Butte",
                                                             "Calaveras" = "Calaveras",
                                                             "Colusa" = "Colusa",
                                                             "Contra Costa" = "Contra Costa",
                                                             "Del Norte" = "Del Norte",
                                                             "El Dorado" = "El Dorado",
                                                             "Fresno" = "Fresno",
                                                             "Glenn" = "Glenn",
                                                             "Humboldt" = "Humboldt",
                                                             "Imperial" = "Imperial",
                                                             "Inyo" = "Inyo",
                                                             "Kern" = "Kern",
                                                             "Kings" = "Kings",
                                                             "Lake" = "Lake",
                                                             "Lassen" = "Lassen",
                                                             "Los Angeles" = "Los Angeles",
                                                             "Madera" = "Madera",
                                                             "Marin" = "Marin",
                                                             "Mariposa" = "Mariposa",
                                                             "Mendocino" = "Mendocino",
                                                             "Merced" = "Merced",
                                                             "Modoc" = "Modoc",
                                                             "Mono" = "Mono",
                                                             "Monterey" = "Monterey",
                                                             "Napa" = "Napa",
                                                             "Nevada" = "Nevada",
                                                             "Orange" = "Orange",
                                                             "Placer" = "Placer",
                                                             "Plumas" = "Plumas",
                                                             "Riverside" = "Riverside",
                                                             "Sacramento" = "Sacramento",
                                                             "San Benito" = "San Benito",
                                                             "San Bernardino" = "San Bernardino",
                                                             "San Diego" = "San Diego",
                                                             "San Francisco" = "San Francisco",
                                                             "San Joaquin" = "San Joaquin",
                                                             "San Luis Obispo" = "San Luis Obispo",
                                                             "San Mateo" = "San Mateo",
                                                             "Santa Barbara" = "Santa Barbara",
                                                             "Santa Clara" = "Santa Clara",
                                                             "Santa Cruz" = "Santa Cruz",
                                                             "Shasta" = "Shasta",
                                                             "Sierra" = "Sierra",
                                                             "Siskiyou" = "Siskiyou",
                                                             "Solano" = "Solano",
                                                             "Sonoma" = "Sonoma",
                                                             "Stanislaus" = "Stanislaus",
                                                             "Sutter" = "Sutter",
                                                             "Tehama" = "Tehama",
                                                             "Trinity" = "Trinity",
                                                             "Tulare" = "Tulare",
                                                             "Tuolumne" = "Tuolumne",
                                                             "Ventura" = "Ventura",
                                                             "Yolo" = "Yolo",
                                                             "Yuba" = "Yuba"),
                                                 selected = "Alameda",
                                                 multiple = T),
                                  width = 4),
                                
                                column(
                                  
                                  br(),
                                  
                                  actionButton("button2", "Apply"),
                                  width = 4),
                                
                                column(
                                  
                                  plotOutput("casesGraph", height = 600),
                                  width = 12,
                                  align = "center")
                                
                                )
                              ),
                            
                            tabPanel(
                              
                              "Sanction Rate",
                              
                              br(),
                              
                              fluidRow(
                                
                                column(
                                  
                                  selectizeInput(inputId = "sanctionRate",
                                                 label = "Select Variable",
                                                 choices = c("Overall" = "overall.sanction.rate",
                                                             "Two Parent" = "two.parent.sanction.rate",
                                                             "Single Parent with Child Less than Two Years Old" = "single.parent.0024.sanction.rate",
                                                             "Single Parent with Child between Two and Six Years Old" = "single.parent.2506.sanction.rate",
                                                             "Single Parent with Child Older than Six Years Old" = "single.parent.0600.sanction.rate",
                                                             "Unidentified" = "unidentified.sanction.rate",
                                                             "Younger than Age 20" = "age.0020.sanction.rate",
                                                             "20 to 24 Years Old" = "age.2024.sanction.rate",
                                                             "25 to 29 Years Old" = "age.2529.sanction.rate",
                                                             "30 to 34 Years Old" = "age.3034.sanction.rate",
                                                             "35 to 39 Years Old" = "age.3539.sanction.rate",
                                                             "40 to 44 Years Old" = "age.4044.sanction.rate",
                                                             "45 Years and Older" = "age.4500.sanction.rate",
                                                             "Other Language" = "other.language.sanction.rate",
                                                             "Spanish" = "spanish.sanction.rate",
                                                             "English" = "english.sanction.rate",
                                                             "Other Race/Ethnicity" = "other.sanction.rate",
                                                             "White" = "white.sanction.rate",
                                                             "Hispanic" = "hispanic.sanction.rate",
                                                             "Black" = "black.sanction.rate",
                                                             "Asian" = "asian.sanction.rate",
                                                             "Indigenous" = "indigenous.sanction.rate"),
                                                 selected = "overall.sanction.rate",
                                                 multiple = F),
                                  width = 4),
                                
                                column(
                                  
                                  selectizeInput(inputId = "county.sanctionRate",
                                                 label = "Select County",
                                                 choices = c("Alameda" = "Alameda",
                                                             "Alpine" = "Alpine",
                                                             "Amador" = "Amador",
                                                             "Butte" = "Butte",
                                                             "Calaveras" = "Calaveras",
                                                             "Colusa" = "Colusa",
                                                             "Contra Costa" = "Contra Costa",
                                                             "Del Norte" = "Del Norte",
                                                             "El Dorado" = "El Dorado",
                                                             "Fresno" = "Fresno",
                                                             "Glenn" = "Glenn",
                                                             "Humboldt" = "Humboldt",
                                                             "Imperial" = "Imperial",
                                                             "Inyo" = "Inyo",
                                                             "Kern" = "Kern",
                                                             "Kings" = "Kings",
                                                             "Lake" = "Lake",
                                                             "Lassen" = "Lassen",
                                                             "Los Angeles" = "Los Angeles",
                                                             "Madera" = "Madera",
                                                             "Marin" = "Marin",
                                                             "Mariposa" = "Mariposa",
                                                             "Mendocino" = "Mendocino",
                                                             "Merced" = "Merced",
                                                             "Modoc" = "Modoc",
                                                             "Mono" = "Mono",
                                                             "Monterey" = "Monterey",
                                                             "Napa" = "Napa",
                                                             "Nevada" = "Nevada",
                                                             "Orange" = "Orange",
                                                             "Placer" = "Placer",
                                                             "Plumas" = "Plumas",
                                                             "Riverside" = "Riverside",
                                                             "Sacramento" = "Sacramento",
                                                             "San Benito" = "San Benito",
                                                             "San Bernardino" = "San Bernardino",
                                                             "San Diego" = "San Diego",
                                                             "San Francisco" = "San Francisco",
                                                             "San Joaquin" = "San Joaquin",
                                                             "San Luis Obispo" = "San Luis Obispo",
                                                             "San Mateo" = "San Mateo",
                                                             "Santa Barbara" = "Santa Barbara",
                                                             "Santa Clara" = "Santa Clara",
                                                             "Santa Cruz" = "Santa Cruz",
                                                             "Shasta" = "Shasta",
                                                             "Sierra" = "Sierra",
                                                             "Siskiyou" = "Siskiyou",
                                                             "Solano" = "Solano",
                                                             "Sonoma" = "Sonoma",
                                                             "Stanislaus" = "Stanislaus",
                                                             "Sutter" = "Sutter",
                                                             "Tehama" = "Tehama",
                                                             "Trinity" = "Trinity",
                                                             "Tulare" = "Tulare",
                                                             "Tuolumne" = "Tuolumne",
                                                             "Ventura" = "Ventura",
                                                             "Yolo" = "Yolo",
                                                             "Yuba" = "Yuba"),
                                                 selected = "Alameda",
                                                 multiple = T),
                                  width = 4),
                                
                                column(
                                  
                                  br(),
                                  
                                  actionButton("button3", "Apply"),
                                  width = 4),
                                
                                column(
                                  
                                  plotOutput("sanctionRateGraph", height = 600),
                                  width = 12,
                                  align = "center")
                                
                                )
                              ),
                            
                            tabPanel(
                              
                              "Resolved",
                              
                              br(),
                              
                              fluidRow(
                                
                                column(
                                  
                                  selectizeInput(inputId = "resolved",
                                                 label = "Select Variable",
                                                 choices = c("Overall" = "overall.resolved",
                                                             "Sanction 12" = "sanction.12.resolved",
                                                             "Sanction Long" = "sanction.long.resolved",
                                                             "Two Parent" = "two.parent.resolved",
                                                             "Single Parent with Child Less than Two Years Old" = "single.parent.0024.resolved",
                                                             "Single Parent with Child between Two and Six Years Old" = "single.parent.2506.resolved",
                                                             "Single Parent with Child Older than Six Years Old" = "single.parent.0600.resolved",
                                                             "Unidentified" = "unidentified.resolved",
                                                             "Younger than Age 20" = "age.0020.resolved",
                                                             "20 to 24 Years Old" = "age.2024.resolved",
                                                             "25 to 29 Years Old" = "age.2529.resolved",
                                                             "30 to 34 Years Old" = "age.3034.resolved",
                                                             "35 to 39 Years Old" = "age.3539.resolved",
                                                             "40 to 44 Years Old" = "age.4044.resolved",
                                                             "45 Years and Older" = "age.4500.resolved",
                                                             "Other Language" = "other.language.resolved",
                                                             "Spanish" = "spanish.resolved",
                                                             "English" = "english.resolved",
                                                             "Other Race/Ethnicity" = "other.resolved",
                                                             "White" = "white.resolved",
                                                             "Hispanic" = "hispanic.resolved",
                                                             "Black" = "black.resolved",
                                                             "Asian" = "asian.resolved",
                                                             "Indigenous" = "indigenous.resolved"),
                                                 selected = "overall.resolved",
                                                 multiple = F),
                                  width = 4),
                                
                                column(
                                  
                                  selectizeInput(inputId = "county.resolved",
                                                 label = "Select County",
                                                 choices = c("Alameda" = "Alameda",
                                                             "Alpine" = "Alpine",
                                                             "Amador" = "Amador",
                                                             "Butte" = "Butte",
                                                             "Calaveras" = "Calaveras",
                                                             "Colusa" = "Colusa",
                                                             "Contra Costa" = "Contra Costa",
                                                             "Del Norte" = "Del Norte",
                                                             "El Dorado" = "El Dorado",
                                                             "Fresno" = "Fresno",
                                                             "Glenn" = "Glenn",
                                                             "Humboldt" = "Humboldt",
                                                             "Imperial" = "Imperial",
                                                             "Inyo" = "Inyo",
                                                             "Kern" = "Kern",
                                                             "Kings" = "Kings",
                                                             "Lake" = "Lake",
                                                             "Lassen" = "Lassen",
                                                             "Los Angeles" = "Los Angeles",
                                                             "Madera" = "Madera",
                                                             "Marin" = "Marin",
                                                             "Mariposa" = "Mariposa",
                                                             "Mendocino" = "Mendocino",
                                                             "Merced" = "Merced",
                                                             "Modoc" = "Modoc",
                                                             "Mono" = "Mono",
                                                             "Monterey" = "Monterey",
                                                             "Napa" = "Napa",
                                                             "Nevada" = "Nevada",
                                                             "Orange" = "Orange",
                                                             "Placer" = "Placer",
                                                             "Plumas" = "Plumas",
                                                             "Riverside" = "Riverside",
                                                             "Sacramento" = "Sacramento",
                                                             "San Benito" = "San Benito",
                                                             "San Bernardino" = "San Bernardino",
                                                             "San Diego" = "San Diego",
                                                             "San Francisco" = "San Francisco",
                                                             "San Joaquin" = "San Joaquin",
                                                             "San Luis Obispo" = "San Luis Obispo",
                                                             "San Mateo" = "San Mateo",
                                                             "Santa Barbara" = "Santa Barbara",
                                                             "Santa Clara" = "Santa Clara",
                                                             "Santa Cruz" = "Santa Cruz",
                                                             "Shasta" = "Shasta",
                                                             "Sierra" = "Sierra",
                                                             "Siskiyou" = "Siskiyou",
                                                             "Solano" = "Solano",
                                                             "Sonoma" = "Sonoma",
                                                             "Stanislaus" = "Stanislaus",
                                                             "Sutter" = "Sutter",
                                                             "Tehama" = "Tehama",
                                                             "Trinity" = "Trinity",
                                                             "Tulare" = "Tulare",
                                                             "Tuolumne" = "Tuolumne",
                                                             "Ventura" = "Ventura",
                                                             "Yolo" = "Yolo",
                                                             "Yuba" = "Yuba"),
                                                 selected = "Alameda",
                                                 multiple = T),
                                  width = 4),
                                
                            column(
                              
                              br(),
                              
                              actionButton("button4", "Apply"),
                              width = 4),

                            column(
                              
                              plotOutput("resolvedGraph", height = 600),
                              width = 12,
                              align = "center")
                            
                            )
                          ),
                          
                            tabPanel(
                            
                            "Sanctions Prior Month",
                            
                            br(),
                            
                            fluidRow(
                              
                              column(
                                
                                selectizeInput(inputId = "sanctionsPriorMonth",
                                               label = "Select Variable",
                                               choices = c("Overall" = "overall.sanctions.prior.month",
                                                           "Sanction 12" = "anction.12.sanctions.prior.month",
                                                           "Sanction Long" = "sanction.long.sanctions.prior.month",
                                                           "Two Parent" = "two.parent.sanctions.prior.month",
                                                           "Single Parent with Child Less than Two Years Old" = "single.parent.0024.sanctions.prior.month",
                                                           "Single Parent with Child between Two and Six Years Old" = "single.parent.2506.sanctions.prior.month",
                                                           "Single Parent with Child Older than Six Years Old" = "single.parent.0600.sanctions.prior.month",
                                                           "Unidentified" = "unidentified.sanctions.prior.month",
                                                           "Younger than Age 20" = "age.0020.sanctions.prior.month",
                                                           "20 to 24 Years Old" = "age.2024.sanctions.prior.month",
                                                           "25 to 29 Years Old" = "age.2529.sanctions.prior.month",
                                                           "30 to 34 Years Old" = "age.3034.sanctions.prior.month",
                                                           "35 to 39 Years Old" = "age.3539.sanctions.prior.month",
                                                           "40 to 44 Years Old" = "age.4044.sanctions.prior.month",
                                                           "45 Years and Older" = "age.4500.sanctions.prior.month",
                                                           "Other Language" = "other.language.sanctions.prior.month",
                                                           "Spanish" = "spanish.sanctions.prior.month",
                                                           "English" = "english.sanctions.prior.month",
                                                           "Other Race/Ethnicity" = "other.sanctions.prior.month",
                                                           "White" = "white.sanctions.prior.month",
                                                           "Hispanic" = "hispanic.sanctions.prior.month",
                                                           "Black" = "black.sanctions.prior.month",
                                                           "Asian" = "asian.sanctions.prior.month",
                                                           "Indigenous" = "indigenous.sanctions.prior.month"),
                                               selected = "overall.sanctions.prior.month",
                                               multiple = F),
                                width = 4),
                                
                                column(
                                  
                                  selectizeInput(inputId = "county.sanctionsPriorMonth",
                                                 label = "Select County",
                                                 choices = c("Alameda" = "Alameda",
                                                             "Alpine" = "Alpine",
                                                             "Amador" = "Amador",
                                                             "Butte" = "Butte",
                                                             "Calaveras" = "Calaveras",
                                                             "Colusa" = "Colusa",
                                                             "Contra Costa" = "Contra Costa",
                                                             "Del Norte" = "Del Norte",
                                                             "El Dorado" = "El Dorado",
                                                             "Fresno" = "Fresno",
                                                             "Glenn" = "Glenn",
                                                             "Humboldt" = "Humboldt",
                                                             "Imperial" = "Imperial",
                                                             "Inyo" = "Inyo",
                                                             "Kern" = "Kern",
                                                             "Kings" = "Kings",
                                                             "Lake" = "Lake",
                                                             "Lassen" = "Lassen",
                                                             "Los Angeles" = "Los Angeles",
                                                             "Madera" = "Madera",
                                                             "Marin" = "Marin",
                                                             "Mariposa" = "Mariposa",
                                                             "Mendocino" = "Mendocino",
                                                             "Merced" = "Merced",
                                                             "Modoc" = "Modoc",
                                                             "Mono" = "Mono",
                                                             "Monterey" = "Monterey",
                                                             "Napa" = "Napa",
                                                             "Nevada" = "Nevada",
                                                             "Orange" = "Orange",
                                                             "Placer" = "Placer",
                                                             "Plumas" = "Plumas",
                                                             "Riverside" = "Riverside",
                                                             "Sacramento" = "Sacramento",
                                                             "San Benito" = "San Benito",
                                                             "San Bernardino" = "San Bernardino",
                                                             "San Diego" = "San Diego",
                                                             "San Francisco" = "San Francisco",
                                                             "San Joaquin" = "San Joaquin",
                                                             "San Luis Obispo" = "San Luis Obispo",
                                                             "San Mateo" = "San Mateo",
                                                             "Santa Barbara" = "Santa Barbara",
                                                             "Santa Clara" = "Santa Clara",
                                                             "Santa Cruz" = "Santa Cruz",
                                                             "Shasta" = "Shasta",
                                                             "Sierra" = "Sierra",
                                                             "Siskiyou" = "Siskiyou",
                                                             "Solano" = "Solano",
                                                             "Sonoma" = "Sonoma",
                                                             "Stanislaus" = "Stanislaus",
                                                             "Sutter" = "Sutter",
                                                             "Tehama" = "Tehama",
                                                             "Trinity" = "Trinity",
                                                             "Tulare" = "Tulare",
                                                             "Tuolumne" = "Tuolumne",
                                                             "Ventura" = "Ventura",
                                                             "Yolo" = "Yolo",
                                                             "Yuba" = "Yuba"),
                                                 selected = "Alameda",
                                                 multiple = T),
                                  width = 4),
                              
                              column(
                                
                                br(),
                                
                                actionButton("button5", "Apply"),
                                width = 4),
                              
                              column(
                                
                                plotOutput("sanctionPriorMonthGraph", height = 600),
                                width = 12,
                                align = "center")
                              
                              )
                            ),
                          
                          tabPanel(
                            
                            "Resolution Rate",
                            
                            br(),
                            
                            fluidRow(
                              
                              column(
                                
                                selectizeInput(inputId = "resolutionRate",
                                               label = "Select Variable",
                                               choices = c("Overall" = "overall.resolution.rate",
                                                           "Sanction 12" = "sanction.12.resolution.rate",
                                                           "Sanction Long" = "sanction.long.resolution.rate",
                                                           "Two Parent" = "two.parent.resolution.rate",
                                                           "Single Parent with Child Less than Two Years Old" = "single.parent.0024.resolution.rate",
                                                           "Single Parent with Child between Two and Six Years Old" = "single.parent.2506.resolution.rate",
                                                           "Single Parent with Child Older than Six Years Old" = "single.parent.0600.resolution.rate",
                                                           "Unidentified" = "unidentified.resolution.rate",
                                                           "Younger than Age 20" = "age.0020.resolution.rate",
                                                           "20 to 24 Years Old" = "age.2024.resolution.rate",
                                                           "25 to 29 Years Old" = "age.2529.resolution.rate",
                                                           "30 to 34 Years Old" = "age.3034.resolution.rate",
                                                           "35 to 39 Years Old" = "age.3539.resolution.rate",
                                                           "40 to 44 Years Old" = "age.4044.resolution.rate",
                                                           "45 Years and Older" = "age.4500.resolution.rate",
                                                           "Other Language" = "other.language.resolution.rate",
                                                           "Spanish" = "spanish.resolution.rate",
                                                           "English" = "english.resolution.rate",
                                                           "Other Race/Ethnicity" = "other.resolution.rate",
                                                           "White" = "white.resolution.rate",
                                                           "Hispanic" = "hispanic.resolution.rate",
                                                           "Black" = "black.resolution.rate",
                                                           "Asian" = "asian.resolution.rate",
                                                           "Indigenous" = "indigenous.resolution.rate"),
                                               selected = "overall.resolution.rate",
                                               multiple = F),
                                width = 4),
                              
                              column(
                                
                                selectizeInput(inputId = "county.resolutionRate",
                                               label = "Select County",
                                               choices = c("Alameda" = "Alameda",
                                                           "Alpine" = "Alpine",
                                                           "Amador" = "Amador",
                                                           "Butte" = "Butte",
                                                           "Calaveras" = "Calaveras",
                                                           "Colusa" = "Colusa",
                                                           "Contra Costa" = "Contra Costa",
                                                           "Del Norte" = "Del Norte",
                                                           "El Dorado" = "El Dorado",
                                                           "Fresno" = "Fresno",
                                                           "Glenn" = "Glenn",
                                                           "Humboldt" = "Humboldt",
                                                           "Imperial" = "Imperial",
                                                           "Inyo" = "Inyo",
                                                           "Kern" = "Kern",
                                                           "Kings" = "Kings",
                                                           "Lake" = "Lake",
                                                           "Lassen" = "Lassen",
                                                           "Los Angeles" = "Los Angeles",
                                                           "Madera" = "Madera",
                                                           "Marin" = "Marin",
                                                           "Mariposa" = "Mariposa",
                                                           "Mendocino" = "Mendocino",
                                                           "Merced" = "Merced",
                                                           "Modoc" = "Modoc",
                                                           "Mono" = "Mono",
                                                           "Monterey" = "Monterey",
                                                           "Napa" = "Napa",
                                                           "Nevada" = "Nevada",
                                                           "Orange" = "Orange",
                                                           "Placer" = "Placer",
                                                           "Plumas" = "Plumas",
                                                           "Riverside" = "Riverside",
                                                           "Sacramento" = "Sacramento",
                                                           "San Benito" = "San Benito",
                                                           "San Bernardino" = "San Bernardino",
                                                           "San Diego" = "San Diego",
                                                           "San Francisco" = "San Francisco",
                                                           "San Joaquin" = "San Joaquin",
                                                           "San Luis Obispo" = "San Luis Obispo",
                                                           "San Mateo" = "San Mateo",
                                                           "Santa Barbara" = "Santa Barbara",
                                                           "Santa Clara" = "Santa Clara",
                                                           "Santa Cruz" = "Santa Cruz",
                                                           "Shasta" = "Shasta",
                                                           "Sierra" = "Sierra",
                                                           "Siskiyou" = "Siskiyou",
                                                           "Solano" = "Solano",
                                                           "Sonoma" = "Sonoma",
                                                           "Stanislaus" = "Stanislaus",
                                                           "Sutter" = "Sutter",
                                                           "Tehama" = "Tehama",
                                                           "Trinity" = "Trinity",
                                                           "Tulare" = "Tulare",
                                                           "Tuolumne" = "Tuolumne",
                                                           "Ventura" = "Ventura",
                                                           "Yolo" = "Yolo",
                                                           "Yuba" = "Yuba"),
                                               selected = "Alameda",
                                               multiple = T),
                                width = 4),
                              
                              column(
                                
                                br(),
                                
                                actionButton("button6", "Apply"),
                                width = 4),
                              
                              column(
                                
                                plotOutput("resolutionRateGraph", height = 600),
                                width = 12,
                                align = "center")
                              
                              )
                            )
                          )
                        ),
                        
                        tabItem(
                          
                          h1('About'),
                          
                          tabName = "about",
                          
                          br(),
                          
                          h5(style="text-align: justify;",
                             "Data displayed with this tool were downloaded from the California CalWORKs Outcomes and Accountability Review (Cal-OAR) data portal on February 24, 2023."),
                          
                          h5("If using any of these tools in publication or presentations, please acknowledge as 'Chang, Y. L., & Brown, C. T. (2023). CalWorks OAR Data Visualization Tool.'"),
                          
                          hr(),
                              
                          h5("App designed and developed by Dr. Yu-Ling Chang and Taylor Brown, 2023.")
                          
                          )
                        
                        )
                      )
                    
                    )

# SERVER
server <- function(input, output, session) {
  
  dta$range <- as.factor(dta$range)
  
  dta$range <- factor(dta$range,
                      levels = c(1:11),
                      labels = c("July 2022", 
                                 "August 2022", 
                                 "September 2022", 
                                 "October 2022", 
                                 "November 2022", 
                                 "December 2022", 
                                 "January 2023", 
                                 "February 2023", 
                                 "March 2023", 
                                 "April 2023", 
                                 "May 2023"))
  
    output$map <- renderLeaflet({
      
      selectedData_map <- reactive({
        
        dta_map <- dta[, c(input$var, 'date', 'county', 'geometry')]
        dta_map <- dta_map %>% filter(date == input$date)
        
      })
      
      dta_map <- selectedData_map()
      
      sf.dta2 <- st_as_sf(dta_map)
      
      pal <- colorNumeric(palette = "Blues", domain = dta_map[, input$var])
      
      leaflet(sf.dta2, options = leafletOptions(zoomControl = FALSE)) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
        }") %>% 
        addTiles() %>% 
        addPolygons(fillColor = ~pal(dta_map[, input$var]),
                    fillOpacity = 0.7,
                    color = "#BDBDBD",
                    weight = 1) %>% 
        addLegend(pal = pal,
                  values = ~dta_map[, input$var],
                  title = input$var,
                  na.label = "NA",
                  position = "bottomright")
      
      })
    
    observeEvent(input$button1, {
      
      req(input$sanctions, input$county.sanctions)
      
      selectedData_sanctions <- reactive({
        
        dta_sanctions <- dta %>% filter(county == input$county.sanctions) %>% subset(!is.na(input$sanctions))
        
      })
      
      dta_sanctions <- selectedData_sanctions()
      
      output$sanctionsGraph <- renderPlot({
        
        ggplot(data = dta_sanctions,
               aes(x = range, color = county, group = county)) +
          geom_line(aes_string(y = input$sanctions)) +
          theme_minimal()
        
      })
    })
    
    observeEvent(input$button2, {
      
      req(input$cases, input$county.cases)
      
      selectedData_cases <- reactive({
        
        dta_cases <- dta %>% filter(county == input$county.cases) %>% subset(!is.na(input$cases))
        
        })
      
      dta_cases <- selectedData_cases()
      
      output$casesGraph <- renderPlot({
        
        ggplot(data = dta_cases,
               aes(x = range, color = county, group = county)) +
          geom_line(aes_string(y = input$cases)) +
          theme_minimal()
        
        })
      })
    
    observeEvent(input$button3, {
      
      req(input$sanctionRate, input$county.sanctionRate)
      
      selectedData_sanctionRate <- reactive({
        
        dta_sanctionRate <- dta %>% filter(county == input$county.sanctionRate) %>% subset(!is.na(input$sanctionRate))
        
        })
      
      dta_sanctionRate <- selectedData_sanctionRate()
      
      output$sanctionRateGraph <- renderPlot({
        
        ggplot(data = dta_sanctionRate,
               aes(x = range, color = county, group = county)) +
          geom_line(aes_string(y = input$sanctionRate)) +
          theme_minimal()
        
        })
      })
    
    observeEvent(input$button4, {
      
      req(input$resolved, input$county.resolved)
      
      selectedData_resolved <- reactive({
        
        dta_resolved <- dta %>% filter(county == input$county.resolved) %>% subset(!is.na(input$resolved))
        
        })
      
      dta_resolved <- selectedData_resolved()
      
      output$resolvedGraph <- renderPlot({
        
        ggplot(data = dta_resolved,
               aes(x = range, color = county, group = county)) +
          geom_line(aes_string(y = input$resolved)) +
          theme_minimal()
        
        })
      })
    
    observeEvent(input$button5, {
      
      req(input$sanctionsPriorMonth, input$county.sanctionsPriorMonth)
      
      selectedData_sanctionsPriorMonth <- reactive({
        
        dta_sanctionsPriorMonth <- dta %>% filter(county == input$county.sanctionsPriorMonth) %>% subset(!is.na(input$sanctionsPriorMonth))
        
        })
      
      dta_sanctionsPriorMonth <- selectedData_sanctionsPriorMonth()
      
      output$sanctionsPriorMonthGraph <- renderPlot({
        
        ggplot(data = dta_sanctionsPriorMonth,
               aes(x = range, color = county, group = county)) +
          geom_line(aes_string(y = input$sanctionsPriorMonth)) +
          theme_minimal()
        
        })
      })
    
    observeEvent(input$button6, {
      
      req(input$resolutionRate, input$county.resolutionRate)
      
      selectedData_resolutionRate <- reactive({
        
        dta_resolutionRate <- dta %>% filter(county == input$county.resolutionRate) %>% subset(!is.na(input$resolutionRate))
        
        })
      
      dta_resolutionRate <- selectedData_resolutionRate()
      
      output$resolutionRateGraph <- renderPlot({
        
        ggplot(data = dta_resolutionRate,
               aes(x = range, color = county, group = county)) +
          geom_line(aes_string(y = input$resolutionRate)) +
          theme_minimal()
        
        })
      })
  
}

# OUTPUT
shinyApp(ui = ui, server = server)







