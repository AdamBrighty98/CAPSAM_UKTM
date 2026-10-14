################################################################################

# DESNZ/UKTM to UKIAM Link - via CAPSAM
# Takes single scenario energy data - from all_fin_year_totals
# Takes total vehicle number (Cap) - from all_cap_ncap_transport
# Translates new vehicle numbers (NCap) into fleet model template - from all_cap_ncap_transport


# Calling in other global files
#################################

# Calling in EFs and IFs 
source("EF_IF_Sheet.R")

# Calling in Road Transport EFs and IFs 
source("Road_EF_IF_Sheet.R")

# Calling in CAPSAM UI variables
source("UI_Variables.R")

source("uktm_translation_function.R")
source("emissions_calculation_function.R")
source("ukiam_translation_function.R")

###############################################################################################################################################
# Calling in libraries
#################################

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(ggplot2)
library(ggtext)
library(plotly)
library(tidyverse)
library(dplyr)
library(reshape2)
library(readxl)
library(writexl)
library(openxlsx)
library(cli)
library(htmltools)
library(shinyalert)
library(scales)


############################################################################################################################################################################################
# UI Side
################################

ui <- dashboardPage(
  
  dashboardHeader(title = "CAPSAM"),
  
  ###############################################################################################################################################
  # Tabs in sidebar
  
  dashboardSidebar(
    
    sidebarMenu(
      #menuItem("Pre-Processing", tabName = "preprocess", icon = icon("dashboard")),
      menuItem("Data Upload & Results", tabName = "upload", icon = icon("dashboard")),
      menuItem("Post-Processing", tabName = "postprocess", icon = icon("atom")),
      menuItem("About CAPSAM", tabName = "about", icon = icon("question"))
      
    )
  ),
  
  dashboardBody(
    
    ### Changing theme
    shinyDashboardThemes(
      
      theme = "purple_gradient"
      
    ),
    
    ###############################################################################################################################################
    # UI aspects for different tabs
    
    tabItems(
      
      ##########################################################################
      # Pre-Process Tab 
      
      # tabItem(tabName = "preprocess",
      #         h1("Pre-Processing", style = "color:white"),
      #         
      #         fixedRow(
      #           tabPanel("Data Input", 
      #                    
      #                    fileInput("UKTM_file", "Input UKTM Energy File: "),
      #                    downloadButton("download_ukiam_pollutant_xlsx", "Download UKIAM Pollutant Emissions", style = "color = #fff; background-color: green; border-color: black")
      #                    
      #           )
      #           
      #      )
      # ),
      
      
      ##########################################################################################################################
      # Upload tab
      
      tabItem(tabName = "upload",
              h1("Data Upload & Results", style = "color:white"),
              
              
              #################################################################################  
              
              # CSS inputs
              
              tags$style(".translationdown {
    
    transform: translateY(5px);
    
    }"
                         
              ),
              
              tags$style(".translationup {
    
    transform: translateY(-5px);
    
    }"
                         
              ),
              
              # Move 5 Pixels right to get all results in tabBox
              tags$style(".translationright {
    
    transform: translateX(15px);
    
    }"
                         
              ),
              
              ################################################################################ 
              
              fixedRow(
                
                # Information and Upload Box
                
                tabBox(title = "Parameters", width = 5,
                       
                       tabPanel("Welcome", 
                                actionButton("welcome", "Quick Information", icon = icon("info"), style = 'font-size:250%; color:white'),
                                #radioButtons("sector", "\n\nPick a sector (for now)", choices = sector, width = 600),
                                radioButtons("scenario", "\n\nSelect an emissions scenario", choices = scenario, width = 600),
                                radioButtons("pollutant", "\n\nPick a pollutant (for source-specific emissions)", choices = pollutant, width = 600))
                ),
                
                
                ##########################################################################################################################################################################################
                
                # Inputs for CSVs, with templates for download underneath
                
                
                column(width = 7,
                       
                       # Inputting elec gen CSV input
                       tabBox(title = "UK TIMES Scenario", 
                              tabPanel("Upload", 
                                       fileInput("uktm_file", "Select your UK TIMES file "),
                                       fileInput("road_uktm_file", "Select your UK TIMES road file"))),
                      
              #########################################################################################################################################################################################
              
              # Results Box
              
              tabBox(id = "resultsTab", title = "Results", width = 12,
                     
                     #tabPanel("Total Emissions", fluidRow(splitLayout(cellWidths = c("32.5%", "32.5%", "32.5%"), plotlyOutput("new_test"), plotlyOutput("new_test_total"), plotlyOutput("total_emissions_pm25"), class = "translationright"))),
                     #tabPanel("Total Impact", fluidRow(splitLayout(cellWidths = c("32.5%", "32.5%", "32.5%"), plotlyOutput("total_impacts_co2"), plotlyOutput("total_impacts_nox"), plotlyOutput("total_impacts_tpm25"), class = "translationright"))),
                     tabPanel("Total UK TIMES", fluidRow(splitLayout(cellWidths = c("97%"), plotlyOutput("uktm_total"), class = "translationright"))),
                     tabPanel("Total Activity Data", fluidRow(splitLayout(cellWidths = c("48.5%", "48.5%"), plotlyOutput("energy_ad"), plotlyOutput("road_ad"), class = "translationright"))),
                     tabPanel("Electricity Generation", fluidRow(splitLayout(cellWidths = c("48.5%", "48.5%"), plotlyOutput("elc_total"), plotlyOutput("elc_source"), class = "translationright"))),
                     tabPanel("Process", fluidRow(splitLayout(cellWidths = c("48.5%", "48.5%"), plotlyOutput("prc_total"), plotlyOutput("prc_source"), class = "translationright"))),
                     tabPanel("Residential Heating", fluidRow(splitLayout(cellWidths = c("48.5%", "48.5%"), plotlyOutput("res_total"), plotlyOutput("res_source"), class = "translationright"))),
                     tabPanel("Public Heating", fluidRow(splitLayout(cellWidths = c("48.5%", "48.5%"), plotlyOutput("ser_total"), plotlyOutput("ser_source"), class = "translationright"))),
                     tabPanel("Industrial Combustion", fluidRow(splitLayout(cellWidths = c("48.5%", "48.5%"), plotlyOutput("ind_total"), plotlyOutput("ind_source"), class = "translationright"))),
                     tabPanel("Road Transport", fluidRow(splitLayout(cellWidths = c("48.5%", "48.5%"), plotlyOutput("road_total"), plotlyOutput("road_source"), class = "translationright")))
              ),
              
              # Results Download Box
              
              tabBox(title = "Results Download", width = 12,
                   
                     tabPanel("Emissions Results Download", downloadButton("download_total_emissions_xlsx", "Download Emissions Results", style = "color = #fff; background-color: cyan; border-color: black")),
              tabPanel("Wide Activity Data Download", downloadButton("download_total_activity_data_xlsx", "Download Wide Activity Data", style = "color = #fff; background-color: green; border-color: black")))
              
                    ))),
      
      ##########################################################################
      # Post-Process Tab 
      
      tabItem(tabName = "postprocess",
              h1("Post-Processing", style = "color:white"),
              
              fixedRow(
                tabPanel("Data Input", 
                         
                         radioButtons("CAPSAM_Year", "Select year for emissions to be placed in UKIAM format", choices = CAPSAM_Year, width = 600),
                         radioButtons("other_emissions_scenario", "Select the scenario used for other sector's emissions within UKIAM", choices = other_emissions_scenario, width = 600),
                         radioButtons("woodburning_scenario", "Select the scenario used for woodburning emissions within UKIAM", choices = woodburning_scenario, width = 600),
                         fileInput("CAPSAM_file", "Input CAPSAM Emissions File: ")
                         
                ),
                
                tabPanel("UKIAM Pollutant Emissions Download", downloadButton("download_ukiam_pollutant_xlsx", "Download UKIAM Pollutant Emissions", style = "color = #fff; background-color: green; border-color: black")),
                tabPanel("UKIAM GHG Emissions Download", downloadButton("download_ukiam_GHG_xlsx", "Download UKIAM GHG Emissions", style = "color = #fff; background-color: orange; border-color: black"))
                
              )
      ),
      
      ##########################################################################
      # About Tab
      
      tabItem(tabName = "about",
              h1("About CAPSAM", style = "color:white"),
              
              fluidRow(tabBox(title = "Schematics and Methodology", width = 12,
                              
                              tabPanel("Overview and Flow Diagram",
                                       h3("The CAPSAM-UKTM tool has been designed to assess air pollutant emissions from the UK TIMES Model (UKTM). The model has 3 pre-installed runs from UKTM: Least Cost, High Hydrogen, and High Electrification. The model looks solely at energy-system decarbonisation."),
                                       h3("The air quality emissions are derived through applying NAEI emission factors (kt/TWh). A simple estimate of contributions to air pollution exposure can be completed by applying sector-specific impact factors (ng m-3 kt-1) to each emissions source. These are derived from the UK Integrated Assessment Model (UKIAM) and take into account the proximity of an emissions source to a given population."),
                                       h3("If choosing emissions, these are given in a format that can then be included within the full UKIAM framework. This emissions file is then appended to baseline emissions for other UKIAM sources before its inclusion into UKIAM."),
                                       
                                       imageOutput("schematic_img")
                                       
                              ),
                    
              ))
              
      )
      
    )))

#########################################################################################################################################################################################


# Server-side logic

server <- function(input, output, session) {
  
  
  # Rendering in images
  
  # Schematic to explain CAPSAM
  output$schematic_img <- renderImage({
    
    list(src = "www/CAPSAM_UKTM.png",
         width = "100%")
    
  }, deleteFile = FALSE)
  
  ###############################################################################
  ###############################################################################
  # STEP 1 - Reading and translating UKTM file
  # Reading input csv files
  
  uktm_scenario <- reactive({
    
    req(input$uktm_file)
    
    read_xlsx(input$uktm_file$datapath)
    
  })
  
  road_uktm_scenario <- reactive({
    
    req(input$road_uktm_file)
    
    read_xlsx(input$road_uktm_file$datapath)
    
  })
  
 ################################################################################
  # Cleaning UKTM file for scenario and sector
  
  processed_uktm_scenario <- reactive({
    
    req(uktm_scenario())
    
    processed_uktm_scenario <- sector_data(uktm_scenario()) # called from uktm_translation_function.R
    
    return(processed_uktm_scenario)
    
  })
  
  processed_road_uktm_scenario <- reactive({
    
    req(road_uktm_scenario())
    
    processed_road_uktm_scenario <- road_data_vkm_translation(road_uktm_scenario(), road_vkm, fleet_mix) # called from uktm_translation_function.R
    
    return(processed_road_uktm_scenario)
    
  })
  
  processed_uktm_scenario_wide <- reactive({
    
    req(uktm_scenario())
    
    processed_uktm_scenario_wide <- sector_data_output(uktm_scenario()) # called from uktm_translation_function.R
    
    return(processed_uktm_scenario_wide)
    
  })
  
  processed_uktm_scenario_fuel_split <- reactive({
    
    req(uktm_scenario())
    
    processed_uktm_scenario_fuel_split <- sector_data_fuel_split(uktm_scenario()) # called from uktm_translation_function.R
    
    return(processed_uktm_scenario_fuel_split)
    
  })
  
  processed_road_uktm_scenario_fuel_split <- reactive({
    
    req(road_uktm_scenario())
    
    processed_road_uktm_scenario_fuel_split <- road_data_fuel_split(road_uktm_scenario()) # called from uktm_translation_function.R
    
    return(processed_road_uktm_scenario_fuel_split)
    
  })
  
  ################################################################################
  ################################################################################
  # STEP 2 - All emissions calculated
  # Emissions calculation function called
  
  emissions <- reactive({
    
    emissions <- emissions_calculation(processed_uktm_scenario(), ef_processed) # called from emissions_calculation_function.R
    
    return(emissions)
    
  })
  
  
  road_emissions <- reactive({
    
    road_emissions <- road_emissions_calculation(processed_road_uktm_scenario(), road_ef_processed) # called from emissions_calculation_function.R
    
    return(road_emissions)
    
  })
  
  ################################################################################
  ################################################################################
  # STEP 3 - Presenting emissions projections in ggplot
  
  ################################################################################
  # Total UKTM emissions ggplot Outputs 
  
  # Total outputs
  output$uktm_total <- renderPlotly({
    
    req(uktm_sid_emissions()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    
    p <- uktm_sid_emissions() %>% 
      dplyr::select(-SID) %>% 
      pivot_longer(values_to = "emissions", names_to = "pollutant", cols = c("NH3", "SO2", "NOx", "PM2.5", "VOC")) %>% 
      dplyr::group_by(year, pollutant) %>% # grouping all emissions from each source by year and pollutant (for col = pollutant needed)
      dplyr::summarise(total = sum(emissions, na.rm = TRUE), .groups = "drop") %>% # summing emissions from each source within each year
      ggplot(aes(year, total, colour = pollutant)) + 
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })
  
  ################################################################################
  # Total UKTM activity data visualised 
  
  # Total energy AD
  output$energy_ad <- renderPlotly({
    
    req(processed_uktm_scenario_fuel_split()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    
    p <- processed_uktm_scenario_fuel_split() %>% 
      ggplot(aes(factor(year), demand, fill = fuel)) +
      geom_col() +
      theme(
        axis.text.x = element_text(angle = 90)
      ) +
      facet_wrap(~ sector) +
      labs(y = "Fuel Input (TWh)", x = "Year", fill = "Fuel")
    
    ggplotly(p)
    
  })
  
  # Total road AD
  output$road_ad <- renderPlotly({
    
    req(processed_road_uktm_scenario_fuel_split()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    
    p <- processed_road_uktm_scenario_fuel_split() %>% 
      ggplot(aes(factor(year), vehicle_numbers, fill = fuel)) +
      geom_col() +
      theme(
        axis.text.x = element_text(angle = 90)
      ) +
      labs(y = "Vehicle numbers (Thousands)", x = "Year", fill = "Fuel")
    
    ggplotly(p)
    
  })
  
  ################################################################################
  # Electricity ggplot Outputs 
  
  # Total outputs
  output$elc_total <- renderPlotly({
    
    req(emissions()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    
    p <- emissions() %>% 
      filter(
        sector == "ELC",
        ef_scenario == input$scenario
        ) %>% 
      dplyr::group_by(year, pollutant) %>% # grouping all emissions from each source by year
      dplyr::summarise(total = sum(emissions, na.rm = TRUE), .groups = "drop") %>% # summing emissions from each source within each year
      ggplot(aes(year, total, colour = pollutant)) + 
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  # Source-specific outputs
  output$elc_source <- renderPlotly({
    
    req(emissions())
    
    p <- emissions() %>% 
      filter(
        sector == "ELC",
        ef_scenario == input$scenario,
        pollutant == input$pollutant
      ) %>% 
      ggplot(aes(year, emissions, col = source)) +
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  ################################################################################
  # Process ggplot Outputs 
  
  # Total outputs
  output$prc_total <- renderPlotly({
    
    req(emissions()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    
    p <- emissions() %>% 
      filter(
        sector == "PRC",
        ef_scenario == input$scenario
      ) %>% 
      dplyr::group_by(year, pollutant) %>% # grouping all emissions from each source by year
      dplyr::summarise(total = sum(emissions, na.rm = TRUE), .groups = "drop") %>% # summing emissions from each source within each year
      ggplot(aes(year, total, colour = pollutant)) + 
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  # Source-specific outputs
  output$prc_source <- renderPlotly({
    
    req(emissions())
    
    p <- emissions() %>% 
      filter(
        sector == "PRC",
        ef_scenario == input$scenario,
        pollutant == input$pollutant
      ) %>% 
      ggplot(aes(year, emissions, col = source)) +
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  ################################################################################
  # Residential heating ggplot Outputs 
  
  # Total outputs
  output$res_total <- renderPlotly({
    
    req(emissions()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    
    p <- emissions() %>% 
      filter(
        sector == "RES",
        ef_scenario == input$scenario
      ) %>% 
      dplyr::group_by(year, pollutant) %>% # grouping all emissions from each source by year
      dplyr::summarise(total = sum(emissions, na.rm = TRUE), .groups = "drop") %>% # summing emissions from each source within each year
      ggplot(aes(year, total, colour = pollutant)) + 
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  # Source-specific outputs
  output$res_source <- renderPlotly({
    
    req(emissions())
    
    p <- emissions() %>% 
      filter(
        sector == "RES",
        ef_scenario == input$scenario,
        pollutant == input$pollutant
      ) %>% 
      ggplot(aes(year, emissions, col = source)) +
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  ################################################################################
  # Public heating ggplot Outputs 
  
  # Total outputs
  output$ser_total <- renderPlotly({
    
    req(emissions()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    
    p <- emissions() %>% 
      filter(
        sector == "SER",
        ef_scenario == input$scenario
        ) %>% 
      dplyr::group_by(year, pollutant) %>% # grouping all emissions from each source by year
      dplyr::summarise(total = sum(emissions, na.rm = TRUE), .groups = "drop") %>% # summing emissions from each source within each year
      ggplot(aes(year, total, colour = pollutant)) + 
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  # Source-specific outputs
  output$ser_source <- renderPlotly({
    
    req(emissions())
    
    p <- emissions() %>% 
      filter(
        sector == "SER",
        ef_scenario == input$scenario,
        pollutant == input$pollutant
      ) %>% 
      ggplot(aes(year, emissions, col = source)) +
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  ################################################################################
  # Industrial combustion ggplot Outputs 
  
  # Total outputs
  output$ind_total <- renderPlotly({
    
    req(emissions()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    
    p <- emissions() %>% 
      filter(
        sector == "IND",
        ef_scenario == input$scenario
      ) %>% 
      dplyr::group_by(year, pollutant) %>% # grouping all emissions from each source by year
      dplyr::summarise(total = sum(emissions, na.rm = TRUE), .groups = "drop") %>% # summing emissions from each source within each year
      ggplot(aes(year, total, colour = pollutant)) + 
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  # Source-specific outputs
  output$ind_source <- renderPlotly({
    
    req(emissions())
    
    p <- emissions() %>% 
      filter(
        sector == "IND",
        ef_scenario == input$scenario,
        pollutant == input$pollutant
      ) %>% 
      ggplot(aes(year, emissions, col = source)) +
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  }) 
  
  
  ################################################################################
  # Road transport ggplot outputs 
  
  # Calculate total PM2.5 emissions
  road_pm25_summary <- reactive({
    
    req(road_emissions())
    
    road_pm25_summary <- road_emissions() %>% 
      filter(
        pollutant == c("PM2.5_Exhaust", "PM2.5_Tyre", "PM2.5_Brake", "PM2.5_Road")
      ) %>% 
      dplyr::group_by(year, euro_tech) %>% 
      dplyr::summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>% 
      dplyr::mutate(
        pollutant = "PM2.5" # mutate() with new LHS of = to create new column
      ) %>% 
      dplyr::relocate(pollutant, .after = 2) # moving pollutant to third column
    
    return(road_pm25_summary)
    
  })
  
  # Include total PM2.5 alongside other emissions
  road_total_summary <- reactive({
    
    req(road_emissions()) # calling emissions from calculation function - grouped by sector and source and then row-wise calc of AD and net EF
    req(road_pm25_summary())
    
    road_emissions_short <- road_emissions() %>% 
      dplyr::select(year, euro_tech, pollutant, emissions) # selecting only relevant columns for road summary to match bind_rows()
    
    road_total_summary <- bind_rows(road_emissions_short, road_pm25_summary()) %>% 
      dplyr::rename(
        source = euro_tech
      )
    
    return(road_total_summary)
    
  })
  
  ######################################
  # Total outputs
  output$road_total <- renderPlotly({
    
    req(road_total_summary())
    
    p <- road_total_summary() %>% 
      dplyr::group_by(year, pollutant) %>% # grouping all emissions from each source by year
      dplyr::summarise(total = sum(emissions, na.rm = TRUE), .groups = "drop") %>% # summing emissions from each source within each year
      ggplot(aes(year, total, colour = pollutant)) + 
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  })  
  
  # Source-specific outputs
  output$road_source <- renderPlotly({
    
    req(road_total_summary())
    
    p <- road_total_summary() %>% 
      filter(
        pollutant == input$pollutant
      ) %>% 
      ggplot(aes(year, emissions, col = source)) +
      geom_line() + 
      labs(y = y_label_emissions)
    
    ggplotly(p)
    
  }) 
  
  
  ###############################################################################
  # UKTM SID Emissions 
  # Using SID mapping list (written long for easy R reading) and left_join() by source to match emissions with source_ids
  # Using hydrogen SID splitting sheet and a left_join() again based on source_id
  
  # UKTM Energy System SID Emissions
  energy_uktm_sid_emissions <- reactive({
    
    req(emissions())
    
    energy_uktm_sid_emissions <- energy_emissions_sid_translation(emissions(), input$scenario)
      
    return(energy_uktm_sid_emissions)
    
  })
  
  # UKTM UKIAM5 Road Transport SID Emissions
  road_uktm_sid_emissions <- reactive({
    
    req(road_total_summary())
    
    road_uktm_sid_emissions <- road_emissions_sid_translation(road_total_summary()) 
    
    return(road_uktm_sid_emissions)
    
  })
  
  # UKTM Emissions into UKIAM format
  uktm_sid_emissions <- reactive({
    
    req(energy_uktm_sid_emissions())
    req(road_uktm_sid_emissions())
    
    uktm_sid_emissions <- total_uktm_sid_translation(energy_uktm_sid_emissions(), road_uktm_sid_emissions()) 
    
    return(uktm_sid_emissions)
    
  })
  
  
  # Annual UKIAM emissions - joining UKTM Emissions and Non-Energy Emissions
  
  # 2020
  annual_ukiam_emissions_2020 <- reactive({
    
    req(uktm_sid_emissions())
    
    annual_ukiam_emissions_2020 <- total_ukiam_emissions_translation(uktm_sid_emissions(), ukiam_remaining_sids_all, dom_wood_adj, pm10_adj, 2020)
    
    return(annual_ukiam_emissions_2020)
    
  })
  
  # 2025
  annual_ukiam_emissions_2025 <- reactive({
    
    req(uktm_sid_emissions())
    
    annual_ukiam_emissions_2025 <- total_ukiam_emissions_translation(uktm_sid_emissions(), ukiam_remaining_sids_all, dom_wood_adj, pm10_adj, 2025)
    
    return(annual_ukiam_emissions_2025)
    
  })
  
  # 2030
  annual_ukiam_emissions_2030 <- reactive({
    
    req(uktm_sid_emissions())
    
    annual_ukiam_emissions_2030 <- total_ukiam_emissions_translation(uktm_sid_emissions(), ukiam_remaining_sids_all, dom_wood_adj, pm10_adj, 2030)
    
    return(annual_ukiam_emissions_2030)
    
  })
  
  # 2035
  annual_ukiam_emissions_2035 <- reactive({
    
    req(uktm_sid_emissions())
    
    annual_ukiam_emissions_2035 <- total_ukiam_emissions_translation(uktm_sid_emissions(), ukiam_remaining_sids_all, dom_wood_adj, pm10_adj, 2035)
    
    return(annual_ukiam_emissions_2035)
    
  })
  
  # 2040
  annual_ukiam_emissions_2040 <- reactive({
    
    req(uktm_sid_emissions())
    
    annual_ukiam_emissions_2040 <- total_ukiam_emissions_translation(uktm_sid_emissions(), ukiam_remaining_sids_all, dom_wood_adj, pm10_adj, 2040)
    
    return(annual_ukiam_emissions_2040)
    
  })
  
  # 2045
  annual_ukiam_emissions_2045 <- reactive({
    
    req(uktm_sid_emissions())
    
    annual_ukiam_emissions_2045 <- total_ukiam_emissions_translation(uktm_sid_emissions(), ukiam_remaining_sids_all, dom_wood_adj, pm10_adj, 2045)
    
    return(annual_ukiam_emissions_2045)
    
  })
  
  # 2050
  annual_ukiam_emissions_2050 <- reactive({
    
    req(uktm_sid_emissions())
    
    annual_ukiam_emissions_2050 <- total_ukiam_emissions_translation(uktm_sid_emissions(), ukiam_remaining_sids_all, dom_wood_adj, pm10_adj, 2050)
    
    return(annual_ukiam_emissions_2050)
    
  })
 
  #################################################################################################################################################################     
  # Results xlsx downloads - individual years for creating .txt files for UKIAM
  
    # Results download buttons
  output$download_total_emissions_xlsx <- downloadHandler(
    
    filename = function(){
      
      paste("CAPSAM_UKTM_Emissions_Data-", Sys.time(),".xlsx")
      
    },
    
    content = function(file){
      
      wb <- createWorkbook()
      
      addWorksheet(wb, sheetName = "2020_Emissions")
      writeData(wb, sheet = "2020_Emissions", x = annual_ukiam_emissions_2020())
      
      addWorksheet(wb, sheetName = "2025_Emissions")
      writeData(wb, sheet = "2025_Emissions", x = annual_ukiam_emissions_2025())
      
      addWorksheet(wb, sheetName = "2030_Emissions")
      writeData(wb, sheet = "2030_Emissions", x = annual_ukiam_emissions_2030())
      
      addWorksheet(wb, sheetName = "2035_Emissions")
      writeData(wb, sheet = "2035_Emissions", x = annual_ukiam_emissions_2035())
      
      addWorksheet(wb, sheetName = "2040_Emissions")
      writeData(wb, sheet = "2040_Emissions", x = annual_ukiam_emissions_2040())
      
      addWorksheet(wb, sheetName = "2045_Emissions")
      writeData(wb, sheet = "2045_Emissions", x = annual_ukiam_emissions_2045())
      
      addWorksheet(wb, sheetName = "2050_Emissions")
      writeData(wb, sheet = "2050_Emissions", x = annual_ukiam_emissions_2050())
      
      addWorksheet(wb, sheetName = "UKTM_Emissions_Data_Sheet")
      writeData(wb, sheet = "UKTM_Emissions_Data_Sheet", x = uktm_sid_emissions())
      
      addWorksheet(wb, sheetName = "UKTM_Energy_Emissions_Raw_Data")
      writeData(wb, sheet = "UKTM_Energy_Emissions_Raw_Data", x = emissions())
      
      addWorksheet(wb, sheetName = "UKTM_Road_Emissions_Raw_Data")
      writeData(wb, sheet = "UKTM_Road_Emissions_Raw_Data", x = road_emissions())
      
      saveWorkbook(wb, file)
      
    }
  )
  
  
  # Results download buttons
  output$download_total_activity_data_xlsx <- downloadHandler(
    
    filename = function(){
      
      paste("CAPSAM_UKTM_Activity_Data-", Sys.time(),".xlsx")
      
    },
    
    content = function(file){
      
      wb <- createWorkbook()
      
      addWorksheet(wb, sheetName = "UKTM_Wide_Actvity_Data")
      writeData(wb, sheet = "UKTM_Wide_Actvity_Data", x = processed_uktm_scenario_wide())
      
      saveWorkbook(wb, file)
      
    }
  )
  
  
  observeEvent(input$welcome, {
    
    shinyalert("Welcome to CAPSAM!", "This website is designed to read energy system data (in TWh) for a variety of sectors. From here, CAPSAM will output GHG emissions, air quality impacts, and damage cost data.\n\nTo use CAPSAM, upload your energy data into the required slots on the 'Import Scenarios CSV' page (empty and completed templates are also provided).\n\nCAPSAM also allows for a range of different data forms for building heating and industrial combustion. Select the drop down on the 'Scenario Questions' page to determine your data type - CAPSAM can then make some basic assumptions to produce the necessary data.\n\nEmissions and impacts can also be changed to be 'per TWh', and using both 20-year and 100-year global warming potentials.\n\nAir pollution and GHG results can be downloaded as a .csv for all sectors in one file, or individually, by clicking to corresponding download button in the 'Results Download' box.\n\nMore information is provided on the 'About CAPSAM' page.", type = "info", size = "m")
    
  })
  
  
  #######################################################################################################################################################################
  ##########################################################################################################################################################################

}

shinyApp(ui = ui, server = server)