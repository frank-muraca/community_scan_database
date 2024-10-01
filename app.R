library(shiny)
library(here)
library(downloadthis)
library(tidyverse)
library(gghighlight)
library(leaflet)
library(reactable)
library(readxl)
library(shinythemes)
library(Cairo)
library(data.table)
library(openxlsx)
library(parallel)

# Set for higher resolution charts

        options(shiny.usecairo = TRUE)

# Set DFI's color palette
        
        DFI_palette <- c("#44546a", "#fca129", "#4b9cd3", "#e65042", "#6954a2", "#42a0ac", "grey50")

# Load data once
        data_list <- list(
          county_tiers = read_csv(here('data', 'economic', '2024_county_tiers.csv')),
          census_pop_estimates = read_csv(here('data', 'demographic', 'census', 'census_pop_estimates.csv')),
          age_over_time = read_csv(here('data', 'demographic', 'census', 'age_over_time.csv')),
          sex_by_age = read_csv(here('data', 'demographic', 'census', 'sex_by_age.csv')),
          tenure_by_age = read_csv(here('data', 'demographic', 'census', 'tenure_by_age.csv')),
          edu_attain = read_csv(here('data', 'demographic', 'census', 'edu_attain.csv')),
          median_hh_income = read_csv(here('data', 'demographic', 'census', 'median_hh_income.csv')),
          hh_type = read_csv(here('data', 'demographic', 'census', 'hh_type.csv')),
          travel_time = read_csv(here('data', 'demographic', 'census', 'travel_time.csv')),
          race_eth = read_csv(here('data', 'demographic', 'census', 'race_eth.csv')),
          population_proj = read_csv(here('data', 'demographic', 'total_projections.csv')),
          age_proj = read_csv(here('data', 'demographic', 'age_projections.csv')),
          component_pop_change = read_csv(here('data', 'demographic', 'census', 'component_pop_change.csv')),
          county2county_flows = read_csv(here('data', 'demographic', 'census', 'county2county_flows.csv')),
          county_lodes = read_csv(here('data', 'demographic', 'census', 'county_lodes.csv')),
          tenure_units_year = read_csv(here('data', 'housing', 'census', 'tenure_units_year.csv')),
          tenure = read_csv(here('data', 'housing', 'census', 'tenure.csv')),
          tenure_race = read_csv(here('data', 'demographic', 'census', 'tenure_race.csv')),
          rent_data = read_csv(here('data', 'housing', 'census', 'rent_data.csv')),
          med_val = read_csv(here('data', 'housing', 'census', 'med_val.csv')),
          tenure_year_built = read_csv(here('data', 'housing', 'census', 'tenure_year_built.csv')),
          tenure_bedrooms = read_csv(here('data', 'housing', 'census', 'tenure_bedrooms.csv')),
          vacancy_status = read_csv(here('data', 'housing', 'census', 'vacancy_status.csv')),
          unemployment_rate = read_csv(here('data', 'economic', 'DETAILLAUS.csv')) %>%
            mutate(month_year = zoo::as.yearmon(paste(Year, Month), "%Y %m")),
          industry = read_csv(here('data', 'economic', 'DETAILQCEW_2000_2023.csv')),
          largest_employ = read_csv(here('data', 'economic', 'QCEWLARGESTEMPLOYERS.csv'))
        )
        
        data_list <- lapply(data_list, as.data.table)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme('yeti'),
  br(),
  sidebarPanel(
    h3(strong("DFI Community Scan Database"), style = "font-size:21px;"),
    style = "position:fixed;width:30%;",
    selectizeInput("select_geo", "Geography type:",
                   choices = c('Counties', 'Places'),
                   multiple = FALSE,
                   selected = 'Counties'),
    uiOutput('select_place'),
    tags$div("This is a",
             strong(tags$a(href = "https://www.commerce.nc.gov/grants-incentives/county-distress-rankings-tiers",
                           textOutput('county_tier', inline = TRUE))),
             'county. (2024)'),
    h4("Note: Some data is only available at the county level."),
    p('This database shows recent demographic, housing, and economic trends in every county and place in North Carolina.'),
    p("The data should be consulted as part of DFI's pre-development process, but is unable to capture everything that might be important for a project. Project team members should investigate other data sources that they think might be important as part of the community scan."),
    p('Project team members can download and visualize data as needed.'),
    downloadButton('download_all', "Download All Data")
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                
                # Demographics UI ====
                tabPanel("Demographics",
                         
                         h3("Longterm population projections"),
                         p('The NC Office of State Budget & Management (NC OSBM) produces population estimates at the county level out to 2050.'),
                         plotOutput('population_proj_plot'),
                         br(), br(),
                         
                         h3("Longterm population projections by age bracket"),
                           p('This charts below show NC OSBM population projections by age bracket.'),
                           p('Each panel shows an age bracket with the other age brackets greyed out in the background.'),
                           div(plotOutput('age_proj_plot', width = 800, height = 700)),
                         
                         h4('Five year population change by age bracket'),
                           p('The chart below shows the same projection data as above but to 2028.'),
                           plotOutput('five_year_age_proj_plot'),
                         br(), br(),
                         
                         h3("Census Population Estimates"),
                           p('The data below shows changes in population estimates at the county AND place level using estimates from the Census Population Estimates Program (PEP).'),
                           p('PEP annually utilizes current data on births, deaths, and migration to calculate population change since the most recent decennial census and produce a time series of estimates of population, demographic components of change, and housing units. The annual time series of estimates begins with the most recent decennial census data and extends to the year before the next decennial census (in this case 2019).'),
                           p(strong("Note:"), "The data below contains estimates from two decennial censuses (2010 and 2020). You may notice a dramatic change in population between 2019 and 2020. This is not necessarily because there was a large shift in that one year but because more accurate population counts were collected during the 2020 decennial census."),
                           plotOutput('total_population_plot'),
                         br(), br(),
                         
                         h3('Population Change by Age Estimates (ACS)'),
                           p('The chart below shows changes in population by age based on 5-year estimates from the Census.'),
                           p('Note: The data is different from the population projections above produced by NC OSBM.'),
                           plotOutput('age_over_time_plot'),
                         
                         h3("Components of population change"),
                           p('The Census produces county-level data on how a population changes. Population change can be from net migration or natural increase (when the number of births is greater than the number of deaths).'),
                           plotOutput('component_pop_change_plot'),
                         br(), br(),
                         
                         h3('County to county migration flows'),
                         tags$div("The Census publishes",
                                  tags$a(href = "https://www.census.gov/topics/population/migration/data.html",
                                         "estimates on which counties people are moving to and from.")),
                         p('For geographies of population 65,000+, estimates are for one year. For geographies 20,000-65,000, estimates are for a three year period and five years for geographies smaller than 20,000.'),
                         reactableOutput('county2county_flow_tbl'),
                         
                         h3('Population by age and binary sex'),
                         p('This population pyramid shows the number people in each 5-year age bracket by binary sex.'),
                         div(plotOutput('sex_by_age_plot', width = 600, height = 500), align = "center"),
                         br(), br(),
                         
                         h3('Population by race & ethnicity'),
                         tags$div("When visualizing or communicating data about race & ethnicity, consider consulting the",
                                  tags$a(href = "https://www.urban.org/research/publication/do-no-harm-guide-applying-equity-awareness-data-visualization",
                                         "Urban Institute's Do No Harm Guide for applying equity awareness to data visualization.")),
                         plotOutput('race_eth_plot'),
                         br(), br(),
                         
                         h3("Tenure by age of householder"),
                         p('This chart shows the number of households by age and tenure.'),
                         plotOutput('tenure_by_age_plot'),
                         br(), br(),
                         
                         h3("Educational attainment compared to North Carolina"),
                         plotOutput('edu_attain_plot'),
                         br(), br(),
                         
                         h3("Change in median household income by tenure"),
                         p('This chart shows changes in the median household income by tenure between 2009 and 2021. It also displays the same data at the state level for comparison.'),
                         plotOutput('median_hh_income_plot'),
                         br(), br(),
                         
                         h3("Household Type"),
                         p("Number of households by partner status and living arrangements."),
                         plotOutput('hh_type_plot'),
                         br(), br(),
                         
                         h3("Change in number of workers by commuting pattern and earnings"),
                         p('The Census produces estimates on the number of workers who commute to and from a locality under the LEHD Origin-Destination Employment Statistics (LODES) program.'),
                         p('The charts below show changes in the number of workers who live and work locally, live locally but commute outside the county for work, and live elsewhere but commute to the selected county for work.'),
                         plotOutput('commuting_pattern_change_plot'),
                         br(), br(),
                         
                         h3("Commute time"),
                         plotOutput('travel_time_plot'),
                         br(), br()
                ),
                # Housing Supply UI ====
                tabPanel("Housing Supply",
                         h3('Vacancy rate by vacancy type'),
                         p('The Census measures several types of vacancy, including units that are for rent to year-round tenants and units for seasonal or recreational use.'),
                         p('"Other vacant" referes to units that are not inhabited nor on the market for rent or fore sale. Housing researchers generally consider these homes to be vacant due to low-quality or physical neglect and is often used as a measure for housing quality.'),
                         plotOutput('vacancy_status_plot'),
                         br(), br(),
                         h3('Change in rental units by contract rent'),
                         p('This chart shows the number of rental units by contract rent (excluding costs of utilities). The trend shows the overall change in rental units and whether the number of low-cost units (<$350) is increasing or decreashing.'),
                         p('Note: The rent categories for this chart are arbitrary and only meant to capture high-level changes in the rental market. Use the DFI Housing Needs Database to lookup which rents are "affordable" to different income brackets.'),
                         div(plotOutput('rent_unit_plot', width = 700), align = "center"),
                         br(), br(),
                         h3('Change in median home value of owner-occupied housing'),
                         p("The ACS asks homeowners to self-report the total value of their home (building + land)."),
                         plotOutput('med_val_plot'),
                         br(), br(),
                         h3('Change in tenure'),
                         plotOutput('tenure_plot'),
                         br(), br(),
                         h3('Change in tenure by race & ethnicity'),
                         p("The charts below show changes in the number of owners and renters by race and ethnicity."),
                         p("Note: Y axis values are different for each chart to show both small and large changes. Racial and ethnic groups that are relatively small within a community are more likely to have larger margins of error."),
                         div(plotOutput('tenure_race_plot', width = 800, height = 700)),
                         br(), br(),
                         h3("Tenure by housing type by year built"),
                         p("The chart below shows two datapoints: Which housing types renters and owners live in and the age of those units."),
                         p("Housing age is one indicator of housing quality. Federal legislation regulating the use of lead and asbestos in building materials was enacted in the late 1970s, so we track the number of units built before 1980 as a potential measure of housing quality."),
                         plotOutput('tenure_units_year_plot'),
                         br(), br(),
                         h3('Tenure by bedroom count'),
                         plotOutput('tenure_bedrooms_plot'),
                         br(), br()
                ),
                # Economics UI ====
                tabPanel("Economics",
                         h3('Long-term unemployment rate'),
                         p('This chart shows the monthly employment rate from 1990 to 2024. The data is not seasonally adjusted.'),
                         plotOutput('unemployment_rate_plot'),
                         br(), br(),
                         h3('Employment + Establishments by Industry'),
                         p('This charts below show change in the total number of employees (public and private) per 2-digit NAICS code.'),
                         p('The data is produced by the BLS Quarterly Census of Employment and Wages (QCEW) and published by NC Commerce.'),
                         p('The BLS will suppress values for years when there were not enough businesses in an industry to protect the privacy of those businesses.'),
                         div(plotOutput('industry_employment_plot', width = 800, height = 1500), align = 'center'),
                         br(), br(),
                         h3("Top Employers in 2023 Q4"),
                         p('This table shows the largest employers in the selected county.'),
                         reactableOutput('top_employers_tbl'),
                         br(), br()
                ),
                # About ====
                tabPanel('About',
                         h2("About the Community Scan Database"),
                         p('This database was created in January 2023 to assist project teams with community scans. The database compiles data sources from the Census, BLS, and other institutions on current demographic and economic topics.'),
                         strong(p("This database does not contain every datapoint that might be important for a project.")), p("Project teams will likely have to consult other data sources that may be relevant for a project."),
                         p("Additionally, some data sources in this tool are not available for municipalities below the county-level."),
                         tags$div("If you have more questions about this database,", tags$a(href = "mailto:muraca@sog.unc.edu", "email muraca@sog.unc.edu.")),
                         h3("What is a community scan?"),
                         p("Community scans are a comprehensive 'first look' at local demographic, economic, and social conditions that may impact a project, and are the first step in DFI's pre-development process."),
                         p("In addition to demographic and economic data, project teams should also consult recent reports, plans, news articles, and stakeholders to understand recent trends in the community."),
                         h3("NC OSBM Population Projections"),
                         p("The NC State Demographer in the Office of State Budget and Management (OSBM) releases population projections for the state and counties of North Carolina, showing population by age, sex, and race/ethnicity."),
                         p("NC OSBM relies on data from the 2000, 2010, and 2020 census in addition to supplementary datasets like vital statistics (births, deaths, etc), school enrollment, and vehicle registration."),
                         p("In December 2022, NC OSBM made some adjustments in response to new information about the impacts of COVID-19 on previous census counts."),
                         tags$div(tags$a(href = "https://www.osbm.nc.gov/facts-figures/population-demographics/state-demographer/countystate-population-projections", "Read more about the population projection methodology and access the original data on the NC OSBM website.")),
                         h3("How to use Census estimates"),
                         p("The Census data included in this database is from the American Community Survey (ACS). The ACS samples 1% of the U.S. population every year and asks questions related to housing, household income, demographics, and many other variables."),
                         p("Unlike the decennial Census, the ACS only produces estimates for each of these datapoints. Estimates tend to have wider margins of error when geographies are below the county level (Census tracts or small places) or when the unit measured is small. For example, if there are relatively few Latinx households in a community, data about those households are likely to have large MOEs."),
                         p("The ACS data in this database comes from 5-year estimates, which pull from a sample across five years rather than a single point in time. While 1-year estimates are only produced for communities with more than 65,000 people, 5-year estimates are produced for all geographies."),
                         p("When using 5-year estimates to compare data over time, it's generally best practice to compare years without overlapping samples. For example, if you compare the ACS 5-year estimates for 2019 and 2021, both years will pull data from the same years (2017, 2018, and 2019). The visualizations in this database compare 5-year estimates for years without overlapping data (2011, 2016, and 2021)."),
                         tags$div(tags$a(href = "https://www.ncdemography.org/2022/02/24/mailbag-questions-about-acs-census-and-population-estimates/", "Carolina Demography has several helpful blog posts on how to use and interpret ACS data.")),
                         h3("Census estimates and COVID-19"),
                         p("COVID-19 had significant impacts on the Census Bureau's ability to collect data in 2020 while social distancing guidelines were in place."),
                         p("During the data collection process, Census employees will go out to households selected for the ACS when households fail to complete the survey. In-person follow-up was suspended for much of 2020, and resulted in a below-average response rate."),
                         tags$div(tags$a(href = "https://www.jchs.harvard.edu/blog/defining-use-caution-how-were-navigating-new-bureau-data",
                                         "Researchers have generally cautioned against using ACS data from 2020 due to disruptions from COVID-19.")),
                         h3("Quarterly Census of Employment & Wages (QCEW)"),
                         p("QCEW is a federal-state initiative between NC Commerce and the Bureau of Labor Statistics (BLS) to produce quarterly employment and wage statistics down to the county level."),
                         p("Not all data series are available for all counties and some values may be suppressed to protect the confidentiality of local employers."),
                         tags$div(tags$a(href = "https://d4.nccommerce.com/QCEWSelection.aspx",
                                         "NC Commerce maintains a QCEW data through their Demand Driven Data Delivery system.")),
                         h3("Largest Employers"),
                         p("NC Commerce publishes quarterly reports on the largest public and private-sector employers at the county level."),
                         tags$div(tags$a(href = "https://d4.nccommerce.com/QCEWLargestEmployers.aspx",
                                         "NC Commerce maintains a largest employer data through their Demand Driven Data Delivery system.")),
                         br(), br()
                )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ## Geography selection =====
  output$select_place <- renderUI({
    dat <- if ("Counties" %in% input$select_geo) {
      data_list$census_pop_estimates |>
        filter(grepl("County", place))
    } else {
      data_list$census_pop_estimates |>
        filter(!grepl("County", place))
    }
    place_list <- sort(unique(dat$place))
    selectizeInput("select_place", "Location:", choices = place_list, multiple = FALSE)
  })
  
  ##Filtered data function =====
  
  filtered_data <- reactiveVal()
  
  observeEvent(input$select_place, {
    place <- input$select_place
    
    # Filter data based on the selected place once and store in a reactive value
    data <- list(
      
      # Population projections
      population_proj = data_list$population_proj[data_list$population_proj$county == place, ],
      
      # Age Projections
      age_proj = data_list$age_proj[data_list$age_proj$county == place, ],
      
      # Census Population Estimates
      census_pop_estimates = data_list$census_pop_estimates[data_list$census_pop_estimates$place == place, ],
      
      # Age over time
      age_over_time = data_list$age_over_time[data_list$age_over_time$NAME == place, ],
      
      # Component Pop Change
      component_pop_change = data_list$component_pop_change[data_list$component_pop_change$NAME == place, ],
      
      # County to county flows
      county2county_flows = data_list$county2county_flows[data_list$county2county_flows$FULL1_NAME == place, ],
      
      # Population Pyramid
      sex_by_age = data_list$sex_by_age[data_list$sex_by_age$NAME == place, ],
      
      # Tenure by age
      tenure_by_age = data_list$tenure_by_age[data_list$tenure_by_age$NAME == place, ],
      
      # Race / eth
      race_eth = data_list$race_eth[data_list$race_eth$NAME == place, ],
      
      # Educational attainment
      edu_attain = data_list$edu_attain[data_list$edu_attain$NAME == place, ],
      
      # Median hh income
      median_hh_income = data_list$median_hh_income[data_list$median_hh_income$NAME == place, ],
      
      # Household Type
      hh_type = data_list$hh_type[data_list$hh_type$NAME == place, ],
      
      # Travel time
      travel_time = data_list$travel_time[data_list$travel_time$NAME == place, ],
      
      # Vacancy status
      vacancy_status = data_list$vacancy_status[data_list$vacancy_status$NAME == place, ],
      
      # Rent unit
      rent_unit = data_list$rent_data[data_list$rent_data$NAME == place, ],
      
      # Median value
      med_val = data_list$med_val[data_list$med_val$NAME == place, ],
      
      # Tenure
      tenure = data_list$tenure[data_list$tenure$NAME == place, ],
      
      # Tenure race
      tenure_race = data_list$tenure_race[data_list$tenure_race$NAME == place, ],
      
      # Tenure units year
      tenure_units_year = data_list$tenure_units_year[data_list$tenure_units_year$NAME == place, ],
      
      # Tenure bedrooms
      tenure_bedrooms = data_list$tenure_bedrooms[data_list$tenure_bedrooms$NAME == place, ],
      
      # Unemployment rate
      unemployment_rate = data_list$unemployment_rate[data_list$unemployment_rate$`Area Name` == place, ],
      
      # Industry employment
      industry_employment = data_list$industry[data_list$industry$`Area Name` == place, ],
      
      # Largest employers
      largest_employ = data_list$largest_employ[data_list$largest_employ$`Area Name` == place, ]
      
    )
    
    filtered_data(data)
  })
  
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste(input$select_place, "All_Data.xlsx", sep = "_")
    },
    content = function(file) {
      place <- input$select_place
      
      # Create a new workbook
      wb <- createWorkbook()
      
      # Add each dataset as a separate sheet
      addWorksheet(wb, "Population Projections")
      writeData(wb, "Population Projections", 
                data_list$population_proj %>% filter(county == place))
      
      addWorksheet(wb, "Age Projections")
      writeData(wb, "Age Projections", 
                data_list$age_proj %>% filter(county == place))
      
      addWorksheet(wb, "Census Population Estimates")
      writeData(wb, "Census Population Estimates", 
                data_list$census_pop_estimates %>% filter(place == place))
      
      addWorksheet(wb, "Age Over Time")
      writeData(wb, "Age Over Time", 
                data_list$age_over_time %>% filter(NAME == place))
      
      addWorksheet(wb, "Component Population Change")
      writeData(wb, "Component Population Change", 
                data_list$component_pop_change %>% filter(NAME == place))
      
      addWorksheet(wb, "County to County Flows")
      writeData(wb, "County to County Flows", 
                data_list$county2county_flows %>% filter(FULL1_NAME == place))
      
      addWorksheet(wb, "Sex by Age")
      writeData(wb, "Sex by Age", 
                data_list$sex_by_age %>% filter(NAME == place))
      
      addWorksheet(wb, "Tenure by Age")
      writeData(wb, "Tenure by Age", 
                data_list$tenure_by_age %>% filter(NAME == place))
      
      addWorksheet(wb, "Race and Ethnicity")
      writeData(wb, "Race and Ethnicity", 
                data_list$race_eth %>% filter(NAME == place))
      
      addWorksheet(wb, "Educational Attainment")
      writeData(wb, "Educational Attainment", 
                data_list$edu_attain %>% filter(NAME == place))
      
      addWorksheet(wb, "Median Household Income")
      writeData(wb, "Median Household Income", 
                data_list$median_hh_income %>% filter(NAME == place))
      
      addWorksheet(wb, "Household Type")
      writeData(wb, "Household Type",
                data_list$hh_type %>% filter(NAME == place))
      
      addWorksheet(wb, "Travel Time")
      writeData(wb, "Travel Time", 
                data_list$travel_time %>% filter(NAME == place))
      
      addWorksheet(wb, "Vacancy Status")
      writeData(wb, "Vacancy Status", 
                data_list$vacancy_status %>% filter(NAME == place))
      
      addWorksheet(wb, "Rent Data")
      writeData(wb, "Rent Data", 
                data_list$rent_data %>% filter(NAME == place))
      
      addWorksheet(wb, "Median Value")
      writeData(wb, "Median Value", 
                data_list$med_val %>% filter(NAME == place))
      
      addWorksheet(wb, "Tenure")
      writeData(wb, "Tenure", 
                data_list$tenure %>% filter(NAME == place))
      
      addWorksheet(wb, "Tenure by Race")
      writeData(wb, "Tenure by Race", 
                data_list$tenure_race %>% filter(NAME == place))
      
      addWorksheet(wb, "Tenure Units by Year")
      writeData(wb, "Tenure Units by Year", 
                data_list$tenure_units_year %>% filter(NAME == place))
      
      addWorksheet(wb, "Tenure by Bedrooms")
      writeData(wb, "Tenure by Bedrooms", 
                data_list$tenure_bedrooms %>% filter(NAME == place))
      
      addWorksheet(wb, "Unemployment Rate")
      writeData(wb, "Unemployment Rate", 
                data_list$unemployment_rate %>% filter(`Area Name` == place))
      
      addWorksheet(wb, "Industry Employment")
      writeData(wb, "Industry Employment", 
                data_list$industry %>% filter(`Area Name` == place))
      
      addWorksheet(wb, "Largest Employers")
      writeData(wb, "Largest Employers", 
                data_list$largest_employ %>% filter(`Area Name` == place))
      
      # Save the workbook to the specified file path
      saveWorkbook(wb, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  
  
  ## County tier print =====
  output$county_tier <- renderText({
    county_tiers <- data_list$county_tiers
    county_tiers |>
      mutate(COUNTY = str_trim(paste0(tools::toTitleCase(tolower(COUNTY)), " County"))) |>
      filter(COUNTY == input$select_place) |>
      mutate(`2023 Tiers` = paste0("Tier ", `2023 Tiers`)) |>
      pull(`2023 Tiers`)
  })
  
  ## NC OSBM Projections ====      
  output$population_proj_plot <- renderPlot({
    
    data <- filtered_data()$population_proj
    
    endpoints <- data |>
      filter(year %in% c(2023, 2050))
    
    
    data |>
      ggplot(aes(x = year, y = total)) +
      geom_point(data = endpoints, aes(x = year, y = total), size = 3) +
      geom_line(linewidth = .8) +
      geom_text(data = endpoints, aes(label = scales::comma(total)), vjust = -0.7, size = 5) +
      expand_limits(y = 0) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = 0.1)) +
      scale_x_continuous(expand = expansion(mult = 0.1)) +
      labs(
        x = "Year",
        y = "Total Population",
        caption = "Source: NC Office of State Budget & Management"
      )
  })
  

  
  ## NC OSBM Population Age Projections ===== 
  output$age_proj_plot <- renderPlot({
    
    data <- filtered_data()$age_proj
    
    endpoints <- data |>
      filter(year %in% c(2023, 2050))
    
    data |>
      ggplot(aes(x = year, y = total, group = decade)) +
      geom_point(data = endpoints, aes(x = year, y = total, group = decade), size = 3, show.legend = FALSE, color = DFI_palette[1]) +
      geom_line(linewidth = .8, show.legend = FALSE, color = DFI_palette[1]) +
      gghighlight(use_direct_label = FALSE, unhighlighted_params = list(linewidth = .5, colour = alpha("grey80", 0.6))) +
      facet_wrap(~decade, ncol = 3) +
      expand_limits(y = 0) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(linewidth = .5), panel.grid.major.y = element_line(linewidth = .5), strip.text = element_text(size = 16, hjust = 0)) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = DFI_palette) +
      labs(
        x = "Year",
        y = "Total Population",
        caption = "Source: NC Office of State Budget & Management",
        color = "Age bracket"
      )
  })
  
  ## Five year population change by age ====
  output$five_year_age_proj_plot <- renderPlot({
    
    data <- filtered_data()$age_proj |>
      
      filter(year %in% c(2023, 2028)) |>
      
      pivot_wider(id_cols = decade, names_from = year, values_from = total) |>
      
      mutate(five_year_change = `2028` - `2023`) |>
      
      pivot_longer(cols = five_year_change, names_to = 'variable', values_to = 'total')
    
    
    data |>
      mutate(color = if_else(total > 0, "One", "Two")) |>
      ggplot(aes(x = decade, y = total, group = decade, fill = color)) +
      geom_col(width = .5, show.legend = FALSE) +
      geom_text(aes(label = scales::comma(total), vjust = 0.5 - sign(total)), size = 5) +
      expand_limits(y = 0) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(linewidth = .5), panel.grid.major.y = element_line(linewidth = .5), strip.text = element_text(size = 16, hjust = 0)) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = DFI_palette[c(1, 4)]) +
      labs(
        x = "Age Bracket",
        y = "Change in population",
        caption = "Source: NC Office of State Budget & Management"
      )
  })
  

  
  ## Census Pop Estimates ====
  output$total_population_plot <- renderPlot({
    
    data <- filtered_data()$census_pop_estimates 
    
    data <- data |> 
      
      filter(input$select_place == place) |>
      
      pivot_longer(cols = c(`2010`:`2022`), names_to = "year", values_to = "estimate")
    
    data |>
      
      ggplot(aes(x = as.double(year), y = estimate)) +
      geom_point(size = 3) +
      geom_line(linewidth = .8) +
      geom_vline(xintercept = 2019.5, linetype = "dashed", color = "firebrick") +
      geom_text(aes(label = scales::comma(estimate)), vjust = -0.7, size = 4.5) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = 0.1)) +
      scale_x_continuous(expand = expansion(mult = 0.1), limits = c(2010, 2021)) +
      expand_limits(y = 0) +
      scale_color_manual(values = DFI_palette[1]) +
      labs(
        x = "Year",
        y = "Total Population Estimate",
        caption = "Source: Census Population Estimates"
      )
  })
  

  
  ## ACS Population Age Change ====
  
  output$age_over_time_plot <- 
      
    renderPlot({
         
        data <- filtered_data()$age_over_time  
    
        data |>
            
          ggplot(aes(x = age_bracket_compress, y = total, fill = as.factor(year))) +
            
          geom_col(width = .45, position = position_dodge(width = .5)) +
            
          theme_minimal(base_size = 20, base_family = "Helvetica") +
            
          theme(legend.position = "top") +
            
          scale_y_continuous(labels = scales::comma) +
      
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      
          scale_fill_manual(values = c("#C8DFE7", "#5B9EB8", DFI_palette[1])) +
      
          expand_limits(y = 0) +
            
          labs(
              x = "Age Bracket",
              y = "Estimate",
              caption = "Source: ACS 2012, 2017, 2022 (5-Year Estimates)",
              fill = "5-year estimates:"
          )
  })
  
  
  
  ## Population Change by Component =====
  output$component_pop_change_plot <- renderPlot({
    
    component_pop_change_plot_data <- data_list$component_pop_change |> 
      filter(NAME == input$select_place)
    component_pop_change_plot_data |>
      ggplot(aes(x = year, y = value, fill = name)) +
      geom_col(position = "dodge") +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      labs( 
        y = 'Estimate', x = "Year",
        caption = "Source: Census Population Estimates",
        fill = "Change type:"
      ) +
      theme(legend.position = "top") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(2010, 2022, 1)) +
      scale_fill_manual(values = DFI_palette)
  })
  

  
  ## County to county migration =====
  output$county2county_flow_tbl <- renderReactable({
      
      data <- filtered_data()$county2county_flows |>
      select(FULL2_NAME, MOVEDIN, MOVEDOUT, MOVEDNET) |>
      rename('County' = 'FULL2_NAME',
             'People moved from' = 'MOVEDIN',
             'People moved to' = 'MOVEDOUT',
             'Net migration' = 'MOVEDNET') |>
      arrange(-`People moved from`)
    reactable(data, defaultColDef = colDef(format = colFormat(separators = TRUE)))
  })
  

  
  ## Population Pyramid ===== 
  output$sex_by_age_plot <- renderPlot({
    
    data <- filtered_data()$sex_by_age
    
    data |>
      mutate(age_bracket_raw = fct_relevel(age_bracket_raw,"Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 84 years", "85 years and over")) |>
      mutate(total = ifelse(binary_sex == "Male", total * (-1), total * 1)) |>
      ggplot(aes(y = age_bracket_raw, x = total, fill = binary_sex)) +
      geom_col(width = .8) +
      labs(
        x = "Total", y = "Age Bracket",
        fill = "Binary Sex", caption = "ACS 2022 (5-year Estimates)"
      ) +
      scale_fill_manual(values = DFI_palette[c(1, 2)]) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      scale_x_continuous(labels = abs)
  })
  
 
  
  ## Tenure by age =====
  output$tenure_by_age_plot <- renderPlot({
   
    
    data <- filtered_data()$tenure_by_age
    
    if (is.null(data)) {
      print("Data is NULL")  # Debugging line
      return(NULL)  # Handle case when data is not available yet
    }
    
    print(data)  # Debugging line to see the content of data
    
    
    data |>
      mutate(estimate = ifelse(tenure == "Owner", estimate * (-1), estimate * 1)) |>
      ggplot(aes(y = age_bracket_new, x = estimate, fill = tenure)) +
      geom_col(width = .6) +
      labs(x = "Total", y = "Age Bracket", fill = "Tenure", caption = "ACS 2022 (5-year Estimates)") +
      scale_fill_manual(values = DFI_palette[c(1, 2)]) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      scale_x_continuous(labels = abs)
  })
  
 
  
  ## Race/ethnicity =====  
  output$race_eth_plot <- renderPlot({
    
    data <- filtered_data()$race_eth
    data |>
      ggplot(aes(x = race_eth, y = total)) +
      geom_col(fill = DFI_palette[1], width = .4) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      expand_limits(y = 0) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        caption = "ACS 2022 (5-Year Estimates)",
        y = "Estimate", x = ""
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  })
  
  
  ## Educational attainment =====
  output$edu_attain_plot <- renderPlot({
    data <- filtered_data()$edu_attain
    state_edu_attain <- data_list$edu_attain |>
      filter(NAME == "North Carolina")
    edu_attain_plot_data <- rbind(data, state_edu_attain)
    edu_attain_plot_data |>
      group_by(NAME, edu_attain) |>
      summarize(total = sum(total, na.rm = TRUE)) |>
      mutate(percent = total / sum(total)) |>
      mutate(edu_attain = fct_rev(fct_relevel(edu_attain, "Less than high school degree", "GED", "High school diploma", "Some college", "Associate's degree", "Bachelor's degree", "Master's degree", "Doctorate degree"))) |>
      ggplot(aes(x = percent, y = NAME, fill = edu_attain)) +
      geom_col(width = .8) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(axis.text.y = element_text(size = 16)) +
      scale_fill_manual(values = c(DFI_palette, "grey80")) +
      scale_x_continuous(labels = scales::percent) +
      labs(fill = "Educational attainment",
           x = "Percent of people over 25",
           y = "",
           caption = "ACS 2022 (5-Year Estimates)")
  })
  
  
  
  ## Median household income by tenure =====
  output$median_hh_income_plot <- renderPlot({
    
    data <- filtered_data()$median_hh_income
    
    state_hh_income <- data_list$median_hh_income |> 
      filter(NAME == "North Carolina") 
    
    median_hh_income_plot_data <- rbind(data, state_hh_income)
    median_hh_income_plot_data |>
      mutate(NAME = fct_rev(fct_relevel(NAME, "North Carolina"))) |>
      ggplot(aes(x = year, y = estimate, group = interaction(tenure, NAME), color = NAME)) +
      geom_point(size = 3) +
      geom_line(linewidth = .8) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(strip.text = element_text(size = 14, hjust = 0),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(linewidth = .5),
            panel.grid.major.y = element_line(linewidth = .5),
            legend.text = element_text(size = 14),
            legend.position = "top") +
      labs(
        color = "Place",
        y = "Estimate", x = "5-Year Estimates", 
        caption = "Source: ACS 2012, 2017, 2022 (5-Year Estimates)"
      ) +
      scale_color_manual(values = DFI_palette) +
      scale_y_continuous(labels = scales::dollar) +
      scale_x_continuous(breaks = c(2012, 2017, 2022)) +
      expand_limits(y = 0) +
      facet_wrap(~tenure, nrow = 1)
  })
  
  ## Household type ===================
  
  output$hh_type_plot <- renderPlot({
    
    data <- filtered_data()$hh_type
    
    data |>
      
      ggplot(aes(x = householder, y = estimate, fill = hh_type)) +
      
      geom_col(width = .45) +
      
      theme_minimal(base_size = 20,
                    base_family = "Helvetica") +
      theme(
        legend.position = "top",
        legend.direction = "vertical",
        legend.title = element_blank()
      ) +
      guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
      
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete() +
      scale_fill_manual(values = DFI_palette) +
      expand_limits(y = 0) +
      labs(
        x = "Householder Type",
        y = "Estimate",
        caption = "Source: ACS 2022 (5-Year Estimates)",
        fill = "Living with:"
      )
    
    
  })
  
  
  ## Change in live and work by income =====
  output$commuting_pattern_change_plot <- renderPlot({
    
    live_and_work <- data_list$county_lodes |>
      filter(w_county_name == input$select_place & h_county_name == input$select_place) |>
      select(year, wage_1250_less, wage_1251_3333, wage_3333_plus) |>
      pivot_longer(cols = -year, names_to = "wage", values_to = "estimate") |>
      mutate(wage = case_when(
        wage == "wage_1250_less" ~ "<$1,250",
        wage == "wage_1251_3333" ~ "$1,250-$3,333",
        wage == "wage_3333_plus" ~ "$3,333+"
      )) |>
      mutate(category = "Live & Work Locally")
    
    live_elsewhere <- data_list$county_lodes |>
      filter(w_county_name == input$select_place & h_county_name != input$select_place) |>
      select(year, wage_1250_less, wage_1251_3333, wage_3333_plus) |>
      group_by(year) |>
      summarize(wage_1250_less = sum(wage_1250_less, na.rm = TRUE), wage_1251_3333 = sum(wage_1251_3333, na.rm = TRUE), wage_3333_plus = sum(wage_3333_plus, na.rm = TRUE)) |>
      pivot_longer(cols = -year, names_to = "wage", values_to = "estimate") |>
      mutate(wage = case_when(
        wage == "wage_1250_less" ~ "<$1,250",
        wage == "wage_1251_3333" ~ "$1,250-$3,333",
        wage == "wage_3333_plus" ~ "$3,333+"
      )) |>
      mutate(category = "Live Elsewhere & Work Locally")
    
    work_elsewhere <- data_list$county_lodes |>
      filter(w_county_name != input$select_place & h_county_name == input$select_place) |>
      select(year, wage_1250_less, wage_1251_3333, wage_3333_plus) |>
      group_by(year) |>
      summarize(wage_1250_less = sum(wage_1250_less, na.rm = TRUE), wage_1251_3333 = sum(wage_1251_3333, na.rm = TRUE), wage_3333_plus = sum(wage_3333_plus, na.rm = TRUE)) |>
      pivot_longer(cols = -year, names_to = "wage", values_to = "estimate") |>
      mutate(wage = case_when(
        wage == "wage_1250_less" ~ "<$1,250",
        wage == "wage_1251_3333" ~ "$1,250-$3,333",
        wage == "wage_3333_plus" ~ "$3,333+"
      )) |>
      mutate(category = "Live Locally & Work Elsewhere")
    
    commuting_pattern_change_plot_data <- rbind(live_and_work, live_elsewhere, work_elsewhere)
    
    commuting_pattern_change_plot_data |>
      mutate(category = fct_relevel(category, "<$1,250", "$1,250-$3,333", "$3,333+")) |>
      ggplot(aes(x = year, y = estimate, color = wage, group = wage)) +
      geom_point(size = 2) +
      geom_line(linewidth = .8) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      labs(
        color = "Monthly wage:",
        y = "Estimate", x = "Year", 
        caption = "Source: Census LODES"
      ) +
      scale_color_manual(values = DFI_palette) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(2009, 2019, 2)) +
      expand_limits(y = 0) +
      facet_wrap(~category, nrow = 1)
  })
  

  
  
  ## Travel time ====  
  output$travel_time_plot <- renderPlot({
    data <- filtered_data()$travel_time
    
    travel_time_state <- data_list$travel_time |> filter(NAME == "North Carolina")
    travel_time_plot_data <- rbind(data, travel_time_state)
    travel_time_plot_data |>
      mutate(NAME = fct_rev(fct_relevel(NAME, "North Carolina"))) |>
      mutate(label = fct_relevel(label, "Less than 5 minutes", "5 to 9 minutes", "10 to 14 minutes", "15 to 19 minutes", "20 to 24 minutes", "25 to 29 minutes", "30 to 34 minutes", "35 to 39 minutes", "40 to 44 minutes", "45 to 59 minutes", "60 to 89 minutes", "90 or more minutes")) |>
      ggplot(aes(x = label, y = estimate, fill = NAME)) +
      geom_col(position = "dodge", show.legend = FALSE) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      scale_fill_manual(values = DFI_palette) +
      facet_wrap(~NAME, nrow = 1, scales = "free_x") +
      scale_y_continuous(labels = scales::comma) +
      coord_flip() +
      labs(x = "Estimate", y = "Commute time",
           caption = "Source: ACS 2022 (5-year Estimates)")
  })
  
  
  ## Housing Stock ===============================================================
  
  ## Vacancy status ====
  
  output$vacancy_status_plot <- renderPlot({
    data <- filtered_data()$vacancy_status
    
    data |>
      ggplot(aes(x = label, y = vacancy_percent, fill = as.factor(year))) +
      geom_col(width = .45, position = position_dodge(width = .5)) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      labs(y = "Estimate", x = "Vacancy Type",
           caption = "Source: ACS 2012, 2017, 2022 (5-year estimates)", fill = "") +
      scale_fill_manual(values = c("#C8DFE7", "#5B9EB8", DFI_palette[1])) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  })
  
  
  ## Units by rent over time =======
  output$rent_unit_plot <- renderPlot({
    
    data <- filtered_data()$rent_unit 
    
    data |>
      mutate(rent_category = fct_relevel(rent_category, "More than $700", "$350 to $700", "Less than $350")) |>
      ggplot(aes(x = as.factor(year), y = estimate, fill = rent_category, group = rent_category)) +
      geom_col(width = .25) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      labs(y = "Estimate", x = "5-Year Estimates",
           caption = "Source: ACS 2017, 2022 (5-year Estimates)", fill = "Contract Rent") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete() +
      scale_fill_manual(values = c(DFI_palette[1], DFI_palette[3], "grey70"))
  })

  
  ## Median value over time ====
  output$med_val_plot <- renderPlot({
    
    data <- filtered_data()$med_val
    
    state_med_val_plot_data <- data_list$med_val |>
      filter(NAME == "North Carolina")
    med_val_plot_data <- rbind(data, state_med_val_plot_data)
    med_val_plot_data |>
      mutate(NAME = fct_rev(fct_relevel(NAME, "North Carolina"))) |>
      ggplot(aes(x = year, y = estimate, color = NAME, group = NAME)) +
      geom_point(size = 3) +
      geom_line(linewidth = .8) +
      geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.3, alpha = .8) +
      geom_text(aes(label = scales::dollar(estimate)), vjust = -0.7, size = 5) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      scale_y_continuous(labels = scales::dollar, expand = expansion(mult = 0.1)) +
      scale_x_continuous(breaks = c(2012, 2017, 2022), expand = expansion(mult = 0.1)) +
      expand_limits(y = 0) +
      scale_color_manual(values = DFI_palette) +
      labs(
        x = "Year",
        y = "Median Home Value",
        caption = "Source: ACS 2012, 2017, 2022 (5-Year Estimates)",
        color = ""
      )
  })
  
  
  ## Tenure ======
  output$tenure_plot <- renderPlot({
    
    data <- filtered_data()$tenure
    
    data |>
      ggplot(aes(x = as.factor(year), y = estimate, fill = tenure)) +
      geom_col(width = .45) +
      geom_text(aes(label = scales::comma(estimate), group = tenure),
                size = 5, color = "white",
                position = position_stack(vjust = .5),
                fontface = "bold") +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete() +
      scale_fill_manual(values = DFI_palette) +
      expand_limits(y = 0) +
      labs(
        x = "5-Year Estimates",
        y = "Estimate",
        caption = "Source: ACS 2012, 2017, 2022 (5-Year Estimates)",
        fill = ""
      )
  })
  
  
  ## Tenure by race over time ====
  output$tenure_race_plot <- renderPlot({
    
    data <- filtered_data()$tenure_race
    
    data |>
      ggplot(aes(x = year, y = estimate, group = tenure, color = tenure)) +
      geom_point(size = 3) +
      geom_line(linewidth = .8) +
      geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.5, alpha = .8) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(strip.text = element_text(size = 14, hjust = 0), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(linewidth = .5), panel.grid.major.y = element_line(linewidth = .5), legend.text = element_text(size = 14)) +
      theme(legend.position = "top") +
      labs(
        color = "Tenure",
        y = "Estimate", x = "5-Year Estimates", 
        caption = "Source: ACS 2012, 2017, 2022 (5-Year Estimates)"
      ) +
      scale_color_manual(values = DFI_palette) +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = 0.1)) +
      scale_x_continuous(breaks = c(2012, 2017, 2022), expand = expansion(mult = 0.1)) +
      expand_limits(y = 0) +
      facet_wrap(~race_eth, ncol = 3, labeller = labeller(race_eth = label_wrap_gen(35)), scales = "free_y")
  })
  
  
  ## Tenure by units by year built ======
  output$tenure_units_year_plot <- renderPlot({
    
    data <- filtered_data()$tenure_units_year
    
    data |>
      mutate(unit_type_grouped = fct_relevel(unit_type_grouped, "Single family", "2 to 49 units", "50 or more units", "Mobile home or other")) |>
      mutate(year_built_grouped = fct_rev(fct_relevel(year_built_grouped, "Before 1980", "1980 to 1999", "2000 or later"))) |>
      ggplot(aes(x = unit_type_grouped, y = estimate, fill = as.factor(year_built_grouped))) +
      geom_col(width = .45) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      labs(y = "Estimate", x = "Housing type",
           caption = "Source: ACS 2022 (5-year estimates)", fill = "") +
      scale_fill_manual(values = c(DFI_palette[1], "#5B9EB8", "grey80")) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_wrap(~tenure, nrow = 1, scales = "free_y")
  })
  
  
  ## Tenure by bedrooms ========
  output$tenure_bedrooms_plot <- renderPlot({
    
    data <- filtered_data()$tenure_bedrooms
    
    data |>
      mutate(bedroom_count = fct_relevel(bedroom_count, "No bedroom")) |>
      ggplot(aes(x = bedroom_count, y = estimate, fill = tenure)) +
      geom_col(position = 'dodge', width = .4) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(legend.position = "top") +
      labs(
        x = "Number of bedrooms in unit", y = "Estimate", fill = "Tenure",
        caption = "ACS 2022 (5-Year Estimates)"
      ) +
      scale_fill_manual(values = DFI_palette)
  })
  
  
  ## Economics ===================================================================
  # Unemployment =====
  output$unemployment_rate_plot <- renderPlot({
    
    data <- filtered_data()$unemployment_rate
    
    endpoints <- data |>
      filter(month_year == "May 2024")
    data |>
      ggplot(aes(x = month_year, y = `Unemployment Rate(%)`)) +
      geom_line(linewidth = .8, color = DFI_palette[1]) +
      geom_point(data = endpoints, aes(x = month_year, y = `Unemployment Rate(%)`), size = 4, color = DFI_palette[1]) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      labs(
        x = "Year", y = "Unemployment Rate", 
        caption = "Local Area Unemployment Statistics (LAUS) from NC Commerce"
      ) +
      expand_limits(y = 0)
  })
  
  
  ## Top industries =====
  output$industry_employment_plot <- renderPlot({
      
      data <- filtered_data()$industry_employment
      
      data <- data |>
      select(Year, `NAICS Code`, Industry, `Average Employment`, Establishments, `Average Weekly Wage`) |>
      rename("Employees" = `Average Employment`)
    endpoints <- data |> 
      filter(Year == 2023)
    data |>
      ggplot(aes(x = Year, y = Employees, color = Industry, group = Industry)) +
      geom_point(data = endpoints, aes(x = Year, y = Employees, group = Industry), size = 3, color = DFI_palette[1], show.legend = FALSE) +
      geom_line(linewidth = .8, color = DFI_palette[1], show.legend = FALSE) +
      facet_wrap(~Industry, ncol = 3, labeller = labeller(Industry = label_wrap_gen(35))) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_family = "Helvetica",base_size = 20) +
      theme(strip.text = element_text(size = 14, hjust = 0), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(linewidth = .5), panel.grid.major.y = element_line(linewidth = .5)) +
      labs(
        x = "Year", y = "Unemployment Rate", 
        caption = "Local Area Unemployment Statistics (LAUS) from NC Commerce"
      ) +
      expand_limits(y = 0)
  })
  
  
  ## Largest employers ====
  output$top_employers_tbl <- renderReactable({
      data <- filtered_data()$largest_employ
      
      data <- data |>
      select(Rank, `Company Name`, Industry, Class, `Employment Range`)
    reactable(data,
              columns = list(
                Rank = colDef(width = 100, align = "left")),
              defaultColDef = colDef(format = colFormat(separators = TRUE)))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
