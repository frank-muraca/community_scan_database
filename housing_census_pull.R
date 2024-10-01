library(tidycensus)
library(tidyverse)
library(here)

vars <- load_variables(year = 2012, dataset = "acs5")




concept_list <- c("VACANCY STATUS",
                  "HOUSING UNITS",
                  "TENURE BY UNITS IN STRUCTURE",
                  "TENURE BY YEAR STRUCTURE BUILT",
                  "CONTRACT RENT",
                  "TENURE",
                  "TENURE BY BEDROOMS"
                  )



# Total Housing Units
# Tenure
# Vacant
# Median home value + tenure?
# Year structure built
# Year structure built + tenure
# Median year structure built + tenure
# Year householder moved into unit + tenure
# Household size + tenure + all
# Bedrooms + tenure + all
# Housing units in structure + tenure
# Monthly owner costs
# Median gross rent
# Contract rent
# Cost burden

    ## Tenure over time    



    ## Collect data ==================

        all_data_time <- NULL
        
        for(i in c(2012, 2017, 2022)){
          
          for(g in c("state","county", "place")){
            
            tmp <- get_acs(state = "NC",
                           geography = g,
                           variables = vars|> filter(concept %in% concept_list)|> pull(name),
                           year = i,
                           survey = "acs5")|>
              left_join(vars|> filter(concept %in% concept_list), by = c("variable" = "name"))  
            
            tmp$year <- i
            
            all_data_time <- rbind(all_data_time, tmp)
          }
        }



    ## Tenure =======

    tenure <-  

     all_data_time |>
  
        filter(concept == "TENURE") |>
  
        mutate(NAME = str_remove(NAME,", North Carolina")) |>
        
        mutate(tenure = case_when(
          grepl("Owner", label) ~ "Owner",
          grepl("Renter", label) ~ "Renter"
        )) |>
  
        filter(!is.na(tenure))

      write_csv(tenure, here::here('data', 'housing', 'census','tenure.csv'))
  
        
  
        




                 ## Contract rent over time =====================
        
  
        
        
      
      rent_data_clean <- 
        all_data_time %>%
        filter(concept == "CONTRACT RENT") %>%
        filter(!label %in% c("Estimate!!Total", "Estimate!!No cash rent", "Estimate!!Total!!No cash rent", "Estimate!!Total!!With cash rent")) %>%
        mutate(label_clean = str_remove(label, "Estimate!!Total!!With cash rent!!")) %>%
        select(-label) %>%
        mutate(
          rent_category = case_when(
            label_clean %in% c("Less than $100", "$100 to $149", "$150 to $199", "$200 to $249", "$250 to $299", "$300 to $349") ~ "Less than $350",
            label_clean %in% c("$350 to $399", "$400 to $449", "$450 to $499", "$500 to $549", "$550 to $599", "$600 to $649", "$650 to $699") ~ "$350 to $700",
            TRUE ~ "More than $700"
          )
        ) %>%
        mutate(rent_category = fct_relevel(rent_category, "More than $700", "$350 to $700", "Less than $350")) %>%
        mutate(NAME = str_remove(NAME, ", North Carolina"))
      
        
        write_csv(rent_data_clean, here('data', 'housing', 'census', "rent_data.csv"))
        
        ## Tenure by units in structureover time ========================================
        
        tenure_units <- 
          all_data_time %>%
          mutate(NAME = str_remove(NAME, ", North Carolina")) %>%
          filter(concept == "TENURE BY UNITS IN STRUCTURE") %>%
          filter(!label %in% c("Estimate!!Total", "Estimate!!Total!!Owner-occupied housing units", "Estimate!!Total!!Renter-occupied housing units")) %>%
          mutate(unit_type = case_when(
            grepl("1, detached", label) ~ "1, detached",
            grepl("1, attached", label) ~ "1, attached",
            grepl("3 or 4", label) ~ "3 or 4 units",
            grepl("5 to 9", label) ~ "5 to 9 units",
            grepl("10 to 19", label) ~ "10 to 19 units",
            grepl("20 to 49", label) ~ "20 to 49 units",
            grepl("50 or more", label) ~ "50 or more units",
            grepl("Mobile home", label) ~ "Mobile home",
            grepl("Boat, RV, van, etc.", label) ~ "Boat, RV, van, etc.",
            TRUE ~ "Duplex"
          )) %>%
          mutate(tenure = case_when(
            grepl("Owner", label) ~ "Owner",
            grepl("Renter", label) ~ "Renter"
          )) %>%
          mutate(unit_type = fct_relevel(unit_type, "1, detached", "1, attached", "Duplex", "3 or 4 units", "5 to 9 units", "10 to 19 units", "20 to 49 units", "50 or more units", "Mobile home", "Boat, RV, van, etc.")) %>%
          filter(year == 2022) %>%
          select(NAME, year, estimate, unit_type, tenure)
       
         write_csv(tenure_units, here('data', 'housing', 'census', "tenure_units.csv"))
        
        ## Year structure built tenure ================
        
        #tenure_year_built <-
        
        all_data_time |>
          filter(year == "2022") |>
          mutate(NAME = str_remove(NAME,", North Carolina")) |>
          filter(concept == "TENURE BY YEAR STRUCTURE BUILT") |>
          filter(!label %in% c("Estimate!!Total:", "Estimate!!Total:!!Owner occupied:", "Estimate!!Total:!!Renter occupied:")) |>
          mutate(tenure = case_when(
            grepl("Owner", label) ~ "Owner",
            grepl("Renter", label) ~ "Renter"
          )) |>
          mutate(label = str_remove_all(label, "Estimate!!Total:!!Owner occupied:!!Built ")) |>
          mutate(label = str_remove_all(label, "Estimate!!Total:!!Renter occupied:!!Built ")) 
        
        write_csv(tenure_year_built, here('data', 'housing', 'census', 'tenure_year_built.csv'))
          
          ## "TENURE BY AGE OF HOUSEHOLDER BY UNITS IN STRUCTURE"
        
        tenure_age_units <- 
        all_data_time |>
          filter(year == "2022") |>
          mutate(NAME = str_remove(NAME, ", North Carolina")) |>
          filter(concept == "TENURE BY AGE OF HOUSEHOLDER BY UNITS IN STRUCTURE") |>
          mutate(tenure = case_when(
            grepl("Owner", label) ~ "Owner",
            grepl("Renter", label) ~ "Renter"
          )) |>
          mutate(unit_type = case_when(
            grepl("1, detached  or attached", label) ~ "Single family",
            grepl("2 to 4", label) ~ "2 to 4 units",
            grepl("5 to 19", label) ~ "5 to 19 units",
            grepl("20 to 49", label) ~ "20 to 49 units",
            grepl("50 or more", label) ~ "50+ units",
            grepl("Mobile home", label) ~ "Mobile home and other"
          )) |>
          filter(!is.na(unit_type)) |>
          mutate(age_bracket = case_when(
            grepl("15 to 34 years", label) ~ "15-34 years",
            grepl("35 to 64 years", label) ~ "35-64 years",
            grepl("65 years and over", label) ~ "65+ years"
          )) |>
          select(-label)
        
        
        write_csv(tenure_age_units, here('data', 'housing', 'census', 'tenure_age_units.csv'))
        
        
        ## Tenure units in structure
        
        tenure_units_year <- NULL
        
          for(g in c("county", "place")){
            
            tmp <- get_acs(state = "NC",
                           geography = g,
                           variables = vars|> filter(concept == "Tenure by Year Structure Built by Units in Structure")|> pull(name),
                           year = 2022,
                           survey = "acs5")|>
              left_join(vars|> filter(concept == "Tenure by Year Structure Built by Units in Structure"), by = c("variable" = "name"))|>
              mutate(year = 2022)|>
              mutate(county = g)
            tmp$state <- "North Carolina"
            tenure_units_year <- rbind(tenure_units_year,tmp)
            
            
          }
        
        
        tenure_unit_year_clean <- 
          tenure_units_year |>
          mutate(NAME = gsub("(.*),.*", "\\1", NAME)) |>
          filter(!label %in% c("Estimate!!Total:", "Estimate!!Total:!!Owner occupied:", "Estimate!!Total:!!Renter occupied:")) |>
          mutate(unit_type = case_when(
            grepl("detached or attached",label) ~ "Single family",
            grepl("2 to 4", label) ~ "2 to 4 units",
            grepl("5 to 19", label) ~ "5 to 19 units",
            grepl("20 to 49", label) ~ "20 to 49 units",
            grepl("50 or more", label) ~"50 or more units",
            grepl("Mobile home", label) ~ "Mobile home or other",
          )) |>
          filter(!is.na(unit_type)) |>
          mutate(tenure = case_when(
            grepl("Owner", label) ~ "Owner",
            grepl("Renter", label) ~ "Renter"
          )) |>
          mutate(year_built = case_when(
            grepl("Built 2020 or later", label) ~ "2020 or later",
            grepl("Built 2000 to 2019", label) ~ "2000 to 2019",
            grepl("Built 1980 to 1999", label) ~ "1980 to 1999",
            grepl("Built 1960 to 1979", label) ~ "1960 to 1979",
            grepl("Built 1940 to 1959", label) ~ "1940 to 1959",
            grepl("Built 1939 or earlier", label) ~ "1939 or earlier",
            
          )) |>
          
          mutate(unit_type_grouped = case_when(
            unit_type %in% c("2 to 4 units", "5 to 19 units", "20 to 49 units") ~ "2 to 49 units",
            TRUE ~ unit_type
          )) |>
          
          mutate(year_built_grouped = case_when(
            year_built %in% c("2000 to 2019", "2020 or later") ~ "2000 or later",
            year_built ==  "1980 to 1999" ~ "1980 to 1999",
            TRUE ~ "Before 1980"
          )) |>
          
          select(-label)
        
        write_csv(tenure_unit_year_clean, here('data', 'housing','census','tenure_units_year.csv'))
        
        ## Vacancy status ==============================
        
        
        housing_units <- 
          
          all_data_time |> 
          
          filter(concept == "HOUSING UNITS") |>
          
          mutate(housing_units = estimate) |>
          
          mutate(NAME = str_remove(NAME,", North Carolina"))
        
        
        vacancy_status <- 
          all_data_time %>%
          mutate(NAME = str_remove(NAME, ", North Carolina")) %>%
          filter(concept == "VACANCY STATUS") %>%
          filter(label != "Estimate!!Total") %>%
          mutate(label = str_remove_all(label, "Estimate!!Total!!")) %>%
          left_join(housing_units %>% select(NAME, housing_units, year), by = c("NAME" = "NAME", "year" = "year")) %>%
          mutate(vacancy_percent = estimate / housing_units) %>%
          mutate(label = fct_relevel(label, "For rent", "Rented, not occupied", "For sale only", "Sold, not occupied", "For seasonal, recreational, or occasional use", "Other vacant", "For migrant workers"))
        
        
        
        write_csv(vacancy_status, here('data', 'housing', 'census', 'vacancy_status.csv'))
        
        
    ## Bedroom count by tenure
        
       tenure_bedrooms <- all_data_time |>
          filter(year == 2022) |>
          filter(concept == "TENURE BY BEDROOMS") |>
          mutate(NAME = str_remove(NAME,", North Carolina")) |>
          filter(!label %in% c("Estimate!!Total", "Estimate!!Total!!Renter occupied", "Estimate!!Total!!Owner occupied")) |>
          mutate(tenure = case_when(
            grepl("Owner", label) ~ "Owner",
            grepl("Renter", label) ~ "Renter"
          )) |>
          mutate(label = str_remove_all(label, "Estimate!!Total!!Owner occupied!!")) |>
          mutate(label = str_remove_all(label, "Estimate!!Total!!Renter occupied!!")) |>
         rename('bedroom_count' = label)
       
       write_csv(tenure_bedrooms, here('data', 'housing', 'census', 'tenure_bedrooms.csv'))
        
       
       ## Median value ====
       
       
       
       med_val <- NULL
       
       for(i in c(2012, 2017, 2022)){
         
         for(g in c("state","county", "place")){
           
           tmp <- get_acs(state = "NC",
                          geography = g,
                          variables = vars|> filter(concept == "MEDIAN VALUE (DOLLARS)")|> pull(name),
                          year = i,
                          survey = "acs5")|>
             left_join(vars|> filter(concept == "MEDIAN VALUE (DOLLARS)"), by = c("variable" = "name"))  
           
           tmp$year <- i
           
           med_val <- rbind(med_val, tmp)
         }
       }
       
       med_val <- med_val |> 
         mutate(NAME = str_remove(NAME,", North Carolina")) 
       
       write_csv(med_val, here::here('data', 'housing', 'census', 'med_val.csv'))
       