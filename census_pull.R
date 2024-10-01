        
        update.packages("tidycensus")

        library(tidycensus)
        library(tidyverse)
        library(here)
        library(sf)


        # County to county migration flows =============

        county2county <- get_flows(geography = "county",
                                   year = 2020, output = "wide",
                                   state = "NC")
        
        county2county <- county2county |>
          mutate(FULL1_NAME = str_remove_all(FULL1_NAME, ", North Carolina")) |>
          mutate(FULL2_NAME = str_remove_all(FULL2_NAME, ", North Carolina"))
        
        write_csv(county2county, here('data', 'demographic', 'census', 'county2county_flows.csv'))



        
        ## Lodes pull ==================================
        
        library(lehdr)
        library(tigris)
        
        counties <- tigris::counties(state = "NC")
        places <- tigris::places(state = "NC")
        
        counties_fip <- counties |> st_drop_geometry() |> select(NAMELSAD, GEOID)
        
        county_lodes <- NULL
        
        for(i in seq(2009, 2021, 1)){
          
        tmp <- grab_lodes(state = "NC",
                                   year = i,
                                   lodes_type = "od",
                                   job_type = "JT00",
                                   agg_geo = "county")
        tmp$year <- i
        
        county_lodes <- rbind(county_lodes, tmp)
        
        }
        
        county_lodes <- county_lodes |>
          
          left_join(counties_fip, by = c("w_county" = "GEOID")) |>
          
          rename('w_county_name' = NAMELSAD) |>
          
          left_join(counties_fip, by = c('h_county' = 'GEOID')) |>
          
          rename('h_county_name' = NAMELSAD) |>
          
          rename('total_jobs' = S000) |>
          rename('age_29_younger' = SA01) |>
          rename('age_30_54' = SA02) |>
          rename('age_55_plus' = SA03) |>
          rename('wage_1250_less' = SE01) |>
          rename('wage_1251_3333' = SE02) |>
          rename('wage_3333_plus' = SE03) |>
          rename('ind_good_producing' = SI01) |>
          rename('ind_trade_transp' = SI02) |>
          rename('ind_all_other' = SI03)
        
        write_csv(county_lodes, here('data', 'demographic', 'census', 'county_lodes.csv'))
        
        bg_lodes <- grab_lodes(state = "NC",
                               year = 2021,
                               lodes_type = "od",
                               job_type = "JT00",
                               agg_geo = "bg")
        
        
       
        
        places_bg_merge <- st_join(bg_sf, places_sf, largest = TRUE)
        
        ## Components of population change ===============
        
        census_pop_estimates <- read_csv(here('data', 'demographic', 'census', 'component_pop_change.csv'))
        
        natural_inc <- 
          
          census_pop_estimates |>
          
          rename("NAME" = "CTYNAME...7") |>
          
          select(NAME, starts_with("NATURALINC") | starts_with("NATURALCHG")) |>
          
          pivot_longer(cols = starts_with("NATURALINC") | starts_with("NATURALCHG")) |>
          
          mutate(year = as.double(substr(name, 11,14))) |>
          
          mutate(name = "Natural Change") 
        
        # Create net migration dataset
        
        net_mig <- 
          
          census_pop_estimates |>
          
          rename("NAME" = "CTYNAME...7") |>
          
          select(NAME, starts_with("NETMIG")) |>
          
          pivot_longer(cols = starts_with("NETMIG")) |>
          
          mutate(year = as.double(substr(name, 7, 10))) |>
          
          mutate(name = "Net Migration")
        
        # Combine data frames
        
        pop_change_merge <- rbind(natural_inc, net_mig)
        
        write_csv(pop_change_merge, here('data', 'demographic', 'census', 'component_pop_change.csv'))
        
        
        ### Population =============================
        
        total_population <- NULL
        
        for(i in c(2012, 2017, 2022)){
        for(g in c("state","county", "place")){
          
          # Pull data
          tmp <- get_acs(state = "NC",
                         geography = g,
                         variables = "B01003_001",
                         year = i,
                         survey = "acs5") |>
            # join labels
            
            mutate(year = i)
          
          # Label geography in the data
          tmp$geo <- g
          
          # Combine with other geographies
          total_population <- rbind(total_population, tmp)
        }
        
        }
        
        total_population <- total_population |> mutate(NAME = str_remove(NAME, ", North Carolina"))
        
        
        
        
        
        
        total_population <- all_data |> filter(concept == "TOTAL POPULATION")
        
        write_csv(total_population, here('data','demographic','census','total_population.csv'))
        
        # Use to look up variables
        vars <- load_variables(year = 2022, dataset = "acs5")
        
        
        # Selected variable list        
        
        concept_list <- c("Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars)",
                          "Sex by Age",
                          "Hispanic or Latino Origin by Race",
                          "Total Population",
                          "Tenure",
                          "Educational Attainment for the Population 25 Years and Over",
                          "Tenure by Age of Householder",
                          "Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) by Tenure",
                          "Travel Time to Work",
                          "Employment Status for the Population 16 Years and Over",
                          "Households by Type")
        
        # Run for all geographies
        all_data <- NULL
        for(g in c("state","county", "place")){
          
          # Pull data
          tmp <- get_acs(state = "NC",
                         geography = g,
                         variables = vars |> filter(concept %in% concept_list)|> pull(name),
                         year = 2022,
                         survey = "acs5") |>
            # join labels
            left_join(vars|> filter(concept %in% concept_list), by = c("variable" = "name"))  
          
          # Label geography in the data
          tmp$geo <- g
          
          # Combine with other geographies
          all_data <- rbind(all_data,tmp)
        }
        
        
        all_data <- all_data |> mutate(NAME = str_remove(NAME, ", North Carolina"))
        
        
        
        
        ### Sex by age =============================
        
        sex_by_age <- 
          
          all_data |> filter(concept == "Sex by Age") |>
          
          filter(!label %in% c("Estimate!!Total:", "Estimate!!Total:!!Male:", "Estimate!!Total:!!Female:")) |>
          
          mutate(binary_sex = case_when(
            
            grepl("Male",label) ~ "Male",
            grepl("Female", label) ~ "Female"
          
          )) |>
          
          mutate(label = str_remove_all(label, "Estimate!!Total:!!Female:!!")) |>
          mutate(label = str_remove_all(label, "Estimate!!Total:!!Male:!!")) |>
          
          mutate(age_bracket_raw = case_when(
            
            label %in% c("15 to 17 years", "18 and 19 years") ~ "15 to 19 years",
            label %in% c("20 years", "21 years", "22 to 24 years") ~ "20 to 24 years",
            label %in% c("60 and 61 years", "62 to 64 years") ~ "60 to 64 years",
            label %in% c("65 and 66 years", "67 to 69 years") ~ "65 to 69 years",
            TRUE ~ label
          )) |>
          
          group_by(NAME, binary_sex, age_bracket_raw) |>
          
          summarize(total = sum(estimate, na.rm = T))
        
        write_csv(sex_by_age, here('data', 'demographic', 'census', 'sex_by_age.csv'))
        
        
        ### Sex by age time =======================================
        
        sex_by_age_time <- NULL
        
        for(i in c(2012, 2017, 2022)){
          for(g in c("state","county", "place")){
            
            # Pull data
            tmp <- get_acs(state = "NC",
                           geography = g,
                           variables = vars |> filter(concept == "Sex by Age") |> pull(name),
                           year = i,
                           survey = "acs5") |>
              # join labels
              
              left_join(vars |> filter(concept == "Sex by Age"), by = c("variable" = "name")) |>
              
              mutate(year = i)
            
            # Label geography in the data
            tmp$geo <- g
            
            # Combine with other geographies
            sex_by_age_time <- rbind(sex_by_age_time, tmp)
          }
          
        }
        
  
        sex_by_age_time <- 
          
          sex_by_age_time |> 
          
          filter(concept == "Sex by Age") |>
          
          filter(!label %in% c("Estimate!!Total:", "Estimate!!Total:!!Male:", "Estimate!!Total:!!Female:")) |>
          
          mutate(binary_sex = case_when(
            
            grepl("Male",label) ~ "Male",
            grepl("Female", label) ~ "Female"
            
          )) |>
          
          mutate(label = str_remove_all(label, "Estimate!!Total:!!Female:!!")) |>
          mutate(label = str_remove_all(label, "Estimate!!Total:!!Male:!!")) |>
          
          mutate(age_bracket_raw = case_when(
            
            label %in% c("15 to 17 years", "18 and 19 years") ~ "15 to 19 years",
            label %in% c("20 years", "21 years", "22 to 24 years") ~ "20 to 24 years",
            label %in% c("60 and 61 years", "62 to 64 years") ~ "60 to 64 years",
            label %in% c("65 and 66 years", "67 to 69 years") ~ "65 to 69 years",
            TRUE ~ label
          )) |>
          
               
          group_by(NAME, year, binary_sex, age_bracket_raw) |>
          
          summarize(total = sum(estimate, na.rm = T))
        
        age_over_time <- sex_by_age_time |>
          
          mutate(age_bracket_compress = case_when(
            age_bracket_raw %in% c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years") ~ "19 years or younger",
            age_bracket_raw %in% c("20 to 24 years", "25 to 29 years") ~ "20-29 years",
            age_bracket_raw %in% c("30 to 34 years", "35 to 39 years") ~ "30-39 years",
            age_bracket_raw %in% c("40 to 44 years", "45 to 49 years") ~ "40-49 years",
            age_bracket_raw %in% c("50 to 54 years", "55 to 59 years") ~ "50-59 years",
            age_bracket_raw %in% c("60 to 64 years", "65 to 69 years")  ~ "60-69 years",
            TRUE ~ "70+ years"
          )) |>
        
        group_by(NAME, year, age_bracket_compress) |>
          
          summarize(total = sum(total, na.rm = T)) |>
          
          mutate(NAME = str_remove(NAME, ", North Carolina"))
        
        write_csv(age_over_time, here('data', 'demographic', 'census', 'age_over_time.csv'))
        
        
        
        ## Household type 
        
        hh_type <- 
        all_data |> 
          
          filter(concept == "Households by Type") |>
          
          filter(!label %in% c("Estimate!!Total:", "Estimate!!Total:!!Married-couple household:",
                               "Estimate!!Total:!!Cohabiting couple household:",
                               "Estimate!!Total:!!Female householder, no spouse or partner present:",
                               "Estimate!!Total:!!Male householder, no spouse or partner present:")) |>
          
          mutate(householder = case_when(
            grepl("Married-couple", label) ~ "Married Couple",
            grepl("Cohabiting couple", label) ~ "Cohabiting couple",
            grepl("Female householder", label) ~ "Female householder",
            grepl("Male householder", label) ~ "Male householder"
          )) |>
          
          mutate(hh_type = case_when(
            grepl("With children", label) ~ "Living with children <18",
            grepl("With no children", label) ~ "Not living with children",
            grepl("Living alone", label) ~ "Living alone",
            grepl("With relatives", label) ~ "Living with relatives",
            grepl("With only nonrelatives", label) ~ "Roommates"
          )) 
       
        write_csv(hh_type, here('data', 'demographic', 'census', 'hh_type.csv'))        
        
        
        ## Educational attainment =======================
        
        edu_attain <- 
        all_data |> 
          
          filter(concept == "Educational Attainment for the Population 25 Years and Over") |>
          
          filter(label != "Estimate!!Total:") |>
          
          mutate(label = str_remove_all(label, "Estimate!!Total:!!")) |>
          
          mutate(edu_attain = case_when(
            
            grepl("Some college", label) ~ "Some college",
            label == "Associate's degree" ~ "Associate's degree",
            label == "Bachelor's degree" ~ "Bachelor's degree",
            label == "Master's degree" ~ "Master's degree",
            label == "Professional school degree" ~ "Some college",
            label == "Doctorate degree" ~ "Doctorate degree",
            label == "Regular high school diploma" ~ "High school diploma",
            label == "GED or alternative credential" ~ "GED",
            TRUE ~ "Less than high school degree"
            
          )) |>
          
          group_by(NAME, edu_attain) |>
          
          summarize(total = sum(estimate, na.rm = T))

        write_csv(edu_attain, here('data', 'demographic', 'census', 'edu_attain.csv'))        
        
        ## Employment status ============================
        
        emp_status <- 
        
        all_data |>
          
          filter(concept == "Employment Status for the Population 16 Years and Over") |>
          
          filter(label != "Estimate!!Total:") |>
          
          mutate(label = str_remove_all(label, "Estimate!!Total:!!")) |>
          
          filter(label != "In labor force:") |>
          
          filter(label != "In labor force:!!Civilian labor force:") |>
          
          mutate(emp_status = case_when(
            
            grepl("Employed", label) ~ "Employed",
            grepl("Unemployed", label) ~ "Unemployed",
            grepl("Armed Forces", label) ~ "Armed Forces",
            grepl("Not in labor force", label) ~ "Not in labor force"
          )) |>
          
          group_by(NAME, emp_status) |>
          
          summarize(total = sum(estimate, na.rm = T)) 
        
        write_csv(emp_status, here('data', 'demographic', 'census', 'emp_status.csv'))
        
        
        ## Ethnic/Race ==============================
        
        remove <- c("Not Hispanic or Latino:!!", "Hispanic or Latino:!!")
        
        race_eth <- 
        
        all_data |>
          
          filter(concept == "Hispanic or Latino Origin by Race") |>
          
          filter(label != "Estimate!!Total:") |>
          filter(label != "Estimate!!Total:!!Not Hispanic or Latino:") |>
          filter(label != "Estimate!!Total:!!Hispanic or Latino:") |>
          
          mutate(label = str_remove_all(label, "Estimate!!Total:!!")) |>
          mutate(ethnicity = case_when(
            
            grepl("Not", label) ~ "Not Hispanic or Latino",
            TRUE ~ "Hispanic or Latino"
          )) |>
            mutate(label = str_remove_all(label, paste(remove, collapse = "|"))) |>
          filter(!label %in% c("Two or more races:!!Two races including Some other race", "Two or more races:!!Two races excluding Some other race, and three or more races")) |>
          
          mutate(race_eth = case_when(
            
            label == "White alone" & ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
            TRUE ~ label
          )) |>
          
          group_by(NAME, race_eth) |>
          
          summarize(total = sum(estimate, na.rm = T))

        write_csv(race_eth, here('data', 'demographic', 'census', 'race_eth.csv'))          
        
        ## Travel time ====================================
        
        travel_time <- all_data |>
          
           filter(concept == "Travel Time to Work") |>
           filter(label != "Estimate!!Total:") |>
          
          mutate(label = str_remove_all(label, "Estimate!!Total:!!")) 
          
          write_csv(travel_time, here('data', 'demographic', 'census', 'travel_time.csv')) 
        
          ## Median household income over time ===========================
          
          median_hh_income <- NULL
          for(g in c("state","county", "place")){
          for(i in c(2012, 2017, 2022)){
            tmp <- get_acs(
              geography = g,
              variables = vars |> filter(concept == "Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) by Tenure") |> pull(name),
              state = "NC",
              year = i,
              survey = "acs5"
            ) |>
              left_join(vars |> filter(concept == "Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) by Tenure"), by = c("variable" = "name"))
            
            tmp$geography <- g
            tmp$year <- i
            
            median_hh_income <- rbind(median_hh_income, tmp)
          }
          }
          
          
          median_hh_income <- 
          median_hh_income |>
            mutate(tenure = case_when(
              variable == "B25119_001" ~ "All",
              variable == "B25119_002" ~ "Owner",
              variable == "B25119_003" ~ "Renter"
            )) |>
            select(NAME, geography, year, tenure, estimate) |>
            mutate(NAME = str_remove(NAME, ", North Carolina"))
          
          write_csv(median_hh_income, here('data','demographic','census','median_hh_income.csv'))

          ## Tenure by race over time ===========================
          
          tenure_race <- NULL
          for(g in c("state","county", "place")){
            for(i in c(2012, 2017, 2022)){
              tmp <- get_acs(
                geography = g,
                variables = vars |> filter(concept %in% c("Tenure (White Alone, Not Hispanic or Latino Householder)",
                                                          "Tenure (Hispanic or Latino Householder)",
                                                          "Tenure (Black or African American Alone Householder)",
                                                          "Tenure (American Indian and Alaska Native Alone Householder)",
                                                          "Tenure (Asian Alone Householder)",
                                                          "Tenure (Native Hawaiian and Other Pacific Islander Alone Householder)",
                                                          "Tenure (Some Other Race Alone Householder)",
                                                          "Tenure (Two or More Races Householder)")) |> pull(name),
                state = "NC",
                year = i,
                survey = "acs5"
              ) |>
                left_join(vars |> filter(concept %in% c("Tenure (White Alone, Not Hispanic or Latino Householder)",
                                                        "Tenure (Hispanic or Latino Householder)",
                                                        "Tenure (Black or African American Alone Householder)",
                                                        "Tenure (American Indian and Alaska Native Alone Householder)",
                                                        "Tenure (Asian Alone Householder)",
                                                        "Tenure (Native Hawaiian and Other Pacific Islander Alone Householder)",
                                                        "Tenure (Some Other Race Alone Householder)",
                                                        "Tenure (Two or More Races Householder)")), by = c("variable" = "name"))
              
              tmp$geography <- g
              tmp$year <- i
              
              tenure_race <- rbind(tenure_race, tmp)
            }
          }
          
          tenure_race <- 
          tenure_race |>
            
            mutate(NAME = str_remove(NAME, ", North Carolina")) |>
            
            mutate(race_eth = case_when(
              grepl("White Alone, Not Hispanic or Latino Householder", concept) ~ "White alone, not hispanic or latino",
              grepl("Hispanic or Latino Householder", concept) ~ "Hispanic or Latino",
              grepl("Black or African American Alone Householder", concept) ~ "Black or African American",
              grepl("American Indian and Alaska Native Alone Householder", concept) ~ "American Indian or Alaska Native",
              grepl("Asian Alone Householder", concept) ~ "Asian",
              grepl("Native Hawaiian and Other Pacific Islander Alone Householder", concept) ~ "Native Hawaiian and other Pacific Islander",
              grepl("Some Other Race Alone Householder", concept) ~ "Some other race",
              grepl("Two or More Races Householder", concept) ~ "Two or more races"
            )) |>
          
            
            mutate(tenure = case_when(
              
              grepl("Owner", label) ~ "Owner",
              grepl("Renter", label) ~ "Renter",
              TRUE ~ "All"
            )) |>
            
            filter(tenure != "All") |>
            
            select(year, NAME, race_eth, tenure, estimate, moe)
            
          
          write_csv(tenure_race, here('data','demographic','census','tenure_race.csv'))
          
          
          ## Tenure by age of householder ========================================
         
          tenure_by_age <- all_data |> filter(concept == "Tenure by Age of Householder")
          
          tenure_by_age <- tenure_by_age |> 
            filter(!label %in% c("Estimate!!Total:", "Estimate!!Total:!!Owner occupied:", "Estimate!!Total:!!Renter occupied:")) |>
            mutate(tenure = case_when(
              grepl("Owner", label) ~ "Owner",
              grepl("Renter", label) ~ "Renter"
            )) |>
            mutate(label = str_remove_all(label, "Estimate!!Total:!!Owner occupied:!!Householder ")) |>
            mutate(label = str_remove_all(label, "Estimate!!Total:!!Renter occupied:!!Householder ")) |>
            mutate(age_bracket_new = case_when(
              label %in% c("55 to 59 years", "60 to 64 years") ~ "55 to 64 years",
              TRUE ~ label
            )) |>
            rename(age_bracket_original = label) |>
            select(NAME, age_bracket_new, age_bracket_original, tenure, estimate, moe)
          
          write_csv(tenure_by_age, here('data', 'demographic', 'census', 'tenure_by_age.csv'))
          
        