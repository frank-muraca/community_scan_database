library(tidyverse)
library(tidycensus)


        age_projections <- read_csv(here('data', 'demographic', "NCprojectionsbyagegrp2022.csv")) |>
          mutate(county = paste0(county, " County"))
        ethn_projections <- read_csv(here('data', 'demographic', "NCPROJECTIONSBYHISP2021.csv")) |>
          mutate(county = paste0(county, " County"))
       
        total_proj <- 
        age_projections |>
          filter(race != "Total") |>
          filter(sex != "Total") |>
          group_by(county, year) |>
          summarise_at(vars(starts_with("age")), sum, na.rm = T) |>
          pivot_longer(cols = -c(county, year), names_to = "age_bracket", values_to = "value") |>
          mutate(decade = case_when(
            age_bracket %in% c("age0to17") ~ "0 to 17",
            age_bracket %in% c("age18to24") ~ "18-24",
            age_bracket %in% c("age25to34") ~ "25-34",
            age_bracket %in% c("age35to44") ~ "35-44",
            age_bracket %in% c("age45to54") ~ "45-54",
            age_bracket %in% c("age55to59", "age60to64") ~ "55-64",
            age_bracket %in% c("age65plus") ~ "65+",
            TRUE ~ "Uncategorized"
          )) |>
          filter(decade != "Uncategorized") |>
          group_by(county, year) |>
          summarize(total = sum(value,na.rm = T)) 
        
        write_csv(total_proj, here('data', 'demographic', 'total_projections.csv'))
        
        age_proj <- age_projections |>
          filter(race != "Total") |>
          filter(sex != "Total") |>
          group_by(county, year) |>
          summarise_at(vars(starts_with("age")), sum, na.rm = T) |>
          pivot_longer(cols = -c(county, year), names_to = "age_bracket", values_to = "value") |>
          mutate(decade = case_when(
            age_bracket %in% c("age0to17") ~ "0 to 17",
            age_bracket %in% c("age18to24") ~ "18-24",
            age_bracket %in% c("age25to34") ~ "25-34",
            age_bracket %in% c("age35to44") ~ "35-44",
            age_bracket %in% c("age45to54") ~ "45-54",
            age_bracket %in% c("age55to59", "age60to64") ~ "55-64",
            age_bracket %in% c("age65plus") ~ "65+",
            TRUE ~ "Uncategorized"
          )) |>
          filter(decade != "Uncategorized") |>
          group_by(county, year, decade) |>
          summarize(total = sum(value, na.rm = T))
        
        write_csv(age_proj, here('data', 'demographic', 'age_projections.csv'))
      