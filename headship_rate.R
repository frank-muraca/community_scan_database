      
library(tidycensus)
library(tidyverse)

        vars <- load_variables(dataset = "acs5", year = 2020)

  ## Headship rate ==========================================
        
        
        data <- NULL
        
        for(i in c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021)){
          for(g in c("state","county", "place")){
            
            # Pull data
            tmp <- get_acs(state = "NC",
                           geography = g,
                           variables = vars |> filter(concept %in% c("SEX BY AGE", "TENURE BY AGE OF HOUSEHOLDER")) |> pull(name),
                           year = i,
                           survey = "acs1") |>
              # join labels
              
              left_join(vars |> filter(concept %in% c("SEX BY AGE", "TENURE BY AGE OF HOUSEHOLDER")), by = c("variable" = "name")) |>
              
              mutate(year = i)
            
            # Label geography in the data
            tmp$geo <- g
            
            # Combine with other geographies
            data <- rbind(data, tmp)
          }
          
        }
        
        
        sex_by_age_time <- 
          
          data |> filter(concept == "SEX BY AGE") |>
          
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
        
        pop_25_34 <-
          
          sex_by_age_time |>
          
          mutate(age_bracket_original = case_when(
            age_bracket_raw %in% c("15 to 19 years", "20 to 24 years") ~ "15 to 24 years",
            age_bracket_raw %in% c("25 to 29 years", "30 to 34 years") ~ "25 to 34 years",
            age_bracket_raw %in% c("35 to 39 years", "40 to 44 years") ~ "35 to 44 years",
            age_bracket_raw %in% c("45 to 49 years", "50 to 54 years") ~ "45 to 54 years",
            age_bracket_raw %in% c("55 to 59 years") ~ "55 to 59 years",
            age_bracket_raw %iN% c("60 to 64 years") ~ "60 to 64 years",
            age_bracket_raw %in% c("65 to 69 years", "70 to 74 years") ~ "65 to 74 years",
            age_bracket_raw
            
          ))
          group_by(year, NAME, age_bracket_raw) |>
          
          mutate(total_pop = sum(total, na.rm = T))
        
        
        hh_25_34 <- 
        data |>
          
          filter(concept == "TENURE BY AGE OF HOUSEHOLDER") |>
          
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
          select(NAME, age_bracket_new, age_bracket_original, tenure, estimate, moe, year) |>
          
          #filter(age_bracket_new == "25 to 34 years") |>
          
          group_by(year, NAME, age_bracket_original) |>
          
          summarise(total_hh = sum(estimate, na.rm = T))

        merge <- pop_25_34 |> left_join(hh_25_34, by = c("NAME" = "NAME", "year" = "year"))
        
        merge <- 
          
          merge |>
          
          mutate(NAME = str_remove(NAME, ", North Carolina")) |>
          
          filter(NAME == "Durham County") |>
          
          mutate(headship_rate = total_hh/total_pop) 
        
        
        summary(merge$headship_rate)
        
        merge |>
          
          group_by(year, NAME) |>
          
          summarize(hh_rate_var = var(headship_rate)) |>
          
          ggplot(aes(x = reorder(year, hh_rate_var), y = hh_rate_var)) +
                   
                   geom_point() +
          
          coord_flip()
        
        
        merge |>
          
          mutate(NAME = str_remove(NAME, ", North Carolina")) |>
          
          mutate(headship_rate = total_hh/total_pop) |>
          
          ggplot(aes(x = year, y = headship_rate, group = age_bracket_new)) +
          
          geom_point() +
          
          geom_line() +
          
          theme_minimal() +
          
          facet_wrap(~age_bracket_new, ncol = 5) +
          
          scale_y_continuous()
          
          
        