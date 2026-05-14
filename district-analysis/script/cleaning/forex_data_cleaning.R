      
      ##Load required packages 
      
      library(tidyverse)
      library(readxl)
      library(countrycode)
      
      # Step 1: List all Excel files in your folder
      folder_path <- "data/raw/IMF Forex Data 2001-2024"
      
      excel_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
      
      # Step 2: Read, clean, and reshape each file
      all_data <- excel_files %>%
        map_df(~ {
          # Read data
          data <- read_xlsx(.x, sheet = "Monthly", col_names = FALSE)
          
          # Record the country name
          country_name <- data[2, 1] %>% pull()
          
          # Clean data
          cleaned_data <- data %>%
            slice(-(1:6)) %>% 
            mutate(
              `...2` = ifelse(is.na(`...2`), "unit", `...2`),
              `...3` = ifelse(is.na(`...3`), "indicator_code", `...3`)
            )
          
          # Set column names based on the first row
          colnames(cleaned_data) <- as.character(cleaned_data[1, ])
          
          # Remove the first row after setting column names
          cleaned_data <- cleaned_data[-1, ]
          
          # Add country name as a column
          cleaned_data <- cleaned_data %>%
            mutate(country = country_name)
          
          # Reshape to longer format
          long_data <- cleaned_data %>%
            pivot_longer(
              cols = -c(1:4, country),
              names_to = "period",
              values_to = "forex"
            )
          
          # Return reshaped data
          long_data
        })
      
      
      
      # Read and process forex data in one efficient pipeline
      forex_data_processed <- all_data %>% 
        # Filter for relevant indicator and remove quarterly/monthly data
        filter(Indicator == "Domestic Currency per U.S. Dollar, Period Average",
               !str_detect(period, "Q|M")) %>%
        
        # Clean and transform data
        mutate(
          # Extract year from period
          year = year(ymd(paste0(substr(period, 1, 4), "-01-01"))),
          
          # Clean forex values - remove non-numeric characters and convert
          forex = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", forex))),
          
          # Standardize country names
          country = recode(country, 
                           "Aruba" = "Netherlands", 
                           "Kingdom of the Netherlands" = "Netherlands", 
                           "Curaçao and Sint Maarten" = "Curaçao", 
                           "Euro Area" = "Eurozone"),
          
          # Convert country names using countrycode for consistency
          country = countrycode(country, origin = "country.name", destination = "country.name")
        ) %>%
        
        # Filter for analysis period and remove invalid entries
        filter(year < 2024, !is.na(country), !is.na(forex)) %>%
        select(country, year, forex) %>%
        distinct()
      
      forex_data_processed %>% distinct(country) %>% count()
      forex_data_processed %>% count(year) %>% view()
      
      
      write.csv(forex_data_processed, file = "data/clean/forex_2000_2023.csv")
      
