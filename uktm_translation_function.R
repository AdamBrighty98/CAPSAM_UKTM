
# UKTM translation from long-form raw energy data to long-form sector data
sector_data <- function(uktm_data) { # function for tidyverse data transformation pipeline taking UKTM data table and sector codename
  
  uktm_data %>%  
    select(epoch, Sector, CAPSAM_name, pv) %>% # Select only the relevant columns - no need for "" within function
    rename(            # Rename columns to something more intuitive
      year = epoch,
      sector = Sector,
      source = CAPSAM_name,
      demand = pv
    ) %>%  
    filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>%                # Keep only the years of interest
    group_by(year, sector, source) %>%                                                          # Groups rows that share the same Year, Sector, and Source
    summarise(demand = sum(demand, na.rm = TRUE), .groups = "drop") %>%                  # Adds up Demands per year
    arrange(source) %>% # Sorting Sources alphabetically
    mutate(
      demand = demand/3.6  # convert PJ → TWh - Demand = Demand to maintain same column
    ) 
}

# UKTM translation from long-form raw vehicle capacity to fleet mix road vkm
road_data_vkm_translation <- function(road_uktm_data, road_vkm, fleet_mix) {
  
  road_uktm_data %>% 
    select(epoch, CAPSAM_name, pv) %>% # only need year, vehicle type, and  vehicle numbers
    rename(
      year = epoch,
      source = CAPSAM_name,
      fleet = pv
    ) %>% 
    filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>% # years of interest
    group_by(year, source) %>% # group vehicles by year and vehicle type
    summarise(fleet = sum(fleet, na.rm = TRUE), .groups = "drop") %>% 
    mutate(
      fleet = fleet * 1000 # get fleet vehicle numbers into single number format
    ) %>% 
    left_join(road_vkm, by = "source") %>% # left_join() road vkm and match by source/vehicle type - creates overall vehicle type vkm
    mutate(
      total_vkm = fleet * annual_vkm # mutate to create overall vehicle type vkm with average vkm for each vehicle type within fleet
    ) %>% 
    left_join(fleet_mix, by = c("source", "year")) %>% # left_join() again by year and source/vehicle type to include fleet mix/share of fleet by euro_type
    mutate(
      vkm = total_vkm * share # mutate to create euro-specific vkm for each euro-type by mutltiplying total vehicle type vkm by share of fleet_mix
    ) %>% 
    rename(
      euro_tech = vtech # changing vtech to euro_tech to match emission factor name
      )
  
}


# UKTM translation from long-form raw energy data to long-form sector data for wide table reading
sector_data_output <- function(uktm_data) { # function for tidyverse data transformation pipeline taking UKTM data table and sector codename
  
  uktm_data %>%  
    select(epoch, Sector, CAPSAM_name, pv) %>% # Select only the relevant columns - no need for "" within function
    rename(            # Rename columns to something more intuitive
      year = epoch,
      sector = Sector,
      source = CAPSAM_name,
      demand = pv
    ) %>%  
    filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>%                # Keep only the years of interest
    group_by(year, sector, source) %>%                                                          # Groups rows that share the same Year, Sector, and Source
    summarise(demand = sum(demand, na.rm = TRUE), .groups = "drop") %>%                  # Adds up Demands per year
    mutate(
      demand = demand/3.6  # convert PJ → TWh - Demand = Demand to maintain same column
    ) %>% 
    pivot_wider(names_from = year, values_from = demand, values_fill = 0) %>% 
    select(sector, source, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`) %>% 
    arrange(sector, source)
}


sector_data_fuel_split <- function(uktm_data) { # function for tidyverse data transformation pipeline taking UKTM data table and sector codename
  
  uktm_data %>%  
    select(epoch, Sector, CAPSAM_name, pv) %>% # Select only the relevant columns - no need for "" within function
    rename(            # Rename columns to something more intuitive
      year = epoch,
      sector = Sector,
      source = CAPSAM_name,
      demand = pv
    ) %>%  
    filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>%                # Keep only the years of interest
    group_by(year, sector, source) %>%                                                          # Groups rows that share the same Year, Sector, and Source
    summarise(demand = sum(demand, na.rm = TRUE), .groups = "drop") %>%                  # Adds up Demands per year
    mutate(
      demand = demand/3.6,  # convert PJ → TWh - Demand = Demand to maintain same column
      fuel = case_when( # creating a new column that takes it data from the correlating specific fuel in the "source" column
        str_detect(source, "gas_") ~ "Natural_Gas",
        str_detect(source, "^gas") ~ "Natural_Gas",
        str_detect(source, "_gas$") ~ "Natural_Gas",
        str_detect(source, "biomethane") ~ "Biomethane",
        str_detect(source, "methane") ~ "Natural_Gas",
        str_detect(source, "biomass_") ~ "Bioenergy", # moved biomass up to avoid issue of 'boiler' going to oil
        str_detect(source, "^biomass") ~ "Bioenergy",
        str_detect(source, "_biomass$") ~ "Bioenergy",
        str_detect(source, "coal") ~ "Coal",
        str_detect(source, "bioenergy") ~ "Bioenergy",
        str_detect(source, "kerosene") ~ "Oil",
        str_detect(source, "oil") ~ "Oil",
        str_detect(source, "waste") ~ "Waste",
        str_detect(source, "hydrogen") ~ "Hydrogen",
        str_detect(source, "autothermal") ~ "Natural_Gas",
        str_detect(source, "other") ~ "Oil",
        str_detect(source, "biogas") ~ "Biomethane",
      str_detect(source, "beccs") ~ "Bioenergy")
      ) %>% 
    filter(
      sector != "TRA"
    ) %>% 
    group_by(year, sector, fuel) %>% # grouping overlapping data
    summarise(demand = sum(demand, na.rm = TRUE), .groups = "drop") # summing the grouped data
  }


road_data_fuel_split <- function(road_uktm_data) { # function for tidyverse data transformation pipeline taking UKTM data table and sector codename
  
  road_uktm_data %>%  
    select(epoch, CAPSAM_name, pv) %>% # Select only the relevant columns - no need for "" within function
    rename(            # Rename columns to something more intuitive
      year = epoch,
      source = CAPSAM_name,
      vehicle_numbers = pv
    ) %>%  
    filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>%                # Keep only the years of interest
    mutate(
      fuel = case_when( # creating a new column that takes it data from the correlating specific fuel in the "source" column
        str_detect(source, "bev") ~ "BEV",
        str_detect(source, "diesel") ~ "Diesel",
        str_detect(source, "fuel_cell") ~ "Hydrogen Fuel Cell",
        str_detect(source, "petrol") ~ "Petrol",
        str_detect(source, "lpg") ~ "LPG")
    ) %>% 
    group_by(year, fuel) %>% # grouping overlapping data for year and fuel
    summarise(vehicle_numbers = sum(vehicle_numbers, na.rm = TRUE), .groups = "drop")                  # Adds up vehicle_numbers per year per fuel type

  }



