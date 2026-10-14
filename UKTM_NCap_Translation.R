# UKTM to BRUTAL NCap translation function

# Reading in NCap UK TIMES data
ncap_data <- file.choose() %>% 
  read_excel(.)

################################################################################
################################################################################
# Total sales function - calculates total sales by fleet model vehicle type

ncap_to_sales_translation_function <- function(ncap_data) {
  
  ncap_sales <- ncap_data %>% 
    dplyr::rename( # rename column to normal names for fleet model
      year = epoch,
      sales = pv
    ) %>% 
    dplyr::mutate(vehicle_type = case_when( # mutate to create a new column called vehicle type - which is populated when str_detect matches cell text within the column directed
      
      str_detect(CAPSAM_name, "_car") ~ "car",
      str_detect(CAPSAM_name, "_lgv") ~ "lgv",
      str_detect(CAPSAM_name, "_bus") ~ "bus",
      str_detect(CAPSAM_name, "_motorcycle") ~ "motorcycle",
      str_detect(CAPSAM_name, "_hgv") & str_detect(Description, "RIGID") ~ "rigid_hgv", # two strings needed
      str_detect(CAPSAM_name, "_hgv") & str_detect(Description, "ARTIC") ~ "artic_hgv", # two strings needed
      TRUE ~ "other"
    )) %>% 
    dplyr::group_by(year, vehicle_type) %>% # grouping by year and vehicle type
    dplyr::summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") # summarising total sales into sales column, overwrites prev sales column - summarise() collapses each group into one row and only keeps: the grouping columns, the summary columns you create. If you want to keep all rows/columns while adding grouped totals, use mutate() instead of summarise() (like when modifying emissions from kt to t in ukiam functions)

}

# Long aggregated total sales by vehicle type
total_sales <- ncap_to_sales_translation_function(ncap_data)

# Wide aggregated total sales by vehicle type - needed for fleet model
total_sales_wide <- total_sales %>% 
  pivot_wider(names_from = vehicle_type, values_from = sales) # pivot wider to get vehicle type across top and sales by year in columns for each type

################################################################################
################################################################################
# Vehicle share functions - taking long aggregated total sales and fleet model vehicle type sales - dividing vehicle type sales by total sales

# Car share function

ncap_to_car_share_translation_function <- function(ncap_data, sales_data) {
  
  total_car_sales <- total_sales %>% 
    dplyr::filter(
      vehicle_type == "car"
    ) %>% 
    dplyr::rename(
      total_sales = sales
    )
  
  car_ncap_share <- ncap_data %>% 
    dplyr::rename( # rename column to normal names for fleet model
      year = epoch,
      sales = pv
    ) %>% 
    dplyr::filter(
     str_detect(CAPSAM_name, "_car")
    ) %>% 
    dplyr::left_join(total_car_sales, by = "year") %>% 
    dplyr::mutate(share = sales/total_sales) %>% 
    dplyr::select(year, CAPSAM_name, share) %>% 
    dplyr::filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>% 
    pivot_wider(names_from = CAPSAM_name, values_from = share, values_fill = 0) %>% # values fill instead of na.rm = TRUE
    dplyr::mutate(
      diesel_hev = 0,
      petrol_hev = 0
    ) %>% 
    dplyr::rename(
      bev = bev_car,
      diesel = diesel_car,
      petrol = petrol_car,
      diesel_phev = phev_diesel_car,
      petrol_phev = phev_petrol_car
    ) %>% 
    dplyr::select(year, diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev) # to make sure all in same order and to remove additional columns e.g., LPG or fuel cell
    
}

car_share <- ncap_to_car_share_translation_function(ncap_data, total_sales)

################################################################################
# LGV share function

ncap_to_lgv_share_translation_function <- function(ncap_data, sales_data) {
  
  total_lgv_sales <- total_sales %>% 
    dplyr::filter(
      vehicle_type == "lgv"
    ) %>% 
    dplyr::rename(
      total_sales = sales
    )
  
  lgv_ncap_share <- ncap_data %>% 
    dplyr::rename( # rename column to normal names for fleet model
      year = epoch,
      sales = pv
    ) %>% 
    dplyr::filter(
      str_detect(CAPSAM_name, "_lgv")
    ) %>% 
    dplyr::left_join(total_lgv_sales, by = "year") %>% 
    dplyr::mutate(share = sales/total_sales) %>% 
    dplyr::select(year, CAPSAM_name, share) %>% 
    dplyr::filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>% 
    pivot_wider(names_from = CAPSAM_name, values_from = share, values_fill = 0) %>% # values fill instead of na.rm = TRUE
    dplyr::mutate(
      diesel_hev = 0,
      petrol_hev = 0,
      diesel_phev = 0
    ) %>% 
    dplyr::rename(
      bev = bev_lgv,
      diesel = diesel_lgv,
      petrol = petrol_lgv,
      petrol_phev = phev_petrol_lgv
    ) %>% 
    dplyr::select(year, diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev) # to make sure all in same order and to remove additional columns e.g., LPG or fuel cell
  
}

lgv_share <- ncap_to_lgv_share_translation_function(ncap_data, total_sales)

################################################################################
# Bus share function - need to include fuelcell

ncap_to_bus_share_translation_function <- function(ncap_data, sales_data) {
  
  total_bus_sales <- total_sales %>% 
    dplyr::filter(
      vehicle_type == "bus"
    ) %>% 
    dplyr::rename(
      total_sales = sales
    )
  
  bus_ncap_share <- ncap_data %>% 
    dplyr::rename( # rename column to normal names for fleet model
      year = epoch,
      sales = pv
    ) %>% 
    dplyr::filter(
      str_detect(CAPSAM_name, "_bus")
    ) %>% 
    dplyr::left_join(total_bus_sales, by = "year") %>% 
    dplyr::mutate(share = sales/total_sales) %>% 
    dplyr::select(year, CAPSAM_name, share) %>% 
    dplyr::filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>% 
    pivot_wider(names_from = CAPSAM_name, values_from = share, values_fill = 0) %>% # values fill instead of na.rm = TRUE
    dplyr::mutate(
      diesel_hev = 0,
      petrol_hev = 0,
      diesel_phev = 0,
      petrol = 0,
      petrol_phev = 0,
      bev_bus = bev_bus + fuel_cell_bus
    ) %>% 
    dplyr::rename(
      bev = bev_bus,
      diesel = diesel_bus,
    ) %>% 
    dplyr::select(year, diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev) # to make sure all in same order and to remove additional columns e.g., LPG or fuel cell
  
}

bus_share <- ncap_to_bus_share_translation_function(ncap_data, total_sales)


################################################################################
# Motorcycle share function - need to include fuelcell

ncap_to_motorcycle_share_translation_function <- function(ncap_data, sales_data) {
  
  total_motorcycle_sales <- total_sales %>% 
    dplyr::filter(
      vehicle_type == "motorcycle"
    ) %>% 
    dplyr::rename(
      total_sales = sales
    )
  
  motorcycle_ncap_share <- ncap_data %>% 
    dplyr::rename( # rename column to normal names for fleet model
      year = epoch,
      sales = pv
    ) %>% 
    dplyr::filter(
      str_detect(CAPSAM_name, "_motorcycle")
    ) %>% 
    dplyr::left_join(total_motorcycle_sales, by = "year") %>% 
    dplyr::mutate(share = sales/total_sales) %>% 
    dplyr::select(year, CAPSAM_name, share) %>% 
    dplyr::filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>% 
    pivot_wider(names_from = CAPSAM_name, values_from = share, values_fill = 0) %>% # values fill instead of na.rm = TRUE
    dplyr::mutate(
      diesel_hev = 0,
      petrol_hev = 0,
      diesel_phev = 0,
      diesel = 0,
      petrol_phev = 0,
      #bev_motorcycle = bev_motorcycle + fuel_cell_motorcycle
    ) %>% 
    dplyr::rename(
      bev = bev_motorcycle,
      petrol = petrol_motorcycle,
    ) %>% 
    dplyr::select(year, diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev) # to make sure all in same order and to remove additional columns e.g., LPG or fuel cell
  
}

motorcycle_share <- ncap_to_motorcycle_share_translation_function(ncap_data, total_sales)

################################################################################
# Articulated HGV share function - need to include fuelcell

ncap_to_artic_hgv_share_translation_function <- function(ncap_data, sales_data) {
  
  total_artic_hgv_sales <- total_sales %>% 
    dplyr::filter(
      vehicle_type == "artic_hgv"
    ) %>% 
    dplyr::rename(
      total_sales = sales
    )
  
  artic_hgv_ncap_share <- ncap_data %>% 
    dplyr::rename( # rename column to normal names for fleet model
      year = epoch,
      sales = pv
    ) %>% 
    dplyr::filter(
      str_detect(CAPSAM_name, "_hgv") & str_detect(Description, "ARTIC")
    ) %>% 
    dplyr::left_join(total_artic_hgv_sales, by = "year") %>% 
    dplyr::mutate(share = sales/total_sales) %>% 
    dplyr::select(year, CAPSAM_name, share) %>% 
    dplyr::filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>% 
    pivot_wider(names_from = CAPSAM_name, values_from = share, values_fill = 0) %>% # values fill instead of na.rm = TRUE
    dplyr::mutate(
      diesel_hev = 0,
      petrol_hev = 0,
      diesel_phev = 0,
      petrol = 0,
      petrol_phev = 0,
      #bev_hgv = bev_hgv + fuel_cell_hgv
    ) %>% 
    dplyr::rename(
      bev = bev_hgv,
      diesel = diesel_hgv,
    ) %>% 
    dplyr::select(year, diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev) # to make sure all in same order and to remove additional columns e.g., LPG or fuel cell
  
}

artic_hgv_share <- ncap_to_artic_hgv_share_translation_function(ncap_data, total_sales)


################################################################################
# Rigid HGV share function - no need to include fuelcell

ncap_to_rigid_hgv_share_translation_function <- function(ncap_data, sales_data) {
  
  total_rigid_hgv_sales <- total_sales %>% 
    dplyr::filter(
      vehicle_type == "rigid_hgv"
    ) %>% 
    dplyr::rename(
      total_sales = sales
    )
  
  rigid_hgv_ncap_share <- ncap_data %>% 
    dplyr::rename( # rename column to normal names for fleet model
      year = epoch,
      sales = pv
    ) %>% 
    dplyr::filter(
      str_detect(CAPSAM_name, "_hgv") & str_detect(Description, "RIGID")
    ) %>% 
    dplyr::left_join(total_rigid_hgv_sales, by = "year") %>% 
    dplyr::mutate(share = sales/total_sales) %>% 
    dplyr::select(year, CAPSAM_name, share) %>% 
    dplyr::filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>% 
    dplyr::group_by(year, CAPSAM_name) %>% # group_by and summarise to capture two rigid classes into one year and CAPSAM_name and total them up, removes duplication that prevents pivot_wider list error
    dplyr::summarise(share = sum(share, na.rm = TRUE), .groups = "drop") %>% 
    pivot_wider(names_from = CAPSAM_name, values_from = share, values_fill = 0) %>% # values fill instead of na.rm = TRUE
    dplyr::mutate(
      diesel_hev = 0,
      petrol_hev = 0,
      diesel_phev = 0,
      petrol = 0,
      petrol_phev = 0
    ) %>% 
    dplyr::rename(
      bev = bev_hgv,
      diesel = diesel_hgv
    ) %>% 
    dplyr::select(year, diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev) # to make sure all in same order and to remove additional columns e.g., LPG or fuel cell
}

rigid_hgv_share <- ncap_to_rigid_hgv_share_translation_function(ncap_data, total_sales)


################################################################################
################################################################################
# Converting into annual sales and shares
# 5 year total sales into annual sales (e.g., 5 year total for 2020 is 2020 - 2024) - creating a stepwise fill, where year column is filled with gap years and sales gaps in interim years are filled with fill(.direction = "down")
# Annual shares to be kept constant with 5 year shares, just with interim years being the same.

# Annual total sales - stepwise complete interim years and fill in gaps, divide by 5 to get annual sales
annual_total_sales <- total_sales %>% 
  dplyr::filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) %>% 
  dplyr::mutate(sales = sales/5) %>% 
  dplyr::group_by(vehicle_type) %>% # group_by vehicle type to perform operations separately for each vehicle
  tidyr::complete(year = full_seq(min(year):max(year), 1)) %>% # from min to max year with increments of 1, can just do complete(year = 2020:2050)
  tidyr::fill(sales, .direction = "down") %>%  # fill in gaps created by complete() within sales column with fill(.direction = "down") to fill 2021-2024 with 2020 data for each vehicle type
  dplyr::relocate(vehicle_type, .after = 2) %>% # putting vehicle_type back in middle after year column
  ungroup()


# Annual vehicle shares - complete interim years and fill in gaps with shares with previous 5-year average
annual_car_share <- car_share %>% 
  tidyr::complete(year = 2020:2050) %>% 
  tidyr::fill(c(diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev), .direction = "down")

annual_lgv_share <- lgv_share %>% 
  tidyr::complete(year = 2020:2050) %>% 
  tidyr::fill(c(diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev), .direction = "down")

annual_bus_share <- bus_share %>% 
  tidyr::complete(year = 2020:2050) %>% 
  tidyr::fill(c(diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev), .direction = "down")

annual_motorcycle_share <- motorcycle_share %>% 
  tidyr::complete(year = 2020:2050) %>% 
  tidyr::fill(c(diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev), .direction = "down")

annual_car_share <- car_share %>% 
  tidyr::complete(year = 2020:2050) %>% 
  tidyr::fill(c(diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev), .direction = "down")

annual_artic_hgv_share <- artic_hgv_share %>% 
  tidyr::complete(year = 2020:2050) %>% 
  tidyr::fill(c(diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev), .direction = "down")

annual_rigid_hgv_share <- rigid_hgv_share %>% 
  tidyr::complete(year = 2020:2050) %>% 
  tidyr::fill(c(diesel, diesel_hev, petrol, petrol_hev, petrol_phev, diesel_phev, bev), .direction = "down")

################################################################################
################################################################################
# Writing .csv files for fleet modelling

# Car Sales .csv
annual_total_sales %>% 
  dplyr::filter(
    vehicle_type == "car"
  ) %>% 
  dplyr::select(c(year, sales)) %>% 
  write.csv(., file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_car_new_sales_volume.csv", row.names = F)

# Car share .csv
write.csv(annual_car_share, file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_car_new_sales_share.csv", row.names = F)

# LGV Sales .csv
annual_total_sales %>% 
  dplyr::filter(
    vehicle_type == "lgv"
  ) %>% 
  dplyr::select(c(year, sales)) %>% 
  write.csv(., file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_lgv_new_sales_volume.csv", row.names = F)

# LGV share .csv
write.csv(annual_lgv_share, file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_lgv_new_sales_share.csv", row.names = F)

# Bus Sales .csv
annual_total_sales %>% 
  dplyr::filter(
    vehicle_type == "bus"
  ) %>% 
  dplyr::select(c(year, sales)) %>% 
  write.csv(., file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_bus_new_sales_volume.csv", row.names = F)

# Bus share .csv
write.csv(annual_bus_share, file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_bus_new_sales_share.csv", row.names = F)

# Motorcycle Sales .csv
annual_total_sales %>% 
  dplyr::filter(
    vehicle_type == "motorcycle"
  ) %>% 
  dplyr::select(c(year, sales)) %>% 
  write.csv(., file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_motorcycle_new_sales_volume.csv", row.names = F)

# Motorcycle share .csv
write.csv(annual_motorcycle_share, file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_motorcycle_new_sales_share.csv", row.names = F)

# Artic HGV Sales .csv
annual_total_sales %>% 
  dplyr::filter(
    vehicle_type == "artic_hgv"
  ) %>% 
  dplyr::select(c(year, sales)) %>% 
  write.csv(., file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_artic_hgv_new_sales_volume.csv", row.names = F)

# Artic HGV share .csv
write.csv(annual_artic_hgv_share, file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_artic_hgv_new_sales_share.csv", row.names = F)

# Rigid HGV Sales .csv
annual_total_sales %>% 
  dplyr::filter(
    vehicle_type == "rigid_hgv"
  ) %>% 
  dplyr::select(c(year, sales)) %>% 
  write.csv(., file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_rigid_hgv_new_sales_volume.csv", row.names = F)

# Rigid HGV share .csv
write.csv(annual_rigid_hgv_share, file = "fleet_model_projections/annual_ftm_projections/d143_P3Road_elc_85p_mileage_comp_rigid_hgv_new_sales_share.csv", row.names = F)


