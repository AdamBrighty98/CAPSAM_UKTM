# Emissions calculation function

# UKTM energy emissions calculation - take activity data and emission factors
emissions_calculation <- function(activity_data,
                                  ef_data) {
  
  emissions_result <- activity_data %>% 
    dplyr::left_join(ef_data,
                     by = c("sector", "source")) %>% 
    dplyr::mutate(emissions = demand * net_ef)
  
  return(emissions_result)
  
}

# UKTM road transport emissions calculation - take activity data and emission factors
road_emissions_calculation <- function(activity_data,
                                  ef_data) {
  
  req(activity_data)
  req(ef_data)
  
  req("sector" %in% names(activity_data))
  req("euro_tech" %in% names(activity_data))
  req("vkm" %in% names(activity_data))
  
  req("sector" %in% names(ef_data))
  req("euro_tech" %in% names(ef_data))
  req("net_ef" %in% names(ef_data))
  
  emissions_result <- activity_data %>% 
    dplyr::left_join(ef_data,
                     by = c("sector", "euro_tech")) %>% 
    dplyr::mutate(emissions = (vkm * net_ef)/1e9)
  
  return(emissions_result)
  
}

