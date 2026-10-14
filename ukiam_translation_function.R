# UKIAM emissions translation function

################################################################################
# Hard sheets

# Source IDs linked to UKTM sources
source_id_sheet <- read_excel("EFs_and_IFs/ukiam_sid_list.xlsx")

# Hydrogen emissions split between 79, 80, 81
hydrogen_sid_split <- read_excel("EFs_and_IFs/hydrogen_sid_split.xlsx")

################################################################################
# UKTM energy emissions translated into SID slots for specific EF scenario

energy_emissions_sid_translation <- function(energy_emissions_data, scenario) { 
  
  uktm_sid_emissions_no_h2_adj <- energy_emissions_data %>% 
    dplyr::left_join(source_id_sheet, by = c("sector", "source")) %>% # left join to include source_id number for each source
    dplyr::group_by(year, source_id, pollutant) %>% # grouping each source_id for each year and pollutant
    dplyr::filter(ef_scenario == scenario) %>% # filter for either improved or limit value emission factors to prevent double counting
    dplyr::summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") # emissions column from within emissions() reactive
  
  energy_uktm_sid_emissions <- uktm_sid_emissions_no_h2_adj %>% 
    dplyr::left_join(hydrogen_sid_split, by = "source_id") %>% 
    dplyr::mutate(
      new_source_id = dplyr::coalesce(new_source_id, source_id),
      share = dplyr::coalesce(share, 1),
      emissions = emissions * share
    ) %>%
    dplyr::select(year, source_id = new_source_id, pollutant, emissions) %>% 
    filter(!is.na(source_id))
  
  return(energy_uktm_sid_emissions)
  
}

################################################################################
# UKTM road transport emissions translated into SID slots

road_emissions_sid_translation <- function(road_emissions_data) {
  
  road_uktm_sid_emissions <- road_emissions_data %>% 
    dplyr::left_join(source_id_sheet, by = c("source")) %>% 
    dplyr::group_by(year, source_id, pollutant) %>% # grouping each source_id for each year and pollutant
    dplyr::summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%  # summarise emissions column from within emissions() reactive - leaves only summarised column and group_by columns
    filter(!is.na(source_id))
  
  return(road_uktm_sid_emissions)
  
}

################################################################################
# Binding UKTM road and energy SID slots

total_uktm_sid_translation <- function(energy_emissions_data, road_emissions_data) {
  
  uktm_sid_emissions <- dplyr::bind_rows(energy_emissions_data, road_emissions_data) %>% 
    dplyr::arrange(year, source_id) %>% 
    pivot_wider(names_from = pollutant, values_from = emissions) %>% 
    dplyr::rename(SID = source_id) %>% 
    dplyr::select(year, SID, NH3, SO2, NOx, PM2.5, VOC) %>% 
    dplyr::group_by(year, SID) %>% 
    dplyr::summarise(
      NH3   = max(NH3, na.rm = TRUE),
      SO2   = max(SO2, na.rm = TRUE),
      NOx   = max(NOx, na.rm = TRUE),
      PM2.5 = max(`PM2.5`, na.rm = TRUE),
      VOC   = max(VOC, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(uktm_sid_emissions)
  
}

################################################################################
# Final UKIAM format for all emissions - combining non-energy and UKTM energy SID slots

# Non-energy UKIAM emissions
ukiam_remaining_sids <- "EFs_and_IFs/UKIAM_Remaining_SIDs.xlsx"
ukiam_remaining_sids_all <- read_excel(ukiam_remaining_sids, sheet = "B_All_Remaining_SIDs") # for final UKIAM emissions results

# Domestic woodburning addition for SID 10
dom_wood_adj <- read_excel(ukiam_remaining_sids, sheet = "Dom_Wood_Adj") # for final UKIAM emissions results

# PM10 factor to apply to PM2.5
pm10_adj <- read_excel(ukiam_remaining_sids, sheet = "PM10_Adj") %>% 
  dplyr::select(SID, Reciprocal) %>% 
  dplyr::rename(factor = Reciprocal) # for final UKIAM emissions results

# full_join() - keep everything from both tables
# Use left_join() if one dataset is your “master” SID list and you only want matching data added.
# Use full_join() if both datasets contain important unique SIDs you want preserved.

# Final UKIAM translation - taking UKTM, UKIAM non-energy, domestic woodburning adjustment, PM10 factor, and the year
total_ukiam_emissions_translation <- function(uktm_ems, remaining_ukiam_ems, dom_wood_adj_fn, pm10_adj_fn, ukiam_year) {
  
  ems_test_total <- uktm_ems %>% 
    dplyr::mutate(
      NH3 = NH3 * 1000,
      SO2 = SO2 * 1000,
      NOx = NOx * 1000,
      PM2.5 = PM2.5 * 1000,
      VOC = VOC * 1000
    ) %>% 
    dplyr::full_join(remaining_ukiam_ems, by = c("year", "SID", "NH3", "SO2", "NOx", "PM2.5", "VOC")) %>% 
    dplyr::arrange(year, SID)
  
  dom_wood_adj_year <- dom_wood_adj_fn %>% 
    dplyr::filter(year == ukiam_year) %>% 
    dplyr::select(-year) # to drop year
  
  total_ukiam_ems_annual <- ems_test_total %>% 
    dplyr::filter(year == ukiam_year) %>% 
    dplyr::select(-year) %>% 
    tidyr::complete(SID = 1:93) %>% 
    tidyr::replace_na(list( # not using fill as want zeros - so complete then replace_na with column_name = 0
      NH3 = 0,
      SO2 = 0,
      NOx = 0,
      PM2.5 = 0,
      VOC = 0
    )) %>% 
    dplyr::full_join(dom_wood_adj_year, by = c("SID", "NH3", "SO2", "NOx", "PM2.5", "VOC")) %>% 
    dplyr::group_by(SID) %>% 
    dplyr::summarise(across(everything(), sum, na.rm = T)) %>% 
    dplyr::left_join(pm10_adj, by = "SID") %>% 
    dplyr::mutate(PM10 = PM2.5 * factor) %>% 
    dplyr::select(SID, NH3, SO2, NOx, PM10, PM2.5, VOC)
  
  return(total_ukiam_ems_annual)
  
}
