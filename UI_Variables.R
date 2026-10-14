# Variables for initial selection

sector <- c("ELC", "PRC", "RES", "SER", "IND", "TRA")
scenario <- c("limit", "improved")
pollutant <- c("NOx", "PM2.5", "SO2", "NH3", "VOC")
CAPSAM_Year <- c("2020", "2025", "2030", "2035", "2040", "2045", "2050")
elec_EFs <- c("Limit Values and NAEI Emission Factors", "Improved Emission Factors")
hyd_EFs <- c("Limit Values and NAEI Emission Factors", "Improved Emission Factors")
dom_heat_EFs <- c("Limit Values and NAEI Emission Factors", "Improved Emission Factors")
pub_heat_EFs <- c("Limit Values and NAEI Emission Factors", "Improved Emission Factors")
industry_EFs <- c("Limit Values and NAEI Emission Factors", "Improved Emission Factors")
road_EFs <- c("Limit Values and NAEI Emission Factors", "Improved Emission Factors")
other_emissions_scenario <- c("2018 NAEI Baseline", "2025 NAEI Baseline", "2030 NAEI Baseline", "2035 NAEI Baseline", "2040 NAEI Baseline")
woodburning_scenario <- c("2018 NAEI Baseline", "2025 NAEI Baseline", "2030 NAEI Baseline", "2035 NAEI Baseline", "2040 NAEI Baseline")

# Creating SID column - global numbers
sid_numbers <- c(1,3,7,8,9,10,11,12,13,14,21,25,28,46,47,48,49,50,79,80,81,85)


# Y-Axis Labels
y_label_co2e <- "CO2 Emissions (kt)"
y_label_co2 <- "CO2 Emissions (kt)"
y_label_nox <- "NOx Exposure (ng m-3)"
y_label_nox_emissions <- "NOx Emissions (kt)"
y_label_pm25 <- "Total PM2.5 Exposure (ng m-3)"
y_label_pm25_emissions <- "PM2.5 Emissions (kt)"
y_label_emissions <- "Emissions (kt)"