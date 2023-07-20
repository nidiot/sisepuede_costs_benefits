#calculate the fractin of emissions of electricity attributable to other sectors

temp_data_cols<-colnames(data)

#get the electricity consumption and top-line emissions data
electricity_consumption_sector_vars<-temp_data_cols[grep(glob2rx('energy_consumption_electricity_*_total'), temp_data_cols)]
electricity_consumption_data<-data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, 'emission_co2e_subsector_total_entc', electricity_consumption_sector_vars)]

#calculate total consumption of electricity
electricity_consumption_data$total_electricity_consumption<-rowSums(electricity_consumption_data[,electricity_consumption_sector_vars])

#get the entc emissions variables to calculate fracton of topline attributable to me, fp, adn generation
tmp_cols<-colnames(data)
entc_emissions_vars<-tmp_cols[grep('emission_co2e_co2_entc_', tmp_cols)]
entc_emissions_data<-data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, entc_emissions_vars)]

entc_emissions_data<-entc_emissions_data %>% mutate(total_emissions = rowSums(select(., starts_with('emission_co2e_co2_entc_'))))
entc_emissions_data<-entc_emissions_data %>% mutate(entc_subsector_me_emissions = rowSums(select(., contains('_fuel_mining_and_extraction_'))))
entc_emissions_data<-entc_emissions_data %>% mutate(entc_subsector_elec_emissions = rowSums(select(., contains('_generation_'))))
entc_emissions_data<-entc_emissions_data %>% mutate(entc_subsector_fp_emissions = rowSums(select(., contains('_processing_and_refinement_fp_'))))
#do hydrogen separately
entc_emissions_data<-entc_emissions_data %>% mutate(entc_subsector_fp_hydrogen_emissions = rowSums(select(., contains('_processing_and_refinement_fp_hydrogen'))))


entc_emissions_data$emissions_me_frac<-entc_emissions_data$entc_subsector_me_emissions/entc_emissions_data$total_emissions
entc_emissions_data$emissions_elec_frac<-entc_emissions_data$entc_subsector_elec_emissions/entc_emissions_data$total_emissions
entc_emissions_data$emissions_fp_frac<-entc_emissions_data$entc_subsector_fp_emissions/entc_emissions_data$total_emissions
entc_emissions_data$emissions_fp_hydrogen_frac<-entc_emissions_data$entc_subsector_fp_hydrogen_emissions/entc_emissions_data$total_emissions


#calculate the total emissions from electricity generation
electricity_consumption_data<-merge(electricity_consumption_data, entc_emissions_data, by=SSP_GLOBAL_SIMULATION_IDENTIFIERS)
electricity_consumption_data$emission_co2e_subsector_total_entc_generation <- electricity_consumption_data$emission_co2e_subsector_total_entc * 
  electricity_consumption_data$emissions_elec_frac
electricity_consumption_data$emission_co2e_subsector_total_entc_fp <- electricity_consumption_data$emission_co2e_subsector_total_entc * 
  electricity_consumption_data$emissions_fp_frac
electricity_consumption_data$emission_co2e_subsector_total_entc_me <- electricity_consumption_data$emission_co2e_subsector_total_entc * 
  electricity_consumption_data$emissions_me_frac

#apply fractions to and emisisons to each sector
t<-electricity_consumption_data %>% 
  mutate(
    across(all_of(electricity_consumption_sector_vars), ~ .x/total_electricity_consumption*emission_co2e_subsector_total_entc_generation, .names = "emission_co2e_electricity_{.col}")
  )
cols_from_t<-colnames(t)[grep('emission_co2e_electricity_', colnames(t))]
sector_codes<-str_remove(cols_from_t, 'emission_co2e_electricity_energy_consumption_electricity_')
sector_codes<-str_remove(sector_codes, '_total')
new_cols<-paste0('emission_co2e_from_electricity_for_', sector_codes)
colnames(t)[colnames(t) %in% cols_from_t]<-new_cols
new_cols<-c(new_cols, 'emission_co2e_subsector_total_entc_me', 'emission_co2e_subsector_total_entc_fp', 'emission_co2e_subsector_total_entc_generation')


t<-t[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, new_cols)]
data<-merge(data, t, by=c('primary_id', 'time_period', 'region', 'strategy_code')) 


