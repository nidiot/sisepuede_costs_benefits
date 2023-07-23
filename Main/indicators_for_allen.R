#This file puts together a table of indicators for allen

#----------Libraries and Source -----------
setwd('~/Desktop/LAC_Decarb_Git/sisepuede_costs_benefits/Main/')

library(dplyr)
library(reshape2)
source('general_ssp_utilities.R')
vars_searched<-c('test')

#----------Get the filenames and data---------------------
path_to_model_results<-'/Users/nidhi/Desktop/LAC Model Results and Visualizations/'
data_filename<-paste0(path_to_model_results, 'sisepuede_results_WIDE_scaled.csv') #path to model output runs
primary_filename<-paste0(path_to_model_results, 'ATTRIBUTE_PRIMARY.csv') #path to model output primary filename
strategy_filename<-paste0(path_to_model_results, 'ATTRIBUTE_STRATEGY.csv') #path to model output strategy filename
output.file<-read.csv(data_filename)

#---------Prepare the data-------------
data<-output.file
run_attributes<-ssp_merge_run_attributes(primary_filename, strategy_filename)
merged_data<-merge(run_attributes[,c('primary_id', 'strategy_code', 'future_id')], data, by=c('primary_id'), x.all=TRUE)
data<-merged_data


vars<-colnames(data)




#Sectoral (country-sector-level, country-level)
#--------------Electricity [DONE]------------------
#Fraction total energy demand met by electricity
electricity_consumption_by_sector_vars<-vars[grep(glob2rx('energy_consumption_electricity_*_total'), vars)]
energy_consumption_by_sector_vars<-vars[grep(glob2rx('energy_consumption_*_total'), vars)]
energy_consumption_by_sector_vars<-energy_consumption_by_sector_vars[!energy_consumption_by_sector_vars %in% electricity_consumption_by_sector_vars]
data$energy_consumption_electricity_total<-rowSums(data[, electricity_consumption_by_sector_vars])
data$energy_consumption_total<-rowSums(data[, energy_consumption_by_sector_vars])
data$ab_electricity_frac_energy_demand_met_by_electricity<-data$energy_consumption_electricity_total/data$energy_consumption_total

#Fraction of electricity produced by renewables
electricity_production_by_technology_vars<-vars[grep(glob2rx('nemomod_entc_annual_production_by_technology_pp_*'), vars)]
renewables_grep<-paste0(c('pp_solar', 'pp_geothermal', 'pp_ocean', 'pp_wind', 'pp_hydropower'), collapse="|")
electricity_production_by_renewables_vars<-electricity_production_by_technology_vars[grep(renewables_grep, electricity_production_by_technology_vars)]

data$nemomod_entc_production_total<-rowSums(data[, electricity_production_by_technology_vars])
data$nemomod_entc_production_renewables<-rowSums(data[, electricity_production_by_renewables_vars])
data$ab_electricity_frac_of_electricity_produced_by_renewables<-data$nemomod_entc_production_renewables/data$nemomod_entc_production_total

#Electricity demand per capita
population_vars<-vars[grep(glob2rx('population_gnrl_*'), vars)]
data$total_population<-rowSums(data[, population_vars])
data$ab_electricity_electricity_demand_per_capita_PJ<-data$energy_consumption_electricity_total/data$total_population

vars_searched<-c(vars_searched, electricity_consumption_by_sector_vars, energy_consumption_by_sector_vars, electricity_production_by_technology_vars,
                 electricity_production_by_renewables_vars, population_vars)

#----------------Transportation [DONE]---------------
#Fraction total travel demand electrified
modes<-c('aviation', 'powered_bikes', 'public', 'rail_passenger', 'road_heavy_regional', 'road_light', 'water_borne')

vkm_electricity_vars<-paste('vehicle_distance_traveled_trns_', modes, '_electricity', sep='')
vkm_total_vars<-paste('vehicle_distance_traveled_trns_', modes, sep='')
pkm_vars<-paste('passenger_distance_traveled_trns_', modes, sep='')
pkm_electrified_vars<-paste('pkm_electrified_', modes, sep='')
nmodes<-length(modes)

for (m in 1:nmodes){
  varname<-paste0('pkm_electrified_', modes[m])
  frac_elec<-data[,vkm_electricity_vars[m]]/data[,vkm_total_vars[m]]
  data[,pkm_electrified_vars[m]] <- data[,vkm_electricity_vars[m]]/data[,vkm_total_vars[m]] * data[,pkm_vars[m]]
}

data$total_pkm_electrified<-rowSums(data[, pkm_electrified_vars])
data$total_pkm<-rowSums(data[, pkm_vars])
data$ab_transport_frac_pkm_electrified<-data$total_pkm_electrified/data$total_pkm

#Fraction local pkm made by public transit -- cannot do
local_modes<-c('human_powered', 'powered_bikes', 'public','road_light')
pkm_local_modes_vars<-paste('passenger_distance_traveled_trns_', local_modes, sep='')
data$pkm_local<-rowSums(data[, pkm_local_modes_vars])
data$ab_transport_frac_local_pkm_transit<-data$passenger_distance_traveled_trns_public/data$pkm_local

vars_searched<-c(vars_searched, vkm_electricity_vars, vkm_total_vars, pkm_vars, pkm_electrified_vars, pkm_local_modes_vars)


#-----------------Agriculture, forestry and other land uses (AFOLU) [DONE]------
#Fraction of land with tree cover deforested and reforested
#Ruminant consumption per capita
#Cattle ranching productivity (head/ha)

#Cattle produced per capita
data$ab_afolu_cattle_head_produced_per_capita_head<-data$pop_lvst_cattle_nondairy/data$total_population
data$ab_afolu_cattle_head_demanded_per_capita_head<-data$demand_lvst_cattle_nondairy/data$total_population

vars_searched<-c(vars_searched, 'pop_lvst_cattle_nondairy', 'demand_lvst_cattle_nondairy')
  
#-----------------Buildings [DONE]------------------
#Fraction of building energy electrified

data$ab_buildings_frac_building_energy_electrified<-data$energy_consumption_electricity_scoe_total/data$energy_consumption_scoe_total
vars_searched<-c(vars_searched, 'energy_consumption_scoe_total', 'energy_consumption_electricity_scoe_total')

#------------------Industry [DONE]-------------------
#Fraction thermal demand electrified
#Fraction of clinker in cement
#Fraction of flue-gases destroyed
#Fraction of steel produced by non-basic oxygen steelmaking (BOS) methods


data$ab_industry_frac_industry_electrified<-data$energy_consumption_electricity_inen_total/data$energy_consumption_inen_total
data$ab_industry_frac_industry_hydrogen<-data$energy_demand_enfu_subsector_total_pj_inen_fuel_hydrogen/data$energy_consumption_inen_total
data$ab_industry_frac_clinker_in_cement<-data$frac_ippu_cement_clinker

#get initial emissions factors for fgases
industry_data_fgas_init<-data[data$time_period==5, c('primary_id', 'region', 'strategy_code', 'future_id', 'ef_ippu_tonne_c2f6_per_tonne_production_electronics')]
colnames(industry_data_fgas_init)[5]<-'ef_ippu_tonne_c2f6_per_tonne_production_electronics_INIT'
data<-merge(data, industry_data_fgas_init, by=c('primary_id', 'region', 'strategy_code', 'future_id'))

data$ab_industry_frac_fgas_destroyed<-1-(data$ef_ippu_tonne_c2f6_per_tonne_production_electronics/
                                                       data$ef_ippu_tonne_c2f6_per_tonne_production_electronics_INIT)

vars_searched<-c(vars_searched, 'energy_consumption_electricity_inen_total', 'energy_consumption_inen_total',
                 'energy_demand_enfu_subsector_total_pj_inen_fuel_hydrogen', 'frac_ippu_cement_clinker', 'ef_ippu_tonne_c2f6_per_tonne_production_electronics',
                 'ef_ippu_tonne_c2f6_per_tonne_production_electronics_INIT')
                 

#------------------Waste [DONE]-------------------
#Per capita organic waste
data$total_consumer_food_waste<-data$qty_waso_total_food_produced_tonne - data$qty_agrc_food_produced_lost_sent_to_msw_tonne
data$ab_waste_food_waste_per_capita_tonne<-data$total_consumer_food_waste/data$total_population
  
#Fraction of landfill gas captured
data$ab_waste_frac_landfill_gas_captured<-data$frac_waso_landfill_gas_recovered

vars_searched<-c(vars_searched, 'qty_waso_total_food_produced_tonne', 'qty_agrc_food_produced_lost_sent_to_msw_tonne', 
                 'total_consumer_food_waste', 'frac_waso_landfill_gas_recovered')

#------------------National [DONE]-----------------
#GHG emissions: total and per capita
sector_emissions_vars<-vars[grep('emission_co2e_subsector_total_', vars)]
data$ab_national_total_emissions_MtCO2e<-rowSums(data[, sector_emissions_vars])
data$ab_national_emissions_per_capita_tCo2e<-data$ab_national_total_emissions_co2e*10^6/data$total_population

#----------------Write Results---------------
cols_to_grep<-c(
  'primary_id', 
  '\\bregion\\b', 
  'time_period',
  'future_id',
#  'area_lndu',
#  'demand_agrc',
#  'demand_lvst',
#  'yield_agrc',
#  'pop_lvst',
  'emission_co2e',
  'totalvalue_enfu_fuel_consumed',
  glob2rx('energy_consumption_*_total'),
  'energy_demand_enfu_subsector_total_pj_',
  'energy_demand_enfu_total_fuel_',
  'nemomod_entc_annual_production_by_technology',
  'qty_waso',
  'ab_'#,
#  vars_searched
)
trimmed_data_long<-ssp_trim_reshape(data, unique(cols_to_grep))
write.csv(trimmed_data_long, '~/Desktop/Indicator Data for AB/indicator_data_for_ab.csv')
#vars_to_copy<-vars[grep(paste0(unique(c('strategy_code', vars_searched, cols_to_grep)), collapse="|"), vars)]


cols_to_grep_wide<-c(
  'primary_id', 
  '\\bregion\\b', 
  'time_period',
  'future_id',
  'strategy_code',
  'ab_',
  vars_searched
)

vars<-colnames(data)
vars_to_copy<-sort(vars[grep(paste0(c('ab_', vars_searched), collapse="|"), vars)])
vars_to_copy<-c(
  'primary_id', 
  'region', 
  'time_period',
  'future_id',
  'strategy_code',
  vars_to_copy)

write.csv(data[, vars_to_copy], '~/Desktop/Indicator Data for AB/indicator_data_for_ab_wide.csv')
