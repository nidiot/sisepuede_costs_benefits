#This file recalculates the cost/PJ of electricity for hte totalvalue_electricity variable

#------------Source files and set paths------------
setwd('~/Desktop/LAC_Decarb_Git/sisepuede_costs_benefits/Main/')
source('cb_config.R')
source('cb_utilities.R')
source('cb_strategy_specific_functions.R')
source('general_ssp_utilities.R')

#Paths to data files
path_to_model_results<-'~/Desktop/LAC Model Results and Visualizations/'

data_filename<-paste0(path_to_model_results, 
                      list.files(path=path_to_model_results, 
                                 pattern = glob2rx('sisepuede_results_WIDE_scaled*'))) #path to model output runs
primary_filename<-paste0(path_to_model_results, 'ATTRIBUTE_PRIMARY.csv') #path to model output primary filename
strategy_filename<-paste0(path_to_model_results, 'ATTRIBUTE_STRATEGY.csv') #path to model output strategy filename


cb_filename<-paste0(path_to_model_results, 'cost_benefit_results.csv')

#-------------Read required files------------------------
output.file<-read.csv(data_filename)
cb_data<-read.csv(cb_filename)
cb_data<-cb_data[,-1] #get rid of the X column

#-------------Clean and Prep the data---------------------
#Read data
data<-output.file


#Merge model output with strategy attributes (mainly the strategy_code)
run_attributes<-ssp_merge_run_attributes(primary_filename, strategy_filename)
merged_data<-merge(run_attributes[,c('primary_id', 'strategy_code', 'future_id')], data, by=c('primary_id'), x.all=TRUE)
data<-merged_data

#clean the data of furnace gas and crude
temp_data_cols<-colnames(data)
cols_to_keep<-temp_data_cols[!grepl('totalvalue.*furnace_gas', temp_data_cols)]
cols_to_keep<-cols_to_keep[!grepl(glob2rx('totalvalue_*_fuel_consumed_*_fuel_crude'), cols_to_keep)]
data = subset(data, select = cols_to_keep )

#add calculation of total TLUs to data
tlu_conversions<-read.csv('../strategy_specific_cb_files/lvst_tlu_conversions.csv')
pop_livestock<-data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, 'future_id', colnames(data)[grep('pop_lvst', colnames(data))])]
pop_livestock<-melt(pop_livestock, id.vars=c('primary_id', 'time_period', 'region', 'strategy_code', 'future_id'))
pop_livestock<-merge(pop_livestock, tlu_conversions, by='variable')
pop_livestock$total_tlu<-pop_livestock$value * pop_livestock$TLU
pop_livestock_summarized<-pop_livestock %>% 
  group_by(primary_id, time_period, region, strategy_code, future_id) %>% #c('primary_id', 'time_period', 'region', 'strategy_code')) %>% 
  summarise(lvst_total_tlu= sum(total_tlu))
data<-merge(data, pop_livestock_summarized, by=c('primary_id', 'time_period', 'region', 'strategy_code', 'future_id')) 

#replace any lagging references to "PFLO:SOCIOTECHNICAL" with "PFLO:CHANGE_CONSUMPTION"
data$strategy_code[data$strategy_code=='PFLO:SOCIOTECHNICAL']<-'PFLO:CHANGE_CONSUMPTION'


SSP_GLOBAL_list_of_strategies<-unique(data$strategy_code)
SSP_GLOBAL_list_of_variables<-setdiff(colnames(data), SSP_GLOBAL_SIMULATION_IDENTIFIERS)

#-------------Get electricity consumption and costs----------

#1. Get total electricity production
electricity_production_variables<-temp_data_cols[grep('nemomod_entc_annual_production_by_technology_pp_', temp_data_cols)]
electricity_loss_variable<-'energylost_enfu_transmission_loss_fuel_electricity_pj'
electricity_production_data<-data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, electricity_production_variables, electricity_loss_variable)]
electricity_production_data$entc_total_annnual_production_before_losses<-rowSums(electricity_production_data[,electricity_production_variables])
electricity_production_data$entc_total_annnual_production <-electricity_production_data$entc_total_annnual_production_before_losses - 
  electricity_production_data$energylost_enfu_transmission_loss_fuel_electricity_pj
electricity_production_data<-electricity_production_data[!colnames(electricity_production_data) %in% electricity_production_variables]

  
#2. Get electricity cost and data
entc_costs<-cb_data[grep('cb:entc', cb_data$variable),]
entc_cost_total<-entc_costs %>% 
  group_by(time_period, region, strategy_code) %>%
  summarise(total_entc_costs= sum(value))

#3. Merge the two datasets
entc_cost_and_production<-merge(entc_cost_total, electricity_production_data, by=c('strategy_code', 'region', 'time_period'))

#4. Calculate cost per PJ
entc_cost_and_production$entc_cost_per_pj <- entc_cost_and_production$total_entc_costs/entc_cost_and_production$entc_total_annnual_production

#6.#get the electricity consumption by sector
electricity_consumption_sector_vars<-temp_data_cols[grep(glob2rx('energy_demand_enfu_subsector_total_pj_*_fuel_electricity'), temp_data_cols)]
electricity_consumption_data<-data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, electricity_consumption_sector_vars)]

#7. Merge with unit cost of PJ and calculate sector costs
entc_sector_consumption_and_cost<-merge(entc_cost_and_production, electricity_consumption_data, by=c('primary_id', 'strategy_code', 'region', 'time_period', 'future_id'))

t<-entc_sector_consumption_and_cost %>% 
  mutate(
    across(all_of(electricity_consumption_sector_vars), ~ .x*entc_cost_per_pj, .names = "entc_cost_{.col}")
  )
entc_sector_consumption_and_cost<-t

#X. Write out the data
write.csv(entc_sector_consumption_and_cost, 'entc_sector_consumption_and_cost.csv')

