
#set up the variables
tmp_cols<-colnames(data)
entc_sector_consumption_vars<-c(tmp_cols[grep(glob2rx('energy_consumption_electricity_*_total'), tmp_cols)], 'exportsadj_enfu_fuel_electricity')
entc_capex_vars<-tmp_cols[grep('nemomod_entc_discounted_capital_investment', tmp_cols)]
entc_opex_vars<-tmp_cols[grep('nemomod_entc_discounted_operating_costs', tmp_cols)]
entc_fuel_vars<-tmp_cols[grep('totalvalue_enfu_fuel_consumed_entc_', tmp_cols)]
entc_transmission_var<-'prod_enfu_fuel_electricity_pj'

#get the data
electricity_cost_data<-data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, entc_capex_vars, entc_opex_vars, entc_fuel_vars, entc_transmission_var, entc_sector_consumption_vars)]

#calculate the total cost of electricity
electricity_cost_data$entc_transmission_costs<-electricity_cost_data$prod_enfu_fuel_electricity_pj * 0.740000 #cost of transmission per PJ in millions
electricity_cost_data$total_electricity_cost<-rowSums(electricity_cost_data[,c(entc_capex_vars, entc_opex_vars, entc_fuel_vars, 'entc_transmission_costs')])

#get the total amount of electricity consumed and the fraction apportioned to each 
electricity_cost_data$total_electricity_consumption<-rowSums(electricity_cost_data[,entc_sector_consumption_vars])
#apply fractions to and emisisons to each sector
electricity_cost_data<-electricity_cost_data %>% 
  mutate(
    across(all_of(entc_sector_consumption_vars), ~ .x/total_electricity_consumption*total_electricity_cost, .names = "entc_cost_to_{.col}")
  )

#Rename the columns
colnames(electricity_cost_data)[grep('entc_cost_to_exportsadj_enfu_fuel_electricity', colnames(electricity_cost_data))]<-'entc_cost_to_energy_consumption_electricity_export_total'
cols_from_t<-colnames(electricity_cost_data)[grep('entc_cost_to_', colnames(electricity_cost_data))]
sector_codes<-str_remove(cols_from_t, 'entc_cost_to_energy_consumption_electricity_')
sector_codes<-str_remove(sector_codes, '_total')
new_electricity_cost_cols<-paste0('cost_of_electricity_for_', sector_codes)
colnames(electricity_cost_data)[colnames(electricity_cost_data) %in% cols_from_t]<-new_electricity_cost_cols

#Downselect columns for merge
electricity_cost_data_for_merge<-electricity_cost_data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, new_electricity_cost_cols)]
data<-merge(data, electricity_cost_data_for_merge, by=c('primary_id', 'time_period', 'region', 'strategy_code'))

#clean the data of electricity costs
temp_data_cols<-colnames(data)
cols_to_keep<-temp_data_cols[!grepl('^totalvalue_enfu_fuel_consumed_.*_fuel_electricity$', temp_data_cols)]
data = subset(data, select = cols_to_keep )


