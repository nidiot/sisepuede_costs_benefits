#This file reallocates the costs and benefits of the ENTC sector to the consuming sectors
#So far, it does it for electricity but requires the final fixes:
#(1) Need to create consumption for agriculture (qty noted in industry) and waste (2% of total)
#(2) Potentially need to offset production of electricity in ag and waste against these fractions or totals
#(3) Need to check that other costs and benefits in the energy and fuels row are accounted for
#(4) Need to remove electricity from all of the sector specific fuel costs?

#---> alternative: just do a reallocation on the net benefits/costs at the top line based on 
#---> total electricity consumption over time. (i.e., fraction out at the top line)
#---> would still need to remove electricity from the sector specific fuel costs
#---> and have those be on their own, and also have ag and ww as fractions. oof.

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

output.file<-read.csv(data_filename)
cb_data<-read.csv(cb_filename)
cb_data<-cb_data[,-1] #get rid of the X column

#-------------PREPARE THE DATA--------------

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

#-------------REALLOCATE ELECTRICITY COSTS-----------

#1. get the electricity consumption and calculate fractions by sector
electricity_consumption_sector_vars<-temp_data_cols[grep(glob2rx('energy_demand_enfu_subsector_total_pj_*_fuel_electricity'), temp_data_cols)]
electricity_consumption_sector_vars<-electricity_consumption_sector_vars[!grepl('entc', electricity_consumption_sector_vars)]
  
electricity_consumption_data<-data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, electricity_consumption_sector_vars)]
electricity_consumption_data$total_electricity_consumption<-rowSums(electricity_consumption_data[,electricity_consumption_sector_vars])

t<-electricity_consumption_data %>% 
  mutate(
    across(all_of(electricity_consumption_sector_vars), ~ .x/total_electricity_consumption, .names = "frac_electricity_{.col}")
  )
electricity_consumption_data<-t
colnames(electricity_consumption_data)<-str_replace(colnames(electricity_consumption_data), 'frac_electricity_energy_demand_enfu_subsector_total_pj_', 'frac_')
frac_colnames<-colnames(electricity_consumption_data)[grep('frac', colnames(electricity_consumption_data))]
electricity_consumption_data<-electricity_consumption_data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, frac_colnames)]

#2. Get the subset of data related to entc costs
entc_costs<-cb_data[grep('cb:entc', cb_data$variable),]

#3. Merge the fractions with the cost data
merged_data<-merge(entc_costs, electricity_consumption_data, by=c('strategy_code', 'region', 'time_period'))

#4. Multiply out each cost variable to each sector
s<-merged_data %>% 
  mutate(
    across(all_of(frac_colnames), ~ .x*value, .names = "cost_electricity_{.col}")
  )
#exclude columns that say 'frac' but not 'cost'
s<-s[,!(grepl('frac', colnames(s)) & !grepl('cost', colnames(s)))]

#5. Create output: Reshape the data to be long, and rename the variables and replace values, and then subset
data_long<-melt(s, id.vars=c(SSP_GLOBAL_COLNAMES_OF_RESULTS, 'primary_id'))

#replace the newly created columns "variable" and "value" with "_new" suffixes
data_long_cols<-colnames(data_long)
ncol_data_long<-ncol(data_long)
data_long_cols[ncol_data_long]<-'value_new'
data_long_cols[ncol_data_long-1]<-'variable_new'

#rename the variables in the variable column, and set the value to be the new value
colnames(data_long)<-data_long_cols
data_long$sector<-str_replace(data_long$variable_new, 'cost_electricity_frac_', 'entc_allocation_to_')
data_long$sector<-str_replace(data_long$sector, '_fuel_electricity', '')
data_long$variable<-str_replace(data_long$variable, ':entc:', paste0(':', data_long$sector, ':'))
data_long$value<-data_long$value_new

#get the output cols
entc_allocation_data<-data_long[,SSP_GLOBAL_COLNAMES_OF_RESULTS]

