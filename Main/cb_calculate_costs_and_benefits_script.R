#This script will calculate all the costs and benefits defined in 
#a cost_benefit_definitions_main.csv file. 
#To run this script, please update the working directory and the paths in cb_config.R and here

#-------------SET WORKING DIRECTORY AND PATHS----------
setwd('~/Desktop/LAC_Decarb_Git/sisepuede_costs_benefits/Main/')

#Paths to data files
#path_to_model_results<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/sisepuede_summary_results_run_sisepuede_run_2023-09-28T11;36;58.322719/'
#path_to_model_results<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Simulations 10_10/'
#path_to_model_results<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Simulations 7_10/'
#path_to_model_results<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/'
path_to_model_results<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Core Runs 10_14/'

data_filename<-paste0(path_to_model_results, 
                      list.files(path=path_to_model_results, 
                                 pattern = glob2rx('sisepuede_results_WIDE_scaled*'))) #path to model output runs




primary_filename<-paste0(path_to_model_results, 'ATTRIBUTE_PRIMARY.csv') #path to model output primary filename
strategy_filename<-paste0(path_to_model_results, 'ATTRIBUTE_STRATEGY.csv') #path to model output strategy filename



cb_output_filename<-paste0(path_to_model_results,'cost_benefit_results.csv') #path to write detailed results
cb_economywide_filename<-paste0(path_to_model_results,'economy_wide_cost_benefit_results.csv') #path to write results for only variables that are economy wide (no sector specific results)
net_benefit_ghg_output_filename<-paste0(path_to_model_results,'net_benefit_net_ghg.csv') #path to write net benefit and net ghg summaries
cb_futures_output_filename<-paste0(path_to_model_results,'cost_benefits_in_futures.csv') #path to write results for costs adn benefits in each future
trimmed_data_filename<-paste0(path_to_model_results, 'sisepuede_results_TRIMMED_LONG.csv') #path to trimmed data for Tableau


#-------------SOURCE LIBRARIES AND CODE-----
source('cb_config.R')
source('cb_utilities.R')
source('cb_strategy_specific_functions.R')
source('general_ssp_utilities.R')

#-------------READ THE DATA-----------------
output.file<-read.csv(data_filename)

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
cols_to_keep<-cols_to_keep[!grepl(glob2rx('totalvalue_*_fuel_consumed_*_fuel_electricity'), cols_to_keep)] #10.13 ADDED THIS SO SECTOR FUELS EXCLUDE ELECTRICITY
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

#-------------PRODUCE A COPY OF TRIMMED INPUT DATA FOR TABLEAU-------------------

#create a column of outputs
cols_to_grep<-c(
  'primary_id', 
  '\\bregion\\b', 
  'time_period',
  'future_id',
  #    'area_agrc',
  'area_lndu',
  'demand_agrc',
  'demand_lvst',
  'yield_agrc',
  'pop_lvst',
#  'exportsadj_lvst',
#  'exportsadj_agrc',
#  'emission_co2e_subsector_total',
'emission_co2e',
#  'lndu_conversion',
  'totalvalue_enfu_fuel_consumed',
  glob2rx('energy_consumption_*_total'),
  'energy_demand_enfu_subsector_total_pj_',
  'energy_demand_enfu_total_fuel_',
'nemomod_entc_annual_production_by_technology',
'qty_waso'
, 'trns'

)
trimmed_data_long<-ssp_trim_reshape(data, cols_to_grep)
write.csv(trimmed_data_long, trimmed_data_filename)


#-------------REMOVE BASE FOR COST BENEFIT ANALYSIS------------
data<-data[data$strategy_code!= 'BASE',]


#-------------READ DEFINITION FILES IN AND CALCULATE COSTS AND BENEFITS----------

#maps strategies to transformations, from James
#This file tells us which transformation in is in each strategy
strategy2tx<-read.csv('../attribute_strategy_code.csv', check.names = FALSE)

#tells us which strategies to evaluate costs and benefit sfor
strategy_cost_instructions<-read.csv('../strategy_cost_instructions.csv')

#the list of all the cost factor files in the system, and the functions they should be evaluated with
cost_factor_names<-read.csv('../system_cost_factors_list.csv')  

#defines how each transformation is evaluated, including difference variables, cost multipliers, etc.
transformation_cost_definitions<-read.csv('../transformation_cost_definitions.csv')


#calculate system costs
results_system<-cb_calculate_system_costs(data, strategy_cost_instructions, cost_factor_names)

#calcualte transformation costs
results_tx<-cb_calculate_transformation_costs(data, 
                                              strategy_cost_instructions,
                                              strategy2tx, 
                                              transformation_cost_definitions)
#combine the results
results_all<-rbind(results_system, results_tx)

SSP_GLOBAL_list_of_cbvars<-unique(results_all$variable)

#-------------POST PROCESS SIMULATION RESULTS---------------
#Post process interactions among strategies that affect the same variables
postprocess_interactions<-read.csv('../strategy_interaction_definitions.csv')
results_all_pp<-cb_process_interactions(results_all, strategy2tx, postprocess_interactions)

#POST PROCESS TRANSPORT CB ISSUES (These should be addressed properly in the next round of analysis)
#congestion and safety benefits should be 0 in strategies that only make fuel efficiency gains and fuel switching
results_all_pp$value[results_all_pp$strategy_code=='PFLO:BETTER_BASE' & grepl('congestion|road_safety', results_all_pp$variable)==TRUE]<-0
results_all_pp$value[results_all_pp$strategy_code=='PFLO:SUPPLY_SIDE_TECH' & grepl('congestion|road_safety', results_all_pp$variable)==TRUE]<-0

#in ALL, cut the benefits in half to be on the safe side.
results_all_pp$value[results_all_pp$strategy_code=='PFLO:ALL_PLUR' & grepl('congestion|road_safety', results_all_pp$variable)==TRUE]<-0.5 * results_all_pp$value[results_all_pp$strategy_code=='PFLO:ALL_PLUR' & grepl('congestion|road_safety', results_all_pp$variable)==TRUE]
results_all_pp$value[results_all_pp$strategy_code=='PFLO:ALL_NO_STOPPING_DEFORESTATION_PLUR' & grepl('congestion|road_safety', results_all_pp$variable)==TRUE]<-0.5 * results_all_pp$value[results_all_pp$strategy_code=='PFLO:ALL_NO_STOPPING_DEFORESTATION_PLUR' & grepl('congestion|road_safety', results_all_pp$variable)==TRUE]

#POST PROCESS WASO CB ISSUES
#where moving from incineration to landfilling appears to have benefits
#when this move should probably not occur
results_all_pp$value[results_all_pp$strategy_code=='PFLO:SUPPLY_SIDE_TECH' & grepl('cb:waso:technical_cost:waste_management', results_all_pp$variable)==TRUE]<-0


#SHIFT any stray costs incurred from 2015 to 2025 to 2025 and 2035
results_all_pp_before_shift<-results_all_pp #keep copy of earlier results just in case/for comparison
res_pre2025<-results_all_pp[results_all_pp$time_period<SSP_GLOBAL_TIME_PERIOD_TX_START,] #get the subset of early costs
res_pre2025$variable<-paste0(res_pre2025$variable, "_shifted", res_pre2025$time_period+SSP_GLOBAL_TIME_PERIOD_0) #create a new variable so they can be recognized as shifted costs
res_pre2025$time_period<-res_pre2025$time_period+SSP_GLOBAL_TIME_PERIOD_TX_START #shift the time period
results_all_pp<-rbind(results_all_pp, res_pre2025) #paste the results
results_all_pp[results_all_pp$time_period<SSP_GLOBAL_TIME_PERIOD_TX_START,c('value')]<-0 #set pre-2025 costs to 0


#Write
write.csv(results_all_pp, file=cb_output_filename)

#-------------DISCOUNTING AND AGGREGATION---------------


#aggregate net benefit results using a 7% discount rate to get a single file of emisisons nad NPV by country, future, strategy
economy_wide_results<-results_all_pp[!grepl('sector_specific', results_all_pp$variable),]
economy_wide_results<-economy_wide_results[!grepl('waste_to_energy_value', economy_wide_results$variable),]

results_net_benefit_and_ghg_7<-cb_net_benefits_and_emissions(data, economy_wide_results, 0.07)
write.csv(results_net_benefit_and_ghg_7, file=net_benefit_ghg_output_filename)

results_present<-cb_present_value_of_each_cost(economy_wide_results, 0.07)


#Edmundo, replace this line with whatever output writing you want to do
write.csv(results_present, cb_economywide_filename)


#---- EDMUNDO, YOUR FOR LOOP SHOULD END HERE----

#-------------RUN FUTURES---------------------

#Create futures from compressed data
num_futures<-length(unique(results_all_pp$future_id))
futures_definitions<-read.csv('../cost_experimental_design.csv')

#replace this with an arbitrary number if you don't want to match them 1:1 with futures from the simulation
#if you do want to match them, then the number of futures should be one less to allow for a baseline
#with no changes from nominal values
futures<-cb_create_futures(futures_definitions, num_futures-1) 
#futures<-cb_create_futures(futures_definitions, 4) 

#do a quick check to make sure each variable is only affected by one uncertainty
one_future<-futures[futures$cbfuture_id==1,]
unique_vars<-unique(one_future$variable)
length(unique_vars)==length(one_future$variable)
one_future$variable[duplicated(one_future$variable)]

#Remove variables that are used for sector specific calculations
economy_wide_results<-results_all_pp[!grepl('sector_specific', results_all_pp$variable),]
economy_wide_results<-results_all_pp[!grepl('waste_to_energy_value', results_all_pp$variable),]

results_present<-cb_present_value_of_each_cost(economy_wide_results, 0.07)

#THIS IS A TEST TO SHOW HOW THE CODE WORKS WITH ARTIFICIAL FUTURES
rp1<-results_present
rp1$future_id<-1

rp2<-results_present
rp2$future_id<-2

rp3<-results_present
rp3$future_id<-3

rp4<-results_present
rp4$future_id<-4

results_present<-rbind(results_present, rp1, rp2, rp3, rp4)
 #END CREATION OF ARTIFICIAL DATA

results_in_futures<-cb_evaluate_in_futures(results_present, futures, TRUE)
write.csv(results_in_futures, file=cb_futures_output_filename)



