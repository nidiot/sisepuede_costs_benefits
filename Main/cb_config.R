#This file is where to configuration parameters

#packages to load
library('purrr')
library('stringr')
library('tidyr')
library('data.table')
library('readxl')
library('dplyr')
library('reshape2')
library('lhs')


#hush dplyr.summarize to avoid the error "`summarise()` has grouped output by 'region', 'time_period'. You can override using the `.groups` argument.")
options(dplyr.summarise.inform = FALSE) 

#paths to git repositories
lac_decarbonization_git_path<-'~/Desktop/LAC_Decarb_Git/lac_decarbonization/'
sisepuede_data_git_path<-'~/Desktop/LAC_Decarb_Git/sisepuede_data/'
ssp_costs_benefits_git_path<-'~/Desktop/LAC_Decarb_Git/sisepuede_costs_benefits/'

#add newlines to csvs for smooth running
system('../cost_factors/append_newlines_to_csvs.sh')
system('../strategy_specific_cb_files/append_newlines_to_csvs.sh')

#set globally the variables for analaysis
SSP_GLOBAL_COLNAMES_OF_RESULTS<-c('strategy_code', 'future_id', 'region', 'time_period', 'difference_variable', 'difference_value', 'variable', 'value')
SSP_GLOBAL_SIMULATION_IDENTIFIERS<-c('primary_id', 'strategy_code', 'region', 'time_period', 'future_id')
SSP_GLOBAL_SIMULATION_LONG_COLS<-c('primary_id', 'strategy_code', 'region', 'time_period', 'variable', 'value')

SSP_GLOBAL_TIME_PERIOD_0<-2015
SSP_GLOBAL_TIME_PERIOD_TX_START<-10 #11
SSP_GLOBAL_TIME_PERIODS<-36
SSP_GLOBAL_TIME_PERIOD_2023<-8 #9
SSP_GLOBAL_COST_YEAR<-4 #cost everything to 2019

SSP_GLOBAL_CCS_CAPTURE_RATE<-0.9

#set globally the variables for output
#0 -- no printing out
#1 -- only transformation names
#2 -- + cost factor or top line variable names
#3 -- + all variables
SSP_PRINT_STRATEGIES<-TRUE
SSP_PRINT_TOP_LINE_VARS<-TRUE
SSP_PRINT_DETAILED_VARS<-TRUE
SSP_PRINT_COST_FACTOR_FILENAMES<-TRUE

#Print to a log all the variables searched for
SSP_GLOBAL_LOG_VARIABLE_SEARCH<-TRUE
SSP_GLOBAL_LOG_OF_SEARCHED_VARS<-list()

SSP_GLOBAL_OUTPUT_NAME_CONVENTION<-c("prefix",	"sector",	"cost_type",	"specification",	"sub_specification")
