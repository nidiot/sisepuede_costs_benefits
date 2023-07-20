#---------------Combine athena outputs
d.input<-read.csv('/Users/nidhi/Downloads/futures_test/model_input_from_athena.csv')
d.output<-read.csv('/Users/nidhi/Downloads/futures_test/model_output_from_athena.csv')
d<-merge(d.input, d.output, by=c('region', 'time_period', 'primary_id'))
write.csv(d, '/Users/nidhi/Downloads/futures_test/model_input_output_from_athena.csv')

#----------------Get Variables Used in Cost Analysis-------------------

#get the cost factor variables
list_of_cost_factors<-read.csv('../system_cost_factors_list.csv')  
a<-ssp_report_cost_factors(list_of_cost_factors)

#append the variables in the cost definitions
x<-read.csv('../transformation_cost_definitions.csv')
a<-append(a, lapply(x$difference_variable, FUN=glob2rx))
a<-append(a, lapply(x$arg1, FUN=glob2rx))
a<-append(a, lapply(x$arg2, FUN=glob2rx))
a<-append(a, c('frac_ippu_cement_clinker', 'prod_ippu_cement_tonne'))
a<-append(a, c('qty_waso_total_food_produced_tonne', 'qty_agrc_food_produced_lost_sent_to_msw_tonne', 
                 'factor_waso_waste_per_capita_scalar_food', 'factor_waso_waste_per_capita_scalar_food'))

#ppaned the variables explicitly searched for
a<-append(a, SSP_GLOBAL_LOG_OF_SEARCHED_VARS)


#a<-append(a, cols_to_grep)

#get the matching terms
b<-unlist(a)
c<-paste0(b, collapse='|')
d<-grep(c, SSP_GLOBAL_list_of_variables)
e<-SSP_GLOBAL_list_of_variables[d]

#mke it unique
f<-unique(e)
write.csv(f, 'LOG_variables_searched.csv')


#----------------INEN: Trim and reshape large,wide data for Tableau-------------------
#largedata<-read.csv('~/Desktop/LAC Model Results and Visualizations/sisepuede_results_WIDE_scaled.csv')
industry_data<-data
industry_data_colnames<-colnames(industry_data)

#Create the indicators
industry_data$indicator_inen_frac_electrified<-industry_data$energy_consumption_electricity_inen_total/industry_data$energy_consumption_inen_total
industry_data$indicator_inen_frac_hydrogen<-industry_data$energy_demand_enfu_subsector_total_pj_inen_fuel_hydrogen/industry_data$energy_consumption_inen_total
industry_data$indicator_inen_frac_clinker_substituted<-industry_data$frac_ippu_cement_clinker

#get initial emissions factors for fgases
industry_data_fgas_init<-industry_data[industry_data$time_period==5 & industry_data$primary_id==0, c('region', 'ef_ippu_tonne_c2f6_per_tonne_production_electronics')]
colnames(industry_data_fgas_init)[2]<-'ef_ippu_tonne_c2f6_per_tonne_production_electronics_INIT'
industry_data<-merge(industry_data, industry_data_fgas_init, by='region')

industry_data$indicator_inen_frac_fgas_destroyed<-1-(industry_data$ef_ippu_tonne_c2f6_per_tonne_production_electronics/
  industry_data$ef_ippu_tonne_c2f6_per_tonne_production_electronics_INIT)

#get relevant columns
sector_codes<-c('inen', 'ippu')
sector_cols_to_grep<-c(
  'primary_id', 
  '\\bregion\\b', 
  'time_period',
  sector_codes
)
trimmed_data_long_industry<-ssp_trim_reshape(industry_data, sector_cols_to_grep)
write.csv(trimmed_data_long_industry, '~/Desktop/LAC Model Results and Visualizations/sisepuede_results_INDUSTRY.csv')

#----------------Bulidings: Trim and reshape large,wide data for Tableau-------------------
#largedata<-read.csv('~/Desktop/LAC Model Results and Visualizations/sisepuede_results_WIDE_scaled.csv')
buildings_data<-data
buildings_data_colnames<-colnames(buildings_data)

#Create the indicators
#buildings_data$indicator_inen_frac_electrified<-buildings_data$energy_consumption_electricity_inen_total/buildings_data$energy_consumption_inen_total
#buildings_data$indicator_inen_frac_hydrogen<-buildings_data$energy_demand_enfu_subsector_total_pj_inen_fuel_hydrogen/buildings_data$energy_consumption_inen_total
#buildings_data$indicator_inen_frac_clinker_substituted<-buildings_data$frac_ippu_cement_clinker

#get relevant columns
sector_codes<-c('scoe')
sector_cols_to_grep<-c(
  'primary_id', 
  '\\bregion\\b', 
  'time_period',
  sector_codes
)
trimmed_data_long_buildings<-ssp_trim_reshape(buildings_data, sector_cols_to_grep)
write.csv(trimmed_data_long_buildings, '~/Desktop/LAC Model Results and Visualizations/sisepuede_results_buildings.csv')



#----------------ALL: Trim and reshape large,wide data for all Sector-------------------
#largedata<-read.csv('~/Desktop/LAC Model Results and Visualizations/si ')

#appendi helpful vvariables
cols_to_grep<-c(
  'primary_id', 
  '\\bregion\\b', 
  'time_period',
  #    'area_agrc',
  'area_lndu',
  'demand_agrc',
  'demand_lvst',
  'yield_agrc',
  'pop_lvst',
  'emission_co2e_subsector_total',
  'totalvalue_enfu_fuel_consumed',
  glob2rx('energy_consumption_*_total'),
  'energy_demand_enfu_subsector_total_pj_',
  'energy_demand_enfu_total_fuel_'
  #  'qty_waso',
  #  'gasrf_ippu_co2_capture'
  
)
trimmed_data_long<-ssp_trim_reshape(data, cols_to_grep)
write.csv(trimmed_data_long, paste0(path_to_model_results, 'sisepuede_results_TRIMMED_LONG.csv'))

#---------------Reshape the afolu runs or antyhing else from long to wide, using only primary id, time period, region
#> View(futures_definitions)
 afolu_runs<-read.csv("~/Desktop/LAC Model Results and Visualizations/AFOLU Experiments 7.4.23/exp_design_lurf_checks_brazil.csv")
 afolu_runs_long<-  melt(afolu_runs, id.vars=c('primary_id', 'time_period', 'region'))
 write.csv(afolu_runs_long, "~/Desktop/LAC Model Results and Visualizations/AFOLU Experiments 7.4.23/exp_design_lurf_checks_brazil_long.csv")


#----------------Calculate the relative effect of strategies--------
data_all_strat<-read.csv("~/Desktop/LAC Model Results and Visualizations/All Strategies/sisepuede_results_sisepuede_run_2023-06-20T03;12;44.178024_WIDE_INPUTS_OUTPUTS.csv")
primary_all_strat<-"~/Desktop/LAC Model Results and Visualizations/All Strategies/ATTRIBUTE_PRIMARY.csv"
strategy_all_strat<-"~/Desktop/LAC Model Results and Visualizations/All Strategies/ATTRIBUTE_STRATEGY.csv"
run_attributes_all_strat<-ssp_merge_run_attributes(primary_all_strat, strategy_all_strat)

merged_data_all_strat<-merge(run_attributes_all_strat[,c('primary_id', 'strategy_code')], data_all_strat, by=c('primary_id'), x.all=TRUE)
all_strat_strategies<-unique(merged_data_all_strat$strategy_code)

rel_effect_scoe<-ssp_relative_effect_of_strategies(merged_data_all_strat, 
                                                   'BASE', 
                                                   c('SCOE:FUEL_SWITCH_HEAT', 'SCOE:INC_EFFICIENCY_APPLIANCE', 'SOCE:DEC_DEMAND_HEAT' ), 
                                                   'energy_consumption_scoe_total')

rel_effect_inen<-ssp_relative_effect_of_strategies(merged_data_all_strat, 
                                                   'BASE',
                                                   c('INEN:FUEL_SWITCH_HI_HEAT', 'INEN:FUEL_SWITCH_LO_HEAT', 
                                                     'INEN:FUEL_SWITCH_HI_AND_LO_HEAT',
                                                     'INEN:INC_EFFICIENCY_ENERGY', 'IPPU:DEC_DEMAND'),
                                                   'energy_consumption_inen_total')

avg_rel_effect_inen<- rel_effect_inen[rel_effect_inen$time_period>11,] %>%
  group_by(strategy_code.tx) %>% 
  summarise(avg_rel_effect = mean(fraction_of_effect))
