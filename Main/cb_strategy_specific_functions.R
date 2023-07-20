# This file contains cost and benefit functions for 
# transformation specific cost and benefit calculations
# that aren't handled by one of the general cost-benefit utility functions




#---------------Manure Management
cb_manure_management_cost<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                               output_mults, change_in_multiplier, country_specific_multiplier,
                               scale_impact, scaling, list_of_variables){
  time_period<-0:(SSP_GLOBAL_TIME_PERIODS-1)
  implementation<-c(c(1:11)*0, seq(0, 0.95, length.out = 25))
  manure_imp<-data.frame(time_period, implementation)
  tlus<-cb_get_data_from_wide_to_long(data, strategy_code_tx, diff_var)
  tlus<-merge(tlus, manure_imp)
  tlus$difference_variable<-diff_var
  tlus$difference_value<-tlus$value*tlus$implementation
  tlus$value<-tlus$difference_value * output_mults
  tlus$variable<-output_vars
  
  tlus<-tlus[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  return(tlus)
  
  
  
}


#---------------System fuel costs
cb_system_fuel_costs<-function(data, definition, fuel_cost_factors, list_of_variables){
  
  run_identifiers<-SSP_GLOBAL_SIMULATION_IDENTIFIERS
  orig_vars<-SSP_GLOBAL_list_of_variables
  
  c1<-orig_vars[grepl('^totalvalue_enfu_fuel_consumed_.*_fuel_electricity$', orig_vars)] #Skip electricity costs because they are duplicative of other costs
  c2<-orig_vars[grepl('totalvalue.*furnace_gas', orig_vars)] #Skip furnace gas because it is crazy buggy and extraneous
  exclude_list<-c(c1, 
                  c2, 
                  'totalvalue_enfu_fuel_consumed_entc_fuel_crude' #Skip crude because it is duplicative
  )
  
  data_columns<-colnames(data)
  cols_to_keep<-data_columns[!grepl(paste(exclude_list, collapse="|"), data_columns)]
  data_cleaned<-data[,cols_to_keep]
  list_of_variables<-setdiff(cols_to_keep, SSP_GLOBAL_SIMULATION_IDENTIFIERS)

  results<-cb_apply_cost_factors(data_cleaned, definition, fuel_cost_factors, list_of_variables)
  return(results)
}

#--------------IPPU: CCS ------------------
cb_ippu_inen_ccs<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                               output_mults, change_in_multiplier, country_specific_multiplier,
                               scale_impact, scaling, list_of_variables){
  
  #get the fraction reductions in CO2
  ccs_fraction_vars<-SSP_GLOBAL_list_of_variables[grep('frac_ippu_production_with_co2_capture_', SSP_GLOBAL_list_of_variables)]
  ccs_fractions<-cb_get_data_from_wide_to_long(data, strategy_code_tx, ccs_fraction_vars)
  
  #given the global capture rate, update the applicaiton fraction
  ccs_fractions$application_rate<-ccs_fractions$value
  
  #get the quantities of production for those variables
  production_vars<-paste0(str_replace(ccs_fraction_vars, 'frac_ippu_production_with_co2_capture_', 'prod_ippu_'), '_tonne')
  ccs_fractions$variable<-paste0(str_replace(ccs_fractions$variable, 'frac_ippu_production_with_co2_capture_', 'prod_ippu_'), '_tonne')
  prod_qty<-cb_get_data_from_wide_to_long(data, strategy_code_tx, production_vars)
  
  #merge the two datasets
  data_merged<-merge(ccs_fractions, prod_qty, by=c('region', 'strategy_code', 'time_period', 'variable'),suffixes=c('ccs', ''))
  
  #multiply the production quantity by te fractions
  data_merged$difference_value<-data_merged$application_rate * data_merged$value
  data_merged$difference_variable<-data_merged$variable
  
  #read the cost definitions
  ccs_cost_factor<-read.csv(paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/ippu_ccs_cost_factors.csv'))
  data_merged<-merge(data_merged, ccs_cost_factor, by='variable')
  data_merged$value<-data_merged$difference_value * data_merged$multiplier
  data_merged$variable<-data_merged$output_variable_name
  
  data_merged<-data_merged[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  return(data_merged)
}


cb_ippu_inen_ccs_old<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                       output_mults, change_in_multiplier, country_specific_multiplier,
                       scale_impact, scaling, list_of_variables){
  
  #get the fraction reductions in CO2
  ccs_fraction_vars<-SSP_GLOBAL_list_of_variables[grep('gasrf_ippu_co2_capture', SSP_GLOBAL_list_of_variables)]
  ccs_fractions<-cb_get_data_from_wide_to_long(data, strategy_code_tx, ccs_fraction_vars)
  
  #given the global capture rate, update the applicaiton fraction
  ccs_fractions$application_rate<-ccs_fractions$value/SSP_GLOBAL_CCS_CAPTURE_RATE
  
  #get the quantities of production for those variables
  production_vars<-paste0(str_replace(ccs_vars, 'gasrf_ippu_co2_capture_', 'prod_ippu_'), '_tonne')
  ccs_fractions$variable<-paste0(str_replace(ccs_fractions$variable, 'gasrf_ippu_co2_capture_', 'prod_ippu_'), '_tonne')
  prod_qty<-cb_get_data_from_wide_to_long(data, strategy_code_tx, production_vars)
  
  #merge the two datasets
  data_merged<-merge(ccs_fractions, prod_qty, by=c('region', 'strategy_code', 'time_period', 'variable'),suffixes=c('ccs', ''))
  
  #multiply the production quantity by te fractions
  data_merged$difference_value<-data_merged$application_rate * data_merged$value
  data_merged$difference_variable<-data_merged$variable
  
  #read the cost definitions
  ccs_cost_factor<-read.csv(paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/ippu_ccs_cost_factors.csv'))
  data_merged<-merge(data_merged, ccs_cost_factor, by='variable')
  data_merged$value<-data_merged$difference_value * data_merged$multiplier
  data_merged$variable<-data_merged$output_variable_name
  
  data_merged<-data_merged[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  return(data_merged)
}

#--------------FGTV: ALL COSTS ------------
cb_fgtv_abatement_costs<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                        output_mults, change_in_multiplier, country_specific_multiplier,
                        scale_impact, scaling, list_of_variables){
  
  energy_vars<-c('energy_demand_enfu_total_fuel_coal', 'energy_demand_enfu_total_fuel_oil', 'energy_demand_enfu_total_fuel_natural_gas')
  fgtv_vars<-SSP_GLOBAL_list_of_variables[grep(glob2rx('emission_co2e_*_fgtv_fuel_*'), SSP_GLOBAL_list_of_variables)]
  
  #1. Get the fugitive emissions per PJ of coal and oil together in the baseline
  energy<-cb_get_data_from_wide_to_long(data, strategy_code_base, energy_vars)
  energy$fuel<-str_replace(energy$variable, 'energy_demand_enfu_total_', '')
  fgtv<-cb_get_data_from_wide_to_long(data, strategy_code_base, fgtv_vars)
  fgtv$fuel<-str_replace(fgtv$variable, glob2rx('emission_co2e_*_fgtv_*'), '')
  
  #1.a summarize the emissions by fuel
  fgtv<-fgtv %>%
    group_by(primary_id, region, time_period, strategy_code, fuel) %>%
    summarize(value = sum(value))
  
  
  data_merged_base<-merge(energy, fgtv, by=c('region', 'time_period', 'strategy_code', 'fuel'), suffixes=c('.en_base', '.fg_base'))
    
  #2. Get the fugitive emissions per PJ of coal and oil together in the transformed future
  energy_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, energy_vars)
  energy_tx$fuel<-str_replace(energy_tx$variable, 'energy_demand_enfu_total_', '')
  fgtv_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, fgtv_vars)
  fgtv_tx$fuel<-str_replace(fgtv_tx$variable, glob2rx('emission_co2e_*_fgtv_*'), '')
  data_merged_tx<-merge(energy_tx, fgtv_tx, by=c('region', 'time_period', 'strategy_code', 'fuel'), suffixes=c('.en_tx', '.fg_tx'))
  
  #2.b summarize the emissions by fuel
  fgtv_tx<-fgtv_tx %>%
    group_by(primary_id, region, time_period, strategy_code, fuel) %>%
    summarize(value = sum(value))
  
  #3. Merge the two together
  data_merged<-merge(data_merged_tx, data_merged_base, by=c('region', 'time_period', 'fuel'), suffixes=c('.tx', '.base'))
  
  #4. Calculate the fugitive emissions per unit demand in the baseline and apply it to the transformed future
  data_merged$fgtv_co2e_per_demand_base<-data_merged$value.fg_base/data_merged$value.en_base
  data_merged$fgtv_co2e_expected_per_demand<-data_merged$value.en_tx*data_merged$fgtv_co2e_per_demand_base
  
  #5. Calculate the difference between observed and expected demand
  data_merged$difference_value<-data_merged$value.fg_tx - data_merged$fgtv_co2e_expected_per_demand
  data_merged$difference_variable<-data_merged$variable.fg_tx
    
  #6. Apply the multiplier
  data_merged$value<-data_merged$difference_value*output_mults
  data_merged$variable<-output_vars
  
  #7. Get columns
  data_merged$strategy_code<-data_merged$strategy_code.tx
  data_merged<-data_merged[, SSP_GLOBAL_COLNAMES_OF_RESULTS]
  return(data_merged)

}

#--------------LNDU: SOIL CARBON/CONSERVATION AGRICULTURE------------

cb_lndu_soil_carbon<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                     output_mults, change_in_multiplier, country_specific_multiplier,
                                     scale_impact, scaling, list_of_variables){
  
  #To run as a script
  # strategy_code_tx<-'LNDU:DEC_SOC_LOSS'
  # strategy_code_base<-'BASE'
  # output_vars<-paste0('soil_carbon_cost_or_benefit')
  # diff_var<-'area_agrc_crops_cereals'
  # output_mults<-41
  
  #get the beginning and ending fractions of acres under soil conservation
  #create transformations as time series per country
  ca_fracs<-read.csv(paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/LNDU_soil_carbon_fractions.csv'))
  time_period<-c(0:(SSP_GLOBAL_TIME_PERIODS-1))
  a<-expand.grid(ca_fracs$region, time_period)
  colnames(a)<-c('region', 'time_period')
  ca_fracs<-merge(ca_fracs, a, by='region')             
  ca_fracs$gains_in_ca<-ca_fracs$end_val - ca_fracs$start_val
  ca_fracs$frac_in_year<-ca_fracs$gains_in_ca/(SSP_GLOBAL_TIME_PERIODS - SSP_GLOBAL_TIME_PERIOD_TX_START +1) * 
    (ca_fracs$time_period - SSP_GLOBAL_TIME_PERIOD_TX_START+1)
  ca_fracs$frac_in_year[ca_fracs$time_period >=0 & 
                                          ca_fracs$time_period<=SSP_GLOBAL_TIME_PERIOD_TX_START-1]<-0
  ca_fracs$frac_in_year<-ca_fracs$frac_in_year+ca_fracs$start_val
  
  #calculate the number of acres under CA in the transformation and in baseline
  #ca_data_tx<-subset(data, strategy_code==strategy_code_tx & variable==diff_var)
  ca_data_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, diff_var)
  
  ca_data_tx<-merge(ca_data_tx, ca_fracs, by=c('region', 'time_period'))
  ca_data_tx$acres_ca<-ca_data_tx$value*ca_data_tx$frac_in_year
  
  #ca_data_base<-subset(data, strategy_code==strategy_code_base & variable==diff_var)
  ca_data_base<-cb_get_data_from_wide_to_long(data, strategy_code_base, diff_var)
  ca_data_base<-merge(ca_data_base, ca_fracs, by=c('region', 'time_period'))
  ca_data_base$acres_ca<-ca_data_base$value*ca_data_base$start_val
    
  ca_data_merged<-merge(ca_data_tx, ca_data_base, by=c('region', 'time_period'), suffixes = c('', '.base'))
  ca_data_merged$difference_variable<-'diff_additional_acres_under_soil_management'
  ca_data_merged$difference_value<-ca_data_merged$acres_ca-ca_data_merged$acres_ca.base
  ca_data_merged$zeros<-0
  ca_data_merged$difference_value<-pmax(ca_data_merged$difference_value, ca_data_merged$zeros)
  
  #for any extra acreage, apply multipliers
  #apply multipliers to those acres
  ca_data_merged$value<-ca_data_merged$difference_value*output_mults
  ca_data_merged$variable<-output_vars
  
  #get the relevant columns
  ca_data_merged<-ca_data_merged[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  return(ca_data_merged)
}

#--------------PFLO:BETTER DIETS------------
#calculate the number of additional people using better diets
#for each such person, there is a $370 cost savings in groceries and 
#$1000/yr cost savings in health



cb_pflo_healthier_diets<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                     output_mults, change_in_multiplier, country_specific_multiplier,
                                     scale_impact, scaling, list_of_variables){
  
  #To run as a script
  #strategy_code_tx<-'PFLO:BETTER_DIETS'
  #output_vars<-paste0('Health benefits of better diets')
  #output_mults<-1000
  
  #Get the population
  #population<-subset(data, strategy_code==strategy_code_tx & variable %in% c('population_gnrl_rural', 'population_gnrl_urban'))
  population<-cb_get_data_from_wide_to_long(data, strategy_code_tx, c('population_gnrl_rural', 'population_gnrl_urban'))
  total_pop<-population %>%
    group_by(primary_id, region, time_period, strategy_code, future_id) %>%
    summarize(total_pop = sum(value))
  
  #get the file with popualtion fractions
  diet_frac<-read.csv(paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/PFLO_transition_to_new_diets.csv'))
  
  
  data_merged<-merge(total_pop, diet_frac, by='time_period')
  data_merged$difference_value<-data_merged$total_pop*(1-data_merged$frac_gnrl_w_original_diet)
  data_merged$difference_variable<-'pop_with_better_diet'
  data_merged$variable<-output_vars
  data_merged$value<-data_merged$difference_value * output_mults
  
  data_merged<-data_merged[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  return(data_merged)
}


#----------AGRCLVST:Productivity----------
#the economic cost of increasing productivity is equal to
#some percent of GDP defined in file
cb_agrc_lvst_productivity<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                                       output_mults, change_in_multiplier, country_specific_multiplier,
                                                       scale_impact, scaling, list_of_variables){
  
  #To run as script
  #strategy_code_tx<-'AGRC:INC_PRODUCTIVITY' #or #LVST:INC_PRODUCTIVITY
  #output_vars<-paste0('Cost of increasing productivity', (strategy_code_tx))
  
  #Get the gdp data
  #gdp<-data[data$strategy_code==strategy_code_tx & data$variable=='gdp_mmm_usd', ]
  gdp<-cb_get_data_from_wide_to_long(data, strategy_code_tx, 'gdp_mmm_usd')
  
  #Get the fractions for each country
  gdp_fracs<-read.csv(paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/AGRC_LVST_productivity_cost_gdp.csv'))
  colnames(gdp_fracs)<-c("ISO3", "cost_frac")
  
  
  #country codes
  path_to_country_codes<-paste0(sisepuede_data_git_path, 'Energy/nemomod_entc_residual_capacity_pp_gas_gw/raw_data/iso3_all_countries.csv')
  country_codes<-read.csv(path_to_country_codes)
  gdp_fracs<-merge(gdp_fracs, country_codes, by=c('ISO3'))
  colnames(gdp_fracs)[colnames(gdp_fracs)=='REGION']<-'region'
  gdp_fracs<-gdp_fracs[,c('region', 'cost_frac')]
  
  #merge wiht gdp
  gdp<-merge(gdp, gdp_fracs, by='region')
  
  gdp$difference_variable<-'diff_fraction_of_GDP_for_productivity'
  gdp$difference_value<-gdp$cost_frac
  gdp$variable<-output_vars
  gdp$value<-gdp$value*10^9*gdp$cost_frac/2*(-1)
  gdp$value[gdp$time_period<SSP_GLOBAL_TIME_PERIOD_TX_START]<-0
  
  gdp<-gdp[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  return(gdp)
}



#----------AGRC:RICE------------
cb_agrc_rice_mgmt<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                            output_mults, change_in_multiplier, country_specific_multiplier,
                            scale_impact, scaling, list_of_variables){
  
  
  #define the transformation as the fraction of acres receiivng better rice management
  tx_definition<-read.csv(paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/AGRC_rice_mgmt_tx.csv'))
  tx_definition$level_of_implementation<-(1-tx_definition$ef_agrc_anaerobicdom_rice_kg_ch4_ha)/0.45
  
  #set vars if we want this to run as a script
  #strategy_code_tx<-'AGRC:DEC_CH4_RICE'
  #diff_var<-'area_agrc_crops_rice'
  #output_vars<-'cost_of_rice_mgmt'
  #output_mults<-(-31)
  
  #rice_management_data<-data[data$strategy_code==strategy_code_tx & data$variable==diff_var, ]
  rice_management_data<-cb_get_data_from_wide_to_long(data, strategy_code_tx, diff_var)
  
  #merge with transformation
  rice_management_data<-merge(rice_management_data, tx_definition, by='time_period')
  
  rice_management_data$difference_variable<-diff_var #paste0('diff_', diff_var)
  rice_management_data$difference_value<-rice_management_data$value*rice_management_data$level_of_implementation
  rice_management_data$variable<-output_vars
  rice_management_data$value<-rice_management_data$difference_value*output_mults
  rice_management_data<-rice_management_data[, SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  
  return(rice_management_data)
}


#----------LVST: ENTERIC FERMENTATION------------------
cb_lvst_enteric<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                          output_mults, change_in_multiplier, country_specific_multiplier,
                          scale_impact, scaling, list_of_variables){
  
  #define the strategy as the fractino of livestock receivving this intervention in a particular year
  tx_definition<-read.csv(paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/LVST_enteric_fermentation_tx.csv'))
  affected_livestock<-tx_definition[tx_definition$application>0,]
  timesteps<-seq(0:35)-1
  a<-expand.grid(affected_livestock$variable,timesteps)
  colnames(a)<-c('variable', 'time_period')
  enteric_pop_fracs<-merge(affected_livestock, a, by='variable')

  enteric_pop_fracs$application_in_year<-enteric_pop_fracs$application/(SSP_GLOBAL_TIME_PERIODS - SSP_GLOBAL_TIME_PERIOD_TX_START) * 
    (enteric_pop_fracs$time_period - SSP_GLOBAL_TIME_PERIOD_TX_START+1)
  enteric_pop_fracs$application_in_year[enteric_pop_fracs$time_period >=0 & 
                                          enteric_pop_fracs$time_period<=SSP_GLOBAL_TIME_PERIOD_TX_START-1]<-0
  
  #apply that to the data
  #data_num_livestock<-subset(data, strategy_code==strategy_code_tx & variable %in% affected_livestock$variable)
  data_num_livestock<-cb_get_data_from_wide_to_long(data, strategy_code_tx, affected_livestock$variable)
  
  data_merged<-merge(data_num_livestock, enteric_pop_fracs, by=c('variable', 'time_period'))
  data_merged$difference_variable<-data_merged$variable #paste0('diff_', data_merged$variable, '_affected')
  data_merged$difference_value<-data_merged$value*data_merged$application_in_year
  data_merged$variable<-paste0(output_vars, data_merged$variable)
  data_merged$value<-data_merged$difference_value*output_mults
  data_merged<-data_merged[, SSP_GLOBAL_COLNAMES_OF_RESULTS]
}


#----------WASO:WASTE REDUCTION TECHNICAL COSTS------------------

# This function calculates consumer food waste avoided, which includes everythign after
#the retailer. From james:
#  consumer_food_waste_avoided = (qty_waso_total_food_produced_tonne - 
#qty_agrc_food_produced_lost_sent_to_msw_tonne) * 
#  (1 - factor_waso_waste_per_capita_scalar_food)/factor_waso_waste_per_capita_scalar_food

cb_waso_reduce_consumer_facing_food_waste<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                                    output_mults, change_in_multiplier, country_specific_multiplier,
                                                    scale_impact, scaling, list_of_variables){
  
  cols_required<-c('qty_waso_total_food_produced_tonne', 'qty_agrc_food_produced_lost_sent_to_msw_tonne', 
                   'factor_waso_waste_per_capita_scalar_food', 'factor_waso_waste_per_capita_scalar_food')
  
  food_waste_data<-data[data$strategy_code==strategy_code_tx, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS,cols_required)]
  
  #Get teh consumer food waste amount
  food_waste_data$consumer_food_waste<-(food_waste_data$qty_waso_total_food_produced_tonne) 
                                        #KLUDGE 07.06/2023
  #UNCOMMENT THIS LINE WHEN JAMES FIXES WHAT 'qty_waso_total_food_produced_tonne' means
                                        #Because of a bug, this is already consumer food waste.
                                       # - food_waste_data$qty_agrc_food_produced_lost_sent_to_msw_tonne)
  
  #Get how much would have been there
  food_waste_data$consumer_food_waste_counterfactual<-food_waste_data$consumer_food_waste/
    food_waste_data$factor_waso_waste_per_capita_scalar_food
  
  #get the difference, whic his hte avoided amount
  food_waste_data$consumer_food_waste_avoided<-food_waste_data$consumer_food_waste -
    food_waste_data$consumer_food_waste_counterfactual
  
  
  food_waste_data$consumer_food_waste_avoided2<-(food_waste_data$qty_waso_total_food_produced_tonne -
                                                food_waste_data$qty_agrc_food_produced_lost_sent_to_msw_tonne) *
    (1-food_waste_data$factor_waso_waste_per_capita_scalar_food)/ food_waste_data$factor_waso_waste_per_capita_scalar_food
  
  food_waste_to_merge<-food_waste_data[, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, 'consumer_food_waste_avoided')]
  
  outputs<-cb_get_data_from_wide_to_long(data, strategy_code_tx, 'qty_waso_total_food_produced_tonne')
  
  merged_data<-merge(outputs, food_waste_to_merge, by=c('strategy_code', 'region', 'time_period'), suffixes=c('', '.food'))
  
  merged_data$difference_variable<-'qty_consumer_food_waste_avoided'
  merged_data$difference_value<-merged_data$consumer_food_waste_avoided
  merged_data$variable<-output_vars
  merged_data$value<-merged_data$difference_value * output_mults
  
  merged_data<-merged_data[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  return(merged_data)
}


#this function just calls the usual difference function but with different multipliers
#depending on food or not food
cb_waso_waste_reduction_technical_costs<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                                  output_mults, change_in_multiplier, country_specific_multiplier,
                                                  frac_var, invert, list_of_variables){
  
  if (grepl('food', diff_var)){
    if (grepl('savings', output_vars)){
      output_mults<-output_mults*12.4 #food is 12.4* more valuable to save
    }else{
      output_mults<-output_mults/10 #food is 10x cheaper to save
    }
    
  }else{
    output_mults<-output_mults
  }
  
   r<-cb_difference_between_two_strategies(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                           output_mults, change_in_multiplier, country_specific_multiplier,
                                           scale_impact, scaling, list_of_variables)
  
  # r<-cb_fraction_change(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
  #                                         output_mults, change_in_multiplier, country_specific_multiplier,
  #                                         frac_var, invert, list_of_variables)

  return(r)
  
}

#----------WALI:SANITATION COSTS AND BENEFITS------------------
cb_wali_sanitation_costs<-function(data, definition, sanitation_cost_factors, list_of_variables){
  
  #To test locally
  #strategy_code_tx<-'WALI:INC_TREATMENT_RURAL'
  #strategy_code_base<-'BASE'
  
  sanitation_classification<-read.csv(paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/WALI_sanitation_classification_strategy_specific_function.csv'))
  
  #Calculate the number of people in each sanitation pathway by merging the data with the sanitation classification
  #and with the population data and keepign onyl rows where the population_variable matches the variable.pop
  #then multiply the fraction by the population
  #There was concern that we need to account for differences in ww production between urban and rural
  #But we don't since the pathway fractions for them are mutually exclusive! Hooray!
  
  #data_strategy<-subset(data, strategy_code==definition$strategy_code & variable %in% sanitation_classification$variable)
  #data_strategy_wide<-data[data$strategy_code==definition$strategy_code, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, sanitation_classification$variable)]
  #data_strategy<-melt(data_strategy_wide, id.vars=SSP_GLOBAL_SIMULATION_IDENTIFIERS)
  data_strategy<-cb_get_data_from_wide_to_long(data, definition$strategy_code, sanitation_classification$variable)
  data_strategy<-merge(data_strategy, sanitation_classification, by='variable')
  
  #population_wide<-data[data$strategy_code==definition$strategy_code, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, 'population_gnrl_rural', 'population_gnrl_urban')]
  #population<-melt(population_wide, id.vars = SSP_GLOBAL_SIMULATION_IDENTIFIERS)
  population<-cb_get_data_from_wide_to_long(data, definition$strategy_code, c('population_gnrl_rural', 'population_gnrl_urban'))
  data_strategy<-merge(data_strategy, population, by=c('region', 'time_period', 'future_id'), suffixes = c("", ".pop"))
  data_strategy<-data_strategy[data_strategy$population_variable==data_strategy$variable.pop,]
  data_strategy$pop_in_pathway<-data_strategy$value*data_strategy$value.pop
  
  #Do the same thing with the baseline strategy
  #data_base<-subset(data, strategy_code==definition$comparison_code & variable %in% sanitation_classification$variable)
  #data_base_wide<-data[data$strategy_code==definition$comparison_code, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, sanitation_classification$variable)]
  #data_base<-melt(data_base_wide, id.vars=SSP_GLOBAL_SIMULATION_IDENTIFIERS)
  data_base<-cb_get_data_from_wide_to_long(data, definition$comparison_code, sanitation_classification$variable)
  data_base<-merge(data_base, sanitation_classification, by='variable')
  
  #population_wide_base<-data[data$strategy_code==definition$comparison_code, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, 'population_gnrl_rural', 'population_gnrl_urban')]
  #population_base<-melt(population_wide_base, id.vars = SSP_GLOBAL_SIMULATION_IDENTIFIERS)
  population_base<-cb_get_data_from_wide_to_long(data, definition$comparison_code, c('population_gnrl_rural', 'population_gnrl_urban'))
  
  data_base<-merge(data_base, population_base, by=c('region', 'time_period', 'future_id'), suffixes = c("", ".pop"))
  data_base<-data_base[data_base$population_variable==data_base$variable.pop,]
  data_base$pop_in_pathway<-data_base$value*data_base$value.pop
  
  data_new<-rbind(data_strategy, data_base)
  
  #reduce it by the sanitation category
  data_new_summarized<-data_new %>% 
    group_by(primary_id, region, time_period, strategy_code, future_id, difference_variable) %>% 
    summarise(value = sum(pop_in_pathway))
  colnames(data_new_summarized)[colnames(data_new_summarized)=='difference_variable']<-'variable'
  
  #now, being geniuses, we can run this using our cb_cost_factors function!
  new_definition<-data.frame(definition$strategy_code, definition$comparison_code)
  colnames(new_definition)<-c('strategy_code', 'comparison_code') 
  new_list_of_variables<-unique(data_new_summarized$variable)
  
  data_new_summarized_wide<-spread(data_new_summarized, variable, value)
  results<-cb_apply_cost_factors(data_new_summarized_wide, new_definition, sanitation_cost_factors, new_list_of_variables)
  
  #and use this new data to calculate benefits of sanitation as well
  sanitation_benefits_factors<-read.csv(paste0(ssp_costs_benefits_git_path, 'cost_factors/wali_benefit_of_sanitation_cost_factors.csv'))
  results_benefits<-cb_apply_cost_factors(data_new_summarized_wide, new_definition, sanitation_benefits_factors, new_list_of_variables)
  
  results<-rbind(results, results_benefits)
  
  return(results)
  
}

#----------IPPU:CLINKER------------------
cb_ippu_clinker<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                          output_mults, change_in_multiplier, country_specific_multiplier,
                          scale_impact, scaling, list_of_variables){
  
  
  #get the clinker fraction data
  #data_frac_clinker_tx<-subset(data, strategy_code==strategy_code_tx & variable == 'frac_ippu_cement_clinker')
  #data_frac_clinker_base<-subset(data, strategy_code==strategy_code_base & variable == 'frac_ippu_cement_clinker')
  
  #get the difference in 
  
  diff_clinker<-cb_difference_between_two_strategies(data, strategy_code_tx, strategy_code_base, 'frac_ippu_cement_clinker', output_vars, 
                                                     1, change_in_multiplier, country_specific_multiplier,
                                                     scale_impact, scaling, list_of_variables)
  
  #data_amt_cement<-subset(data, strategy_code==strategy_code_tx & variable == 'prod_ippu_cement_tonne')
  data_amt_cement<-cb_get_data_from_wide_to_long(data, strategy_code_tx, 'prod_ippu_cement_tonne')
  
  data_merged<-merge(diff_clinker, data_amt_cement, by = c('region', 'time_period'), suffixes = c("", ".cement"))
  
  data_merged$difference_value<-data_merged$value.cement/(1-data_merged$difference_value) - data_merged$value.cement
  data_merged$value<-data_merged$difference_value*output_mults
  
  output_col_names<-colnames(diff_clinker)
  data_output<-data_merged[,output_col_names]
  return(data_output)
  
}


#----------IPPU:FGASES-------------------
cb_ippu_florinated_gases<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                   output_mults, change_in_multiplier, country_specific_multiplier,
                                   scale_impact, scaling, list_of_variables){
  #get all the variables with florinated gases
  #use nomenclature "emission_co2e_NAMEOFGAS_ippu_" where name of gas contains an "f"
  emissions_vars<-list_of_variables[grep('emission_co2e_', list_of_variables)]
  exclude<-"_co2_|_n2o_|_ch4_|_subsector_"
  fgases<-emissions_vars[!str_detect(emissions_vars, exclude)]
  
  #sum up for both strategies
  #data_strategy<-subset(data, strategy_code==strategy_code_tx & variable %in% fgases)
  data_strategy<-cb_get_data_from_wide_to_long(data, strategy_code_tx, fgases)
  
  data_strategy_summarized<-data_strategy %>% 
    group_by(region, time_period, strategy_code) %>% 
    summarise(difference_value = sum(value))
  data_strategy_summarized$difference_variable<-'emission_co2e_all_fgases_ippu'
  data_strategy_summarized$variable<-output_vars
  
  #data_strategy_base<-subset(data, strategy_code==strategy_code_base & variable %in% fgases)
  data_strategy_base<-cb_get_data_from_wide_to_long(data, strategy_code_base, fgases)
  data_strategy_base_summarized<-data_strategy_base %>% 
    group_by(region, time_period, strategy_code) %>% 
    summarise(difference_value = sum(value))
  data_strategy_base_summarized$difference_variable<-'emission_co2e_all_fgases_ippu'
  data_strategy_base_summarized$variable<-output_vars
  
  #take difference and multiply by cost / CO2e
  data_fgases_merged<-merge(data_strategy_summarized, data_strategy_base_summarized, by=c('region', 'time_period'), suffixes = c("", ".base"))
  data_fgases_merged$difference_value<-data_fgases_merged$difference_value - data_fgases_merged$difference_value.base
  data_fgases_merged$value<-data_fgases_merged$difference_value * output_mults
  
  data_fgases_merged<-data_fgases_merged[, c("region","time_period","strategy_code",
                                             "difference_variable", "difference_value","variable","value")]
  
  #return result
  return(data_fgases_merged)
}

#----------ENTC:REDUCE_LOSSES: Technical cost of maintaining grid ----------

cb_entc_reduce_losses<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                output_mults, change_in_multiplier, country_specific_multiplier,
                                scale_impact, scaling, list_of_variables){
  
  #get the loss file
  path_to_loss_reduction_costs<-paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/ENTC_REDUCE_LOSSES_cost_file.xlsx')
  cb_transmission_loss_costs<-read_xlsx(path_to_loss_reduction_costs, sheet = "Annual Loss Reduction Cost")
  
  #map ISO3 to the reigons
  path_to_country_codes<-paste0(sisepuede_data_git_path, 'Energy/nemomod_entc_residual_capacity_pp_gas_gw/raw_data/iso3_all_countries.csv')
  country_codes<-read.csv(path_to_country_codes)
  cb_transmission_loss_costs<-merge(cb_transmission_loss_costs, country_codes, by=c('ISO3'))
  colnames(cb_transmission_loss_costs)[colnames(cb_transmission_loss_costs)=='REGION']<-'region'
  
  #assign fixed losses to each country from 2025 to 2050
  data_strategy<-cb_get_data_from_wide_to_long(data, strategy_code_tx, diff_var)
  data_output<-merge(data_strategy, cb_transmission_loss_costs, by=c('region'), )
  data_output$variable<-output_vars
  data_output$value<-data_output$annual_investment_USD
  data_output$difference_variable<-'N/A (constant annual cost)'
  data_output$difference_value<-data_output$annual_investment_USD
  
  data_output <- data_output[, SSP_GLOBAL_COLNAMES_OF_RESULTS] #select = -c(primary_id, ISO3, Country, annual_investment_USD, Code, Category.Name))
  
  return(data_output)
  
}

# #----------TRNS:DEC_DEMAND: Economic impact of reducing transport demand----------
# #the economic effect of reduced transportation demand is equal to
# #the transport-driven GDP (from input output tables)
# #and the fraction of that transport that is avoided relative to a baseline
# #THIS WILL NOT WORK BECAUSE DO.CALL IS PASSING OTHER ARGUMENTS
# #NOT FIXING BECAUSE WE AREN"T USING THIS TRANSFORMATON RIGHT NOW
# cb_economic_effect_of_reduced_transport_demand<-function(data, definition){
#   
#   #Get the LAC gdp data and format it for time_period, region, and gdp columns
#   path_to_gdp_data<-paste0(sisepuede_data_git_path, 'SocioEconomic/gdp_mmm_usd/input_to_sisepuede/projected/gdp_mmm_usd.csv')
#   gdp_data<-read.csv(path_to_gdp_data)
#   lac_iso3_codes<-ssp_LAC_iso3()
#   gdp_data<-gdp_data[gdp_data$iso_code3 %in% lac_iso3_codes, ]
#   
#   #time period
#   gdp_data$time_period<-gdp_data$Year-2015
#   
#   #country codes
#   colnames(gdp_data)[1]<-'ISO3'
#   path_to_country_codes<-paste0(sisepuede_data_git_path, 'Energy/nemomod_entc_residual_capacity_pp_gas_gw/raw_data/iso3_all_countries.csv')
#   country_codes<-read.csv(path_to_country_codes)
#   gdp_data<-merge(gdp_data, country_codes, by=c('ISO3'))
#   colnames(gdp_data)[colnames(gdp_data)=='REGION']<-'region'
#   gdp_data<-gdp_data[,c('region', 'time_period', 'gdp_mmm_usd')]
#   
#   #Create the demand reduction data frame
#   time_period<-seq(from=0, to=35, by=1)
#   demand_reduction<-c(rep(0,10), seq(from = 0, to = 0.25, by = (0.25)/25))
#   demand_reduction_transformation<-data.frame(time_period, demand_reduction)
#   
#   #merge with demand reduction
#   gdp_data<-merge(gdp_data, demand_reduction_transformation, by='time_period')
#   gdp_data$cost_of_demand_reduction<-gdp_data$gdp_mmm_usd*gdp_data$demand_reduction*definition$multiplier
#   
#   #put these in the output data
#   data_strategy<-subset(data, strategy_code==definition$test_id & variable==definition$difference_variable)
#   data_output<-merge(data_strategy, gdp_data, by=c('region', 'time_period'))
#   
#   data_output$variable<-definition$output_variable_name
#   data_output$value<-data_output$cost_of_demand_reduction*10^9 #convert out of mmm
#   data_output$difference_variable<-'trns_frac_demand_reduced'
#   colnames(data_output)[colnames(data_output) == 'demand_reduction']<-'difference_value'
#   
#   data_output <- subset(data_output, select = -c(gdp_mmm_usd, cost_of_demand_reduction, primary_id))
# }
