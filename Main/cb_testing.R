largedata<-read.csv('~/Desktop/LAC Model Results and Visualizations/sisepuede_results_WIDE_INPUTS_OUTPUTS.csv')
trimmed_data_long<-ssp_trim_reshape(largedata)
#trimmed_data_long<-melt(trimmed_data, id.vars=c('primary_id', 'time_period', 'region'))
write.csv(trimmed_data_long, '~/Desktop/LAC Model Results and Visualizations/sisepuede_results_TRIMMED_LONG.csv')


#---------pasting and unlisting
a<-strsplit(rollup_test_input$variable, ":")
sapply( a, paste0, collapse=":")

b<-sapply(a, 'paste')
paste0(unlist(strsplit(rollup_test_input$variable, ":")[1]), collapse=":")



#--------SCOE scalars
rel_effect_scoe<-ssp_relative_effect_of_strategies(data, 
                                                   'BASE', 
                                                   c('SCOE:FUEL_SWITCH_HEAT', 'SCOE:INC_EFFICIENCY_APPLIANCE', 'SOCE:DEC_DEMAND_HEAT' ), 
                                                   'energy_consumption_scoe_total')

rel_effect_inen<-ssp_relative_effect_of_strategies(data, 
                                                   'BASE',
                                                   c('INEN:FUEL_SWITCH_HI_HEAT', 'INEN:FUEL_SWITCH_LO_HEAT', 
                                                     'INEN:FUEL_SWITCH_HI_AND_LO_HEAT',
                                                     'INEN:INC_EFFICIENCY_ENERGY'),
                                                   'energy_consumption_inen_total')

#-----------EFFECT Of fgas effect factors
a<-data[data$region=='brazil' & data$time_period==35 & data$strategy_code=='BASE', 
        c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, SSP_GLOBAL_list_of_variables[grep('ef_ippu_tonne_', SSP_GLOBAL_list_of_variables)])]
b<-data[data$region=='brazil' & data$time_period==35 & data$strategy_code=='IPPU:BUNDLE_DEC_FGAS', 
        c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, SSP_GLOBAL_list_of_variables[grep('ef_ippu_tonne_', SSP_GLOBAL_list_of_variables)])]
d<-data[data$region=='brazil' & data$time_period==35 & data$strategy_code=='IPPU:DEC_N2O', 
        c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, SSP_GLOBAL_list_of_variables[grep('ef_ippu_tonne_', SSP_GLOBAL_list_of_variables)])]


e<-rbind(a, b, d)

f[,grep('n2o', colnames(c))]



  
#------------------WASTE

strategy_code_tx<-'WASO:DEC_MSW_PER_CAPITA'
strategy_code_base<-'BASE'
frac_var<-SSP_GLOBAL_list_of_variables[grep(glob2rx('factor_waso_waste_per_capita_scalar_*'), SSP_GLOBAL_list_of_variables)]
fraction_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, frac_var)
fraction_base<-cb_get_data_from_wide_to_long(data, strategy_code_base, frac_var)
frac_merged_waste<-merge(fraction_tx, fraction_base, by=c('time_period', 'region', 'variable'), suffixes = c('.tx_frac', '.ba_frac'))
frac_merged_waste$fraction_change<-frac_merged_waste$value.tx_frac/frac_merged_waste$value.ba_frac
#-------------------Increase an output by a fractional difference

#create teh variables
frac_var<-'fuelefficiency_trns_road_light_gasoline_km_per_litre'
diff_var<-'energy_consumption_trns_total'
strategy_code_tx<-'TRNS:INC_EFFICIENCY_NON_ELEC'
strategy_code_base<-'BASE'
output_vars<-'cost_of_trns_energy_efficiency'
output_mult<-(-880000)

fraction_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, frac_var)
fraction_base<-cb_get_data_from_wide_to_long(data, strategy_code_base, frac_var)
data_merged<-merge(fraction_tx, fraction_base, by=c('time_period', 'region', 'variable'), suffixes = c('.tx_frac', '.ba_frac'))
data_merged$fraction_change<-data_merged$value.tx_frac/data_merged$value.ba_frac

#get the output results
output_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, diff_var)
data_merged<-merge(data_merged, output_tx, by=c('time_period', 'region'), suffixes=c('.tx_frac', '.effect'))
colnames(data_merged)[colnames(data_merged)=='value']<-'effect_value'

#get the avoided value
data_merged$difference_variable<-diff_var
data_merged$difference_value<-data_merged$effect_value*(data_merged$fraction_change-1)
data_merged$variable<-output_vars
data_merged$value<-data_merged$difference_value * output_mult
  
data_merged_results<-data_merged[,SSP_GLOBAL_COLNAMES_OF_RESULTS]

#--------

#create teh change in the factor
fraction_base<-cb_get_data_from_wide_to_long(data, 'BASE', frac_var)
fraction_tx<-cb_get_data_from_wide_to_long(data, 'TRNS:INC_EFFICIENCY_NON_ELEC', frac_var)
data_merged<-merge(fraction_tx, fraction_base, by=c('time_period', 'region', 'variable'), suffixes = c('.tx', '.ba'))
data_merged$fraction_change<-data_merged$value.tx/data_merged$value.ba

#merge teh data
output_tx<-cb_get_data_from_wide_to_long(data, 'TRNS:INC_EFFICIENCY_NON_ELEC', effect_var)
data_merged<-merge(data_merged, output_tx, by=c('time_period', 'region'), suffixes = c('', '.tx_effect'))

#get the avoided value
data_merged$difference_variable<-data_merged$value*(data_merged$fraction_change-1)


#vars<-SSP_GLOBAL_list_of_variables[grep(glob2rx('energy_consumption_trns_total'), SSP_GLOBAL_list_of_variables)]
#vars<-vars[!grepl('electricity', vars)]
#grep_term<-paste(trns_fuels$CAT.FUEL,collapse="|")
#trns_energy_vars<-trns_energy_vars[grep(trns_fuels_grep_term, trns_energy_vars)]


#read the category fuels names
trns_fuels<-read.csv('/Users/nidhi/Desktop/LAC_Decarb_Git/lac_decarbonization/docs/source/csvs/attribute_cat_fuel.csv', check.names = FALSE)
colnames(trns_fuels)[colnames(trns_fuels) == 'X...CAT.FUEL...']<-'CAT.FUEL'
trns_fuels$CAT.FUEL<-str_replace_all(trns_fuels$CAT.FUEL, '``', '')
trns_fuels$CAT.FUEL<-str_replace_all(trns_fuels$CAT.FUEL, 'fuel_', '')

#get the data for the efficiency change factors in each non-electricity category
trns_efficiency_factor_vars<-SSP_GLOBAL_list_of_variables[grep(glob2rx('fuelefficiency_trns*'), SSP_GLOBAL_list_of_variables)]
trns_efficiency_tx<-cb_get_data_from_wide_to_long(data, 'TRNS:INC_EFFICIENCY_NON_ELEC', trns_efficiency_factor_vars)
trns_efficiency_base<-cb_get_data_from_wide_to_long(data, 'BASE', trns_efficiency_factor_vars)
trns_efficiency_merged<-merge(trns_efficiency_tx, trns_efficiency_base, by=c('time_period', 'region', 'variable'), suffixes = c('.tx', '.ba'))
trns_efficiency_merged$ef_tx_over_base<-trns_efficiency_merged$value.tx/trns_efficiency_merged$value.ba

#get the data for the energy consumption in each non-electricity category
trns_energy_vars<-SSP_GLOBAL_list_of_variables[grep(glob2rx('energy_consumption_trns_*'), SSP_GLOBAL_list_of_variables)]
trns_fuels_grep_term<-paste(trns_fuels$CAT.FUEL,collapse="|")
trns_energy_vars<-trns_energy_vars[grep(trns_fuels_grep_term, trns_energy_vars)]
trns_energy_vars<-trns_energy_vars[!grepl('electricity', trns_energy_vars)]
trns_energy_tx<-cb_get_data_from_wide_to_long(data, 'TRNS:INC_EFFICIENCY_NON_ELEC', trns_energy_vars)

#final steps are to match the yearly increase in efficiency factors with teh energy conumption data
#calculate the energy consumption avoided
#multiply by the cost per unit of energy avoided


#assume we have read the header in the cb_calculate script


strategy2tx_filename<-paste0(ssp_costs_benefits_git_path, 'attribute_strategy_code.csv')
strategy2tx<-read.csv(strategy2tx_filename, check.names = FALSE)


strategy_cost_instructions_glob<-read.csv('~/Desktop/LAC_Decarb_Git/ssp_cost_benefits/strategy_cost_instructions.csv')
system_cost_definitions_glob<-read.csv('~/Desktop/LAC_Decarb_Git/ssp_cost_benefits/system_cost_factors_list.csv')  

transformation_definitions<-read.csv('~/Desktop/LAC_Decarb_Git/ssp_cost_benefits/transformation_cost_definitions.csv')

results<-cb_calculate_transformation_costs(data, 
                                           strategy_cost_instructions_glob[strategy_cost_instructions_glob$strategy_code=='LVST:ALL',],
                                           strategy2tx, 
                                           transformation_definitions)





#evaluate system costs
r<-cb_calculate_system_costs(data, strategy_cost_definitions_glob[strategy_cost_definitions_glob$strategy_code=='TRNS:FUEL_SWITCH_LIGHT_DUTY' 
                                                                  | strategy_cost_definitions_glob$strategy_code=='TRNS:FUEL_SWITCH_RAIL',], 
                             system_cost_definitions_glob)

r2<-cb_calculate_transformation_costs(data, )
