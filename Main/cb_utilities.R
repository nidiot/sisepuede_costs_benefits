#Cost Benefit Utility Functions

library(reshape)
library(stringr)


#---------------Support Functions-----------------
#Read a list of cost factor names and return a list of 
cb_create_list_of_cost_factors<-function(cost_factor_csvs){
  cost_factor_filenames<-paste0(ssp_costs_benefits_git_path, 'cost_factors/', cost_factor_csvs)
  cost_factors_list<-lapply(cost_factor_filenames,read.csv)
  return(cost_factors_list)
}

#Get a column of data from a wide data table and return it as long for a single strategy
cb_get_data_from_wide_to_long<-function(data, strategy_code, variables){

  if (SSP_GLOBAL_LOG_VARIABLE_SEARCH)
    SSP_GLOBAL_LOG_OF_SEARCHED_VARS<<-append(SSP_GLOBAL_LOG_OF_SEARCHED_VARS, list(variables))
  
  data_wide<-data[data$strategy_code %in% strategy_code, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, variables)]
  data_long<-melt(data_wide, id.vars=SSP_GLOBAL_SIMULATION_IDENTIFIERS)
  

  return(data_long)  

}

#-------5. Uncertainty Analysis--------------

#-------apply cost uncertainties to present values of all output
cb_evaluate_in_futures<-function(results_present, futures, match_futures){
  
  cbfuture_ids<-unique(futures$cbfuture_id)
  
  if (match_futures){
    #if we are matching futures, then use the future ids as codes.
    #sorting both lists means the 0s should match
    cbfuture_ids_sorted<-sort(cbfuture_ids)
    results_future_ids<-sort(unique(results_present$future_id))
    futures_match<-data.frame(results_future_ids, cbfuture_ids)
    colnames(futures_match)<-c('future_id', 'cbfuture_id')
    results_present_expanded<-merge(results_present, futures_match, by='future_id', all.x=TRUE)
  }
  else{
    #expand the list of variables so that there is a copy for each future
    #combine the npv results iwth the expanded set of variables
    variables_expanded<-expand.grid(SSP_GLOBAL_list_of_cbvars, cbfuture_ids)
    colnames(variables_expanded)<-c('variable', 'cbfuture_id')
    results_present_expanded<-merge(results_present, variables_expanded, by='variable', all.x=TRUE)
  }
  #merge the results with each uncertainty
  merged_results<-merge(results_present_expanded, futures, by=c('variable',  'cbfuture_id'), all.x=TRUE)
  
  #replace any NA scalars with 1s and then multiply
  merged_results$scalar[is.na(merged_results$scalar)]<-1
  merged_results$value<-merged_results$value * merged_results$scalar
  
  #replace any NA uncertainties with "Nominal"
  merged_results$uncertainty<-as.character(merged_results$uncertainty)
  merged_results$uncertainty[is.na(as.character(merged_results$uncertainty))]<-'Nominal'
  
  
  return(merged_results)
}

#-------calculate an LHS sample of futures related to strategy effectiveness, costs, and benefits
cb_create_futures<-function(futures_definitions, num_futures){
  
  futures_definitions<-futures_definitions[futures_definitions$include==1,]
  
  #First create a dataframe of uncertainty scalars
  #generate the uncertainty scalars and store as a dataframe
  num_uncertainties<-nrow(futures_definitions)
  lhs_matrix <- t(randomLHS(num_futures, num_uncertainties))
  futures_scalars<-lhs_matrix*(futures_definitions$scalar_max - futures_definitions$scalar_min) + futures_definitions$scalar_min

  if (any(grepl('exp', futures_definitions$type)))  {
    for(u in 1:num_uncertainties){
      if (futures_definitions$type[u]=='exp')
        futures_scalars[u,]<-10^(futures_scalars[u,])
      
    }
  }
  futures_scalars<-t(futures_scalars)
  
  #add a baseline future
  futures_df<-as.data.frame(futures_scalars)
  baseline_future<-integer(num_uncertainties)+1
  futures_df<-rbind(baseline_future, futures_df)
  
  colnames(futures_df)<-futures_definitions$uncertainty
  futures_df$cbfuture_id<-c(0:num_futures)
  futures_df<-select(futures_df, "cbfuture_id", everything())
  
  #turn it into long form
  futures_long<-melt(futures_df, id.vars='cbfuture_id', variable_name='uncertainty')

  
  #Second create a dataframe of variables to which the uncertainties apply
  vars_df<-data.frame()
  for(u in 1:num_uncertainties){
    applicable_vars<-SSP_GLOBAL_list_of_cbvars[grep(futures_definitions$output_variables_regexp[u], SSP_GLOBAL_list_of_cbvars)]
    if (length(applicable_vars) > 0){
    tmpdf<-data.frame(uncertainty=futures_definitions$uncertainty[u],variable=applicable_vars)
    
    vars_df<-rbind(vars_df, tmpdf)
    }else{
      stop(paste("No variables match ", futures_definitions$output_variables_regexp[u]))
    }
  }

  futures_long<-merge(futures_long, vars_df, by='uncertainty')  
  futures_long_colnames<-colnames(futures_long)
  colnames(futures_long)[futures_long_colnames=='value']<-'scalar'
  
  return(futures_long)
}

#-------4. Post Process Data-----------

cb_present_value_of_each_cost_unsummed<-function(results, discount_rate){
  
  #trim the dataset from 2025 to 2050
  results<-results[results$time_period >= SSP_GLOBAL_TIME_PERIOD_TX_START &
                     results$time_period < SSP_GLOBAL_TIME_PERIODS,]
  
  #discount each cost
  #TABLEAU CODE:
  #[Value] / (1+[Discount Rate])^([Year] - [Cost Discounted To Year])
  results$value<-results$value/((1+discount_rate)^(results$time_period - SSP_GLOBAL_COST_YEAR))
  
  return(results)
}


cb_present_value_of_each_cost<-function(results, discount_rate){
  
  #trim the dataset from 2025 to 2050
  results<-results[results$time_period >= SSP_GLOBAL_TIME_PERIOD_TX_START &
                     results$time_period < SSP_GLOBAL_TIME_PERIODS,]
  
  #discount each cost
  #TABLEAU CODE:
  #[Value] / (1+[Discount Rate])^([Year] - [Cost Discounted To Year])
  results$value<-results$value/((1+discount_rate)^(results$time_period - SSP_GLOBAL_COST_YEAR))
  
  #summarize the net benefits over the entire time series
  results_present_value<- results %>%
    group_by (region, strategy_code, variable, future_id) %>%
    summarize(value=sum(value))
  
  return(results_present_value)
}

#calculate the net ghg and net discounted benefits
cb_net_benefits_and_emissions<-function(data, results, discount_rate){
  
  #exclude any sector-specific costs
  results<-results[!grepl('sector_specific', results$variable),]
  
  results$discounted_value<-results$value/((1+discount_rate)^(results$time_period - SSP_GLOBAL_COST_YEAR))
  
  #Summarize the net benefits in each year
  results_discounted<- results %>%
    group_by (region, time_period, strategy_code, future_id) %>%
    summarize(discounted_value=sum(discounted_value))

  #Trim the data from 2025 to 2050
  results_discounted<-results_discounted[results_discounted$time_period >= SSP_GLOBAL_TIME_PERIOD_TX_START &
                                           results_discounted$time_period < SSP_GLOBAL_TIME_PERIODS,]
  
  #summarize the net benefits over the entire time series
  results_summed<- results_discounted %>%
    group_by (region, strategy_code, future_id) %>%
    summarize(net_present_value=sum(discounted_value))
  
  
  #get the emissions results
  strategies_in_results<-unique(results$strategy_code)
  ghg_vars<-SSP_GLOBAL_list_of_variables[grep('emission_co2e_subsector_total_', SSP_GLOBAL_list_of_variables)]
  nstrat<-length(strategies_in_results)
  
  ghg_data<-list()
  for (s in 1:nstrat){
    ghg_data_s<-cb_get_data_from_wide_to_long(data, strategies_in_results[s], ghg_vars)
    ghg_data_s<-ghg_data_s[ghg_data_s$time_period==(SSP_GLOBAL_TIME_PERIODS-1),]
    ghg_data_s<-ghg_data_s %>%
      group_by (region, strategy_code, future_id) %>%
      summarize(net_emission_co2e=sum(value))
    ghg_data<-append(ghg_data, list(ghg_data_s))
  }
  ghg_data<-do.call(rbind, ghg_data)
  
  #merge the two 
  cb_and_ghg<-merge(results_summed, ghg_data, by=c('region', 'strategy_code', 'future_id'))
  cb_and_ghg$discount_rate<-discount_rate

  return(cb_and_ghg)

}

#This function summarizes cost benefit data using the naming convention of output variables
#Roll levels are: prefix	sector	cost_type	specification	sub_specificaiton
cb_roll_up_results<-function(res, roll_up_to){
  if (length(grep(roll_up_to, SSP_GLOBAL_OUTPUT_NAME_CONVENTION)) == 0){
    message(paste("ERROR in rollup. Invalid rollup level:", roll_up_to))
    message(paste("Valid strings are ", paste(SSP_GLOBAL_OUTPUT_NAME_CONVENTION, collapse=", ")))
    return(res)
  }
  
  #Each output variable has up to 5 substrings delimited by ":"
  #Figure out which delimeter the rollup asks for
  n<-grep(paste0('\\b', roll_up_to, '\\b'), SSP_GLOBAL_OUTPUT_NAME_CONVENTION)
  
  #create a pattern match for that
  #Found this here: https://stackoverflow.com/questions/33062016/how-to-delete-everything-after-nth-delimiter-in-r
  pat <- paste0('^([^:]+(?::[^:]+){',n-1,'}).*')
  res$variable<-sub(pat, '\\1', res$variable)
  
  #summarize the reuslts, dropping the difference_variable and difference_value parameters
  res_summarized<-res %>% 
    group_by(region, time_period, strategy_code, variable) %>% 
    summarise(value = sum(value))
  
  
  return(res_summarized)
}

cb_process_interactions<-function(res, st2tx, pp_instructions){

  #to run as script
  #interactions<-read.csv('../strategy_interaction_definitions.csv')
  #st2tx<-strategy2tx
  #res<-results_all
  #s<-1
  #i<-1
  
  interactions<-pp_instructions
  
  #get the list of interactions
  list_of_interactions<-unique(interactions$interaction_name)
  n_interactions<-length(list_of_interactions)
  
  #get the strategies in the results file
  strategies<-unique(res$strategy_code)
  n_strat<-length(strategies)
  for (s in 1:n_strat){
    
    #get the list of transformations
    strategy_code<-strategies[s]
    strategy_definition<-st2tx[st2tx$strategy_code==strategy_code,]
    strategy_definition<-strategy_definition[,strategy_definition != 0]
    
    #update the strategy codes in the definition file
    tx_in_strategy<-colnames(strategy_definition)
    tx_in_strategy<-tx_in_strategy[tx_in_strategy != 'strategy_code']
    
    
    #for each interaction
    for (i in 1:n_interactions){
      #transformations that interact
      tx_interacting<-interactions[interactions$interaction_name==list_of_interactions[i],]
      tx_in_interaction<-unique(tx_interacting$transformation_code)
      tx_in_both<-tx_in_interaction[tx_in_interaction %in% tx_in_strategy]
      
      #only count the transfomrations actully in the strategy
      tx_interacting<-tx_interacting[tx_interacting$transformation_code %in% tx_in_both, ]

      if (SSP_PRINT_STRATEGIES) {
        message(paste('Resolving Interactions in', 
                      list_of_interactions[i], ':', paste(tx_interacting$transformation_code, collapse=', '))) 
      }
      
      if (length(tx_interacting$transformation_code) == 0){
        if (SSP_PRINT_STRATEGIES) 
          message(paste('No interactions, skipping...', strategy_code))
        next
        
      }
            
      #rescale
      tx_rescale<-tx_interacting %>%
        group_by(transformation_code) %>%
        summarize(original_scalar = mean(relative_effect))
      
      new_sum<-sum(tx_rescale$original_scalar)
      tx_rescale$newscalar<-tx_rescale$original_scalar/new_sum
      
      #update the original scalars in the intracting tx
      tx_interacting<-merge(tx_interacting, tx_rescale, by='transformation_code')
      tx_interacting$strategy_code<-strategy_code
      
      #apply these scalars to the data
      res_subset<-res[res$strategy_code==strategy_code & res$variable %in% tx_interacting$variable,]
      res_subset<-merge(res_subset, tx_interacting, by=c('strategy_code', 'variable'), suffixes=c('', '.int'))
      res_subset$newscalar[res_subset$scale_variable==0]<-1
      res_subset$value<-res_subset$value * res_subset$newscalar
      res_subset$difference_value<-res_subset$difference_value * res_subset$newscalar
      
      #make a replacement dataset
      res_for_replacement<-res_subset[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
      
      #remove the other rows from the dataset
      res<-res[!(res$strategy_code==strategy_code & res$variable %in% tx_interacting$variable),]
      res<-rbind(res, res_for_replacement)
    }
  }
  
return(res)
  
}


#-------3. Apply Costs and Benefits (cb_functions)-----------

#This function calculates the costs adn benefits as a multiplier applied to the
#difference in a variable between two strategies, where that difference is
#defined by a fraction reduction that occurs in each case.
#To be specific, let E_tx be the effect we observe (e.g., T of fugitive emisisons) in the transformation
#let f_base and f_tx be the fractions of fugitive emissions reduced in each transformation
#then the amount that was originally prouced is Etx/(1-ftx). And, the ammount that would be
#observed after the original abatement fraction would be Etx/(1-ftx)*(1-fbase)
#Then the difference we observe is D = Etx - Etx/(1-ftx)*(1-fbase)
#Which reduces to D = Etx * (1- (1-fbase)/(1-ftx)) and hte multiplier becomes
# M = (1- (1-fbase)/(1-ftx))

cb_fraction_avoided<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                             output_mults, change_in_multiplier, country_specific_multiplier,
                             frac_avoided, invert, list_of_variables){
  
  #get the change in fractions
  fraction_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, frac_avoided)
  fraction_base<-cb_get_data_from_wide_to_long(data, strategy_code_base, frac_avoided)
  

  data_merged<-merge(fraction_tx, fraction_base, by=c('time_period', 'region', 'variable'), suffixes = c('.tx_frac', '.ba_frac'))
  data_merged$fraction_multiplier<-1-((1-data_merged$value.ba_frac)/(1-data_merged$value.tx_frac))
  
  #get the output results
  output_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, diff_var)
  data_merged<-merge(data_merged, output_tx, by=c('time_period', 'region'), suffixes=c('.tx_frac', '.effect'))
  colnames(data_merged)[colnames(data_merged)=='value']<-'effect_value'
  
  #get the avoided value
  data_merged$difference_variable<-diff_var
  data_merged$difference_value<-data_merged$effect_value*data_merged$fraction_multiplier
  data_merged$variable<-output_vars
  
  data_merged$time_period_for_multiplier_change<-pmax(0,data_merged$time_period-SSP_GLOBAL_TIME_PERIOD_2023)
  data_merged$value<-t(t(data_merged$difference_value)*output_mults*change_in_multiplier^data_merged$time_period_for_multiplier_change)    
  
  data_merged_results<-data_merged[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  return(data_merged_results)
  
}


#This function calculates the costs adn benefits as a multiplier applied to the
#difference in a variable between two strategies, where that difference is
#defined by some change in a factor, e.g., km/l or tons-waste/person. frac_var
#gives the name of the variable that has the fraction. Invert tells us whether we
#need to flip the fractions ot make our calculation correct. If our effect variable
#is already in the denominator of our fraction (e.g., effect in L, fraction is km/L) then
#we are good. If the effect variable is in the numerator (e.g., effect in T, fraction is 
#T/person) then we need to flip it.
#To be specific, let E_tx be the effect we observe (e.g., L of fuel) in the transformation
#let f_base and f_tx be the fractions of km/L in the base and transformed futures
#then the distance that has been traveled in the transformation, d_tx = E_tx*f_tx. 
#traveling that same distance with the old efficiency would have required 
#E_base = d_tx/f_base L. So, E_tx-E_base = E_tx - d_tx/f_base = E_tx - Etx*f_tx/f_base
# = E_tx(1-ftx/f_base)
cb_fraction_change<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                             output_mults, change_in_multiplier, country_specific_multiplier,
                             frac_var, invert, list_of_variables){
  
  #get teh change in fractions
  fraction_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, frac_var)
  fraction_base<-cb_get_data_from_wide_to_long(data, strategy_code_base, frac_var)
  
  if (invert==1){
    fraction_tx$value<-1/fraction_tx$value
    fraction_base$value<-1/fraction_base$value
  }
  data_merged<-merge(fraction_tx, fraction_base, by=c('time_period', 'region', 'variable'), suffixes = c('.tx_frac', '.ba_frac'))
  data_merged$fraction_change<-data_merged$value.tx_frac/data_merged$value.ba_frac
  data_merged$fraction_multiplier<-(1-data_merged$fraction_change)
  
  #get the output results
  output_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, diff_var)
  data_merged<-merge(data_merged, output_tx, by=c('time_period', 'region'), suffixes=c('.tx_frac', '.effect'))
  colnames(data_merged)[colnames(data_merged)=='value']<-'effect_value'
  
  #get the avoided value
  data_merged$difference_variable<-diff_var
  data_merged$difference_value<-data_merged$effect_value*data_merged$fraction_multiplier
  data_merged$variable<-output_vars

  data_merged$time_period_for_multiplier_change<-pmax(0,data_merged$time_period-SSP_GLOBAL_TIME_PERIOD_2023)
  data_merged$value<-t(t(data_merged$difference_value)*output_mults*change_in_multiplier^data_merged$time_period_for_multiplier_change)    
  
  data_merged_results<-data_merged[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  
  #any divide-by-zero NAs from our earlier division gets a 0
  data_merged_results[is.na(data_merged_results)]<-0
  return(data_merged_results)
  
}
#This function calculates costs and benefits as just a scalar applied to a variable within
# a single strategy. It uses code from cb_difference_between_two_strategies, so the use of
# data_merged, for example, is holdover from that function.
cb_scale_variable_in_strategy<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                        output_mults, change_in_multiplier, country_specific_multiplier,
                                        arg1, arg2, list_of_variables){
  
  
  id_vars <-c('region','time_period', 'strategy_code', 'future_id') #primary_id->strategy_code
  
  #diff_var_name<-paste0("diff_", diff_var)
  diff_var_name<-diff_var
  #datap_tx<-subset(data,strategy_code==strategy_code_tx & variable==diff_var) #primary_id->strategy_code
  datap_tx<-cb_get_data_from_wide_to_long(data, strategy_code_tx, diff_var)
  
  datap_tx$region<-as.character(datap_tx$region)
  datap_tx$variable<-as.character(datap_tx$variable)
  
  #This is code copied over from another function, so data_merged is just datap_tx
  data_merged<-datap_tx
  data_merged$difference<-data_merged$value
  #data_merged$time_period_for_multiplier_change<-data_merged$time_period
  data_merged$time_period_for_multiplier_change<-pmax(0,data_merged$time_period-SSP_GLOBAL_TIME_PERIOD_2023)
  data_merged$values<-t(t(data_merged$difference)*output_mults*change_in_multiplier^data_merged$time_period_for_multiplier_change)  
  
  tmp<-data_merged[,c(id_vars)]
  tmp$difference_variable<-diff_var_name
  tmp$difference_value<-data_merged$difference
  tmp$variable<-output_vars
  tmp$value<-data_merged$values
  output<-tmp
  
  return(output)
}

#This function calculates the costs adn benefits as a multiplier applied to the 
#difference in a variable between two strategies
cb_difference_between_two_strategies<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_var_name, 
                                               output_mults, change_in_multiplier, country_specific_multiplier,
                                               arg1, arg2, list_of_variables){
  
  
  #diff_var_name<-paste0("diff_", diff_var)
  diff_var_name<-diff_var
  
  #get the data tables and merge them
  datap_base<-data[data$strategy_code==strategy_code_base, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, diff_var)]
  datap_tx<-data[data$strategy_code==strategy_code_tx, c(SSP_GLOBAL_SIMULATION_IDENTIFIERS, diff_var)]
  datap_base$primary_id<-NULL
  datap_base$strategy_code<-NULL
  
  tx_suffix<-'.tx'
  base_suffix<-'.base'
#  data_merged<-merge(datap_tx, datap_base, by=c('region', 'time_period'), suffixes=c(tx_suffix, base_suffix))
  data_merged<-merge(datap_tx, datap_base, by=c('region', 'time_period', 'future_id'), suffixes=c(tx_suffix, base_suffix))
  
    
  #Calculate the difference in variables and then apply the multiplier, which may change over time
  #Assume cost change only begins in 2023
  data_merged$difference_variable<-diff_var_name
  data_merged$difference_value<-data_merged[,paste0(diff_var, tx_suffix)]-data_merged[,paste0(diff_var, base_suffix)]
                                                                                
  data_merged$time_period_for_multiplier_change<-pmax(0,data_merged$time_period-SSP_GLOBAL_TIME_PERIOD_2023)

  data_merged$variable<-output_var_name
  data_merged$value<-t(t(data_merged$difference_value)*output_mults*change_in_multiplier^data_merged$time_period_for_multiplier_change)  
  
  data_merged<-data_merged[,SSP_GLOBAL_COLNAMES_OF_RESULTS]
  return(data_merged)
}


#-------2. Loop Through Variables--------

cb_wrapper<-function(cb_function, data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                     output_mults, change_in_multiplier, country_specific_multiplier,
                     arg1, arg2, sum_results, list_of_variables){
  
  result_tmp<-list()
    
  #Get all the variables that match the difference variable
  diff_var_list<-as.character(list_of_variables[grep(glob2rx(diff_var), list_of_variables)])
  #diff_var_list<-as.character(list_of_variables[grep(diff_var, list_of_variables, perl=TRUE)])
  
  if (length(diff_var_list) == 0){
    message(paste('ERROR IN CB_WRAPPER: No variables match ', diff_var))
    return(NULL)
    
  }
  
  #For each variable that matches the substring, calculate the costs and benefits
  for (v in diff_var_list){
    if (SSP_PRINT_DETAILED_VARS) message(paste("          ", v))
    do_call_args<-list(data, 
                       strategy_code_tx, 
                       strategy_code_base, 
                       v,
                       output_vars,
                       as.numeric(output_mults),
                       change_in_multiplier,
                       country_specific_multiplier,
                       arg1, arg2, list_of_variables)
    result<-do.call(cb_function, do_call_args)
    
    result$region<-as.character(result$region)
    result$strategy_code<-strategy_code_tx
    result_tmp<-append(result_tmp, list(result))
  }
  
  #If flagged, sum up the variables in value and difference_value columns
  #Create a new output data frame and append it to the existing list
  #Note that the difference variable may be garbage if we are summing across different comparison variables
  if (sum_results==1){
    #print('...Combining prior variables')
    
    #create one long dataset
    result_tmp<-do.call("rbind", result_tmp)
    
    #summarize the results by region, time period, and strategy code
    #while summing up hte values and difference values
    results_summarized<-result_tmp %>% 
      group_by(region, time_period, strategy_code, future_id) %>% 
      summarise(value = sum(value), difference_value = sum(difference_value))
    
    #readd the difference variable and variable fields
    results_summarized$difference_variable<-diff_var #paste0('diff_', diff_var)
    results_summarized$variable<-output_vars
    return(results_summarized)
    
  }else{
    appended_results<-do.call("rbind", result_tmp)
    return(appended_results)
  }
}

cb_apply_cost_factors<-function(data, definition, cost_factors, list_of_variables_in_dataset){
  
  cb_results<-list()
  num_rows<-nrow(cost_factors)
  
  #loop through each row in the factors file and calculate the costs and benefits specified
  for (r in 1:num_rows){
    cost_line<-cost_factors[r,]  
    if (SSP_PRINT_TOP_LINE_VARS) message(paste0('---------Costs for: ', cost_line$output_variable_name))
    
#    if (cost_line$output_variable_name == 'cb:trww:water_pollution:wastewater_treatment:COD'){
#      message("UH OH 2")
#    }
    
    result<-cb_wrapper('cb_difference_between_two_strategies',
                       data, 
                       definition$strategy_code, 
                       definition$comparison_code, 
                       cost_line$difference_variable,
                       cost_line$output_variable_name,
                       as.numeric(cost_line$multiplier),
                       cost_line$annual.change,
                       FALSE,
                       0, 'TEST', 
                       cost_line$sum,
                       list_of_variables_in_dataset)
    
    cb_results<-rbind(cb_results, result)
  }
  return(cb_results)
}







#------------------1. Loop Through Cost Definitions -------
#This function loops through the strategies and applies system costs 
#which are defined in the cost factors
cb_calculate_system_costs<-function(data, strategy_cost_definitions_param, system_cost_definitions){
  
  list_of_variables<-SSP_GLOBAL_list_of_variables

  #cut the list of strategies to evaluate to those that are marked to be evaluated, and that are in the data
  strategy_cost_definitions<-strategy_cost_definitions_param
  strategy_cost_definitions<-strategy_cost_definitions[strategy_cost_definitions$evaluate_system_costs==1 & 
                                               strategy_cost_definitions$strategy_code %in% SSP_GLOBAL_list_of_strategies,]
  ndefs<-nrow(strategy_cost_definitions)
  
  #cut the list of system cost definitions to those that are marked to be evaluated
  system_cost_definitions<-system_cost_definitions[system_cost_definitions$include==1,]
  cost_factors_list<-cb_create_list_of_cost_factors(system_cost_definitions$system_cost_filename)
  ncost_factors<-length(cost_factors_list)
  
  results<-list()
  for (d in 1:ndefs){
    if (SSP_PRINT_STRATEGIES)
      message(paste('============Evaluating system costs for', strategy_cost_definitions$strategy_code[d]), '=================')
  
    for (c in 1:ncost_factors){
      if (SSP_PRINT_COST_FACTOR_FILENAMES)
        message(paste0('Cost Factor Loop: ', strategy_cost_definitions$strategy_code[d], 
                       ' vs. ',
                       strategy_cost_definitions$comparison_code[d],
                       ' for cost factor: ', 
                       system_cost_definitions$system_cost_filename[c]))
      
#      if (system_cost_definitions$system_cost_filename[c] == 'trww_treatment_water_pollution_cost_factors.csv'){
#        message("Uh OH!")
#      }
      
      do_call_args<-list(data, strategy_cost_definitions[d,], data.frame(cost_factors_list[c]), list_of_variables)
      r<-do.call(system_cost_definitions$cb_function[c], do_call_args)
      results<-append(results, list(r))
    }
  }
  
  #append all the outputs
  cb_results<-do.call("rbind", results)
  
  #boom. done.
  return(cb_results)
}

#This function loops through the strategies and creates a cost benefit definition table
#uniqe to each strategy based on the componetn transformations
#It then calls a support function to loop through the strategy definition
cb_calculate_transformation_costs<-function(data, strategy_cost_definitions, strategy_definitions_table, tx_definitions_table){

  #cut the list of strategies to evaluate to those that are marked to be evaluated, and that are in the data
  strategy_cost_definitions<-strategy_cost_definitions[strategy_cost_definitions$evaluate_transformation_costs==1 & 
                                                         strategy_cost_definitions$strategy_code %in% SSP_GLOBAL_list_of_strategies,]
  ndefs<-nrow(strategy_cost_definitions)
  
  #For each strategy, create a set of instructions for calculating transformation-specific costs and benefits
  #based on the (a) strategy to transformation mapping and (b) transformation cost table
  #call the instructions and append them to the list.
  #Note: it is possible we could speed this up by creating a list of all instructions and then running the code once
  #And, the function that calls cb_wrapper could probably be integrated into this function
  results<-list()
  for (d in 1:ndefs){
    
    strategy_definition<-strategy_definitions_table[strategy_definitions_table$strategy_code==strategy_cost_definitions$strategy_code[d],]
    strategy_definition<-strategy_definition[,strategy_definition != 0]
    
    #update the strategy codes in the definition file
    transformations_list<-colnames(strategy_definition)
    transformations_list<-transformations_list[transformations_list != 'strategy_code']
    
    strategy_cb_table<-tx_definitions_table[tx_definitions_table$transformation_code %in% transformations_list, ]
    strategy_cb_table[is.na(strategy_cb_table)] <- 0
    
    #------Just a debug section------
    message(paste("The following transformations are in strategy: ", strategy_cost_definitions$strategy_code[d]))
    message(paste(transformations_list, collapse='\n'))
    dbg_has_cost<-transformations_list %in% tx_definitions_table$transformation_code
    dbg_df<-data.frame(transformations_list, dbg_has_cost)
    message(paste(capture.output(dbg_df), collapse="\n"))
    
    
    strategy_cb_table$strategy_code<-strategy_cost_definitions$strategy_code[d]
    strategy_cb_table$test_id<-strategy_cost_definitions$strategy_code[d]
    strategy_cb_table$comparison_id<-strategy_cost_definitions$comparison_code[d]
    
    r<-cb_calculate_transformation_costs_in_strategy(data, strategy_cb_table)
    results<-append(results, list(r))
  }
  
  #append all the outputs
  cb_results<-do.call("rbind", results)
  
  #boom. done.
  return(cb_results)

}

#This function loops through a strategy-specific cost benefit definition created by the 
#cb_calculate_transformation_costs function and calls cb_wrapper on each and returns the results
cb_calculate_transformation_costs_in_strategy<-function(data, strategy_specific_definitions){
  
  
  #cut the list of strategies to evaluate to those that are marked to be evaluated, and that are in the data
  strategy_specific_definitions<-strategy_specific_definitions[strategy_specific_definitions$include==1 & 
                                                                 strategy_specific_definitions$strategy_code %in% SSP_GLOBAL_list_of_strategies,]
  
  nstrat<-nrow(strategy_specific_definitions) #update number of strategies

  for (s in 1:nstrat){
  if (SSP_PRINT_STRATEGIES)
    message(paste('Evaluating transformation costs for', strategy_specific_definitions$strategy_code[s],
                  'which is ',
                  strategy_specific_definitions$test_id[s],
                  ' vs. ',
                  strategy_specific_definitions$comparison_id[s],
                  "for", strategy_specific_definitions$output_variable_name[s]))
  }

  results<-list()
  for (s in 1:nstrat){
    
    #if the strategy is to be included and is in the data....
    if (SSP_PRINT_STRATEGIES)
      message(paste('============Evaluating transformation costs for', strategy_specific_definitions$strategy_code[s], 
                    "-", strategy_specific_definitions$output_variable_name[s], '============'))
    
    #call the wrapper function and append
    r<-cb_wrapper(strategy_specific_definitions$cb_function[s],
                  data,
                  strategy_specific_definitions$test_id[s],
                  strategy_specific_definitions$comparison_id[s],
                  strategy_specific_definitions$difference_variable[s],
                  strategy_specific_definitions$output_variable_name[s],
                  as.numeric(strategy_specific_definitions$multiplier[s]),
                  strategy_specific_definitions$annual.change[s],
                  FALSE,
                  strategy_specific_definitions$arg1[s],
                  strategy_specific_definitions$arg2[s],
                  strategy_specific_definitions$sum[s],
                  SSP_GLOBAL_list_of_variables)
    results<-append(results, list(r))
    #message(paste0("Num cols: ", ncol(r)))
    
  }
  #append all the outputs
  cb_results<-do.call("rbind", results)
  
  #boom. done.
  return(cb_results)
}


