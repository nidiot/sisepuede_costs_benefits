#This script scales costs and benefits by the intensity of different strategies
#It is only used for a few transformations whose costs and benefits are not
#fully specified in the model output results.

#--------0. SETUP-------

#A. Read the files that have the lhs specification, and the mapping from intensity variables to sample group
intensities<-read.csv('/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/ATTRIBUTE_TIME_PERIOD/lhc_trials_lever_effects_by_sample_group.csv')
intensity_variables<-read.csv('/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/ATTRIBUTE_TIME_PERIOD/variable_specification_to_sample_group.csv')
var_list<-read.csv('../intensity_cost_scaling_variables.csv')

#B. specify the range of actual intensities explored in the experiment (the intensity values are scaled accordingly)
INTENSITY_MIN<-0.5 
INTENSITY_MAX<-1

#--- START LOOP HERE----


#C. Read each future's file (put this inside the loop)
cb_orig<-read.csv('/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/ATTRIBUTE_TIME_PERIOD/Cost_futures_detail/2.csv')
scaled_outputfile<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/ATTRIBUTE_TIME_PERIOD/Cost_futures_detail/2_scaled.csv'



#--------1. LOOP THROUGH SCALINGS-------- 
cb_results<-cb_orig
nvars<-nrow(var_list)
for (v in 1:nvars){
  
  #v<-1 #for debugging
  
  #Find the group name from the variable specification, and turn that into a column name for the LHS sample
  grp<-intensity_variables[grep(var_list$Intensity.Variable[v], intensity_variables$variable_specification),]
  grp_col<-paste0("X", grp$sample_group)
  
  #Create scalars from the intensity column
  grp_intensities<-intensities[,c('future_id', grp_col)]
  colnames(grp_intensities)<-c('future_id', 'intensity_value')
  
  #Add a row for future id = 0
  grp_intensities$intensity_scalar<-grp_intensities$intensity_value*(INTENSITY_MAX-INTENSITY_MIN) + INTENSITY_MIN

  #Get the subset of cost benefit data that needs to be scaled
  grp_cb_data<-cb_results[grep(var_list$Cost.Variable[v], cb_results$variable),]
  
  #Merge it with the intensity data by future id
  grp_cb_data_merged<-merge(grp_cb_data, grp_intensities, by='future_id')
  
  #Multiply the value by the intensity scalar
  grp_cb_data_merged$scaled_value<-grp_cb_data_merged$value * grp_cb_data_merged$intensity_scalar
  
  #Replace the original dataset anywhere that the scaled value is not NA
  cb_results<-left_join(cb_results, grp_cb_data_merged, by=c('future_id', 'region', 'strategy_code', 'variable')) %>% 
    mutate(value = ifelse(!is.na(scaled_value), scaled_value, value.x)) %>% 
    select('future_id', 'region', 'strategy_code', 'variable', 'value')
  
  #For debugging -- this uses brazil as an example to show the original variable, scaled variable, and the scalar implied by the change
  if (TRUE){
    brazil_orig<-cb_orig[cb_orig$region=='brazil' & grepl(var_list$Cost.Variable[v], cb_orig$variable), c('value')]
    brazil_new<-cb_results[cb_results$region=='brazil' & grepl(var_list$Cost.Variable[v], cb_results$variable), c('value')]
    message(paste0("-----------", v, " ",  var_list$Cost.Variable[v], " ", var_list$Intensity.Variable[v], " ", grp_col))
    message(paste0("    Brazil orig: ", brazil_orig))
    message(paste0("    Brazil new:  ", brazil_new))
    message(paste0("    Implied Scalar   :  ", (brazil_new/brazil_orig-INTENSITY_MIN)/(INTENSITY_MAX-INTENSITY_MIN) ))
  }
}
#write the results to file
write.csv(cb_results, scaled_outputfile)

#---- END LOOP HERE

