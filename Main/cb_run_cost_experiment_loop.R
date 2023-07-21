#--------More libraries
library(tools)

#---------Paths to key files and lists of files-------
futures_file<-'../futures.csv'
variable_int_ext_file<-'../variable_definitions_int_ext.csv'

#This is artificial list of economy wide results filepaths. Replace with your own list
tmp1<-'~/Desktop/LAC_Decarb_Git/sisepuede_costs_benefits/Main/economy_wide_present_results_test1.csv'
tmp2<-'~/Desktop/LAC_Decarb_Git/sisepuede_costs_benefits/Main/economy_wide_present_results_test2.csv'

cb_filenames_list<-c(tmp1,tmp2)
nfiles<-length(cb_filenames_list)

#---------Generate and write to file a cost experimental design-------
#You can skip this if you already have a cost experimental design
num_futures<-1000
futures_definitions<-read.csv('../cost_experimental_design.csv')

#the code will create N+1 futures. Future 0 will be nominal values
futures<-cb_create_futures(futures_definitions, num_futures-1) 


#do a quick check to make sure each variable is only affected by one uncertainty
one_future<-futures[futures$cbfuture_id==1,]
unique_vars<-unique(one_future$variable)
length(unique_vars)==length(one_future$variable)
one_future$variable[duplicated(one_future$variable)]

write.csv(futures, futures_file)


#---------Edmundo, your for loop should start here ------------------

#----------Get the file with futures definitions
futures<-read.csv(futures_file)

for (f in 1:nfiles){

#----------Read Economy-Wide Results----------------------------------
  ecr<-read.csv(cb_filenames_list[f])

#----------Attach a future and apply scalars to those results---------
  #Get the matching cb_future, starting from 0
  future_f<-futures[futures$cbfuture_id==(f-1),]
  ecr_merged<-merge(ecr, future_f, by=('variable'), all.x=TRUE)
  
  #clean up the file
  #variables that don't match will have 'NA' for
  #cbfuture_id. This just cleans it up, but it doesn't matter.
  ecr_merged$cbfuture_id<-f-1
  
  #replace any NA uncertainties with "Nominal"
  ecr_merged$uncertainty<-as.character(ecr_merged$uncertainty)
  ecr_merged$uncertainty[is.na(as.character(ecr_merged$uncertainty))]<-'Nominal'
  
  #replace any NA scalars with 1s
  ecr_merged$scalar[is.na(ecr_merged$scalar)]<-1
  
  #Get the scaled values
  ecr_merged$scaled_value<-ecr_merged$value * ecr_merged$scalar
  
  #create a new filename
  ecr_outputfile<-paste0(file_path_sans_ext(basename(cb_filenames_list[f])), '_costscaled.csv')
  write.csv(ecr_merged, ecr_outputfile)
  
#----------Summarize internal and external net present benefits
  
  ecr_summarized<- ecr_merged %>%
    group_by(region, strategy_code, future_id, cbfuture_id) %>%
    summarise(scaled_NPV = sum(scaled_value))
  ecr_summarized$NPV_type<-'All Costs and Benefits'
    
  variable_int_ext <- read.csv(variable_int_ext_file)
  ecr_merged_int_ext<-merge(ecr_merged, variable_int_ext, by='variable')
  ecr_merged_int<-ecr_merged_int_ext[ecr_merged_int_ext$i_or_e=='Internal',]
  

  ecr_summarized_int<- ecr_merged_int %>%
    group_by(region, strategy_code, future_id, cbfuture_id) %>%
    summarise(scaled_NPV = sum(scaled_value))
  ecr_summarized_int$NPV_type<-'Internal Costs and Benefits Only'
  
  ecr_summarized_all<-rbind(ecr_summarized, ecr_summarized_int)
  
  
#----------Save the resultssv
  ecr_summary_outputfile<-paste0(file_path_sans_ext(basename(cb_filenames_list[f])), '_costscaled_summarized.csv')
  write.csv(ecr_summarized, ecr_summary_outputfile)


#----------Edmundo, your for loop should end here.
  
}
