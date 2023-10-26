#This file rolls economy wide cost results into major categories
#Costs and benefits are generally described by the "Variabe" column which
#Has five name parts: cb:sector:benefit_type:benefit_subcategory:benefit_detail
#This file rolls up results to the first three levels: cb:sector:benefit_type
#It also summarizes these results for hte region as a whole
#This script is meant to be used on net present value of econony wide results
#(i.e., no time_period column)

#Load libraries
library(stringr)
library(dplyr)

#EDMUNDO: (1) PLEASE REPLACE THIS WITH YOUR LIST OF COST BENEFIT RESULTS FILES
f1<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/ATTRIBUTE_TIME_PERIOD/Cost_futures_detail/2_scaled.csv'
f2<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/ATTRIBUTE_TIME_PERIOD/Cost_futures_detail/2_scaled.csv'
cb_filenames_list<-c(f1,f2)

#EDMUNDO: (2) PLEASE SPECIFY WHERE YOU'D LIKE THE RESULTS
country_summary_table_filename<-'/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/ATTRIBUTE_TIME_PERIOD/Cost_futures_detail/cb_summary_country_level.csv'
lac_summary_table_filename<- '/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Intensity 10.13/ATTRIBUTE_TIME_PERIOD/Cost_futures_detail/cb_summary_lac.csv'


nfiles<-length(cb_filenames_list)
country_summary_results<-list()
lac_summary_results<-list()
for (f in 1:nfiles){
  f<-1
  #Read the file
  ecr<-read.csv(cb_filenames_list[f])
  ecr$value[is.na(ecr$value)] <- 0
  
  
  #Break up the variable name into its pieces and reconstitute as the first 3 parts.
  name_parts<-as.data.frame((str_split_fixed(ecr$variable,':',5)))
  new_names<-paste(name_parts$V1, name_parts$V2, name_parts$V3, sep=":")
  ecr$summary_variable<-new_names
  
  #Summarize the data on variable name
  ecr_summarized<-ecr %>% 
    group_by(future_id, region, strategy_code, summary_variable) %>%
    summarise(summary_value= sum(value))
  
  #summarize the data across regions too
  ecr_summarized_lac<-ecr_summarized %>% 
    group_by(future_id, strategy_code, summary_variable) %>%
    summarise(summary_value= sum(summary_value))

  #paste into the lists
  country_summary_results<-append(country_summary_results, list(ecr_summarized))
  lac_summary_results<-append(lac_summary_results, list(ecr_summarized_lac))
}

#create a dataframe from the results
country_summary_table<-do.call("rbind", country_summary_results)
lac_summary_table<-do.call("rbind", lac_summary_results)

#Print the dataframes
write.csv(country_summary_table, country_summary_table_filename)
write.csv(lac_summary_table, lac_summary_table_filename)

