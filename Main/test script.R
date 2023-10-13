#discounting all costs from 2020 to 2050 @ 7% rate, discounted to the year 2019
discounted_value<-value / (1+0.07)^(2050 - 2019)

#discount the results
discounted_results_by_year<-cb_present_value_of_each_cost_unsummed(economy_wide_results, 0.07)
discounted_results_summed<-cb_present_value_of_each_cost(economy_wide_results, 0.07)

#get the crop/lvst values for Brazil from 2025-2050
crop_lvst_vals_bra<-discounted_results_summed[discounted_results_summed$region=='brazil'
                                              & discounted_results_summed$strategy_code=='PFLO:ALL_PLUR'
                                              & (grepl('crop_value', discounted_results_summed$variable) | grepl('lvst_value', discounted_results_summed$variable)),]

sum(crop_lvst_vals_bra$value)



#get the crop/lvst values for all countries from 2025-2050
crop_lvst_vals_all<-discounted_results_summed[discounted_results_summed$region!='honduras'
                                              & discounted_results_summed$strategy_code=='PFLO:ALL_PLUR'
                                              & (grepl('crop_value', discounted_results_summed$variable) | grepl('lvst_value', discounted_results_summed$variable)),]
sum(crop_lvst_vals_all$value)


all_cb_all_plur<-discounted_results_summed[
  (discounted_results_summed$region!='honduras') &
  discounted_results_summed$strategy_code=='PFLO:ALL_PLUR',]
sum(all_cb_all_plur$value)




#-----------------classify costs and benefits by category internally
#Reimplement tableau function for classifying costs and benefits
all_cb_all_plur$category<-''
for (i in 1:nrow(all_cb_all_plur)){
  if (grepl('air_pollution', all_cb_all_plur$variable[i]) | 
      grepl('land_pollution', all_cb_all_plur$variable[i]) |
      grepl('water_pollution', all_cb_all_plur$variable[i]) |
      grepl('env_pollution', all_cb_all_plur$variable[i])){
    all_cb_all_plur$category[i]<-'pollution'
  }
  
  else if (grepl('congestion', all_cb_all_plur$variable[i]) | 
            grepl('human_health', all_cb_all_plur$variable[i]) |
            grepl('road_safety', all_cb_all_plur$variable[i]) |
            grepl('consumer_savings', all_cb_all_plur$variable[i])){
    all_cb_all_plur$category[i]<-'human health'
  }
  
  else if (grepl('crop_value', all_cb_all_plur$variable[i]) | 
           grepl('lvst_value', all_cb_all_plur$variable[i])){
    all_cb_all_plur$category[i]<-'ag value'
  }
  
  else if (grepl('ippu_value', all_cb_all_plur$variable[i]) | 
           grepl('technical_savings', all_cb_all_plur$variable[i]) |
           grepl('system_cost', all_cb_all_plur$variable[i])){
    all_cb_all_plur$category[i]<-'sector savings'
  }
  
  
  
  else if (grepl('fuel_cost', all_cb_all_plur$variable[i])){
    all_cb_all_plur$category[i]<-'fuel cost'
  }
  
  else if (grepl('electricity', all_cb_all_plur$variable[i])){
    all_cb_all_plur$category[i]<-'electricity'
  
  }
  
  else if (grepl('ecosystem', all_cb_all_plur$variable[i])){
    all_cb_all_plur$category[i]<-'ecosystem services'
  }
  

  else if (grepl('technical_cost', all_cb_all_plur$variable[i])){
    all_cb_all_plur$category[i]<-'technical_cost'
  }

  else{ 
    all_cb_all_plur$category[i]<-'unknown'
  }
}

all_cb_all_plur_summarized<-all_cb_all_plur %>%
  group_by(category) %>%
  summarise(total_cb= sum(value))



all_cb_all_plur$category<-''
all_cb_all_plur$category[grepl('air_pollution', all_cb_all_plur$variable) | 
                             grepl('land_pollution', all_cb_all_plur$variable) |
                             grepl('water_pollution', all_cb_all_plur$variable) |
                             grepl('env_pollution', all_cb_all_plur$variable)]<-'pollution'


#POLLUTION
pollution<-all_cb_all_plur[grepl('air_pollution', all_cb_all_plur$variable) | 
                              grepl('land_pollution', all_cb_all_plur$variable) |
                              grepl('water_pollution', all_cb_all_plur$variable) |
                              grepl('env_pollution', all_cb_all_plur$variable),]
sum(pollution$value)
  
  
#HUMAN HEALTH
human_health_etc<-all_cb_all_plur[grepl('congestion', all_cb_all_plur$variable) | 
                                    grepl('human_health', all_cb_all_plur$variable) |
                                    grepl('road_safety', all_cb_all_plur$variable) |
                                    grepl('consumer_savings', all_cb_all_plur$variable),]
sum(human_health_etc$value)

#ECOSYSTEM
ecosystem<-all_cb_all_plur[grepl('ecosystem', all_cb_all_plur$variable),]
sum(ecosystem$value)


#FUEL COST
fuel_cost<-all_cb_all_plur[grepl('fuel_cost', all_cb_all_plur$variable),]
sum(fuel_cost$value)

#ELECTRICITY
electricity<-all_cb_all_plur[grepl('electricity', all_cb_all_plur$variable) & grepl('entc', all_cb_all_plur$variable),]
sum(electricity$value)


#SECTOR SAVINGS SLIGHTLY OFF
sector_savings<-all_cb_all_plur[grepl('ippu_value', all_cb_all_plur$variable) | 
                                  grepl('technical_savings', all_cb_all_plur$variable) |
                                  grepl('system_cost', all_cb_all_plur$variable),]
sum(sector_savings$value)



#CROP VALUE
crop_value<-all_cb_all_plur[grepl('crop_value', all_cb_all_plur$variable) | 
                                    grepl('lvst_value', all_cb_all_plur$variable),]
sum(crop_value$value)



#TECH COST
technical<-all_cb_all_plur[grepl('technical_cost', all_cb_all_plur$variable),]
sum(technical$value)




#(electricity breakdown)
electricity_capex<-all_cb_all_plur[grepl('electricity:capex', all_cb_all_plur$variable),]
sum(electricity_capex$value)

electricity_opex<-all_cb_all_plur[grepl('electricity:opex', all_cb_all_plur$variable),]
sum(electricity_opex$value)

electricity_transmission<-all_cb_all_plur[grepl('electricity:transmission', all_cb_all_plur$variable),]
sum(electricity_transmission$value)

electricity_loss<-all_cb_all_plur[grepl('loss_reduction:electricity', all_cb_all_plur$variable),]
sum(electricity_loss$value)



sector_savings_detail<-all_cb_all_plur[grepl('trns:technical_savings', all_cb_all_plur$variable) | grepl('trns:system_cost', all_cb_all_plur$variable),]
sum(sector_savings_detail$value)




