#10/19/23 -- Replace Peru fugitive emissions data for coal since this data was garbled.
data[data$region=='peru' & 
       data$strategy_code=='PFLO:ALL_PLUR' & 
       data$time_period %in% c(22, 23, 24), 
     c('emission_co2e_ch4_fgtv_fuel_coal')] <- data[data$region=='peru' & 
                                                      data$strategy_code=='PFLO:ALL_PLUR' & 
                                                      data$time_period %in% c(26), c('emission_co2e_ch4_fgtv_fuel_coal')]
data[data$region=='peru' & 
       data$strategy_code=='PFLO:ALL_PLUR' & 
       data$time_period %in% c(22, 23, 24), 
     c('emission_co2e_co2_fgtv_fuel_coal')] <- data[data$region=='peru' & 
                                                      data$strategy_code=='PFLO:ALL_PLUR' & 
                                                      data$time_period %in% c(26), c('emission_co2e_co2_fgtv_fuel_coal')]
data[data$region=='peru' & 
       data$strategy_code=='PFLO:ALL_PLUR' & 
       data$time_period %in% c(22, 23, 24), 
     c('emission_co2e_n2o_fgtv_fuel_coal')] <- data[data$region=='peru' & 
                                                      data$strategy_code=='PFLO:ALL_PLUR' & 
                                                      data$time_period %in% c(26), c('emission_co2e_n2o_fgtv_fuel_coal')]

