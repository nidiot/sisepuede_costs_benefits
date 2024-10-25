#This file takes the summary costs in an experiment and (A) reallocates some of hte fuel cost back to the entc sector to make it complete and
#(B) summarizes all costs.

cb_summary<-read.csv('/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Vulnerability Analysis 10.25/cb_summary_lac.csv')

#(1) Create new variable that is entc-fuel-cost-savings and make it 30% of fuel costs and put it in a new variable
entc_fuel_cost<-cb_summary[grep('cb:enfu:fuel_cost', cb_summary$summary_variable),]
entc_fuel_cost$summary_value_entc<-0.3*entc_fuel_cost$summary_value
entc_fuel_cost$summary_variable<-'cb:entc:fuel_cost'

#(2)Reduce fuel cost savings to 70% of current value and put it in a new variable
cb_summary$summary_value_entc<-cb_summary$summary_value
cb_summary$summary_value_entc[grep('cb:enfu:fuel_cost', cb_summary$summary_variable)]<-0.7*cb_summary$summary_value[grep('cb:enfu:fuel_cost', cb_summary$summary_variable)]
cb_summary<-rbind(cb_summary, entc_fuel_cost)

#(3) summarieze all ccosts
cb_net<-cb_summary %>% 
  group_by(future_id, strategy_code) %>%
  summarise(net_benefit = sum(summary_value))

write.csv(cb_summary, '/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Vulnerability Analysis 10.25/cb_summary_lac.csv')
write.csv(cb_net, '/Users/nidhi/OneDrive - RAND Corporation/LAC Decarb QA Simulations/Vulnerability Analysis 10.25/cb_summary_lac_net_benefits.csv')