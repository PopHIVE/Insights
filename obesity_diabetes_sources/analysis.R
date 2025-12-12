library(patchwork)
library(arrow)
library(tidyverse)

combined_diabetes <- read_parquet('https://github.com/PopHIVE/Ingest/raw/refs/heads/new_epic_chronic_definition/data/bundle_chronic_diseases/dist/prevalence_by_geography_and_year_and_source.parquet')


comp1 <- pivot_wider(combined_diabetes, id_cols = c(geography, age, year, outcome_name), names_from=source, values_from=value) %>%
  filter(year==2023 & outcome_name =='Diabetes' & age =='65+ Years') 

p1 <- ggplot(comp1) +
  geom_point(aes(x=`Epic Cosmos: HbA1c`, y=`Epic Cosmos: ICD10`))+
  geom_abline(xintercept=0, slope=1)+
  ylim(0,35)+
  xlim(0,35)+
  theme_minimal() +
  ggtitle('Epic HbA1c vs Epic CCW definition')

p2 <- ggplot(comp1) +
  geom_point(aes(x=`CDC BRFSS`, y=`Epic Cosmos: ICD10`))+
  geom_abline(xintercept=0, slope=1)+
  ylim(0,35)+
  xlim(0,35)+
  theme_minimal() +
  ggtitle('Epic CCW definition vs BRFSS')


p3 <- ggplot(comp1) +
  geom_point(aes(x=`Medicare FFS`, y=`Epic Cosmos: ICD10`))+
  geom_abline(xintercept=0, slope=1)+
  ylim(0,35)+
  xlim(0,35)+
  theme_minimal()+
  ggtitle('Epic CCW definition vs Medicare FFS')

p4 <- ggplot(comp1) +
  geom_point(aes(x=`Medicare FFS`, y=`CDC BRFSS`))+
  geom_abline(intercept=0, slope=1)+
  ylim(0,35)+
  xlim(0,35)+
  theme_minimal()+
  ggtitle('BRFSS vs Medicare FFS')

(p1+p2)/(p3+p4)

#
comp2 <- pivot_wider(combined_diabetes, id_cols = c(geography, age, year, outcome_name), names_from=source, values_from=value) 
  

combined_diabetes %>%
  filter(age =='Total' & source == "Epic Cosmos: HbA1c" & outcome_name=='Diabetes' & geography %in% c('New York','Mississippi', 'Florida', 'Michigan','Ohio')) %>%
  ggplot()+
  geom_line(aes(x=year, y=value, group=geography, color=geography))+
  theme_minimal()

combined_diabetes %>%
  filter(age =='Total' & source == "Epic Cosmos: ICD10" & outcome_name=='Diabetes'& geography %in% c('New York','Mississippi', 'Florida', 'Michigan','Ohio')) %>%
  ggplot()+
  geom_line(aes(x=year, y=value, group=geography, color=geography))+
  theme_minimal()+
  ylim(10,17)

combined_diabetes %>%
  filter(age =='Total' & source == "CDC BRFSS" & outcome_name=='Diabetes'& geography %in% c('New York','Mississippi', 'Florida', 'Michigan','Ohio')) %>%
  ggplot()+
  geom_line(aes(x=year, y=value, group=geography, color=geography))+
  theme_minimal()+
  ylim(10,17)



combined_diabetes %>%
  filter(age =='Total' & source %in% c("CDC BRFSS","Epic Cosmos: ICD10") & outcome_name=='Diabetes'& geography %in% c('New York','Mississippi', 'Florida', 'Michigan','Ohio')) %>%
  ggplot()+
  geom_line(aes(x=year, y=value, group=source, color=source))+
  theme_minimal()+
  ylim(10,20) +
  facet_wrap(~geography)

combined_diabetes %>%
  filter(age =='Total' & source %in% c("CDC BRFSS","Epic Cosmos: ICD10") & outcome_name=='Diabetes'& geography %in% c('United States')) %>%
  ggplot()+
  geom_line(aes(x=year, y=value, group=source, color=source))+
  geom_errorbar(aes(x=year, ymin=value_lcl, ymax=value_ucl, color=source), width=0)+
  theme_minimal()+
  ylim(10,20) 


