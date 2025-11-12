library(patchwork)
library(dplyr)

combined_diabetes <- read_parquet('https://github.com/PopHIVE/Ingest/raw/refs/heads/new_epic_chronic_definition/data/bundle_chronic_diseases/dist/prevalence_by_geography_and_year_and_source.parquet')


comp1 <- pivot_wider(combined_diabetes, id_cols = c(geography, age, year, outcome_name), names_from=source, values_from=value) %>%
  filter(year==2023 & outcome_name =='Diabetes' & age =='65+ Years') 

p1 <- ggplot(comp1) +
  geom_point(aes(x=`Epic Cosmos: HbA1c`, y=`Epic Cosmos: ICD10`))+
  geom_abline(xintercept=0, slope=1)+
  ylim(0,35)+
  xlim(0,35)+
  theme_minimal() +
  ggtitle('Epic HbA1c vs CCW definition')

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
  geom_abline(xintercept=0, slope=1)+
  ylim(0,35)+
  xlim(0,35)+
  theme_minimal()+
  ggtitle('BRFSS vs Medicare FFS')

(p1+p2)/(p3+p4)