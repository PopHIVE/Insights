##NOTE it seems that with Epic, estimates are reasonable for <2 year old,s but then the denominator collapses, and coverage rates shoot up

library(tidyverse)
library(arrow)

a1 <- read_csv('./data/mmr_over_time.csv', skip=11) %>%
  rename(age = "Age at Encounter in Years" ,
         geography = "State of Residence (U.S.)",
         year = "Year",
         pct_vax_epic = "...4",
         n_patients= "...5"                     ) %>%
  tidyr::fill(.,age, geography, .direction='down') %>%
  mutate(year = str_extract(year, "\\d{4}"),
         pct_vax_epic = gsub('%','', pct_vax_epic),
         pct_vax_epic = as.numeric(pct_vax_epic)
         )

#alternative definiion, uses all patients with an encounter as thebase
b1 <- read_csv('./data/raw/all_encounter_base.csv', skip=12) %>%
  rename(age = "Age at Encounter in Years" ,
         geography = "State of Residence",
         year = "Year",
         pct_vax_epic_alt = "...4"              ) %>%
  tidyr::fill(.,age, geography, .direction='down') %>%
  mutate(year = str_extract(year, "\\d{4}"),
         pct_vax_epic_alt = gsub('%','', pct_vax_epic_alt),
         pct_vax_epic_alt = as.numeric(pct_vax_epic_alt)
  )

c1 <- a1 %>% 
  full_join(b1, by=c('age','geography','year'))
  
  
schoolvax <- read_parquet("https://github.com/PopHIVE/Ingest/raw/refs/heads/main/data/bundle_childhood_immunizations/dist/overall_rates_by_source.parquet") %>%
  filter(vaccine=='MMR')

nis <- read_parquet("https://github.com/PopHIVE/Ingest/raw/refs/heads/main/data/bundle_childhood_immunizations/dist/overall_rates_by_source.parquet") %>%
  filter(vaccine=="≥1 Dose MMR" & age=='24 Months')

schoolvax_epic_compare <- c1 %>%
  filter(age=="? 4 and < 5 Years") %>%
  left_join(schoolvax, by=c('year','geography'))

nis_epic_compare <- c1 %>%
  filter(age=="? 1 and < 2 Years") %>%
  left_join(nis, by=c('year','geography'))

p1 <- ggplot(schoolvax_epic_compare)+
  geom_point(aes(x=pct_vax_epic, y=value, color=year, group=geography))+
  ylim(70,100)+
  xlim(70,100)+
  facet_wrap(~year) 
p1
plotly::ggplotly(p1)


p1 <- ggplot(schoolvax_epic_compare)+
  geom_point(aes(x=pct_vax_epic_alt, y=value, color=year, group=geography))+
  ylim(70,100)+
  xlim(70,100)+
  facet_wrap(~year) 
p1
plotly::ggplotly(p1)


p2<- nis_epic_compare %>%
  filter(!is.na(value)) %>%
  ggplot()+
  geom_point(aes(x=pct_vax_epic, y=value, color=year, group=geography))+
  ylim(80,100)+
  xlim(80,100) + 
  facet_wrap(~year) +
  geom_abline(intercept=0, slope=1)+
  ylab('NIS')
p2

plotly::ggplotly(p2)

p2<- nis_epic_compare %>%
  filter(!is.na(value)) %>%
  ggplot()+
  geom_point(aes(x=pct_vax_epic_alt, y=value, color=year, group=geography))+
  ylim(40,100)+
  xlim(40,100) + 
  facet_wrap(~year) +
  geom_abline(intercept=0, slope=1)+
  ylab('NIS')
p2

plotly::ggplotly(p2)

p3<- nis_epic_compare %>%
  filter(!is.na(value)) %>%
  mutate(year=as.numeric(year)) %>%
  ggplot()+
  geom_line(aes(x=year, y=value))+
  geom_line(aes(x=year, y=pct_vax_epic), color='red')+
  geom_line(aes(x=year, y=pct_vax_epic_alt), color='blue')+
  
    ylim(40,100)+
  geom_abline(intercept=0, slope=1)+
  facet_wrap(~geography, nrow=5)+
  ylab('Uptake') +
  theme_classic()
p3



tx <- read_csv('./data/tx_mmr_month.csv', skip=11) %>%
  rename(age = "Age at Encounter in Years" ,
         geography = "State of Residence",
         year = "Year",
         month='Month',
         pct_vax_epic = "...5",
         n_patients= "...6"                     ) %>%
  tidyr::fill(.,age, geography, year, month, .direction='down') %>%
  mutate(year = str_extract(year, "\\d{4}"),
         pct_vax_epic = gsub('%','', pct_vax_epic),
         pct_vax_epic = as.numeric(pct_vax_epic),
         month = iconv(month, from = "CP1252", to = "UTF-8"),
         month=substr(month,1,3),
         date= paste(year, month, '01', sep=' '),
         date = as.Date(date, format = "%Y %b %d"),
         
         age =  dplyr::case_when(
           grepl("Less than 1", age) ~ "<1",
           grepl("or more", age)     ~ "5+",
           TRUE ~ gsub("^\\?\\s*(\\d+) and < (\\d+) Years$", "\\1-\\2", age)
         ),
         age = paste(age, 'Years'),
        age =  factor(
           age,
           levels = c("<1 Years", "1-2 Years", "2-3 Years", "3-4 Years", "4-5 Years", "5+ Years"),
           ordered = TRUE
         )
  ) %>%
  filter(geography=='Texas' & age %in% c("<1 Years", "1-2 Years", "2-3 Years", "3-4 Years", "4-5 Years", "5+ Years") )

ggplot(tx) +
  geom_line(aes(x=date, y=pct_vax_epic)) +
  facet_wrap(~age, scales='free_y')+
  theme_classic()
