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


schoolvax <- read_parquet("https://github.com/PopHIVE/Ingest/raw/refs/heads/main/data/bundle_childhood_immunizations/dist/overall_rates_by_source.parquet") %>%
  filter(vaccine=='MMR')

nis <- read_parquet("https://github.com/PopHIVE/Ingest/raw/refs/heads/main/data/bundle_childhood_immunizations/dist/overall_rates_by_source.parquet") %>%
  filter(vaccine=="≥1 Dose MMR" & age=='24 Months')

schoolvax_epic_compare <- a1 %>%
  filter(age=="? 4 and < 5 Years") %>%
  left_join(schoolvax, by=c('year','geography'))


nis_epic_compare <- a1 %>%
  filter(age=="? 1 and < 2 Years") %>%
  left_join(nis, by=c('year','geography'))


p1 <- ggplot(schoolvax_epic_compare)+
  geom_point(aes(x=pct_vax_epic, y=value, color=year, group=geography))+
  ylim(70,100)+
  xlim(70,100)
p1
plotly::ggplotly(p1)


p2<- ggplot(nis_epic_compare)+
  geom_point(aes(x=pct_vax_epic, y=value, color=year, group=geography))+
  ylim(80,100)+
  xlim(80,100) + 
  facet_wrap(~year) +
  geom_abline(intercept=0, slope=1)+
  ylab('NIS')
p2

plotly::ggplotly(p2)
