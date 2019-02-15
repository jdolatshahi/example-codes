## Ex. 1) Summary table ----

#create a summary table with subtotals and a grand total TOTAL
df2 <- subset(df, batch_year == '2018') %>%
  group_by(batch_year, batch_month, sites) %>%
  dplyr::summarise(total_recip = sum(total_recip),
                   total_kits = sum(total_kits)) %>%
  bind_rows(group_by(.,batch_year, batch_month) %>% #bind rows to create a sum total row
              summarise_at(vars(starts_with("total_")), .funs = sum) %>% #summarise everything starting with total_
              mutate(sites ='TOTAL')) %>% #rename a site to TOTAL
  arrange(batch_year, batch_month) %>% #sort
  mutate(sites = factor(sites, levels = c("Site 1", "Site 2", "Site 3", "TOTAL"))) %>%
  complete(sites) #ensures each site is listed, even if amount is 0

## Ex. 2) Create summary race ethnicity table ----

s %>% 
  group_by(race) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)*100) %>% #create proportion
  complete(race, fill = list(n = 0, prop = 0)) %>% #make the missings show as 0
  mutate(n = ifelse(race != 'Missing' & n >0 & n<5, NA, n), #if less than 5, suppress number with an NA
         prop = ifelse(race != 'Missing' & n >0 & n<5, NA, prop))


## Ex. 3) Create a function to make 2x2 summary tables ----
# EXAMPLE USES THE BRFSS DATASET CREATED IN 'Data cleaning.R'
#summary table from the BRFSS dataset, summarizing number of people by sex and age category
brfss %>% 
  group_by(sex, bmi_cat) %>% 
  summarise(n = n()) %>% 
  tidyr::spread(key = sex, value = n) #spreads sex across the top of the table


## create the dplyr function
sums <- function (df, ...) {
  require(dplyr)
  
  x_quos <- quos(...)
  
  df %>% 
    group_by(!!! x_quos) %>% 
    summarize(n = n())
  
}

sums(brfss, sex, bmi_cat) %>% 
  tidyr::spread(key = sex, value = n)

sums(brfss, genhlth, sex) %>% 
  tidyr::spread(key = sex, value = n)