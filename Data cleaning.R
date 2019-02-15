## Examples of commonly used cleaning, manipulation, analysis codes ---- 
## All codes are just illustrative, but have no data attached (except for the BRFSS example)

## Ex. 1) Importing and cleaning a data set----

#import full 2016 BRFSS file as a SAS EXPORT (could be for any year with these variables)
library(Hmisc)
brfss <- sasxport.get("MMSA2016.xpt")

#check vars
names(brfss)

#find NY
table(brfss$mmsaname)

#subset for just NYC-NJ Metro area
ny <- brfss %>% 
  subset(mmsaname %in% 'New York-Jersey City-White Plains, NY-NJ, Metropolitan Division')

#look at structure of data
str(ny)

#select subset of variables
library(dplyr)
ny_vars <- ny %>% 
  dplyr::select(physhlth, menthlth, weight2,height3,x.bmi5,x.bmi5cat, x.age80, genhlth, sex, ecigaret)

str(ny_vars)

# 
stargazer::stargazer(ny_vars, type = "text")

brfss_class <- ny_vars %>% 
  mutate(physhlth = ifelse(88, 0,
                           ifelse(77, NA_real_,
                                  ifelse(99, NA_real_,
                                         as.numeric(physhlth)))),
         menthlth = ifelse(88, 0,
                           ifelse(77, NA_real_,
                                  ifelse(99, NA_real_,
                                         as.numeric(menthlth)))),
         weight = ifelse(weight2 <1000, as.numeric(weight2),
                         ifelse(weight2 == 7777, NA_real_,
                                ifelse(weight2 == 9999, NA_real_,
                                       ifelse(weight2 > 8999 & weight2 < 9999, weight2/2.20462, NA_real_
                                       ))
                         )),
         bmi = as.numeric(x.bmi5),
         bmi_cat = factor(x.bmi5cat),
         age = as.numeric(x.age80),
         genhlth = ifelse(genhlth %in% c(7,9), NA_real_, as.numeric(genhlth)),
         sex = ifelse(sex == 9, NA_real_, as.numeric(sex)),
         ecigaret = ifelse(ecigaret %in% c(7,9), NA_real_, as.numeric(ecigaret))
  ) %>% 
  dplyr::select(physhlth, menthlth, weight, bmi, bmi_cat, age, genhlth, sex, ecigaret)
str(brfss_class)

write.csv(brfss_class, "brfss_class.csv")

names(brfss)



## Ex. 2) turn select variables into character vars----
df2 <- df %>% # a handy pipe to connect them - read as 'then'
  mutate_at(.vars = vars(c("COUNT_DATE_MIN","SYMPTOMATIC","GENDER_BIRTH","BOROUGH", #select by name
                           "US_BORN","RACE","ETHNICITY","MAJOR_SITE_OF_DISEASE"),
                         starts_with("FINAL")), #select all that have similar names
            .funs= funs(as.character))



## Recoding a race/ethnicity free-text variable ----
library(stringr)

unique(df$RACE) #returns each potential value of a variable


new_df <- df %>% 
  mutate(race = case_when(
    str_detect(RACE, ', Unknown') ~ str_sub(RACE, 1, -10), # for all cateogries that end in ', Unknown',removes the unknown and assigns to first category
    str_detect(RACE, ",")       ~ 'Multiple races', #grabs all the race/ethnicity combos with a , - e.g., "black, white"
    !(str_detect(RACE, ","))    ~ as.character(RACE))
  )

#get frequency table of new race var
freq_table <- new_df %>% 
  group_by(race) %>% 
  dplyr::summarise(n = n()) %>% 
  data.frame()


# one multi race value not pulling because edited on first line of mutate, fix it below & add in ethnicity
new_df <- new_df %>% 
  mutate(race_eth = factor(
    case_when(ETHNICITY == 'Hispanic' & (is.na(race) | race %in% c("Other", "Unknown"))     ~ "Hispanic", #race is NA or (|) race is Other or Unknown
              ETHNICITY == 'Hispanic' & (!is.na(race) | !(race %in% c("Other", "Unknown"))) ~ "Multiple races/ethnicities", #race is NOT NA (!) or is not Other or Unknown
              race2 == 'Multiple races'                                                     ~ "Multiple races/ethnicities",
              str_detect(race, ",")                                                         ~ "Multiple races/ethnicities", # grab the one not being pulled in above
              ETHNICITY != 'Hispanic'                                                       ~ as.character(race2) 
    )
  ))



## Putting data manipulation all together ----

df_exp2 <- df_exp %>%
  setNames(tolower(names(.))) %>% #make all var names lowercase
  left_join(all_ids, by = "case_id") %>% #merge the datasets
  rename_at(.vars = vars(ends_with("_nqp")), # select all vars ending with _nqp
            .funs = funs(sub("[_]nqp$", "", .))) %>% # drop that ending
  dplyr::select(c(-facility_name.x, -facility_name_cqp, -facility_name_nc.y, -facility_name_nc.x)) %>% #keep everything but these (-)
  mutate_at(vars(date_provided), funs(as.Date)) %>% #change to a date var
  mutate(batch_year = as.character(year(date_provided)), #make a character variable out of the year portion of the date
         month = month(date_provided), 
         batch_month = ifelse(month < 10, paste0(0, as.character(month), sep = ""), as.character(month)),# add batch month variable
         agency = 'My Agency') %>%
  subset(subset = batch_year == '2018') %>% #only 2018 cases
  group_by(case_id, date_provided, recipient, facility_name.y, agency, batch_year, batch_month) %>%
  dplyr::summarise(total_recip = n_distinct(recipient), #counts by indiviual case #
                   total_kits = n()) #summarises counts of items received
#n_distinct is useful when you have multiple rows for the same person, e.g., a person received 2 items, but they are logged as 2 distinc rows,
# here I can tally the number of people accurately and the number of items

