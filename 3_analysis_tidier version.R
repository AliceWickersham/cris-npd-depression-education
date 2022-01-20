####LIBRARIES####
#Note that some of the packages loaded below may not be used in this script, but they contain functions which I have found useful in exploring the data.
library(pacman)
pacman::p_load(janitor,
               dplyr,
               tidyr,
               tidyverse,
               lubridate,
               foreign,
               gmodels,
               mice,
               Matrix,
               MASS,
               survey,
               AF,
               lmtest,
               purrr,
               stdReg)

####ELIGIBILIITY####
npd_cris <- npd_cris %>% mutate(eligible=ifelse((local_borough==1 
                                                 & !is.na(local_borough)
                                                 & !is.na(ks4_level2_em) 
                                                 & ks4_age_start>=15
                                                 & ks2_matlev!="A"
                                                 & ks2_matlev!="D"
                                                 & ks2_englev!="A"
                                                 & ks2_englev!="D"),
                                                "Yes", "No"))

####GENDER####
npd_cris <- npd_cris %>% 
  mutate(gender = case_when(
    ks1_gender == "M" ~ "Male",
    ks1_gender == "F" ~ "Female",
    ks2_gender == "M" ~ "Male",
    ks2_gender == "F" ~ "Female",
    ks4_gender == "M" ~ "Male",
    ks4_gender == "F" ~ "Female",
    gender_ab06 == "M" ~ "Male",
    gender_ab06 == "F" ~ "Female",
    gender_ab07 == "M" ~ "Male",
    gender_ab07 == "F" ~ "Female",
    gender_ab08 == "M" ~ "Male",
    gender_ab08 == "F" ~ "Female",
    gender_ab09 == "M" ~ "Male",
    gender_ab09 == "F" ~ "Female",
    gender_ab10 == "M" ~ "Male",
    gender_ab10 == "F" ~ "Female",
    gender_ab11 == "M" ~ "Male",
    gender_ab11 == "F" ~ "Female",
    gender_ab12 == "M" ~ "Male",
    gender_ab12 == "F" ~ "Female",
    gender_ab13 == "M" ~ "Male",
    gender_ab13 == "F" ~ "Female",
    gender_ab14 == "M" ~ "Male",
    gender_ab14 == "F" ~ "Female",
    cris_Gender_ID == "Male" ~ "Male",
    cris_Gender_ID == "Female" ~ "Female",
  ))

npd_cris <- npd_cris %>% mutate(gender=factor(gender))

####ETHNICITY####
npd_cris <- npd_cris %>% 
  mutate(ethnicity = case_when(
    ethnic_group == "White" ~ "a_White",
    ethnic_group == "Black" ~ "b_Black",
    ethnic_group == "Asian" | 
      ethnic_group == "Mixed" | 
      ethnic_group == "Any other ethnic group" | 
      ethnic_group == "Chinese" ~ "c_Other",
    cris_ethnicitycleaned == "British (A)" | 
      cris_ethnicitycleaned == "Irish (B)" | 
      cris_ethnicitycleaned == "Any other white background (C)" ~ "a_White",
    cris_ethnicitycleaned == "Caribbean (M)" | 
      cris_ethnicitycleaned == "African (N)" | 
      cris_ethnicitycleaned == "Any other black background (P)" ~ "b_Black",
    cris_ethnicitycleaned == "Indian (H)" | 
      cris_ethnicitycleaned == "Pakistani (J)" | 
      cris_ethnicitycleaned == "Bangladeshi (K)" | 
      cris_ethnicitycleaned == "Any other Asian background (L)" | 
      cris_ethnicitycleaned == "Any other mixed background (G)" | 
      cris_ethnicitycleaned == "White and Asian (F)" | 
      cris_ethnicitycleaned == "White and black Caribbean (D)" | 
      cris_ethnicitycleaned == "White and Black African (E)" | 
      cris_ethnicitycleaned == "Chinese (R)" |
      cris_ethnicitycleaned == "Any other ethnic group (S)" ~ "c_Other",
  ))

npd_cris <- npd_cris %>% mutate(ethnicity=factor(ethnicity))

#####FSM####
npd_cris <- npd_cris %>% 
  mutate(fsm=case_when(census_fsmeligible==1 | 
                          census_everfsm_6==1 |
                          fsmeligible_ab==1 |
                          fsmeligible_ab06==1 |
                          fsmeligible_ab07==1 |
                          fsmeligible_ab08==1 |
                          fsmeligible_ab09==1 |
                          fsmeligible_ab10==1 |
                          fsmeligible_ab11==1 |
                          fsmeligible_ab12==1 |
                          fsmeligible_ab13==1 |
                          fsmeligible_ab14==1 ~ "Yes",
                        is.na(census_fsmeligible) & 
                          is.na(census_everfsm_6) &
                          is.na(fsmeligible_ab) &
                          is.na(fsmeligible_ab06) &
                          is.na(fsmeligible_ab07) &
                          is.na(fsmeligible_ab08) &
                          is.na(fsmeligible_ab09) &
                          is.na(fsmeligible_ab10) &
                          is.na(fsmeligible_ab11) &
                          is.na(fsmeligible_ab12) &
                          is.na(fsmeligible_ab13) &
                          is.na(fsmeligible_ab14) ~ NA_character_ ,
                        TRUE ~ "No"))

npd_cris <- npd_cris %>% mutate(fsm=factor(fsm))

####KEY STAGE 2####
npd_cris <- npd_cris %>% 
  mutate(ks2_eng_4=case_when(ks2_englev=="B" | 
                               ks2_englev=="T" |
                               ks2_englev=="A" |
                               ks2_englev=="N" |
                               ks2_englev=="Q" |
                               ks2_englev=="2" |
                               ks2_englev=="3" ~ "No",
                             ks2_englev=="4" | 
                               ks2_englev=="5" |
                               ks2_englev=="6" ~ "Yes"))

npd_cris <- npd_cris %>% 
  mutate(ks2_mat_4=case_when(ks2_matlev=="B" | 
                               ks2_matlev=="T" |
                               ks2_matlev=="A" |
                               ks2_matlev=="N" |
                               ks2_matlev=="Q" |
                               ks2_matlev=="2" |
                               ks2_matlev=="3" ~ "No",
                             ks2_matlev=="4" | 
                               ks2_matlev=="5" |
                               ks2_matlev=="6" ~ "Yes"))                             

npd_cris$ks2_mat_4[npd_cris$ks2_slt_app==1 & npd_cris$ks2_matlev=="N"] <- NA

npd_cris <- npd_cris %>% 
  mutate(ks2_total_4=case_when(ks2_eng_4=="Yes" & ks2_mat_4=="Yes" ~ "Yes",
                               ks2_eng_4=="No" | ks2_mat_4=="No" ~ "No"))  

npd_cris <- npd_cris %>% mutate(ks2_total_4=factor(ks2_total_4))

####RELATIVE AGE####
npd_cris <- npd_cris %>% mutate(birthmonth = coalesce(census_monthofbirth,
                                                      ks1_monthofbirth,
                                                      ks2_monthofbirth,
                                                      ks4_monthofbirth,
                                                      monthofbirth_ab,
                                                      monthofbirth_ab06,
                                                      monthofbirth_ab07,
                                                      monthofbirth_ab08,
                                                      monthofbirth_ab09,
                                                      monthofbirth_ab10,
                                                      monthofbirth_ab11,
                                                      monthofbirth_ab12,
                                                      monthofbirth_ab13,
                                                      monthofbirth_ab13,
                                                      month(as.Date(cris_cleaneddateofbirth))))


npd_cris <- npd_cris %>% mutate(relative_age = case_when(birthmonth>=9 & birthmonth<=12 ~ "a_Autumn",
                                                         birthmonth>=1 & birthmonth<=4 ~ "a_Spring",
                                                         birthmonth>=5 & birthmonth<=8 ~ "a_Summer"))
                                  
npd_cris <- npd_cris %>% mutate(relative_age=factor(relative_age))

####NEURODEVELOPMENTAL DISORDER####
npd_cris <- npd_cris %>% mutate(neuro = case_when(cris_id_ever==1 ~ "Yes",
                                                  cris_pdd_ever==1 ~ "Yes",
                                                  cris_adhd_ever==1 ~ "Yes",
                                                  TRUE ~ "No"))

npd_cris <- npd_cris %>% mutate(neuro=factor(neuro))

####DATE FORMATTING####
#Birth year
npd_cris <- npd_cris %>% mutate(ks1_acadyr_num = case_when(ks1_acadyr=="1997/1998" ~ 1997,
                                                           ks1_acadyr=="1998/1999" ~ 1998,
                                                           ks1_acadyr=="1999/2000" ~ 1999,
                                                           ks1_acadyr=="2000/2001" ~ 2000,
                                                           ks1_acadyr=="2001/2002" ~ 2001,
                                                           ks1_acadyr=="2002/2003" ~ 2002,
                                                           ks1_acadyr=="2003/2004" ~ 2003,
                                                           ks1_acadyr=="2004/2005" ~ 2004,
                                                           ks1_acadyr=="2005/2006" ~ 2005,
                                                           ks1_acadyr=="2006/2007" ~ 2006,
                                                           ks1_acadyr=="2007/2008" ~ 2007,
                                                           ks1_acadyr=="2008/2009" ~ 2008,
                                                           ks1_acadyr=="2009/2010" ~ 2009,
                                                           ks1_acadyr=="2010/2011" ~ 2010,
                                                           ks1_acadyr=="2011/2012" ~ 2011,
                                                           ks1_acadyr=="2012/2013" ~ 2012,
                                                           ks1_acadyr=="2013/2014" ~ 2013))

npd_cris <- npd_cris %>% mutate(birth_year_ks1 = ifelse((ks1_monthofbirth>=9 & !is.na(ks1_monthofbirth)), 
                                                        ks1_acadyr_num - ks1_age_start - 1, 
                                                        ks1_acadyr_num - ks1_age_start))

npd_cris <- npd_cris %>% mutate(ks2_acadyr_num = case_when(ks2_acadyr=="1997/1998" ~ 1997,
                                                           ks2_acadyr=="1998/1999" ~ 1998,
                                                           ks2_acadyr=="1999/2000" ~ 1999,
                                                           ks2_acadyr=="2000/2001" ~ 2000,
                                                           ks2_acadyr=="2001/2002" ~ 2001,
                                                           ks2_acadyr=="2002/2003" ~ 2002,
                                                           ks2_acadyr=="2003/2004" ~ 2003,
                                                           ks2_acadyr=="2004/2005" ~ 2004,
                                                           ks2_acadyr=="2005/2006" ~ 2005,
                                                           ks2_acadyr=="2006/2007" ~ 2006,
                                                           ks2_acadyr=="2007/2008" ~ 2007,
                                                           ks2_acadyr=="2008/2009" ~ 2008,
                                                           ks2_acadyr=="2009/2010" ~ 2009,
                                                           ks2_acadyr=="2010/2011" ~ 2010,
                                                           ks2_acadyr=="2011/2012" ~ 2011,
                                                           ks2_acadyr=="2012/2013" ~ 2012,
                                                           ks2_acadyr=="2013/2014" ~ 2013))

npd_cris$ks2_age_start[npd_cris$ks2_age_start==0 | npd_cris$ks2_age_start==39 | npd_cris$ks2_age_start==100] <- NA
npd_cris <- npd_cris %>% mutate(birth_year_ks2 = ifelse((ks2_monthofbirth>=9 & !is.na(ks2_monthofbirth)), 
                                                        ks2_acadyr_num - ks2_age_start - 1, 
                                                        ks2_acadyr_num - ks2_age_start))

npd_cris <- npd_cris %>% mutate(ks4_acadyr_num = case_when(ks4_acadyr=="1997/1998" ~ 1997,
                                                           ks4_acadyr=="1998/1999" ~ 1998,
                                                           ks4_acadyr=="1999/2000" ~ 1999,
                                                           ks4_acadyr=="2000/2001" ~ 2000,
                                                           ks4_acadyr=="2001/2002" ~ 2001,
                                                           ks4_acadyr=="2002/2003" ~ 2002,
                                                           ks4_acadyr=="2003/2004" ~ 2003,
                                                           ks4_acadyr=="2004/2005" ~ 2004,
                                                           ks4_acadyr=="2005/2006" ~ 2005,
                                                           ks4_acadyr=="2006/2007" ~ 2006,
                                                           ks4_acadyr=="2007/2008" ~ 2007,
                                                           ks4_acadyr=="2008/2009" ~ 2008,
                                                           ks4_acadyr=="2009/2010" ~ 2009,
                                                           ks4_acadyr=="2010/2011" ~ 2010,
                                                           ks4_acadyr=="2011/2012" ~ 2011,
                                                           ks4_acadyr=="2012/2013" ~ 2012,
                                                           ks4_acadyr=="2013/2014" ~ 2013))

npd_cris <- npd_cris %>% mutate(birth_year_ks4 = ifelse((ks4_monthofbirth>=9 & !is.na(ks4_monthofbirth)), 
                                                        ks4_acadyr_num - ks4_age_start - 1, 
                                                        ks4_acadyr_num - ks4_age_start))


npd_cris <- npd_cris %>% mutate(birth_year_census = year(as.Date(censusderiveddob)))
npd_cris <- npd_cris %>% mutate(birth_year_cris = year(as.Date(cris_cleaneddateofbirth)))

npd_cris <- npd_cris %>% 
  rowwise() %>% 
  mutate(birth_year = floor(0.5 + (mean(c(birth_year_census, birth_year_cris, birth_year_ks1, birth_year_ks2, birth_year_ks4), na.rm=TRUE))))
  
#Creating date of birth variable
npd_cris <- npd_cris %>% 
  mutate(dob = paste(birthmonth, "/", 01, "/", birth_year, sep=""))
npd_cris$dob[is.na(npd_cris$birthmonth) | is.na(npd_cris$birth_year)] <- NA
npd_cris <- npd_cris %>% 
  mutate(dob = as.Date(as.character(dob), 
                       tryFormats = c("%m/%d/%Y"), 
                       optional=FALSE))

#Formatting diagnosis dates
#F32
npd_cris$cris_f32_diagnosis_date[npd_cris$cris_f32_diagnosis_date=="NULL"] <- NA
npd_cris <- npd_cris %>% 
  mutate(cris_f32_diagnosis_date_formatted = as.Date(as.character(cris_f32_diagnosis_date), 
                                                     tryFormats = c("%m/%d/%Y"), 
                                                     optional=FALSE))

npd_cris$f32_age <- interval(npd_cris$dob, 
                             npd_cris$cris_f32_diagnosis_date_formatted) %/% years(1)

#F33
npd_cris$cris_f33_diagnosis_date[npd_cris$cris_f33_diagnosis_date=="NULL"] <- NA
npd_cris <- npd_cris %>% 
  mutate(cris_f33_diagnosis_date_formatted = as.Date(as.character(cris_f33_diagnosis_date), 
                                                      tryFormats = c("%m/%d/%Y"), 
                                                      optional=FALSE))

npd_cris$f33_age <- interval(npd_cris$dob, 
                             npd_cris$cris_f33_diagnosis_date_formatted) %/% years(1)

####DEPRESSION####
#F32
npd_cris <- npd_cris %>% mutate(f32=ifelse((cris_f32_diagnosis==1
                                            & !is.na(cris_f32_diagnosis)
                                            & f32_age<=14),
                                           1, 0))

#F33
npd_cris <- npd_cris %>% mutate(f33=ifelse((cris_f33_diagnosis==1
                                            & !is.na(cris_f33_diagnosis)
                                            & f33_age<=14),
                                           1, 0))

#F32 or F33
npd_cris <- npd_cris %>% mutate(dep=ifelse((f32==1
                                            | f33==1),
                                           1, 0))

npd_cris <- npd_cris %>% mutate(dep=factor(dep))

#Depression age
npd_cris <- npd_cris %>% mutate(f32_age_ifdep=ifelse(dep==1 ,
                                                     f32_age, NA_real_))

npd_cris <- npd_cris %>% mutate(f33_age_ifdep=ifelse(dep==1 ,
                                                     f33_age, NA_real_))

npd_cris <- npd_cris %>% mutate(dep_first_age=case_when(f33_age_ifdep<=f32_age_ifdep & !is.na(f33_age_ifdep) ~ f33_age_ifdep,
                                                        is.na(f32_age_ifdep) ~ f33_age_ifdep,
                                                        TRUE ~ f32_age_ifdep))

tabyl(npd_cris$dep_first_age)

####GCSE OUTCOME####
npd_cris <- npd_cris %>% mutate(ks4_level2_em=factor(ks4_level2_em))

####SUBSETTING ELIGIBLE COHORT####
eligible <- subset(npd_cris, eligible=="Yes", select=c(dep, ks4_level2_em, gender, ethnicity, fsm, relative_age, neuro, ks2_total_4, birth_year, dep_first_age))

####SUBSETTING COMPLETE RECORDS####
eligible <- eligible %>% mutate(complete_cases=complete.cases(dep, ks4_level2_em, gender, ethnicity, fsm, relative_age, neuro, ks2_total_4))
complete_cases <- subset(eligible, complete_cases==TRUE)

####RESULTS####
####Table S2####
table_missing <- function(df,v1,v2) {
  out <- df %>% 
    tabyl(!! rlang::sym(v1), !! rlang::sym(v2), show_missing_levels = TRUE, show_na = TRUE) %>%
    adorn_totals(where = "col") %>% 
    adorn_percentages(denominator = "col") %>% 
    adorn_pct_formatting() %>%
    adorn_ns(position = "front")
  return(out)
}

map(c('dep', 'gender', 'ethnicity', 'fsm', 'relative_age', 'neuro', 'ks2_total_4'), 
    table_missing, 
    df = eligible, 
    v2 = 'ks4_level2_em')

####Figure S1####
nrow(npd_cris)
sum(npd_cris$local_borough==0 | is.na(npd_cris$local_borough), na.rm=TRUE)
sum(is.na(npd_cris$ks4_level2_em) , na.rm=TRUE)
sum(npd_cris$ks4_age_start<15 , na.rm=TRUE)
sum(npd_cris$ks2_matlev=="A" | npd_cris$ks2_matlev=="D" | npd_cris$ks2_englev=="A" | npd_cris$ks2_englev=="D", na.rm=TRUE)
sum(npd_cris$eligible=="No" , na.rm=TRUE)
sum(npd_cris$eligible=="Yes" , na.rm=TRUE)

sum(is.na(eligible$ethnicity), na.rm=TRUE)
sum(is.na(eligible$fsm), na.rm=TRUE)
sum(is.na(eligible$ks2_total_4), na.rm=TRUE)
table(eligible$complete_cases)

####Figure S2####
nrow(subset(npd_cris, dep==1))
nrow(subset(npd_cris, dep==1 & eligible=="No"))
nrow(subset(npd_cris, dep==1 & eligible=="Yes"))
nrow(subset(eligible, dep==1 & complete_cases==FALSE))
nrow(subset(eligible, dep==1 & complete_cases==TRUE))

nrow(subset(npd_cris, dep==0))
nrow(subset(npd_cris, dep==0 & eligible=="No"))
nrow(subset(npd_cris, dep==0 & eligible=="Yes"))
nrow(subset(eligible, dep==0 & complete_cases==FALSE))
nrow(subset(eligible, dep==0 & complete_cases==TRUE))

####Table S3####
table_no_missing <- function(df,v1,v2) {
  out <- df %>% 
    tabyl(!! rlang::sym(v1), !! rlang::sym(v2), show_missing_levels = FALSE, show_na = FALSE) %>%
    adorn_totals(where = "col") %>% 
    adorn_percentages(denominator = "col") %>% 
    adorn_pct_formatting() %>%
    adorn_ns(position = "front")
  return(out)
}

map(c('dep', 'ks4_level2_em', 'gender', 'ethnicity', 'fsm', 'relative_age', 'neuro', 'ks2_total_4'), 
    table_no_missing, 
    df = npd_cris, 
    v2 = 'eligible')

map(c('dep', 'ks4_level2_em', 'gender', 'ethnicity', 'fsm', 'relative_age', 'neuro', 'ks2_total_4'), 
    table_missing, 
    df = npd_cris, 
    v2 = 'eligible')

map(c('dep', 'ks4_level2_em', 'gender', 'ethnicity', 'fsm', 'relative_age', 'neuro', 'ks2_total_4'), 
    table_no_missing, 
    df = eligible, 
    v2 = 'complete_cases')

map(c('dep', 'ks4_level2_em', 'gender', 'ethnicity', 'fsm', 'relative_age', 'neuro', 'ks2_total_4'), 
    table_missing, 
    df = eligible, 
    v2 = 'complete_cases')

####Table 1####
map(c('dep', 'gender', 'ethnicity', 'fsm', 'relative_age', 'neuro', 'ks2_total_4'), 
    table_no_missing, 
    df = complete_cases, 
    v2 = 'ks4_level2_em')

####Table S4####
map(c('ks4_level2_em', 'gender', 'ethnicity', 'fsm', 'relative_age', 'neuro', 'ks2_total_4'), 
    table_no_missing, 
    df = complete_cases, 
    v2 = 'dep')

####Table 2####
#Generating inverse probability weights
complete_cases_reg <- glm(complete_cases ~ dep + ks4_level2_em + gender + relative_age + neuro + birth_year, data=eligible, family="binomial")
summary(complete_cases_reg)
exp(cbind(OR=coef(complete_cases_reg), confint(complete_cases_reg)))

eligible$prob <- predict(complete_cases_reg, type = "response")
eligible$invp=1/eligible$prob
complete_cases <- subset(eligible, complete_cases==TRUE)

#Logistic regression analyses
complete_cases_weighted <- svydesign(id=~1, weights=~invp, data=complete_cases)

model_1 <- svyglm(ks4_level2_em ~ dep, design=complete_cases_weighted, family="binomial")
summary(model_1)
exp(cbind(OR=coef(model_1), confint(model_1)))

model_2 <- svyglm(ks4_level2_em ~ dep + gender + ethnicity + fsm + relative_age + neuro, design=complete_cases_weighted, family="binomial")
summary(model_2)
exp(cbind(OR=coef(model_2), confint(model_2)))

model_3 <- svyglm(ks4_level2_em ~ dep + gender + ethnicity + fsm + relative_age + neuro + ks2_total_4, design=complete_cases_weighted, family="binomial")
summary(model_3)
exp(cbind(OR=coef(model_3), confint(model_3)))

####Interaction results####
#Gender
interaction_gender <- svyglm(ks4_level2_em ~ dep + gender + ethnicity + fsm + relative_age + neuro + ks2_total_4 + dep*gender, design=complete_cases_weighted, family="binomial")
summary(interaction_gender)
exp(cbind(OR=coef(interaction_gender), confint(interaction_gender)))
regTermTest(interaction_gender, ~dep:gender, df=Inf)

#Ethnicity
interaction_ethnicity <- svyglm(ks4_level2_em ~ dep + gender + ethnicity + fsm + relative_age + neuro + ks2_total_4 + dep*ethnicity, design=complete_cases_weighted, family="binomial")
summary(interaction_ethnicity)
exp(cbind(OR=coef(interaction_ethnicity), confint(interaction_ethnicity)))
regTermTest(interaction_ethnicity, ~dep:ethnicity, df=Inf)

#FSM
interaction_fsm <- svyglm(ks4_level2_em ~ dep + gender + ethnicity + fsm + relative_age + neuro + ks2_total_4 + dep*fsm, design=complete_cases_weighted, family="binomial")
summary(interaction_fsm)
exp(cbind(OR=coef(interaction_fsm), confint(interaction_fsm)))
regTermTest(interaction_fsm, ~dep:fsm, df=Inf)
