library(tidyverse)
library(haven)
library(labelled)
library(table1)
library(MASS)
library(lubridate)
library(VIM)
library(WeightIt)
library(survey)
library(mice)
library(epiDisplay)
library(jtools)
library(ggstance)


idlist= read_dta('C:/metabo_check/daicho202003_personalnum.dta')
screenings = read_dta('C:/metabo_check/screening_long_v2.dta') %>% 
  mutate_at(vars(u_g, u_pro), 
            funs(as.integer(case_when(
              . == "－" ~ "1",
              . == "±" ~ "2", 
              . == "１＋" ~ "3",
              . == "２＋" ~"4",
              . == "３＋" ~ "5",
              . == "４＋" ~"6",
              . == "" ~  NA_character_)))) %>%
  mutate_at(vars(drug_ht_yn, drug_dm_yn, drug_hl_yn, past_stroke_yn, past_cardio_yn, past_renal_yn,
                 past_anemia_yn, smoking_yn, increse_bw_20_yn, ex_habits_yn, walking_yn,
                 speed_walk_yn, increase_bw_1y_yn, eat_habits_bed_yn, eat_habits_snack_yn,
                 breakfast_def_habits_yn, sleeping_yn),
            funs(as.integer(case_when(
              . == "いいえ" ~ "2", 
              . == "はい"  ~ "1",
              . == "" ~ NA_character_)))) %>%
  mutate(alcohol_habits = as.integer(
    case_when(
      alcohol_habits == "ほとんど飲まない"~ "3",
      alcohol_habits == "時々" ~ "2",
      alcohol_habits == "毎日" ~ "1",
      alcohol_habits == "" ~ NA_character_)),
    alcohol_amount = as.integer(
      case_when(
        alcohol_amount == "１合未満" ~ "1",
        alcohol_amount == "１～２合未満" ~ "2",
        alcohol_amount == "２～３合未満" ~ "3", 
        alcohol_amount == "３合以上" ~ "4",
        alcohol_amount == "" ~ NA_character_)),
    change_habits = as.integer(
      case_when(
        change_habits == "意志なし" ~ "1", 
        change_habits == "意志あり（６か月以内）" ~ "2",
        change_habits == "意志あり（近いうち）" ~ "3", 
        change_habits =="取組済み（６か月未満）" ~ "4", 
        change_habits =="取組済み（６か月以上）" ~ "5",
        change_habits == "" ~ NA_character_))
  ) %>%
  mutate(vis_year = case_when(vis_ymd <= 20140331 ~ 2013,
                              vis_ymd <= 20150331 ~ 2014,
                              vis_ymd <= 20160331 ~ 2015,
                              vis_ymd <= 20170331 ~ 2016,
                              vis_ymd <= 20180331 ~ 2017,
                              vis_ymd <= 20190331 ~ 2018,
                              vis_ymd <= 20200331 ~ 2019))

instructions = read_dta('C:/metabo_check/instruction_long.dta') %>% 
  mutate(vis_year = case_when(guide_ymd <= 20140331 ~ 2013,
                              guide_ymd <= 20150331 ~ 2014,
                              guide_ymd <= 20160331 ~ 2015,
                              guide_ymd <= 20170331 ~ 2016,
                              guide_ymd <= 20180331 ~ 2017,
                              guide_ymd <= 20190331 ~ 2018,
                              guide_ymd <= 20200331 ~ 2019))
clinics = read_dta('C:/metabo_check/MED_DPC_append.dta') %>% 
  mutate(vis_year = case_when(prac_ym <= 201403 ~ 2013,
                              prac_ym <= 201503 ~ 2014,
                              prac_ym <= 201603 ~ 2015,
                              prac_ym <= 201703 ~ 2016,
                              prac_ym <= 201803 ~ 2017,
                              prac_ym <= 201903 ~ 2018,
                              prac_ym <= 202003 ~ 2019))

########################Data processing####################################################

df = screenings %>%
  dplyr::select(personalnum, vis_ymd, vis_year) %>%
  group_by(personalnum) %>%
  distinct(vis_year, .keep_all = TRUE) %>% #choose first observation for each year
  ungroup(personalnum)

df2 = df %>%
  left_join(idlist %>% dplyr::select(personalnum, id, birthday), by = 'personalnum') %>%
  left_join(clinics %>% dplyr::select(id, prac_ym), by = "id") %>%
  mutate(vis_ymd = as.Date(as.character(vis_ymd), format = "%Y%m%d"),
         prac_ym = as.Date(paste(as.character(prac_ym), "01"),format = "%Y%m%d") %m+% months(1) -1,
         gapdays = prac_ym - vis_ymd,
         clinic1Y = ifelse(gapdays >= 0 & gapdays < 365, 1, 0)) %>%
  group_by(personalnum, vis_year, vis_ymd, id, birthday) %>%
  summarize(clinic1Y = max(clinic1Y)) %>%
  ungroup(personalnum, vis_year, vis_ymd, id, birthday) %>%
  dplyr::select(-id) %>%
  left_join(instructions %>% dplyr::select(personalnum, guide_ymd), by = "personalnum") %>%
  mutate(guide_ymd = as.Date(as.character(guide_ymd), format = "%Y%m%d"),
         gapdays = guide_ymd - vis_ymd,
         guide1Y = ifelse(gapdays >= 0 & gapdays < 365, 1, 0)) %>%
  group_by(personalnum, vis_year, vis_ymd, birthday,  
           clinic1Y) %>%
  summarize(guide1Y = max(guide1Y)) %>% 
  ungroup(personalnum, vis_year, vis_ymd, birthday, 
          clinic1Y) %>%
  replace_na(list(clinic1Y = 0, guide1Y = 0)) %>%
  left_join(screenings %>%
              mutate(vis_ymd = as.Date(as.character(vis_ymd), format = "%Y%m%d")) %>%
              dplyr::select(-vis_year), by = c("personalnum", "vis_ymd")) %>%
  mutate(birthday = as.Date(as.character(birthday), format = "%Y%m%d"),
         age = as.integer((vis_ymd - birthday)/365),
         female = as.factor(sex_div - 1),
         clinic1Y = as.factor(clinic1Y),
         guide1Y = as.factor(guide1Y),
         on_drugs = ifelse((drug_dm_yn - 1)*(drug_hl_yn - 1)*(drug_ht_yn -1) == 1, 0, 1)) %>%
  mutate(waist_criterion = ifelse((waist>=85 & female == 0) | (waist>=90 & female == 1),1,0),
         glu_criterion = case_when(fbg>=100 ~ 1,
                                   hba1c_ngsp>=5.6 ~ 1,
                                   hba1c_ngsp < 5.6 ~ 0,
                                   fbg <100 ~ 0),
         dlp_criterion = ifelse(tg>=150 | hdl<40 ,1,0),
         htn_criterion = ifelse(sbp>=130 | dbp>=85 , 1,0),
         num_risks = glu_criterion + dlp_criterion + htn_criterion,
         calculated_class_advice = case_when(
           waist_criterion ==1 & num_risks >=2 & age>=40 & age<=64 ~ 1,
           waist_criterion ==1 & num_risks >=2 & age>=65 & age <=74 ~ 2,
           waist_criterion ==1 & num_risks ==1 & smoking_yn ==1 & age>=40 & age<=64 ~ 1,
           waist_criterion ==1 & num_risks ==1 & smoking_yn ==1 & age>=65 & age <=74 ~ 2,
           waist_criterion ==1 & num_risks ==1 & smoking_yn ==2 ~ 2,
           waist_criterion ==0 & bmi >= 25 & num_risks ==3 & age>=40 & age<=64 ~ 1,
           waist_criterion ==0 & bmi >= 25 & num_risks ==3 & age>=65 & age<=74 ~ 2,
           waist_criterion ==0 & bmi >= 25 & num_risks ==2 & smoking_yn ==1 & age>=40 & age<=64 ~ 1,
           waist_criterion ==0 & bmi >= 25 & num_risks ==2 & smoking_yn ==1 & age>=65 & age<=74 ~ 2,
           waist_criterion ==0 & bmi >= 25 & num_risks ==2 & smoking_yn ==2 ~2,
           waist_criterion ==0 & bmi >= 25 & num_risks ==1 ~ 2,
           TRUE ~ 3)) %>%
  group_by(personalnum) %>%
  mutate(calculated_class_advice_last_year = lag(calculated_class_advice),
         guide1Y_last_year = lag(guide1Y),
         clinic1Y_last_year = lag(clinic1Y),
         guideclinic1Y = ifelse(guide1Y == 1 | clinic1Y == 1, 1, 0),
         ad_pattern_ref_year = case_when(calculated_class_advice %in% c(1,2) & guideclinic1Y == 0 ~ 1,
                                         calculated_class_advice %in% c(1,2) & guideclinic1Y == 1 ~ 2,
                                         calculated_class_advice == 3 ~ 3)
  ) %>%
  ungroup(personalnum) %>%
  filter(vis_year >= 2014 & vis_year <= 2018) 

df3 = idlist %>%
  dplyr::select(get_cert_ymd, lost_cert_ymd, personalnum, birthday, family, sex_div) %>%
  replace_na(list(lost_cert_ymd = 20201231)) %>%
  filter(lost_cert_ymd > 20150101) %>%
  mutate("2014" = ifelse(get_cert_ymd <= 20140401 & lost_cert_ymd > 20150331 &
                           birthday <= 19750331 & birthday >= 19390401, 1, 0),
         "2015" = ifelse(get_cert_ymd <= 20150401 & lost_cert_ymd > 20160331 &
                           birthday <= 19760331 & birthday >= 19400401, 1, 0),
         "2016" = ifelse(get_cert_ymd <= 20160401 & lost_cert_ymd > 20170331 &
                           birthday <= 19770331 & birthday >= 19410401, 1, 0),
         "2017" = ifelse(get_cert_ymd <= 20170401 & lost_cert_ymd > 20180331 &
                           birthday <= 19780331 & birthday >= 19420401, 1, 0),
         "2018" = ifelse(get_cert_ymd <= 20180401 & lost_cert_ymd > 20190331 &
                           birthday <= 19790331 & birthday >= 19430401, 1, 0)) %>%
  dplyr::select(-c(get_cert_ymd)) %>%
  pivot_longer(col = -c(personalnum, family, birthday, sex_div, lost_cert_ymd), names_to = "ref_year", values_to = "in_cohort") %>%
  mutate(ref_year = as.double(ref_year)) %>%
  left_join(df2 %>% dplyr::select(-c(birthday, sex_div)), by = c("personalnum", "ref_year" = "vis_year")) %>%
  mutate(ad_pattern_ref_year = case_when(in_cohort == 0 ~ 4,
                                         in_cohort == 1 ~ ad_pattern_ref_year)) %>%
  group_by(personalnum) %>%
  mutate(ad_pattern_last_year = lag(ad_pattern_ref_year)) %>%
  ungroup(personalnum) %>%
  replace_na(replace = list(ad_pattern_ref_year = 0, ad_pattern_last_year = 0)) %>%
  filter(in_cohort == 1) %>%
  mutate_at(vars(ad_pattern_ref_year, ad_pattern_last_year),
            funs(factor(., levels = c(3,2,1,0,4),
                        labels = c("not metabo", "metabo, participated", "metabo, not participated", "no screenings", "ineligible")))) %>%
  mutate(checkup_visit = factor(ifelse(is.na(age), 0, 1),
                                levels = c(0,1),
                                labels = c('no visit','visited')),
         on_drugs = factor(on_drugs, levels = c(0,1), labels = c('Not prescribed', 'Prescribed')),
         lost_cert_ymd = as.Date(as.character(lost_cert_ymd), format = "%Y%m%d"), 
         lost_to_fu = factor(ifelse(lost_cert_ymd - vis_ymd < 365, 1, 0)),
         female = factor(sex_div - 1,
                         levels = c(0,1),
                         labels = c('Male', 'Female')),
         main_insured_person = factor(ifelse(family == 0, 1, 0),
                                      levels = c(1,0),
                                      labels = c('Main insured','Insured family member')),
         waist_group = factor(case_when(waist >= 85 & female == 'Male' ~ 1,
                                        waist >= 90 & female == 'Female' ~ 1,
                                        waist < 85 & female == 'Male' ~ 0,
                                        waist < 85 & female == 'Female' ~ 0),
                              levels = c(0,1),
                              labels = c('Below cutoff', 'Over cutoff')),
         bmi_group = factor(ifelse(bmi >= 25, 1, 0),
                            levels = c(0,1),
                            labels = c('<25', '>=25')),
         birthday = as.Date(as.character(birthday), format = "%Y%m%d"),
         firstday_ref_year = as.Date(paste(as.character(ref_year), "0401"),format = "%Y%m%d"),
         firstday_last_year = as.Date(paste(as.character(ref_year-1), "0401"),format = "%Y%m%d"),
         age = as.integer((firstday_ref_year - birthday)/365),
         age_last_year = as.integer((firstday_last_year - birthday)/365),
         age_group = factor(case_when(age >=39 & age <45 ~ 0,
                                      age >=45 & age <50 ~ 1,
                                      age >=50 & age <55 ~ 2,
                                      age >=55 & age <60 ~ 3,
                                      age >=60 & age <65 ~ 4,
                                      age >=65 & age <70 ~ 5,
                                      age >=70 & age <75 ~ 6),
                            levels = c(0,1,2,3,4,5,6),
                            labels = c('40-44','45-49','50-54','55-59','60-64','65-69','70-74')),
         age_group_last_year = factor(case_when(age_last_year >=38 & age_last_year <45 ~ 0,
                                                age_last_year >=45 & age_last_year <50 ~ 1,
                                                age_last_year >=50 & age_last_year <55 ~ 2,
                                                age_last_year >=55 & age_last_year <60 ~ 3,
                                                age_last_year >=60 & age_last_year <65 ~ 4,
                                                age_last_year >=65 & age_last_year <70 ~ 5,
                                                age_last_year >=70 & age_last_year <75 ~ 6),
                                      levels = c(0,1,2,3,4,5,6),
                                      labels = c('40-44','45-49','50-54','55-59','60-64','65-69','70-74')),
         BP_group = factor(case_when(sbp <130 & dbp <85 ~ 0,
                                     (sbp >=130 & sbp <=159) | (dbp >=85 & dbp<=99) ~ 1,
                                     sbp >=160 | dbp >=100 ~ 2),
                           levels = c(0,1,2),
                           labels = c('None','Mail post','Specific letter')),
         DLP_group = factor(ifelse(tg >= 50 | hdl < 40, 1, 0),
                            levels = c(0,1),
                            labels = c('None','Mail post')),
         DM_group =  factor(case_when(fbg >= 150 | hba1c_ngsp >= 8.0 ~ 3,
                                      fbg >= 130 | hba1c_ngsp >= 7.0 ~ 2,
                                      fbg >= 100 | hba1c_ngsp >= 5.6 ~ 1,
                                      fbg < 100 | hba1c_ngsp < 5.6 ~ 0),
                            levels = c(0,1,2,3),
                            labels = c('None','Mail post','Specific letter','Employer involvement')),
         smoking = factor(2 - smoking_yn, levels = c(0,1), labels = c('Non-smoker','Smoker')),#0: non-smoke, 1: smoke,
         alcohol_group = factor(case_when(alcohol_habits == 1 & alcohol_amount >= 3 ~ 2,
                                          alcohol_habits == 3 ~ 0,
                                          alcohol_habits >= 1 ~ 1),
                                levels = c(0,1,2),
                                labels = c('Almost never','Chance drink','Overdrink')),
         exercise = factor(2-ex_habits_yn,
                           levels = c(0,1),
                           labels = c('Insufficient','>30min, >2days/week')),
         change_habits_will = factor(case_when(change_habits == 1 ~ 0,
                                               change_habits %in% c(2,3) ~ 1,
                                               change_habits %in% c(4,5) ~ 2),
                                     levels = c(0,1,2),
                                     labels = c('No interest','Interested','Taking action'))
  ) 

########################Analyses#########
#Analysis 1
analysis1 = function(df, Y){
  ans = df %>% 
    dplyr::filter(ref_year == Y) %>%
    summarise(n_eligible = n(),
              prob_screenings = 1 - mean(ad_pattern_ref_year == 'no screenings'),
              n_screenings = sum(ad_pattern_ref_year != 'no screenings'),
              prob_eligible_to_intervention = mean(ad_pattern_ref_year %in% c('metabo, not participated', 'metabo, participated')
                                                   /mean(ad_pattern_ref_year != 'no screenings')),
              n_eligible_to_intervention = sum(ad_pattern_ref_year %in% c('metabo, not participated', 'metabo, participated')),
              prob_followed_up = mean(ad_pattern_ref_year %in% c('metabo, not participated', 'metabo, participated') &
                                        lost_to_fu == 0)/(prob_screenings*prob_eligible_to_intervention),
              n__followed_up = sum(ad_pattern_ref_year %in% c('metabo, not participated', 'metabo, participated') &
                                     lost_to_fu == 0),
              prob_received_intervention = mean(ad_pattern_ref_year == 'metabo, participated' &
                                                  lost_to_fu == 0)/(prob_screenings*prob_followed_up*prob_eligible_to_intervention),
              n_received_intervention = sum(ad_pattern_ref_year == 'metabo, participated' &
                                              lost_to_fu == 0))
  return(ans)
}
analysis1(df3, 2017) %>% transpose()
analysis1(df3, 2016) %>% transpose()
analysis1(df3, 2018) %>% transpose()
analysis1(df3 %>% filter(on_drugs == 'Prescribed'), 2017) %>% transpose()
analysis1(df3 %>% filter(on_drugs == 'Not prescribed'), 2017) %>% transpose()
analysis1(df3 %>% filter(on_drugs == 'Not prescribed'), 2016) %>% transpose()
analysis1(df3 %>% filter(on_drugs == 'Not prescribed'), 2018) %>% transpose()
analysis1(2016)


#Analysis2
#Table1
set.seed(1)

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

T1 = table1(~ age_group + female + main_insured_person + BP_group + 
              DM_group + DLP_group +smoking + alcohol_group + exercise + change_habits_will| ad_pattern_ref_year,  
            data = df3 %>% 
              filter(lost_to_fu == 0 & on_drugs == 'Not prescribed') %>%
              set_variable_labels(age_group = 'Age',
                                  female = 'Gender',
                                  main_insured_person = 'Insured status',
                                  BP_group = 'Recommendation intensity - HTN',
                                  DM_group = 'Recommendation intensity - DM',
                                  DLP_group = 'Recommendation intensity - Dyslipidemia',
                                  smoking = 'Smoking',
                                  alcohol_group = 'Alcohol consumption',
                                  exercise = 'Exercise habit',
                                  change_habits_will = 'Self-reported motivation for lifestyle change') %>%
              mutate(ad_pattern_ref_year = factor(ad_pattern_ref_year,
                                                  labels = c("not metabo", "Adhered", "Not adhered"))) %>%
              filter(ref_year == 2017) %>%
              filter(ad_pattern_ref_year %in% c("Adhered", "Not adhered")),
            render.continuous=my.render.cont, render.categorical=my.render.cat,
            footnote = c('"Chance" alcohol drinking indicates any other style of alcohol consumption than overdrinking.', 
                         '"Overdrink" indicates everyday consumption of >60g alcohol.'))

#Table2: Comparison of results between original and sensitivity analyses with different methods 
Analysis2 = function(df3, Y, method) {
  d = df3 %>% 
    filter(ref_year == Y & ad_pattern_ref_year %in% c('metabo, not participated', 'metabo, participated')) %>%
    mutate(not_lost_to_fu = factor(ifelse(lost_to_fu == 1, 0, 1)),
           participation = factor(ifelse(ad_pattern_ref_year == 'metabo, not participated', 0, 1))) %>%
    filter(on_drugs == 'Not prescribed') %>%
    dplyr::select(not_lost_to_fu, age_group, female, main_insured_person, BP_group,
                  DM_group, DLP_group, smoking, alcohol_group, exercise, change_habits_will, participation)
  if (method == 'mi') {
    d = d %>% 
      mice(m=5, maxit = 5, method = 'pmm', printFlag = FALSE, seed = 12345) 
    fit = with(d, glm(participation ~ age_group + female + main_insured_person + BP_group +
                        DM_group + DLP_group + smoking + alcohol_group + exercise + change_habits_will, 
                      family = binomial))
    pool.fit = pool(fit)
    table = summary(pool.fit, conf.int = TRUE,conf.level = 0.95, exponentiate = TRUE) %>%
      dplyr::select(-c(std.error, statistic, df))
    rownames(table) = table$term
    output = table %>% dplyr::select(-term) %>% slice(-1)
  }
  if (method == 'complete'){
    d = d %>% drop_na()
    output = glm(participation ~ age_group + female + main_insured_person + BP_group +
                   DM_group + DLP_group + smoking + alcohol_group + exercise + change_habits_will, 
                 data = d, family = binomial)
  }
  if (method == 'ipcw') {
    d = d %>% drop_na()
    W_ltfu = weightit(not_lost_to_fu ~ age_group + female + main_insured_person + BP_group +
                        DM_group + DLP_group + smoking + alcohol_group + exercise + change_habits_will, 
                      data = d,
                      method = 'ps',
                      estimand = 'ATE')
    d.w = svydesign(~1, weights = W_ltfu$weights, data = d)
    output = svyglm(participation ~ age_group + female + main_insured_person + BP_group +
                      DM_group + DLP_group + smoking + alcohol_group + exercise + change_habits_will, 
                    subset(d.w, not_lost_to_fu == 1),
                    family = binomial)
  }
  return(output)
}
Analysis2_comparison = function(df, Y){
  model1 = Analysis2(df, Y, method = 'complete') %>% logistic.display(simplified = TRUE) 
  table1 = model1$table %>% round(digits = 2) %>% format(nsmall = 2) %>% 
    data.frame() %>% mutate_all(~gsub(.,pattern = ' ', replacement = '')) %>% 
    mutate(OR = paste0(OR,' ','[',lower95ci,'-',upper95ci,']')) %>% dplyr::select(OR)
  model2 = Analysis2(df, Y, method = 'mi') 
  table2 = model2 %>% round(digits = 2) %>% format(nsmall = 2) %>% set_names(c("a","b","c","d")) %>%
    mutate_all(~gsub(.,pattern = ' ', replacement = '')) %>%
    mutate(OR = paste0(a,' ','[',c,'-',d,']')) %>% dplyr::select(OR)
  model3 = Analysis2(df, Y, method = 'ipcw') %>% logistic.display(simplified = TRUE)
  table3 = model3$table  %>% round(digits = 2) %>% format(nsmall = 2) %>% 
    data.frame() %>% mutate_all(~gsub(.,pattern = ' ', replacement = '')) %>%
    mutate(OR = paste0(OR,' ','[',lower95ci,'-',upper95ci,']')) %>% dplyr::select(OR)
  t = cbind(table2, table3, table1) %>% set_names(c('Original', 'IPCW', 'Complete cases')) 
  return(t)
}

Analysis2_comparison_table_2017 = Analysis2_comparison(df = df3, Y=2017)
Analysis2_comparison(df = df3, Y=2018)
write.csv(Analysis2_comparison_table_2017, "C:/metabo_check/coef_comparison_2017_adherence.csv", row.names = TRUE)

model1 = Analysis2(df3, 2017, method = 'complete')
model2 = Analysis2(df3, 2017, method = 'mi')
model3 = Analysis2(df3, 2017, method = 'ipcw')

table2 = model2 %>% round(digits = 2) %>% format(nsmall = 2) %>% set_names(c("a","b", "c", "d")) %>%
  mutate(OR = paste(a, '(', c, '-', d, ')', sep = "")) %>% dplyr::select(OR)
table2

#Figure3: Coef plots
d = df3 %>% 
  filter(on_drugs == 'Not prescribed') %>%
  filter(ref_year == Y & ad_pattern_ref_year %in% c('metabo, not participated', 'metabo, participated')) %>%
  mutate(not_lost_to_fu = factor(ifelse(lost_to_fu == 1, 0, 1)),
         non_participation = factor(ifelse(ad_pattern_ref_year == 'metabo, not participated', 0, 1))) %>%
  dplyr::select(not_lost_to_fu, age_group, female, main_insured_person, BP_group,
                DM_group, DLP_group, smoking, alcohol_group, exercise, change_habits_will, non_participation) %>%
  mice(m=5, maxit = 5, method = 'pmm', printFlag = FALSE, seed = 12345) 
fit = with(d, glm(non_participation ~ age_group + female + main_insured_person + BP_group +
                    DM_group + DLP_group + smoking + alcohol_group + exercise + change_habits_will, 
                  family = binomial))
pool.fit = pool(fit)

fig = plot_coefs(pool.fit, 
                 coefs = c('Age 45-49 vs 40-44' = 'age_group45-49',
                           'Age 50-54 vs 40-44' = 'age_group50-54',
                           'Age 55-59 vs 40-44' = 'age_group55-59',
                           'Age 60-64 vs 40-44' = 'age_group60-64',
                           'Age 65-69 vs 40-44' = 'age_group65-69',
                           'Age 70-74 vs 40-44' = 'age_group70-74',
                           'Female vs male' = 'femaleFemale',
                           'Insured family member vs main insured person' = 'main_insured_personInsured family member',
                           'HTN - Mail Post' = 'BP_groupMail post',
                           'HTN - Urgent Letter' = 'BP_groupSpecific letter',
                           'DM - Mail Post' = 'DM_groupMail post',
                           'DM - Urgent Letter' = 'DM_groupSpecific letter',
                           'DM - Employer Involvement' = 'DM_groupEmployer involvement',
                           'DLP - Mail Post' = 'DLP_groupMail post',
                           'Smoking' = 'smokingSmoker',
                           'Chance drink'='alcohol_groupChance drink',
                           'Overdrink'='alcohol_groupOverdrink',
                           'Exercise habit' = 'exercise>30min, >2days/week',
                           'Interested in lifestyle change' = 'change_habits_willInterested',
                           'Taking action on lifestyle change'='change_habits_willTaking action'
                 ),
                 exp = TRUE, scale = TRUE,
                 colors = 'Black') +
  labs(x = 'Odds ratio of adherence for health guidance invitation (log scale)') +
  scale_x_log10() +
  geom_pointrangeh(size = 1.5, shape = 20, show.legend = FALSE)

fig


# Supplement
# S1:Intervals between checkups
S1 = screenings %>%
  dplyr::select(personalnum, vis_ymd, vis_year) %>%
  distinct(personalnum, vis_year, .keep_all = TRUE) %>%
  group_by(personalnum) %>%
  filter(vis_year %in% c(2017, 2018)) %>%
  mutate(last_year_vis_ymd = lag(vis_ymd),
         vis_ymd = as.Date(as.character(vis_ymd), format = "%Y%m%d"),
         last_year_vis_ymd = as.Date(as.character(last_year_vis_ymd), format = "%Y%m%d"),
         vis_interval = vis_ymd - last_year_vis_ymd) 

ggplot(S1 %>% filter(!is.na(vis_interval)), aes(x = vis_interval)) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = 365), color = "red", linetype = "dotdash") +
  labs(x = "Interval (days)",
       y = "Count") +
  annotate(geom = "text", x = 420, y = 10000, label = "365 days") +
  theme_minimal()

# S2:Intervals between checkup and guidance
S2 = df %>%
  left_join(idlist %>% dplyr::select(personalnum, id, birthday), by = 'personalnum') %>%
  left_join(clinics %>% dplyr::select(id, prac_ym), by = "id") %>%
  left_join(instructions %>% dplyr::select(personalnum, guide_ymd), by = "personalnum") %>%
  mutate(vis_ymd = as.Date(as.character(vis_ymd), format = "%Y%m%d"),
         prac_ym = as.Date(paste(as.character(prac_ym), "01"),format = "%Y%m%d") %m+% months(1) -1,
         guide_ymd = as.Date(as.character(guide_ymd), format = "%Y%m%d"),
         gapdaysclinic = prac_ym - vis_ymd,
         gapdaysguide = guide_ymd - vis_ymd) %>%
  filter(!is.na(gapdaysguide)| !is.na(gapdaysclinic)) %>%
  replace_na(list(gapdaysguide=0, gapdaysclinic=0)) %>%
  mutate(gapdays = gapdaysclinic + gapdaysguide) %>%
  filter(gapdays >= 0 & vis_year == 2017) %>%
  dplyr::select(personalnum, vis_ymd, gapdays) %>%
  group_by(personalnum, vis_ymd) %>%
  summarise(gapdays = min(gapdays)) %>%
  left_join(screenings %>%
              filter(vis_year == 2017) %>%
              dplyr::select(personalnum, vis_ymd, drug_dm_yn, drug_hl_yn, drug_ht_yn) %>%
              mutate(vis_ymd = as.Date(as.character(vis_ymd), format = "%Y%m%d")),
            by = c("personalnum", "vis_ymd")) %>%
  mutate(on_drugs = ifelse((drug_dm_yn - 1)*(drug_hl_yn - 1)*(drug_ht_yn -1) == 1, 0, 1)) %>%
  filter(on_drugs == 0) %>%
  distinct(personalnum,.keep_all = TRUE)

ggplot(S2, aes(x=gapdays)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(aes(xintercept = 365), color = "red", linetype = "dotdash") +
  labs(x = "Interval (days)",
       y = "Count") +
  annotate(geom = "text", x = 480, y = 115, label = "365 days") +
  theme_minimal()



