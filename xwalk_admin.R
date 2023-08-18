#####################################################################
## Author: Megan Knight                                            ##
## Description: Adjust admin data using IHME census and survey     ##
## data                                                            ##
#####################################################################
## clear memory
rm(list=ls())

## source functions
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_population.R")
library(crosswalk002, lib.loc = "FILEPATH")
library(dummies, lib.loc = "FILEPATH")

## library
pacman::p_load(data.table, stringr)

## define directories 
dir <- 'FILEPATH'

## define functions 
'%ni%' <- Negate('%in%')

#####################################################################
## 1. XWALK DATA PREP                                              ##
#####################################################################
## pull location metadata 
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = 'iterative')
#pop <- get_population(location_set_id = 35, location_id = 'all', gbd_round_id = 7, decomp_step = 'iterative', year_id = seq(1990,2019))

## pull employment ratios
emp_ratio <- fread(file.path('FILEPATH/prop_emp_oftotal.csv'))
emp_ratio$emp_ratio_mean <- rowMeans(emp_ratio[, -c('location_id', 'year_id')])

## read in input data for all cadres from first submission
hrh <- rbindlist(lapply(list.files(path = dir, pattern = 'hrh_phys|hrh_dent|hrh_pharm', full.names = T), fread))
hrh_nurseagg <- fread(file.path(dir, 'hrh_nurseagg_1.csv'))[, me_name := 'hrh_nurseagg']
hrh <- rbind(hrh[, c('location_id', 'nid', 'year_id', 'age_group_id', 'sex_id', 'is_outlier', 'measure', 'survey_name', 'ihme_loc_id', 'val', 'sample_size', 'variance', 'me_name')], 
             hrh_nurseagg[, c('location_id', 'nid', 'year_id', 'age_group_id', 'sex_id', 'is_outlier', 'measure', 'survey_name', 'ihme_loc_id', 'val', 'sample_size', 'variance', 'me_name')])

## update outliers in input data 
hrh[, is_outlier := ifelse(me_name == 'hrh_phys' & ihme_loc_id == 'UGA' & year_id == 2009, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'NOR' & year_id %in% c(2011, 2012, 2013) & val < 250, 1, 
                    ifelse(me_name == 'hrh_pharm' & ihme_loc_id == 'POL' & year_id == 2002, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'GEO' & year_id == 2016 & val < 130, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'SVK' & year_id == 2016, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'HUN' & year_id == 1994, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'ITA' & year_id %in% seq(2004, 2010,1),1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'DNK' & year_id %in% seq(2011, 2016) & val < 100, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'ISL' & year_id == 2015, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'ITA' & val < 100, 1,
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'MEX' & year_id == 2003, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'BWA' & year_id == 2005, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'IND' & year_id == 2017, 1,
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'NIC' & year_id == 2005, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & ihme_loc_id == 'LTU' & val > 150, 1,
                    ifelse(me_name == 'hrh_pharm' & ihme_loc_id == 'NZL' & year_id == 2008, 1, 
                    ifelse(me_name == 'hrh_pharm' & ihme_loc_id == 'HRV' & year_id == 2011, 1,
                    ifelse(me_name == 'hrh_dent' & ihme_loc_id == 'CRI' & year_id == 2006, 1,is_outlier))))))))))))))))))]
hrh <- hrh[is_outlier == 0]

## read administrative data for all available cadres 
who <- fread('FILEPATH/who_admin_prepped.csv')
who <- who[is_outlier == 0,]

## convert metric from rate per 10,000 employed population to per 10,000 population
hrh <- merge(hrh, emp_ratio[, c('location_id','year_id', 'emp_ratio_mean')], by = c('location_id', 'year_id'))
hrh[, c('val', 'variance') := list(val * emp_ratio_mean, variance * emp_ratio_mean)]

## calculate average across surveys and censuses 
hrh <- hrh[, lapply(.SD, mean), .SDcols = c('val', 'variance'), by = c('location_id', 'ihme_loc_id', 'year_id', 'me_name')]

## assign survey and census average as the gold standard for crosswalking
hrh <- hrh[,dorm_ref := "gold_standard"]

## calculate variance by region-year for administrative variance imputation
hrh <- merge(hrh, locs[, c('location_id', 'region_name', 'super_region_name', 'location_name')], by = 'location_id')
hrh_r <- hrh[, lapply(.SD, mean), .SDcols = c('variance'), by = c('region_name', 'me_name')]
hrh_sr <- hrh[, lapply(.SD, mean), .SDcols = c('variance'), by = c('super_region_name', 'me_name')]

## update variable names 
setnames(hrh, old = c('val', 'variance'), new = c('ref_val', 'ref_variance'))

## pull count of IHME data source count
hrh_count <- hrh[, .N, by = c('location_id','location_name', 'me_name')]

## assignment administrative data as alternative for crosswalking 
who[, dorm_alt := 'admin']
who <- merge(who, hrh_r[, c('region_name', 'variance', 'me_name')], by = c('region_name', 'me_name'), all.x = T)
who_sr_var <- merge(who[is.na(variance)], hrh_sr[, c('super_region_name', 'variance', 'me_name')], by = c('super_region_name', 'me_name'))
who <- rbind(who[!(is.na(variance))], who_sr_var[, variance := variance.y][, variance.x := NULL][,variance.y := NULL])

## combine all cadre data 
ihme_data <- hrh[, c('val', 'variance', 'obs_method') := list(ref_val, ref_variance, dorm_ref)] 
who_data <- who[, c('val', 'variance', 'obs_method') := list(alt_val, variance, dorm_alt)] 
df_orig <- rbind(ihme_data[, c('location_id', 'ihme_loc_id', 'year_id', 'me_name', 'val', 'variance', 'obs_method', 'region_name', 'super_region_name', 'location_name')], 
                 who_data[, c('location_id', 'ihme_loc_id', 'year_id', 'me_name', 'val', 'variance', 'obs_method', 'region_name', 'super_region_name', 'location_name')])

## create location and super-region/region dummies 
loc_dummies <- dummy.data.frame(df_orig[, c('location_name')])
sr_reg_dummies <- dummy.data.frame(df_orig[, c('region_name', 'super_region_name')])

## pull covariates for crosswalk from Bayesian lasso coefficients 
hrh_phys_loc <- fread('FILEPATH/hrh_phys_loc_blasso_coefs.csv')[, me_name := 'hrh_phys']
hrh_nurseagg_loc <- fread('FILEPATH/hrh_nurseagg_loc_blasso_coefs.csv')[, me_name := 'hrh_nurseagg']
hrh_dent_loc <- fread('FILEPATH/hrh_dent_loc_blasso_coefs.csv')[, me_name := 'hrh_dent']
hrh_dentass_loc <- fread('FILEPATH/hrh_dentass_loc_blasso_coefs.csv')[, me_name := 'hrh_dentass']
hrh_pharm_loc <- fread('FILEPATH/hrh_pharm_loc_blasso_coefs.csv')[, me_name := 'hrh_pharm']
hrh_pharmtech_loc <- fread('FILEPATH/hrh_pharmtech_loc_blasso_coefs.csv')[, me_name := 'hrh_pharmtech']
loc_covs <- rbind(hrh_phys_loc, 
                  hrh_nurseagg_loc,
                  hrh_dent_loc,
                  hrh_dentass_loc, 
                  hrh_pharm_loc, 
                  hrh_pharmtech_loc)

hrh_phys_sr_reg <- fread('FILEPATH/hrh_phys_sr_reg_blasso_coefs.csv')[, me_name := 'hrh_phys']
hrh_nurseagg_sr_reg <- fread('FILEPATH/hrh_nurseagg_sr_reg_blasso_coefs.csv')[, me_name := 'hrh_nurseagg']
hrh_dent_sr_reg <- fread('FILEPATH/hrh_dent_sr_reg_blasso_coefs.csv')[, me_name := 'hrh_dent']
hrh_dentass_sr_reg <- fread('FILEPATH/hrh_dentass_sr_reg_blasso_coefs.csv')[, me_name := 'hrh_dentass']
hrh_pharm_sr_reg <- fread('FILEPATH/hrh_pharm_sr_reg_blasso_coefs.csv')[, me_name := 'hrh_pharm']
hrh_pharmtech_sr_reg <- fread('FILEPATH/hrh_pharmtech_sr_reg_blasso_coefs.csv')[, me_name := 'hrh_pharmtech']
sr_reg_covs <- rbind(hrh_phys_sr_reg, 
                     hrh_nurseagg_sr_reg,
                     hrh_dent_sr_reg,
                     hrh_dentass_sr_reg, 
                     hrh_pharm_sr_reg, 
                     hrh_pharmtech_sr_reg)

## subset covariates to non-zero Bayesian lasso coefficients 
df_orig <- cbind(df_orig, loc_dummies[, names(loc_dummies) %in% c(unique(loc_covs$V2), 'location_nameRwanda', 'location_nameThailand', 'location_namePanama')])
df_orig <- cbind(df_orig, sr_reg_dummies[, names(sr_reg_dummies) %in% unique(sr_reg_covs[!(sr_reg_covs$V2 %like% 'Sub-Saharan Africa'), V2])], sr_reg_dummies[,'super_region_nameSub-Saharan Africa'])
setnames(df_orig, old = 'V3', new = 'super_region_nameSub-Saharan Africa')

## pull matched pairs
## change years for specific admin points to match 
who[ihme_loc_id == 'THA' & me_name == 'hrh_phys' & year_id == 1991, year_id := 1990]
who[ihme_loc_id == 'THA' & me_name == 'hrh_phys' & year_id == 2001, year_id := 2000]
who[ihme_loc_id == 'RWA' & me_name == 'hrh_phys' & year_id == 2011, year_id := 2012]
who[ihme_loc_id == 'COD' & me_name == 'hrh_phys' & year_id == 2013, year_id := 2012]
who[ihme_loc_id == 'MOZ' & me_name == 'hrh_phys' & year_id == 2006, year_id := 2007]
who[ihme_loc_id == 'THA' & me_name == 'hrh_nurseagg' & year_id == 1991, year_id := 1990]
who[ihme_loc_id == 'THA' & me_name == 'hrh_nurseagg' & year_id == 2001, year_id := 2000]
who[ihme_loc_id == 'CHN' & me_name == 'hrh_dent' & year_id == 2011, year_id := 2012]
who[ihme_loc_id == 'THA' & me_name == 'hrh_dent' & year_id == 1991, year_id := 1990]
who[ihme_loc_id == 'THA' & me_name == 'hrh_dent' & year_id == 2001, year_id := 2000]
who[ihme_loc_id == 'PAN' & me_name == 'hrh_dent' & year_id == 2001, year_id := 2000]

df_matched <- merge(hrh[, c('location_id', 'ihme_loc_id', 'year_id', 'me_name', 'ref_val', 'ref_variance', 'dorm_ref', 'region_name', 'super_region_name')], 
                    who[, c('location_id', 'ihme_loc_id', 'location_name', 'year_id', 'me_name', 'alt_val', 'variance', 'dorm_alt')], by = c('location_id', 'year_id', 'ihme_loc_id', 'me_name'))

## calculate difference between reference and alternative pairs
df_matched[, diff := log(alt_val) - log(ref_val)]

## calculate variance of the differences by region
df_matched[, diff_variance := mean(sd(diff)/sqrt(length(diff)), na.rm = T), by = c('region_name')]
df_matched[ihme_loc_id == 'COD', diff_variance := df_matched[super_region_name == 'Sub-Saharan Africa', mean(sd(diff)/sqrt(length(diff)), na.rm = T), by = c('super_region_name')]$V1]

## impute variance by adding mean variance from IHME's input data by region-year with variance from difference 
df_matched[, alt_variance := diff_variance + variance]

## data prep for matched pairs 
df_matched[, id := seq.int(nrow(df_matched))]
df_matched$id2 <- as.integer(as.factor(df_matched$id)) 

## save copy of matched 
df_matched_orig <- copy(df_matched)

#####################################################################
## 1. XWALK                                                        ##
#####################################################################
############################ PHYSICIANS #############################
## subset matched pairs
df_matched <- df_matched_orig[me_name == 'hrh_phys']

## create location and super-region/region dummies for matched pairs
sr_reg_dummies_matched <- dummy.data.frame(df_matched[, c('region_name', 'super_region_name')])
loc_dummies_matched <- dummy.data.frame(df_matched[, c('location_name')])

## subset dummy covariates to non-zero Bayesian lasso coefficients 
df_matched <- cbind(df_matched, loc_dummies_matched[, names(loc_dummies_matched) %in% c(unique(loc_covs[me_name == 'hrh_phys']$V2), 'location_nameRwanda', 'location_nameThailand', 'location_nameMozambique', 'location_nameDemocratic Republic of the Congo')])
df_matched <- cbind(df_matched, sr_reg_dummies_matched[, names(sr_reg_dummies_matched) %in% unique(sr_reg_covs[me_name == 'hrh_phys' & !(sr_reg_covs$V2 %like% 'Sub-Saharan Africa'), V2])], sr_reg_dummies_matched[,'super_region_nameSub-Saharan Africa'])
setnames(df_matched, old = 'V3', new = 'super_region_nameSub-Saharan Africa')

dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$alt_val,
    sd = df_matched$alt_variance,
    transformation = "linear_to_log" ),
  delta_transform(
    mean = df_matched$ref_val, 
    sd = df_matched$ref_variance,
    transformation = "linear_to_log")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("log_diff", "log_diff_se")] <- calculate_diff(
  df = dat_diff, 
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )

dat1 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in log space
  obs_se = "log_diff_se", # SE of matched differences in log space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("region_nameAndean Latin America", 
              "region_nameAustralasia", 
              "region_nameCaribbean", 
              "region_nameCentral Europe", 
              "region_nameCentral Latin America",             
              "region_nameEast Asia",                         
              "region_nameEastern Europe",                    
              "region_nameHigh-income Asia Pacific",          
              "region_nameHigh-income North America",         
              "region_nameNorth Africa and Middle East",      
              "region_nameSouth Asia",                        
              "region_nameSoutheast Asia", 
              "region_nameTropical Latin America",            
              "region_nameWestern Europe",                    
              'super_region_nameSub-Saharan Africa'),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

dat2 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in logit space
  obs_se = "log_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("location_nameAlbania",                         
              "location_nameAustria",                         
              "location_nameBolivia (Plurinational State of)",
              "location_nameBotswana",                        
              "location_nameBrazil",                          
              "location_nameBulgaria",                        
              "location_nameCambodia",                        
              "location_nameCameroon",                        
              "location_nameChile",                           
              "location_nameChina",                           
              "location_nameCroatia",                         
              "location_nameCuba",                            
              "location_nameCyprus",                          
              "location_nameCzechia",                         
              "location_nameDenmark",                         
              "location_nameDominican Republic",              
              "location_nameEcuador",                         
              "location_nameEstonia",                         
              "location_nameFinland",                         
              "location_nameGeorgia",                         
              "location_nameGermany",                         
              "location_nameGhana",                           
              "location_nameGreece",                          
              "location_nameHonduras",                        
              "location_nameHungary",                         
              "location_nameIceland",                         
              "location_nameIreland",  
              "location_nameItaly",                           
              "location_nameJordan",                          
              "location_nameLatvia",                          
              "location_nameLithuania",                       
              "location_nameMongolia",                        
              "location_nameNew Zealand",                     
              "location_nameNicaragua",                       
              "location_nameNorway",                          
              "location_namePakistan",                        
              "location_namePhilippines",                     
              "location_namePoland",                          
              "location_namePortugal",                        
              "location_nameRepublic of Korea",               
              "location_nameRomania",                         
              "location_nameRussian Federation",  
              "location_nameRwanda",
              "location_nameSlovakia",                        
              "location_nameSouth Africa",                    
              "location_nameSpain",                           
              "location_nameSwitzerland",                     
              "location_nameTajikistan",                      
              "location_nameTurkey",   
              "location_nameThailand",
              "location_nameUkraine",                         
              "location_nameUnited Kingdom",                  
              "location_nameUnited Republic of Tanzania",     
              "location_nameUnited States of America",        
              "location_nameUruguay",                         
              "location_nameZambia"),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list( CovModel(cov_name = "region_nameAndean Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameAustralasia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameCaribbean", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameCentral Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameCentral Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameEast Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameEastern Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameHigh-income Asia Pacific", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameHigh-income North America", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameNorth Africa and Middle East", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameSouth Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameSoutheast Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameTropical Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameWestern Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = 'super_region_nameSub-Saharan Africa', prior_beta_gaussian = list(admin = array(c(0,7.5))))
                     # specify covariate details
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

fit2 <- CWModel(
  cwdata = dat2,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list( CovModel(cov_name = "location_nameAlbania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameAustria", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameBolivia (Plurinational State of)", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameBotswana", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameBrazil", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameBulgaria", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameCambodia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameCameroon", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameChile", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameChina", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameCroatia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameCuba", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameCyprus", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameCzechia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameDenmark", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameDominican Republic", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameEcuador", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameEstonia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameFinland", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameGeorgia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameGermany", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameGhana", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameGreece", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameHonduras",prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameHungary", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameIceland", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameIreland", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameItaly", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameThailand", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameJordan", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameLatvia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameLithuania", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameMongolia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameNew Zealand", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameNicaragua", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameNorway", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_namePakistan", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_namePhilippines", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_namePoland", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_namePortugal", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameRepublic of Korea", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameRomania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameRussian Federation", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameRwanda", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameSlovakia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameSouth Africa", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameSpain", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameSwitzerland", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameTajikistan", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameTurkey", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameUkraine", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameUnited Kingdom", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameUnited Republic of Tanzania", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameUnited States of America", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "location_nameUruguay", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "location_nameZambia", prior_beta_gaussian = list(admin = array(c(0,7.5))))
                     # specify covariate details
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

df_result1 <- data.table(fit1$create_result_df())
df_result1 <- df_result1[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

df_result2 <- data.table(fit2$create_result_df())
df_result2 <- df_result2[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

results1 <- adjust_orig_vals(fit_object = fit1, 
                             df = df_orig[location_name %ni% gsub("location_name", "", c(loc_covs$V2, 'location_nameRwanda', 'location_nameThailand', 'location_nameMozambique', 'location_nameDemocratic Republic of the Congo')) & location_name %ni% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')

results2 <- adjust_orig_vals(fit_object = fit2, 
                             df = df_orig[location_name %in% gsub("location_name", "", c(loc_covs$V2, 'location_nameRwanda', 'location_nameThailand', 'location_nameMozambique', 'location_nameDemocratic Republic of the Congo')) | location_name %in% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')


sr_reg_adj <- cbind(df_orig[location_name %ni% gsub("location_name", "", c(loc_covs$V2, 'location_nameRwanda', 'location_nameThailand', 'location_nameMozambique', 'location_nameDemocratic Republic of the Congo')) & location_name %ni% unique(df_matched$location_name) , c("location_id",                                  
                                                                                          "ihme_loc_id",                                  
                                                                                          "year_id",                                      
                                                                                          "me_name",                                      
                                                                                          "val",                                          
                                                                                          "variance",                                     
                                                                                          "obs_method",                                   
                                                                                          "region_name",                                  
                                                                                          "super_region_name")], results1)[, adjustment_type := 'super-region/region adjustment']

loc_adj <- cbind(df_orig[location_name %in% gsub("location_name", "", c(loc_covs$V2, 'location_nameRwanda', 'location_nameThailand', 'location_nameMozambique', 'location_nameDemocratic Republic of the Congo')) | location_name %in% unique(df_matched$location_name), c("location_id",                                  
                                                                                      "ihme_loc_id",                                  
                                                                                      "year_id",                                      
                                                                                      "me_name",                                      
                                                                                      "val",                                          
                                                                                      "variance",                                     
                                                                                      "obs_method",                                   
                                                                                      "region_name",                                  
                                                                                      "super_region_name")], results2)[, adjustment_type := 'location adjustment']

xwalk_adj <- rbind(sr_reg_adj, loc_adj)
xwalk_adj[, adjustment_type := ifelse(pred_diff_mean == 0, 'no adjustment', adjustment_type)]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_phys', pred_diff_mean := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta])]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_phys', pred_diff_sd := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta_sd])]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_phys', ref_vals_mean := exp(log(val)-pred_diff_mean)]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_phys', ref_vals_sd := variance+pred_diff_sd]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_phys', adjustment_type := 'super-region/region adjustment']

write.csv(xwalk_adj, 'FILEPATH/hrh_phys_admin_adj.csv', row.names = F)

############################ XWALK #####################################
## Nursing and midwifery personnel 
## pull matched pairs for cadre
df_matched <- df_matched_orig[me_name == 'hrh_nurseagg']

## create location and super-region/region dummies for matched pairs
sr_reg_dummies_matched <- dummy.data.frame(df_matched[, c('region_name', 'super_region_name')])
loc_dummies_matched <- dummy.data.frame(df_matched[, c('location_name')])

## subset dummy covariates to non-zero Bayesian lasso coefficients 
df_matched <- cbind(df_matched, loc_dummies_matched[, names(loc_dummies_matched) %in% c(unique(loc_covs[me_name == 'hrh_nurseagg']$V2), 'location_nameThailand')])
df_matched <- cbind(df_matched, sr_reg_dummies_matched[, names(sr_reg_dummies_matched) %in% unique(sr_reg_covs[me_name == 'hrh_nurseagg' & !(sr_reg_covs$V2 %like% 'Sub-Saharan Africa'), V2])], sr_reg_dummies_matched[,'super_region_nameSub-Saharan Africa'])
setnames(df_matched, old = 'V3', new = 'super_region_nameSub-Saharan Africa')

## transform data into log space
dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$alt_val,
    sd = df_matched$alt_variance,
    transformation = "linear_to_log" ),
  delta_transform(
    mean = df_matched$ref_val, 
    sd = df_matched$ref_variance,
    transformation = "linear_to_log")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

## calculate difference between administrative and IHME survey/censuses
df_matched[, c("log_diff", "log_diff_se")] <- calculate_diff(
  df = dat_diff, 
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )

dat1 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in log space
  obs_se = "log_diff_se", # SE of matched differences in log space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("region_nameAndean Latin America",             
              "region_nameAustralasia",                      
              "region_nameCentral Asia",                     
              "region_nameCentral Europe",                   
              "region_nameCentral Latin America",            
              "region_nameEast Asia",                        
              "region_nameEastern Europe",                   
              "region_nameHigh-income North America",        
              "region_nameNorth Africa and Middle East",     
              "region_nameSoutheast Asia",                   
              "region_nameSouthern Latin America",           
              "region_nameTropical Latin America",           
              "region_nameWestern Europe",                   
              "super_region_nameHigh-income",                
              "super_region_nameLatin America and Caribbean",
              "super_region_nameSub-Saharan Africa"),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

dat2 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in logit space
  obs_se = "log_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("location_nameAlbania",                                             
              "location_nameArmenia",                                             
              "location_nameAustralia",                                           
              "location_nameAustria",                                             
              "location_nameBelgium",   
              "location_nameBenin",
              "location_nameBolivia (Plurinational State of)",                    
              "location_nameBrazil",                                              
              "location_nameBulgaria",                                            
              "location_nameCambodia",                                            
              "location_nameCameroon",                                            
              "location_nameChile",                                               
              "location_nameChina",                                               
              "location_nameCroatia",                                             
              "location_nameCyprus",                                              
              "location_nameCzechia",                                             
              "location_nameDenmark",                                             
              "location_nameEcuador",                                             
              "location_nameEgypt",                                               
              "location_nameEl Salvador",   
              "location_nameEstonia", 
              "location_nameFinland",                                             
              "location_nameFrance",                                             
              "location_nameGeorgia",                                             
              "location_nameGermany",                                             
              "location_nameGhana",                                               
              "location_nameGreece",                                              
              "location_nameHonduras",                                            
              "location_nameHungary",                                             
              "location_nameIceland",                                             
              "location_nameIndia",                                               
              "location_nameIran (Islamic Republic of)",                          
              "location_nameIsrael",                                              
              "location_nameItaly",                                               
              "location_nameJapan",                                               
              "location_nameLatvia",                                              
              "location_nameLithuania",                                           
              "location_nameLuxembourg",                                          
              "location_nameMalaysia",                                            
              "location_nameMexico",                                              
              "location_nameMongolia",                                            
              "location_nameMozambique",                                          
              "location_nameNamibia",                                             
              "location_nameNetherlands",                                         
              "location_nameNew Zealand",                                         
              "location_nameNigeria",                                             
              "location_nameNorway",                                              
              "location_namePakistan",                                            
              "location_nameParaguay",                                            
              "location_namePhilippines",                                         
              "location_namePoland",                                              
              "location_namePortugal",                                            
              "location_nameRepublic of Korea",                                   
              "location_nameRomania",                                             
              "location_nameRussian Federation",   
              "location_nameSierra Leone",                                        
              "location_nameSlovakia",                                            
              "location_nameSlovenia",                                            
              "location_nameSouth Africa",                                        
              "location_nameSpain",                                               
              "location_nameSuriname",                                            
              "location_nameSweden",                                              
              "location_nameSwitzerland", 
              "location_nameThailand",
              "location_nameTajikistan",         
              "location_nameTurkey",
              "location_nameUkraine",                                             
              "location_nameUnited Kingdom",                                      
              "location_nameUnited Republic of Tanzania",    
              "location_nameUruguay",
              "location_nameVenezuela (Bolivarian Republic of)",                  
              "location_nameZambia"          ),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list(CovModel(cov_name = "region_nameAndean Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameAustralasia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameCentral Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameCentral Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameCentral Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameEast Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameEastern Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameHigh-income North America", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameNorth Africa and Middle East", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameSoutheast Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                    CovModel(cov_name = "region_nameSouthern Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                    CovModel(cov_name = "region_nameTropical Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "region_nameWestern Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "super_region_nameHigh-income", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                    CovModel(cov_name = "super_region_nameLatin America and Caribbean", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                    CovModel(cov_name = "super_region_nameSub-Saharan Africa", prior_beta_gaussian = list(admin = array(c(0,7.5))))
                     # specify covariate details
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

fit2 <- CWModel(
  cwdata = dat2,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list(CovModel(cov_name = "location_nameAlbania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameArmenia",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameAustralia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                          
                    CovModel(cov_name = "location_nameAustria",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameBelgium",prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                    CovModel(cov_name = "location_nameBenin",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameBolivia (Plurinational State of)", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                    
                    CovModel(cov_name = "location_nameBrazil",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_nameBulgaria", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameCambodia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameCameroon", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameChile", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_nameChina", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_nameCroatia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameCyprus",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_nameCzechia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameDenmark",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameEcuador",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameEgypt", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_nameEl Salvador", prior_beta_gaussian = list(admin = array(c(0,7.5)))),  
                    CovModel(cov_name = "location_nameEstonia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                        
                    CovModel(cov_name = "location_nameFinland", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameFrance", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameGeorgia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameGermany",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameGhana", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_nameGreece", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameHonduras",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameHungary",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameIceland", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameIndia",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameIran (Islamic Republic of)", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                         
                    CovModel(cov_name = "location_nameIsrael",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameItaly", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_nameJapan",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameLatvia",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameLithuania",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameLuxembourg",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                          
                    CovModel(cov_name = "location_nameMalaysia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameMexico", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameMongolia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_nameMozambique",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                         
                    CovModel(cov_name = "location_nameNamibia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameNetherlands", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                        
                    CovModel(cov_name = "location_nameNew Zealand",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                         
                    CovModel(cov_name = "location_nameNigeria", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameNorway", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_namePakistan",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameParaguay", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                           
                    CovModel(cov_name = "location_namePhilippines", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                        
                    CovModel(cov_name = "location_namePoland",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_namePortugal",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameRepublic of Korea",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                 
                    CovModel(cov_name = "location_nameRomania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameRussian Federation",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                
                    CovModel(cov_name = "location_nameSierra Leone", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                       
                    CovModel(cov_name = "location_nameSlovakia",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameSlovenia",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameSouth Africa", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                       
                    CovModel(cov_name = "location_nameSpain", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                              
                    CovModel(cov_name = "location_nameSuriname",  prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                          
                    CovModel(cov_name = "location_nameSweden", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                             
                    CovModel(cov_name = "location_nameSwitzerland", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                        
                    CovModel(cov_name = "location_nameThailand", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                        
                    CovModel(cov_name = "location_nameTajikistan",prior_beta_gaussian = list(admin = array(c(0,7.5)))),  
                    CovModel(cov_name = "location_nameTurkey",prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                          
                    CovModel(cov_name = "location_nameUkraine", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                            
                    CovModel(cov_name = "location_nameUnited Kingdom", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                                     
                    CovModel(cov_name = "location_nameUnited Republic of Tanzania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),      
                    CovModel(cov_name = "location_nameUruguay", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                        
                    CovModel(cov_name = "location_nameVenezuela (Bolivarian Republic of)", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                 
                    CovModel(cov_name = "location_nameZambia",prior_beta_gaussian = list(admin = array(c(0,7.5))))           

),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

df_result1 <- data.table(fit1$create_result_df())
df_result1 <- df_result1[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

df_result2 <- data.table(fit2$create_result_df())
df_result2 <- df_result2[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

results1 <- adjust_orig_vals(fit_object = fit1, 
                             df = df_orig[location_name %ni% gsub("location_name", "", c(loc_covs[me_name == 'hrh_nurseagg']$V2, 'location_nameThailand')) & location_name %ni% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')

results2 <- adjust_orig_vals(fit_object = fit2, 
                             df = df_orig[location_name %in% gsub("location_name", "", c(loc_covs[me_name == 'hrh_nurseagg']$V2, 'location_nameThailand')) | location_name %in% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')


sr_reg_adj <- cbind(df_orig[location_name %ni% gsub("location_name", "", c(loc_covs[me_name == 'hrh_nurseagg']$V2, 'location_nameThailand')) & location_name %ni% unique(df_matched$location_name) , c("location_id",                                  
                                                                                                                                                "ihme_loc_id",                                  
                                                                                                                                                "year_id",                                      
                                                                                                                                                "me_name",                                      
                                                                                                                                                "val",                                          
                                                                                                                                                "variance",                                     
                                                                                                                                                "obs_method",                                   
                                                                                                                                                "region_name",                                  
                                                                                                                                                "super_region_name")], results1)[, adjustment_type := 'super-region/region adjustment']

loc_adj <- cbind(df_orig[location_name %in% gsub("location_name", "", c(loc_covs[me_name == 'hrh_nurseagg']$V2, 'location_nameThailand')) | location_name %in% unique(df_matched$location_name), c("location_id",                                  
                                                                                      "ihme_loc_id",                                  
                                                                                      "year_id",                                      
                                                                                      "me_name",                                      
                                                                                      "val",                                          
                                                                                      "variance",                                     
                                                                                      "obs_method",                                   
                                                                                      "region_name",                                  
                                                                                      "super_region_name")], results2)[, adjustment_type := 'location adjustment']

xwalk_adj <- rbind(sr_reg_adj, loc_adj)
xwalk_adj[, adjustment_type := ifelse(pred_diff_mean == 0, 'no adjustment', adjustment_type)]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_nurseagg', pred_diff_mean := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta])]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_nurseagg', pred_diff_sd := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta_sd])]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_nurseagg', ref_vals_mean := exp(log(val)-pred_diff_mean)]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_nurseagg', ref_vals_sd := variance+pred_diff_sd]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_nurseagg', adjustment_type := 'super-region/region adjustment']

write.csv(xwalk_adj, 'FILEPATH/hrh_nurseagg_admin_adj.csv', row.names = F)

##
df_matched <- df_matched_orig[me_name == 'hrh_dent']

## create location and super-region/region dummies for matched pairs
sr_reg_dummies_matched <- dummy.data.frame(df_matched[, c('region_name', 'super_region_name')])
loc_dummies_matched <- dummy.data.frame(df_matched[, c('location_name')])

## subset dummy covariates to non-zero Bayesian lasso coefficients 
df_matched <- cbind(df_matched, loc_dummies_matched[, names(loc_dummies_matched) %in% c(unique(loc_covs[me_name == 'hrh_dent']$V2), 'location_nameChina', 'location_nameThailand', 'location_namePanama')])
df_matched <- cbind(df_matched, sr_reg_dummies_matched[, names(sr_reg_dummies_matched) %in% unique(sr_reg_covs[me_name == 'hrh_dent' & !(sr_reg_covs$V2 %like% 'Sub-Saharan Africa'), V2])], sr_reg_dummies_matched[,'super_region_nameSub-Saharan Africa'])
setnames(df_matched, old = 'V3', new = 'super_region_nameSub-Saharan Africa')

############################ XWALK #####################################
dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$alt_val,
    sd = df_matched$alt_variance,
    transformation = "linear_to_log" ),
  delta_transform(
    mean = df_matched$ref_val, 
    sd = df_matched$ref_variance,
    transformation = "linear_to_log")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("log_diff", "log_diff_se")] <- calculate_diff(
  df = dat_diff, 
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )

dat1 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in log space
  obs_se = "log_diff_se", # SE of matched differences in log space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("region_nameAndean Latin America",                                  
              "region_nameAustralasia",                                           
              "region_nameCaribbean",                                             
              "region_nameCentral Asia",                                          
              "region_nameCentral Latin America",                                 
              "region_nameEastern Europe",                                        
              "region_nameHigh-income Asia Pacific",                              
              "region_nameHigh-income North America",                             
              "region_nameSouth Asia",                                            
              "region_nameSoutheast Asia",                                        
              "region_nameSouthern Latin America",                                
              "region_nameTropical Latin America",                                
              "super_region_nameCentral Europe, Eastern Europe, and Central Asia",
              "super_region_nameHigh-income",                                     
              "super_region_nameSoutheast Asia, East Asia, and Oceania",                 
              "super_region_nameSub-Saharan Africa"),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

dat2 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in logit space
  obs_se = "log_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("location_nameAlbania",
              "location_nameAustria",                           
              "location_nameBelarus",
              "location_nameBolivia (Plurinational State of)",  
              "location_nameBotswana",
              "location_nameBrazil",                            
              "location_nameBulgaria",                           
              "location_nameCambodia",                          
              "location_nameCameroon",                     
              "location_nameChile",  
              "location_nameChina",
              "location_nameCosta Rica",                      
              "location_nameCroatia",                           
              "location_nameCyprus",                          
              "location_nameCzechia",                           
              "location_nameDenmark",                          
              "location_nameDominican Republic",                
              "location_nameEcuador",                        
              "location_nameEstonia",                           
              "location_nameFinland",                        
              "location_nameFrance",                            
              "location_nameGeorgia",                       
              "location_nameGermany",                           
              "location_nameGreece",                          
              "location_nameHungary",                           
              "location_nameIceland",                         
              "location_nameIndia",                             
              "location_nameIreland",                          
              "location_nameJapan",                             
              "location_nameJordan",                          
              "location_nameLatvia",                            
              "location_nameLithuania",                        
              "location_nameLuxembourg",                        
              "location_nameMalaysia",                        
              "location_nameMexico",                           
              "location_nameNamibia",                           
              "location_nameNetherlands",                       
              "location_nameNew Zealand",                      
              "location_nameNicaragua",                         
              "location_nameNorway",                           
              "location_namePakistan", 
              "location_namePanama",
              "location_namePhilippines",                     
              "location_namePoland",                            
              "location_nameRepublic of Korea",                
              "location_nameRomania",                           
              "location_nameRussian Federation",                
              "location_nameSlovenia",                          
              "location_nameSouth Africa",                     
              "location_nameSpain",                             
              "location_nameSwitzerland",                      
              "location_nameTajikistan",                        
              "location_nameTrinidad and Tobago",  
              "location_nameThailand",
              "location_nameUnited Kingdom",                    
              "location_nameUnited Republic of Tanzania",     
              "location_nameUnited States of America",          
              "location_nameUruguay",                         
              "location_nameVenezuela (Bolivarian Republic of)",
              "location_nameZambia"),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list( CovModel(cov_name = "region_nameAndean Latin America", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "region_nameAustralasia", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "region_nameCaribbean", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "region_nameCentral Asia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "region_nameCentral Latin America", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "region_nameEastern Europe", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "region_nameHigh-income Asia Pacific", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "region_nameHigh-income North America", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "region_nameSouth Asia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "region_nameSoutheast Asia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "region_nameSouthern Latin America", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "region_nameTropical Latin America", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = 'super_region_nameCentral Europe, Eastern Europe, and Central Asia', prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = 'super_region_nameHigh-income', prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     #CovModel(cov_name = 'super_region_nameSoutheast Asia, East Asia, and Oceania', prior_beta_gaussian = list(admin = array(c(0,50.0)))),                     
                     CovModel(cov_name = 'super_region_nameSub-Saharan Africa', prior_beta_gaussian = list(admin = array(c(0,50.0))))
                     # specify covariate details
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

fit2 <- CWModel(
  cwdata = dat2,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list(
                     # specify covariate details
                     CovModel(cov_name = "location_nameAlbania", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "location_nameAustria", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                           
                     CovModel(cov_name = "location_nameBelarus",prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "location_nameBolivia (Plurinational State of)",prior_beta_gaussian = list(admin = array(c(0,50.0)))),  
                     CovModel(cov_name = "location_nameBotswana",prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "location_nameBrazil",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                            
                     CovModel(cov_name = "location_nameBulgaria",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                           
                     CovModel(cov_name = "location_nameCambodia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                          
                     CovModel(cov_name = "location_nameCameroon", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                    
                     CovModel(cov_name = "location_nameChile", prior_beta_gaussian = list(admin = array(c(0,50.0)))),  
                     CovModel(cov_name = "location_nameChina", prior_beta_gaussian = list(admin = array(c(0,50.0)))),  
                     CovModel(cov_name = "location_nameCosta Rica",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                      
                     CovModel(cov_name = "location_nameCroatia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                          
                     CovModel(cov_name = "location_nameCyprus", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
                     CovModel(cov_name = "location_nameCzechia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                           
                     CovModel(cov_name = "location_nameDenmark", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
                     CovModel(cov_name = "location_nameDominican Republic",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                
                     CovModel(cov_name = "location_nameEcuador", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                       
                     CovModel(cov_name = "location_nameEstonia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                          
                     CovModel(cov_name = "location_nameFinland", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                       
                     CovModel(cov_name = "location_nameFrance", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                            
                     CovModel(cov_name = "location_nameGeorgia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                      
                     CovModel(cov_name = "location_nameGermany", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                          
                     CovModel(cov_name = "location_nameGreece", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
                     CovModel(cov_name = "location_nameHungary", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                          
                     CovModel(cov_name = "location_nameIceland",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
                     CovModel(cov_name = "location_nameIndia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                            
                     CovModel(cov_name = "location_nameIreland", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
                     CovModel(cov_name = "location_nameJapan",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                             
                     CovModel(cov_name = "location_nameJordan", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
                     CovModel(cov_name = "location_nameLatvia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                           
                     CovModel(cov_name = "location_nameLithuania",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                        
                     CovModel(cov_name = "location_nameLuxembourg",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                        
                     CovModel(cov_name = "location_nameMalaysia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                       
                     CovModel(cov_name = "location_nameMexico", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                          
                     CovModel(cov_name = "location_nameNamibia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                          
                     CovModel(cov_name = "location_nameNetherlands",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                       
                     CovModel(cov_name = "location_nameNew Zealand", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                     
                     CovModel(cov_name = "location_nameNicaragua", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                        
                     CovModel(cov_name = "location_nameNorway",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
                     CovModel(cov_name = "location_namePakistan", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "location_namePanama", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                    
                     CovModel(cov_name = "location_namePhilippines", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                    
                     CovModel(cov_name = "location_namePoland",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),                          
                     CovModel(cov_name = "location_nameRepublic of Korea", prior_beta_gaussian = list(admin = array(c(0,50.0)))),               
                     CovModel(cov_name = "location_nameRomania",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),                        
                     CovModel(cov_name = "location_nameRussian Federation", prior_beta_gaussian = list(admin = array(c(0,50.0)))),               
                     CovModel(cov_name = "location_nameSlovenia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
                     CovModel(cov_name = "location_nameSouth Africa",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),                   
                     CovModel(cov_name = "location_nameSpain", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                            
                     CovModel(cov_name = "location_nameSwitzerland", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                     
                     CovModel(cov_name = "location_nameTajikistan", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                       
                     CovModel(cov_name = "location_nameTrinidad and Tobago",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),  
                     CovModel(cov_name = "location_nameThailand",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),            
                     CovModel(cov_name = "location_nameUnited Kingdom", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                   
                     CovModel(cov_name = "location_nameUnited Republic of Tanzania",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),   
                     CovModel(cov_name = "location_nameUnited States of America", prior_beta_gaussian = list(admin = array(c(0,50.0)))),         
                     CovModel(cov_name = "location_nameUruguay",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),                       
                     CovModel(cov_name = "location_nameVenezuela (Bolivarian Republic of)",prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "location_nameZambia", prior_beta_gaussian = list(admin = array(c(0,50.0))))
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

df_result1 <- data.table(fit1$create_result_df())
df_result1 <- df_result1[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

df_result2 <- data.table(fit2$create_result_df())
df_result2 <- df_result2[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

results1 <- adjust_orig_vals(fit_object = fit1, 
                             df = df_orig[location_name %ni% gsub("location_name", "", c(loc_covs[me_name == 'hrh_dent']$V2, 'location_nameChina', 'location_nameThailand', 'location_namePanama')) & location_name %ni% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')

results2 <- adjust_orig_vals(fit_object = fit2, 
                             df = df_orig[location_name %in% gsub("location_name", "", c(loc_covs[me_name == 'hrh_dent']$V2), 'location_nameChina', 'location_nameThailand', 'location_namePanama') | location_name %in% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')


sr_reg_adj <- cbind(df_orig[location_name %ni% gsub("location_name", "", c(loc_covs[me_name == 'hrh_dent']$V2, 'location_nameChina', 'location_nameThailand', 'location_namePanama')) & location_name %ni% unique(df_matched$location_name) , c("location_id",                                  
                                                                                                                                                "ihme_loc_id",                                  
                                                                                                                                                "year_id",                                      
                                                                                                                                                "me_name",                                      
                                                                                                                                                "val",                                          
                                                                                                                                                "variance",                                     
                                                                                                                                                "obs_method",                                   
                                                                                                                                                "region_name",                                  
                                                                                                                                                "super_region_name")], results1)[, adjustment_type := 'super-region/region adjustment']

loc_adj <- cbind(df_orig[location_name %in% gsub("location_name", "", c(loc_covs[me_name == 'hrh_dent']$V2), 'location_nameChina', 'location_nameThailand') | location_name %in% unique(df_matched$location_name), c("location_id",                                  
                                                                                      "ihme_loc_id",                                  
                                                                                      "year_id",                                      
                                                                                      "me_name",                                      
                                                                                      "val",                                          
                                                                                      "variance",                                     
                                                                                      "obs_method",                                   
                                                                                      "region_name",                                  
                                                                                      "super_region_name")], results2)[, adjustment_type := 'location adjustment']

xwalk_adj <- rbind(sr_reg_adj, loc_adj)
xwalk_adj[, adjustment_type := ifelse(pred_diff_mean == 0, 'no adjustment', adjustment_type)]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dent', pred_diff_mean := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta])]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dent', pred_diff_sd := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta_sd])]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dent', ref_vals_mean := exp(log(val)-pred_diff_mean)]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dent', ref_vals_sd := variance+pred_diff_sd]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dent', adjustment_type := 'super-region/region adjustment']

write.csv(xwalk_adj, 'FILEPATH/hrh_dent_admin_adj.csv', row.names = F)

##
df_matched <- df_matched_orig[me_name == 'hrh_pharm']

## create location and super-region/region dummies for matched pairs
sr_reg_dummies_matched <- dummy.data.frame(df_matched[, c('region_name', 'super_region_name')])
loc_dummies_matched <- dummy.data.frame(df_matched[, c('location_name')])

## subset dummy covariates to non-zero Bayesian lasso coefficients 
df_matched <- cbind(df_matched, loc_dummies_matched[, names(loc_dummies_matched) %in% unique(loc_covs[me_name == 'hrh_pharm']$V2)])
df_matched <- cbind(df_matched, sr_reg_dummies_matched[, names(sr_reg_dummies_matched) %in% unique(sr_reg_covs[me_name == 'hrh_pharm' & !(sr_reg_covs$V2 %like% 'Sub-Saharan Africa'), V2])], sr_reg_dummies_matched[,'super_region_nameSub-Saharan Africa'])
setnames(df_matched, old = 'V3', new = 'super_region_nameSub-Saharan Africa')

############################ XWALK #####################################
dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$alt_val,
    sd = df_matched$alt_variance,
    transformation = "linear_to_log" ),
  delta_transform(
    mean = df_matched$ref_val, 
    sd = df_matched$ref_variance,
    transformation = "linear_to_log")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("log_diff", "log_diff_se")] <- calculate_diff(
  df = dat_diff, 
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )

dat1 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in log space
  obs_se = "log_diff_se", # SE of matched differences in log space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("region_nameAndean Latin America",                                  
              "region_nameCentral Asia",                                          
              "region_nameEastern Europe",                                        
              "region_nameSouthern Latin America",                                
              "region_nameWestern Europe",                                        
              "super_region_nameCentral Europe, Eastern Europe, and Central Asia",
              "super_region_nameHigh-income", 
              "super_region_nameSub-Saharan Africa"),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

dat2 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in logit space
  obs_se = "log_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("location_nameAlbania",                                             
              "location_nameArmenia",                                             
              "location_nameAustralia",                                           
              "location_nameAustria",                                             
              "location_nameBelarus",                                             
              "location_nameBelgium",                                             
              "location_nameBolivia (Plurinational State of)",                    
              "location_nameBotswana",                                            
              "location_nameBrazil",                                              
              "location_nameBulgaria",                                            
              "location_nameCameroon",                                            
              "location_nameChile",                                               
              "location_nameCosta Rica",                                          
              "location_nameCyprus",                                              
              "location_nameCzechia",                                             
              "location_nameDenmark",                                             
              "location_nameEcuador",                                             
              "location_nameEgypt",                                               
              "location_nameEl Salvador",                                         
              "location_nameFinland",                                             
              "location_nameFrance",                                             
              "location_nameGeorgia",                                             
              "location_nameGermany",                                             
              "location_nameGreece",                                              
              "location_nameHungary",                                             
              "location_nameIceland",                                             
              "location_nameIndia",                                               
              "location_nameIran (Islamic Republic of)",                          
              "location_nameIreland",                                             
              "location_nameIsrael",                                             
              "location_nameJordan",                                              
              "location_nameLatvia",                                              
              "location_nameLithuania",                                           
              "location_nameLuxembourg",                                          
              "location_nameMalaysia",                                            
              "location_nameMexico",                                              
              "location_nameMongolia",                                            
              "location_nameNetherlands",                                         
              "location_nameNew Zealand",                                         
              "location_nameNorway",                                              
              "location_nameParaguay",                                            
              "location_namePhilippines",                                         
              "location_namePoland",                                              
              "location_namePortugal",                                            
              "location_nameRepublic of Korea",                                   
              "location_nameRomania",                                             
              "location_nameRussian Federation",                                  
              "location_nameSlovenia",                                            
              "location_nameSouth Africa",                                        
              "location_nameSpain",                                               
              "location_nameSweden",                                              
              "location_nameSwitzerland",                                         
              "location_nameTrinidad and Tobago",                                 
              "location_nameTurkey",                                              
              "location_nameUnited Kingdom",                                      
              "location_nameUnited Republic of Tanzania",                         
              "location_nameZambia"   ),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list( CovModel(cov_name = "region_nameAndean Latin America", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "region_nameCentral Asia", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "region_nameEastern Europe", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "region_nameSouthern Latin America", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "region_nameWestern Europe", prior_beta_gaussian = list(admin = array(c(0,50.0)))), 
                     CovModel(cov_name = "super_region_nameCentral Europe, Eastern Europe, and Central Asia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "super_region_nameHigh-income", prior_beta_gaussian = list(admin = array(c(0,50.0)))),
                     CovModel(cov_name = "super_region_nameSub-Saharan Africa", prior_beta_gaussian = list(admin = array(c(0,50.0))))
                     # specify covariate details
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

fit2 <- CWModel(
  cwdata = dat2,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list(CovModel(cov_name = "location_nameAlbania",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
                    CovModel(cov_name = "location_nameArmenia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
                             CovModel(cov_name = "location_nameAustralia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                           
                                      CovModel(cov_name = "location_nameAustria",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
                                               CovModel(cov_name = "location_nameBelarus", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_nameBelgium", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                         
    CovModel(cov_name = "location_nameBolivia (Plurinational State of)", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                   
    CovModel(cov_name = "location_nameBotswana", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                           
    CovModel(cov_name = "location_nameBrazil", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameBulgaria",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_nameCameroon", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                           
    CovModel(cov_name = "location_nameChile", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                              
    CovModel(cov_name = "location_nameCosta Rica", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                         
    CovModel(cov_name = "location_nameCyprus", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameCzechia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_nameDenmark",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameEcuador", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_nameEgypt", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                              
    CovModel(cov_name = "location_nameEl Salvador", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                        
    CovModel(cov_name = "location_nameFinland",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameFrance",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameGeorgia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameGermany",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameGreece",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                              
    CovModel(cov_name = "location_nameHungary",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameIceland",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameIndia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                               
    CovModel(cov_name = "location_nameIran (Islamic Republic of)", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
    CovModel(cov_name = "location_nameIreland",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameIsrael", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_nameJordan", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameLatvia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                              
    CovModel(cov_name = "location_nameLithuania",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                           
    CovModel(cov_name = "location_nameLuxembourg",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                          
    CovModel(cov_name = "location_nameMalaysia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_nameMexico",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                              
    CovModel(cov_name = "location_nameMongolia",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_nameNetherlands",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                         
    CovModel(cov_name = "location_nameNew Zealand",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                         
    CovModel(cov_name = "location_nameNorway", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameParaguay",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_namePhilippines", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                        
    CovModel(cov_name = "location_namePoland", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_namePortugal", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                           
    CovModel(cov_name = "location_nameRepublic of Korea",   prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                
    CovModel(cov_name = "location_nameRomania", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                            
    CovModel(cov_name = "location_nameRussian Federation",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                
    CovModel(cov_name = "location_nameSlovenia", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                           
    CovModel(cov_name = "location_nameSouth Africa", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                       
    CovModel(cov_name = "location_nameSpain", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                              
    CovModel(cov_name = "location_nameSweden", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameSwitzerland", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                        
    CovModel(cov_name = "location_nameTrinidad and Tobago",  prior_beta_gaussian = list(admin = array(c(0,50.0)))),                               
    CovModel(cov_name = "location_nameTurkey", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                             
    CovModel(cov_name = "location_nameUnited Kingdom", prior_beta_gaussian = list(admin = array(c(0,50.0)))),                                     
    CovModel(cov_name = "location_nameUnited Republic of Tanzania",prior_beta_gaussian = list(admin = array(c(0,50.0)))),                         
    CovModel(cov_name = "location_nameZambia", prior_beta_gaussian = list(admin = array(c(0,50.0))))   
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

df_result1 <- data.table(fit1$create_result_df())
df_result1 <- df_result1[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

df_result2 <- data.table(fit2$create_result_df())
df_result2 <- df_result2[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

results1 <- adjust_orig_vals(fit_object = fit1, 
                             df = df_orig[location_name %ni% gsub("location_name", "", loc_covs[me_name == 'hrh_pharm']$V2) & location_name %ni% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')

results2 <- adjust_orig_vals(fit_object = fit2, 
                             df = df_orig[location_name %in% gsub("location_name", "", loc_covs[me_name == 'hrh_pharm']$V2) | location_name %in% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')


sr_reg_adj <- cbind(df_orig[location_name %ni% gsub("location_name", "", loc_covs[me_name == 'hrh_pharm']$V2) & location_name %ni% unique(df_matched$location_name) , c("location_id",                                  
                                                                                                                                                                       "ihme_loc_id",                                  
                                                                                                                                                                       "year_id",                                      
                                                                                                                                                                       "me_name",                                      
                                                                                                                                                                       "val",                                          
                                                                                                                                                                       "variance",                                     
                                                                                                                                                                       "obs_method",                                   
                                                                                                                                                                       "region_name",                                  
                                                                                                                                                                       "super_region_name")], results1)[, adjustment_type := 'super-region/region adjustment']

loc_adj <- cbind(df_orig[location_name %in% gsub("location_name", "", loc_covs[me_name == 'hrh_pharm']$V2) | location_name %in% unique(df_matched$location_name), c("location_id",                                  
                                                                                                             "ihme_loc_id",                                  
                                                                                                             "year_id",                                      
                                                                                                             "me_name",                                      
                                                                                                             "val",                                          
                                                                                                             "variance",                                     
                                                                                                             "obs_method",                                   
                                                                                                             "region_name",                                  
                                                                                                             "super_region_name")], results2)[, adjustment_type := 'location adjustment']

xwalk_adj <- rbind(sr_reg_adj, loc_adj)
xwalk_adj[, adjustment_type := ifelse(pred_diff_mean == 0, 'no adjustment', adjustment_type)]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharm', pred_diff_mean := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta])]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharm', pred_diff_sd := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta_sd])]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharm', ref_vals_mean := exp(log(val)-pred_diff_mean)]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharm', ref_vals_sd := variance+pred_diff_sd]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharm', adjustment_type := 'super-region/region adjustment']

write.csv(xwalk_adj, 'FILEPATH/hrh_pharm_admin_adj.csv', row.names = F)

##
df_matched <- df_matched_orig[me_name == 'hrh_dentass']

## create location and super-region/region dummies for matched pairs
sr_reg_dummies_matched <- dummy.data.frame(df_matched[, c('region_name', 'super_region_name')])
loc_dummies_matched <- dummy.data.frame(df_matched[, c('location_name')])

## subset dummy covariates to non-zero Bayesian lasso coefficients 
df_matched <- cbind(df_matched, loc_dummies_matched[, names(loc_dummies_matched) %in% unique(loc_covs[me_name == 'hrh_dentass']$V2)])
df_matched <- cbind(df_matched, sr_reg_dummies_matched[, names(sr_reg_dummies_matched) %in% unique(sr_reg_covs[me_name == 'hrh_dentass' & !(sr_reg_covs$V2 %like% 'Sub-Saharan Africa'), V2])], sr_reg_dummies_matched[,'super_region_nameSub-Saharan Africa'])
setnames(df_matched, old = 'V3', new = 'super_region_nameSub-Saharan Africa')

############################ XWALK #####################################
dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$alt_val,
    sd = df_matched$alt_variance,
    transformation = "linear_to_log" ),
  delta_transform(
    mean = df_matched$ref_val, 
    sd = df_matched$ref_variance,
    transformation = "linear_to_log")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("log_diff", "log_diff_se")] <- calculate_diff(
  df = dat_diff, 
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )

dat1 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in log space
  obs_se = "log_diff_se", # SE of matched differences in log space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("region_nameAndean Latin America",                        
              "region_nameAustralasia",                                 
              "region_nameCentral Asia",                                
              "region_nameCentral Europe",                              
              "region_nameEastern Europe",                              
              "region_nameHigh-income Asia Pacific",                    
              "region_nameNorth Africa and Middle East",                
              "region_nameSoutheast Asia",                              
              "region_nameSouthern Latin America",                      
              "region_nameWestern Europe",                              
              "super_region_nameSoutheast Asia, East Asia, and Oceania",
              "super_region_nameSub-Saharan Africa"),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

dat2 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in logit space
  obs_se = "log_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("location_nameBolivia (Plurinational State of)",
              "location_nameCambodia",                        
              "location_nameChile",                            
              "location_nameCosta Rica",                      
              "location_nameCyprus",                           
              "location_nameEstonia",                         
              "location_nameIceland",                          
              "location_nameIran (Islamic Republic of)",      
              "location_nameJordan",                           
              "location_nameMongolia",                        
              "location_nameNew Zealand",                      
              "location_nameParaguay",                        
              "location_nameRepublic of Korea" ,               
              "location_nameRomania",                         
              "location_nameSouth Africa",                     
              "location_nameUnited Kingdom",                  
              "location_nameUnited Republic of Tanzania",      
              "location_nameZambia" ),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"                                 
  cov_models = list( CovModel(cov_name = "region_nameAndean Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameAustralasia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameCentral Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameCentral Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameEastern Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameHigh-income Asia Pacific", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameNorth Africa and Middle East", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameSoutheast Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameSouthern Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "region_nameWestern Europe", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     #CovModel(cov_name = "super_region_nameSoutheast Asia, East Asia, and Oceania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "super_region_nameSub-Saharan Africa", prior_beta_gaussian = list(admin = array(c(0,7.5))))
                     # specify covariate details
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

fit2 <- CWModel(
  cwdata = dat2,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list(CovModel(cov_name = "location_nameBolivia (Plurinational State of)", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                    CovModel(cov_name = "location_nameCambodia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                        
                    CovModel(cov_name = "location_nameChile", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                           
                    CovModel(cov_name = "location_nameCosta Rica", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                     
                    CovModel(cov_name = "location_nameCyprus", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                          
                    CovModel(cov_name = "location_nameEstonia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                        
                    CovModel(cov_name = "location_nameIceland", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                          
                    CovModel(cov_name = "location_nameIran (Islamic Republic of)", prior_beta_gaussian = list(admin = array(c(0,7.5)))),     
                    CovModel(cov_name = "location_nameJordan", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                          
                    CovModel(cov_name = "location_nameMongolia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                       
                    CovModel(cov_name = "location_nameNew Zealand", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                     
                    CovModel(cov_name = "location_nameParaguay", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                       
                    CovModel(cov_name = "location_nameRepublic of Korea", prior_beta_gaussian = list(admin = array(c(0,7.5)))),              
                    CovModel(cov_name = "location_nameRomania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                        
                    CovModel(cov_name = "location_nameSouth Africa", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                    
                    CovModel(cov_name = "location_nameUnited Kingdom", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                 
                    CovModel(cov_name = "location_nameUnited Republic of Tanzania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),     
                    CovModel(cov_name = "location_nameZambia", prior_beta_gaussian = list(admin = array(c(0,7.5))))
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

df_result1 <- data.table(fit1$create_result_df())
df_result1 <- df_result1[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

df_result2 <- data.table(fit2$create_result_df())
df_result2 <- df_result2[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

results1 <- adjust_orig_vals(fit_object = fit1, 
                             df = df_orig[location_name %ni% gsub("location_name", "", loc_covs[me_name == 'hrh_dentass']$V2) & location_name %ni% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')

results2 <- adjust_orig_vals(fit_object = fit2, 
                             df = df_orig[location_name %in% gsub("location_name", "", loc_covs[me_name == 'hrh_dentass']$V2) | location_name %in% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')


sr_reg_adj <- cbind(df_orig[location_name %ni% gsub("location_name", "", loc_covs[me_name == 'hrh_dentass']$V2) & location_name %ni% unique(df_matched$location_name) , c("location_id",                                  
                                                                                                                                                                        "ihme_loc_id",                                  
                                                                                                                                                                        "year_id",                                      
                                                                                                                                                                        "me_name",                                      
                                                                                                                                                                        "val",                                          
                                                                                                                                                                        "variance",                                     
                                                                                                                                                                        "obs_method",                                   
                                                                                                                                                                        "region_name",                                  
                                                                                                                                                                        "super_region_name")], results1)[, adjustment_type := 'super-region/region adjustment']

loc_adj <- cbind(df_orig[location_name %in% gsub("location_name", "", loc_covs[me_name == 'hrh_dentass']$V2) | location_name %in% unique(df_matched$location_name), c("location_id",                                  
                                                                                                              "ihme_loc_id",                                  
                                                                                                              "year_id",                                      
                                                                                                              "me_name",                                      
                                                                                                              "val",                                          
                                                                                                              "variance",                                     
                                                                                                              "obs_method",                                   
                                                                                                              "region_name",                                  
                                                                                                              "super_region_name")], results2)[, adjustment_type := 'location adjustment']

xwalk_adj <- rbind(sr_reg_adj, loc_adj)
xwalk_adj[, adjustment_type := ifelse(pred_diff_mean == 0, 'no adjustment', adjustment_type)]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dentass', pred_diff_mean := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta])]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dentass', pred_diff_sd := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta_sd])]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dentass', ref_vals_mean := exp(log(val)-pred_diff_mean)]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dentass', ref_vals_sd := variance+pred_diff_sd]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_dentass', adjustment_type := 'super-region/region adjustment']

write.csv(xwalk_adj, 'FILEPATH/hrh_dentass_admin_adj.csv', row.names = F)

##
df_matched <- df_matched_orig[me_name == 'hrh_pharmtech']

## create location and super-region/region dummies for matched pairs
sr_reg_dummies_matched <- dummy.data.frame(df_matched[, c('region_name', 'super_region_name')])
loc_dummies_matched <- dummy.data.frame(df_matched[, c('location_name')])

## subset dummy covariates to non-zero Bayesian lasso coefficients 
df_matched <- cbind(df_matched, loc_dummies_matched[, names(loc_dummies_matched) %in% unique(loc_covs[me_name == 'hrh_pharmtech']$V2)])
df_matched <- cbind(df_matched, sr_reg_dummies_matched[, names(sr_reg_dummies_matched) %in% unique(sr_reg_covs[me_name == 'hrh_pharmtech' & !(sr_reg_covs$V2 %like% 'Sub-Saharan Africa'), V2])], sr_reg_dummies_matched[,'super_region_nameSub-Saharan Africa'])
setnames(df_matched, old = 'V3', new = 'super_region_nameSub-Saharan Africa')

############################ XWALK #####################################
dat_diff <- as.data.frame(cbind(
  delta_transform(
    mean = df_matched$alt_val,
    sd = df_matched$alt_variance,
    transformation = "linear_to_log" ),
  delta_transform(
    mean = df_matched$ref_val, 
    sd = df_matched$ref_variance,
    transformation = "linear_to_log")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

df_matched[, c("log_diff", "log_diff_se")] <- calculate_diff(
  df = dat_diff, 
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )

dat1 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in log space
  obs_se = "log_diff_se", # SE of matched differences in log space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("region_nameHigh-income North America",
              "region_nameNorth Africa and Middle East",
              "region_nameSoutheast Asia",
              "region_nameSouthern Latin America",  
              "super_region_nameSub-Saharan Africa"),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

dat2 <- CWData(
  df = df_matched,
  obs = "log_diff",       # matched differences in logit space
  obs_se = "log_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = list("location_nameCambodia",
              "location_nameChile",                      
              "location_nameCosta Rica",
              "location_nameCyprus",                     
              "location_nameIran (Islamic Republic of)",
              "location_nameUnited Republic of Tanzania",
              "location_nameUnited States of America"),       # list of (potential) covariate columns
  study_id = "id2"          # var for random intercepts; i.e. (1|study_id)
)

fit1 <- CWModel(
  cwdata = dat1,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"                                 
  cov_models = list( CovModel(cov_name = "region_nameHigh-income North America", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameNorth Africa and Middle East", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameSoutheast Asia", prior_beta_gaussian = list(admin = array(c(0,7.5)))), 
                     CovModel(cov_name = "region_nameSouthern Latin America", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                     CovModel(cov_name = "super_region_nameSub-Saharan Africa", prior_beta_gaussian = list(admin = array(c(0,7.5))))
                     # specify covariate details
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

fit2 <- CWModel(
  cwdata = dat2,           # result of CWData() function call
  obs_type = "diff_log", # must be "diff_logit" or "diff_log"
  cov_models = list(CovModel(cov_name = "location_nameCambodia", prior_beta_gaussian = list(admin = array(c(0,7.5)))),
                    CovModel(cov_name = "location_nameChile", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                        
                    CovModel(cov_name = "location_nameCosta Rica", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                           
                    CovModel(cov_name = "location_nameCyprus", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                     
                    CovModel(cov_name = "location_nameIran (Islamic Republic of)", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                          
                    CovModel(cov_name = "location_nameUnited Republic of Tanzania", prior_beta_gaussian = list(admin = array(c(0,7.5)))),                        
                    CovModel(cov_name = "location_nameUnited States of America", prior_beta_gaussian = list(admin = array(c(0,7.5))))                        
                    
  ),
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

df_result1 <- data.table(fit1$create_result_df())
df_result1 <- df_result1[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

df_result2 <- data.table(fit2$create_result_df())
df_result2 <- df_result2[dorms == 'admin', c("dorms","cov_names","beta","beta_sd","gamma")]

results1 <- adjust_orig_vals(fit_object = fit1, 
                             df = df_orig[location_name %ni% gsub("location_name", "", loc_covs[me_name == 'hrh_pharmtech']$V2) & location_name %ni% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')

results2 <- adjust_orig_vals(fit_object = fit2, 
                             df = df_orig[location_name %in% gsub("location_name", "", loc_covs[me_name == 'hrh_pharmtech']$V2) | location_name %in% unique(df_matched$location_name),], 
                             orig_dorms = 'obs_method', 
                             orig_vals_mean = 'val', 
                             orig_vals_se = 'variance')


sr_reg_adj <- cbind(df_orig[location_name %ni% gsub("location_name", "", loc_covs[me_name == 'hrh_pharmtech']$V2) & location_name %ni% unique(df_matched$location_name) , c("location_id",                                  
                                                                                                                                                                          "ihme_loc_id",                                  
                                                                                                                                                                          "year_id",                                      
                                                                                                                                                                          "me_name",                                      
                                                                                                                                                                          "val",                                          
                                                                                                                                                                          "variance",                                     
                                                                                                                                                                          "obs_method",                                   
                                                                                                                                                                          "region_name",                                  
                                                                                                                                                                          "super_region_name")], results1)[, adjustment_type := 'super-region/region adjustment']

loc_adj <- cbind(df_orig[location_name %in% gsub("location_name", "", loc_covs[me_name == 'hrh_pharmtech']$V2) | location_name %in% unique(df_matched$location_name), c("location_id",                                  
                                                                                                                "ihme_loc_id",                                  
                                                                                                                "year_id",                                      
                                                                                                                "me_name",                                      
                                                                                                                "val",                                          
                                                                                                                "variance",                                     
                                                                                                                "obs_method",                                   
                                                                                                                "region_name",                                  
                                                                                                                "super_region_name")], results2)[, adjustment_type := 'location adjustment']

xwalk_adj <- rbind(sr_reg_adj, loc_adj)
xwalk_adj[, adjustment_type := ifelse(pred_diff_mean == 0, 'no adjustment', adjustment_type)]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharmtech', pred_diff_mean := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta])]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharmtech', pred_diff_sd := mean(df_result1[cov_names %like% "Sub-Saharan Africa", beta_sd])]

#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharmtech', ref_vals_mean := exp(log(val)-pred_diff_mean)]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharmtech', ref_vals_sd := variance+pred_diff_sd]
#xwalk_adj[obs_method == 'admin' & region_name == 'Central Sub-Saharan Africa' & me_name == 'hrh_pharmtech', adjustment_type := 'super-region/region adjustment']

write.csv(xwalk_adj, 'FILEPATH/hrh_pharmtech_admin_adj.csv', row.names = F)