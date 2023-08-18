#####################################################################
## Name: Megan Knight                                              ##
## Purpose: Prepping inputs for female share of health workers to  ##
## apply to all final models.                                      ##
## Date: 07/16/2023                                                ##
#####################################################################
## clear memory
rm(list=ls())

## runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <- paste0("/homes/", Sys.getenv("USER"), "/")
  k <- "/ihme/cc_resources/libraries"
} else {
  j <- "J:/"
  h <- "H:/"
  k <- "K:/libraries"
}

## libraries 
pacman::p_load(openxlsx,stringr)

## directories 
## TODO: use this directory once splititng has been performed 
dir <- "/mnt/share/scratch/projects/hssa/occ/prepped/hrh/gbd_2020/final"
tab_dir <- '/mnt/share/scratch/projects/hssa/occ/by_sex/sources/raw/data'
source_dir <- '/ihme/cc_resources/libraries/current/r/'
tracker_dir <- '/mnt/share/scratch/projects/hssa/occ/model/gbd_2022/trackers'

## source functions 
source(file.path(source_dir, 'get_location_metadata.R'))
source(file.path(source_dir, 'get_population.R'))
source(file.path(source_dir, "upload_bundle_data.R"))
source(file.path(source_dir, "get_bundle_data.R"))
source(file.path(source_dir, "save_bundle_version.R"))
source(file.path(source_dir, "get_bundle_version.R"))
source(file.path(source_dir, "save_crosswalk_version.R"))
source(file.path(source_dir, "get_crosswalk_version.R"))
source(file.path(source_dir, "get_covariate_estimates.R"))
source("/ihme/code/st_gpr/central/src/stgpr/api/public.R")

## get location information 
locations <- get_location_metadata(release_id = 9, location_set_id = 22)
locs <- locations[, c('location_id','ihme_loc_id'), with=F]

## get population information 
pop <- get_population(release_id = 9,
                      location_id = locs$location_id,
                      year_id = seq(1980, 2022), 
                      sex_id = c(1,2), 
                      age_group_id = seq(8,18))

## parameters
make_covs <- T

################################################################################
## MICRODATA ###################################################################
################################################################################
## read model inputs for final 
prep_micro <- function(file){
  ## read input (for some reason nas are reading in for NIDS? )
  df <- fread(paste0('/mnt/share/scratch/projects/hssa/occ/prepped/hrh/thesis/final/', file, '.csv'))[!is.na(nid)]

  ## make dataset wide 
  df_wide <- dcast(df[, c('nid', 'location_id', 'year_id', 'sex_id', 'val', 'file_path')], 
                   formula = nid + location_id + year_id + file_path ~ sex_id, value.var = c('val'))
  
  ## calculate aggregate sample size 
  agg_sample <- df[, .(sample_size = sum(sample_size)), by = c('nid', 'location_id', 'year_id', 'file_path')]
  df_wide <- merge(df_wide, agg_sample, by = c('nid', 'location_id', 'year_id', 'file_path'))
  
  ## replace NAs with 0s for sources with observed densities for one sex 
  df_wide[, `1` := ifelse(is.na(`1`), 0, `1`)][, `2` := ifelse(is.na(`2`), 0, `2`)]
  
  ## calculate share of female health workers 
  df_wide[, val := `2`/(`1`+`2`)]
  
  ## drop shares where there wasn't information from both sexes
  df_wide <- df_wide[val != 0 & val != 1, ]
  
  ## calculate variance with median offset among observations 
  df_wide[, variance := val*(1-val)/sample_size]
  df_wide[, variance := ifelse(val == 0, variance + median(variance,na.rm=T), 
                               ifelse(val == 1, variance + median(variance,na.rm=T),
                                      ifelse(is.na(variance), median(variance,na.rm=T),variance)))]
  
  df_wide <- df_wide[!is.na(val) & !is.na(variance)] ## these need to be investigated
  
  ## subset to columns for stgpr 
  df_wide[, age_group_id := 22][, measure_id := 18][, sex_id := 3][, is_outlier := 0]
  
  return(df_wide[, c('sample_size', 'age_group_id', 'measure_id', 'sex_id', 'nid', 'is_outlier', 'variance', 'val', 'year_id', 'location_id')])
}

## call for all final cadres 
hrh_phys <- prep_micro(file='hrh_phys_2')
hrh_any <- prep_micro(file='hrh_any_2')
hrh_nurseagg <- prep_micro(file='hrh_nurseagg_2')
# hrh_nurseprof <- prep_micro(file='hrh_nurseprof_2')
# hrh_clinic <- prep_micro(file='hrh_clinic_2')
# hrh_opt <- prep_micro(file='hrh_opt_2')
# hrh_dent <- prep_micro(file='hrh_dent_2')
# hrh_pharm <- prep_micro(file='hrh_pharm_2')
# hrh_diet <- prep_micro(file='hrh_diet_2')
# hrh_radio <- prep_micro(file='hrh_radio_2')
# hrh_pharmtech <- prep_micro(file='hrh_pharmtech_2')
# hrh_nurseass <- prep_micro(file='hrh_nurseass_2')
# hrh_midass <- prep_micro(file='hrh_midass_2')
# hrh_dentass <- prep_micro(file='hrh_dentass_2')
# hrh_pcare <- prep_micro(file='hrh_pcare_2')
# hrh_psych <- prep_micro(file='hrh_psych_2')
# hrh_therap <- prep_micro(file='hrh_therap_2')
# hrh_audio <- prep_micro(file='hrh_audio_2')
# hrh_trad <- prep_micro(file='hrh_trad_2')
# hrh_amb <- prep_micro(file='hrh_amb_2')
# hrh_envir <- prep_micro(file='hrh_envir_2')
# hrh_medtech <- prep_micro(file='hrh_medtech_2')

## save all the names
final_hrh <- c('hrh_phys','hrh_any','hrh_nurseagg', 'hrh_dent', 'hrh_pharm', 'hrh_nurseprof','hrh_clinic','hrh_opt','hrh_diet','hrh_radio','hrh_pharmtech','hrh_nurseass',
               'hrh_midass','hrh_dentass','hrh_pcare','hrh_psych','hrh_therap','hrh_audio',
               'hrh_trad','hrh_amb','hrh_envir','hrh_medtech')

################################################################################
## TABULATED ###################################################################
################################################################################
## read tabulated data (only available for physicians + nursing and midwifery personnel)
hrh_phys_tab <- fread('/mnt/share/scratch/projects/hssa/occ/by_sex/sources/raw/data/physicians_by_sex_06152023.csv')
hrh_nurseagg_tab <- fread('/mnt/share/scratch/projects/hssa/occ/by_sex/sources/raw/data/nurses_by_sex_06152023.csv')

## subset to share of female workers (duplicates present in raw data)
hrh_phys_tab <- hrh_phys_tab[Dim1 == 'Female']
hrh_nurseagg_tab <- hrh_nurseagg_tab[Dim1 == 'Female']

## subset to columns of interest 
hrh_phys_tab <- hrh_phys_tab[, c('SpatialDimValueCode','Location','Period','Value')]
hrh_nurseagg_tab <- hrh_nurseagg_tab[, c('SpatialDimValueCode','Location','Period','Value')]

## convert percentage to share 
hrh_phys_tab[, val := Value/100][, Value := NULL]
hrh_nurseagg_tab[, val := Value/100][, Value := NULL]

## attach location information 
hrh_phys_tab <- merge(hrh_phys_tab, locs[, c('ihme_loc_id', 'location_id')], by.x = 'SpatialDimValueCode', by.y = 'ihme_loc_id')
hrh_phys_tab[, SpatialDimValueCode := NULL][, Location := NULL]

hrh_nurseagg_tab <- merge(hrh_nurseagg_tab, locs[, c('ihme_loc_id', 'location_id')], by.x = 'SpatialDimValueCode', by.y = 'ihme_loc_id')
hrh_nurseagg_tab[, SpatialDimValueCode := NULL][, Location := NULL]

## add columsn for st-gpr 
setnames(hrh_phys_tab, old = c('Period'), new = c('year_id'))
hrh_phys_tab[, age_group_id := 22][, measure_id := 18][, sex_id := 3][, is_outlier := 0]
hrh_phys_tab[, sample_size := quantile(hrh_phys$sample_size, 0.05)] ## impute 5th percentile of sample size 
hrh_phys_tab[, nid := 346038] ## TODO: request a NID for these sources 
hrh_phys_tab[, variance := median(hrh_phys$variance)] ## impute median variance 

setnames(hrh_nurseagg_tab, old = c('Period'), new = c('year_id'))
hrh_nurseagg_tab[, age_group_id := 22][, measure_id := 18][, sex_id := 3][, is_outlier := 0]
hrh_nurseagg_tab[, sample_size := quantile(hrh_nurseagg$sample_size, 0.05)] ## impute 5th percentile of sample size 
hrh_nurseagg_tab[, nid := 346038] ## TODO: request a NID for these sources 
hrh_nurseagg_tab[, variance := median(hrh_nurseagg$variance)] ## impute median variance 

## combine data sources 
hrh_phys <- rbind(hrh_phys, 
                  hrh_phys_tab[, c('sample_size', 'age_group_id', 'measure_id', 'sex_id', 'nid', 'is_outlier', 'variance', 'val', 'year_id', 'location_id')])
hrh_nurseagg <- rbind(hrh_nurseagg, 
                  hrh_nurseagg_tab[, c('sample_size', 'age_group_id', 'measure_id', 'sex_id', 'nid', 'is_outlier', 'variance', 'val', 'year_id', 'location_id')])

################################################################################
## COVARIATES ##################################################################
################################################################################
if (make_covs == T){
  ## calculate population weights for aggregating age-specific models 
  pop_agg <- pop[, .(pop_all_ages = sum(population)), by = c('location_id','year_id','sex_id')]
  pop <- merge(pop, pop_agg)
  pop[, pop_weight := population/pop_all_ages]
  
  ## female education 
  ## TODO: will have to transition to age-specific version 
  fem_educ <- get_covariate_estimates(covariate_id=845,release_id=9)
  
  ## subset column names 
  fem_educ <- fem_educ[, c('location_id','year_id','age_group_id','sex_id','mean_value')]
  
  ## only pull female education 
  fem_educ <- fem_educ[sex_id == 2,]
  
  ## re=attach  st-gpr columns 
  fem_educ[, sex_id := 3][, cv_fem_educ := mean_value][, mean_value := NULL]
  
  ## female employment and female occupation 
  fem_emp <- get_estimates(202868, entity = "final")
  fem_occ <- get_estimates(149198, entity = "final")
  
  ## subset to females only 
  fem_emp <- fem_emp[sex_id == 2, ]
  fem_occ <- fem_occ[sex_id == 2, ]
  
  ## apply population weights
  fem_emp <- merge(fem_emp, pop[, c('location_id','year_id','sex_id','age_group_id','pop_weight')])
  fem_emp[, val := val * pop_weight]
  
  ## sum aggregate measures 
  fem_emp <- fem_emp[, .(cv_fem_emp = sum(val)), by = c('year_id', 'location_id')]
  
  ## re=attach st-gpr columsn 
  fem_emp[, age_group_id := 22][, sex_id := 3]
  fem_occ[, age_group_id := 22][, sex_id := 3]
  
  ## 
  fem_occ[, cv_fem_occ := val][, val := NULL][, lower := NULL][, upper := NULL]
  
  ## combine 
  covs <- merge(fem_educ, fem_emp)
  covs <- merge(covs, fem_occ)
  
  ## export covariates 
  write.csv(covs, '/mnt/share/scratch/projects/hssa/occ/prepped/occ/thesis/custom_covs/hrh_by_sex_custom_covs.csv', row.names = F)
}

################################################################################
## EXPORTS #####################################################################
################################################################################
export <- function(file, data){
  ## specify filename
  filepath <- paste0('/mnt/share/scratch/projects/hssa/occ/by_sex/sources/prepped/', file, '.csv')
  
  ## specify data
  df <- data 
  
  ## csv export 
  write.csv(df, filepath)
  
  ## prep data for bundle export 
  df[, underlying_nid := NA][, seq := 1:nrow(df)][, sex := "Both"][, measure := "continuous"][, me_name := str_remove(file, '_2')]
  
  ## export xlsx 
  write.xlsx(df, paste0('/mnt/share/scratch/projects/hssa/occ/by_sex/sources/prepped/', file, '.xlsx'), sheetName = 'extraction', rowNames = F)
}

## export for all final cadre models
export(file='hrh_phys_sex_2', data=hrh_phys)
export(file='hrh_any_sex_2', data=hrh_any)
export(file='hrh_nurseagg_sex_2', data=hrh_nurseagg)
# export(file='hrh_nurseprof_sex_2', data=hrh_nurseprof)
# export(file='hrh_clinic_sex_2', data=hrh_clinic)
# export(file='hrh_opt_sex_2', data=hrh_opt)
export(file='hrh_dent_sex_2', data=hrh_dent)
export(file='hrh_pharm_sex_2', data=hrh_pharm)
# export(file='hrh_diet_sex_2', data=hrh_diet)
# export(file='hrh_radio_sex_2', data=hrh_radio)
# export(file='hrh_pharmtech_sex_2', data=hrh_pharmtech)
# export(file='hrh_nurseass_sex_2', data=hrh_nurseass)
# export(file='hrh_midass_sex_2', data=hrh_midass)
# export(file='hrh_dentass_sex_2', data=hrh_dentass)
# export(file='hrh_pcare_sex_2', data=hrh_pcare)
# export(file='hrh_psych_sex_2', data=hrh_psych)
# export(file='hrh_therap_sex_2', data=hrh_therap)
# export(file='hrh_audio_sex_2', data=hrh_audio)
# export(file='hrh_trad_sex_2', data=hrh_trad)
# export(file='hrh_amb_sex_2', data=hrh_amb)
# export(file='hrh_envir_sex_2', data=hrh_envir)
# export(file='hrh_medtech_sex_2', data=hrh_medtech)

bundles <- fread('~/hrh_bundles.csv')
collapse.version <- 2
make_bundles <- T

if (make_bundles == T){
  for (categ in paste0(final_hrh, "_sex")[1:3]){
    
    print(categ)
    ## TODO: manually update description 
    description <- "Update inputs for sex models by cardre"
    
    ## directories 
    out.dir <- '/mnt/share/scratch/projects/hssa/occ/by_sex/sources/prepped/'
    
    ## input data for prelim models 
    filepath <- file.path(out.dir,paste0(categ,"_",collapse.version,".xlsx"))
    print(filepath)
    
    ## pull bundle id
    bundle_id <- as.integer(bundles[me_name == categ, "bundle_id"])
    
    ## upload to bundle
    bundle_result <- upload_bundle_data(
      bundle_id = bundle_id,
      filepath = filepath)
    
    ## save bundle version
    bundle_version_result <- save_bundle_version(
      bundle_id = bundle_id)
    
    # write out bundle version info
    bundle_version_tracker <- fread(paste0("/mnt/share/scratch/projects/hssa/occ/model/gbd_2022/trackers/bundle_version_tracker.csv"))
    bundle_version_tracker <- data.table(rbind(bundle_version_tracker, data.table(date = as.character(Sys.Date()), 
                                                                                  filepath = filepath,
                                                                                  bundle_id = bundle_id, 
                                                                                  bundle_version_id= bundle_version_result$bundle_version_id,
                                                                                  details = description)))
    fwrite(bundle_version_tracker, paste0("/mnt/share/scratch/projects/hssa/occ/model/gbd_2022/trackers/bundle_version_tracker.csv"))
    
    ## get bundle version data
    bundle_version <- data.table(get_bundle_version(bundle_version_result$bundle_version_id, fetch = "all"))
    nrow(bundle_version)
    
    ## add some necessary columns for crosswalk 
    bundle_version[, crosswalk_parent_seq:=NA]
    bundle_version[, unit_value_as_published:=1]
    
    ## save crosswalk data set
    crosswalk_filepath <- paste0("/mnt/share/scratch/projects/hssa/occ/prepped/occ/thesis/crosswalk/", categ , "_", Sys.Date(), ".xlsx")
    write.xlsx(bundle_version, crosswalk_filepath, sheetName = "extraction")
    
    ## save crosswalk version
    crosswalk_version_result <- save_crosswalk_version(
      bundle_version_id = bundle_version_result$bundle_version_id,
      data_filepath = crosswalk_filepath,
      description = description
    )
    print(crosswalk_version_result)
    
    ## check crosswalk upload
    crosswalk_version <- get_crosswalk_version(crosswalk_version_result$crosswalk_version_id)
    
    # write out crosswalk version info
    crosswalk_version_tracker <- fread(paste0("/mnt/share/scratch/projects/hssa/occ/model/gbd_2022/trackers/crosswalk_version_tracker.csv"))
    head(crosswalk_version_tracker)
    crosswalk_version_tracker <- data.table(rbind(crosswalk_version_tracker, data.table(date =as.character(Sys.Date()),
                                                                                        bundle_id = bundle_id,
                                                                                        bundle_version_id =bundle_version_result$bundle_version_id,
                                                                                        crosswalk_version_id = crosswalk_version_result$crosswalk_version_id,
                                                                                        crosswalk_filepath = crosswalk_filepath,
                                                                                        details = description)))
    write.csv(crosswalk_version_tracker, paste0("/mnt/share/scratch/projects/hssa/occ/model/gbd_2022/trackers/crosswalk_version_tracker.csv"), row.names = F)
  }
}
