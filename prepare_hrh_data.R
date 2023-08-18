############################################################################################################
## Name: USERNAME
## Purpose: Prepare HRH collapse code outputs for preliminary cadre modeling
## R Version: FILEPATH
## source("FILEPATH",echo=T)
###########################################################################################################

## clear memory
rm(list=ls())

## runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
} else {
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
}

## libraries and settings
pacman::p_load(data.table,magrittr,ggplot2,parallel)
gbd_round <- 6

## load locations to merge on
source(file.path(k,"FILEPATH"))
locations <- get_location_metadata(gbd_round_id = gbd_round, location_set_id = 22)
locs <- locations[, c("location_id","ihme_loc_id"), with=F]

## in/out
collapse.version <- 1
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"
in.files <- list.files(in.dir,full.names = T)
in.files.sex <- in.files[grepl("hrh_",in.files) & grepl("_sex",in.files)]
in.files <- in.files[grepl("hrh_",in.files) & !grepl("_sex",in.files)]

## Sex-Aggregated Data
df <- rbindlist(lapply(in.files,fread),fill = T)
df <- unique(df[ihme_loc_id != ""])
df <- merge(df,locs,by="ihme_loc_id")

## check for duplicates
dups <- df[,.N,by=.(nid,ihme_loc_id,year_start,var)]
dups[N > 1,unique(nid)]


keepers <- df[nid %in% dups[N>1,nid],.(keep_ss = max(sample_size)),by=.(nid,ihme_loc_id,year_start)]
df <- merge(df,keepers,all.x=T,by=c("nid","ihme_loc_id","year_start"))
df <- df[!nid %in% dups[N>1,nid] | sample_size == keep_ss,]
df[,keep_ss := NULL]

## create standard ST-GPR variables
df[,data := mean]
df[,variance := standard_error**2]
df[,year_id := floor((year_start + year_end)/2)]
df[,age_group_id := 22]
df[,sex_id := 3]


df <- df[!grepl("ISSP",survey_name) | !grepl("_",ihme_loc_id)]
## remove small sample sizes for cadre-specific data
df <- df[var != "hrh_any" | sample_size >= 100]
df <- df[!nid %in% c(66763,111487)]


## where variance is non-existent or unexplainably low , impute it using sample size and
## the 2.5th percentile of data (whichever end is closer to the 0/1 border)
df[,impute_variance := data*(1-data)/sample_size]
df[data != 0 & !grepl("_",ihme_loc_id),minx := quantile(data,.025),by=var]
df[,minx := max(minx,na.rm=T),by=var]
df[data != 1  & !grepl("_",ihme_loc_id),miny := 1-quantile(data,.975),by=var]
df[,miny := max(miny,na.rm=T),by=var]
df[,minimum := ifelse(minx < miny,minx,miny)]
df[impute_variance == 0, impute_variance := minimum*(1-minimum)/sample_size]
df[!grepl("survey_adult_skills",tolower(survey_name)) & !grepl("_",ihme_loc_id) & variance != 0,low_var := quantile(variance,.025,na.rm=T),by=var]
df[,low_var := max(low_var,na.rm=T),by=var]
df[is.na(variance) | (variance < low_var & impute_variance > variance),variance := impute_variance]
df[,c("impute_variance","minx","miny","minimum","low_var") := NULL]

## write output for whole file for use in sourcing
write.csv(df,file.path(out.dir,"all_hrh_data.csv"),row.names=F)

## write raw cadre-specific files (pre-cleaning) for reference
for (categ in unique(df$var)){
  dt <- df[var == categ]
  dt[,me_name := categ]
  write.csv(dt,file.path(out.dir,"uncleaned",paste0(categ,"_",collapse.version,".csv")),row.names=F)
}


# CLEAN THE DATA ----------------------------------------------------------

df[,me_name := var]

## convert specific cadre data from proportions to # per 10,000 population
df[var != "hrh_any",c("data","variance","measure") := .(data*10000,variance*(10000**2),"continuous")]
df[var == "hrh_any",measure := "proportion"]

## At this point we want to create a template of survey metadata (to be used later)
## for specifically those surveys that:
##    1. are coded to coding systems for which we can identify (or practically identify)
##       every cadre that we model, and
##    2. require splitting out non-HRH residual codes before we can calculate an "hrh_any"
##       data point
## This template will be used during 3-digit splits to map on appropriate metadata and ensure
## that all underlying cadres were properly split before calculating additional data to add to
## the all-HRH envelope model
template <- df[var == "hrh_nurse_mid_ass" & (is.na(occ_length) | grepl("ISCO|TASCO|BOLCEOB_08",toupper(occ_code_type)))]
template[,me_name := var]
write.csv(template,file.path(out.dir,"clean_split_template.csv"),row.names=F)

df <- df[data != 0 | var == "hrh_any"]


# OUTLIERING AND OUTPUT --------------------------------------------------------------
df[,is_outlier := 0]
source(file.path(j,"FILEPATH"))


## write model-ready output files. Since nursing professionals are mappable at 3-digit level, this
## is the only cadre for which the data is already ready for the final model
for (categ in unique(df$var)){
  dt <- df[var == categ]
  setnames(dt,"data","val")
  write.csv(dt,file.path(out.dir,"intermediate",paste0(categ,"_",collapse.version,".csv")),row.names=F)
  if (categ == "hrh_nurseprof") write.csv(dt,file.path(out.dir,"final",paste0(categ,"_",collapse.version,".csv")),row.names=F)
}
