############################################################################################################
## Name: USERNAME
## Purpose: Split out 3-digit HRH codes using intermediate model results
## R Version: FILENAME
## source("FILENAME",echo=T)
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

pacman::p_load(data.table,magrittr,ggplot2,parallel)

## settings
output.version <- 1
gbd_round <- 6

## in/out
root.dir <- "FILEPATH"
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"
split_mes <- fread("FILEPATH")

## sourcing
### clean workspace and load special functions
central_root <- "FILEPATH"
setwd(central_root)
source("FILEPATH")
source(file.path(k,"FILEPATH"))
source(file.path(j,"FILEPATH"))

locations <- get_location_metadata(gbd_round_id = gbd_round, location_set_id = 22)

## load a template of 3-digit surveys expected to be split 
total <- fread(file.path(root.dir,"clean_split_template.csv"))
metadata <- names(total)[!names(total) %in% c("var","mean","standard_error","design_effect","standard_deviation","standard_deviation_se","data","sample_size","variance","me_name","region_id","level","measure","missing_absent","missing_military")]
total <- total[,c(metadata,"sample_size"),with=F]

## determine suffix for files of interest
suffix <- paste0("_",output.version,".csv")

## list out files of relevant 3-digit estimates to be split
files <- list.files(in.dir)[grepl(paste0("[0-9]",suffix,"|nurse_mid_ass",suffix),list.files(in.dir))]


## loop through files of 3-digit codes to be split
for (file in files){
  ## read in 3-digit estimates
  raw_data <- fread(file.path(in.dir,file))

  ## compile list of underlying 4-digit categories
  mes <- unique(split_mes[envelope_code == gsub(suffix,"",file),me_name])

  ## find their run_ids
  mes <- get_model_documentation(as.list(mes))[,.(run_id = max(stgpr_version_id)),by=me_name]
  setnames(mes,"me_name","categ")

  ## read in mean estimates from st-gpr and merge onto 3-digit dataset
  for (me in mes$categ){
    x <- model_load(mes[categ == me,run_id],'raked')[,list(location_id,year_id,gpr_mean)]
    setnames(x,"gpr_mean",me)
    raw_data <- merge(raw_data,x,by=c("location_id","year_id"))
  }

  ## SPECIAL CASES: coding systems where the residual category differs from that of normal isco need to be adjusted
  if (grepl("isco_88_222",file)) raw_data[grepl("csco",tolower(occ_code_type)),c("hrh_phys","hrh_pharm") := NA]
  if (grepl("isco_88_322",file)) raw_data[grepl("csco|tha_ipums_1990",tolower(occ_code_type)),c("hrh_opt") := NA]
  if (grepl("isco_88_513",file)) raw_data[grepl("idn",tolower(occ_code_type)),c("isco_88_513_other") := NA]

  ## calculate proportions of 3-digit code made up by each 4-digit category
  raw_data[,total := rowSums(.SD,na.rm=T), .SDcols = mes$categ]
  raw_data[, (mes$categ) := lapply(.SD, function(x) {x/total}), .SDcols = mes$categ]

  ## split 3-digit codes into each underlying 4-digit category, append to the main cadre dataset, and write to folder for
  ## final st-gpr model. Also append split data to total dataset for summation into hrh_any for usable sources
  for (col in mes$categ[!grepl("other",mes$categ)]){ ## exclude residual 4-digit categories that are not of interest
    ## multiply 4-digit code proportion by 3-digit code prevalence to get split data, and prep for binding onto main dataset
    split <- copy(raw_data)
    split[,me_name := col]
    split[,c("mean","val") := lapply(.SD, function(x) {x*get(col)}), .SDcols = c("mean","val")]
    split[,variance := variance*(get(col)**2)]
    split[,c(mes$categ,"total") := NULL]

    ## add split data to main cadre-specific dataset
    df <- fread(file.path(in.dir,paste0(col,suffix)))
    df <- rbind(df,split[!is.na(val)])
    df <- df[order(ihme_loc_id,year_start)]

    ## write cadre-specific file to final modeling folder
    write.csv(df,file.path(out.dir,paste0(col,suffix)),row.names = F)

    ## Add cadre data to the total dataset to aggregate into hrh_any
    ## Possible that this cadre is already a column in the total dataset (ex from other coding version's split). If not, start by filling the
    ## cadre with estimates from any surveys for which the 3-digit code was sufficient to provide an estimate for the cadre
    if (!col %in% names(total)){
      mapped <- fread(file.path(in.dir,paste0(col,suffix)))
      total <- merge(total,mapped[,c(metadata,"val","variance"),with=F],by=c(metadata),all.x=T)
      setnames(total,c("val","variance"),c(col,paste0(col,"_var")))
    }

    ## merge split data onto total dataset
    total <- merge(total,split[,c(metadata,"val","variance"),with=F],by=c(metadata),all.x=T)

    ## if survey did not have an estimate for the cadre but the data was not split, it should mean we are
    ## working with a cleaned dataset in which the 3-digit code was dropped for equaling zero
    total[is.na(val),val := 0]
    total[is.na(get(col)) | get(col) == 0,c(col,paste0(col,"_var")) := list(val,variance)] ## fill in the estimates
    total[,c("val","variance") := list(NULL,NULL)]
  }
}

## also have to add hrh_nurseprof onto total dataset, which is directly mappable from 3-digits in both ISCO coding systems
col <- "hrh_nurseprof"
mapped <- fread(file.path(in.dir,paste0(col,suffix)))
total <- merge(total,mapped[,c(metadata,"val","variance"),with=F],by=c(metadata),all.x=T)
setnames(total,c("val","variance"),c(col,paste0(col,"_var")))
total[is.na(hrh_nurseprof),hrh_nurseprof := 0]

## list out all hrh cadres
hrh_categs <- c(split_mes[!grepl("other",me_name),unique(me_name)],"hrh_nurseprof")

## sum all categories to obtain estimate of hrh_any
total[,hrh_any := rowSums(.SD), .SDcols = hrh_categs]
total[,val := hrh_any/10000] ## convert estimate back to proportion
total[,variance := val*(1-val)/sample_size]

## read in existing hrh_any dataset and add summed split data to it
df <- fread(file.path(in.dir,paste0("hrh_any",suffix)))
df <- rbind(df,total,fill=T)
df[,c("me_name","var") := "hrh_any"]
df[,measure := "proportion"]

## check for odd results before writing to final prepped folder
df[is.na(val) | is.na(variance) | val == 0]

## for cases where variance is low, impute using formula and a very low data replacement
dmin <- df[val != 0,quantile(val,.05)]
small_var <- df[variance != 0,quantile(variance,.05)]
df[,impute_var := dmin*(1-dmin)/sample_size]
df[val == 0 & variance < small_var & impute_var > variance,variance := impute_var]
df[,impute_var := NULL]

## write output to final modeling folder
write.csv(df,file.path(out.dir,paste0("hrh_any",suffix)),row.names = F)


# OUTLIERING AND OUTPUT --------------------------------------------------------------

for (file in list.files(out.dir,full.names = T)) {
  df <- fread(file)

  ## Mark outliers
  df[,is_outlier := 0]
  source(file.path(j,"FILEPATH"))

  ##  write outputs
  write.csv(df,file,row.names = F)
}

