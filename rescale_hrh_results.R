############################################################################################################
## Name: USERNAME
## Purpose: Split out 3-digit HRH codes using intermediate model results
## R Version: singularity exec FILEPATH
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

pacman::p_load(data.table,magrittr,parallel,ggplot2)

## settings
output.version <- 1
suffix <- paste0("_",output.version,".csv")
gbd_round <- 6

## in/out
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"
summary.dir <- "FILEPATH"
draws.dir <- "FILEPATH"
model.dir <- "FILEPATH"

## sourcing
central_root <- "FILEPATH"
setwd(central_root)
source("FILEPATH")
source(file.path("FILEPATH"))
source(file.path("FILEPATH"))
locations <- get_location_metadata(gbd_round_id = gbd_round, location_set_id = 22)
locs <- locations[,list(location_id,ihme_loc_id)]

## create list of all files, modeled entities, and specific cadres
files <- list.files(file.path(in.dir,"final"))
categs <- gsub(suffix,"",files) ## all categories
cadres <- categs[!grepl("hrh_any",categs)] ## just the cadres (no hrh_any)

## find most recent run_id for each cadre (
mes <- get_model_documentation(as.list(categs))[,.(run_id = max(stgpr_version_id)),by=me_name]

## conversion factors for proportion employed to proportion of total pop
employment <- fread(file.path(in.dir,"prop_emp_oftotal.csv"))[,-c("age_group_id","sex_id"),with=F]



id.vars <- c("location_id","year_id","age_group_id","sex_id")
draw.cols <- paste0("draw_",seq(0,999))

## read in final hrh_any envelope
total <- rbindlist(lapply(list.files(file.path(model.dir,mes[me_name == "hrh_any",run_id],"draws_temp_0"),full.names = T),fread),use.names=T)
total <- melt(total,id.vars = id.vars,value.name = "hrh_any",variable.name = "draw")
total[,hrh_any := 10000*hrh_any]

## merge on cadre-specific estimates
for (cadre in cadres) {
  print(paste0("MERGING ON ",cadre))
  x <- rbindlist(lapply(list.files(file.path("FILEPATH",mes[me_name == cadre,run_id],"draws_temp_0"),full.names = T),fread),use.names=T)
  x <- melt(x,id.vars = id.vars,value.name = cadre,variable.name = "draw")
  total <- merge(total,x,by=c(id.vars,"draw"))
}

## generate sum of cadre-specific estimates and squeeze proportions to envelope
total[,cadre_total := rowSums(.SD), .SDcols = cadres]
total[, (cadres) := lapply(.SD,function(x) {x*hrh_any/cadre_total}),.SDcols = cadres]

## merge on ihme_loc_id
total <- merge(total,locs,by="location_id")
id.vars2 <- c(id.vars,"ihme_loc_id")

## apply employment ratios
employment <- melt(employment, id.vars = id.vars[1:2],value.name = "emp",variable.name = "draw")
total <- merge(total,employment,by=c(id.vars[1:2],"draw"))
total[, (categs) := lapply(.SD,function(x) {x*emp}),.SDcols = categs]

## create aggregate cadre groups of interest
total[,agg_nurses := hrh_nurseprof + hrh_nurseass + hrh_midass]
total[,agg_dent := hrh_dent + hrh_dentass]
total[,agg_pharm := hrh_pharm + hrh_pharmtech]
total[,agg_clinic := hrh_phys + hrh_clinic]

## write summary of results post-application of employment ratios
all_cat <- data.table()
for (categ in c(categs,"agg_nurses","agg_dent","agg_pharm","agg_clinic")) {
  print(paste0("COLLAPSING ",categ, ", POST-EMP"))
  post_emp <- total[,c(id.vars2,"draw",categ),with=F]
  post_emp[,c("lower","mean","upper") := list(quantile(get(categ),0.025),mean(get(categ)),quantile(get(categ),0.975)),by=c(id.vars2)]
  post_emp <- post_emp[,c(id.vars2,c("lower","mean","upper")),with=F] %>% unique()
  post_emp[,me_name := categ]
  all_cat <- rbind(all_cat,post_emp)
}
write.csv(all_cat,file.path(summary.dir,"all_categs.csv"),row.names=F)

## write out draws of final proportions of total population
for (categ in c(categs,"agg_nurses","agg_dent","agg_pharm","agg_clinic")){
  print(paste0("WRITING FINAL ",categ, " DRAWS TO FILE"))
  x <- total[,c(id.vars2,"draw",categ),with=F]
  x <- dcast(x, location_id + ihme_loc_id + year_id + age_group_id + sex_id ~ draw, value.var = categ)
  x[,me_name := categ]
  write.csv(x,file.path(draws.dir,paste0(categ,suffix)),row.names=F)
}
