############################################################################################################
## Name: USERNAME
## Purpose: Take output of employment ratio model and aggregate mean estimates for HRH conversions
## R Version: FILEPATH
## source("FILEPATHE",echo=T)
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

pacman::p_load(data.table,magrittr,parallel)

## most recent runs for occ_emp_ratio
run_ids <- c(54629,54758) # 43617
draw.cols <- paste0("draw_",seq(0,999))
id.vars <- c("location_id","year_id","sex_id","age_group_id")

## in/out
files <- list.files(file.path("FILEPATH",run_ids,"draws_temp_0"),full.names = T)
out_dir <- "FILEPATH"
outfile <- "FILEPATH"

## load and merge on  populations
source(file.path(k,"FILEPATH"))
pops <- data.table(get_population(decomp_step = "iterative",location_set_id = 22, year_id = seq(1980,2019), sex_id = seq(1,2), location_id = -1, age_group_id = seq(8,18)))
pops[,run_id := NULL]
poptotal <- data.table(get_population(decomp_step = "iterative",location_set_id = 22, year_id = seq(1980,2019), sex_id = 3, location_id = -1, age_group_id = 22))
setnames(poptotal,"population","poptotal")
poptotal <- poptotal[,list(location_id,year_id,poptotal)]
pops <- merge(pops,poptotal,by=id.vars[1:2])

## read in draws and collapse to just the mean
df <- rbindlist(lapply(files,fread),use.names=T)
df <- melt(df,id.vars = id.vars,value.name = "emp_prop",variable.name = "draw")
df <- merge(df, pops, by=id.vars)

## calculate employment proportions of total population, and write to file
dt <- df[,sum(emp_prop*population/poptotal),by=c(id.vars[1:2],"draw")]
dt <- dcast(dt,location_id + year_id ~ draw,value.var = "V1")
dt[,age_group_id := 22]
dt[,sex_id := 3]
write.csv(dt,file.path(out_dir,outfile),row.names=F)
