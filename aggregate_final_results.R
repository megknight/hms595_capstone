############################################################################################################
## Name: USERNAME
## Purpose: Prepare data for HRH Paper tables and figures
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

pacman::p_load(data.table,magrittr,ggplot2,parallel,boot,lme4,frontier,splines,mgcv,scales,feather,ggrepel,dplyr,openxlsx,devtools)
library(RColorBrewer, lib.loc = file.path(h,"FILEPATH"))
library(sfa, lib.loc = file.path(h,"FILEPATH"))
library(gam, lib.loc = file.path(h,"FILEPATH"))
source(file.path(k,"FILEPATH"))
source(file.path(k,"FILEPATH"))
source(file.path(k,"FILEPATH"))
source(file.path(k,"FILEPATH"))
source(file.path(j,"FILEPATH"))

## in/out
in.dir <- "FILEPATH"
out.dir <- "FILEPATH"
model.dir <- "FILEPATH"
generate_data <- F
generate_table_1 <- F
gbd_round <- 6

## identify main years and cadres of interest, and all cadre titles
main_years <- c(1990,2005,2019)
main_categs <- c("hrh_any","hrh_phys","agg_nurses","hrh_other")
titles <- fread(file.path(out.dir,"data/hrh_titles.csv"))
cadre_order <- c("hrh_phys","agg_nurses","agg_dent","agg_pharm","hrh_clinic","hrh_amb","hrh_therap",
              "hrh_radio","hrh_medtech","hrh_envir","hrh_opt","hrh_diet","hrh_audio","hrh_psych",
              "hrh_pcare","hrh_trad","hrh_any")

## load locs
locations <- get_location_metadata(gbd_round_id = gbd_round, location_set_id = 22)
locs <- copy(locations[level == 3, list(location_id,ihme_loc_id,location_name,super_region_name)])

# DATA PREP ---------------------------------------------------------------

## generate data that only requires mean estimates of HRH cadres
if (generate_data) {
  ## subset to only level 3 results
  df <- fread(file.path(in.dir,"all_categs.csv"))[!grepl("_",ihme_loc_id),list(ihme_loc_id,year_id,me_name,mean)]

  ## make other category from all cadres besides physicians and nurses/midwives
  other <- df[!me_name %in% c("hrh_any","hrh_phys","hrh_nurseprof","hrh_nurseass","hrh_midass","agg_dent","agg_pharm","agg_clinic","agg_nurses")]
  other <- other[,sum(mean),by=c("ihme_loc_id","year_id")]
  other[,me_name := "hrh_other"]
  setnames(other,"V1","mean")
  df <- rbind(df,other)

  ## make aggregate physicians, nurses, and midwives group
  physnurse <- df[me_name %in% c("hrh_phys","agg_nurses")]
  physnurse <- physnurse[,sum(mean),by=c("ihme_loc_id","year_id")]
  physnurse[,me_name := "phys_nurse"]
  setnames(physnurse,"V1","mean")
  df <- rbind(df,physnurse)

  ## merge on cadre titles
  df <- merge(df,titles,by="me_name")

  ## merge on location vars
  df <- merge(df,locs,by="ihme_loc_id")

  ## merge on populations
  pops <- get_population(age_group_id = 22, sex_id = 3,location_id = -1,year_id = seq(1980,2019),gbd_round_id = gbd_round,decomp_step = "iterative")[,list(location_id,year_id,population)]
  df <- merge(df,pops,by=c("location_id","year_id"))

  ## merge on SDI and SDI quintiles (based on 2019 values)
  sdi <- get_covariate_estimates(covariate_id = 881,gbd_round_id = gbd_round,decomp_step = "iterative")[location_id %in% locs$location_id]
  df <- merge(df,sdi[,list(location_id,year_id,sdi = mean_value)],by=c("location_id","year_id"))
  sdi <- sdi[year_id == 2019]
  sdi[,quintile := cut(sdi$mean_value,quantile(sdi$mean_value, probs=seq(0,1, by=0.2)),
                       labels = c("Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI"))]
  sdi[is.na(quintile),quintile := "Low SDI"]
  df <- merge(df,sdi[,list(location_id,sdi_quintile = quintile)],by="location_id")

  ## merge on HAQ and HAQ quintiles (based on that year's values)
  haq <- get_covariate_estimates(covariate_id = 1099,gbd_round_id = gbd_round,decomp_step = "iterative")[location_id %in% locs$location_id]
  df <- merge(df,haq[,list(location_id,year_id,haq = mean_value)],by=c("location_id","year_id"))
  for (year in seq(1980,2019)){
    haq_vals <- haq[year_id == year,mean_value]
    haq[year_id == year,haq := cut(haq_vals,quantile(haq_vals, probs=seq(0,1, by=0.2)),
                                   labels = c("Low HAQ","Low-middle HAQ","Middle HAQ","High-middle HAQ","High HAQ"))]
    haq[year_id == year & !grepl("HAQ",haq),haq := "Low HAQ"]
  }
  df <- merge(df,haq[,list(location_id,year_id,haq_quintile = haq)],by=c("location_id","year_id"))

  ## merge on log THE per capita and quintiles (using that year's values)
  log_the_pc <- get_covariate_estimates(covariate_id = 1984,gbd_round_id = gbd_round,decomp_step = "iterative")[location_id %in% locs$location_id]
  df <- merge(df,log_the_pc[,list(location_id,year_id,log_the_pc = mean_value/100)],by=c("location_id","year_id"),all.x=T)
  for (year in seq(1980,2019)){
    the_vals <- log_the_pc[year_id == year,mean_value]
    log_the_pc[year_id == year,the := cut(the_vals,quantile(the_vals, probs=seq(0,1, by=0.2)),
                                          labels = c("Low THE pc","Low-middle THE pc","Middle THE pc","High-middle THE pc","High THE pc"))]
    log_the_pc[year_id == year & !grepl("THE",the),the := "Low UHC"]
  }
  df <- merge(df,log_the_pc[,list(location_id,year_id,the_quintile = the)],by=c("location_id","year_id"),all.x=T)

  ## read in draws of UHC
  id.vars <- c("location_id","year_id")
  uhc <- as.data.table(read.xlsx(file.path(j,"FILEPATH")))[year_id > 1979 & level == 3,c(id.vars,paste0("draw_",seq(0,999))),with=F]
  uhc <- melt(uhc,id.vars=id.vars,value.name = "value")
  uhc[,c("uhc","uhc_variance") := .(mean(value),var(value)),by=id.vars]
  uhc <- uhc[,c(id.vars,"uhc","uhc_variance"),with=F] %>% unique
  df <- merge(df,uhc[,list(location_id,year_id,uhc,uhc_variance)],by=c("location_id","year_id"))

  ## generate UHC quintiles
  for (year in seq(1980,2019)){
    uhc_vals <- uhc[year_id == year,uhc]
    uhc[year_id == year,uhc_quint := cut(uhc_vals,quantile(uhc_vals, probs=seq(0,1, by=0.2)),
                                         labels = c("Low UHC","Low-middle UHC","Middle UHC","High-middle UHC","High UHC"))]
    uhc[year_id == year & !grepl("UHC",uhc_quint),uhc_quint := "Low UHC"]
  }
  df <- merge(df,uhc[,list(location_id,year_id,uhc_quintile = uhc_quint)],by=c("location_id","year_id"))

  ## output data in long format
  write.csv(df,file.path(out.dir,"data/cadre_data_long.csv"),row.names=F)

  ## output data in wide format
  df_wide <- dcast(...~me_name,value.var="mean",data=df[,-c("title","long_title"),with=F])
  write.csv(df_wide,file.path(out.dir,"data/cadre_data_wide.csv"),row.names=F)

  ## create frontier dataset (all in one) and data for table 2 (threshold values)
  front <- data.table()
  threshes <- data.table()
  for (cadre in c("hrh_phys","agg_nurses","agg_dent","agg_pharm")) {
    x <- fread(file.path(out.dir,"data",paste0(cadre,"_frontier_5trim_PUP.csv")))[,.(me_name = cadre,is_outlier = as.numeric(is_outlier),xvals,yvals,frontier)]
    front <- rbind(front,x)
    thresholds <- data.table(me_name = cadre, UHC80 = round(x[,approx(y=xvals,x=frontier,xout = 80)$y],1), UHC90 = round(x[,approx(y=xvals,x=frontier,xout = 90)$y],1))
    threshes <- rbind(threshes,thresholds)
  }
  front[,merge_val := round(xvals,8)]
  metadata <- df[me_name %in% front$me_name,.(location_id,year_id,ihme_loc_id,super_region_name,me_name,mean,uhc)]
  metadata[,merge_val := round(mean,8)]
  front <- merge(metadata,front,by=c("merge_val","me_name"),all=T)
  front[,c("merge_val","xvals","yvals") := NULL]
  front <- front[order(me_name,location_id,year_id)]
  write.csv(front,file.path(out.dir,"data/frontier_data_combined.csv"),row.names=F)
  write.csv(threshes,file.path(out.dir,"data/table2_data.csv"),row.names=F)

  ## generate data for table 3 (number of countries with shortages and sum of shortages by super-region)
  table3 <- merge(df[year_id == 2019,.(location_id,super_region_name,population,me_name,mean)],threshes[,.(me_name,UHC80)],by="me_name")
  glob <- copy(table3)
  glob[,super_region_name := "Global"]
  table3 <- rbind(table3,glob)[order(me_name)]
  table3[,shortage := ifelse(mean < UHC80,(UHC80-mean)*population/10000,0)]
  num_short <- table3[shortage > 0,.(num_short = .N),by=.(me_name,super_region_name)]
  table3 <- merge(table3,num_short,by=c("me_name","super_region_name"),all.x=T)
  table3[is.na(num_short),num_short := 0]
  table3[,total_num := .N,by=.(me_name,super_region_name)]
  table3[,perc_short := 100*round(num_short/total_num,3)]
  table3[,shortage_count := round(sum(shortage)),by=.(me_name,super_region_name)]
  table3 <- unique(table3[,.(me_name,super_region_name,num_short,perc_short,shortage_count)])
  table3[,c("me_name","super_region_name") := .(factor(me_name,levels=threshes$me_name),factor(super_region_name,levels=c("Global",sort(unique(locs$super_region_name)))))]
  table3 <- table3[order(me_name,super_region_name)]
  write.csv(table3,file.path(out.dir,"data/table3_data.csv"),row.names=F)

  ## generate data for table 2S (country-level densities and shortages)
  table2s <- df[year_id == 2019 & me_name %in% c("hrh_phys","agg_nurses","agg_dent","agg_pharm"),.(location_name,population,me_name,mean)]
  table2s <- merge(table2s,threshes[,.(me_name,UHC80)],by="me_name")
  table2s[,short_density := UHC80 - mean]
  table2s[,abs_short := ifelse(short_density > 0,short_density*population/10000,0)]
  table2s[,UHC80 := NULL]
  table2s[,me_name := factor(me_name,levels=threshes$me_name)]
  table2s <- table2s[order(me_name,location_name)]
  write.csv(table2s,file.path(out.dir,"data/table2S_data.csv"),row.names=F)

  ## generate data for table 3S (country-level SDI and UHC values)
  table3s <- merge(locations[,.(ihme_loc_id,lancet_label)],df_wide[year_id == 1990,.(ihme_loc_id,sdi_quintile,uhc_1990=round(uhc,1))],by="ihme_loc_id")
  table3s <- merge(table3s,df_wide[year_id == 2005,.(ihme_loc_id,uhc_2005=round(uhc,1))],by="ihme_loc_id")
  table3s <- merge(table3s,df_wide[year_id == 2019,.(ihme_loc_id,uhc_2019=round(uhc,1))],by="ihme_loc_id")
  table3s <- table3s[order(-uhc_2019,-uhc_2005,-uhc_1990),-c("ihme_loc_id")]
  write.csv(table3s,file.path(out.dir,"data/table3S_data.csv"),row.names=F)

  ## generate properly formatted results file
  results <- fread(file.path(in.dir,"all_categs.csv"))[!grepl("_",ihme_loc_id) & year_id > 1989 & me_name %in% cadre_order,
                                                       list(location_id,year_id,me_name,mean=round(mean,3),lower=round(lower,3),upper=round(upper,3))]
  results <- merge(results,titles[,.(me_name,long_title)],by="me_name")
  results[,me_name := factor(me_name,levels = cadre_order)]
  results <- merge(results,locs[,.(location_id,location_name)],by="location_id")
  results <- results[order(location_name,year_id,me_name),.(location_id,location_name,sex_id=3,sex="Both",age_group_id=22,age_name="All Ages",
                   year_id,cadre=long_title,measure="Health worker density",metric="Workers per 10,000 population",
                   mean,lower,upper)]
  write.csv(results,file.path(out.dir,"data/final_results_formatted.csv"),row.names=F)

  ## generate basic file of input data and list of unique input sources
  in.data <- fread("/ihme/scratch/projects/hssa/occ/prepped/hrh/gbd_2019_paper/all_hrh_data.csv")[nid != 272887,.(nid,survey_name,location_id,ihme_loc_id,year_start,year_end,year_id = floor((year_start+year_end)/2),sex_id,age_group_id,occ_code_type,occupational_code_type,occ_length,var,sample_size,data,standard_error,variance)]
  write.csv(in.data,file.path(out.dir,"data/input_data.csv"),row.names=F)
  sources <- unique(in.data[,.(nid,survey_name,ihme_loc_id = substr(ihme_loc_id,1,3),year_id,occ_code_type,occupational_code_type,occ_length)])
  sources[,coding_system := ifelse((grepl("isco",tolower(occ_code_type)) & grepl("08",occ_code_type)) | (grepl("isco",tolower(occupational_code_type)) & grepl("08",occupational_code_type)),"ISCO 08",
                                   ifelse((grepl("isco",tolower(occ_code_type)) & grepl("88",occ_code_type)) | (grepl("isco",tolower(occupational_code_type)) & grepl("88",occupational_code_type)),"ISCO 88","Country-Specific System"))]
  sources[is.na(occ_length),occ_length := 3]
  write.csv(sources,file.path(out.dir,"data/source_list.csv"),row.names=F)

  ## generate properly formatted input data sources file (have to impute standard_error)
  inputs <- in.data[,.(nid,underlying_nid="",component_id=1,location_id,covariate_id=1111,year_start,year_end,
                       sex_id,age_start=15,age_end=69,sample_size,sample_size_type="Persons",standard_error=sqrt(variance),
                       representative_id=1,urbanicity_type_id=1)]
  write.csv(inputs,file.path(out.dir,"data/input_sources_formatted.csv"),row.names=F)

} else {
  df <- fread(file.path(out.dir,"data/cadre_data_long.csv"))
  df_wide <- fread(file.path(out.dir,"data/cadre_data_wide.csv"))
  front <- fread(file.path(out.dir,"data/frontier_data_combined.csv"))
  threshes <- fread(file.path(out.dir,"data/table2_data.csv"))
  table3 <- fread(file.path(out.dir,"data/table3_data.csv"))
  in.data <- fread(file.path(out.dir,"data/input_data.csv"))
  sources <- fread(file.path(out.dir,"data/source_list.csv"))
  results <- fread(file.path(out.dir,"data/final_results_formatted.csv"))
  table2s <- fread(file.path(out.dir,"data/table2S_data.csv"))
  table3s <- fread(file.path(out.dir,"data/table3S_data.csv"))

}


## in order to generate data at super-regional and global levels, and to find uncertainty in
## differences, need to go to the draw files
if (generate_table_1) {
  ## read in and append all draws together
  files <- list.files(gsub("summaries","draws",in.dir),full.names=T)
  files <- files[!grepl("archive",files)]
  dt <- fread(files[1])[,-c("age_group_id","sex_id"),with=F]
  dt <- melt(dt[,-c("me_name"),with=F],id.vars=c("location_id","ihme_loc_id","year_id"),value.name = dt[,unique(me_name)])
  for (i in seq(2,length(files))) {
    x <- fread(files[i])[,-c("age_group_id","sex_id"),with=F]
    x <- melt(x[,-c("me_name"),with=F],id.vars=c("location_id","ihme_loc_id","year_id"),value.name = x[,unique(me_name)])
    dt <- merge(dt,x,by=names(dt)[1:4])
  }
  setnames(dt,"variable","draw")

  ## make additional combo cadres
  dt[,agg_emergence := hrh_amb + hrh_clinic]
  dt[,agg_tech := hrh_medtech + hrh_radio + hrh_therap]
  dt[,agg_residuals := hrh_audio + hrh_diet + hrh_envir + hrh_opt + hrh_pcare + hrh_psych + hrh_trad]
  dt[,phys_nurse := hrh_phys + agg_nurses]
  dt[,phys_nurse_dent_pharm := hrh_phys + agg_nurses + agg_dent + agg_pharm]
  dt[,other := hrh_any - phys_nurse]

  ## merge on region and super-region info
  locs3 <- copy(locations[level == 3, list(location_id,ihme_loc_id,location_name,region_name,super_region_name)])
  all_locs <- copy(locations[, list(location_id,ihme_loc_id,location_name,region_name,super_region_name)])
  dt <- merge(dt,locs3,by=c("location_id","ihme_loc_id"))
  dt <- dt[year_id > 1989]

  ## aggregate to region, super_region, and global level results
  supers <- copy(dt)
  pops <- get_population(age_group_id = 22, sex_id = 3,location_id = -1,year_id = seq(1990,2019),gbd_round_id = gbd_round,decomp_step = "iterative")[,list(location_id,year_id,population)]
  supers <- merge(supers,pops,by=c("location_id","year_id"),all.x=T)
  supers[,popreg := sum(population),by=.(region_name,year_id,draw)]
  supers[,popsuper := sum(population),by=.(super_region_name,year_id,draw)]
  supers[,popglobal := sum(population),by=.(year_id,draw)]
  aggregators <- names(supers)[grepl("^hrh|^agg|^phys|^other",names(supers))]
  supers[,paste0("region_",aggregators) := lapply(.SD,function(x) {x*population/popreg}),.SDcols = aggregators]
  supers[,paste0("super_",aggregators) := lapply(.SD,function(x) {x*population/popsuper}),.SDcols = aggregators]
  supers[,paste0("global_",aggregators) := lapply(.SD,function(x) {x*population/popglobal}),.SDcols = aggregators]
  for (col in aggregators) {
    print(col)
    supers[,(paste0("region_",col)) := sum(get(paste0("region_",col))),by=.(region_name,year_id,draw)]
    supers[,(paste0("super_",col)) := sum(get(paste0("super_",col))),by=.(super_region_name,year_id,draw)]
    supers[,(paste0("global_",col)) := sum(get(paste0("global_",col))),by=.(year_id,draw)]
  }
  globals <- unique(supers[,c("year_id","draw",names(supers)[grepl("^global_",names(supers))]),with=F])
  regions <- unique(supers[,c("year_id","draw",names(supers)[grepl("^region_",names(supers))]),with=F])
  supers <- unique(supers[,c("year_id","draw",names(supers)[grepl("^super_",names(supers))]),with=F])
  setnames(globals,names(globals)[grepl("^global_",names(globals))],gsub("global_","",names(globals)[grepl("^global_",names(globals))]))
  setnames(regions,names(regions)[grepl("^region_",names(regions))],gsub("region_","",names(regions)[grepl("^region_",names(regions))]))
  setnames(supers,names(supers)[grepl("^super_",names(supers))],gsub("super_","",names(supers)[grepl("^super_",names(supers))]))
  globals[,c("location_name","location_id","ihme_loc_id","super_region_name","region_name") := .("Global",1,"G","","")]
  setnames(supers,"region_name","super_region_name")
  supers[,location_name := super_region_name]
  supers <- merge(supers,all_locs[grepl("^S",ihme_loc_id)],by=c("location_name","super_region_name"))
  setnames(regions,"name","region_name")
  regions[,location_name := region_name]
  regions <- merge(regions,all_locs[grepl("^R",ihme_loc_id)],by=c("location_name","region_name"))
  dt <- rbind(dt,regions)
  dt <- rbind(dt,supers)
  dt <- rbind(dt,globals)

  ## calculate mean lower and upper of cadre densities, as well as of 1990-2019 annualized rates of change
  for (col in aggregators) {
    print(col)
    dt[,paste0(col,"_mean") := mean(get(col)),by=.(location_id,year_id)]
    dt[,paste0(col,"_lower") := quantile(get(col),.025),by=.(location_id,year_id)]
    dt[,paste0(col,"_upper") := quantile(get(col),.975),by=.(location_id,year_id)]

    ## calculate annualized rate of change
    roc <- unique(dt[year_id == 1990,c("location_id","draw",col),with=F])
    setnames(roc,col,"val_1990")
    roc <- merge(unique(dt[year_id == 2019,c("location_id","draw",col),with=F]),roc,by=c("location_id","draw"))
    setnames(roc,col,"val_2019")
    roc[,paste0(col,"_roc") := log(val_2019/val_1990)/(2019-1990)]
    roc[,paste0(col,"_roc_mean") := mean(get(paste0(col,"_roc"))),by=location_id]
    roc[,paste0(col,"_roc_lower") := quantile(get(paste0(col,"_roc")),.025),by=location_id]
    roc[,paste0(col,"_roc_upper") := quantile(get(paste0(col,"_roc")),.975),by=location_id]
    roc[,c("draw","val_1990","val_2019",paste0(col,"_roc")) := NULL]
    roc <- unique(roc)
    dt <- merge(dt,roc,by="location_id")
  }

  draft_wide <- unique(dt[draw == "draw_0",-c("draw",aggregators),with=F])
  draft <- melt(draft_wide,id.vars = c("location_id","ihme_loc_id","location_name","region_name","super_region_name","year_id"))
  draft[,me_name := gsub("_mean|_lower|_upper|_roc","",variable)]
  draft <- merge(draft,titles,by="me_name")
  setcolorder(draft,c("location_name","location_id","ihme_loc_id","region_name","super_region_name","year_id","me_name","title","long_title","variable"))
  draft[grepl("_roc_",variable),year_id := NA]
  draft <- unique(draft)

  ## output files with all data
  write.csv(draft,file.path(out.dir,"data/all_results_collapsed.csv"),row.names=F)

  ## subset to national, regional, super-regional, and global estimates for just 1990 and 2019 (and roc between)
  final <- draft[!grepl("_",ihme_loc_id) & year_id %in% c(1990,2019,NA),list(location_id,location_name,region_name,super_region_name,year_id,variable,value)]
  final <- dcast(final,...~variable + year_id,value.var = "value")
  setnames(final,names(final),gsub("_NA","",names(final)))
  final <- final[!(location_name == region_name & region_name == super_region_name)]
  final[,region_name := factor(region_name,levels = final[order(location_id),unique(region_name)])]

  ## create table 1 (1990, 2019, and rate of change values for physicians and nurses for every country,
  ## region, super-region, and global)
  cols <- CJ(c("hrh_phys_","agg_nurses_","other_"),c("","roc_"),c("mean_","lower_","upper_"),c("1990","2019"))
  cols[V2 == "roc_",c("V3","V4") := .(gsub("_","",V3),"")]
  cols <- unique(cols)
  cols[,c("V1","V3") := .(factor(V1,levels = c("hrh_phys_","agg_nurses_","other_")),factor(V3,levels = c("mean_","lower_","upper_","mean","lower","upper")))]
  table1 <- final[order(super_region_name,region_name,!location_name %in% final$region_name,location_name),c("location_name","region_name","super_region_name",cols[order(V1,V2,V4,V3),paste0(V1,V2,V3,V4)]),with=F]
  write.csv(table1,file.path(out.dir,"data/table1_data.csv"),row.names=F)

  ## create table 1S (2019 counts of HRH for every cadre by super-region and global)
  table1s <- draft[year_id == 2019 & ((location_name == super_region_name & location_name != region_name) | ihme_loc_id == "G") & me_name %in% cadre_order]
  table1s[,me_name := factor(me_name,levels = cadre_order)]
  table1s <- table1s[order(me_name)]
  table1s <- merge(table1s,pops,by=c("location_id","year_id"),all.x=T)
  table1s[,value := value*population/10000]
  write.csv(table1s,file.path(out.dir,"data/table1S_data.csv"),row.names=F)

} else {
  table1 <- fread(file.path(out.dir,"data/table1_data.csv"))
  table1s <- fread(file.path(out.dir,"data/table1S_data.csv"))

}


# VETTING VISUALIZATIONS -----------------------------------------------------

## Compare results to those of GBD 2017
old <- fread(file.path(out.dir,"gbd17/data/cadre_data_wide.csv"))
old[,c("agg_dent","agg_pharm") := .(hrh_dent + hrh_dentass,hrh_pharm + hrh_pharmtech)]
cadre_comps <- c("hrh_any","hrh_phys","agg_nurses","agg_dent","agg_pharm")
pdf(file.path(out.dir,"vetting/gbd17_comp.pdf"),width = 14,height = 7)
for (cadre in cadre_comps) {
  years <- c(2017)
  x <- merge(df_wide,old[,.(location_id,year_id,old = get(cadre))],by=c("location_id","year_id"))
  x <- x[year_id %in% years]
  x[,dif := get(cadre) - old]
  x[,ref := mean(get(cadre))]
  x[,lab := ifelse(abs(get(cadre) - old) > ref/3,ihme_loc_id,"")]

  gg <- ggplot(x,aes(y=get(cadre),x=old)) +
    geom_point(size = 3, shape = 21, color = "black",aes(fill=super_region_name)) +
    geom_abline(slope=1) +
    geom_text_repel(aes(label = lab),size=4,alpha=1, segment.size=0.3,force = 3) +
    theme_classic() +
    labs(x = "GBD 17 version", y = "GBD 19 version") +
    theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5),legend.title = element_blank()) +
    scale_y_continuous(expand = c(.05,0.05)) +
    scale_x_continuous(expand = c(.05,0.05)) +
    ggtitle(paste0(titles[me_name == cadre,long_title]," per 10k pop, in 2017"))
  print(gg)
}
dev.off()

## maps of main cadres in 1990 and 2019
for (cadre in c("hrh_any","hrh_phys","agg_nurses","agg_dent","agg_pharm")) {
  for (year in c(1990,2019)) {
    x <- df[year_id == year & me_name == cadre,list(ihme_loc_id,mapvar=mean)]

    ## set cadre-specific bins
    if (cadre == "hrh_any"){
      bins <- data.table(limits = c(20,35,50,75,100,150,200,275,350,max(x$mapvar)),
                         labels = c("<20","20-35","35-50","50-75","75-100","100-150","150-200","200-275","275-350",">350"))
    } else if (cadre == "hrh_phys") {
      bins <- data.table(limits = c(2,3,5,7,10,15,20,30,45,max(x$mapvar)),
                         labels = c("<2","2-3","3-5","5-7","7-10","10-15","15-20","20-30","30-45",">45"))
    } else if (cadre == "agg_nurses") {
      bins <- data.table(limits = c(8,10,15,20,30,45,60,80,100,max(x$mapvar)),
                         labels = c("<8","8-10","10-15","15-20","20-30","30-45","45-60","60-80","80-100",">100"))
    } else if (cadre == "agg_dent") {
      bins <- data.table(limits = c(.5,1,2,4,6,9,12,15,18,max(x$mapvar)),
                         labels = c("<0.5","0.5-1","1-2","2-4","4-6","6-9","9-12","12-15","15-18",">18"))
    } else if (cadre == "agg_pharm") {
      bins <- data.table(limits = c(.5,1,2,4,6,9,12,15,18,max(x$mapvar)),
                         labels = c("<0.5","0.5-1","1-2","2-4","4-6","6-9","9-12","12-15","15-18",">18"))
    }

    gbd_map(x, col.reverse = F, na.color = "#D3D3D3", col="Spectral",
            limits = c(0,bins$limits),
            labels = bins$labels,
            title = paste0(df[me_name == cadre,unique(title)]," per 10k population, ",year),
            fname = paste0(out.dir,"vetting/",cadre,"_map_",year,".pdf"))
  }
}


# MAIN TEXT and APPENDIX VISUALS -------------------------------------------------------

## Figure 1: IQR plots of cadre density by SDI quintile, over time
for (cadre in c("hrh_phys","agg_nurses")){
  x <- df[me_name == cadre & year_id %in% main_years,list(ihme_loc_id,sdi_quintile,year_id,mean)]
  x[,sdi_quintile := factor(sdi_quintile,levels = c("Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI"))]
  ## cleaned title variable to remove & sign for axis labels - MAK (09.16.2019)
  df[, title_c := gsub("&", "and", df[,title])]

  # added conditional logic for title allocation - MAK (09.12.2019)
  if (cadre == "hrh_phys"){
    title <- "Figure 1a. Interquartile range boxplots of physician densities by SDI \n in 1990, 2005, and 2019"
  } else if (cadre == "agg_nurses") {
    title <- "Figure 1b. Interquartile range boxplots of nurse and midwife densities by SDI \n in 1990, 2005, and 2019"
  }

  pdf(file.path(out.dir,"figures_tables",paste0("Fig1_",cadre,"_by_sdi_quint.pdf")),width = 14,height = 7)
  gg <- ggplot(x,aes(sdi_quintile,mean,fill=as.factor(year_id))) + geom_boxplot() +
    theme_classic() + scale_y_continuous(expand = c(0.01,0)) +
    labs(x = "\nSDI quintile", y= paste0(df[me_name == cadre,unique(title_c)]," per 10 000 population")) +
    theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5, face="bold"), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold")) +
    scale_fill_discrete(name = "Year") +
    ggtitle(paste0(title)) +
    theme(plot.title = element_text(face = "bold"))
  print(gg)
  dev.off()
}


## Figure 2 (and Appendix Figure 2): maps of physicians and nurses in 1990 and 2019
for (cadre in c("hrh_phys","agg_nurses")) {
  for (year in c(1990,2019)) {
    x <- df[year_id == year & me_name == cadre,list(ihme_loc_id,mapvar=mean)]

    ## set cadre-specific bins
    if (cadre == "hrh_any"){
      bins <- data.table(limits = c(20,35,50,75,100,150,200,275,350,max(x$mapvar)),
                         labels = c("<20","20-35","35-50","50-75","75-100","100-150","150-200","200-275","275-350",">350"))
    } else if (cadre == "hrh_phys") {
      bins <- data.table(limits = c(3,5,7,10,15,20,25,30,45,max(x$mapvar)),
                         labels = c("<3","3-5","5-7","7-10","10-15","15-20","20-25","25-30","30-45",">45"))
    } else if (cadre == "agg_nurses") {
      bins <- data.table(limits = c(10,15,20,30,40,50,65,85,120,max(x$mapvar)),
                         labels = c("<10","10-15","15-20","20-30","30-40","40-50","50-65","65-85","85-120",">120"))
    } else if (cadre == "hrh_pharm") {
      bins <- data.table(limits = c(.5,1,2,3,4,5,6,8,10,max(x$mapvar)),
                         labels = c("<0.5","0.5-1","1-2","2-3","3-4","4-5","5-6","6-8","8-10",">10"))
    } else if (cadre == "hrh_other") {
      bins <- data.table(limits = c(10,15,20,35,50,75,100,150,200,max(x$mapvar)),
                         labels = c("<10","10-15","15-20","20-35","35-50","50-75","75-100","100-150","150-200",">200"))
    } else if (cadre == "phys_nurse") {
      bins <- data.table(limits = c(10,15,20,30,50,75,100,125,150,max(x$mapvar)),
                         labels = c("<10","10-15","15-20","20-30","30-50","50-75","75-100","100-125","125-150",">150"))
    }

    # added conditional logic for title allocation 
    if (cadre == "hrh_phys" & year == 2019){
      title <- "Figure 2a. Density of physicians per 10 000 population by country and territory, 2019"
    } else if (cadre == "agg_nurses" & year == 2019) {
      title <- "Figure 2b. Density of nurses and midwives per 10 000 population by country and territory, 2019"
    } else if (cadre == "hrh_phys" & year == 1990) {
      title <- "Appendix figure 2a. Density of physicians per 10 000 population by country and territory, 1990"
    } else if (cadre == "agg_nurses" & year == 1990) {
      title <- "Appendix figure 2b. Density of nurses and midwives per 10 000 population by country and territory, 1990"
    }

    filename <- ifelse(year == 2019,paste0(out.dir,"/figures_tables/Fig2_",cadre,"_map_",year,".pdf"),
                       paste0(out.dir,"/figures_tables/ApxFig2_",cadre,"_map_",year,".pdf"))
    gbd_map(x, col.reverse = F, na.color = "#D3D3D3", col="Spectral",
            limits = c(0,bins$limits),
            labels = bins$labels,
            title = paste0(title),
            fname = filename)
  }
}


## Figure 3: frontier analysis
for (cadre in unique(front$me_name)) {
  print(cadre)
  year <- 2019
  title <- titles[me_name == cadre,gsub("&", "and", title)]
  x <- front[me_name == cadre & year_id == year]

  # added conditional logic for title allocation - MAK (09.12.2019)
  if (cadre == "hrh_phys"){
    fig_title <- "Figure 3a. Maximum possible UHC effective coverage index achievement given density of \nphysicians per 10 000 population in 2019"
  } else if (cadre == "agg_nurses") {
    fig_title <- "Figure 3b. Maximum possible UHC effective coverage index achievement given density of \nnurses and midwives per 10 000 population in 2019"
  } else if (cadre == "phys_nurse"){
    fig_title <- "Highest possible UHC achievement given physicians, nurses, and midwives per 10 000 population in 2019"
  } else if (cadre == "agg_dent"){
    fig_title <- "Figure 3c. Maximum possible UHC effective coverage index achievement given density of \ndentistry personnel per 10 000 population in 2019"
  } else if (cadre == "agg_pharm"){
    fig_title <- "Figure 3d. Maximum possible UHC effective coverage index achievement given density of \npharmaceutical personnel per 10 000 population in 2019"
  } else if (cadre == "hrh_any"){
    fig_title <- "Maximum possible UHC achievement given all health workers per 10 000 population in 2019"
  }

  pdf(file.path(out.dir,"figures_tables",paste0("Fig3_",cadre,"_frontier_",year,"_5trim_PUP.pdf")),width = 16,height = 10)
  gg <- ggplot(x,aes(x=mean,y=uhc)) +
    geom_point(size = 4,  aes(shape = as.factor(is_outlier),fill=(super_region_name))) +
    geom_line(aes(y=frontier),color="black",size = 1) +
    geom_text_repel(aes(label = ""),size=4,alpha=1, segment.size=0.3,force = 3) +
    theme_classic() +
    labs(x = paste0("\n",title," per 10 000 population"), y = "UHC effective coverage index", caption="\nEach point represents one out of 204 countries and territories. The line in the figure refers to the frontier, which is the maximum expected UHC attainment at a given health worker density. \nThe frontier was fit using all estimates from 1990-2019. This figure only shows the 2019 values for each location. GBD=Global Burden of Diseases, Injuries, and Risk Factors Study. \nUHC=universal health coverage.", shape="Estimate type") +
    theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(expand = c(.01,0.01),breaks=seq(20,90,10)) +
    scale_x_continuous(expand = c(.01,0.01)) +
    scale_fill_manual("GBD super-region",values = c(hue_pal()(7),"white")) +
    scale_color_manual("",values = c("black","red")) +
    scale_shape_manual(values=c(21,25), labels= c('Estimate', 'Trimmed estimate')) +
    guides(fill = guide_legend(override.aes=list(shape=21))) +
    ggtitle(paste0(fig_title)) +
    theme(plot.title = element_text(hjust = 0.5, face="bold"), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"), plot.caption=element_text(hjust = 0.0), legend.title.align = (hjust=0.0))
  print(gg)
  dev.off()
}


## Appendix Figure 1: Input data coverage map
x <- sources[,.(mapvar = .N),by=ihme_loc_id]


bins <- data.table(limits = c(2,5,10,20,max(x$mapvar)),labels = c("1","2-4","5-9","10-19","20+"))
title <- "Appendix Figure 1: Number of included occupation sources by country and territory"
filename <- paste0(out.dir,"/figures_tables/ApxFig1_data_input_map.pdf")
gbd_map(x, col.reverse = F, na.color = "#D3D3D3", col="Spectral",
        limits = c(0,bins$limits),
        labels = bins$labels,
        title = paste0(title),
        fname = filename)


# ADDITIONAL CALCULATIONS FOR RESULTS SECTION ---------------------------------------------
## Number of total sources
sources[,.N]
## Number of total sources by different metrics
sources[,.N,by=grepl("census",tolower(survey_name))]
sources[,.N,by=.(coding_system,occ_length)]
sources[,length(unique(ihme_loc_id))]

## Global stocks of HRH (in millions)
table1s[location_id == 1 & grepl("hrh_any",me_name),.(variable,round(value/1000000,1))]
table1s[location_id == 1 & grepl("hrh_phys",me_name),.(variable,round(value/1000000,1))]
table1s[location_id == 1 & grepl("agg_nurses",me_name),.(variable,round(value/1000000,1))]
## Global densities of HRH
table1s[location_id == 1 & grepl("hrh_phys",me_name),.(variable,round(value*10000/population,1))]
table1s[location_id == 1 & grepl("agg_nurses",me_name),.(variable,round(value*10000/population,1))]
## Super regions with lowest HRH densities
table1s[grepl("hrh_any_mean",variable),.(location_name,variable,round(value*10000/population,1))] %>% .[order(V3)]
table1s[grepl("hrh_phys_mean",variable),.(location_name,variable,round(value*10000/population,1))] %>% .[order(V3)]
table1s[grepl("agg_nurses_mean",variable),.(location_name,variable,round(value*10000/population,1))] %>% .[order(V3)]
table1s[grepl("agg_dent_mean",variable),.(location_name,variable,round(value*10000/population,1))] %>% .[order(V3)]
table1s[grepl("agg_pharm_mean",variable),.(location_name,variable,round(value*10000/population,1))] %>% .[order(V3)]
## Sum of national shortages
table3[super_region_name == "Global",.(me_name,round(shortage_count/1000000,1))]

## Total number of countries
df[,length(unique(location_id))]
## Number of countries covered by sources
sources[,length(unique(ihme_loc_id))]
## Percent of global population for which we have any national-level data
100*df_wide[year_id == 2019 & ihme_loc_id %in% sources$ihme_loc_id,sum(population)]/table1s[location_id == 1,unique(population)]
## Number of countries with shortage in at least one cadre
204 - df_wide[year_id == 2019 & hrh_phys >= threshes[me_name == "hrh_phys",UHC80] &
                agg_nurses >= threshes[me_name == "agg_nurses",UHC80] &
                agg_dent >= threshes[me_name == "agg_dent",UHC80] &
                agg_pharm >= threshes[me_name == "agg_pharm",UHC80],.N]
## locations with sources by super-region
df_wide[year_id == 2019 & ihme_loc_id %in% sources$ihme_loc_id,.N,by=super_region_name]
df_wide[year_id == 2019,.N,by=super_region_name]

## X-fold differences between highest and lowest SDI quintiles for physicians and nurses
df_wide[year_id == 2019 & grepl("high sdi",tolower(sdi_quintile)),median(hrh_phys)]/df_wide[year_id == 2019 & grepl("low sdi",tolower(sdi_quintile)),median(hrh_phys)]
df_wide[year_id == 2019 & grepl("high sdi",tolower(sdi_quintile)),median(agg_nurses)]/df_wide[year_id == 2019 & grepl("low sdi",tolower(sdi_quintile)),median(agg_nurses)]

## Range across super-regions and within super-regions for physicians
## super-regional country results
table1[location_name == "Global",.(location_name,mean=round(hrh_phys_mean_2019,1),round(hrh_phys_lower_2019,1),round(hrh_phys_upper_2019,1),roc=round(hrh_phys_roc_mean*100,1),round(hrh_phys_roc_lower*100,1),round(hrh_phys_roc_upper*100,1))]
table1[location_name == super_region_name,.(location_name,mean=round(hrh_phys_mean_2019,1),round(hrh_phys_lower_2019,1),round(hrh_phys_upper_2019,1))] %>% .[order(mean)]
table1[location_name == super_region_name,.(location_name,roc=round(hrh_phys_roc_mean*100,1),round(hrh_phys_roc_lower*100,1),round(hrh_phys_roc_upper*100,1))] %>% .[order(roc)]
table1[location_name == super_region_name,.(location_name,round(hrh_phys_mean_2019-hrh_phys_mean_1990,1))] %>% .[order(V2)]
table1[hrh_phys_mean_2019 %in% table1[,max(hrh_phys_mean_2019),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,mean = round(hrh_phys_mean_2019,1),round(hrh_phys_lower_2019,1),round(hrh_phys_upper_2019,1))][order(mean)]
table1[hrh_phys_mean_2019 %in% table1[,min(hrh_phys_mean_2019),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,mean = round(hrh_phys_mean_2019,1),round(hrh_phys_lower_2019,1),round(hrh_phys_upper_2019,1))][order(mean)]
table1[hrh_phys_roc_mean %in% table1[,max(hrh_phys_roc_mean),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,roc = round(hrh_phys_roc_mean*100,1),round(hrh_phys_roc_lower*100,1),round(hrh_phys_roc_upper*100,1))][order(roc)]
table1[(hrh_phys_mean_2019 - hrh_phys_mean_1990) %in% table1[,max(hrh_phys_mean_2019 - hrh_phys_mean_1990),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,diff = round(hrh_phys_mean_2019 - hrh_phys_mean_1990,1))][order(diff)]
table1[hrh_phys_roc_mean %in% table1[,min(hrh_phys_roc_mean),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,roc = round(hrh_phys_roc_mean*100,1),round(hrh_phys_roc_lower*100,1),round(hrh_phys_roc_upper*100,1))][order(roc)]
table1[(hrh_phys_mean_2019 - hrh_phys_mean_1990) %in% table1[,min(hrh_phys_mean_2019 - hrh_phys_mean_1990),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,diff = round(hrh_phys_mean_2019 - hrh_phys_mean_1990,1))][order(diff)]

table1[location_name == "Global",.(location_name,mean=round(hrh_phys_mean_2019,1),round(hrh_phys_lower_2019,1),round(hrh_phys_upper_2019,1),roc=round(hrh_phys_roc_mean*100,1),round(hrh_phys_roc_lower*100,1),round(hrh_phys_roc_upper*100,1))]
table1[location_name == super_region_name,.(location_name,mean=round(hrh_phys_mean_2019,1),round(hrh_phys_lower_2019,1),round(hrh_phys_upper_2019,1))] %>% .[order(mean)]
## Regional disparities
table1[location_name %in% c(region_name,super_region_name),.(location_name,round(hrh_phys_mean_2019,1),round(hrh_phys_lower_2019,1),round(hrh_phys_upper_2019,1),as.numeric(location_name == super_region_name))]
extremes <- table1[location_name == region_name,.(max(hrh_phys_mean_2019),min(hrh_phys_mean_2019)),by=super_region_name]
extremes[,dif := V1 - V2]
extremes[,rel_dif := V1/V2]
extremes[order(rel_dif)]
extremes[order(dif)]
table1[hrh_phys_mean_2019 %in% c(extremes$V1,extremes$V2),.(location_name,hrh_phys_mean_2019)]
table1[hrh_phys_mean_2019 %in% c(extremes[order(rel_dif)][nrow(extremes),c(V1,V2)],extremes[order(dif)][nrow(extremes),c(V1,V2)]),.(location_name,hrh_phys_mean_2019,hrh_phys_lower_2019,hrh_phys_upper_2019)]
## Country-level disparities
extremes <- table1[super_region_name != "",.(max(hrh_phys_mean_2019),min(hrh_phys_mean_2019)),by=super_region_name]
extremes[,dif := V1 - V2]
extremes[,rel_dif := V1/V2]
extremes[order(rel_dif)]
extremes[order(dif)]
table1[hrh_phys_mean_2019 %in% c(extremes$V1,extremes$V2),.(location_name,hrh_phys_mean_2019)]
table1[hrh_phys_mean_2019 %in% c(extremes[order(rel_dif)][nrow(extremes),c(V1,V2)],extremes[order(dif)][nrow(extremes),c(V1,V2)]),.(location_name,hrh_phys_mean_2019,hrh_phys_lower_2019,hrh_phys_upper_2019)]
## Rates of change
table1[location_name == "Global",.(location_name,mean=round(hrh_phys_mean_2019,1),round(hrh_phys_lower_2019,1),round(hrh_phys_upper_2019,1),roc=round(hrh_phys_roc_mean*100,1),round(hrh_phys_roc_lower*100,1),round(hrh_phys_roc_upper*100,1))]
table1[location_name == super_region_name,.(location_name,roc=round(hrh_phys_roc_mean*100,1),round(hrh_phys_roc_lower*100,1),round(hrh_phys_roc_upper*100,1))] %>% .[order(roc)]
table1[location_name %in% c(region_name,super_region_name),.(location_name,round(hrh_phys_roc_mean*100,1),as.numeric(location_name == super_region_name))]
extremes <- table1[location_name == region_name,.(max(hrh_phys_roc_mean),min(hrh_phys_roc_mean)),by=super_region_name]
extremes[,dif := 100*(V1 - V2)]
extremes[,rel_dif := V1/V2]
extremes[order(rel_dif)]
extremes[order(dif)]
table1[hrh_phys_roc_mean %in% c(extremes$V1,extremes$V2),.(location_name,hrh_phys_roc_mean*100)]
table1[hrh_phys_roc_mean %in% c(extremes[order(rel_dif)][nrow(extremes),c(V1,V2)],extremes[order(dif)][nrow(extremes),c(V1,V2)]),.(location_name,hrh_phys_roc_mean*100,hrh_phys_roc_lower*100,hrh_phys_roc_upper*100)]

## Range across super-regions and within super-regions for nurses
table1[location_name == "Global",.(location_name,mean=round(agg_nurses_mean_2019,1),round(agg_nurses_lower_2019,1),round(agg_nurses_upper_2019,1),roc=round(agg_nurses_roc_mean*100,1),round(agg_nurses_roc_lower*100,1),round(agg_nurses_roc_upper*100,1))]
table1[location_name == super_region_name,.(location_name,mean=round(agg_nurses_mean_2019,1),round(agg_nurses_lower_2019,1),round(agg_nurses_upper_2019,1))] %>% .[order(mean)]
table1[location_name == super_region_name,.(location_name,roc=round(agg_nurses_roc_mean*100,1),round(agg_nurses_roc_lower*100,1),round(agg_nurses_roc_upper*100,1))] %>% .[order(roc)]
table1[location_name == super_region_name,.(location_name,round(agg_nurses_mean_2019-agg_nurses_mean_1990,1))] %>% .[order(V2)]
table1[agg_nurses_mean_2019 %in% table1[,max(agg_nurses_mean_2019),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,mean = round(agg_nurses_mean_2019,1),round(agg_nurses_lower_2019,1),round(agg_nurses_upper_2019,1))][order(mean)]
table1[agg_nurses_mean_2019 %in% table1[,min(agg_nurses_mean_2019),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,mean = round(agg_nurses_mean_2019,1),round(agg_nurses_lower_2019,1),round(agg_nurses_upper_2019,1))][order(mean)]
table1[agg_nurses_roc_mean %in% table1[,max(agg_nurses_roc_mean),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,roc = round(agg_nurses_roc_mean*100,1),round(agg_nurses_roc_lower*100,1),round(agg_nurses_roc_upper*100,1))][order(roc)]
table1[(agg_nurses_mean_2019 - agg_nurses_mean_1990) %in% table1[,max(agg_nurses_mean_2019 - agg_nurses_mean_1990),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,diff = round(agg_nurses_mean_2019 - agg_nurses_mean_1990,1))][order(diff)]
table1[agg_nurses_roc_mean %in% table1[,min(agg_nurses_roc_mean),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,roc = round(agg_nurses_roc_mean*100,1),round(agg_nurses_roc_lower*100,1),round(agg_nurses_roc_upper*100,1))][order(roc)]
table1[(agg_nurses_mean_2019 - agg_nurses_mean_1990) %in% table1[,min(agg_nurses_mean_2019 - agg_nurses_mean_1990),by=super_region_name]$V1 & location_name != "Global",.(super_region_name,location_name,diff = round(agg_nurses_mean_2019 - agg_nurses_mean_1990,1))][order(diff)]

table1[location_name == "Global",.(location_name,mean=round(agg_nurses_mean_2019,1),round(agg_nurses_lower_2019,1),round(agg_nurses_upper_2019,1),roc=round(agg_nurses_roc_mean*100,1),round(agg_nurses_roc_lower*100,1),round(agg_nurses_roc_upper*100,1))]
table1[location_name == super_region_name,.(location_name,mean=round(agg_nurses_mean_2019,1),round(agg_nurses_lower_2019,1),round(agg_nurses_upper_2019,1))] %>% .[order(mean)]
## Regional disparities
table1[location_name %in% c(region_name,super_region_name),.(location_name,round(agg_nurses_mean_2019,1),as.numeric(location_name == super_region_name))]
extremes <- table1[location_name == region_name,.(max(agg_nurses_mean_2019),min(agg_nurses_mean_2019)),by=super_region_name]
extremes[,dif := V1 - V2]
extremes[,rel_dif := V1/V2]
extremes[order(rel_dif)]
extremes[order(dif)]
table1[agg_nurses_mean_2019 %in% c(extremes$V1,extremes$V2),.(location_name,agg_nurses_mean_2019)]
table1[agg_nurses_mean_2019 %in% c(extremes[order(rel_dif)][nrow(extremes),c(V1,V2)],extremes[order(dif)][nrow(extremes),c(V1,V2)]),.(location_name,agg_nurses_mean_2019,agg_nurses_lower_2019,agg_nurses_upper_2019)]
## Country-level disparities
extremes <- table1[super_region_name != "",.(max(agg_nurses_mean_2019),min(agg_nurses_mean_2019)),by=super_region_name]
extremes[,dif := V1 - V2]
extremes[,rel_dif := V1/V2]
extremes[order(rel_dif)]
extremes[order(dif)]
table1[agg_nurses_mean_2019 %in% c(extremes$V1,extremes$V2),.(location_name,agg_nurses_mean_2019)]
table1[agg_nurses_mean_2019 %in% c(extremes[order(rel_dif)][nrow(extremes),c(V1,V2)],extremes[order(dif)][nrow(extremes),c(V1,V2)]),.(location_name,agg_nurses_mean_2019,agg_nurses_lower_2019,agg_nurses_upper_2019)]
## rates of change
table1[location_name == "Global",.(location_name,mean=round(agg_nurses_mean_2019,1),round(agg_nurses_lower_2019,1),round(agg_nurses_upper_2019,1),roc=round(agg_nurses_roc_mean*100,1),round(agg_nurses_roc_lower*100,1),round(agg_nurses_roc_upper*100,1))]
table1[location_name == super_region_name,.(location_name,roc=round(agg_nurses_roc_mean*100,1),round(agg_nurses_roc_lower*100,1),round(agg_nurses_roc_upper*100,1))] %>% .[order(roc)]
table1[location_name %in% c(region_name,super_region_name),.(location_name,round(agg_nurses_roc_mean*100,1),as.numeric(location_name == super_region_name))]
extremes <- table1[location_name == region_name,.(max(agg_nurses_roc_mean),min(agg_nurses_roc_mean)),by=super_region_name]
extremes[,dif := 100*(V1 - V2)]
extremes[,rel_dif := V1/V2]
extremes[order(rel_dif)]
extremes[order(dif)]
table1[agg_nurses_roc_mean %in% c(extremes$V1,extremes$V2),.(location_name,agg_nurses_roc_mean*100)]
table1[agg_nurses_roc_mean %in% c(extremes[order(rel_dif)][nrow(extremes),c(V1,V2)],extremes[order(dif)][nrow(extremes),c(V1,V2)]),.(location_name,agg_nurses_roc_mean*100,agg_nurses_roc_lower*100,agg_nurses_roc_upper*100)]


## shortages globally and by super-region
table3[super_region_name == "Global",.(me_name,num_short,round(shortage_count/1000000,1))]
table3[super_region_name != "Global",.(super_region_name,me_name,round(shortage_count/1000000,1))]

## Combined physician and nursing thresholds and countries already achieving it
threshes[me_name %in% c("hrh_phys","agg_nurses"),sum(UHC80)]
df_wide[year_id == 2019 & hrh_phys + agg_nurses >= threshes[me_name %in% c("hrh_phys","agg_nurses"),sum(UHC80)],.N]
## Sum of shortages across all cadres globally
table3[super_region_name == "Global",round(sum(shortage_count)/1000000,1)]

## percentage data coverage by SDI quintiles
df_wide[year_id == 2019,sum(ihme_loc_id %in% sources$ihme_loc_id)*100/.N,by=sdi_quintile]
## percentage data coverage by population quintiles
x <- df_wide[year_id == 2019]
x[,pop_quintile := cut(population,quantile(population, probs=seq(0,1, by=0.2)),
                     labels = c("Low pop","Low-middle pop","Middle pop","High-middle pop","High pop"))]
x[is.na(pop_quintile),pop_quintile := "Low pop"]
x[,sum(ihme_loc_id %in% sources$ihme_loc_id)*100/.N,by=pop_quintile]
## table of new sources since 2017 analysis
old_sources <- fread(file.path(out.dir,"data/2017_data.csv"))[!grepl("_",ihme_loc_id),.(ihme_loc_id,location_id,year_id,nid,survey_name,occ_code_type,occ_length)] %>% unique()
## identify sources with mislabeled nid
old_sources[nid %in% (old_sources[,.N,by=nid] %>% .[N > 1,nid])] %>% .[order(nid)]
old_sources <- old_sources[!nid %in% c(313090) & (!nid %in% c(151797,151802) | occ_code_type == "ISCO 88")]
old_sources <- old_sources[!(nid == 313623 & ihme_loc_id == "CYP")]
old_sources <- old_sources[!(nid == 313635 & ihme_loc_id == "DOM")]
old_sources <- old_sources[!(nid == 313648 & ihme_loc_id == "BEL")]
old_sources <- old_sources[!(nid == 313648 & ihme_loc_id == "MEX")]
old_sources <- old_sources[!(nid == 313840 & ihme_loc_id == "ESP")]
old_sources[nid == 312148 & ihme_loc_id == "DEU", nid := 396943]
old_sources[nid == 312148 & ihme_loc_id == "HUN", nid := 396942]
old_sources[nid == 312208 & ihme_loc_id == "HUN", nid := 396868]
old_sources[nid == 312208 & ihme_loc_id == "NZL", nid := 396867]
old_sources[nid == 312211 & ihme_loc_id == "CHE", nid := 396950]
old_sources[nid == 312211 & ihme_loc_id == "HUN", nid := 396951]
old_sources[nid == 312526 & ihme_loc_id == "NZL", nid := 421342]
old_sources[nid == 313084 & ihme_loc_id == "CAN", nid := 396931]
old_sources[nid == 313084 & ihme_loc_id == "DEU", nid := 396941]
old_sources[nid == 313084 & ihme_loc_id == "POL", nid := 396940]
old_sources[nid == 313108 & ihme_loc_id == "POL", nid := 421344]
old_sources[nid == 313153 & ihme_loc_id == "DEU", nid := 421112]
old_sources[nid == 313281 & ihme_loc_id == "NZL", nid := 421177]
old_sources[nid == 313357 & ihme_loc_id == "NZL", nid := 396947]
old_sources[nid == 313503 & ihme_loc_id == "AUT", nid := 396953]
old_sources[nid == 313503 & ihme_loc_id == "HUN", nid := 396952]
old_sources[nid == 272887 & ihme_loc_id == "SVN", nid := 421351]
old_sources[nid == 313648 & ihme_loc_id == "HUN", nid := 396954]
old_sources[nid == 313648 & ihme_loc_id == "NZL", nid := 397115]
old_sources[nid == 313765 & ihme_loc_id == "BEL", nid := 421110]
old_sources[nid == 313765 & ihme_loc_id == "ESP", nid := 313745]
old_sources[nid == 313765 & ihme_loc_id == "NZL", nid := 421176]
old_sources[nid == 313840 & ihme_loc_id == "BEL", nid := 421106]
old_sources[nid == 313840 & ihme_loc_id == "NZL", nid := 421156]
old_sources[nid == 322395 & ihme_loc_id == "SVK", nid := 322367]
old_sources[nid == 341987 & ihme_loc_id == "CYP", nid := 341997]
old_sources[nid == 343099 & ihme_loc_id == "PRT", nid := 421073]
old_sources <- old_sources[,.(old_nid = nid,ihme_loc_id,year_id,survey_name)] %>% unique()
## identify those sources that were dropped or added (and not just recoded)
overlap <- merge(sources,old_sources,by=c("ihme_loc_id","year_id","survey_name"),all=T)
View(overlap[is.na(nid)] %>% .[order(survey_name,ihme_loc_id,year_id)])
View(overlap[is.na(old_nid)] %>% .[order(survey_name,ihme_loc_id,year_id)])
dropped_data <- overlap[is.na(nid),.(survey_name,ihme_loc_id,year_id)] %>% .[order(survey_name,ihme_loc_id,year_id)]
dropped_data[grepl("ISSP",survey_name),reason := "Duplicative with existing input data"]
dropped_data[grepl("WHO",survey_name),reason := "Substantial outliers in nearly every HRH cadre, considered unreliable"]
dropped_data[grepl("MACRO",survey_name),reason := "Upon detailed review, determined to not meet inclusion criteria"]
dropped_data[is.na(reason),reason := "Upon detailed review, notable issues found in mapping to ISCO-88 system"]
dropped_data <- merge(dropped_data,locations[,.(location_name,ihme_loc_id)],by="ihme_loc_id")
new_data <- overlap[is.na(old_nid),.(survey_name,ihme_loc_id,year_id)] %>% .[order(survey_name,ihme_loc_id,year_id)]
new_data <- merge(new_data,df_wide[year_id == 2019,.(ihme_loc_id,super_region_name,sdi_quintile)],by="ihme_loc_id")
new_data <- merge(new_data,locations[,.(ihme_loc_id,region_name,location_name)],by="ihme_loc_id")
new_data[,c("min_year","max_year") := .(min(year_id),max(year_id)),by=.(ihme_loc_id,survey_name)]
new_data[,.N,by=.(location_name,min_year,max_year,survey_name)]
write.csv(new_data[,.N,by=.(location_name,paste0(min_year,"-",max_year),survey_name)] %>% .[order(location_name)],file.path(out.dir,"data/new_2019_sources.csv"),row.names=F)
write.csv(dropped_data[,.(location_name,year_id,survey_name,reason)],file.path(out.dir,"data/dropped_2019_sources.csv"),row.names=F)
