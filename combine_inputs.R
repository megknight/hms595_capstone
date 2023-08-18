#####################################################################
## Name: Megan Knight                                              ##
## Purpose: Produce input data data for stochastic frontier        ##
## Date: 05/10/2021                                                ##
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
pacman::p_load(data.table, ggplot2, scales, ggrepel)

## source functions
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_population.R")


## load location metadata 
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 6, decomp_step = 'iterative')
pop <- get_population(age_group_id = 22, sex_id = 3, location_id = -1,  year_id = 2019,gbd_round_id = 6, decomp_step = 'iterative')[,list(location_id,year_id,population)]
global_pop <- get_population(age_group_id = 22, sex_id = 3, location_id = 1,  year_id = 2019,gbd_round_id = 6, decomp_step = 'iterative')[,list(location_id,year_id,population)]

## hrh
hrh_amb <- fread('FILEPATH/hrh_amb_1.csv')
hrh_any <- fread('FILEPATH/hrh_any_1.csv')
hrh_audio <- fread('FILEPATH/hrh_audio_1.csv')
hrh_clinic <- fread('FILEPATH/hrh_clinic_1.csv')
hrh_diet <- fread('FILEPATH/hrh_diet_1.csv')
hrh_envir <- fread('FILEPATH/hrh_envir_1.csv')
hrh_medtech <- fread('FILEPATH/hrh_medtech_1.csv')
hrh_opt <- fread('FILEPATH/hrh_opt_1.csv')
hrh_pcare <- fread('FILEPATH/hrh_pcare_1.csv')
hrh_psych <- fread('FILEPATH/hrh_psych_1.csv')
hrh_radio <- fread('FILEPATH/hrh_radio_1.csv')
hrh_therap <- fread('FILEPATH/hrh_therap_1.csv')
hrh_trad <- fread('FILEPATH/hrh_trad_1.csv')
hrh_phys <- fread('FILEPATHhrh_phys_1_admin_adj.csv')
hrh_dent <- fread('FILEPATHhrh_dent_1_admin_adj.csv')
hrh_dentass <- fread('FILEPATHhrh_dentass_1_admin_adj.csv')
hrh_pharm <- fread('FILEPATHhrh_pharm_1_admin_adj.csv')
hrh_pharmtech <- fread('FILEPATHhrh_pharmtech_1_admin_adj.csv')
hrh_nurseagg <- fread('FILEPATHhrh_nurseagg_1_admin_adj.csv')

hrh <- rbind(hrh_amb[, .(nid, location_id)], 
             hrh_any[, .(nid, location_id)], 
             hrh_audio[, .(nid, location_id)], 
             hrh_clinic[, .(nid, location_id)], 
             hrh_diet[, .(nid, location_id)], 
             hrh_envir[, .(nid, location_id)], 
             hrh_medtech[, .(nid, location_id)], 
             hrh_opt[, .(nid, location_id)], 
             hrh_pcare[, .(nid, location_id)], 
             hrh_psych[, .(nid, location_id)], 
             hrh_radio[, .(nid, location_id)], 
             hrh_therap[, .(nid, location_id)], 
             hrh_trad[, .(nid, location_id)], 
             hrh_phys[, .(nid, location_id)], 
             hrh_dent[, .(nid, location_id)], 
             hrh_dentass[, .(nid, location_id)], 
             hrh_pharm[, .(nid, location_id)], 
             hrh_pharmtech[, .(nid, location_id)], 
             hrh_nurseagg[, .(nid, location_id)])

hrh <- merge(hrh, locs[, c('location_id', 'level')], by = 'location_id')
hrh <- merge(hrh, pop[, c('location_id', 'population')], by = 'location_id')

who <- fread('FILEPATH/who_admin_prepped.csv')
