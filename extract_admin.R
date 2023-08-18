#####################################################################
## Author: Megan Knight                                            ##
## Date: 03.01.2021                                                ##
## Description: Prep WHO administrative data                       ##
#####################################################################
## clear memory
rm(list=ls())

## libraries 
pacman::p_load(data.table)

## define directories 
indir <- 'FILEPATH'

## source functions
invisible(sapply(list.files('FILEPATH', full.names = T), source))

## pull location metadata 
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 7, decomp_step = 'iterative')
pop <- get_population(location_set_id = 35, location_id = 'all', gbd_round_id = 7, decomp_step = 'iterative', year_id = seq(1970,2019))

## read administrative data from WHO
who_phys <- fread(file.path(indir, 'HWF_0001,HWF_0002,HWF_0003,HWF_0004,HWF_0005.csv'))[, me_name := 'hrh_phys']
who_nurseagg <- fread(file.path(indir, 'HWF_0006,HWF_0007,HWF_0008,HWF_0009.csv'))[, me_name := 'hrh_nurseagg']
who_dent <- fread(file.path(indir, 'HWF_0010,HWF_0011,HWF_0012,HWF_0013.csv'))[, c('Country', 'Year', 'Dentists (per 10 000 population)', 'Dentists (number)')][, me_name := 'hrh_dent']
who_dentass <- fread(file.path(indir, 'HWF_0010,HWF_0011,HWF_0012,HWF_0013.csv'))[, c('Country', 'Year', 'Dental Assistants and Therapists (number)')][, me_name := 'hrh_dentass']
who_pharm <- fread(file.path(indir, 'HWF_0014,HWF_0015,HWF_0016.csv'))[, c('Country', 'Year', 'Pharmacists  (per 10 000 population)', 'Pharmacists (number)')][, me_name := 'hrh_pharm']
who_pharmtech <- fread(file.path(indir, 'HWF_0014,HWF_0015,HWF_0016.csv'))[, c('Country', 'Year', 'Pharmaceutical Technicians and Assistants (number)')][, me_name := 'hrh_pharmtech']

## update names for binding together 
setnames(who_phys, old = c('Country', 'Year', 'Medical doctors (per 10 000 population)', 'Medical doctors (number)'), 
         new = c('location_name', 'year_id', 'rate', 'num'))
setnames(who_nurseagg, old = c('Country', 'Year', 'Nursing and midwifery personnel (per 10 000 population)', 'Nursing and midwifery personnel  (number)'), 
         new = c('location_name', 'year_id', 'rate', 'num'))
setnames(who_dent, old = c('Country', 'Year', 'Dentists (per 10 000 population)', 'Dentists (number)'), 
         new = c('location_name', 'year_id', 'rate', 'num'))
setnames(who_dentass, old = c('Country', 'Year', 'Dental Assistants and Therapists (number)'), 
         new = c('location_name', 'year_id', 'num'))
setnames(who_pharm, old = c('Country', 'Year', 'Pharmacists  (per 10 000 population)', 'Pharmacists (number)'), 
         new = c('location_name', 'year_id', 'rate', 'num'))
setnames(who_pharmtech, old = c('Country', 'Year', 'Pharmaceutical Technicians and Assistants (number)'), 
         new = c('location_name', 'year_id', 'num'))

## drop observations with no rate or number 
who_phys <- who_phys[!(is.na(num) & is.na(rate))]
who_nurseagg <- who_nurseagg[!(is.na(num) & is.na(rate))]
who_dent <- who_dent[!(is.na(num) & is.na(rate))]
who_dentass <- who_dentass[!is.na(num)]
who_pharm <- who_pharm[!(is.na(num) & is.na(rate))]
who_pharmtech <- who_pharmtech[!is.na(num)]

## add rate column for merge (won't be used)
who_dentass$rate <- 0
who_pharmtech$rate <- 0

## combine WHO admin data 
who <- rbind(who_phys[, c('location_name', 'year_id', 'num', 'rate',  'me_name')], 
             who_nurseagg[, c('location_name', 'year_id', 'num', 'rate', 'me_name')],
             who_dent[, c('location_name', 'year_id', 'num', 'rate', 'me_name')], 
             who_dentass[, c('location_name', 'year_id', 'num', 'rate', 'me_name')], 
             who_pharm[, c('location_name', 'year_id', 'num', 'rate', 'me_name')], 
             who_pharmtech[, c('location_name', 'year_id', 'num', 'rate', 'me_name')])

## update location metadata for merging 
who$location_name <- ifelse(who$location_name == 'occupied Palestinian territory, including east Jerusalem', 'Palestine',
                            ifelse(who$location_name == 'United Kingdom of Great Britain and Northern Ireland', 'United Kingdom', who$location_name))

## attach location metadata and population 
who <- merge(who, locs[level == 3, c('location_name', 'location_id', 'ihme_loc_id', 'region_name', 'super_region_name')], by = 'location_name')
who <- merge(who[, year_id := as.integer(year_id)], pop[, c('location_id', 'year_id', 'population')], by = c('location_id', 'year_id'), all.x = T)

## calculate HRH density with IHME population count if number of workers is available (otherwise just use density from WHO)
who <- who[, alt_val := ifelse(!is.na(num), (num/population) * 10000, rate)]

## outliering physicians 
who[, is_outlier := ifelse(me_name == 'hrh_phys' & location_name == 'India' & year_id == 1991, 1, 
                    ifelse(me_name == 'hrh_phys' & location_name == 'Zambia' & year_id == 2018, 1, 
                    ifelse(me_name == 'hrh_phys' & location_name == 'Turkmenistan' & year_id == 2002, 1,
                    ifelse(me_name == 'hrh_phys' & location_name == "Democratic People's Republic of Korea", 1, 
                    ifelse(me_name == 'hrh_phys' & location_name == 'Egypt' & year_id == 2018, 1, 
                    ifelse(me_name == 'hrh_phys' & location_name == 'Guatemala' & year_id == 2018, 1, 
                    ifelse(me_name == 'hrh_phys' & location_name == 'Italy' & year_id < 2000, 1,
                    ifelse(me_name == 'hrh_phys' & location_name == 'Spain' & year_id <= 1995, 1,
                    ifelse(me_name == 'hrh_phys' & location_name == 'Belize' & year_id == 2014, 1, 
                    ifelse(me_name == 'hrh_phys' & location_name == 'Philippines' & year_id == 2017, 1, 
                    ifelse(me_name == 'hrh_phys' & location_name == 'Republic of Moldova' & year_id < 1990, 1, 0)))))))))))]

who[, is_outlier := ifelse(me_name == 'hrh_nurseagg' & location_name == "Democratic People's Republic of Korea", 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == "Brazil" & year_id %in% seq(2005, 2010), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Japan' & year_id %in% seq(1980, 2004), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Slovakia' & year_id == 2017, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Switzerland' & year_id == 2019, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Australia' & year_id %in% c(1996, 2001, 2006), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Israel' & year_id == 2018, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Zambia' & year_id %in% c(2013, 2016), 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Slovenia' & year_id %in% c(1998, 1999), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Finland' & year_id == 2015, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Austria'& year_id == 2018, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'France' & year_id %in% c(1991, 1996, 1999), 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Greece' & year_id %in% c(1990, 1991, 1992, 1993), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Luxembourg' & year_id %in% c(2007, 2008), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Switzerland' & year_id == 1990, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'United Kingdom' & year_id == 1997, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Jordan' & year_id == 2015, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Chile' & year_id %in% c(seq(2002, 2009), 2013), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Serbia' & year_id <= 2010, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Estonia' & year_id == 2000, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Russian Federation' & year_id <= 2004, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Belgium' & year_id >= 2017, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Uruguay' & year_id %in% c(2002, 2017), 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Turkey' & year_id %in% seq(1990, 2000), 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'India' & year_id %in% seq(2000,2005), 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Mauritius' & year_id == 2004, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Uganda' & year_id == 2004, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Botswana' & year_id == 2013, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'South Africa' & year_id <= 2004, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Mauritania' & year_id == 2013, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Sudan' & year_id == 2013, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Bulgaria' & year_id %in% seq(1981, 1984), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Croatia' & year_id %in% seq(1981, 1984), 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Hungary' & year_id < 1990, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Latvia' & year_id < 1990, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Greece' & year_id <= 1993, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Romania' & year_id < 2000, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Austria' & year_id < 1985, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Switzerland' & year_id <= 1990, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Panama' & year_id < 2000, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Turkey' & year_id <= 2000, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Malta' & year_id %in% c(1984, 1993), 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Sao Tome and Principe' & year_id %in% c(2013, 2017), 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Guinea' & year_id == 2013, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Gambia' & year_id == 2013, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Cameroon' & year_id == 2013, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Rwanda' & year_id == 2013, 1,is_outlier)))))))))))))))))))))))))))))))))))))))))))))))]

who[, is_outlier := ifelse(me_name == 'hrh_nurseagg' & location_name == 'Malawi' & year_id == 2013, 1, 
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Burundi' & year_id == 2004, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Democratic Republic of the Congo' & year_id == 2016, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Angola' & year_id == 2013, 1,
                    ifelse(me_name == 'hrh_nurseagg' & location_name == 'Iran (Islamic Republic of)' & year_id == 2018, 1, is_outlier)))))]

## outlier dentists 
who[, is_outlier := ifelse(me_name == 'hrh_dent' & location_name == 'Republic of Moldova' & year_id < 2000, 1, 
                    ifelse(me_name == 'hrh_dent' & location_name == 'Montenegro', 1,   
                    ifelse(me_name == 'hrh_dent' & location_name == 'Chile' & year_id %in% seq(2002, 2009), 1, 
                    ifelse(me_name == 'hrh_dent' & location_name == 'Costa Rica' & year_id == 2017, 1,
                    ifelse(me_name == 'hrh_dent' & location_name == 'Guatemala' & year_id == 2018, 1, 
                    ifelse(me_name == 'hrh_dent' & location_name == 'Angola' & year_id == 1997, 1, is_outlier))))))]

## outlier dental assistants 
who[, is_outlier := ifelse(me_name == 'hrh_dentass' & location_name == 'Paraguay' & year_id == 2018, 1,
                    ifelse(me_name == 'hrh_dentass' & location_name == 'India' & year_id >= 2017, 1, 
                    ifelse(me_name == 'hrh_dentass' & location_name == 'India' & year_id >= 2017, 1, is_outlier)))]

## outlier pharmacists 
who[, is_outlier := ifelse(me_name == 'hrh_pharm' & location_name == 'Cyprus' & year_id == 2015, 1, is_outlier)]

## outlier pharmacy technicians
who[, is_outlier := ifelse(me_name == 'hrh_pharmtech' & location_name == 'Costa Rica' & year_id >= 2015, 1,
                    ifelse(me_name == 'hrh_pharmtech' & location_name == 'Egypt', 1,
                    ifelse(me_name == 'hrh_pharmtech' & location_name == 'Central African Republic', 1, is_outlier)))]

who[location_name == 'Belize' & me_name == 'hrh_nurseagg' & year_id %in% seq(2013, 2017), alt_val := alt_val/100]
who[location_name == 'Romania' & me_name == 'hrh_nurseagg' & year_id <= 1999, alt_val := alt_val*10]
who[location_name == 'Netherlands' & alt_val <= 50 & me_name == 'hrh_nurseagg', alt_val := alt_val*100]
who[location_name == 'Algeria' & me_name == 'hrh_nurseagg' & year_id == 2014, alt_val := alt_val*10]
who[location_name == 'Gabon' & me_name == 'hrh_nurseagg' & year_id == 2013, alt_val := alt_val*10]
who[location_name == 'Philippines' & me_name == 'hrh_dent' & year_id %in% c(2015, 2016, 2019), alt_val := alt_val*100]
who[location_name == 'Egypt' & me_name == 'hrh_dent' & year_id == 2018, alt_val := alt_val/10]

## export prepped data 
write.csv(who, file.path(indir, 'who_admin_prepped.csv'), row.names = F)
