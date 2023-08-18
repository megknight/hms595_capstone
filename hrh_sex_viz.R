## plots for thesis 


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


## library 
pacman::p_load(ggplot2,stringr)
library(forcats, lib.loc = "~/rlibs/")

## source functions 
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
source('/ihme/cc_resources/libraries/current/r/get_population.R')
source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
source("~/repos/functions/gbd_2021_map.R")

## directories 
dir <- '/mnt/share/scratch/projects/hssa/occ/by_sex/results/estimates'

## pull covariates
the <- fread('/mnt/share/resource_tracking/forecasting/the/V48/stats/the_pc.csv')[scenario == 0]
locs <- get_location_metadata(release_id = 9, location_set_id = 35)
sdi <- get_location_metadata(release_id = 9, location_set_id = 40)
sdi_labs <- sdi[level==0, c('parent_id', 'location_name')]
sdi <- merge(sdi[, c('location_id', 'parent_id')], sdi_labs, by = 'parent_id')
setnames(sdi, old = c('parent_id', 'location_name'), new = c('sdi','sdi_group'))
pop <- get_population(release_id = 9, 
                      location_id = locs[level==3,location_id],
                      age_group_id=seq(8,18,1), 
                      year_id = seq(1980,2022,1)) 

## 
pop <- merge(pop, locs[, c('location_id', 'region_name', 'super_region_name', 'sort_order')])
loc_pop <- pop[,.(pop = sum(population)), by = c('location_id', 'year_id', 'super_region_name', 'region_name')]
reg_pop <- pop[,.(reg_pop = sum(population)), by = c('region_name', 'year_id')]
srreg_pop <- pop[,.(srreg_pop = sum(population)), by = c('super_region_name', 'year_id')]

pop <- merge(loc_pop, reg_pop, by = c('region_name', 'year_id'))
pop <- merge(pop, srreg_pop, by = c('super_region_name', 'year_id'))
pop[, reg_weight := pop/reg_pop][, srreg_weight := pop/srreg_pop]
pop[, sum(reg_weight), by = c('region_name', 'year_id')]
pop[,sum(srreg_weight), by = c('super_region_name', 'year_id')]

## aggregate to both sexes population between 15-69 (age_group_id = 201)
sex <- fread(file.path(dir, 'hrh_sex_stgpr_2023-08-10.csv'))
sex <- merge(sex, locs[, c('location_id','level','super_region_name','region_name', 'location_name', 'ihme_loc_id', 'sort_order')], by = 'location_id', all.x = T)
sex <- merge(sex, pop[, c('location_id', 'year_id', 'reg_weight', 'srreg_weight')], by = c('location_id', 'year_id'), all.x = T)
sex <- merge(sex, sdi[, c('location_id', 'sdi','sdi_group')], by = 'location_id', all.x = T)
sex <- merge(sex, the[, c('iso3', 'year', 'mean')], by.x = c('ihme_loc_id', 'year_id'), by.y = c('iso3', 'year'), all.x = T)
sex <- merge(sex, pop[, c('location_id', 'year_id','pop')], by = c('location_id', 'year_id'), all.x = T)

sex_reg_avg <- sex[level == 3, ][, reg_weighted_val := val*reg_weight]
sex_reg_avg <- sex_reg_avg[, .(val = sum(reg_weighted_val)), by = c('region_name', 'year_id', 'me_name')]

sex_srreg_avg <- sex[level == 3, ][, srreg_weighted_val := val*srreg_weight]
sex_srreg_avg <- sex_srreg_avg[, .(val = sum(srreg_weighted_val)), by = c('super_region_name', 'year_id', 'me_name')]

sex[, me_name_clean := str_replace(sex$me_name,"_sex", "")]
## sex <- merge(sex, titles, by.x = 'me_name_clean', by.y = 'me_name', all.x = T)
sex[me_name == 'hrh_nurseagg_sex', title := 'Nursing and midwifery personnel']

cadres <- c("hrh_any_sex","hrh_nurseagg_sex","hrh_phys_sex","hrh_dent_sex","hrh_pharm_sex")
dir <- "/mnt/share/scratch/projects/hssa/occ/by_sex/results/viz"

## subset data and format for plotting 
any <- sex[level == 3 & me_name == 'hrh_any_sex' & year_id == 2022, c('location_id', 'val')]
setnames(any, old = 'val', new = 'mapvar')

nurse <- sex[level == 3 & me_name == 'hrh_nurseagg_sex' & year_id == 2022, c('location_id', 'val')]
setnames(nurse, old = 'val', new = 'mapvar')

phys <- sex[level == 3 & me_name == 'hrh_phys_sex' & year_id == 2022, c('location_id', 'val')]
setnames(phys, old = 'val', new = 'mapvar')

all <- rbind(phys, nurse)

quants <- quantile(all$mapvar, seq(0,1,by = 0.1))  

pdf('~/test.pdf')
gbd_map(any, 
        col.reverse = F, 
        na.color = "#D3D3D3",
        col=c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'),
        limits = quants,
        labels = c("14.7 to 41.7", "41.7 to 48.8","48.8 to 58.7", "58.7 to 67.2",  "67.2 to 75.1", "75.1 to 79.9", "79.9 to 83.4", "83.4 to 86.8", "86.8 to 91.1", "91.1 to 99.7"),
        sub_nat = "none",
        legend.cex = 1.6,
        legend.title = 'Share of female workers', 
        legend.columns = 1,
        inset = F)

gbd_map(phys, 
        col.reverse = F, 
        na.color = "#D3D3D3",
        col=c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'),
        limits = quants,
        labels = c("14.7 to 37.3", "37.3 to 44.1","44.1 to 48.9", "48.9 to 54.0",  "54.0 to 64.1", "64.1 to 74.4", "74.4 to 83.7", "83.7 to 88.8", "88.8 to 94.8", "94.8 to 99.7"),
        sub_nat = "none",
        legend.cex = 1.6,
        legend.title = 'Share of female workers', 
        legend.columns = 1, 
        inset = F,
        fname = file.path(dir, 'mak_thesis_fig1b.pdf'))

gbd_map(nurse, 
        col.reverse = F, 
        na.color = "#D3D3D3",
        col=c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'),
        labels = c("14.7 to 37.3", "37.3 to 44.1","44.1 to 48.9", "48.9 to 54.0",  "54.0 to 64.1", "64.1 to 74.4", "74.4 to 83.7", "83.7 to 88.8", "88.8 to 94.8", "94.8 to 99.7"),
        limits = quants,
        sub_nat = "none",
        legend.cex = 1.6,
        legend.title = 'Share of female workers', 
        legend.columns = 1, 
        inset = F, 
        fname = file.path(dir, 'mak_thesis_fig1c.pdf'))


for (cadre in cadres){
  p1 <- ggplot(data = sex[me_name == cadre & level == 3], 
              aes(x = year_id, y= val, group=location_id, color = super_region_name)) + 
    geom_line() + 
    labs(x = "Year", y = "Share of female workers", color = "Super-region", title=paste("Share of female", tolower(unique(sex[me_name == cadre & level == 3, 'title'])), "by location, 1990-2022"))+
    theme_bw()
  
  p2 <- ggplot(data = sex_reg_avg[me_name == cadre], 
               aes(x = year_id, y= val, group=region_name, color = region_name)) + 
    geom_line() + 
    labs(x = "Year", y = "Share of female workers", color = "Region")+
    theme_bw()
  
  p3 <- ggplot(data = sex_srreg_avg[me_name == cadre], 
               aes(x = year_id, y= val, group=super_region_name, color = super_region_name)) + 
    geom_line() + 
    labs(x = "Year", y = "Share of female workers", color = "Super-region")+
    theme_bw()
  
  pdf(file.path(dir, paste0(cadre, "_1990_2022.pdf")), width=8*1.68, height=8)
  print(p1)
  print(p2)
  print(p3)
  dev.off()
}

sex[, me_name := factor(me_name, levels = c('hrh_any_sex', 'hrh_phys_sex', 'hrh_nurseagg_sex'))]
sex[, global_ind := ifelse(location_name == 'Global', "yes", "no")]

pdf(file.path(dir, paste0("hrh_sex_bycadre_1990_2022.pdf")), width=15, height=8)
p <- ggplot(data = sex[me_name %in% c('hrh_phys_sex', 'hrh_nurseagg_sex','hrh_any_sex') & location_id %in% c(1,31,64,103,137,158,4,166)], 
       aes(lwd = global_ind, x = year_id, y=100*val, group=location_name, color = factor(location_name, levels = c('Global',
                                                                                                                      'Central Europe, Eastern Europe, and Central Asia',
                                                                                                                      'High-income',
                                                                                                                      'Latin America and Caribbean',
                                                                                                                      'North Africa and Middle East',
                                                                                                                      'South Asia',
                                                                                                                      'Southeast Asia, East Asia, and Oceania',
                                                                                                                      'Sub-Saharan Africa')))) + 
  geom_line() + 
  scale_color_manual(values = c("#4d4d4d", '#b35806', '#f1a340', '#fee0b6', '#d8b365', '#d8daeb', '#998ec3', '#542788')) + 
  scale_size_manual(values = c(1, 2)) + 
  facet_wrap(~me_name, labeller = labeller(me_name = 
                                             c("hrh_any_sex" = "All Health Workers",
                                               "hrh_phys_sex" = "Physicians",
                                               "hrh_nurseagg_sex" = "Nursing and Midwives"))) +
  labs(x = "", y = "Share of Female Workers", color = "GBD super region") +
  lims(y = c(0,100)) + 
  theme_bw() + 
  theme(strip.background = element_blank(), 
        legend.position = 'bottom') + 
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) + 
  theme(axis.title=element_text(size=14), 
        axis.text=element_text(size=12), 
        strip.text.x = element_text(size = 14), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=14)) + 
  guides(color = guide_legend(override.aes = list(linewidth = 4), ncol = 2, title.position = "top", title.hjust=0.5), 
         linewidth = guide_legend(position="none"))
  
print(p)
dev.off()

# phys1990 <- sex[me_name == 'hrh_phys_sex' & year_id == 1990 & level <= 3, ][, val_1990 := paste(round(val*100,1), "\n (",round(lower*100,1), " to ", round(upper*100,1), ")")]
# phys2022 <- sex[me_name == 'hrh_phys_sex' & year_id == 2022 & level <= 3, ][, val_2022 := paste(round(val*100,1), "\n (",round(lower*100,1), " to ", round(upper*100,1), ")")]
# phys <- merge(phys1990[, c('sort_order', 'location_id', 'me_name', 'super_region_name','region_name', 'val_1990','location_name')],
#              phys2022[, c('location_id',  'super_region_name','region_name', 'val_2022')], by = c('location_id',  'super_region_name','region_name'))

phys1990 <- sex[me_name == 'hrh_phys_sex' & year_id == 1990 & level <= 3, ][, label := 'Physicians \n in 1990']
phys2022 <- sex[me_name == 'hrh_phys_sex' & year_id == 2022 & level <= 3, ][, label := 'Physicians \n in 2022']
phys <- rbind(phys1990[, c('year_id','level', 'sort_order', 'location_id', 'me_name', 'super_region_name','region_name', 'val','location_name', 'label')],
             phys2022[, c('year_id','level', 'sort_order', 'location_id', 'me_name', 'super_region_name','region_name', 'val','location_name', 'label')])

# any1990 <- sex[me_name == 'hrh_any_sex' & year_id == 1990 & level <= 3, ][, val_1990 := paste(round(val*100,1), "\n (",round(lower*100,1), " to ", round(upper*100,1), ")")]
# any2022 <- sex[me_name == 'hrh_any_sex' & year_id == 2022 & level <= 3, ][, val_2022 := paste(round(val*100,1), "\n (",round(lower*100,1), " to ", round(upper*100,1), ")")]
# any <- merge(any1990[, c('location_id', 'me_name', 'super_region_name','region_name', 'val_1990')],
#              any2022[, c('location_id',  'super_region_name','region_name', 'val_2022')], by = c('location_id',  'super_region_name','region_name'))

any1990 <- sex[me_name == 'hrh_any_sex' & year_id == 1990 & level <= 3, ][, label := 'All health workers \n in 1990']
any2022 <- sex[me_name == 'hrh_any_sex' & year_id == 2022 & level <= 3, ][, label := 'All health workers \n in 2022']
any <- rbind(any1990[, c('year_id','level', 'sort_order', 'location_id', 'me_name', 'super_region_name','region_name', 'val','location_name', 'label')],
             any2022[, c('year_id','level', 'sort_order', 'location_id', 'me_name', 'super_region_name','region_name', 'val','location_name', 'label')])


# df <- merge(phys, any)[, !c('me_name.x','me_name.y')]
# setnames(df, old = c('val_1990.x',"val_2022.x","val_1990.y","val_2022.y"), new = c('hrh_phys_1990','hrh_phys_2022','hrh_any_1990','hrh_any_2022'))

# nurseagg1990 <- sex[me_name == 'hrh_nurseagg_sex' & year_id == 1990 & level <= 3, ][, val_1990 := paste(round(val*100,1), "\n (",round(lower*100,1), " to ", round(upper*100,1), ")")]
# nurseagg2022 <- sex[me_name == 'hrh_nurseagg_sex' & year_id == 2022 & level <= 3, ][, val_2022 := paste(round(val*100,1), "\n (",round(lower*100,1), " to ", round(upper*100,1), ")")]
# nurseagg <- merge(nurseagg1990[, c('location_id', 'me_name', 'super_region_name','region_name', 'val_1990')],
#                   nurseagg2022[, c('location_id',  'super_region_name','region_name', 'val_2022')], by = c('location_id',  'super_region_name','region_name'))

nurseagg1990 <- sex[me_name == 'hrh_nurseagg_sex' & year_id == 1990 & level <= 3, ][, label := 'Nursing and midwifery \n personnel in 1990']
nurseagg2022 <- sex[me_name == 'hrh_nurseagg_sex' & year_id == 2022 & level <= 3, ][, label := 'Nursing and midwifery \n personnel in 2022']
nurseagg <- rbind(nurseagg1990[, c('year_id', 'level', 'sort_order', 'location_id', 'me_name', 'super_region_name','region_name', 'val','location_name', 'label')],
                  nurseagg2022[, c('year_id', 'level', 'sort_order', 'location_id', 'me_name', 'super_region_name','region_name', 'val','location_name', 'label')])

# df <- merge(df,nurseagg)[, !c('me_name')]
# setnames(df, old = c('val_1990',"val_2022"), new = c('hrh_nurseagg_1990','hrh_nursagg_2022'))

df <- rbind(phys, any, nurseagg)
library(forcats)
df[, location_name := fct_reorder(location_name, sort_order, .desc = T)]

#df[, c("year_id","location_id","me_name","mean")][, val := mean][, mean := NULL]
#df <- merge(df, locs[, c('level', 'sort_order', 'super_region_name', 'region_name', 'location_name', 'location_id')], by = 'location_id')
df[, label := ifelse(me_name == 'hrh_phys' & year_id == 1990, "hrh_phys_dens_1990", 
                ifelse(me_name == 'hrh_phys' & year_id == 2022, "hrh_phys_dens_2022", 
                ifelse(me_name == 'hrh_nurseagg' & year_id == 1990, "hrh_nurseagg_dens_1990", 
                ifelse(me_name == 'hrh_nurseagg' & year_id == 2022, "hrh_nurseagg_dens_2022",
                ifelse(me_name == 'hrh_any' & year_id == 1990, "hrh_any_dens_1990", 
                ifelse(me_name == 'hrh_any' & year_id == 2022, "hrh_any_dens_2022", me_name))))))]

df <- df[year_id %in% c(1990, 2022) & label %in% c("hrh_phys_dens_1990", "hrh_phys_dens_2022", "hrh_nurseagg_dens_1990","hrh_nurseagg_dens_2022","hrh_any_dens_1990","hrh_any_dens_2022") & level <= 3]

pdf(file = "~/mak_capstone_table1.pdf", width=13, height=45)
ggplot(df, aes(x=label, y=location_name, fill=val*100, label=round(val*100))) +
  geom_tile() + 
  geom_text() + 
  scale_fill_gradientn(colours = c("#b35806", "white", "#542788"),
                       values = c(0, 0.5, 1)) +
  labs(fill = "Share of female workers", x = '', y = '') + 
  scale_x_discrete(
    expand = expansion(mult = c(0,0)),
    position = "top"
  ) +
  theme_classic() +
  theme(strip.text = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.line = element_blank())
dev.off()



write.csv(df, file.path(dir, paste0("hrh_sex_table_1990_2022.csv")),row.names=F)

sex[is.na(sdi_group)]
p <- ggplot(data = sex[me_name %in% c("hrh_any_sex", "hrh_phys_sex", 'hrh_nurseagg_sex') & year_id %in% c(1990,2000,2010,2022) & level == 3 & !is.na(sdi),],
            aes(x = factor(sdi_group, levels = c("Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"),
                           labels = c("Low SDI", "Low-middle \n SDI", "Middle SDI", "High-middle \n SDI", "High SDI")), y =100*val, fill = factor(year_id))) + 
  geom_boxplot() + 
  facet_wrap(~factor(me_name, levels = c('hrh_any_sex', 'hrh_phys_sex', 'hrh_nurseagg_sex'), 
                              labels = c('hrh_any_sex' = "All Health Workers", 
                                         'hrh_phys_sex' = "Physicians", 
                                         'hrh_nurseagg_sex' = "Nursing and Midwifery Personnel"))) + 
  scale_fill_manual(values = c("#e66101","#fdb863", "#b2abd2", "#5e3c99")) + 
  
  labs(x = "Socio-demographic Index (SDI)", fill = 'Year', y = 'Share of Female Workers') + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.title=element_text(size=14), 
        axis.text=element_text(size=12), 
        strip.text.x = element_text(size = 14), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=14), 
        legend.position="bottom") 

pdf(file.path(dir, paste0("hrh_sex_sdi_boxplots.pdf")), width = 16, height = 8)
print(p)
dev.off()

p <- ggplot(data = sex[me_name %in% c("hrh_any_sex", "hrh_phys_sex", "hrh_nurseagg_sex") & 
                       year_id %in% c(2022) & level == 3,],
            aes(x = log(mean), y = val, fill = super_region_name)) + 
  geom_point(shape = 21) +
  facet_wrap(~me_name, ncol=1, labeller = labeller(me_name = 
                                                     c("hrh_any_sex" = "All health workers",
                                                       "hrh_phys_sex" = "Physicians",
                                                       "hrh_nurseagg_sex" = "Nursing and midwifery personnel"))) + 
  labs(x = 'Total health expenditure per capita (logged)',
       y = 'Share of female health workers', fill = 'Super-region')+
  scale_fill_manual(values = c('#b35806', '#f1a340', '#fee0b6', '#d8b365', '#d8daeb', '#998ec3', '#542788')) + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        legend.position = 'bottom') + 
  guides(fill = guide_legend(ncol = 2, title.position = "top", title.hjust=0.5))

pdf(file.path(dir, paste0("hrh_sex_the_scatters.pdf")), width = 6, height = 8)
print(p)
dev.off()

############################################################################################################
## Name: Caleb Irvine
## Purpose: Prepare data for HRH Paper tables and figures
## Date:
## Notes:
## R Version: /usr/local/codem/public_use_anaconda/bin/R
## source("",echo=T)
###########################################################################################################
## clear memory
rm(list=ls())

## runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <- "/homes/calebi/"
  k <- "/ihme/cc_resources/libraries"
} else {
  j <- "J:/"
  h <- "H:/"
  k <- "K:/libraries"
}

invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
pacman::p_load(data.table,ggplot2,grid,gridExtra,ggpubr)

## location metadata 
locations <- get_location_metadata(release_id = 9, location_set_id = 22)
locs <- copy(locations[level == 3, list(location_id,ihme_loc_id,location_name,super_region_name)])

## in/out
in.dir <- "/ihme/scratch/projects/hssa/occ/results/summaries/hrh/thesis"
out.dir <- "/share/scratch/projects/hssa/occ/hrh_paper/figures/"
model.dir <- "/ihme/covariates/ubcov/model/output"

## draws for all health workers, physicians, and nursing and midwifery personnel sex-disaggregation
any_files <- list.files(file.path("/share/covariates/ubcov/model/output",206480,"draws_temp_0"),full.names = T)
any_sex <- rbindlist(lapply(any_files,fread),use.names=T)[, me_name := 'hrh_any']
phys_files <- list.files(file.path("/share/covariates/ubcov/model/output",206527,"draws_temp_0"),full.names = T)
phys_sex <- rbindlist(lapply(phys_files,fread),use.names=T)[, me_name := 'hrh_phys']
nurses_files <- list.files(file.path("/share/covariates/ubcov/model/output",206525,"draws_temp_0"),full.names = T)
nurses_sex <- rbindlist(lapply(nurses_files,fread),use.names=T)[, me_name := 'hrh_nurseagg']

## melt and combine 
any_sex <- melt(any_sex, id.vars = c("year_id","location_id","sex_id","age_group_id","me_name"))[, hrh_any_sex := value][, !c('value','me_name')]
phys_sex <- melt(phys_sex, id.vars = c("year_id","location_id","sex_id","age_group_id","me_name"))[, hrh_phys_sex := value][, !c('value','me_name')]
nurses_sex <- melt(nurses_sex, id.vars = c("year_id","location_id","sex_id","age_group_id","me_name"))[, hrh_nurseagg_sex := value][, !c('value','me_name')]
sex <- merge(any_sex, phys_sex, by = c("year_id","location_id","sex_id","age_group_id", 'variable'))
sex <- merge(sex, nurses_sex, by = c("year_id","location_id","sex_id","age_group_id", 'variable'))

## draws for all health workers, physicians, and nursing and midwifery personnel densities
files <- list.files(gsub("summaries","draws",in.dir),full.names=T)
files <- files[grepl("hrh_any|hrh_nurseagg|hrh_phys",files)]
dt <- fread(files[1])[,-c("age_group_id","sex_id"),with=F]
dt <- melt(dt[,-c("me_name"),with=F],id.vars=c("location_id","ihme_loc_id","year_id"),value.name = dt[,unique(me_name)])
for (i in seq(2,length(files))) {
  x <- fread(files[i])[,-c("age_group_id","sex_id"),with=F]
  x <- melt(x[,-c("me_name"),with=F],id.vars=c("location_id","ihme_loc_id","year_id"),value.name = x[,unique(me_name)])
  dt <- merge(dt,x,by=names(dt)[1:4])
}
setnames(dt,"variable","draw")

# dt <- merge(dt, sex, by.x = c('year_id', 'location_id', 'draw'),   by.y = c('year_id', 'location_id', 'variable'))
# val <- dt[, .(hrh_any = mean(hrh_any), 
#        hrh_any_sex = mean(hrh_any_sex), 
#        hrh_phys = mean(hrh_phys), 
#        hrh_phys_sex = mean(hrh_phys_sex),
#        hrh_nurseagg = mean(hrh_nurseagg), 
#        hrh_nurseagg_sex = mean(hrh_nurseagg_sex)), by = c('year_id','location_id', 'ihme_loc_id')]
# 
# lower <- dt[, .(hrh_any = quantile(hrh_any, p=0.025), 
#               hrh_any_sex = quantile(hrh_any_sex, p=0.025), 
#               hrh_phys = quantile(hrh_phys, p=0.025), 
#               hrh_phys_sex = quantile(hrh_phys_sex, p=0.025),
#               hrh_nurseagg = quantile(hrh_nurseagg, p=0.025),  
#               hrh_nurseagg_sex = quantile(hrh_nurseagg_sex, p=0.025)), by = c('year_id','location_id', 'ihme_loc_id')]
# 
# upper <- dt[, .(hrh_any = quantile(hrh_any, p=0.975), 
#                 hrh_any_sex = quantile(hrh_any_sex, p=0.975), 
#                 hrh_phys = quantile(hrh_phys, p=0.975), 
#                 hrh_phys_sex = quantile(hrh_phys_sex, p=0.975),
#                 hrh_nurseagg = quantile(hrh_nurseagg, p=0.975),  
#                 hrh_nurseagg_sex = quantile(hrh_nurseagg_sex, p=0.975)), by = c('year_id','location_id', 'ihme_loc_id')]
# 
# val <- melt(val, id.vars = c('year_id', 'location_id', 'ihme_loc_id'), value.name = 'val')
# lower <- melt(lower, id.vars = c('year_id', 'location_id', 'ihme_loc_id'), value.name = 'lower')
# upper <- melt(upper, id.vars = c('year_id', 'location_id', 'ihme_loc_id'), value.name = 'upper')
# 
# df <- merge(val, lower, by = c('year_id', 'location_id', 'ihme_loc_id', 'variable'))
# df <- merge(df, upper, by = c('year_id', 'location_id', 'ihme_loc_id', 'variable'))
# 
# df[, me_name := ifelse(variable == 'hrh_any', 'All health workers', 
#                        ifelse(variable == 'hrh_phys', 'Physicians', 
#                               ifelse(variable == 'hrh_nurseagg', 'Nursing and midwifery personnel', 
#                                      ifelse(variable == 'hrh_any_sex', 'Female share of all health workers', 
#                                             ifelse(variable == 'hrh_phy_sex', 'Female share of physicians', 'Female share of nursing and midwifery personnel')))))]
# df <- df[, !c('variable')]
# write.csv(df, '~/MAK_ESTIMATES_DESC.csv', row.names=F)


## merge on region and super-region info and remove subnats and non-reportable years
locs3 <- copy(locations[level == 3, list(location_id,ihme_loc_id,location_name,region_name,super_region_name)])
all_locs <- copy(locations[, list(location_id,ihme_loc_id,location_name,region_name,super_region_name)])
dt <- merge(dt,locs3,by=c("location_id","ihme_loc_id"))

## combine sex and densities
dt <- merge(dt, sex, by.x = c('location_id','year_id','draw'), by.y = c('location_id','year_id','variable'))
dt[, hrh_any_f := hrh_any*hrh_any_sex][, hrh_any_m := hrh_any*(1-hrh_any_sex)]
dt[, hrh_phys_f := hrh_phys*hrh_phys_sex][, hrh_phys_m := hrh_phys*(1-hrh_phys_sex)]
dt[, hrh_nurseagg_f := hrh_nurseagg*hrh_nurseagg_sex][, hrh_nurseagg_m := hrh_any*(1-hrh_nurseagg_sex)]

## aggregate to region, super_region, and global level results
supers <- copy(dt)
pops <- get_population(age_group_id = 22, sex_id = 3,location_id = -1,year_id = seq(1990,2022),release_id = 9)[,list(location_id,year_id,population)]
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

globals <- globals[, lapply(.SD, mean), .SDcols = c('global_hrh_any_sex', 'global_hrh_any',
                                                    'global_hrh_phys_sex', 'global_hrh_phys',
                                                    'global_hrh_nurseagg_sex', 'global_hrh_nurseagg'), by = 'year_id']

globals[, global_hrh_any_sex_f := global_hrh_any_sex*global_hrh_any][, global_hrh_any_sex_m := (1-global_hrh_any_sex)*global_hrh_any]
globals[, global_hrh_phys_sex_f := global_hrh_phys_sex*global_hrh_phys][, global_hrh_phys_sex_m := (1-global_hrh_phys_sex)*global_hrh_phys]
globals[, global_hrh_nurseagg_sex_f := global_hrh_nurseagg_sex*global_hrh_nurseagg][, global_hrh_nurseagg_sex_m := (1-global_hrh_nurseagg_sex)*global_hrh_nurseagg]
globals <- globals[, c('global_hrh_any_sex_f', 'global_hrh_any_sex_m',
                       'global_hrh_phys_sex_f', 'global_hrh_phys_sex_m',
                       'global_hrh_nurseagg_sex_f', 'global_hrh_nurseagg_sex_m', 'year_id')]

globals <- melt(globals, id.vars = 'year_id')
globals[, cadre := str_replace(variable, "_f", "")]
globals[, cadre := str_replace(cadre, "_m", "")]
globals[, sex := ifelse(variable %like% "_m", "Males", "Females")]

agg <- globals[, .(value_replace = sum(value)), by = c('year_id','cadre')][, sex := 'Females']
agg_alt <- globals[, .(value_replace_alt = sum(value)), by = c('year_id','cadre')][, sex := 'Males']

globals <- merge(globals, agg, by = c('year_id','cadre', 'sex'), all.x = T)
globals <- merge(globals, agg_alt, by = c('year_id','cadre', 'sex'), all.x = T)

globals[, value_test := ifelse(!is.na(value_replace), value_replace, value)]


any <- ggplot(data=globals[cadre == 'global_hrh_any_sex'], aes(x=year_id,y=value_test,fill=factor(sex, levels = c('Females', 'Males')))) + 
  geom_density(stat = "identity", alpha = 1) + 
  labs(x="Year",y="All health workers per 10,000") +
  scale_fill_manual(values=c("#998ec3", "#f1a340")) + 
  lims(y=c(0, 150)) + 
  geom_vline(xintercept=2020, linewidth = 1) + 
  theme_bw() + 
  theme(legend.position ='none')

phys <- ggplot(data=globals[cadre == 'global_hrh_phys_sex'], aes(x=year_id,y=value_test,fill=factor(sex, levels = c(  'Females', 'Males')))) + 
  geom_density(stat = "identity", alpha = 1) + 
  labs(x="Year",y="Physicians per 10,000") + 
  scale_fill_manual(values=c("#998ec3", "#f1a340")) + 
  geom_vline(xintercept=2020, linewidth = 1) + 
  theme_bw() + 
  theme(legend.position ='none')


nurses <- ggplot(data=globals[cadre == 'global_hrh_nurseagg_sex'], aes(x=year_id,y=value_test,fill=factor(sex, levels = c('Females', 'Males')))) + 
  geom_density(stat = "identity", alpha = 1) + 
  labs(x="Year",y="Nursing and midwifery personnel per 10,000") + 
  lims(y=c(0,50)) + 
  scale_fill_manual(values=c("#998ec3", "#f1a340")) + 
  geom_vline(xintercept=2020, linewidth = 1) + 
  theme_bw() + 
  theme(legend.position ='none')


legend <- cowplot::get_legend(ggplot(data=globals[cadre == 'global_hrh_phys_sex'], aes(x=year_id,y=value,fill=factor(sex, levels = c('Females', 'Males')))) + 
                                geom_density(stat = "identity", alpha = 1) + 
                                labs(fill = "Sex") + 
                                guides(fill=guide_legend(nrow=1))+ 
                                scale_fill_manual(values=c("#f1a340","#998ec3")) + 
                                theme(legend.title.align=0.5))


pdf('~/test.pdf', height = 8*1.68, width = 8)
ggarrange(any, phys, nurses, legend, nrow=4)
dev.off()


any_diff <- melt(data.frame(diff(as.matrix(any_sex[location_id == 1 & year_id %in% c(1990,2022), 5:1004]))))
mean(any_diff$value)
quantile(any_diff$value, 0.025)
quantile(any_diff$value, 0.975)

phys_diff <- melt(data.frame(diff(as.matrix(phys_sex[location_id == 1 & year_id %in% c(1990,2022), 5:1004]))))
mean(phys_diff$value)
quantile(phys_diff$value, 0.025)
quantile(phys_diff$value, 0.975)

nurse_diff <- melt(data.frame(diff(as.matrix(nurses_sex[location_id == 1 & year_id %in% c(1990,2022), 5:1004]))))
mean(nurse_diff$value)
quantile(nurse_diff$value, 0.025)
quantile(nurse_diff$value, 0.975)

phys_diff_sea <- melt(data.frame(diff(as.matrix(phys_sex[location_id == 4 & year_id %in% c(1990,2022), 5:1004]))))
mean(phys_diff_sea$value)
quantile(phys_diff_sea$value, 0.025)
quantile(phys_diff_sea$value, 0.975)

nurses_diff_sea <- melt(data.frame(diff(as.matrix(nurses_sex[location_id == 4 & year_id %in% c(1990,2022), 5:1004]))))
mean(nurses_diff_sea$value)
quantile(nurses_diff_sea$value, 0.025)
quantile(nurses_diff_sea$value, 0.975)

any_diff_hi <- melt(data.frame(diff(as.matrix(any_sex[location_id == 64 & year_id %in% c(1990,2022), 5:1004]))))
mean(any_diff_hi$value)
quantile(any_diff_hi$value, 0.025)
quantile(any_diff_hi$value, 0.975)



