# See also https://github.com/CD-LINKS/factsheet/blob/master/src/Indicators/Calculate_indicators_Dec2019.R
# For Mark's script that was originally used to populate the tool with CD-LINKS data

# Load data ---------------------------------------------------------------
setwd("~/disks/y/Project/E555163_COMMIT/Data/Database/Snapshots/Scripts/R/Bridge/Bridge")
config <- "config_bridge" 
scencateg <- "scen_categ_bridge"  
variables <- "variables_bridge"  
adjust <- "adjust_reporting_COMMIT"
addvars <- F
datafile <-"commit_bridge_compare_20201119-151637" 
source("load_data.R") 

# check whether there's only one scenario per category for each model
check=all[,list(unique(scenario)),by=c("model","Category")]
View(check)
check2=check[,list(length(unique(V1))),by=c("model","Category")]
View(check2)

# For models with NDCplus, NDCMCS is outdated so remove. For others, keep using NDCMCS until NDCplus is submitted
check3=check[Category=="NDCplus"]
View(check3)
all=all[!c(Category=="NDCMCS"&model%in%unique(check3$model))]

# Load functions and library for plotting
source("functions/calcVariable.R")
source("functions/plot_LineNationalScens.R")
source("functions/plotstyle.R")
library(grid)
library(gridExtra)
library(xlsx)

# fix stupid R mystery
all$period<-as.numeric(as.character(all$period))

# Figure 1 - emissions ----------------------------------------------------
# 001v_gst19.xlsx --> 
# variable, scenario, region, unit, source, statistic, years as columns
# Emissions|Kyoto Gases
# max, mean, median, min, ninetyp, tenp, value

kyoto = all[variable%in%c("Emissions|Kyoto Gases")]

# national models
kyotonational = kyoto[Scope=="national"]
kyotonational$scenario <- NULL
kyotonational$Baseline <- NULL
kyotonational$Scope <- NULL
kyotonational$model<-NULL
setnames(kyotonational,"Category","scenario")
kyotonational$source<-"COMMIT_national"
kyotonational$statistic<-"value"
setcolorder(kyotonational,c("variable","scenario","region","unit","source","statistic","value"))
kyotonational = spread(kyotonational,period,value)

# global models
kyotorange=kyoto[Scope=="global",list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T),mean=mean(value,na.rm=T),ninetyp=quantile(value,probs=0.9,na.rm=T),tenp=quantile(value,probs=0.1,na.rm=T)),by=c("Category","region","variable","unit","period")]
kyotorange=data.table(gather(kyotorange,statistic,value,c("min","max","median","mean","ninetyp","tenp")))
kyotorange=kyotorange[period%in%c(2005:2050)]
setnames(kyotorange,"Category","scenario")
kyotorange$source<-"COMMIT_global"
setcolorder(kyotorange,c("variable","scenario","region","unit","source","statistic","value"))
kyotorange=spread(kyotorange,period,value)

# together and save
emisrange=rbind(kyotonational,kyotorange)
write.xlsx2(emisrange,paste("Stocktaketool","/001v_gst20.xlsx",sep=""),sheetName="data",append=F,row.names = F)


# Figure 2 - emissions by GHG source ----------------------------------------------------------------
# 002v_gst19.xlsx --> 
# variable, scenario, region, unit, source, statistic, years as columns
# Emissions|CH4, CO2|AFOLU, N2O, F-Gases, CO2|Excl. AFOLU
# mean

emis = all[variable%in%c("Emissions|CH4","Emissions|CO2|AFOLU","Emissions|CO2","Emissions|N2O","Emissions|F-Gases")]

# Not needed anymore, after converting all units to MtCO2equiv
# units=emis[,list(unique(unit)),by="variable"]
# setnames(units,"V1","unit")
# unitnew=data.table("Emissions|CO2|Excl. AFOLU",unique(units[variable=="Emissions|CO2"]$unit))
# setnames(unitnew,"V1","variable")
# setnames(unitnew,"V2","unit")
# units=rbind(units,unitnew)

emis = spread(emis[,!c('unit'),with=FALSE],variable,value)
emis = emis%>%mutate(`Emissions|CO2|Excl. AFOLU`=`Emissions|CO2`-`Emissions|CO2|AFOLU`,
                     `Emissions|CH4`=`Emissions|CH4`*25,`Emissions|N2O`=`Emissions|N2O`*298/1000)
emis = data.table(gather(emis,variable,value,c("Emissions|CH4","Emissions|CO2|AFOLU","Emissions|CO2","Emissions|N2O","Emissions|F-Gases",`Emissions|CO2|Excl. AFOLU`)))
emis$unit<-"Mt CO2-equiv/yr"
#emis=merge(emis,units,by="variable")
emissions=emis[Scope=="global",list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T),mean=mean(value,na.rm=T),ninetyp=quantile(value,probs=0.9,na.rm=T),tenp=quantile(value,probs=0.1,na.rm=T)),by=c("Category","region","variable","unit","period")]
emissions=data.table(gather(emissions,statistic,value,c("min","max","median","mean","ninetyp","tenp")))
emissions=emissions[period%in%c(2005:2050)]
setnames(emissions,"Category","scenario")
emissions$source<-"COMMIT_global"
setcolorder(emissions,c("variable","scenario","region","unit","source","statistic","value"))
emissions=spread(emissions,period,value)

# save
write.xlsx2(emissions,paste("Stocktaketool","/002v_gst20.xlsx",sep=""),sheetName="data",append=F,row.names = F)


# Figure 3 - emissions by country----------------------------------------------------------------
# 003v_gst19.xlsx --> 
# variable, scenario, region, unit, source, statistic, years as columns
# Emissions|CH4, CO2|AFOLU, N2O, F-Gases, CO2|Excl. AFOLU, Kyoto Gases
# mean

emissionsc = rbind(emissions,kyotorange)
emisc=data.table(gather(emissionsc,year,value, c('2005','2010','2015','2020','2025','2030','2035','2040','2045','2050')))
emisc$year=as.numeric(emisc$year)
emisc=spread(emisc,region,value)
emisc=emisc%>%mutate(ROW=World-AUS-BRA-CAN-CHN-EU-IND-IDN-JPN-ROK-RUS-USA) #for now calculating ROW as world minus the shown countries, using the COMMIT countries to be shown
emisc=data.table(gather(emisc,region,value,c("World","ROW","AUS","BRA","CAN","CHN","EU","IND","IDN","JPN","ROK","RUS","USA","TUR","R5OECD90+EU","R5LAM","R5MAF","R5ASIA","R5REF")))
emisc=spread(emisc,year,value)
setcolorder(emisc,c("variable","scenario","region","unit","source","statistic"))

# save
write.xlsx2(emisc,paste("Stocktaketool","/003v_gst20.xlsx",sep=""),sheetName="data",append=F,row.names = F)


# Figure 4 - GHG/carbon intensity----------------------------------------------------------------
# 004v_gst19.xlsx --> 
# variable, scenario, region, unit, statistic, years as columns
# GHG Intensity of GDP|PPP, Carbon Intensity of GDP|PPP (excl AFOLU)
# %/yr
# max, mean, median, min, ninetyp, tenp

intensity = emis[variable%in%c("Emissions|CO2|Excl. AFOLU")]
setcolorder(intensity,colnames(all))
intensity=rbind(intensity,all[variable%in%c("Emissions|Kyoto Gases","GDP|PPP")])

intensity <- calcVariable(intensity,'`Carbon Intensity of GDP|PPP (excl AFOLU)` ~ `Emissions|CO2|Excl. AFOLU`/`GDP|PPP` ' , newUnit='kg CO2/$US 2010')
intensity <- calcVariable(intensity,'`GHG Intensity of GDP|PPP` ~ `Emissions|Kyoto Gases`/`GDP|PPP` ' , newUnit='kg CO2e/$US 2010')

intens=filter(intensity, Scope=="global", variable%in%c("Carbon Intensity of GDP|PPP (excl AFOLU)","GHG Intensity of GDP|PPP"),period<=2050)
intens <- arrange(intens, variable, Category, model, region, year)
write.table(intens,"Stocktaketool/stocktake_toolfigure4_GHG_intensity.csv", sep=";", row.names = FALSE)
intens_stat <- group_by(intens, Category, region, period, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                      median=median(value,na.rm=TRUE),
                                                                                      min=min(value, na.rm=TRUE),
                                                                                      max=max(value, na.rm=TRUE),
                                                                                      tenp=quantile(value, .10, na.rm=TRUE),
                                                                                      ninetyp=quantile(value, .90, na.rm=TRUE))
write.table(intens_stat, paste0("Stocktaketool/stocktake_toolGHG_intensity_PPP", "_stat.csv"), sep=";", row.names=F)

# calculate annual rate (based on five year periods)
intensrate <- group_by(intens, Category, model, region, unit, variable) %>% mutate(prev=lag(value)) %>% 
  mutate(value=(value/prev)^(1/5)-1) %>%
  select(variable, Category, region, unit, model, period, value) %>% 
  filter(period>=2010)
intensrate$value <- 100*intensrate$value
intensrate$unit <- "%/yr"
intensrate_stat <- group_by(intensrate, variable, Category, region, period, unit) %>% summarise(median=median(value,na.rm=TRUE),
                                                                                                        mean=mean(value,na.rm=TRUE),
                                                                                                        min=min(value, na.rm=TRUE),
                                                                                                        max=max(value, na.rm=TRUE),
                                                                                                        tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                        ninetyp=quantile(value, .90, na.rm=TRUE))
intensrate_stat <- gather(intensrate_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
intensrate_stat <- data.table(spread(intensrate_stat, period, value))
setnames(intensrate_stat,"Category","scenario")
write.xlsx2(intensrate_stat,paste("Stocktaketool","/004v_gst20.xlsx",sep=""),sheetName="data",append=F,row.names = F)

# Figure 5 - budget depletion----------------------------------------------------------------
#TODO update this for COMMIT

regions_indicators_PRIMAP_history <- c("BRA", "CAN", "CHN", "EU28", "IDN", "IND", "JPN", "RUS", "USA", "CAN", "TUR", "EARTH")
start_year_projections_fig5=2010
end_year_projections_fig5=2050
# Three steps 1) determine historical emissions (per region) for 1850-2015, 2) determine projections 3) In Excel, determine budgets
#1. Historical cumulative emissions 1990-2015
#tmp_data <- (PRIMAP_selec_CO2_1850_2015, 7:ncol(PRIMAP_selec_CO2_1850_2015),key='year',value=value)
# determine cumulative budgets for historic periods 1850-1990 and 1990-2015
# make dataframe structure for CO2 emissions equal to data used for figure
# 1850-1989
PRIMAP_selec_CO2_1850_2015 <- filter(PRIMAP, country %in% regions_indicators_PRIMAP_history, category=="CAT0", entity=="CO2")
PRIMAP_selec_CO2_1850_2015 <- select(PRIMAP_selec_CO2_1850_2015, scenario, country, category, entity, unit, num_range("X", 1850:start_year_projections_fig5))
PRIMAP_selec_CO2_1850_2015 <- cbind(PRIMAP_selec_CO2_1850_2015[1:5], PRIMAP_selec_CO2_1850_2015[, 6:ncol(PRIMAP_selec_CO2_1850_2015)]/1000)
# make dataframe structure for Kyoto emissions equal to data used for figure
PRIMAP_selec_CO2_1850_2015$country=str_replace_all(PRIMAP_selec_CO2_1850_2015$country,"EARTH","World")
PRIMAP_selec_CO2_1850_2015$country=str_replace_all(PRIMAP_selec_CO2_1850_2015$country,"EU28","EU")
PRIMAP_selec_CO2_1850_2015$scenario=str_replace_all(PRIMAP_selec_CO2_1850_2015$scenario,"HISTORY","History")
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, variable="Emissions|CO2")
colnames(PRIMAP_selec_CO2_1850_2015)[colnames(PRIMAP_selec_CO2_1850_2015)=="country"] <- "region"
PRIMAP_selec_CO2_1850_2015$region <- factor(PRIMAP_selec_CO2_1850_2015$region, levels=regions_indicators)
PRIMAP_selec_CO2_1850_2015$unit <- "Mt CO2"
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, source="PRIMAP")
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, statistic="value")
PRIMAP_selec_CO2_1850_2015 <- select(PRIMAP_selec_CO2_1850_2015, variable, scenario, region, unit, source, statistic, num_range("X", 1850:start_year_projections_fig5))
colnames(PRIMAP_selec_CO2_1850_2015) = gsub("X", "", colnames(PRIMAP_selec_CO2_1850_2015))

#2. Remaining emissions depending on budget (400, 1000, 1600)
d_cd_links_CO2 <- filter(all_cd_links, Scope=="global") %>%
  select(scenario, model, region, year, value, unit, variable)
d_cd_links_CO2 <- filter(d_cd_links_CO2, scenario %in% scens_indicators, region %in% regions_indicators, variable=="Emissions|CO2")
d_cd_links_CO2=data.table(d_cd_links_CO2)
yy=seq(start_year_projections_fig5,2100)
d_cd_links_CO2 = d_cd_links_CO2[,list(approx(x=year,y=value,xout=yy)$y,approx(x=year,y=value,xout=yy)$x),by=c('scenario','model','region', 'unit', 'variable')]
setnames(d_cd_links_CO2,"V1","value")
setnames(d_cd_links_CO2,"V2","year")
setcolorder(d_cd_links_CO2,c('scenario','model','region', 'year', 'value', 'unit', 'variable'))
d_cd_links_CO2 <- filter(d_cd_links_CO2, year>start_year_projections_fig5, year<=end_year_projections_fig5)
d_cd_links_CO2 <- spread(d_cd_links_CO2, key=year, value=value)

data_figure5_CO2emissions_model <- inner_join(PRIMAP_selec_CO2_1850_2015, d_cd_links_CO2, by=c('region'))
data_figure5_CO2emissions_model <- select(data_figure5_CO2emissions_model, variable.y, region, scenario.y, model, unit.y, num_range("", 1850:end_year_projections_fig5))
data_figure5_CO2emissions_model <- rename(data_figure5_CO2emissions_model, variable=variable.y, unit=unit.y)
data_figure5_CO2emissions_model <- rename(data_figure5_CO2emissions_model, scenario=scenario.y)
data_figure5_CO2emissions_model$region <- factor(data_figure5_CO2emissions_model$region, levels=regions_indicators)
data_figure5_CO2emissions_model$scenario <- factor(data_figure5_CO2emissions_model$scenario, levels=scens_indicators)

#data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_model, num_range("", start_year_projections_fig5:end_year_projections_fig5), key="year", value=value)
data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_model, 6:ncol(data_figure5_CO2emissions_model), key="year", value=value)
data_figure5_CO2emissions_stat <- group_by(data_figure5_CO2emissions_stat, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                                 median=median(value,na.rm=TRUE),
                                                                                                                                 min=min(value, na.rm=TRUE),
                                                                                                                                 max=max(value, na.rm=TRUE),
                                                                                                                                 tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
data_figure5_CO2emissions_stat <- spread(data_figure5_CO2emissions_stat, key=year, value=value)
data_figure5_CO2emissions_stat$statistic <- factor(data_figure5_CO2emissions_stat$statistic, level=stats_indicators)

# 3. Determine statistics for 2100 budgets
data_figure5_CO2budget <- filter(all_cd_links, Scope=="global", scenario %in% scens_indicators, region %in% regions_indicators, year==2100, variable=="Carbon budget")
data_figure5_CO2budget_stat <- group_by(data_figure5_CO2budget, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                      median=median(value,na.rm=TRUE),
                                                                                                                      min=min(value, na.rm=TRUE),
                                                                                                                      max=max(value, na.rm=TRUE),
                                                                                                                      tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                      ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_CO2budget_stat <- data_figure5_CO2budget_stat %>% ungroup() %>% select(-year)
data_figure5_CO2budget_stat <- gather(data_figure5_CO2budget_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=`2100`)
data_figure5_CO2budget_stat$statistic <- factor(data_figure5_CO2budget_stat$statistic, level=stats_indicators)

#4 Export to Excel (change later to R code) to translate annual CO2 emissions into budgets
write.table(data_figure5_CO2emissions_model , file="Indicators/data/stocktake_tool/figure5_CO2emissions_model.csv", sep=";", row.names = FALSE) 
write.table(data_figure5_CO2emissions_stat , file="Indicators/data/stocktake_tool/figure5_CO2emissions_stat.csv", sep=";", row.names = FALSE)
write.table(data_figure5_CO2budget_stat , file="Indicators/data/stocktake_tool/figure5_CO2budget_stat.csv", sep=";", row.names = FALSE)

#5 Import again in R (effort sharing)
data_figure5_model <- read.csv("Indicators/data/stocktake_tool/data_figure5_ToR.csv", header=TRUE, sep=";")
#data_figure5_stat <- data_figure5_model
colnames(data_figure5_model) = gsub("X", "", colnames(data_figure5_model))
data_figure5_model <- gather(data_figure5_model, 9:ncol(data_figure5_model), key="year", value=value)
data_figure5_model$value <- as.numeric(data_figure5_model$value)
data_figure5_stat <- group_by(data_figure5_model, variable, budget.scenario, budget.effort.sharing, scenario, region, unit, year) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                                                median=median(value,na.rm=TRUE),
                                                                                                                                                min=min(value, na.rm=TRUE),
                                                                                                                                                max=max(value, na.rm=TRUE),
                                                                                                                                                tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                                ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_stat <- gather(data_figure5_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
#data_figure5_stat <- filter(data_figure5_stat, year>=1990, year<=2050)
data_figure5_stat$value <- as.numeric(data_figure5_stat$value)
data_figure5_stat <- spread(data_figure5_stat, key=year, value=value)
data_figure5_stat$statistic <- factor(data_figure5_stat$statistic, level=stats_indicators)
colnames(data_figure5_stat) = gsub("X", "", colnames(data_figure5_stat))
data_figure5_stat_fig <- select(data_figure5_stat, -(num_range("", 1850:1989))) %>% as.data.frame()
write.table(data_figure5_stat_fig , file="Indicators/data/stocktake_tool/figure5_stat.csv", sep=";", row.names = FALSE)


# Figure 6 - innovation----------------------------------------------------------------
all <- calcVariable(all,'`Renewables Share|Incl. Hydro and Nuclear` ~ ( `Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind` + `Secondary Energy|Electricity|Nuclear` + `Secondary Energy|Electricity|Hydro` + `Secondary Energy|Electricity|Biomass` + `Secondary Energy|Electricity|Geothermal`) / `Secondary Energy|Electricity` * 100 ' , newUnit='%')
#TODO update this for COMMIT
# calculation of nonfossilresbuildings:
# # x=%-REN electricity, y=electricity fuel use, x.x = bio fuel use, y.y = total fuel use
# NonFossilResBuildingsShare <- NonFossilResBuildingsShare %>% mutate(value=(0.01*value.x*value.y+value.x.x)/value.y.y) %>% select(year, region, value, population_group)
# NonFossilResBuildingsShare$value <- 100*NonFossilResBuildingsShare$value
# NonFossilResBuildingsShare <- mutate(NonFossilResBuildingsShare, unit= "%")  %>% as.data.frame()

# # x=%-NonFossil electricity, y=electricity fuel use, x.x = bio fuel use, y.y = total fuel use
# NonFossilTransportShare <- NonFossilTransportShare %>% mutate(value=(0.01*value.x*value.y+value.x.x)/value.y.y) %>% select(year, region, value, travel_mode, type)
# NonFossilTransportShare$value <- 100*NonFossilTransportShare$value
# NonFossilTransportShare <- mutate(NonFossilTransportShare, unit= "%")  %>% as.data.frame() 

innovation = all[variable%in%c("Renewables Share|Incl. Hydro and Nuclear","NonFossilResBuildingsShare","NonFossilTransportShare")&period%in%c(2010:2050)]
innovation[variable=="Renewables Share|Incl. Hydro and Nuclear"]$variable<-"Final Energy|Electricity|Non-fossil share"
innovation[variable=="NonFossilResBuildingsShare"]$variable<-"Final Energy|Residential buildings|Non-fossil share"
innovation[variable=="NonFossilTransportShare"]$variable<-"Final Energy|Transport|Non-fossil share"

# data_figure6 <- mutate(data_figure6, source="PBL")
# data_figure6 <- mutate(data_figure6, statistic="value")
# data_figure6 <- select(data_figure6, variable, scenario, region, unit, source, statistic, year, value)
# data_figure6 <- spread(data_figure6, key=year, value=value)

write.xlsx2(innovation,paste("Stocktaketool","/006v_gst20.xlsx",sep=""),sheetName="data",append=F,row.names = F)

# Figure 7 - investments----------------------------------------------------------------
# Investments based on McCollum - no R action needed? Or try to deliver COMMIT data?

# Figure 8 - air pollution ----------------------------------------------------------------
data_figure8 <- filter(all, Scope=="global", variable%in%c("Emissions|Sulfur", "Emissions|OC", "Emissions|BC"))
d_COMMIT_Air_pollution <- filter(data_figure8, year>=2010, year<=2050)
d_COMMIT_Air_pollution_stat <- group_by(d_COMMIT_Air_pollution, Category, region, period, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                          median=median(value,na.rm=TRUE),
                                                                                                                          min=min(value, na.rm=TRUE),
                                                                                                                          max=max(value, na.rm=TRUE),
                                                                                                                          tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                          ninetyp=quantile(value, .90, na.rm=TRUE))
d_COMMIT_Air_pollution_stat <- gather(d_COMMIT_Air_pollution_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_COMMIT_Air_pollution_stat <- spread(d_COMMIT_Air_pollution_stat, key=period, value=value)
d_COMMIT_Air_pollution_stat <- mutate(d_COMMIT_Air_pollution_stat, source="COMMIT")
d_COMMIT_Air_pollution_stat <- data.table(select(d_COMMIT_Air_pollution_stat, variable, Category, region, unit, source, statistic, everything()))
write.xlsx2(d_COMMIT_Air_pollution_stat,paste("Stocktaketool","/008v_gst20.xlsx",sep=""),sheetName="data",append=F,row.names = F)


# Figure 9 - land cover----------------------------------------------------------------
data_figure9 <- filter(all, Scope=="global", value>0, variable%in%c("Land Cover|Forest"))
data_figure9 <- group_by(data_figure9, Category, model, region, variable) %>% 
  mutate(growth=value-lag(value), perc_growth=(value/lag(value))^(1/5)-1) %>% filter(period>=2010)
d_COMMIT_Deforestation <- filter(data_figure9, period>=2010, period<=2050)
d_COMMIT_Deforestation_stat <- group_by(d_COMMIT_Deforestation, Category, region, period, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                          median=median(value,na.rm=TRUE),
                                                                                                                          min=min(value, na.rm=TRUE),
                                                                                                                          max=max(value, na.rm=TRUE),
                                                                                                                          tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                          ninetyp=quantile(value, .90, na.rm=TRUE))
d_COMMIT_Deforestation_stat <- gather(d_COMMIT_Deforestation_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_COMMIT_Deforestation_stat <- spread(d_COMMIT_Deforestation_stat, key=period, value=value)
d_COMMIT_Deforestation_stat <- mutate(d_COMMIT_Deforestation_stat, source="COMMIT")
d_COMMIT_Deforestation_stat <- data.table(select(d_COMMIT_Deforestation_stat, variable, Category, region, unit, source, statistic, everything()))
setnames(d_COMMIT_Deforestation_stat,"Category","scenario")
write.xlsx2(d_COMMIT_Deforestation_stat,paste("Stocktaketool","/009v_gst20.xlsx",sep=""),sheetName="data",append=F,row.names = F)

# Figure 10 - policy 1---------------------------------------------------------------
# Policy - based on Iacobuta et al., so no R action needed

# Figure 11 - policy 2---------------------------------------------------------------
# Policy - based on Iacobuta et al., so no R action needed

# Figure 12 - peak and net zero year --------------------------------------
# 012v_gst19.xlsx --> 
# scenario, region, year, variable, unit, statistic, value, source
# Peak Year|CO2, Peak Year|Kyoto Gases, Zero Emissions Year|CO2, Zero Emissions Year|Kyoto Gases
# max, mean, median, min, ninetyp, tenp

# peak year
ghg = all[variable%in%c("Emissions|Kyoto Gases","Emissions|CO2")]
peak = ghg[,list(value=as.numeric(period[which.max(value)])),by=c('scenario','Category','Baseline','model','region','variable','unit','Scope')]
peakrange=peak[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T),mean=mean(value,na.rm=T),ninetyp=quantile(value,probs=0.9,na.rm=T),tenp=quantile(value,probs=0.1,na.rm=T)),by=c("Category","region","variable","unit")]
peakrange=data.table(gather(peakrange,statistic,value,c("min","max","median","mean","ninetyp","tenp")))
setnames(peakrange,"Category","scenario")
peakrange$unit<-"Year"
peakrange$variable=str_replace_all(peakrange$variable,"Emissions","Peak year")
peakrange$source<-"COMMIT"
write.xlsx2(peakrange,paste("Stocktaketool","/012v_gst20.xlsx",sep=""),sheetName="Peak Year",append=F,row.names = F)

# net zero year
poy=ghg[!duplicated(ghg[,list(model,Category,region,variable),with=TRUE]),!c('value','period'),with=FALSE] #"Scope","Baseline","scenario"
poy=merge(poy,ghg[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
setnames(poy,"V1","value")
#poy=na.omit(poy)
poyrange=poy[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T),mean=mean(value,na.rm=T),ninetyp=quantile(value,probs=0.9,na.rm=T),tenp=quantile(value,probs=0.1,na.rm=T)),by=c("Category","region","variable","unit")]
poyrange=data.table(gather(poyrange,statistic,value,c("min","max","median","mean","ninetyp","tenp")))
setnames(poyrange,"Category","scenario")
poyrange$unit<-"Year"
poyrange$variable=str_replace_all(poyrange$variable,"Emissions","Zero Emissions Year")
poyrange$source<-"COMMIT"
write.xlsx2(poyrange,paste("Stocktaketool","/012v_gst20.xlsx",sep=""),sheetName="Zero Emissions Year",append=T,row.names = F)
