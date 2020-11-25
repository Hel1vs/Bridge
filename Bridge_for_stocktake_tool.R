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


# Figure 3 ----------------------------------------------------------------


# Figure 4 ----------------------------------------------------------------




# Figure 5 ----------------------------------------------------------------


# Figure 6 ----------------------------------------------------------------


# Figure 7 ----------------------------------------------------------------


# Figure 8 ----------------------------------------------------------------


# Figure 9 ----------------------------------------------------------------


# Figure 10 ---------------------------------------------------------------


# Figure 11 ---------------------------------------------------------------


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
