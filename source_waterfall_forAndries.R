# For Andries: 
# 1. Run sectoral waterfall script for individual regions
# 2. Create merged waterfall script showing regions and sectors in one (sectoral breakdown per region)


# 0. Load data ------------------------------------------------------------
setwd("~/disks/y/Project/E555163_COMMIT/Data/Database/Snapshots/Scripts/R/Bridge/Bridge")
config <- "config_bridge_IMAGE"
scencateg <- "scen_categ_bridge"
variables <- "variables_bridge" 
adjust <- "adjust_reporting_IMAGE"
addvars <- F
datafile <-"commit_bridge_image_native_20201207-101452" 
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

# fix stupid R mystery
all$period<-as.numeric(as.character(all$period))


# 1. Sectoral waterfalls per region ---------------------------------------
# Add non-CO2 emissions
if(!model%in%c("TIAM_Grantham_v3.2","PROMETHEUS")){
  nonco2=all[variable%in%c("Emissions|CH4","Emissions|N2O","Emissions|F-Gases")]
  nonco2$unit<-NULL
  nonco2=spread(nonco2,variable,value)
  nonco2=nonco2%>%mutate(`Emissions|Non-CO2`=((`Emissions|CH4`*25)+(`Emissions|N2O`*298/1000)+`Emissions|F-Gases`))
  nonco2=data.table(gather(nonco2,variable,value,c("Emissions|CH4","Emissions|N2O","Emissions|F-Gases","Emissions|Non-CO2")))
  nonco2=nonco2[variable=="Emissions|Non-CO2"]
  nonco2$unit<-"Mt CO2-equiv/yr"
  setcolorder(nonco2,colnames(all))
  all=rbind(all,nonco2)
}

# get rid of slash in model name - trouble with saving
cdata=all
cdata[model=="AIM/CGE"]$model<-"AIM-CGE"

# loop over models and regions TODO check why it stops after POLES in the full dataset - but now only running for IMAGE native
for(i in unique(cdata[Scope=="global"]$model)){
  for(j in unique(cdata[Scope=="global"&!region=="World"]$region)){
    model=i
    source("waterfall_bridge_loop.R")
  }
  }


# 2. Merged sectoral and regional waterfall -------------------------------

#use region waterfall as basis, build up each region's waterfall element as stack with sectoral reductions?
# better: use Christoph's original script for Elmar's paper
