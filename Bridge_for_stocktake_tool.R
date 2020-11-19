# Load data ---------------------------------------------------------------
setwd("~/disks/y/Project/E555163_COMMIT/Data/Database/Snapshots/Scripts/R/Bridge/Bridge")
config <- "config_bridge" 
scencateg <- "scen_categ_bridge"  
variables <- "variables_bridge"  
adjust <- "adjust_reporting_COMMIT"
addvars <- F
datafile <-"commit_bridge_compare_20201118-093614" 
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

# Figure 1 - emissions ----------------------------------------------------


# Figure 2 ----------------------------------------------------------------


# Figure 3 ----------------------------------------------------------------


# Figure 4 ----------------------------------------------------------------



# Figure 12 - peak and net zero year --------------------------------------
# 012v_gst19.xlsx --> 
# scenario, region, year, variable, unit, statistic, value, source
# Peak Year|CO2, Peak Year|Kyoto Gases, Zero Emissions Year|CO2, Zero Emissions Year|Kyoto Gases



