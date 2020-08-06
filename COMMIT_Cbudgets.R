setwd("~/disks/local/COMMIT_bridge")
config <-"config_COMMIT"
scencateg <- "scen_categ_COMMIT"
variables <- "variables_xCut"
adjust <- "adjust_reporting_COMMIT"
addvars <- F
datafile <-"commit_cd-links_compare_20191015-114544"
source("load_data.R")

source("functions/calcBudget_2015.R")
all <- calcBudget_2015(all,'Emissions|CO2','Carbon budget_2015')
all <- calcBudget_2015(all,'Emissions|CO2|Energy and Industrial Processes','Carbon budget_2015|Energy and Industry')
all <- calcBudget_2015(all,'Emissions|CO2|Energy','Carbon budget_2015|Energy')

budgets = all[variable%in%c("Carbon budget_2015","Carbon budget_2015|Energy and Industry","Carbon budget_2015|Energy")&
                Category%in%c("2030_low","2020_low")&period%in%c(2050,2100)]
budgets = spread(budgets,variable,value)
write.csv(budgets,paste("output","/NationalCbudgets.csv",sep=""))
