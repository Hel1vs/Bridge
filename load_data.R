#############################################################
########## Main file that loads and processes data ##########
########## and produces the national fact sheet    ##########
#############################################################

# INPUT needed: variables (your settings/variables csv file), scencateg (your settings/scenario categorisation file), 
# config (your settings/configuration file), adjust (your adjust_reporting file)

library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all
#library(tidyverse)

#set working directory for R right if it is not by default (it is the right one by default if you open Rstudio by clicking on this main.R file)
#setwd("~/disks/local/factsheet/src")

if(!exists("variables")){
  stop("Please specify 'variables', i.e. the name of the settings/variables_....csv file you want to use (without the extension)")
}
if(!exists("config")){
  stop("Please specify 'config', i.e. the name of the settings/config_....R file you want to use (without the extension)")
}
if(!exists("scencateg")){
  stop("Please specify 'scencateg', i.e. the name of the settings/scenario categorisation_....csv file you want to use (without the extension)")
}
if(!exists("adjust")){
  stop("Please specify 'adjust', i.e. the name of the src/adjust_reporting_....R file you want to use (without the extension)")
}

if(!exists("addvars")){
  stop("Please set flag 'addvars' to TRUE or FALSE, i.e. whether you want to add calculated indicators from reported variables (takes some time)")
}

#source configuration file for region-specific data
source(paste("settings/",config,".R",sep=""))

#overwrite file to be used for analysis
cfg$infile<-datafile

#source function for factorizing data frames
source("functions/factor.data.frame.R")
# source functions process_data() and add_variables()
source("functions/data_processing.R")
#source function overwrite for rbinding a dataframe with another, removing duplicates
source("functions/overwrite.R")
#source file with plot functions
source("functions/plot_functions.R")

# flag to process data, reprocess even if .._reg_proc.RData file is available (i.e. overwrite existing RData)
# set to true if you always want data re-processed
b.procdata = T

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

# keep old version of data
if (exists('all')) {all_old <- all}

#############################################################
####################### Load data ###########################
#############################################################
if(length(cfg$r)>0){reg=""}else{reg=paste0("_",cfg$r)}
#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, please set b.procdata = TRUE)
if (file.exists(paste0("data/",cfg$infile,reg,"_proc.Rdata")) & !b.procdata) {
  cat("Loading processed data from file", paste0("data/",cfg$infile,".Rdata"),"\n",
      "set b.procdata flag and re-run if you want to do the data processing again", "\n")
  load(paste0("data/",cfg$infile,reg,"_proc.Rdata"))
  Sys.sleep(2)#give everybody the chance to read the above message
} else {
  
  if (file.exists(paste0("data/",cfg$infile,".Rdata"))) {
    cat("Loading file", paste0("data/",cfg$infile,".Rdata"),"\n")
    load(paste0("data/",cfg$infile,".Rdata"))
  } else {
    cat("Reading data from file",paste0("data/",cfg$infile,".csv"),"\n")
    all <- invisible(fread(paste0("data/",cfg$infile,".csv"),header=TRUE))
    save("all",file = paste0("data/",cfg$infile,".Rdata"))
  }

  # keep original data
  all_import <- all  
  
  # Add new column "Category" and fill with name according to scenario-to-Category-mapping in "scens"
  scens <- fread(paste("settings/",scencateg,".csv",sep=""), header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]
  
  #reduce size of the data frame
  vars <- fread(paste("settings/",variables,".csv",sep=""),header=TRUE,stringsAsFactors=FALSE,sep='\n')
  all  <- all[VARIABLE %in% vars$variable & REGION %in% cfg$r]
  

  #############################################################
  ####################### Process data ########################
  #############################################################
  cat("Processing data\n")
  
  #delete previous scenario versions, only use the latest
  all <- all[!c(MODEL %in% c("REMIND-MAgPIE 1.7-3.0","PROMETHEUS","MESSAGEix-GLOBIOM_1.0","PRIMES_V1","TIAM_Grantham_v3.1")&SCENARIO%in%c("2Deg2020","2Deg2030","BAU","Bridge","CurPol","GPP","NDCMCS"))]
  all <- all[!c(MODEL %in% c("IMAGE 3.0","AIM/Enduse[Japan]","AIM/CGE","PROMETHEUS","PRIMES_V1")&SCENARIO%in%c("2Deg2020_V1","2Deg2030_V1","BAU_V1","Bridge_V1","CurPol_V1","GPP_V1","NDCMCS_V1"))]
  all <- all[!c(MODEL %in% c("IMAGE 3.0","PROMETHEUS","AIM/Enduse[Japan]","BLUES")&SCENARIO%in%c("2Deg2020_V2","2DEG2020_V2","2Deg2030_V2","BAU_V2","Bridge_V2","CurPol_V2","GPP_V2","NDCMCS_V2",
                                                        "2Deg2020_V3","2Deg2030_V3","BAU_V3","Bridge_V3","CurPol_V3","GPP_V3","NDCMCS_V3"))]
  all <- all[!c(MODEL %in% c("TIAM_Grantham_v3.2")& SCENARIO %in%c("BAU_v4","CurPol_v4","NDCPlus_v4","GPP_v4","Bridge_v4","2Deg2020_v4","2Deg2030_v4"))]
  all <- all[!c(MODEL %in% c("PRIMES_V1")& SCENARIO %in%c("GPP_v4","Bridge_v4"))]
  all <- all[!c(MODEL %in% c("DDPP Energy")&SCENARIO%in%c("BAU"))]
  #CD-LINKS scenarios used until new ones are submitted for GCAM-USA, India MARKAL, RU-TIMES, so delete for the other models that also have CD-LINKS scenarios
  all <- all[!c(MODEL%in%c("AIM/Enduse[Japan]","BLUES","COPPE-COFFEE 1.0","IMAGE 3.0","REMIND-MAgPIE 1.7-3.0","MESSAGEix-GLOBIOM_1.0") & SCENARIO%in%c("NoPOL","NPi2020_low","INDC2030_low"))] #"CD-LINKS-NoPOL_V3","CD-LINKS-NoPOL_V4","NoPOL","CD-LINKS-NPi_V3","CD-LINKS-NPi_V4","NPi","CD-LINKS-INDC_V4","INDC","CD-LINKS-NPi2020_low_V3","CD-LINKS-NPi2020_low_V4","NPi2020_low","CD-LINKS-INDC2030_low_V3","CD-LINKS-INDC2030_low_V4","INDC2030_low"
  
    
  #### from raw wide format to long format with additional columns
  cat("- change format data\n")
  all <- process_data(all,scens)

  #re-factorize all character and numeric columns
  cat("- factorise data\n")
  all <- factor.data.frame(all)
 
  ###### Manual changes before addition of calculated variables  
  cat("- make adjustments to data\n")
  source(paste(adjust,".R",sep=""))  
 
  #### add variables
  if(addvars){
    cat("- add variables to data\n")
    all <- add_variables(all,scens)
  }

  #set scope to "national" for national models
  all[all$model %in% cfg$models_nat,]$Scope <- "national"
  #special case GEM-E3: national model for EU, global for other regions
  all[model=="GEM-E3"&region!="EU"]$Scope<-"global"
  #change model name for national models, so that they appear first
  all<-data.table(all)
  if(!substr(cfg$models_nat[1],1,1)=="*"){
    all[model %in% cfg$models_nat]$model <- paste0("*",all[model %in% cfg$models_nat]$model)
    cfg$models_nat <- paste0("*",cfg$models_nat)
  }

  #get rid of Historical duplicates
  #all <- all[Category!="Historical"]
  
  #save country specific file with processed data
  save("all",file = paste0("data/",cfg$infile,reg,"_proc.Rdata"))
  
}# end if-else: process data
