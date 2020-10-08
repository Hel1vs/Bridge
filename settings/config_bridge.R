######################################################
################ Configuration file         ##########
################ with user defined settings ##########
######################################################

cfg <- list()

######################################################
################ General settings ####################
######################################################

# csv files that contain input data and will be read (only file name, without folder and extension)
cfg$infile    <- "commit_bridge_compare_20200213-103752"

# Name of corresponding national model(s)
cfg$models_nat <- c("AIM/Enduse[Japan]","BLUES","GCAM_Canada","PRIMES_V1","AIM/CGE[Korea]",
                    "IPAC-AIM/technology V1.0","India MARKAL","DDPP Energy","DDPP AFOLU","GCAM-USA_COMMIT","PECE V2.0", #these still need to submit new/update/ more scenarios (heard from) #GCAM-USA_CDLINKS
                    "RU-TIMES 3.2","GEM-E3_EU","TIMES-AUS") # these still need to submit (not heard from)

# Name of directory plots are stored in
cfg$outdir    <- "output"

# file format for plots
cfg$format <- ".png"

######################################################
############### Define plot styles ###################
######################################################

# Color palette
cfg$colors <- c("#0072B2","#000000","#D55E00","#33cc00","#cc0000","#999999")

# Shapes TODO add * when national team submits
cfg$man_shapes=c("AIM V2.1" = 2, #global
                 "AIM/CGE" = 2, #SSPs
                 "*AIM/CGE[Korea]" = 1, #national
                 "*AIM/Enduse[Japan]" = 1, #national
                 "*BLUES" = 1, #national
                 "COPPE-COFFEE 1.0" = 3, #global
                 "*GCAM-USA_COMMIT" = 1, #national
                 "GCAM4" = 4, #SSPs
                 "*GCAM_Canada" = 1, #national
                 "GCAM_LAMP" = 4, #LAMP
                 "GEM-E3" = 5, #global
                 "GEM-E3_IPTS_World" = 5, #MILES
                 "IMAGE 3.0" = 6, #global
                 "*IPAC-AIM/technology V1.0" = 1, #national
                 "*India MARKAL" = 1, #national
                 "MESSAGE-GLOBIOM_1.0" = 7, #SSPs
                 "MESSAGEix-GLOBIOM_1.0" = 7, #global
                 "*PECE V2.0" = 2, #national
                 "POLES GECO2019" = 8, #GECO
                 "*PRIMES_V1" = 1, #national
                 "PROMETHEUS" = 11, # global
                 "REMIND 1.7" = 9, #SR1.5
                 "REMIND_MAGPIE" = 9, #SSPs
                 "REMIND-MAgPIE 1.7-3.0" = 9, #global
                 "*RU-TIMES 3.2" = 1, #national
                 "TIAM_Grantham_v3.2" = 5, #global
                 "*TIMES-AUS" = 1, #national
                 "WITCH-GLOBIOM" = 10, #SSPs
                 "WITCH2013" = 10, #MILES
                 "WITCH2016" = 10, #CD-LINKS
                 "WITCH 5.0" = 10) #COMMIT version global

# Linestyles TODO add * when national team submits
cfg$man_lines=c("AIM V2.1" = "dashed", #global
                 "AIM/CGE" = "dashed", #SSPs
                 "*AIM/CGE[Korea]" = "solid", #national
                 "*AIM/Enduse[Japan]" = "solid", #national
                 "*BLUES" = "solid", #national
                 "COPPE-COFFEE 1.0" = "dotted", #global
                 "*GCAM-USA_COMMIT" = "solid", #national
                 "GCAM4" = "longdash", #SSPs
                 "*GCAM_Canada" = "solid", #national
                 "GCAM_LAMP" = "longdash", #LAMP
                 "GEM-E3" = "twodash", #global
                 "GEM-E3_IPTS_World" = "twodash", #MILES
                 "IMAGE 3.0" = "dotdash", #global
                 "*IPAC-AIM/technology V1.0" = "solid", #national
                 "*India MARKAL" = "solid", #national
                 "MESSAGE-GLOBIOM_1.0" = "longdash", #SSPs
                 "MESSAGEix-GLOBIOM_1.0" = "longdash", #global
                "*PECE V2.0" = "dashed", #national
                 "POLES GECO2019" = "dashed", #GECO
                 "*PRIMES_V1" = "solid", #national
                 "PROMETHEUS" = "longdash", # global
                 "REMIND 1.7" = "twodash", #SR1.5
                 "REMIND_MAGPIE" = "twodash", #SSPs
                 "REMIND-MAgPIE 1.7-3.0" = "twodash", #global
                 "*RU-TIMES 3.2" = "solid", #national
                 "TIAM_Grantham_v3.2" = "dashed", #global
                 "*TIMES-AUS" = "solid", #national
                 "WITCH-GLOBIOM" = "dotdash", #SSPs
                 "WITCH2013" = "dotdash", #MILES
                 "WITCH2016" = "dotdash", #CD-LINKS
                 "WITCH 5.0" = "dotdash") #COMMIT version global

cfg$r <- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS","USA", "World","CAN","AUS","ROK","IDN","TUR",
            "R5ASIA","R5LAM","R5REF","R5OECD90+EU","R5MAF")
