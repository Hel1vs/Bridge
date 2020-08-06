# Adjust regions ----------------------------------------------------------
# MESSAGE and COPPE adjust region EU (exclude Turkey)
all=data.table(all)
# all[Category=="NoPOL"]$Baseline <- str_replace_na(all[Category=="NoPOL"]$Baseline,"-")
# tmp1 <- all[model%in%c("MESSAGEix-GLOBIOM_1.0","COPPE-COFFEE 1.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
#                                                                                      "Emissions|CO2|Energy|Demand|Residential and Commercial",
#                                                                                      "Emissions|CO2|Energy|Demand|Transportation",
#                                                                                      "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
#                                                                                      "Emissions|F-Gases",
#                                                                                      "Emissions|CO2|Energy|Demand|Industry",
#                                                                                      "Emissions|CO2|Industrial Processes") & region%in%c("EU")]
# tmp1 <- tmp1[!c(model=="COPPE-COFFEE 1.0"&variable%in%c("Emissions|F-Gases","Emissions|CO2|Energy|Demand|Residential and Commercial"))]
# tmp2 <- all[model=="IMAGE 3.0" & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
#                                                  "Emissions|CO2|Energy|Demand|Residential and Commercial",
#                                                  "Emissions|CO2|Energy|Demand|Transportation",
#                                                  "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
#                                                  "Emissions|F-Gases",
#                                                  "Emissions|CO2|Energy|Demand|Industry",
#                                                  "Emissions|CO2|Industrial Processes")  
#             & region=="TUR"]
# tmp3 <- tmp2[!c(variable%in%c("Emissions|F-Gases","Emissions|CO2|Energy|Demand|Residential and Commercial"))]
# tmp2$model<-"MESSAGEix-GLOBIOM_1.0"
# tmp3$model<-"COPPE-COFFEE 1.0"
# tmp1 <- rbind(tmp1,tmp2,tmp3)
# tmp1=spread(tmp1,region,value)
# tmp1 = na.omit(tmp1)
# tmp1 = tmp1%>%mutate(EU=EU-TUR)
# tmp1=gather(tmp1,region,value,c(EU,TUR))
# tmp1=data.table(tmp1)
# tmp1=tmp1[region=="EU"]
# all <- all[!c(model%in%c("MESSAGEix-GLOBIOM_1.0","COPPE-COFFEE 1.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
#                                                                                        "Emissions|CO2|Energy|Demand|Residential and Commercial",
#                                                                                        "Emissions|CO2|Energy|Demand|Transportation",
#                                                                                        "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
#                                                                                        "Emissions|F-Gases",
#                                                                                        "Emissions|CO2|Energy|Demand|Industry",
#                                                                                        "Emissions|CO2|Industrial Processes") & region%in%c("EU"))]
# all<-rbind(all,tmp1)

# MESSAGE change region USA (exclude Canada)
# tmp1 <- all[model%in%c("MESSAGEix-GLOBIOM_1.0","IMAGE 3.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
#                                                                               "Emissions|CO2|Energy|Demand|Residential and Commercial",
#                                                                               "Emissions|CO2|Energy|Demand|Transportation",
#                                                                               "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
#                                                                               "Emissions|F-Gases",
#                                                                               "Emissions|CO2|Energy|Demand|Industry",
#                                                                               "Emissions|CO2|Industrial Processes") & region%in%c("USA","CAN")]
# tmp1=tmp1[!c(model=="IMAGE 3.0"&region=="USA")]
# tmp1$model<-"MESSAGEix-GLOBIOM_1.0"
# tmp1=spread(tmp1,region,value)
# tmp1 = na.omit(tmp1)
# tmp1 = tmp1%>%mutate(USA=USA-CAN)
# tmp1=gather(tmp1,region,value,c(USA,CAN))
# tmp1=data.table(tmp1)
# tmp1=tmp1[region=="USA"]
# all <- all[!c(model%in%c("MESSAGEix-GLOBIOM_1.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
#                                                                     "Emissions|CO2|Energy|Demand|Residential and Commercial",
#                                                                     "Emissions|CO2|Energy|Demand|Transportation",
#                                                                     "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
#                                                                     "Emissions|F-Gases",
#                                                                     "Emissions|CO2|Energy|Demand|Industry",
#                                                                     "Emissions|CO2|Industrial Processes") & region%in%c("USA"))]
# all<-rbind(all,tmp1)

# To do/decide:
# Removing MESSAGE model for India, as MESSAGE has South Asia (including Afghanistan, Bangladesh, Bhutan, Maldives, Nepal, Pakistan, Sri Lanka), not India separately
India = all[region=="IND"]
India = India[!model=="MESSAGE V.4"]
all=rbind(subset(all, !region=="IND"),India)

# Removing MESSAGE model for China, as MESSAGE has CPA (including Cambodia, Hong Kong, Korea, Lao, Macau, Mongolia, Taiwan, Viet Nam), not China separately
# China = all[region=="CHN"]
# China = China[!model=="MESSAGE V.4"]
# all=rbind(subset(all, !region=="CHN"),China)


# Add total as sum of sub-categories, when missing ------------------------
tmp1 <- all[model %in% setdiff(unique(all[variable=="Agricultural Production|Energy|Crops"]$model),unique(all[variable=="Agricultural Production|Energy"]$model)) &
              variable %in% c("Agricultural Production|Energy|Crops","Agricultural Production|Energy|Residues")]
if(dim(tmp1)[1]!=0 & "Agricultural Production|Energy|Crops" %in% unique(tmp1$variable)){
  tmp=spread(tmp1,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Agricultural Production|Energy`=`Agricultural Production|Energy|Crops` + `Agricultural Production|Energy|Residues`)
  tmp1=gather(tmp, variable, value, c(`Agricultural Production|Energy`,`Agricultural Production|Energy|Crops`, `Agricultural Production|Energy|Residues`))
  tmp1=data.table(tmp1)
  tmp1=tmp1[variable=="Agricultural Production|Energy"]
  all <- rbind(all,tmp1)} 

tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Energy|Demand|Residential"]$model),unique(all[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]$model)) &
              variable %in% c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
if(dim(tmp1)[1]!=0 & "Emissions|CO2|Energy|Demand|Residential" %in% unique(tmp1$variable)){
  tmp=spread(tmp1,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Emissions|CO2|Energy|Demand|Residential and Commercial`=`Emissions|CO2|Energy|Demand|Residential` + `Emissions|CO2|Energy|Demand|Commercial`)
  tmp1=gather(tmp, variable, value, c(`Emissions|CO2|Energy|Demand|Residential and Commercial`,`Emissions|CO2|Energy|Demand|Residential`, `Emissions|CO2|Energy|Demand|Commercial`))
  tmp1=data.table(tmp1)
  tmp1=tmp1[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]
  all <- rbind(all,tmp1)} 

# Fix AUS reporting -------------------------------------------------------
otherAUS=all[region%in%c("AUS")&variable=="Emissions|CO2|Energy|Supply"&model=="TIMES-AUS"]
otherAUS$value<-0
otherAUS$variable<-"Emissions|CO2|Energy|Demand|Other Sector"
all=rbind(all,otherAUS)

#data[Region=="AUS"&Variable=="Emissions|CH4|AFOLU"]$value=data[Region=="AUS"&Variable=="Emissions|CH4|AFOLU"]$value/25
CO2AUS=all[region=="AUS"&model=="TIMES-AUS"&variable%in%c("Emissions|CO2|AFOLU","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Other Sector","Emissions|CO2|Energy|Supply","Emissions|CO2|Industrial Processes")]
CO2AUS=spread(CO2AUS,variable,value)
CO2AUS=CO2AUS%>%mutate(`Emissions|CO2`=`Emissions|CO2|AFOLU`+`Emissions|CO2|Energy|Demand|Industry`+`Emissions|CO2|Energy|Demand|Residential and Commercial`+`Emissions|CO2|Energy|Demand|Transportation`+`Emissions|CO2|Energy|Demand|Other Sector`+`Emissions|CO2|Energy|Supply`) # +`Emissions|CO2|Industrial Processes`
CO2AUS=data.table(gather(CO2AUS,variable,value,c("Emissions|CO2|AFOLU", "Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Other Sector","Emissions|CO2|Energy|Supply",
                                                 "Emissions|CO2"))) #,"Emissions|CO2|Industrial Processes"
CO2AUS=CO2AUS[variable=="Emissions|CO2"]
all=rbind(all,CO2AUS)

nucelecaus=all[region%in%c("AUS")&model%in%c("TIMES-AUS")&variable=="Secondary Energy|Electricity"]
nucelecaus$value<-0
nucelecaus$variable<-"Secondary Energy|Electricity|Nuclear"
all=rbind(all,nucelecaus)



# Add 2015 for MESSAGE ----------------------------------------------------
mesg=spread(all[period%in%c(2010,2020)&model%in%c("MESSAGEix-GLOBIOM_1.0","TIAM_Grantham_v3.2")],period,value)
mesg = mesg%>%mutate(`2015`=(`2010`+`2020`)/2)
mesg = data.table(gather(mesg,period,value,c(`2010`,`2015`,`2020`)))
mesg = mesg[period==2015]
all = rbind(all,mesg)



# Add F-gases for COFFEE --------------------------------------------------
FCOF=all[variable=="Emissions|Kyoto Gases"&model=="COPPE-COFFEE 1.0"]
FCOF$value<-0
FCOF$variable<-"Emissions|F-Gases"
all=rbind(all,FCOF)



# Copy NDC as CurPol for PECE ---------------------------------------------
PECE=all[Category=="NDCMCS"&model=="PECE V2.0"]
PECE$Category<-"CurPol"
all=rbind(all,PECE)

