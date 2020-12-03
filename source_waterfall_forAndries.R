# For Andries: 
# 1. Run sectoral waterfall script for individual regions
# 2. Create merged waterfall script showing regions and sectors in one (sectoral breakdown per region)

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

# loop over models and regions TODO check why it stops after POLES
for(i in unique(cdata[Scope=="global"]$model)){
  for(j in unique(cdata[Scope=="global"&!region=="World"]$region)){
    model=i
    source("waterfall_bridge_loop.R")
  }
  }


# 2. Merged sectoral and regional waterfall -------------------------------

#use region waterfall as basis, build up each region's waterfall element as stack with sectoral reductions?
