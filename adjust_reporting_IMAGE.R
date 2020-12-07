all=data.table(all)

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
