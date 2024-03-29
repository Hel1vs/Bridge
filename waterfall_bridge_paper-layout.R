#stackedbar waterfall plot - adjusted from the script by Christoph Bertram at PIK

factor.data.frame <- function(df) {
  for (c in 1:ncol(df)) 
    if (is.factor(df[[c]]) | typeof(df[[c]]) == "character") {
      df[[c]] <- factor(df[[c]])
      
      if ("" %in% levels(df[[c]]))
        levels(df[[c]])[levels(df[[c]]) == ""] <- NA
    }
  return(df)
}

# vars <- c(    "Emissions|CO2|Energy|Supply",
#                 "Emissions|CO2|Energy|Demand|Industry",
#                 "Emissions|CO2|Energy|Demand|Residential and Commercial",
#                 "Emissions|CO2|Energy|Demand|Transportation",
#                 "Emissions|CO2|Industrial Processes",
#                 "Emissions|CO2|AFOLU",
#                 "Emissions|Non-CO2")  

if(model=="PROMETHEUS"){
  vars <- c(    "Emissions|CO2|Energy|Supply",
                "Emissions|CO2|Energy|Demand|Industry",
                "Emissions|CO2|Energy|Demand|Residential and Commercial",
                "Emissions|CO2|Energy|Demand|Transportation",
                "Emissions|CO2|Industrial Processes")  
}else{if(model=="TIAM_Grantham_v3.2"){
  vars <- c(    "Emissions|CO2|Energy|Supply",
                "Emissions|CO2|Energy|Demand|Industry",
                "Emissions|CO2|Energy|Demand|Residential and Commercial",
                "Emissions|CO2|Energy|Demand|Transportation")  
}else{
  vars <- c(    "Emissions|CO2|Energy|Supply",
                "Emissions|CO2|Energy|Demand|Industry",
                "Emissions|CO2|Energy|Demand|Residential and Commercial",
                "Emissions|CO2|Energy|Demand|Transportation",
                "Emissions|CO2|Industrial Processes",
                "Emissions|CO2|AFOLU",
                "Emissions|Non-CO2") 
}}


ylab <-  bquote(paste("Emissions [Gt", CO[2],"eq]"))
titletag <- "Waterfall_"
file.prefix <- "Fig2_sector_"

catsnat <- c("2Deg2020","Bridge") #c("Bridge","NDCplus")
labcat <-  c("2Deg2020","Bridge") #c("Bridge","NDCplus") 

#choose reference scenario INDC or NoPOL
reference_cat <- catsnat[2]

for(icat in c(1)){
  tt = c("2015", "2050")
  for(iper in c(1,2)){
    if(iper == 2) {tt[2] <- "2030"}
    dtn <- filter(cdata, Category %in% catsnat, variable %in% vars, period %in% tt)  %>% group_by(Category,region,variable,period,unit) %>% summarize(value=mean(value)) %>% ungroup()
    
    # defining the stacks
    # 2015
    dtn_1 <- filter(dtn, Category==reference_cat, variable %in% vars, period == "2015") %>%
      mutate(bar_position = "1") 
    
    # 2050 reference scenario
    dtn_2 <- filter(dtn, Category==reference_cat, variable %in% vars, period == tt[2]) %>%
      mutate(bar_position = "2") %>% factor.data.frame()
    
    #last stack: ambitious mitigation scenario
    # dtn_9 <- filter(dtn, Category==catsnat[1], variable %in% vars, period == tt[2]) %>%
    #   mutate(bar_position = "10") %>% factor.data.frame()       
    if(model=="PROMETHEUS"){
      dtn_9 <- filter(dtn, Category==catsnat[1], variable %in% vars, period == tt[2]) %>%
        mutate(bar_position = "8") %>% factor.data.frame()       
    }else{if(model=="TIAM_Grantham_v3.2"){
      dtn_9 <- filter(dtn, Category==catsnat[1], variable %in% vars, period == tt[2]) %>%
        mutate(bar_position = "7") %>% factor.data.frame()        
    }else{
      dtn_9 <- filter(dtn, Category==catsnat[1], variable %in% vars, period == tt[2]) %>%
        mutate(bar_position = "10") %>% factor.data.frame()
    } }
    
    # calculating the difference
    dtn_tmp <- filter(dtn, variable %in% vars, period == tt[2])
    dtn_tmp1 <- spread(dtn_tmp,Category, value=value)
    
    setnames(dtn_tmp1,paste0("",catsnat[1],""),"low")
    setnames(dtn_tmp1,paste0("",catsnat[2],""),"ref")
    dtn_tmp2 <- mutate(dtn_tmp1,diff_to_reference = ref - low)
    
    # Energy supply
    if(model%in%c("PROMETHEUS","TIAM_Grantham_v3.2")){
      dtn_3 <- filter(dtn_tmp2, variable == vars[1], period == tt[2]) %>%
        mutate(value = diff_to_reference) %>% mutate(bar_position = "6")%>%  factor.data.frame()     
    }else{
      dtn_3 <- filter(dtn_tmp2, variable == vars[1], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "7")%>%  factor.data.frame()
    }
    
    # Industry
    if(model%in%c("PROMETHEUS","TIAM_Grantham_v3.2")){
      dtn_4 <- filter(dtn_tmp2, variable == vars[2], period == tt[2]) %>%
        mutate(value = diff_to_reference) %>% mutate(bar_position = "3")%>%  factor.data.frame()
    }else{
      dtn_4 <- filter(dtn_tmp2, variable == vars[2], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "4")%>%  factor.data.frame()
    } 
    
    # Buildings  
    if(model%in%c("PROMETHEUS","TIAM_Grantham_v3.2")){
      dtn_5 <- filter(dtn_tmp2, variable == vars[3], period == tt[2]) %>%
        mutate(value = diff_to_reference) %>% mutate(bar_position = "4")%>%  factor.data.frame()
    }else{
      dtn_5 <- filter(dtn_tmp2, variable == vars[3], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "5")%>%  factor.data.frame()
    }
    
    # Transport
    if(model%in%c("PROMETHEUS","TIAM_Grantham_v3.2")){
      dtn_6 <- filter(dtn_tmp2, variable == vars[4], period == tt[2]) %>%
        mutate(value = diff_to_reference) %>% mutate(bar_position = "5")%>%  factor.data.frame()
    }else{
      dtn_6 <- filter(dtn_tmp2, variable == vars[4], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "6")%>%  factor.data.frame()
    } 
    
    # Industrial processes
    if(model=="PROMETHEUS"){
      dtn_7 <- filter(dtn_tmp2, variable == vars[5], period == tt[2]) %>%
        mutate(value = diff_to_reference) %>% mutate(bar_position = "7")%>%  factor.data.frame()       
    }else{if(model=="TIAM_Grantham_v3.2"){
              
    }else{
      dtn_7 <- filter(dtn_tmp2, variable == vars[5], period == tt[2]) %>%
        mutate(value = diff_to_reference) %>% mutate(bar_position = "8")%>%  factor.data.frame()
    } }
    
    
    # AFOLU
    if(!model%in%c("TIAM_Grantham_v3.2","PROMETHEUS")){
      dtn_8 <- filter(dtn_tmp2, variable == vars[6], period == tt[2]) %>%
        mutate(value = diff_to_reference) %>% mutate(bar_position = "3")%>%  factor.data.frame()
    } 
    
    # Non-CO2
    if(!model%in%c("TIAM_Grantham_v3.2","PROMETHEUS")){
      dtn_10 <- filter(dtn_tmp2, variable == vars[7], period == tt[2]) %>%
        mutate(value = diff_to_reference) %>% mutate(bar_position = "9")%>%  factor.data.frame()
    }
    
    
    # only region, value (and later variable) needed
    reduction <- c("region","value", "variable","bar_position")
    dtn_2_reduced <- dtn_2 %>% group_by(region,bar_position) %>% summarise(value = sum(value)) %>%
      factor.data.frame() %>%  mutate(variable = "total")
    dtn_1_stack_reduced <- dtn_1[names(dtn_1) %in% reduction]
    dtn_2_stack_reduced <- dtn_2[names(dtn_2) %in% reduction]
    dtn_3_reduced <- dtn_3[names(dtn_3) %in% reduction]
    dtn_4_reduced <- dtn_4[names(dtn_4) %in% reduction]
    dtn_5_reduced <- dtn_5[names(dtn_5) %in% reduction]
    dtn_6_reduced <- dtn_6[names(dtn_6) %in% reduction]
    if(!model%in%c("TIAM_Grantham_v3.2")){
      dtn_7_reduced <- dtn_7[names(dtn_7) %in% reduction]
    }
    if(!model%in%c("TIAM_Grantham_v3.2","PROMETHEUS")){
      dtn_8_reduced <- dtn_8[names(dtn_8) %in% reduction]
    }
    if(!model%in%c("TIAM_Grantham_v3.2","PROMETHEUS")){
      dtn_10_reduced <- dtn_10[names(dtn_10) %in% reduction]
    }
    dtn_9_stack_reduced <- dtn_9[names(dtn_9) %in% reduction]
    
    #ydummy region transparent #TODO update this to make it a waterfall again with the new order (dtn_8 = 3, dtn_4 =4, 5=5, 6=6, dtn_3=7, dtn_7=8, dtn_10=9)
    if(model=="PROMETHEUS"){
      #industry
      ydummy4<-data.frame("ydummy",sum(dtn_2_reduced$value)-sum(dtn_4_reduced$value),"total-dummy",3)
      names(ydummy4)<-reduction
      dtn_4_reduced = rbind(dtn_4_reduced,ydummy4)
      #buildings
      ydummy5<-data.frame("ydummy",sum(dtn_4_reduced[dtn_4_reduced$region == "ydummy",]$value)-sum(dtn_5_reduced$value),"total-dummy",4)
      names(ydummy5)<-reduction
      dtn_5_reduced = rbind(dtn_5_reduced,ydummy5)
      #transport
      ydummy6<-data.frame("ydummy",sum(dtn_5_reduced[dtn_5_reduced$region == "ydummy",]$value)-sum(dtn_6_reduced$value),"total-dummy",5)
      names(ydummy6)<-reduction
      dtn_6_reduced = rbind(dtn_6_reduced,ydummy6)
      #supply
      ydummy3<-data.frame("ydummy",sum(dtn_6_reduced[dtn_6_reduced$region == "ydummy",]$value)-sum(dtn_3_reduced$value),"total-dummy",6)
      names(ydummy3)<-reduction
      dtn_3_reduced = rbind(dtn_3_reduced,ydummy3)
      #industrial processes
      ydummy7<-data.frame("ydummy",sum(dtn_3_reduced[dtn_3_reduced$region == "ydummy",]$value)-sum(dtn_7_reduced$value),"total-dummy",7)
      names(ydummy7)<-reduction
      dtn_7_reduced = rbind(dtn_7_reduced,ydummy7)
    }else{if(model=="TIAM_Grantham_v3.2"){
      #industry
      ydummy4<-data.frame("ydummy",sum(dtn_2_reduced$value)-sum(dtn_4_reduced$value),"total-dummy",3)
      names(ydummy4)<-reduction
      dtn_4_reduced = rbind(dtn_4_reduced,ydummy4)
      #buildings
      ydummy5<-data.frame("ydummy",sum(dtn_4_reduced[dtn_4_reduced$region == "ydummy",]$value)-sum(dtn_5_reduced$value),"total-dummy",4)
      names(ydummy5)<-reduction
      dtn_5_reduced = rbind(dtn_5_reduced,ydummy5)
      #transport
      ydummy6<-data.frame("ydummy",sum(dtn_5_reduced[dtn_5_reduced$region == "ydummy",]$value)-sum(dtn_6_reduced$value),"total-dummy",5)
      names(ydummy6)<-reduction
      dtn_6_reduced = rbind(dtn_6_reduced,ydummy6)
      #supply
      ydummy3<-data.frame("ydummy",sum(dtn_6_reduced[dtn_6_reduced$region == "ydummy",]$value)-sum(dtn_3_reduced$value),"total-dummy",6)
      names(ydummy3)<-reduction
      dtn_3_reduced = rbind(dtn_3_reduced,ydummy3)
    }else{
      #AFOLU
      ydummy8<-data.frame("ydummy",sum(dtn_2_reduced$value)-sum(dtn_8_reduced$value),"total-dummy",3)
      names(ydummy8)<-reduction
      dtn_8_reduced = rbind(dtn_8_reduced,ydummy8)
      #industry
      ydummy4<-data.frame("ydummy",sum(dtn_8_reduced[dtn_8_reduced$region == "ydummy",]$value)-sum(dtn_4_reduced$value),"total-dummy",4)
      names(ydummy4)<-reduction
      dtn_4_reduced = rbind(dtn_4_reduced,ydummy4)
      #buildings
      ydummy5<-data.frame("ydummy",sum(dtn_4_reduced[dtn_4_reduced$region == "ydummy",]$value)-sum(dtn_5_reduced$value),"total-dummy",5)
      names(ydummy5)<-reduction
      dtn_5_reduced = rbind(dtn_5_reduced,ydummy5)
      #transport
      ydummy6<-data.frame("ydummy",sum(dtn_5_reduced[dtn_5_reduced$region == "ydummy",]$value)-sum(dtn_6_reduced$value),"total-dummy",6)
      names(ydummy6)<-reduction
      dtn_6_reduced = rbind(dtn_6_reduced,ydummy6)
      #supply
      ydummy3<-data.frame("ydummy",sum(dtn_6_reduced[dtn_6_reduced$region == "ydummy",]$value)-sum(dtn_3_reduced$value),"total-dummy",7)
      names(ydummy3)<-reduction
      dtn_3_reduced = rbind(dtn_3_reduced,ydummy3)
      #industrial processes
      ydummy7<-data.frame("ydummy",sum(dtn_3_reduced[dtn_3_reduced$region == "ydummy",]$value)-sum(dtn_7_reduced$value),"total-dummy",8)
      names(ydummy7)<-reduction
      dtn_7_reduced = rbind(dtn_7_reduced,ydummy7)
      #non-CO2
      ydummy10<-data.frame("ydummy",sum(dtn_7_reduced[dtn_7_reduced$region == "ydummy",]$value)-sum(dtn_10_reduced$value),"total-dummy",9)
      names(ydummy10)<-reduction
      dtn_10_reduced = rbind(dtn_10_reduced,ydummy10)
    }}
      
    #archive:  
    #ydummy8<-data.frame("ydummy",sum(dtn_7_reduced[dtn_7_reduced$region == "ydummy",]$value)-sum(dtn_8_reduced$value),"total-dummy",3)
    #ydummy3<-data.frame("ydummy",sum(dtn_6_reduced$value)-sum(dtn_3_reduced$value),"total-dummy",7)
    
    
    #dtn_all=bind_rows(dtn_1_stack_reduced,dtn_2_stack_reduced, dtn_3_reduced, dtn_4_reduced, dtn_5_reduced, dtn_6_reduced,dtn_7_reduced,dtn_8_reduced, dtn_10_reduced,dtn_9_stack_reduced)
    if(model%in%c("PROMETHEUS")){
      dtn_all=bind_rows(dtn_1_stack_reduced,dtn_2_stack_reduced, dtn_3_reduced, dtn_4_reduced, dtn_5_reduced, dtn_6_reduced,dtn_7_reduced,dtn_9_stack_reduced)  
    }else{
      if(model=="TIAM_Grantham_v3.2"){
        dtn_all=bind_rows(dtn_1_stack_reduced,dtn_2_stack_reduced, dtn_3_reduced, dtn_4_reduced, dtn_5_reduced, dtn_6_reduced,dtn_9_stack_reduced)
      }else{
        dtn_all=bind_rows(dtn_1_stack_reduced,dtn_2_stack_reduced, dtn_3_reduced, dtn_4_reduced, dtn_5_reduced, dtn_6_reduced,dtn_7_reduced,dtn_8_reduced, dtn_10_reduced,dtn_9_stack_reduced)
      }}
    
    dtn_all$alpha <- 0*dtn_all$value
    dtn_all[dtn_all$region !="ydummy",]$alpha <- 1
    dtn_all <- data.frame(dtn_all)
    
    dtn_all$bar_position <- as.numeric(dtn_all$bar_position)
    dtn_all <- dtn_all[order(dtn_all$bar_position),]
    dtn_all$value <- dtn_all$value/1000

    ### waterfall plot - TODO: add 2030 and 2050 in one graph? save plots in environment for grid arrange?
    #TODO: layout Filip
    ggplot()+
      geom_bar(data = dtn_all, aes(x = bar_position, y = value , fill = variable ,alpha=alpha),stat='identity',width=0.7)+
      scale_fill_manual(values=c("total"="#cc0000","Emissions|CO2|Energy|Demand|Residential and Commercial"="#7777ff","Emissions|CO2|Energy|Demand|Industry"="#228888",
                                 "Emissions|CO2|Energy|Supply"="#552288","Emissions|CO2|Energy|Demand|Transportation"="#222288","Emissions|CO2|Energy"="transparent",
                                 "Emissions|CO2|Non-Energy"="#006400","Emissions|Non-CO2"="#7cfc00","total-dummy"="transparent",
                                 "Emissions|CO2|Industrial Processes"="#a9a9a9","Emissions|CO2|AFOLU"="#006400"),
                        labels=c("Emissions|CO2|Energy|Demand|Transportation"="Transport","Emissions|CO2|Energy|Demand|Industry"="Industry",
                                 "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings","Emissions|CO2|Energy|Supply"="Supply",
                                 "Emissions|CO2|Non-Energy"="Non-Energy CO2","Emissions|Non-CO2"="Non-CO2","total"="Total","total-dummy"="",
                                 "Emissions|CO2|Industrial Processes"="Industrial processes","Emissions|CO2|AFOLU"="AFOLU"),
                        name="Sector",guide=F) +
      geom_hline(yintercept=0)+
      ggtitle(unique(cdata$model))+ #paste(country,"- 2050 emissions in reference scenario vs. decarbonisation scenario")
      scale_alpha(guide = "none")+coord_cartesian(ylim=c(-10,60))+
      ylab(ylab)+xlab("") + theme_bw() + theme(axis.text.x = element_text(angle=90))+
      # scale_x_continuous(breaks=unique(dtn_all$bar_position),minor_breaks = NULL,
      #                      labels=c(tt[1], paste0(tt[2],"\n",labcat[2]), "Supply","Industry","Buildings","Transport","Industrial processes","AFOLU","Non-CO2",paste0(tt[2],"\n",labcat[icat]))) #bquote(paste("Non-", CO[2]))
      if(model=="PROMETHEUS"){
        scale_x_continuous(breaks=unique(dtn_all$bar_position),minor_breaks = NULL,
                           labels=c(tt[1], paste0(labcat[2],"\n",tt[2]), "Industry","Buildings","Transport","Supply","Industrial Processes",paste0(labcat[icat],"\n",tt[2])))
      }else{if(model=="TIAM_Grantham_v3.2"){
        scale_x_continuous(breaks=unique(dtn_all$bar_position),minor_breaks = NULL,
                           labels=c(tt[1], paste0(labcat[2],"\n",tt[2]), "Industry","Buildings","Transport","Supply",paste0(labcat[icat],"\n",tt[2])))
      }else{
        scale_x_continuous(breaks=unique(dtn_all$bar_position),minor_breaks = NULL,
                           labels=c(tt[1], paste0(labcat[2],"\n",tt[2]), "AFOLU","Industry","Buildings","Transport","Supply","Industrial processes","Non-CO2",paste0(labcat[icat],"\n",tt[2])))
      }}
    
    ggsave(filename=paste0("~/disks/y/Project/E555163_COMMIT/Data/Database/Snapshots/Scripts/R/Bridge/Bridge/output/",file.prefix,unique(cdata$model),"-",labcat[icat],"_",tt[2],".png"),width=5, height=3.5)  
    
  }
}
