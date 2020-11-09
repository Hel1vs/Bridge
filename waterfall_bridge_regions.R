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

vars <- c("R5ASIA","R5LAM","R5REF","R5OECD90+EU","R5MAF")  

ylab <-  bquote(paste("Emissions [Mt", CO[2],"eq]"))
titletag <- "Waterfall_"
file.prefix <- "Fig2_region_"

catsnat <- c("Bridge","NDCplus")
labcat <- c("Bridge","NDCplus")

#choose reference scenario INDC or NoPOL
reference_cat <- catsnat[2]

for(icat in c(1)){
  tt = c("2015", "2050")
  for(iper in c(1,2)){
    if(iper == 2) {tt[2] <- "2030"}
    dtn <- filter(cdata, Category %in% catsnat, period %in% tt)  %>% group_by(Category,region,variable,period,unit) %>% summarize(value=mean(value)) %>% ungroup()
    
    # defining the stacks
    dtn_1 <- filter(dtn, Category==reference_cat, region %in% vars, period == "2015") %>%
      mutate(bar_position = "1") 
    
    dtn_2 <- filter(dtn, Category==reference_cat, region %in% vars, period == tt[2]) %>%
      mutate(bar_position = "2") %>% factor.data.frame()
    
    #last stack: ambitious mitigation scenario
    dtn_9 <- filter(dtn, Category==catsnat[1], region %in% vars, period == tt[2]) %>%
      mutate(bar_position = "8") %>% factor.data.frame()        
    
    # calculating the difference
    dtn_tmp <- filter(dtn, region %in% vars, period == tt[2])
    dtn_tmp1 <- spread(dtn_tmp,Category, value=value)
    
    setnames(dtn_tmp1,paste0("",catsnat[1],""),"low")
    setnames(dtn_tmp1,paste0("",catsnat[2],""),"ref")
    dtn_tmp2 <- mutate(dtn_tmp1,diff_to_reference = ref - low)
    
    dtn_3 <- filter(dtn_tmp2, region == vars[1], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "3")%>%  factor.data.frame()
    
    dtn_4 <- filter(dtn_tmp2, region == vars[2], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "4")%>%  factor.data.frame()
    
    dtn_5 <- filter(dtn_tmp2, region == vars[3], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "5")%>%  factor.data.frame()
    
    dtn_6 <- filter(dtn_tmp2, region == vars[4], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "6")%>%  factor.data.frame()
    
    dtn_7 <- filter(dtn_tmp2, region == vars[5], period == tt[2]) %>%
      mutate(value = diff_to_reference) %>% mutate(bar_position = "7")%>%  factor.data.frame()
    
        # only region, value (and later variable) needed
    reduction <- c("region","value", "variable","bar_position")
    dtn_2_reduced <- dtn_2 %>% group_by(variable,bar_position) %>% summarise(value = sum(value)) %>%
      factor.data.frame() %>%  mutate(variable = "total")
    dtn_1_stack_reduced <- dtn_1[names(dtn_1) %in% reduction]
    dtn_2_stack_reduced <- dtn_2[names(dtn_2) %in% reduction]
    dtn_3_reduced <- dtn_3[names(dtn_3) %in% reduction]
    dtn_4_reduced <- dtn_4[names(dtn_4) %in% reduction]
    dtn_5_reduced <- dtn_5[names(dtn_5) %in% reduction]
    dtn_6_reduced <- dtn_6[names(dtn_6) %in% reduction]
    dtn_7_reduced <- dtn_7[names(dtn_7) %in% reduction]
    dtn_9_stack_reduced <- dtn_9[names(dtn_9) %in% reduction]
    
    #ydummy region transparent
    ydummy3<-data.frame("ydummy",sum(dtn_2_reduced$value)-sum(dtn_3_reduced$value),"total-dummy",3)
    names(ydummy3)<-reduction
    dtn_3_reduced = rbind(dtn_3_reduced,ydummy3)
    ydummy4<-data.frame("ydummy",sum(dtn_3_reduced[dtn_3_reduced$region == "ydummy",]$value)-sum(dtn_4_reduced$value),"total-dummy",4)
    names(ydummy4)<-reduction
    dtn_4_reduced = rbind(dtn_4_reduced,ydummy4)
    ydummy5<-data.frame("ydummy",sum(dtn_4_reduced[dtn_4_reduced$region == "ydummy",]$value)-sum(dtn_5_reduced$value),"total-dummy",5)
    names(ydummy5)<-reduction
    dtn_5_reduced = rbind(dtn_5_reduced,ydummy5)
    ydummy6<-data.frame("ydummy",sum(dtn_5_reduced[dtn_5_reduced$region == "ydummy",]$value)-sum(dtn_6_reduced$value),"total-dummy",6)
    names(ydummy6)<-reduction
    dtn_6_reduced = rbind(dtn_6_reduced,ydummy6)
    ydummy7<-data.frame("ydummy",sum(dtn_6_reduced[dtn_6_reduced$region == "ydummy",]$value)-sum(dtn_7_reduced$value),"total-dummy",7)
    names(ydummy7)<-reduction
    dtn_7_reduced = rbind(dtn_7_reduced,ydummy7)
    
    dtn_all=bind_rows(dtn_1_stack_reduced,dtn_2_stack_reduced, dtn_3_reduced, dtn_4_reduced, dtn_5_reduced, dtn_6_reduced,dtn_7_reduced,dtn_9_stack_reduced)
    dtn_all$alpha <- 0*dtn_all$value
    dtn_all[dtn_all$region !="ydummy",]$alpha <- 1
    dtn_all <- data.frame(dtn_all)
    
    dtn_all$bar_position <- as.numeric(dtn_all$bar_position)
    dtn_all$value <- dtn_all$value/1000
    
    ### waterfall plot - TODO: add 2030 and 2050 in one graph? save plots in environment for grid arrange?
    #TODO: layout Filip
    ggplot()+
      geom_bar(data = dtn_all, aes(x = bar_position, y = value , fill = region ,alpha=alpha),stat='identity',width=0.7)+
      scale_fill_manual(values=c("total"="#cc0000","R5ASIA"="#7777ff","R5LAM"="#bb7700","R5MAF"="#993a44","R5OECD90+EU"="#222288","R5REF"="#006400","total-dummy"="transparent"),
                        labels=c("R5ASIA"="Asia","R5LAM"="Latin America","R5MAF"="Middle East and Africa","R5OECD90+EU"="OECD90+EU","R5REF"="Reforming economies","total"="Total","total-dummy"=""),
                        name="Region",guide=F) +
      ggtitle(unique(cdata$model))+ #paste(country,"- 2050 emissions in reference scenario vs. decarbonisation scenario")
      scale_alpha(guide = "none")+#coord_cartesian(ylim=c(-1000,12000))+
      ylab(ylab)+xlab("") + theme_bw() + theme(axis.text.x = element_text(angle=90))+
      scale_x_continuous(breaks=unique(dtn_all$bar_position),minor_breaks = NULL,
                         labels=c(tt[1], paste0(tt[2],"\n",labcat[2]), "Asia","Latin America","Middle East and Africa","OECD90+EU","Reforming economies",paste0(tt[2],"\n",labcat[icat]))) #bquote(paste("Non-", CO[2]))
    ggsave(filename=paste0("~/disks/y/Project/E555163_COMMIT/Data/Database/Snapshots/Scripts/R/Bridge/Bridge/output/",file.prefix,unique(cdata$model),"-",labcat[icat],"_",tt[2],".png"),width=5, height=3.5)  
    
  }
}
