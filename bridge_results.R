# Load data ---------------------------------------------------------------
setwd("~/disks/y/Project/E555163_COMMIT/Data/Database/Snapshots/Scripts/R/Bridge/Bridge")
config <- "config_bridge" #"config_COMMIT"
scencateg <- "scen_categ_bridge"  #"scen_categ_COMMIT"
variables <- "variables_bridge"  #"variables_xCut"
adjust <- "adjust_reporting_COMMIT"
addvars <- F
datafile <-"commit_bridge_compare_20201013-092239" #commit_cd-links_compare_20191015-114544
source("load_data.R") 

# check whether there's only one scenario per category for each model
check=all[,list(unique(scenario)),by=c("model","Category")]
View(check) #TODO Check Bridge IPAC included in graphs?
check2=check[,list(length(unique(V1))),by=c("model","Category")]
View(check2)

# For models with NDCplus, NDCMCS is outdated so remove. For others, keep using NDCMCS until NDCplus is submitted
check3=check[Category=="NDCplus"]
View(check3)
all=all[!c(Category=="NDCMCS"&model%in%unique(check3$model))]
# For REMIND, only global model with NDCMCS, label it NDCplus to show up in the same bar / statistics
#all[model=="REMIND-MAgPIE 1.7-3.0"&Category=="NDCMCS"]$Category<-"NDCplus"

# Load functions and library for plotting
source("functions/plot_LineNationalScens.R")
source("functions/plotstyle.R")
library(grid)
library(gridExtra)

# fix stupid R mystery
all$period<-as.numeric(as.character(all$period))

# Make a selection for WP2 paper Panagiotis: China scenarios
#wp2 = all[model%in%c("*PECE V2.0","*IPAC-AIM/technology V1.0")]
#write.csv(wp2,"WP2_China.csv")


# Plot emissions ----------------------------------------------------------
vars = "Emissions|Kyoto Gases"
scens <- c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")
# scensglob <- c("NPi","2030_low") 
# scensnat <- c("NPi","2030_low")

a<-plot_lineNationalScens(reg = "AUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="AUS (TIMES-AUS)",file_pre = "GHG") #,ylim=c(-300,1200) #,nolegend=T
b<-plot_lineNationalScens(reg = "BRA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="Brazil (BLUES)",file_pre = "GHG") #,ylim=c(-300,1200) #,nolegend=T
ca<-plot_lineNationalScens(reg = "CAN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="Canada (GCAM_Canada)", file_pre = "GHG")
c<-plot_lineNationalScens(reg = "CHN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="China (IPAC)", file_pre = "GHG")
e<-plot_lineNationalScens(reg = "EU", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="EU (PRIMES: -, GEM-E3: --)", file_pre = "GHG") #,ylim=c(0,8000)
j<-plot_lineNationalScens(reg = "JPN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]",title="Japan (AIM/E-NIES)", file_pre = "GHG") #,ylim=c(-200,1600)
r<-plot_lineNationalScens(reg = "RUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="Russia (RU-TIMES)",file_pre = "GHG") #,ylim=c(0,2500)
i<-plot_lineNationalScens(reg = "IND", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="India (IND-MARKAL)", file_pre = "GHG") #,ylim=c(0,15000)
id<-plot_lineNationalScens(reg = "IDN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="Indonesia (DDPP Ennergy)", file_pre = "GHG") #,ylim=c(0,15000)
u<-plot_lineNationalScens(reg = "USA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="USA (GCAM_USA)", file_pre = "GHG") #,ylim=c(-500,8000)
k<-plot_lineNationalScens(reg = "ROK", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="Korea (AIM/CGE[Korea])", file_pre = "GHG") #,ylim=c(-500,8000)
w<-plot_lineNationalScens(reg = "World", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = "GHG emissions [MtCO2e]", title="World", file_pre = "GHG") #,ylim=c(-500,8000)

tmp<-ggplot_gtable(ggplot_build(j))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
a=a+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
b=b+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
c=c+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
ca=ca+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
e=e+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
i=i+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
id=id+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
j=j+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
r=r+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
u=u+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
k=k+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
w=w+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
lay<-rbind(c(1,2,3,4,5,6),c(7,8,9,10,11,12))
h=grid.arrange(a,b,c,ca,e,i,id,j,r,u,k,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/GHG_natscens_gridarrange.png",sep=""),h,width=24,height=14,dpi=200)

# AFOLU emissions
vars = "Emissions|CO2|AFOLU"
scens <- c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")
ylab = "AFOLU CO2 emissions (MtCO2/year)"
file_pre = "CO2-AFOLU"

a<-plot_lineNationalScens(reg = "AUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Australia (TIMES-AUS)",file_pre = file_pre,nolegend=T) #,ylim=c(-300,1200) 
b<-plot_lineNationalScens(reg = "BRA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Brazil (BLUES)",file_pre = file_pre,nolegend=T) #,ylim=c(-300,1200) 
ca<-plot_lineNationalScens(reg = "CAN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                           ylab = ylab, title="Canada (GCAM_Canada)", file_pre = file_pre)
c<-plot_lineNationalScens(reg = "CHN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="China (IPAC)", file_pre = file_pre)
e<-plot_lineNationalScens(reg = "EU", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="EU (PRIMES: -, GEM-E3: --)", file_pre = file_pre) #,ylim=c(0,8000)
j<-plot_lineNationalScens(reg = "JPN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab,title="Japan (AIM/E-NIES)", file_pre = file_pre) #,ylim=c(-200,1600)
r<-plot_lineNationalScens(reg = "RUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Russia (RU-TIMES)",file_pre = file_pre) #,ylim=c(0,2500)
i<-plot_lineNationalScens(reg = "IND", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="India (IND-MARKAL)", file_pre = file_pre) #,ylim=c(0,15000)
id<-plot_lineNationalScens(reg = "IDN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Indonesia (DDPP Energy)", file_pre = file_pre) #,ylim=c(0,15000)
u<-plot_lineNationalScens(reg = "USA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="USA (GCAM_USA)", file_pre = file_pre) #,ylim=c(-500,8000)
k<-plot_lineNationalScens(reg = "ROK", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Korea (AIM/CGE[Korea])", file_pre = file_pre) #,ylim=c(-500,8000)
w<-plot_lineNationalScens(reg = "World", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="World", file_pre = file_pre) #,ylim=c(-500,8000)

tmp<-ggplot_gtable(ggplot_build(j))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
a=a+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
b=b+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
c=c+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
ca=ca+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
e=e+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
i=i+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
id=id+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
j=j+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
r=r+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
u=u+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
k=k+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
w=w+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
lay<-rbind(c(1,2,3,4,5,6),c(7,8,9,10,11,12))
h=grid.arrange(a,b,c,ca,e,i,id,j,r,u,k,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/CO2-AFOLU_natscens_gridarrange.png",sep=""),h,width=24,height=14,dpi=200)

# CO2 emissions
vars = "Emissions|CO2"
scens <- c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")
ylab = "CO2 emissions (MtCO2/year)"
file_pre = "CO2"

a<-plot_lineNationalScens(reg = "AUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Australia (TIMES-AUS)",file_pre = file_pre,nolegend=T) #,ylim=c(-300,1200) 
b<-plot_lineNationalScens(reg = "BRA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Brazil (BLUES)",file_pre = file_pre,nolegend=T) #,ylim=c(-300,1200) 
ca<-plot_lineNationalScens(reg = "CAN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                           ylab = ylab, title="Canada (GCAM_Canada)", file_pre = file_pre)
c<-plot_lineNationalScens(reg = "CHN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="China (IPAC)", file_pre = file_pre)
e<-plot_lineNationalScens(reg = "EU", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="EU (PRIMES: -, GEM-E3: --)", file_pre = file_pre) #,ylim=c(0,8000)
j<-plot_lineNationalScens(reg = "JPN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab,title="Japan (AIM/E-NIES)", file_pre = file_pre) #,ylim=c(-200,1600)
r<-plot_lineNationalScens(reg = "RUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Russia (RU-TIMES)",file_pre = file_pre) #,ylim=c(0,2500)
i<-plot_lineNationalScens(reg = "IND", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="India (IND-MARKAL)", file_pre = file_pre) #,ylim=c(0,15000)
id<-plot_lineNationalScens(reg = "IDN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Indonesia (DDPP Energy)", file_pre = file_pre) #,ylim=c(0,15000)
u<-plot_lineNationalScens(reg = "USA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="USA (GCAM_USA)", file_pre = file_pre) #,ylim=c(-500,8000)
k<-plot_lineNationalScens(reg = "ROK", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Korea (AIM/CGE[Korea])", file_pre = file_pre) #,ylim=c(-500,8000)
w<-plot_lineNationalScens(reg = "World", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="World", file_pre = file_pre) #,ylim=c(-500,8000)

tmp<-ggplot_gtable(ggplot_build(j))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
a=a+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
b=b+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
c=c+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
ca=ca+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
e=e+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
i=i+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
id=id+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
j=j+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
r=r+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
u=u+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
k=k+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
w=w+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
lay<-rbind(c(1,2,3,4,5,6),c(7,8,9,10,11,12))
h=grid.arrange(a,b,c,ca,e,i,id,j,r,u,k,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/CO2_natscens_gridarrange.png",sep=""),h,width=24,height=14,dpi=200)


# Quick plot IMAGE
vars = "Emissions|Kyoto Gases"
i = ggplot(all[variable%in%vars & Category%in%scens & model=="IMAGE 3.0"]) # & !region=="World"
i = i + geom_line(aes(x=period,y=value,linetype=model,colour=Category))
i = i + scale_colour_manual(values=plotstyle(scens))
#e = e + scale_linetype_manual(values=cfg$man_lines)# TODO use plotstyle for linetypes per model or cfg$man_lines?
i = i + facet_wrap(~region,scales="free_y")
i = i + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))
i = i + theme_bw()
i
ggsave(file=paste(cfg$outdir,"/GHG_IMAGE.png",sep=""),i,width=18,height=14,dpi=200)

# quick plot all models
vars = "Emissions|Kyoto Gases"
m = ggplot(all[variable%in%vars & Category%in%scens&!Scope=="national"&!region=="TUR"]) # & !region=="World"
m = m + geom_line(aes(x=period,y=value,colour=Category))
m = m + scale_colour_manual(values=plotstyle(scens))
#e = e + scale_linetype_manual(values=cfg$man_lines)# TODO use plotstyle for linetypes per model or cfg$man_lines?
m = m + facet_grid(region~model,scales="free_y")
m = m + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))
m = m + theme_bw() + theme(axis.text.y=element_text(size=14)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
m
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models.png",sep=""),m,width=16,height=14,dpi=200)

vars = "Emissions|CO2"
m = ggplot(all[variable%in%vars & Category%in%scens&!Scope=="global"&!region%in%c("TUR","World","R5OECD90+EU")]) 
m = m + geom_line(aes(x=period,y=value,colour=Category))
m = m + xlim(2005,2050)
m = m + scale_colour_manual(values=plotstyle(scens))
#e = e + scale_linetype_manual(values=cfg$man_lines)# TODO use plotstyle for linetypes per model or cfg$man_lines?
m = m + facet_wrap(~region,scales="free_y")
m = m + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))
m = m + theme_bw()
m
ggsave(file=paste(cfg$outdir,"/CO2_all_national_models.png",sep=""),m,width=18,height=10,dpi=200)

# plot build-up
vars = "Emissions|Kyoto Gases"
m1 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol")&!Scope=="national"&region=="World"]) 
m1 = m1 + geom_line(aes(x=period,y=value,colour=Category),size=1.5)
m1 = m1 + xlim(2000,2050) + scale_y_continuous(breaks=c(40000,50000,60000,70000,80000),limits=c(40000,85000))
m1 = m1 + scale_colour_manual(values=plotstyle(scens))
m1 = m1 + facet_grid(~model,scales="free_y")
m1 = m1 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]")) + xlab("")
m1 = m1 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m1
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models_world_BAU-CurPol.png",sep=""),m1,width=18,height=14,dpi=200)

m2 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS")&!Scope=="national"&region=="World"]) 
m2 = m2 + geom_line(aes(x=period,y=value,colour=Category),size=1.5)
m2 = m2 + xlim(2000,2050)+ scale_y_continuous(breaks=c(40000,50000,60000,70000,80000),limits=c(40000,85000))
m2 = m2 + scale_colour_manual(values=plotstyle(scens))
m2 = m2 + facet_grid(~model,scales="free_y")
m2 = m2 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m2 = m2 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m2
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models_world_BAU-CurPol-NDC.png",sep=""),m2,width=16,height=14,dpi=200)

m3 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","2Deg2020")&!Scope=="national"&region=="World"]) 
m3 = m3 + geom_line(aes(x=period,y=value,colour=Category),size=1.5)
m3 = m3 + xlim(2000,2050)+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m3 = m3 + scale_colour_manual(values=plotstyle(scens))
m3 = m3 + facet_grid(~model,scales="free_y")
m3 = m3 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m3 = m3 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m3
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models_world_BAU-CurPol-NDC-2Deg2020.png",sep=""),m3,width=16,height=14,dpi=200)

m4 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","2Deg2020")&!Scope=="national"&region=="World"]) 
m4 = m4 + geom_line(aes(x=period,y=value,colour=Category),size=1.5)
m4 = m4 + xlim(2000,2050)+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m4 = m4 + scale_colour_manual(values=plotstyle(scens))
m4 = m4 + facet_grid(~model,scales="free_y")
m4 = m4 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m4 = m4 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m4
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2020.png",sep=""),m4,width=16,height=14,dpi=200)

m5 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2020")&!Scope=="national"&region=="World"]) 
m5 = m5 + geom_line(aes(x=period,y=value,colour=Category),size=1.5)
m5 = m5 + xlim(2000,2050)+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m5 = m5 + scale_colour_manual(values=plotstyle(scens))
m5 = m5 + facet_grid(~model,scales="free_y")
m5 = m5 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m5 = m5 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m5
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2020-Bridge.png",sep=""),m5,width=16,height=14,dpi=200)

m6 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m6 = m6 + geom_line(aes(x=period,y=value,colour=Category),size=1.5)
m6 = m6 + xlim(2000,2050)+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m6 = m6 + scale_colour_manual(values=plotstyle(scens))
m6 = m6 + facet_grid(~model,scales="free_y")
m6 = m6 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m6 = m6 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m6
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020.png",sep=""),m6,width=16,height=14,dpi=200)
m6 = m6 + theme(legend.position="bottom")
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_wide.png",sep=""),m6,width=16,height=12,dpi=200)

# models as lines, ranges as funnels
range=all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World",list(min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","variable","period")]
m7 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m7 = m7 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m7 = m7 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m7 = m7 + xlim(2010,2050)+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m7 = m7 + scale_colour_manual(values=plotstyle(scens))
m7 = m7 + scale_fill_manual(values=plotstyle(scens))
m7 = m7 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m7 = m7 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m7 = m7 + theme(legend.position="bottom")
m7
ggsave(file=paste(cfg$outdir,"/GHG_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_funnel.png",sep=""),m7,width=16,height=12,dpi=200)

vars="Emissions|CO2|Energy|Demand|Transportation"
range=all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World",list(min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","variable","period")]
m8 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m8 = m8 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m8 = m8 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m8 = m8 + xlim(2000,2050) #+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m8 = m8 + scale_colour_manual(values=plotstyle(scens))
m8 = m8 + scale_fill_manual(values=plotstyle(scens))
m8 = m8 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m8 = m8 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m8 = m8 + theme(legend.position="bottom")
m8
ggsave(file=paste(cfg$outdir,"/CO2_Transport_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_funnel.png",sep=""),m8,width=16,height=12,dpi=200)

vars="Emissions|CO2|Energy|Demand|Residential and Commercial"
range=all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World",list(min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","variable","period")]
m9 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m9 = m9 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m9 = m9 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m9 = m9 + xlim(2000,2050) #+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m9 = m9 + scale_colour_manual(values=plotstyle(scens))
m9 = m9 + scale_fill_manual(values=plotstyle(scens))
m9 = m9 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m9 = m9 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m9 = m9 + theme(legend.position="bottom")
m9
ggsave(file=paste(cfg$outdir,"/CO2_Buildings_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_funnel.png",sep=""),m9,width=16,height=12,dpi=200)

vars="Emissions|CO2|Energy|Demand|Industry"
range=all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World",list(min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","variable","period")]
m10 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m10 = m10 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m10 = m10 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m10 = m10 + xlim(2000,2050) #+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m10 = m10 + scale_colour_manual(values=plotstyle(scens))
m10 = m10 + scale_fill_manual(values=plotstyle(scens))
m10 = m10 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m10 = m10 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m10 = m10 + theme(legend.position="bottom")
m10
ggsave(file=paste(cfg$outdir,"/CO2_Industry_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_funnel.png",sep=""),m10,width=16,height=12,dpi=200)

vars="Emissions|CO2|Energy and Industrial Processes"
range=all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World",list(min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","variable","period")]
m11 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m11 = m11 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m11 = m11 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m11 = m11 + xlim(2000,2050) #+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m11 = m11 + scale_colour_manual(values=plotstyle(scens))
m11 = m11 + scale_fill_manual(values=plotstyle(scens))
m11 = m11 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m11 = m11 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m11 = m11 + theme(legend.position="bottom")
m11
ggsave(file=paste(cfg$outdir,"/CO2_FFI_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_funnel.png",sep=""),m11,width=16,height=12,dpi=200)

vars="Emissions|CO2|Energy|Supply"
range=all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World",list(min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","variable","period")]
m12 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m12 = m12 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m12 = m12 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m12 = m12 + xlim(2000,2050) #+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m12 = m12 + scale_colour_manual(values=plotstyle(scens))
m12 = m12 + scale_fill_manual(values=plotstyle(scens))
m12 = m12 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m12 = m12 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m12 = m12 + theme(legend.position="bottom")
m12
ggsave(file=paste(cfg$outdir,"/CO2_Supply_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_funnel.png",sep=""),m12,width=16,height=12,dpi=200)

# Check BAU vs CurPol for Alex
x=ggplot(all[Category%in%c("BAU","CurPol")&variable=="Emissions|Kyoto Gases"&Scope=="global"])
x=x+facet_grid(region~model,scale="free_y")
x=x+geom_line(aes(x=period,y=value,colour=Category))
x=x+theme_bw()
x
ggsave(file=paste(cfg$outdir,"/GHG_BAU-CurPol_models-regions.png",sep=""),x,width=16,height=12,dpi=200)

checkcp=all[Category%in%c("BAU","CurPol")&variable=="Emissions|Kyoto Gases"]
checkcp$scenario<-NULL
checkcp$Baseline<-NULL
checkcp=spread(checkcp,Category,value)
checkcp$flag=ifelse(checkcp$CurPol>checkcp$BAU,"Check","Fine")
checkcp=checkcp[flag=="Check"]  
write.csv(checkcp,"CurPolvsBAU.csv")

# Emissions reduction rate
# source("functions/calcRate.R") # calcrate does not work for negative emissions!
# emisred = all[variable%in%c("Emissions|Kyoto Gases","Emissions|CO2")&Category%in%c("CurPol","GPP","Bridge")]
# emisred = calcRate(emisred,c("Emissions|Kyoto Gases","Emissions|CO2"))
# emisredm = emisred[,list(median=median(value,na.rm=T),min=min(value,na.rm=T),max=max(value,na.rm=T)),
#                    by=c("Category","region","variable","unit","period")] #,min=min(value,na.rm=T),max=max(value,na.rm=T)
# emisred$Category = factor(emisred$Category,levels=c("CurPol","GPP","Bridge"))
# emisredm$Category = factor(emisredm$Category,levels=c("CurPol","GPP","Bridge"))
# 
# e = ggplot()
# e = e + geom_bar(data=emisredm[Category%in%c("CurPol","GPP","Bridge")&variable=="Rate of Change| Emissions|Kyoto Gases"&!region%in%c("TUR","R5OECD90+EU","R5LAM","R5ASIA","R5MAF","R5REF")],
#                  aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
# # e = e + geom_pointrange(data=emisredm[Category%in%c("CurPol","GPP","Bridge")&variable=="Rate of Change| Emissions|Kyoto Gases"],
# #                         aes(ymin=min,ymax=max,y=median, x=period, colour=Category),alpha=0.5,size=5,fatten=1,position=position_dodge(width=0.66)) #,show.legend = F
# e = e + geom_point(data=emisred[Category%in%c("CurPol","GPP","Bridge")&variable=="Rate of Change| Emissions|Kyoto Gases"&!region%in%c("TUR","R5OECD90+EU","R5LAM","R5ASIA","R5MAF","R5REF")],
#                    aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
# e = e + scale_shape_manual(values=cfg$man_shapes)
# e = e + facet_wrap(~region,scales="free_y")
# e = e + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
#         theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
# e = e + ylab("Emission reduction rate (%/yr, CAGR)")
# ggsave(file=paste(cfg$outdir,"/GHG-emissions-reduction-rate.png",sep=""),e,width=18,height=12,dpi=300)


# Plot energy -------------------------------------------------------------
vars = "Final Energy|Electricity"
scens <- c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")
ylab = "Final energy - electricity (EJ/yr)"
file_pre = "FE-elec"

a<-plot_lineNationalScens(reg = "AUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Australia (TIMES-AUS)",file_pre = file_pre) #,ylim=c(-300,1200) 
b<-plot_lineNationalScens(reg = "BRA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Brazil (BLUES)",file_pre = file_pre,nolegend=T) #,ylim=c(-300,1200) 
ca<-plot_lineNationalScens(reg = "CAN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                           ylab = ylab, title="Canada (GCAM_Canada)", file_pre = file_pre)
c<-plot_lineNationalScens(reg = "CHN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="China (IPAC)", file_pre = file_pre)
e<-plot_lineNationalScens(reg = "EU", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="EU (PRIMES: -, GEM-E3: --)", file_pre = file_pre) #,ylim=c(0,8000)
j<-plot_lineNationalScens(reg = "JPN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab,title="Japan (AIM/E-NIES)", file_pre = file_pre) #,ylim=c(-200,1600)
r<-plot_lineNationalScens(reg = "RUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Russia (RU-TIMES)",file_pre = file_pre) #,ylim=c(0,2500)
i<-plot_lineNationalScens(reg = "IND", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="India (IND-MARKAL)", file_pre = file_pre) #,ylim=c(0,15000)
id<-plot_lineNationalScens(reg = "IDN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                           ylab = ylab, title="Indonesia (DDPP Energy)", file_pre = file_pre) #,ylim=c(0,15000)
u<-plot_lineNationalScens(reg = "USA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="USA (GCAM_USA)", file_pre = file_pre) #,ylim=c(-500,8000)
k<-plot_lineNationalScens(reg = "ROK", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Korea (AIM/CGE[Korea])", file_pre = file_pre) #,ylim=c(-500,8000)
w<-plot_lineNationalScens(reg = "World", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="World", file_pre = file_pre) #,ylim=c(-500,8000)

tmp<-ggplot_gtable(ggplot_build(j))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
a=a+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
b=b+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
c=c+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
ca=ca+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
e=e+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
i=i+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
id=id+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
j=j+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
r=r+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
u=u+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
k=k+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
w=w+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
lay<-rbind(c(1,2,3,4,5,6),c(7,8,9,10,11,12))
h=grid.arrange(a,b,c,ca,e,i,id,j,r,u,k,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/FE-elec_natscens_gridarrange.png",sep=""),h,width=24,height=14,dpi=200)

#TODO - use addvars instead?
vars=c("Secondary Energy|Electricity|Solar","Secondary Energy|Electricity|Wind","Secondary Energy|Electricity|Hydro","Secondary Energy|Electricity|Biomass","Secondary Energy|Electricity|Geothermal","Secondary Energy|Electricity")
REN = all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]
REN = spread(REN,variable,value)
REN[model%in%c("IMAGE 3.0","PROMETHEUS","WITCH 5.0")]$`Secondary Energy|Electricity|Geothermal`<-"0"
REN = REN%>%mutate(REN_elec=(`Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind` + `Secondary Energy|Electricity|Hydro` + `Secondary Energy|Electricity|Biomass` + `Secondary Energy|Electricity|Geothermal`) / `Secondary Energy|Electricity` * 100 )
REN = data.table(gather(REN,variable,value,c("Secondary Energy|Electricity|Solar","Secondary Energy|Electricity|Wind","Secondary Energy|Electricity|Hydro","Secondary Energy|Electricity|Biomass","Secondary Energy|Electricity|Geothermal","Secondary Energy|Electricity","REN_elec")))
REN = REN[variable=="REN_elec"]
REN$unit <- "%"
range=REN[,list(min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","variable","period")]
m13 = ggplot(REN) 
m13 = m13 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m13 = m13 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m13 = m13 + xlim(2000,2050) #+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
m13 = m13 + scale_colour_manual(values=plotstyle(scens))
m13 = m13 + scale_fill_manual(values=plotstyle(scens))
m13 = m13 + ylab(paste(unique(REN$variable),"[",unique(REN$unit),"]"))+ xlab("")
m13 = m13 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m13 = m13 + theme(legend.position="bottom")
m13
ggsave(file=paste(cfg$outdir,"/REN_elec_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_funnel.png",sep=""),m13,width=16,height=12,dpi=200)


# Plot costs --------------------------------------------------------------
# carbon tax
vars = "Price|Carbon" #TODO check whether this is weighted average or max - for world region. Present for the three protocol tiers?
scens <- c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")
ylab = "Carbon price (US$2010/tCO2)"
file_pre = "ctax"

a<-plot_lineNationalScens(reg = "AUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Australia (TIMES-AUS)",file_pre = file_pre,ylim=c(0,7000))  
b<-plot_lineNationalScens(reg = "BRA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Brazil (BLUES)",file_pre = file_pre,nolegend=T,ylim=c(0,7000)) #,ylim=c(-300,1200) 
ca<-plot_lineNationalScens(reg = "CAN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                           ylab = ylab, title="Canada (GCAM_Canada)", file_pre = file_pre,ylim=c(0,7000))
c<-plot_lineNationalScens(reg = "CHN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="China (IPAC)", file_pre = file_pre,ylim=c(0,7000))
e<-plot_lineNationalScens(reg = "EU", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="EU (PRIMES: -, GEM-E3: --)", file_pre = file_pre,ylim=c(0,7000)) 
j<-plot_lineNationalScens(reg = "JPN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab,title="Japan (AIM/E-NIES)", file_pre = file_pre,ylim=c(0,7000)) #,ylim=c(-200,1600)
r<-plot_lineNationalScens(reg = "RUS", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Russia (RU-TIMES)",file_pre = file_pre,ylim=c(0,7000)) #,ylim=c(0,2500)
i<-plot_lineNationalScens(reg = "IND", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="India (IND-MARKAL)", file_pre = file_pre,ylim=c(0,7000)) #,ylim=c(0,15000)
id<-plot_lineNationalScens(reg = "IDN", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                           ylab = ylab, title="Indonesia (DDPP Energy)", file_pre = file_pre,ylim=c(0,7000))
u<-plot_lineNationalScens(reg = "USA", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="USA (GCAM_USA)", file_pre = file_pre,ylim=c(0,7000)) #,ylim=c(-500,8000)
k<-plot_lineNationalScens(reg = "ROK", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="Korea (AIM/CGE[Korea])", file_pre = file_pre,ylim=c(0,7000)) #,ylim=c(-500,8000)
w<-plot_lineNationalScens(reg = "World", dt = all, vars = vars, scensnat = scens, scensglob = scens,
                          ylab = ylab, title="World", file_pre = file_pre,ylim=c(0,7000)) #,ylim=c(-500,8000)

tmp<-ggplot_gtable(ggplot_build(j))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
a=a+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
b=b+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
c=c+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
ca=ca+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
e=e+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
i=i+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
id=id+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
j=j+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
r=r+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
u=u+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
k=k+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
w=w+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
lay<-rbind(c(1,2,3,4,5,6),c(7,8,9,10,11,12))
h=grid.arrange(a,b,c,ca,e,i,id,j,r,u,k,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/Ctax_natscens_gridarrange.png",sep=""),h,width=24,height=14,dpi=200)

vars="Price|Carbon"
range=all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"&!period%in%c(2015,2025,2035,2045,2055,2065,2075,2085,2095),
          list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","variable","period")]
m14 = ggplot(all[variable%in%vars & Category%in%c("BAU","CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m14 = m14 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m14 = m14 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m14 = m14 + xlim(2000,2050) + scale_y_continuous(breaks=c(0,200,400,600,800,1000,1200,1400,1600,1800,2000),limits=c(0,2000))
m14 = m14 + scale_colour_manual(values=plotstyle(scens))
m14 = m14 + scale_fill_manual(values=plotstyle(scens))
m14 = m14 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m14 = m14 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m14 = m14 + theme(legend.position="bottom")
m14
ggsave(file=paste(cfg$outdir,"/Cprice_all_global_models_world_BAU-CurPol-NDC-GPP-2Deg2030-Bridge-2Deg2020_funnel.png",sep=""),m14,width=16,height=12,dpi=200)

# split up mitigation and reference scenarios for readability. TODO: put these together with grid arrange / facet grid
m14a = ggplot(all[variable%in%vars & Category%in%c("CurPol","NDCplus","NDCMCS","GPP")&!Scope=="national"&region=="World"]) 
m14a = m14a + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m14a = m14a + geom_ribbon(data=range[Category%in%c("CurPol","NDCplus","NDCMCS","GPP")],aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m14a = m14a + xlim(2000,2050) 
m14a = m14a + scale_y_continuous(breaks=c(0,20,40,60,80,100,120,140,160,180,200),limits=c(0,200))
m14a = m14a + scale_colour_manual(values=plotstyle(scens))
m14a = m14a + scale_fill_manual(values=plotstyle(scens))
m14a = m14a + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m14a = m14a + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m14a = m14a + theme(legend.position="bottom")
m14a
ggsave(file=paste(cfg$outdir,"/Cprice_all_global_models_world_CurPol-NDC-GPP_funnel.png",sep=""),m14a,width=16,height=12,dpi=200)

m14b = ggplot(all[variable%in%vars & Category%in%c("Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"]) 
m14b = m14b + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
m14b = m14b + geom_ribbon(data=range[Category%in%c("Bridge","2Deg2030","2Deg2020")],aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
m14b = m14b + xlim(2000,2050)
m14b = m14b + scale_y_continuous(breaks=c(0,200,400,600,800,1000,1200,1400,1600,1800,2000),limits=c(0,2000))
m14b = m14b + scale_colour_manual(values=plotstyle(scens))
m14b = m14b + scale_fill_manual(values=plotstyle(scens))
m14b = m14b + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
m14b = m14b + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
m14b = m14b + theme(legend.position="bottom")
m14b
ggsave(file=paste(cfg$outdir,"/Cprice_all_global_models_world_Bridge-2Deg2030-2Deg2020_funnel.png",sep=""),m14b,width=16,height=12,dpi=200)

# or as bar chart
cpricebar=all[variable%in%vars & Category%in%c("CurPol","NDCplus","NDCMCS","GPP","Bridge","2Deg2030","2Deg2020")&!Scope=="national"&region=="World"&period%in%c(2030,2040,2050)]
cpricebarm=range[period%in%c(2030,2040,2050)&!Category=="BAU"]
cpricebar$period=as.factor(cpricebar$period)
cpricebarm$period=as.factor(cpricebarm$period)
cpricebar$Category = factor(cpricebar$Category,levels=c("CurPol","NDCMCS","NDCplus","GPP","Bridge","2Deg2020","2Deg2030"))
cpricebarm$Category = factor(cpricebarm$Category,levels=c("CurPol","NDCMCS","NDCplus","GPP","Bridge","2Deg2020","2Deg2030"))

m14c = ggplot()
m14c = m14c + geom_bar(data=cpricebarm[Category%in%c("CurPol","NDCplus","Bridge","2Deg2020","2Deg2030")],aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
# m14c = m14c + geom_pointrange(data=emisredm[Category%in%c("CurPol","GPP","Bridge")&variable=="Rate of Change| Emissions|Kyoto Gases"],
#                         aes(ymin=min,ymax=max,y=median, x=period, colour=Category),alpha=0.5,size=5,fatten=1,position=position_dodge(width=0.66)) #,show.legend = F
m14c = m14c + geom_point(data=cpricebar[Category%in%c("CurPol","NDCplus","Bridge","2Deg2020","2Deg2030")], aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
m14c = m14c + ylim(0,2000)
m14c = m14c + scale_shape_manual(values=cfg$man_shapes)
m14c = m14c + scale_color_manual(values=plotstyle(scens))
m14c = m14c + scale_fill_manual(values=plotstyle(scens))
#m14c = m14c + facet_wrap(~region,scales="free_y")
m14c = m14c + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
m14c = m14c + ylab("Carbon price (US$2010/tCO2")
m14c
ggsave(file=paste(cfg$outdir,"/Carbon_price_bar.png",sep=""),m14c,width=18,height=12,dpi=300)

m14d = ggplot()
m14d = m14d + geom_bar(data=cpricebarm,aes(x=Category,y=median,fill=period),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
m14d = m14d + geom_point(data=cpricebar, aes(x=Category,y=value,shape=model,colour=period,group=period),size=3,position=position_dodge(width=0.66))
m14d = m14d + scale_shape_manual(values=cfg$man_shapes)
# m14d = m14d + scale_color_manual(values=plotstyle(scens))
# m14d = m14d + scale_fill_manual(values=plotstyle(scens))
m14d = m14d + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
m14d = m14d + ylab("Carbon price (US$2010/tCO2")
m14d
ggsave(file=paste(cfg$outdir,"/Carbon_price_bar_2.png",sep=""),m14d,width=18,height=12,dpi=300)


# Key paper figures -------------------------------------------------------
# settings
scens = c("CurPol","NDCplus","Bridge","2Deg2020") #"NDCMCS",
regio = c("World")
regions = c("AUS","BRA","CAN","CHN","EU","IDN","IND","JPN","ROK","RUS","USA")
year = c("2030")
years = c("2030","2050")
  
  # Energy system indicators ------------------------------------------------
#TODO for all graphs: connect individual model points over time to see trend (Lara to help)

### Figure elements 
# Figure 1a share of REN
REbar=REN[Category%in%scens&region%in%regio&period%in%years] # &!Scope=="national"
REbarm=REbar[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","variable","period")]
REbar$period=as.factor(REbar$period)
REbarm$period=as.factor(REbarm$period)
REbar$Category = factor(REbar$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020")) #,"NDCMCS"
REbarm$Category = factor(REbarm$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020"))

F1a = ggplot()
F1a = F1a + geom_bar(data=REbarm,aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F1a = F1a + geom_point(data=REbar, aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F1a = F1a + geom_errorbar(data=REbarm,aes(x=period,ymin=min,ymax=max,colour=Category),position=position_dodge(width=0.66))
F1a = F1a + geom_text(aes(x="2030",y=88),label ="a)",size=10)
F1a = F1a + scale_shape_manual(values=cfg$man_shapes)
F1a = F1a + scale_color_manual(values=plotstyle(scens))
F1a = F1a + scale_fill_manual(values=plotstyle(scens))
F1a = F1a + xlab("")
F1a = F1a + theme_bw() + theme(axis.text.y=element_text(size=18)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=18)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
F1a = F1a +ylab("Share of renewables in electricity (%)")
#F1a = F1a + ylab(paste(unique(REN$variable),"[",unique(REN$unit),"]"))
#F1a = F1a + geom_text(aes(x=2030,y=75),label="a)")
F1a
ggsave(file=paste(cfg$outdir,"/F1a_REN-share-elec_bar.png",sep=""),F1a,width=18,height=12,dpi=300)

# Figure 1b share of electric transport
EVbar=all[variable%in%c("Final Energy|Transportation|Electricity","Final Energy|Transportation")&Category%in%scens&!Scope=="national"& region%in%regio &period%in%years]
EVbar = spread(EVbar,variable,value)
EVbar = EVbar%>%mutate(EVshare= `Final Energy|Transportation|Electricity`/`Final Energy|Transportation` * 100 )
EVbar = data.table(gather(EVbar,variable,value,c("Final Energy|Transportation|Electricity","Final Energy|Transportation","EVshare")))
EVbar = EVbar[variable=="EVshare"]
EVbar$unit <- "%"

EVbarm=EVbar[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","variable","period")]
EVbar$period=as.factor(EVbar$period)
EVbarm$period=as.factor(EVbarm$period)
EVbar$Category = factor(EVbar$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020")) #,"NDCMCS"
EVbarm$Category = factor(EVbarm$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020"))

F1b = ggplot()
F1b = F1b + geom_bar(data=EVbarm,aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F1b = F1b + geom_point(data=EVbar, aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F1b = F1b + geom_errorbar(data=EVbarm,aes(x=period,ymin=min,ymax=max,colour=Category),position=position_dodge(width=0.66))
F1b = F1b + geom_text(aes(x="2030",y=30),label ="b)",size=10)
F1b = F1b + scale_shape_manual(values=cfg$man_shapes)
F1b = F1b + scale_color_manual(values=plotstyle(scens))
F1b = F1b + scale_fill_manual(values=plotstyle(scens))
F1b = F1b + xlab("")
F1b = F1b + theme_bw() + theme(axis.text.y=element_text(size=18)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=18)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
F1b = F1b + ylab(paste("Share of electricity in transportation final energy demand","[",unique(EVbar$unit),"]"))
F1b
ggsave(file=paste(cfg$outdir,"/F1b_EV-transport_bar.png",sep=""),F1b,width=18,height=12,dpi=300)

# Figure 1c Industry efficiency? Need value added... (only reported by IMAGE). For now CCS as it is part of protocol. Maybe add F-gases? 
CCSbar=all[variable%in%c("Carbon Sequestration|CCS|Fossil|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Industry")&Category%in%scens&!Scope=="national"& region%in%regio &period%in%years]
CCSbar = spread(CCSbar,variable,value)
CCSbar=na.omit(CCSbar)
CCSbar = CCSbar%>%mutate(CCSshare= `Carbon Sequestration|CCS|Fossil|Energy|Demand|Industry`/`Emissions|CO2|Energy|Demand|Industry` * 100 )
CCSbar = data.table(gather(CCSbar,variable,value,c("Carbon Sequestration|CCS|Fossil|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Industry","CCSshare")))
CCSbar = CCSbar[variable=="CCSshare"]
CCSbar$unit <- "%"

CCSbarm=CCSbar[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","variable","period")]
CCSbar$period=as.factor(CCSbar$period)
CCSbarm$period=as.factor(CCSbarm$period)
CCSbar$Category = factor(CCSbar$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020")) #,"NDCMCS"
CCSbarm$Category = factor(CCSbarm$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020"))

F1c = ggplot()
F1c = F1c + geom_bar(data=CCSbarm,aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F1c = F1c + geom_point(data=CCSbar, aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F1c = F1c + geom_errorbar(data=CCSbarm,aes(x=period,ymin=min,ymax=max,colour=Category),position=position_dodge(width=0.66))
#F1c = F1c + geom_text(aes(x="2030",y=80),label ="c)",size=10)
F1c = F1c + scale_shape_manual(values=cfg$man_shapes)
F1c = F1c + scale_color_manual(values=plotstyle(scens))
F1c = F1c + scale_fill_manual(values=plotstyle(scens))
F1c = F1c + xlab("")
F1c = F1c + theme_bw() + theme(axis.text.y=element_text(size=18)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=18)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
F1c = F1c + ylab(paste("Industrial CCS as share of industry CO2 emissions","[",unique(CCSbar$unit),"]"))
F1c
ggsave(file=paste(cfg$outdir,"/F1c_CCS-industry_bar.png",sep=""),F1c,width=18,height=12,dpi=300)

#Alternative: industrial process & F-gas emissions
IEbar=all[variable%in%c("Emissions|CO2|Industrial Processes","Emissions|F-Gases")&Category%in%scens&!Scope=="national"& region%in%regio &period%in%c(2015,2030,2050)]
IEbar$unit<-"Mt CO2-equiv/yr"
IEbar = spread(IEbar,variable,value)
IEbar=na.omit(IEbar)
IEbar = IEbar%>%mutate(IEtotal= `Emissions|CO2|Industrial Processes`+`Emissions|F-Gases` )
IEbar = data.table(gather(IEbar,variable,value,c("Emissions|CO2|Industrial Processes","Emissions|F-Gases","IEtotal")))
IEbar = IEbar[variable=="IEtotal"]
IEbar = spread(IEbar,period,value)
IEbar=na.omit(IEbar)
IEbar = IEbar%>%mutate(rel50= ((`2050`-`2015`)/`2015`)*100,rel30=((`2030`-`2015`)/`2015`)*100)
IEbar = data.table(gather(IEbar,period,value,c('2015','2030','2050','rel30','rel50')))
IEbar = IEbar[period%in%c("rel50","rel30")]
IEbar$unit <- "%"
IEbar[period=="rel50"]$period<-2050
IEbar[period=="rel30"]$period<-2030
  
IEbarm=IEbar[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","variable","period")]
IEbar$period=as.factor(IEbar$period)
IEbarm$period=as.factor(IEbarm$period)
IEbar$Category = factor(IEbar$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020")) #,"NDCMCS"
IEbarm$Category = factor(IEbarm$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020"))

F1c2 = ggplot()
F1c2 = F1c2 + geom_bar(data=IEbarm,aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F1c2 = F1c2 + geom_point(data=IEbar, aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F1c2 = F1c2 + geom_errorbar(data=IEbarm,aes(x=period,ymin=min,ymax=max,colour=Category),position=position_dodge(width=0.66))
F1c2 = F1c2 + geom_text(aes(x="2030",y=80),label ="c)",size=10)
F1c2 = F1c2 + scale_shape_manual(values=cfg$man_shapes)
F1c2 = F1c2 + scale_color_manual(values=plotstyle(scens))
F1c2 = F1c2 + scale_fill_manual(values=plotstyle(scens))
F1c2 = F1c2 + xlab("")
F1c2 = F1c2 + theme_bw() + theme(axis.text.y=element_text(size=18)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=18)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
F1c2 = F1c2 + ylab(paste("F-Gases and Industrial process CO2 emissions","[relative to 2015, ",unique(IEbar$unit),"]"))
F1c2
ggsave(file=paste(cfg$outdir,"/F1c2_emissions-industry_bar.png",sep=""),F1c2,width=18,height=12,dpi=300)

# Figure 1d Buildings share of electricity / efficiency?
EBbar=all[variable%in%c("Final Energy|Residential and Commercial|Electricity","Final Energy|Residential and Commercial")&Category%in%scens&!Scope=="national"& region%in%regio &period%in%years]
EBbar = spread(EBbar,variable,value)
EBbar = EBbar%>%mutate(EBshare= `Final Energy|Residential and Commercial|Electricity`/`Final Energy|Residential and Commercial` * 100 )
EBbar = data.table(gather(EBbar,variable,value,c("Final Energy|Residential and Commercial|Electricity","Final Energy|Residential and Commercial","EBshare")))
EBbar = EBbar[variable=="EBshare"]
EBbar$unit <- "%"

EBbarm=EBbar[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","variable","period")]
EBbar$period=as.factor(EBbar$period)
EBbarm$period=as.factor(EBbarm$period)
EBbar$Category = factor(EBbar$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020")) #,"NDCMCS"
EBbarm$Category = factor(EBbarm$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020"))

F1d = ggplot()
F1d = F1d + geom_bar(data=EBbarm,aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F1d = F1d + geom_point(data=EBbar, aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F1d = F1d + geom_errorbar(data=EBbarm,aes(x=period,ymin=min,ymax=max,colour=Category),position=position_dodge(width=0.66))
F1d = F1d + geom_text(aes(x="2030",y=70),label ="d)",size=10)
F1d = F1d + scale_shape_manual(values=cfg$man_shapes)
F1d = F1d + scale_color_manual(values=plotstyle(scens))
F1d = F1d + scale_fill_manual(values=plotstyle(scens))
F1d = F1d + xlab("")
F1d = F1d + theme_bw() + theme(axis.text.y=element_text(size=18)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=18)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
F1d = F1d + ylab(paste("Share of electricity in buildings final energy demand","[",unique(EBbar$unit),"]"))
F1d
ggsave(file=paste(cfg$outdir,"/F1d_Elec-buildings_bar.png",sep=""),F1d,width=18,height=12,dpi=300)

# Figure 1e? AFOLU
#"Emissions|CH4|AFOLU"
#"Emissions|N2O|AFOLU"
AFbar=all[variable%in%c("Land Cover|Forest|Afforestation and Reforestation")&Category%in%scens&!Scope=="national"& region%in%regio &period%in%years]

AFbarm=AFbar[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","variable","period")]
AFbar$period=as.factor(AFbar$period)
AFbarm$period=as.factor(AFbarm$period)
AFbar$Category = factor(AFbar$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020")) #,"NDCMCS"
AFbarm$Category = factor(AFbarm$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020"))

F1e = ggplot()
F1e = F1e + geom_bar(data=AFbarm,aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F1e = F1e + geom_point(data=AFbar, aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F1e = F1e + geom_errorbar(data=AFbarm,aes(x=period,ymin=min,ymax=max,colour=Category),position=position_dodge(width=0.66))
F1e = F1e + geom_text(aes(x="2030",y=450),label ="e)",size=10)
F1e = F1e + scale_shape_manual(values=cfg$man_shapes)
F1e = F1e + scale_color_manual(values=plotstyle(scens))
F1e = F1e + scale_fill_manual(values=plotstyle(scens))
F1e = F1e + xlab("")
F1e = F1e + theme_bw() + theme(axis.text.y=element_text(size=18)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=18)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
F1e = F1e + ylab(paste("Afforestation and reforestation","[",unique(AFbar$unit),"]"))
F1e
ggsave(file=paste(cfg$outdir,"/F1e_Afforestation_bar.png",sep=""),F1e,width=18,height=12,dpi=300)

# Figure 1f multi-model FE/PE stack
vars=c("Primary Energy|Biomass|w/ CCS","Primary Energy|Biomass|w/o CCS","Primary Energy|Coal|w/ CCS","Primary Energy|Coal|w/o CCS","Primary Energy|Gas|w/ CCS","Primary Energy|Gas|w/o CCS",
       "Primary Energy|Geothermal","Primary Energy|Hydro","Primary Energy|Nuclear","Primary Energy|Oil|w/ CCS","Primary Energy|Oil|w/o CCS","Primary Energy|Other","Primary Energy|Solar","Primary Energy|Wind",
       "Primary Energy|Ocean","Primary Energy|Secondary Energy Trade")
PEstack=all[variable%in%vars&Category%in%scens&!Scope=="national"& region%in%regio &period%in%2030]
PEstack$Category = factor(PEstack$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020"))

F1f = ggplot(data=PEstack) #TODO different year? #[!Category=="NDCplus"]
F1f = F1f + geom_bar(aes(x=Category,y=value,fill=variable),stat="identity", position="stack",width=0.5)
F1f = F1f + facet_wrap(~model,nrow=1,labeller = labeller(model=c("IMAGE 3.0"="IMAGE","REMIND-MAgPIE 1.7-3.0"="REMIND", "POLES GECO2019"="POLES","AIM/CGE"="AIM/CGE","COPPE-COFFEE 1.0"="COFFEE","PROMETHEUS"="PROMETHEUS","MESSAGEix-GLOBIOM_1.0"="MESSAGE","WITCH 5.0"="WITCH","TIAM_Grantham_v3.2"="TIAM")))
F1f = F1f + scale_fill_manual(values=plotstyle(vars))
F1f = F1f + theme_bw() + theme(axis.text.y=element_text(size=18)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=18,angle=90)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12)) + theme(panel.spacing = unit(0, "lines"))
F1f = F1f + ylab(paste("Primary energy by source","[",unique(PEstack$unit),"]"))+xlab("")
F1f
ggsave(file=paste(cfg$outdir,"/F1f_PE-stack-model-scens_bar.png",sep=""),F1f,width=18,height=12,dpi=300)

# Figure 1X Rate of change in demand / supply (emissions?) TODO

### Figure collection
tmp<-ggplot_gtable(ggplot_build(F1a))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
F1a=F1a+theme(legend.position = "none")#+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
F1b=F1b+theme(legend.position = "none")#+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
F1c2=F1c2+theme(legend.position = "none")#+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
F1d=F1d+theme(legend.position = "none")#+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
F1e=F1e+theme(legend.position = "none")#+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
#F1f=F1f+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
lay<-rbind(c(1,2,3,4),c(5,6,7,7))
F1=grid.arrange(F1a,F1b,F1c2,legend,F1d,F1e,F1f,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/F1_gridarrange.png",sep=""),F1,width=24,height=14,dpi=200)

## alternative: only panels a-e
F1a=F1a+theme(legend.position = "right",legend.text=element_text(size=22),legend.title=element_text(size=24))
tmp<-ggplot_gtable(ggplot_build(F1a))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
F1a=F1a+theme(legend.position = "none",axis.text.x=element_text(size=22),axis.text.y=element_text(size=22))
F1b=F1b+theme(axis.text.x=element_text(size=22),axis.text.y=element_text(size=22))
F1c2=F1c2+theme(axis.text.x=element_text(size=22),axis.text.y=element_text(size=22))
F1d=F1d+theme(axis.text.x=element_text(size=22),axis.text.y=element_text(size=22))
F1e=F1e+theme(axis.text.x=element_text(size=22),axis.text.y=element_text(size=22))
lay<-rbind(c(1,2,3),c(4,5,6))
F1alt=grid.arrange(F1a,F1b,F1c2,F1d,F1e,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/F1_gridarrange_alt.png",sep=""),F1alt,width=24,height=14,dpi=200)

  # Waterfall ---------------------------------------------------------------

### Figure elements
# Figure 2a Sectors
# select data
cdata=all[model=="COPPE-COFFEE 1.0"&region=="World"] # POLES GECO2019, AIM/CGE, IMAGE 3.0, PROMETHEUS, REMIND-MAgPIE 1.7-3.0, COPPE-COFFEE 1.0,MESSAGEix-GLOBIOM_1.0, WITCH 5.0, TIAM_Grantham_v3.2
model=unique(cdata$model)

# add non-CO2
if(!model%in%c("TIAM_Grantham_v3.2","PROMETHEUS")){
nonco2=cdata[variable%in%c("Emissions|CH4","Emissions|N2O","Emissions|F-Gases")]
nonco2$unit<-NULL
nonco2=spread(nonco2,variable,value)
nonco2=nonco2%>%mutate(`Emissions|Non-CO2`=((`Emissions|CH4`*25)+(`Emissions|N2O`*298/1000)+`Emissions|F-Gases`))
nonco2=data.table(gather(nonco2,variable,value,c("Emissions|CH4","Emissions|N2O","Emissions|F-Gases","Emissions|Non-CO2")))
nonco2=nonco2[variable=="Emissions|Non-CO2"]
nonco2$unit<-"Mt CO2-equiv/yr"
setcolorder(nonco2,colnames(cdata))
cdata=rbind(cdata,nonco2)
}
if(unique(cdata$model=="AIM/CGE")){cdata$model<-"AIM-CGE"}

# source script
source("waterfall_bridge.R")

# Figure 2b countries
cdata=all[model=="COPPE-COFFEE 1.0"&region%in%c("R5ASIA","R5LAM","R5REF","R5OECD90+EU","R5MAF")&variable=="Emissions|Kyoto Gases"] # POLES GECO2019, AIM/CGE, IMAGE 3.0, REMIND-MAgPIE 1.7-3.0, COPPE-COFFEE 1.0,MESSAGEix-GLOBIOM_1.0, WITCH 5.0
if(unique(cdata$model=="AIM/CGE")){cdata$model<-"AIM-CGE"}
source("waterfall_bridge_regions.R")

# for PROMETHEUS and TIAM for CO2 instead of GHG
cdata=all[model=="TIAM_Grantham_v3.2"&region%in%c("R5ASIA","R5LAM","R5REF","R5OECD90+EU","R5MAF")&variable=="Emissions|CO2"] #TIAM_Grantham_v3.2, PROMETHEUS
source("waterfall_bridge_regions.R")

### Figure collection
#TODO: models as panel (1 figure sector, 1 figure region, latter to SI?)

  # Emissions ---------------------------------------------------------------

### Figure elements
# Figure 3a GHG emissions pathways
vars="Emissions|Kyoto Gases"
scens <- c("CurPol","NDCplus","Bridge","2Deg2020") #"NDCMCS",

plotdata=all[variable%in%vars & Category%in%scens&!Scope=="national"&region=="World"]
#plotdata$period=as.numeric(as.character(plotdata$period))
range=all[variable%in%vars & Category%in%scens&!Scope=="national"&region=="World",list(min=min(value,na.rm=T),max=max(value,na.rm=T),med=median(value,na.rm=T)),by=c("Category","variable","period")]
#range$period=as.numeric(as.character(range$period))
F3a = ggplot(plotdata) 
F3a = F3a + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
F3a = F3a + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
F3a = F3a + geom_segment(data=range[period %in% c(2050) & Category=="CurPol"], stat="identity", aes(x=2050, xend=2050, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3a = F3a + geom_segment(data=range[period %in% c(2050) & Category=="NDCplus"], stat="identity", aes(x=2050, xend=2050, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3a = F3a + geom_segment(data=range[period %in% c(2050) & Category=="Bridge"], stat="identity", aes(x=2050, xend=2050, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3a = F3a + geom_segment(data=range[period %in% c(2050) & Category=="2Deg2020"], stat="identity", aes(x=2050.5, xend=2050.5, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3a = F3a + geom_segment(data=range[period %in% c(2030) & Category=="Bridge"], stat="identity", aes(x=2030, xend=2030, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3a = F3a + geom_segment(data=range[period %in% c(2030) & Category=="2Deg2020"], stat="identity", aes(x=2030.5, xend=2030.5, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3a = F3a + geom_point(data=range[period %in% c(2050)&Category%in%c("2Deg2020")],aes(x=2050.7,y=med,colour=Category,size=1.5),show.legend = FALSE)
F3a = F3a + geom_point(data=range[period %in% c(2050)&Category%in%c("Bridge","CurPol","NDCplus")],aes(x=2050.2,y=med,colour=Category,size=1.5),show.legend = FALSE)
F3a = F3a + geom_point(data=range[period %in% c(2030)&Category%in%c("2Deg2020")],aes(x=2030.7,y=med,colour=Category,size=1.5),show.legend = FALSE)
F3a = F3a + geom_point(data=range[period %in% c(2030)&Category%in%c("Bridge")],aes(x=2030.2,y=med,colour=Category,size=1.5),show.legend = FALSE)
F3a = F3a + xlim(2010,2051)+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
F3a = F3a + scale_colour_manual(values=plotstyle(scens))
F3a = F3a + scale_fill_manual(values=plotstyle(scens))
F3a = F3a + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
F3a = F3a + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=14),legend.title=element_text(size=14))
F3a = F3a + theme(legend.position="bottom")
F3a
ggsave(file=paste(cfg$outdir,"/F3a_GHG_all_global_models_world_CurPol-NDC-Bridge-2Deg2020_funnel.png",sep=""),F3a,width=16,height=12,dpi=200)

F3aSI = ggplot(all[variable%in%vars & Category%in%scens&!Scope=="national"&region=="World"]) 
F3aSI = F3aSI + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
#F3aSI = F3aSI + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
F3aSI = F3aSI + geom_segment(data=range[period %in% c(2100) & Category=="CurPol"], stat="identity", aes(x=2100, xend=2100, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3aSI = F3aSI + geom_segment(data=range[period %in% c(2100) & Category=="NDCplus"], stat="identity", aes(x=2100, xend=2100, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3aSI = F3aSI + geom_segment(data=range[period %in% c(2100) & Category=="Bridge"], stat="identity", aes(x=2100, xend=2100, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3aSI = F3aSI + geom_segment(data=range[period %in% c(2100) & Category=="2Deg2020"], stat="identity", aes(x=2100.5, xend=2100.5, y=min, yend=max, size=1.5, colour=Category), show.legend=FALSE) 
F3aSI = F3aSI + geom_point(data=range[period %in% c(2100)&Category%in%c("2Deg2020")],aes(x=2100.7,y=med,colour=Category,size=1.5),show.legend = FALSE)
F3aSI = F3aSI + geom_point(data=range[period %in% c(2100)&Category%in%c("Bridge","CurPol","NDCplus")],aes(x=2100.2,y=med,colour=Category,size=1.5),show.legend = FALSE)
F3aSI = F3aSI + xlim(2050,2101) + scale_y_continuous(breaks=c(-20000,-10000,0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000),limits=c(-20000,100000))
F3aSI = F3aSI + scale_colour_manual(values=plotstyle(scens))
F3aSI = F3aSI + scale_fill_manual(values=plotstyle(scens))
F3aSI = F3aSI + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
F3aSI = F3aSI + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=14),legend.title=element_text(size=14))
F3aSI = F3aSI + theme(legend.position="bottom")
F3aSI
ggsave(file=paste(cfg$outdir,"/F3aSI_GHG_all_global_models_world_CurPol-NDC-Bridge-2Deg2020_funnel.png",sep=""),F3aSI,width=16,height=12,dpi=200)

scens=c("GPP_notax","Bridge_notax","Bridge","GPP")
mods=unique(all[Category=="GPP_notax"]$model)
range=all[variable%in%vars & Category%in%scens&!Scope=="national"&region=="World"&model%in%mods,list(min=min(value,na.rm=T),max=max(value,na.rm=T),med=median(value,na.rm=T)),by=c("Category","variable","period")]
F3aSI2 = ggplot(all[variable%in%vars & Category%in%scens&!Scope=="national"&region=="World"&model%in%mods]) 
F3aSI2 = F3aSI2 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
F3aSI2 = F3aSI2 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
F3aSI2 = F3aSI2 + xlim(2010,2051)+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
F3aSI2 = F3aSI2 + scale_colour_manual(values=plotstyle(scens))
F3aSI2 = F3aSI2 + scale_fill_manual(values=plotstyle(scens))
F3aSI2 = F3aSI2 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
F3aSI2 = F3aSI2 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
F3aSI2 = F3aSI2 + theme(legend.position="bottom")
F3aSI2
ggsave(file=paste(cfg$outdir,"/F3aSI2_GHG_all_global_models_world_GPP-Bridge-notax_funnel.png",sep=""),F3aSI2,width=16,height=12,dpi=200)

scens=c("Bridge","GPP")
mods=c("IMAGE 3.0","MESSAGEix-GLOBIOM_1.0") #,"AIM/CGE" "PROMETHEUS"
range=all[variable%in%vars & Category%in%scens&!Scope=="national"&region=="World"&model%in%mods,list(min=min(value,na.rm=T),max=max(value,na.rm=T),med=median(value,na.rm=T)),by=c("Category","variable","period")]
F3aSI3 = ggplot(all[variable%in%vars & Category%in%scens&!Scope=="national"&region=="World"&model%in%mods]) 
F3aSI3 = F3aSI3 + geom_line(aes(x=period,y=value,colour=Category, linetype=model),size=1.5)
#F3aSI3 = F3aSI3 + geom_ribbon(data=range,aes(x=period,ymin=min, ymax=max,fill=Category),alpha=0.5)
F3aSI3 = F3aSI3 + xlim(2010,2051)+ scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000),limits=c(0,85000))
F3aSI3 = F3aSI3 + scale_colour_manual(values=plotstyle(scens))
F3aSI3 = F3aSI3 + scale_fill_manual(values=plotstyle(scens))
F3aSI3 = F3aSI3 + ylab(paste(unique(all[variable%in%vars]$variable),"[",unique(all[variable%in%vars]$unit),"]"))+ xlab("")
F3aSI3 = F3aSI3 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=16,angle=90)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
F3aSI3 = F3aSI3 + theme(legend.position="bottom")
F3aSI3
ggsave(file=paste(cfg$outdir,"/F3aSI3_GHG_IMAGE-MESSAGE_world_GPP-Bridge_funnel.png",sep=""),F3aSI3,width=16,height=12,dpi=200)

# Figure 3b Emissions relative to NDC

# Figure 3c Rate of change
scens <- c("CurPol","NDCplus","Bridge","2Deg2020") #"NDCMCS",
emisrednew = all[variable%in%c("Emissions|Kyoto Gases","Emissions|CO2")&Category%in%scens&period%in%c(2015,2030,2050)]
emisrednew = spread(emisrednew,period,value)
emisrednew=na.omit(emisrednew)
emisrednew = emisrednew%>%mutate(rel50= ((`2050`-`2015`)/`2015`)*100,rel30=((`2030`-`2015`)/`2015`)*100)
emisrednew = data.table(gather(emisrednew,period,value,c('2015','2030','2050','rel30','rel50')))
emisrednew = emisrednew[period%in%c("rel50","rel30")]
emisrednew$unit <- "%"
emisrednew[period=="rel50"]$period<-2050
emisrednew[period=="rel30"]$period<-2030

emisrednewm = emisrednew[,list(median=median(value,na.rm=T),min=min(value,na.rm=T),max=max(value,na.rm=T)),
                   by=c("Category","region","variable","unit","period")] #,min=min(value,na.rm=T),max=max(value,na.rm=T)
emisrednew$Category = factor(emisrednew$Category,levels=c("CurPol","NDCplus","Bridge","2Deg2020"))
emisrednewm$Category = factor(emisrednewm$Category,levels=c("CurPol","NDCplus","Bridge","2Deg2020"))

F3c = ggplot()
F3c = F3c + geom_bar(data=emisrednewm[Category%in%scens&variable=="Emissions|Kyoto Gases"&region%in%c(regions,"World")],
                 aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F3c = F3c + geom_point(data=emisrednew[Category%in%scens&variable=="Emissions|Kyoto Gases"&region%in%c(regions,"World")],
                   aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F3c = F3c + scale_shape_manual(values=cfg$man_shapes)
F3c = F3c + scale_fill_manual(values=plotstyle(scens))
F3c = F3c + scale_colour_manual(values=plotstyle(scens))
F3c = F3c + facet_wrap(~region,scales="free_y")
F3c = F3c + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
F3c = F3c + ylab("GHG emissions relative to 2015 (%)")
F3c
ggsave(file=paste(cfg$outdir,"/F3c-GHG-emissions-reduction.png",sep=""),F3c,width=18,height=12,dpi=300)

### Figure collection

  # Costs / investments -----------------------------------------------------

### Figure elements
# Figure 4a carbon price #TODO check differences between 3 tiers as in protocol
vars=c("Price|Carbon")
scens <- c("CurPol","NDCplus","Bridge","2Deg2020")
cpricebar=all[variable%in%vars & Category%in%scens&!Scope=="national"&region%in%c("R5OECD90+EU","R5LAM","R5MAF")&period%in%c(2030,2050)]
cpricebarm=cpricebar[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","region","variable","period")]

cpricebar$period=as.factor(cpricebar$period)
cpricebarm$period=as.factor(cpricebarm$period)
cpricebar$Category = factor(cpricebar$Category,levels=c("CurPol","NDCplus","Bridge","2Deg2020"))
cpricebarm$Category = factor(cpricebarm$Category,levels=c("CurPol","NDCplus","Bridge","2Deg2020"))

F4a = ggplot()
F4a = F4a + geom_bar(data=cpricebarm,aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F4a = F4a + geom_point(data=cpricebar, aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F4a = F4a + facet_wrap(~region,nrow=1)
F4a = F4a + scale_shape_manual(values=cfg$man_shapes)
F4a = F4a + scale_color_manual(values=plotstyle(scens))
F4a = F4a + scale_fill_manual(values=plotstyle(scens))
F4a = F4a + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=11),legend.title=element_text(size=12)) + theme(panel.spacing = unit(0, "lines"))
F4a = F4a + ylab("Carbon price (US$2010/tCO2")
F4a
ggsave(file=paste(cfg$outdir,"/F4a_Carbon_price_bar.png",sep=""),F4a,width=18,height=12,dpi=300)

# Figure 4b policy costs
costsGDP = fread("data/policy costs.csv",sep=";", header=T)
costsGDP = data.table(gather(costsGDP,period,value,c(`2030`,`2050`)))
costsGDP = spread(costsGDP,Scenario,value)
costsGDP = costsGDP%>%mutate(Bridgevs2020 = ((Bridge_V4 / `2Deg2020_V4`)-1)*100, Bridgevs2030 = ((Bridge_V4 / `2deg2030_v4` )-1)*100)
costsGDP = data.table(gather(costsGDP,Scenario,value,c('2Deg2020_V4','2deg2030_v4','Bridge_V4','Bridgevs2020','Bridgevs2030')))
costsGDP = costsGDP[Scenario%in%c('Bridgevs2020','Bridgevs2030')]

costsGDPm=costsGDP[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Scenario","period")]

F4b = ggplot()
F4b = F4b + geom_bar(data=costsGDPm,aes(x=period,y=median,fill=Scenario),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F4b = F4b + geom_point(data=costsGDP, aes(x=period,y=value,shape=Model,colour=Scenario,group=Scenario),size=3,position=position_dodge(width=0.66))
F4b = F4b + geom_errorbar(data=costsGDPm,aes(x=period,ymin=min,ymax=max,colour=Scenario),position=position_dodge(width=0.66))
F4b = F4b + scale_shape_manual(values=cfg$man_shapes)
F4b = F4b + scale_color_manual(values=c("Bridgevs2020"="#56B4E9","Bridgevs2030"="#2860E9"),labels=c("Bridgevs2020"="Bridge vs 2Deg2020","Bridgevs2030"="Bridge vs 2Deg2030"))
F4b = F4b + scale_fill_manual(values=c("Bridgevs2020"="#56B4E9","Bridgevs2030"="#2860E9"),labels=c("Bridgevs2020"="Bridge vs 2Deg2020","Bridgevs2030"="Bridge vs 2Deg2030"))
F4b = F4b + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
F4b = F4b + ylab("GDP loss in Bridge relative to 2Deg2020 or 2Deg2030 (%)")
F4b
ggsave(file=paste(cfg$outdir,"/F4b_policy_costs_GDP_bar.png",sep=""),F4b,width=18,height=12,dpi=300)

# Figure 4c Investments
# potential indicators:
# i.	Investment|Energy supply 
# ii.	Total investments 
# iii.	Investment|Energy Efficiency 
# iv.	Investment|Energy Supply|CO2 Transport and Storage 
# v.	Investment|Energy Supply|Electricity|Transmission and Distribution 
# vi.	Investment|Energy Supply|Electricity|Electricity Storage 
# vii.	Investment|Energy Supply|Electricity|Nuclear 
# viii.	Investment|Energy Supply|Extraction|Bioenergy 
# ix.	Investment|Energy Supply|Electricity|Non-Biomass Renewables
# x.	Investment|Energy Supply|Hydrogen|Fossil
# xi.	Investment|Energy Supply|Hydrogen|Renewable
# xii.	Investment|Energy Supply|Electricity|Fossil  - or better:
#   1.	Investment|Energy Supply|Electricity|Oil|w/o CCS
# 2.	Investment|Energy Supply|Electricity|Gas|w/o CCS
# 3.	Investment|Energy Supply|Electricity|Coal|w/o CCS
# xiii.	Investment|Energy Supply|Extraction|Fossil 
# xiv.	Investment|Infrastructure|Residential and Commercial|Building Retrofits (because of the protocol)
# xv.	Investment|Energy Demand|Transportation|Passenger|Road|LDV|EV (because of the protocol)

vars=c("Investment|Energy Supply|Electricity|Non-Biomass Renewables","Investment|Energy Supply|Electricity|Fossil") 
scens <- c("CurPol","NDCplus","Bridge","2Deg2020")

INVbar=all[variable%in%vars&Category%in%scens&region%in%regio&period%in%years] 
INVbarm=INVbar[,list(min=min(value,na.rm=T),max=max(value,na.rm=T),median=median(value,na.rm=T)),by=c("Category","variable","period")]
INVbar$period=as.factor(INVbar$period)
INVbarm$period=as.factor(INVbarm$period)
INVbar$Category = factor(INVbar$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020")) 
INVbarm$Category = factor(INVbarm$Category,levels=c("CurPol","NDCplus","NDCMCS","Bridge","2Deg2020"))

F4c1 = ggplot()
F4c1 = F4c1 + geom_bar(data=INVbarm[variable=="Investment|Energy Supply|Electricity|Fossil"],aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F4c1 = F4c1 + geom_point(data=INVbar[variable=="Investment|Energy Supply|Electricity|Fossil"], aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F4c1 = F4c1 + geom_errorbar(data=INVbarm[variable=="Investment|Energy Supply|Electricity|Fossil"],aes(x=period,ymin=min,ymax=max,colour=Category),position=position_dodge(width=0.66))
F4c1 = F4c1 + scale_shape_manual(values=cfg$man_shapes)
F4c1 = F4c1 + scale_color_manual(values=plotstyle(scens))
F4c1 = F4c1 + scale_fill_manual(values=plotstyle(scens))
F4c1 = F4c1 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
F4c1 = F4c1 + ylab(paste(unique(INVbar[variable=="Investment|Energy Supply|Electricity|Fossil"]$variable),"[",unique(INVbar$unit),"]"))
F4c1
ggsave(file=paste(cfg$outdir,"/F4c_Investments_fossil_bar.png",sep=""),F4c1,width=18,height=12,dpi=300)

F4c2 = ggplot()
F4c2 = F4c2 + geom_bar(data=INVbarm[variable=="Investment|Energy Supply|Electricity|Non-Biomass Renewables"],aes(x=period,y=median,fill=Category),stat="identity",alpha=0.5, position=position_dodge(width=0.66),width=0.66)
F4c2 = F4c2 + geom_point(data=INVbar[variable=="Investment|Energy Supply|Electricity|Non-Biomass Renewables"], aes(x=period,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=0.66))
F4c2 = F4c2 + geom_errorbar(data=INVbarm[variable=="Investment|Energy Supply|Electricity|Non-Biomass Renewables"],aes(x=period,ymin=min,ymax=max,colour=Category),position=position_dodge(width=0.66))
F4c2 = F4c2 + scale_shape_manual(values=cfg$man_shapes)
F4c2 = F4c2 + scale_color_manual(values=plotstyle(scens))
F4c2 = F4c2 + scale_fill_manual(values=plotstyle(scens))
F4c2 = F4c2 + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
  theme(axis.text.x = element_text(size=14)) + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
F4c2 = F4c2 + ylab(paste(unique(INVbar[variable=="Investment|Energy Supply|Electricity|Non-Biomass Renewables"]$variable),"[",unique(INVbar$unit),"]"))
F4c2
ggsave(file=paste(cfg$outdir,"/F4c_Investments_NBR_bar.png",sep=""),F4c2,width=18,height=12,dpi=300)
