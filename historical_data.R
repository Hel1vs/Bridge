# Historical data PRIMAP and EDGAR --------------------------------------------------


# Historical data from PRIMAP
# http://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:2959897
PRIMAP <- read.csv("data/PRIMAP-hist_v1.2_14-Dec-2017.csv", header=TRUE, sep=",") #PRIMAP-hist_v2.1_09-Nov-2019
PRIMAP_CDLINKS <- PRIMAP
regions_indicators_CDLINKS_history <- c("AUS","CAN", "BRA", "CHN", "EU28", "IDN", "IND", "JPN", "RUS", "USA","KOR", "EARTH")
#years_indicators_CDLINKS_history <- c("X1990", "X1995", "X2000", "X2005", "X2010", "X2015")
colnames(PRIMAP_CDLINKS)[colnames(PRIMAP_CDLINKS)=="country"] <- "region"
PRIMAP_CDLINKS <- filter(PRIMAP_CDLINKS, region %in% regions_indicators_CDLINKS_history)
PRIMAP_CDLINKS$region=str_replace_all(PRIMAP_CDLINKS$region,"EARTH","World")
PRIMAP_CDLINKS$region=str_replace_all(PRIMAP_CDLINKS$region,"EU28","EU")
PRIMAP_CDLINKS$scenario=str_replace_all(PRIMAP_CDLINKS$scenario,"HISTORY","History")

# Historical data from EDGAR
# http://edgar.jrc.ec.europa.eu/overview.php?v=432_GHG&SECURE=123
EDGAR <- read.csv("data/EDGAR.csv", header=TRUE, sep=";")
EDGAR_bunkers <- filter(EDGAR, Name %in% c('Int. Aviation', 'Int. Shipping'))
colnames(EDGAR_bunkers) = gsub("X", "", colnames(EDGAR_bunkers))
colnames(EDGAR_bunkers)[colnames(EDGAR_bunkers)=="World.Region"] <- "region"
EDGAR_bunkers <- gather(EDGAR_bunkers, 7:ncol(EDGAR_bunkers), key="year", value=value) %>%
  select(-IPCC.Annex, -ISO_A3, -Name, -IPCC, -IPCC_description)
EDGAR_bunkers_total <- group_by(EDGAR_bunkers, year) %>% summarize(value=sum(value)) %>%
  mutate(region="Bunkers") %>%
  select(year, region, value)
EDGAR_bunkers_total$value <- 10^-3*EDGAR_bunkers_total$value
EDGAR_bunkers_total$unit <- "Mt CO2eq/yr"

# Historical data Figure 1 ------------------------------------------------

# for figure 1)
# select data for Total Kyoto emissions from 1990 and transfer to MtCO2eq
PRIMAP_selec_Kyoto <- filter(PRIMAP_CDLINKS, category=="CAT0", entity=="KYOTOGHGAR4")
PRIMAP_selec_Kyoto <- select(PRIMAP_selec_Kyoto, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_Kyoto <- cbind(PRIMAP_selec_Kyoto[1:5], PRIMAP_selec_Kyoto[, 6:ncol(PRIMAP_selec_Kyoto)]/1000)
# make dataframe structure for Kyoto emissions equal to data used for figure
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, variable="Emissions|Kyoto Gases")
PRIMAP_selec_Kyoto$unit <- "Mt CO2eq/yr"
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, source="PRIMAP")
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, statistic="value")
PRIMAP_selec_Kyoto <- select(PRIMAP_selec_Kyoto, variable, scenario, region, unit, source, statistic, num_range("X", 1970:2015))
colnames(PRIMAP_selec_Kyoto) = gsub("X", "", colnames(PRIMAP_selec_Kyoto))
# add missing 2013, 2014, 2015 data
tmp1 <- filter(EDGAR_bunkers_total, year==2012)
tmp1$year <- 2013
tmp2 <- filter(EDGAR_bunkers_total, year==2012)
tmp2$year <- 2014
tmp3 <- filter(EDGAR_bunkers_total, year==2012)
tmp3$year <- 2015
EDGAR_bunkers_total_adj <- rbind(EDGAR_bunkers_total, tmp1) %>% rbind(tmp2) %>% rbind(tmp3)
# add bunkers to World total
tmp <- filter(PRIMAP_selec_Kyoto, region=="World")
tmp <- gather(tmp, 7:ncol(PRIMAP_selec_Kyoto), key="year", value=value)
tmp <- inner_join(tmp, EDGAR_bunkers_total_adj, by=c('year')) %>% 
  mutate(value=value.x+value.y) %>%
  select(variable, scenario, region.x, unit.x, source, statistic, year, value) %>%
  rename(region=region.x, unit=unit.x)
tmp <- as.data.frame(tmp)
tmp <- spread(tmp, key=year, value=value)
PRIMAP_selec_Kyoto <- filter(PRIMAP_selec_Kyoto, region!="World")
PRIMAP_selec_Kyoto <- rbind(PRIMAP_selec_Kyoto, tmp)
PRIMAP_selec_Kyoto_output <- select(PRIMAP_selec_Kyoto, variable, scenario, region, unit, source, statistic, num_range("", 1990:2015))
write.table(PRIMAP_selec_Kyoto_output, file="Stocktaketool/History_Kyoto.csv", sep=";", row.names = FALSE)

# Historical data Figure 2, 3 -----------------------------------------

# for figure 2-3)
# Individual gases, split up in 'CO2 excl AFOLU CO2' and "AFOLU CO2"
PRIMAP_Fig2_3 <- PRIMAP_CDLINKS

PRIMAP_selec_CO2 <- filter(PRIMAP_Fig2_3, category=="CAT0", entity=="CO2")
PRIMAP_selec_CO2 <- select(PRIMAP_selec_CO2, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_CO2 <- cbind(PRIMAP_selec_CO2[1:5], PRIMAP_selec_CO2[, 6:ncol(PRIMAP_selec_CO2)]/1000)
PRIMAP_selec_CO2$unit <- "Mt CO2eq/yr"
PRIMAP_selec_CO2 <- data.frame(PRIMAP_selec_CO2)
PRIMAP_selec_CO2 <- mutate(PRIMAP_selec_CO2, variable="Emissions|CO2") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_CO2) = gsub("X", "", colnames(PRIMAP_selec_CO2))
PRIMAP_selec_CO2 <- select(PRIMAP_selec_CO2, -category, -entity)
write.table(PRIMAP_selec_CO2, file="Stocktaketool/History_CO2.csv", sep=";", row.names = FALSE)

PRIMAP_selec_Agriculture_CO2 <- filter(PRIMAP_Fig2_3, category=="CAT4", entity=="CO2")
PRIMAP_selec_Agriculture_CO2 <- select(PRIMAP_selec_Agriculture_CO2, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_Agriculture_CO2 <- cbind(PRIMAP_selec_Agriculture_CO2[1:5], PRIMAP_selec_Agriculture_CO2[, 6:ncol(PRIMAP_selec_Agriculture_CO2)]/1000)
PRIMAP_selec_Agriculture_CO2$unit <- "Mt CO2eq/yr"
PRIMAP_selec_Agriculture_CO2 <- data.frame(PRIMAP_selec_Agriculture_CO2)
PRIMAP_selec_Agriculture_CO2 <- mutate(PRIMAP_selec_Agriculture_CO2, variable="Emissions|CO2|Agriculture") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_Agriculture_CO2) = gsub("X", "", colnames(PRIMAP_selec_Agriculture_CO2))
PRIMAP_selec_Agriculture_CO2 <- select(PRIMAP_selec_Agriculture_CO2, -category, -entity)
write.table(PRIMAP_selec_Agriculture_CO2, file="Stocktaketool/History_CO2_Agriculture.csv", sep=";", row.names = FALSE)

PRIMAP_selec_LULUCF_CO2 <- filter(PRIMAP_Fig2_3, category=="CAT5", entity=="CO2")
PRIMAP_selec_LULUCF_CO2 <- select(PRIMAP_selec_LULUCF_CO2, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_LULUCF_CO2 <- cbind(PRIMAP_selec_LULUCF_CO2[1:5], PRIMAP_selec_LULUCF_CO2[, 6:ncol(PRIMAP_selec_LULUCF_CO2)]/1000)
PRIMAP_selec_LULUCF_CO2$unit <- "Mt CO2eq/yr"
PRIMAP_selec_LULUCF_CO2 <- data.frame(PRIMAP_selec_LULUCF_CO2)
PRIMAP_selec_LULUCF_CO2 <- mutate(PRIMAP_selec_LULUCF_CO2, variable="Emissions|CO2|LULUCF") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_LULUCF_CO2) = gsub("X", "", colnames(PRIMAP_selec_LULUCF_CO2))
PRIMAP_selec_LULUCF_CO2 <- select(PRIMAP_selec_LULUCF_CO2, -category, -entity)
write.table(PRIMAP_selec_LULUCF_CO2, file="Stocktaketool/History_CO2_LULUCF.csv", sep=";", row.names = FALSE)

# create AFOLU CO2' 
PRIMAP_selec_Agriculture_CO2_tmp <- gather(PRIMAP_selec_Agriculture_CO2, 5:ncol(PRIMAP_selec_Agriculture_CO2), key="year", value=value)
PRIMAP_selec_LULUCF_CO2_tmp <- gather(PRIMAP_selec_LULUCF_CO2, 5:ncol(PRIMAP_selec_LULUCF_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2 <- rbind(PRIMAP_selec_Agriculture_CO2_tmp, PRIMAP_selec_LULUCF_CO2_tmp)
PRIMAP_selec_AFOLU_CO2 <- group_by(PRIMAP_selec_AFOLU_CO2, scenario, region, unit, year) %>% summarise(value=sum(value))
PRIMAP_selec_AFOLU_CO2 <- mutate(PRIMAP_selec_AFOLU_CO2, category="CAT4+5") %>% select(scenario, region, unit, year, value)
PRIMAP_selec_AFOLU_CO2 <- spread(PRIMAP_selec_AFOLU_CO2, key=year, value=value)
PRIMAP_selec_AFOLU_CO2 <- data.frame(PRIMAP_selec_AFOLU_CO2)
PRIMAP_selec_AFOLU_CO2 <- mutate(PRIMAP_selec_AFOLU_CO2, variable="Emissions|CO2|AFOLU") %>% select(variable, scenario, region, unit, everything())
colnames(PRIMAP_selec_AFOLU_CO2) = gsub("X", "", colnames(PRIMAP_selec_AFOLU_CO2))
#PRIMAP_selec_AFOLU_CO2 <- select(PRIMAP_selec_AFOLU_CO2, -category, -entity)
write.table(PRIMAP_selec_AFOLU_CO2, file="Stocktaketool/History_CO2_AFOLU.csv", sep=";", row.names = FALSE)

# create 'CO2 excl AFOLU CO2'
PRIMAP_selec_CO2_tmp <- gather(PRIMAP_selec_CO2, 5:ncol(PRIMAP_selec_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2_tmp <- gather(PRIMAP_selec_AFOLU_CO2, 5:ncol(PRIMAP_selec_AFOLU_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2_tmp$value <- -1*PRIMAP_selec_AFOLU_CO2_tmp$value
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- rbind(PRIMAP_selec_CO2_tmp, PRIMAP_selec_AFOLU_CO2_tmp)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- group_by(PRIMAP_selec_CO2_Excl_AFOLU_CO2, scenario, region, unit, year) %>% summarise(value=sum(value))
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- mutate(PRIMAP_selec_CO2_Excl_AFOLU_CO2, category="CAT0-CAT4/5") %>% select(scenario, region, unit, year, value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- spread(PRIMAP_selec_CO2_Excl_AFOLU_CO2, key=year, value=value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- data.frame(PRIMAP_selec_CO2_Excl_AFOLU_CO2)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- mutate(PRIMAP_selec_CO2_Excl_AFOLU_CO2, variable="Emissions|CO2|Excl. AFOLU") %>% select(variable, scenario, region, unit, everything())
colnames(PRIMAP_selec_CO2_Excl_AFOLU_CO2) = gsub("X", "", colnames(PRIMAP_selec_CO2_Excl_AFOLU_CO2))
# add bunkers to world total
tmp <- filter(PRIMAP_selec_CO2_Excl_AFOLU_CO2, region=="World")
tmp <- gather(tmp, 5:ncol(PRIMAP_selec_CO2_Excl_AFOLU_CO2), key="year", value=value)
tmp <- inner_join(tmp, EDGAR_bunkers_total_adj, by=c('year')) %>% 
  mutate(value=value.x+value.y) %>%
  select(variable, scenario, region.x, unit.x, year, value) %>%
  rename(region=region.x, unit=unit.x)
tmp <- as.data.frame(tmp)
tmp <- spread(tmp, key=year, value=value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- filter(PRIMAP_selec_CO2_Excl_AFOLU_CO2, region!="World")
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- rbind(PRIMAP_selec_CO2_Excl_AFOLU_CO2, tmp)
# add bunkers as region
tmp_bunkers <- spread(EDGAR_bunkers_total_adj, key=year, value=value)
tmp_bunkers <- mutate(tmp_bunkers, variable="Emissions|CO2|Excl. AFOLU")
tmp_bunkers <- mutate(tmp_bunkers, scenario="History")
tmp_bunkers <- select(tmp_bunkers, variable, scenario, region, unit, everything())
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- rbind(PRIMAP_selec_CO2_Excl_AFOLU_CO2, tmp_bunkers)
write.table(PRIMAP_selec_CO2_Excl_AFOLU_CO2, file="Stocktaketool/History_CO2_ExclAFOLU.csv", sep=";", row.names = FALSE)

GWPCH4 = 25
GWPN2O = 298

PRIMAP_selec_CH4 <- filter(PRIMAP_Fig2_3, category=="CAT0", entity=="CH4")
PRIMAP_selec_CH4 <- select(PRIMAP_selec_CH4, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_CH4 <- cbind(PRIMAP_selec_CH4[1:5], GWPCH4*PRIMAP_selec_CH4[, 6:ncol(PRIMAP_selec_CH4)]/1000)
PRIMAP_selec_CH4$unit <- "Mt CO2eq/yr"
PRIMAP_selec_CH4 <- data.frame(PRIMAP_selec_CH4)
PRIMAP_selec_CH4 <- mutate(PRIMAP_selec_CH4, variable="Emissions|CH4") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_CH4) = gsub("X", "", colnames(PRIMAP_selec_CH4))
PRIMAP_selec_CH4 <- select(PRIMAP_selec_CH4, -category, -entity)
write.table(PRIMAP_selec_CH4, file="Stocktaketool/History_CH4.csv", sep=";", row.names = FALSE)

PRIMAP_selec_N2O <- filter(PRIMAP_Fig2_3, category=="CAT0", entity=="N2O")
PRIMAP_selec_N2O <- select(PRIMAP_selec_N2O, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_N2O <- cbind(PRIMAP_selec_N2O[1:5], GWPN2O*PRIMAP_selec_N2O[, 6:ncol(PRIMAP_selec_N2O)]/1000)
PRIMAP_selec_N2O$unit <- "Mt CO2eq/yr"
PRIMAP_selec_N2O <- data.frame(PRIMAP_selec_N2O)
PRIMAP_selec_N2O <- mutate(PRIMAP_selec_N2O, variable="Emissions|N2O") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_N2O) = gsub("X", "", colnames(PRIMAP_selec_N2O))
PRIMAP_selec_N2O <- select(PRIMAP_selec_N2O, -category, -entity)
write.table(PRIMAP_selec_N2O, file="Stocktaketool/History_N2O.csv", sep=";", row.names = FALSE)

PRIMAP_selec_FGases <- filter(PRIMAP_Fig2_3, category=="CAT0", entity==ifelse(GWPCH4==25, "FGASESAR4", "FGASES"))
PRIMAP_selec_FGases <- select(PRIMAP_selec_FGases, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_FGases <- cbind(PRIMAP_selec_FGases[1:5], PRIMAP_selec_FGases[, 6:ncol(PRIMAP_selec_FGases)]/1000)
PRIMAP_selec_FGases$unit <- "Mt CO2eq/yr"
PRIMAP_selec_FGases <- data.frame(PRIMAP_selec_FGases)
PRIMAP_selec_FGases <- mutate(PRIMAP_selec_FGases, variable="Emissions|F-Gases") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_FGases) = gsub("X", "", colnames(PRIMAP_selec_FGases))
PRIMAP_selec_FGases <- select(PRIMAP_selec_FGases, -category, -entity)
write.table(PRIMAP_selec_FGases, file="Stocktaketool/History_FGases.csv", sep=";", row.names = FALSE)

# add gases to one data frame
PRIMAP_selec_Gases <- rbind(PRIMAP_selec_CO2_Excl_AFOLU_CO2, PRIMAP_selec_AFOLU_CO2) %>% 
  rbind(PRIMAP_selec_CH4) %>%
  rbind(PRIMAP_selec_N2O) %>% 
  rbind(PRIMAP_selec_FGases)
PRIMAP_selec_Gases <- mutate(PRIMAP_selec_Gases, source="PRIMAP")
PRIMAP_selec_Gases <- mutate(PRIMAP_selec_Gases, statistic="value")
PRIMAP_selec_Gases <- select(PRIMAP_selec_Gases, variable, scenario, region, unit, source, statistic, everything())
# add total Kyoto emissions
tmp_bunkers_fig3 <- as.data.frame(tmp_bunkers)
tmp_bunkers_fig3$variable <- "Emissions|Kyoto Gases"
tmp_bunkers_fig3 <- mutate(tmp_bunkers_fig3, source="EDGAR")
tmp_bunkers_fig3 <- mutate(tmp_bunkers_fig3, statistic="value") 
tmp_bunkers_fig3 <- select(tmp_bunkers_fig3, variable, scenario, region, unit, source, statistic, everything())
PRIMAP_selec_Kyoto_fig3 <- rbind(PRIMAP_selec_Kyoto, tmp_bunkers_fig3)
PRIMAP_selec_Gases <- rbind(PRIMAP_selec_Gases, PRIMAP_selec_Kyoto_fig3)
#PRIMAP_selec_Gases <- as.data.frame(PRIMAP_selec_Gases)
#colnames(PRIMAP_selec_Gases) = gsub("X", "", colnames(PRIMAP_selec_Gases))
# determine RoW category
Countries_Gases_hist <- gather(PRIMAP_selec_Gases, 7:ncol(PRIMAP_selec_Gases), key="year", value=value)
Countries_Gases_hist <- Countries_Gases_hist %>% mutate(value2=ifelse(region!="World", -1*value, value)) %>% select(-value) %>% rename(value=value2)
RoW_Gases_hist <- group_by(Countries_Gases_hist, variable, scenario, unit, statistic, year) %>% summarise(value=sum(value,na.rm=TRUE))
RoW_Gases_hist <- mutate(RoW_Gases_hist, source="PRIMAP")
RoW_Gases_hist <- mutate(RoW_Gases_hist, region="ROW") %>% select(variable, scenario, unit, source, statistic, region, year, value)
RoW_Gases_hist <- spread(RoW_Gases_hist, key='year', value=value)
RoW_Gases_hist <- as.data.frame(RoW_Gases_hist)
PRIMAP_selec_Gases <- rbind(PRIMAP_selec_Gases, RoW_Gases_hist)
PRIMAP_selec_Gases <- arrange(PRIMAP_selec_Gases, variable, region)
PRIMAP_selec_Gases_output <- select(PRIMAP_selec_Gases, variable, scenario, region, unit, source, statistic, num_range("", 1990:2015))
write.table(filter(PRIMAP_selec_Gases_output, variable!="Emissions|Kyoto Gases"), file="Stocktaketool/History_fig2.csv", sep=";", row.names = FALSE)
write.table(PRIMAP_selec_Gases_output, file="Stocktaketool/History_fig3.csv", sep=";", row.names = FALSE)
