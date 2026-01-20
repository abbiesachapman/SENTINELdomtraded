## 20th April 2024
## Abbie Chapman, SENTINEL project
## As Ghana (and possibly other countries) does not feature in the data for 2018,
## I am running this script now for 2015-2017 to try to smooth out abnormalities
## but ensure compatability for SPAM 2017 crops for the SENTINEL focal countries.
## Note: we have these data courtesy of Carole Dalin and Thomas Kastner, among others.
## This means that the data aren't available on GitHub but could be calculated from FAO trade data
## as described in the cited articles (see Methods) or possibly available on request from
## those authors. 

rm(list = ls())
library(dplyr)
#options(scipen = 999)

setwd("~/Data/")
outdir = "Overlay Analysis/August 2023/"
cropdir = "SHEFS/6_Trade_Matrices/Corrected_trade_long/" # bringing in the long format data
yearslist = c("2015", "2016", "2017")
croplist = read.csv(paste0("MapSPAM/2020_version/", "crop_list_from_read_me.csv"))
croplist
str(croplist)
croplist1 = croplist$Earthstat_cropname
croplist1
#croplist2 = croplist1[-25]

raw.file.list = list.files('SHEFS/6_Trade_Matrices/Corrected_trade_long/')
# select.files = raw.file.list[grepl(paste(yearslist, collapse = "|"), raw.file.list)]
# str(raw.file.list); str(select.files)

# subset the crops:
croplist2 = croplist1[-41]
croplist2 = croplist2[-45]
cropsub <- lapply(croplist2, FUN = function(x) {
  raw.file.list[grepl(x, raw.file.list)]
})
cropsub # note that some missing from trade are expected (like 'other fibres') but tobacco is also missing
cropsub1 = unlist(cropsub)

# subset the years:
yearsub <- lapply(yearslist, FUN = function(y) {
  cropsub1[grepl(y, cropsub1)]
})
yearsub
yearsub1 = unlist(yearsub)

cropdata_1 = yearsub1
str(cropdata_1)

# read in the big trade csv files, tidy, and output as one csv for the years of interest:
library(stringr)
df_1 = NULL
for (m in 1:141) {
  file1 = read.csv(paste0("SHEFS/6_Trade_Matrices/Corrected_trade_long/",cropdata_1[m]))
  file2 = file1[complete.cases(file1),]
  cropname = stringr::str_remove(cropdata_1[m], "SHEFS/6_Trade_Matrices/Corrected_trade_long/")
  cropname1 = stringr::str_remove(cropname, ".csv")
  cropname2 = stringr::str_split_fixed(cropname1,"_", 2) 
  file_with_name = cbind(file2, cropname2)
  df_1 = rbind(df_1, file_with_name)}

str(df_1)
form_1_1 = paste0(outdir, "trade_data_all_1997_to_2017_20thApril2024.csv")
# write.csv(df_1, file = form_1_1)
View(df_1)
# quick check:
head(df_1, 3)

# tidying and summarisation:
tradedata = df_1
colnames(tradedata) = c("reporter", "partner", "dmi_tonnes", "crop", "year")
trade_data_c2017 = tradedata %>% 
  group_by(crop, reporter, partner) %>% 
  summarise(dmi_tonnes_average = mean(dmi_tonnes, na.rm = TRUE))
trade_data_c2017 
# View(trade_data_c2017)

### use some of the scripting below to get the import and export and domestic values needed
head(trade_data_c2017, 3)

# domestic total per crop (for each of the SENTINEL countries)
ghana_dom = data.frame(filter(trade_data_c2017, reporter == "Ghana", partner == "Ghana"))
head(ghana_dom) # average tonnes for which Ghana is reporter and partner
ghana_dom_percrop = subset(ghana_dom, select = c("crop", "dmi_tonnes_average"))
colnames(ghana_dom_percrop) = c("crop", "domestic_tonnes")
ethiopia_dom = data.frame(filter(trade_data_c2017, reporter == "Ethiopia", partner == "Ethiopia"))
head(ethiopia_dom) # average tonnes for which Ethiopia is reporter and partner
ethiopia_dom_percrop = subset(ethiopia_dom, select = c("crop", "dmi_tonnes_average"))
colnames(ethiopia_dom_percrop) = c("crop", "domestic_tonnes")
zambia_dom = data.frame(filter(trade_data_c2017, reporter == "Zambia", partner == "Zambia"))
head(zambia_dom) # average tonnes for which Zambia is reporter and partner
zambia_dom_percrop = subset(zambia_dom, select = c("crop", "dmi_tonnes_average"))
colnames(zambia_dom_percrop) = c("crop", "domestic_tonnes")

# export total per crop (for each of the SENTINEL countries)
ghana_exp = data.frame(filter(trade_data_c2017, reporter != "Ghana", partner == "Ghana"))
head(ghana_exp) # average tonnes for which Ghana is reporter and partner
# to get the total exports per crop, an extra step is needed
ghana_exp_percrop = data.frame(ghana_exp %>% 
  group_by(crop) %>% 
  summarise(total_export_tonnes_across_all_reporters = sum(dmi_tonnes_average, na.rm = TRUE)))

ethiopia_exp = data.frame(filter(trade_data_c2017, reporter != "Ethiopia", partner == "Ethiopia"))
head(ethiopia_exp) # average tonnes for which Ethiopia is reporter and partner
# to get the average exports per crop, an extra step is needed
ethiopia_exp_percrop = data.frame(ethiopia_exp %>% 
                                 group_by(crop) %>% 
                                 summarise(total_export_tonnes_across_all_reporters = sum(dmi_tonnes_average, na.rm = TRUE)))

zambia_exp = data.frame(filter(trade_data_c2017, reporter != "Zambia", partner == "Zambia"))
head(zambia_exp) # average tonnes for which Zambia is reporter and partner
# to get the average exports per crop, an extra step is needed
zambia_exp_percrop = data.frame(zambia_exp %>% 
                                 group_by(crop) %>% 
                                 summarise(total_export_tonnes_across_all_reporters = sum(dmi_tonnes_average, na.rm = TRUE)))


ghanatradedata = cbind(ghana_dom_percrop, ghana_exp_percrop)
colnames(ghanatradedata) = c("crop", "domestic_tonnes", "country", "total_export_tonnes_all_reporters")
ghanatradedata$country = "Ghana"
ethiopiatradedata = cbind(ethiopia_dom_percrop, ethiopia_exp_percrop)
colnames(ethiopiatradedata) = c("crop", "domestic_tonnes", "country", "total_export_tonnes_all_reporters")
ethiopiatradedata$country = "Ethiopia"
zambiatradedata = cbind(zambia_dom_percrop, zambia_exp_percrop)
colnames(zambiatradedata) = c("crop", "domestic_tonnes", "country", "total_export_tonnes_all_reporters")
zambiatradedata$country = "Zambia"

alltradedata = rbind(ghanatradedata, ethiopiatradedata, zambiatradedata)
form_1_2 = paste0(outdir, "trade_data_all_sentinel_countries_summarised_1997_to_2017_20thApril2024.csv")
# write.csv(alltradedata, file = form_1_2)
