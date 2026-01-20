### R Script to accompany manuscript:
### "Assessing trade-off risk between crops and vertebrate biodiversity in three African countries"
### A Chapman, Jan 2025 version (edited Jan 2026)

### Before running this script, I have run the following: 
### 0_Trade_Data_Read_in.R 
### 1_TOR_April2024.R
### 2_TOR_April2024_BMonly.R
### 3_TOR_April2024_threatened.R
### (1-3 are the equivalent of 1_Trade_Off_Risk_Computation.R provided on GitHub)
### but as these scripts rm_ls() and work individually, I exported their end dataframes
### as csv's to call here.

rm(list = ls())
library(raster); library(maptools); library(sp); library(readbulk);
library(rgdal); library(rgeos); library(ggplot2); library(snow); library(dplyr); library(viridis);
library(gridExtra); library(reshape2); library(ggsci)
#options(scipen = 999)

setwd("~/Data/")
# newdir = "Overlay Analysis/Jan2025/"
newdir = "~Documents/Manuscripts/Overlay Analysis/Revision/"
tor_allverts = read.csv(paste0(newdir, "alldata_tor_verts.csv"))
head(tor_allverts)
tor_allverts_GHA= subset(tor_allverts, country == "Ghana")
t.test(tor~domtraded, data = tor_allverts_GHA)
tor_allverts_ETH= subset(tor_allverts, country == "Ethiopia")
t.test(tor~domtraded, data = tor_allverts_ETH)
tor_allverts_ZMB= subset(tor_allverts, country == "Zambia")
t.test(tor~domtraded, data = tor_allverts_ZMB)

riskpotential_allverts = read.csv(paste0(newdir, "alldata_riskpotential_verts.csv"))
head(riskpotential_allverts)
riskpotential_allverts_GHA= subset(riskpotential_allverts, country == "Ghana")
t.test(tor~domtraded, data = riskpotential_allverts_GHA)
riskpotential_allverts_ETH= subset(riskpotential_allverts, country == "Ethiopia")
t.test(tor~domtraded, data = riskpotential_allverts_ETH)
riskpotential_allverts_ZMB= subset(riskpotential_allverts, country == "Zambia")
t.test(tor~domtraded, data = riskpotential_allverts_ZMB)

tor_bm = read.csv(paste0(newdir, "alldata_tor_bm.csv"))
riskpotential_bm = read.csv(paste0(newdir, "alldata_riskpotential_bm.csv"))

tor_bm_GHA= subset(tor_bm, country == "Ghana")
t.test(tor~domtraded, data = tor_bm_GHA)
tor_bm_ETH= subset(tor_bm, country == "Ethiopia")
t.test(tor~domtraded, data = tor_bm_ETH)
tor_bm_ZMB= subset(tor_bm, country == "Zambia")
t.test(tor~domtraded, data = tor_bm_ZMB)

riskpotential_bm_GHA= subset(riskpotential_bm, country == "Ghana")
t.test(tor~domtraded, data = riskpotential_bm_GHA)
riskpotential_bm_ETH= subset(riskpotential_bm, country == "Ethiopia")
t.test(tor~domtraded, data = riskpotential_bm_ETH)
riskpotential_bm_ZMB= subset(riskpotential_bm, country == "Zambia")
t.test(tor~domtraded, data = riskpotential_bm_ZMB)

tor_threatened = read.csv(paste0(newdir, "alldata_tor_threatened.csv"))
riskpotential_threatened = read.csv(paste0(newdir, "alldata_riskpotential_threatened.csv"))

tor_threatened_GHA= subset(tor_threatened, country == "Ghana")
t.test(tor~domtraded, data = tor_threatened_GHA)
tor_threatened_ETH= subset(tor_threatened, country == "Ethiopia")
t.test(tor~domtraded, data = tor_threatened_ETH)
tor_threatened_ZMB= subset(tor_threatened, country == "Zambia")
t.test(tor~domtraded, data = tor_threatened_ZMB)

riskpotential_threatened_GHA= subset(riskpotential_threatened, country == "Ghana")
t.test(tor~domtraded, data = riskpotential_threatened_GHA)
riskpotential_threatened_ETH= subset(riskpotential_threatened, country == "Ethiopia")
t.test(tor~domtraded, data = riskpotential_threatened_ETH)
riskpotential_threatened_ZMB= subset(riskpotential_threatened, country == "Zambia")
t.test(tor~domtraded, data = riskpotential_threatened_ZMB)


## Bring the trade-off risk data into one place:
tor_allverts1 = subset(tor_allverts, select = c(relative_tor, crop, country, domtraded))
head(tor_allverts1)
head(tor_bm)
tor_bm1 = subset(tor_bm, select = c(relative_tor, crop, country, domtraded))
head(tor_bm1)
head(tor_threatened)
tor_threatened1 = subset(tor_threatened, select = c(relative_tor, crop, country, domtraded))
head(tor_threatened1)

str(tor_allverts1)
str(tor_bm1)
str(tor_threatened1)
# all three now have 108 observations of 4 variables, of which crop, country, domtraded will match
colnames(tor_allverts1) = c("tor_allverts", "crop", "country", "domtraded")
colnames(tor_bm1) = c("tor_bm", "crop", "country", "domtraded")
colnames(tor_threatened1) = c("tor_threatened", "crop", "country", "domtraded")

test = full_join(tor_allverts1, tor_bm1, by = c("crop", "country", "domtraded"))
head(test)
tor_all_together = full_join(test, tor_threatened1, by = c("crop", "country", "domtraded"))
head(tor_all_together, n = 39)

tor_all_together_tidy = melt(tor_all_together, id.vars = c("crop", "country", "domtraded"), 
                             measure.vars = c("tor_allverts", "tor_bm", "tor_threatened"))
head(tor_all_together)
head(tor_all_together_tidy)



library(ggforce)

max(tor_all_together_tidy$value, na.rm = TRUE)
min(tor_all_together_tidy$value, na.rm = TRUE)

## Plot 1: TOR (toggle facet_zoom and scale_y_log10 on and off)

(plot1_all_tor = tor_all_together_tidy %>% 
    ggplot(aes(x=variable,y=value))+
    geom_boxplot(aes(width=.5, fill = domtraded), notch = TRUE, notchwidth = 0.9,
                 coef = NULL)+ # coef = NULL sets the whiskers to the min and max values
    # notching adds a way of comparing the medians between plots
    scale_fill_futurama() +
    theme_bw()+
    #scale_y_log10() +
    #facet_zoom(ylim = c(0, 0.5), zoom.data = ifelse(value <=0.5, NA, FALSE)) +
    facet_wrap(~country) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
    labs(x = "Data Subset", y = "Trade-Off Risk Relative to Median Expectation") +
    scale_x_discrete(labels = c("Vertebrates", "Birds & Mammals", "Threatened", "Vertebrates", "Birds & Mammals", "Threatened", "Vertebrates", "Birds & Mammals", "Threatened")) +
    theme(legend.position="none",
          text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 2, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.title.x = element_text(size = 12, vjust = -2, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20),
          strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)))


## Plot 2: Trade-off risk potential:

## Bring the risk potential data together:

riskpotential_allverts1 = subset(riskpotential_allverts, select = c(relative_TRP, crop, country, domtraded))
head(riskpotential_allverts1)
head(riskpotential_bm)
riskpotential_bm1 = subset(riskpotential_bm, select = c(relative_TRP, crop, country, domtraded))
head(riskpotential_bm1)
head(riskpotential_threatened)
riskpotential_threatened1 = subset(riskpotential_threatened, select = c(relative_TRP, crop, country, domtraded))
head(riskpotential_threatened1)

str(riskpotential_allverts1)
str(riskpotential_bm1)
str(riskpotential_threatened1)
# all three now have 114 observations of 4 variables, of which crop, country, domtraded will match
colnames(riskpotential_allverts1) = c("riskpotential_allverts", "crop", "country", "domtraded")
colnames(riskpotential_bm1) = c("riskpotential_bm", "crop", "country", "domtraded")
colnames(riskpotential_threatened1) = c("riskpotential_threatened", "crop", "country", "domtraded")

test = full_join(riskpotential_allverts1, riskpotential_bm1, by = c("crop", "country", "domtraded"))
head(test)
riskpotential_all_together = full_join(test, riskpotential_threatened1, by = c("crop", "country", "domtraded"))
head(riskpotential_all_together, n = 39)

riskpotential_all_together_tidy = melt(riskpotential_all_together, id.vars = c("crop", "country", "domtraded"), 
                             measure.vars = c("riskpotential_allverts", "riskpotential_bm", "riskpotential_threatened"))
head(riskpotential_all_together)
head(riskpotential_all_together_tidy)

library(ggforce)

max(riskpotential_all_together_tidy$value, na.rm = TRUE)
min(riskpotential_all_together_tidy$value, na.rm = TRUE)
mean(riskpotential_all_together_tidy$value, na.rm = TRUE)
median(riskpotential_all_together_tidy$value, na.rm = TRUE)

## Plot risk potential: (toggle facet_zoom on and scale_y_log10 and off)

max(riskpotential_all_together_tidy$value, na.rm = T)
min(riskpotential_all_together_tidy$value, na.rm = T)


(plot1_all_riskpotential = riskpotential_all_together_tidy %>% 
    ggplot(aes(x=variable,y=value))+
    geom_boxplot(aes(width=.5, fill = domtraded), notch = TRUE, notchwidth = 0.9,
                 coef = NULL)+ # coef = NULL sets the whiskers to the min and max values
    # notching adds a way of comparing the medians between plots
    scale_fill_futurama() +
    facet_wrap(~country) +
    theme_bw()+
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
    coord_cartesian(ylim = c(0, 30)) +
    #scale_y_log10()+
    #facet_zoom(ylim = c(0, 100000), zoom.data = ifelse(value <=1000000, NA, FALSE)) +
    labs(x = "Data Subset", y = "Total Risk Potential Relative to Median Expectation \n
         (excluding values >30 for visualisation)") +
    scale_x_discrete(labels = c("Vertebrates", "Birds & Mammals", "Threatened", "Vertebrates", "Birds & Mammals", "Threatened", "Vertebrates", "Birds & Mammals", "Threatened")) +
    theme(legend.position="none",
          text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 2, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.title.x = element_text(size = 12, vjust = -2, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20),
          strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)))
