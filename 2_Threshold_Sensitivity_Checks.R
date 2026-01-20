## SENTINEL Project Manuscript R Script for Traded Crop Classification Tests (recommended in review)
## Abbie Chapman, 26th November 2025
## This script is an edited form of part of the trade-off risk script, to try different cutoffs for classifying
## traded crops and to see the impact on the domestic vs. traded results, as a sensitivity check.
## This script is best run after reading in the output of 0_Trade_Data_Read_In.R and running
## up to line 683 of 1_Trade_Off_Risk_Computation.R.

alldata$proportion_domestic = (alldata$domestic_tonnes/(alldata$domestic_tonnes+alldata$total_export_tonnes_all_reporters))*100
alldata$proportion_traded = (alldata$total_export_tonnes_all_reporters/(alldata$domestic_tonnes+alldata$total_export_tonnes_all_reporters))*100

plotting_data = subset(alldata, select = c("crop", "country", "proportion_traded"))

library(forcats)

######################
## ChatGPT query after a few trial plots:
# Hi ChatGPT, I have some data with country, crop, and the proportion of each crop kept domestically and the proportion traded (with the proportions as percentages of the total). I am keen to create a plot which shows for each of the countries these proportions per crop in descending order, so it's clear where a good threshold would be to consider something as an important traded crop.

library(tidytext)
library(tidyverse)

# removing NA as the NA is different to zero for these data
df <- plotting_data %>% filter(!is.na(proportion_traded))

# order the data within each country:
data_ordered = df %>% 
  mutate(crop_reordered = reorder_within(crop, proportion_traded, country, .desc = TRUE)) # this is a reordering that sets up for plotting, so won't show as reordered in the data view

ggplot(data_ordered, aes(x = crop_reordered, y = proportion_traded)) +
  geom_col()+
  coord_flip() +
  facet_wrap(~country, scales = "free_y") +
  scale_x_reordered() + # this removes the extra suffix from reorder_within 
  labs(x = "Crop", y = "Proportion traded (%)") +
  geom_hline(yintercept = 10) +
  theme_minimal()

## Suggesting an elbow check, as I had tested different thresholds manually and investigated the number of crops remaining,
## but the elbow detection per country would be better.

df_elbow <- df %>%
  group_by(country) %>%
  arrange(desc(proportion_traded)) %>%
  mutate(drop = proportion_traded - lead(proportion_traded),
         elbow = row_number() == which.max(drop),
         important = row_number() <= which.max(drop)) %>%
  ungroup()

# Plot
ggplot(df_elbow, aes(x = reorder_within(crop, proportion_traded, country),
                     y = proportion_traded,
                     fill = important)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ country, scales = "free_y") +
  scale_x_reordered() +
  scale_fill_manual(values = c("grey70", "steelblue"),
                    name = "Important crop") +
  theme_minimal()

# However, as we can see here, if we use the elbow method, we end up with one main traded crop in two of the countries,
# which doesn't allow a fair and representative sample for comparison.

# ChatGPT query: The elbow was great thanks, especially plotted. What's difficult is I'd want to identify enough crops above a given threshold to compare. I originally chose a 10% cutoff but need to sensitivity check this.

# thresholds to check:
thresholds = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
# how many crops exceed this threshold per country?
sensitivity = expand_grid( # produces a sorted output as a tibble
  threshold = thresholds, df) %>% 
  group_by(country, threshold) %>% # grouped by country
  summarise(n_crops = sum(proportion_traded >= threshold, na.rm = T), # get the sum of crops where proportion traded exceeds each threshold
            .groups = "drop")

# as a plot:
ggplot(sensitivity, aes(x = threshold, y = n_crops, colour = country)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +                 # higher threshold on left makes a nice elbow shape
  labs(
    x = "Threshold for proportion traded",
    y = "Number of crops above threshold",
    title = "Sensitivity of important-crop counts to threshold choice"
  ) +
  theme_minimal()

###################### End of chatgpt assisted code 

# So, just to check, how to the results differ if we look at the split point of 15%? - only 5 crops different
head(alldata)
alldata$domtraded = "domestic"
alldata$domtraded[alldata$proportion_traded >15] <- "traded"
unique(alldata$crop)
alldata = filter(alldata, crop != "cerealnes" & crop != "other fibre" &
                   crop != "other oil" & crop != "pulsenes" & crop != "other roots"
                 & crop != "rest of crops" 
                 & crop != "fruitnes" & crop != "tropicalnes" & crop != "vegetablenes"
                 & crop != "rootnes" & crop != "broadbean" & crop != "buckwheat" 
                 & crop != "greenbean" & crop != "greenbroadbean" & crop != "millet" & crop != "stringbean")
unique(alldata$crop)

alldata$label = paste(alldata$crop, ",", alldata$country)

ghanadata = filter(alldata, country == "Ghana")
ethiopiadata = filter(alldata, country == "Ethiopia")
zambiadata = filter(alldata, country == "Zambia")

library(ggrepel)
set.seed(42)

(plot1_all = alldata %>% 
    ggplot(aes(x=domtraded,y=tor, label = crop))+
    geom_boxplot(aes(width=.5, color = domtraded))+
    geom_text_repel(max.overlaps = 20) +
    scale_color_futurama() +
    facet_wrap(~country) +
    # jittered text with geom_text
    theme_bw()+
    labs(x = "Crop Group", y = "Trade-Off Risk") +
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

head(alldata)

# Figure: Panel - trade-off risk:
(plot1_all_tor = alldata %>% 
    ggplot(aes(x=domtraded,y=relative_tor, label = crop))+
    geom_boxplot(aes(width=.5, fill = domtraded), notch = TRUE, notchwidth = 0.9,
                 coef = NULL)+ # coef = NULL sets the whiskers to the min and max values
    # notching adds a way of comparing the medians between plots
    scale_fill_futurama() +
    facet_wrap(~country) +
    theme_bw()+
    labs(x = "Crop Group", y = "Trade-Off Risk Relative to Median Expectation") +
    geom_hline(yintercept = 1, linetype = "dotted") +
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

head(alldata)
# want a Welch two sample t-test to compare two treatments: domestic and traded
# assumes that the variance of two groups is unequal so no changes needed first (https://www.datacamp.com/tutorial/t-tests-r-tutorial)
# want the numeric first then category column
t.test(tor~domtraded, data = alldata) # no significant difference
# separating the countries:
t.test(tor~domtraded, data = ghanadata) # no significant difference
t.test(tor~domtraded, data = ethiopiadata) # no significant difference
t.test(tor~domtraded, data = zambiadata) # no significant difference

ghanadata = filter(alldata, country == "Ghana")
ethiopiadata = filter(alldata, country == "Ethiopia")
zambiadata = filter(alldata, country == "Zambia")

# what is the effect size of the crops?
(ghanaeffect = ghanadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(ghana_effect = 4.95-4.78) # 0.17

# what is the effect size of the crops?
(ethiopiaeffect = ethiopiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(ethiopia_effect = 3.43-3.71) # -0.28

# what is the effect size of the crops?
(zambiaeffect = zambiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(zambia_effect = 5.8-5.74) # -0.06 

# and what about for 5%?
head(alldata)
alldata$domtraded = "domestic"
alldata$domtraded[alldata$proportion_traded >5] <- "traded"
unique(alldata$crop)
alldata = filter(alldata, crop != "cerealnes" & crop != "other fibre" &
                   crop != "other oil" & crop != "pulsenes" & crop != "other roots"
                 & crop != "rest of crops" 
                 & crop != "fruitnes" & crop != "tropicalnes" & crop != "vegetablenes"
                 & crop != "rootnes" & crop != "broadbean" & crop != "buckwheat" 
                 & crop != "greenbean" & crop != "greenbroadbean" & crop != "millet" & crop != "stringbean")
unique(alldata$crop)

alldata$label = paste(alldata$crop, ",", alldata$country)

ghanadata = filter(alldata, country == "Ghana")
ethiopiadata = filter(alldata, country == "Ethiopia")
zambiadata = filter(alldata, country == "Zambia")

library(ggrepel)
set.seed(42)

(plot1_all = alldata %>% 
    ggplot(aes(x=domtraded,y=tor, label = crop))+
    geom_boxplot(aes(width=.5, color = domtraded))+
    geom_text_repel(max.overlaps = 20) +
    scale_color_futurama() +
    facet_wrap(~country) +
    # jittered text with geom_text
    theme_bw()+
    labs(x = "Crop Group", y = "Trade-Off Risk") +
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

head(alldata)

# Figure: Panel - trade-off risk:
(plot1_all_tor = alldata %>% 
    ggplot(aes(x=domtraded,y=relative_tor, label = crop))+
    geom_boxplot(aes(width=.5, fill = domtraded), notch = TRUE, notchwidth = 0.9,
                 coef = NULL)+ # coef = NULL sets the whiskers to the min and max values
    # notching adds a way of comparing the medians between plots
    scale_fill_futurama() +
    facet_wrap(~country) +
    theme_bw()+
    geom_hline(yintercept = 1, linetype = "dotted") +
    labs(x = "Crop Group", y = "Trade-Off Risk Relative to Median Expectation") +
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

head(alldata)
# want a Welch two sample t-test to compare two treatments: domestic and traded
# assumes that the variance of two groups is unequal so no changes needed first (https://www.datacamp.com/tutorial/t-tests-r-tutorial)
# want the numeric first then category column
t.test(tor~domtraded, data = alldata) # no significant difference
# separating the countries:
t.test(tor~domtraded, data = ghanadata) # no significant difference
t.test(tor~domtraded, data = ethiopiadata) # no significant difference
t.test(tor~domtraded, data = zambiadata) # no significant difference but close

# what is the effect size of the crops?
(ghanaeffect = ghanadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(ghana_effect = 4.9-4.97) # -0.07 

# what is the effect size of the crops?
(ethiopiaeffect = ethiopiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(ethiopia_effect = 3.39-3.74) # -0.35 

# what is the effect size of the crops?
(zambiaeffect = zambiadata %>% 
    group_by(domtraded) %>% 
    summarise(mean = mean(tor, na.rm = TRUE)))
(zambia_effect = 5.73-5.91) # -0.18 
