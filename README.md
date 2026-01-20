# SENTINELdomtraded
Data and R Scripts to support article: 'Measuring trade-off risk between food crops and vertebrate biodiversity in three African countries'

###########################
# CROP-SPECIFIC HOTSPOT DATA:

'Crop_Specific_Trade_Off_Hotspots.zip': This contains the trade-off hotspot raster (tif) map files for individual crops and species richness, as described in Supplementary Information section 7. The filenames are long but the main pieces of information to look out for are: country names (given after 'raster_') and the crop codes (e.g. BANA, BEAN), which are as per the labels described in the READ ME file provided when downloading raw SPAM data, copied below:

ID   SPAM name	Crop name
1	whea	Wheat
2	rice	Rice
3	maiz	Maize
4	barl	Barley
5	mill	Small Millet
6	pmil	Pearl Millet
7	sorg	Sorghum
8	ocer	Other Cereals
9	pota	Potato
10	swpo	Sweet Potato
11	yams	Yams
12	cass	Cassava
13	orts	Other Roots
14	bean	Bean
15	chic	Chickpea
16	cowp	Cowpea
17	pige	Pigeon Pea
18	lent	Lentil
19	opul	Other Pulses
20	soyb	Soybean
21	grou	Groundnut
22	cnut	Coconut
23	oilp	Oilpalm
24	sunf	Sunflower
25	rape	Rapeseed
26	sesa	Sesame Seed
27	ooil	Other Oil Crops
28	sugc	Sugarcane
29	sugb	Sugarbeet
30	cott	Cotton
31	ofib	Other Fibre Crops
32	coff	Arabic Coffee
33	rcof	Robust Coffee
34	coco	Cocoa
35	teas	Tea
36	toba	Tobacco
37	bana	Banana
38	plnt	Plantain
39	citr	Citrus
40	trof	Other Tropical Fruit
41	temf	Temperate Fruit
42	toma	Tomato
43	onio	Onion
44	vege	Other Vegetables
45	rubb	Rubber
46	rest	Rest Of Crops

#########Note that some of these crops are not provided (e.g. crop groups, such as 'rest of crops' or 'other vegetables', and those not grown in a given country), as described in the journal article these data accompany.

###########################
# R SCRIPTS:

0_Trade_Data_Read_In.R - this script shows how I read in trade data provided by Dr C Dalin (method to create data from FAOSTAT described in Kastner et al. 2911 and Dalin et al., 2017)

1_Trade_Off_Risk_Computation.R - this script documents the steps from reading in the species and crop physical area data to computing trade-off risk, identifying overlap between crops and conservation priority areas, and calculating summary statistics

2_Threshold_Sensitivity_Checks.R - this R script documents some tests run during the review process to compare outcomes with domestic and traded crops classified at 5% and 15% thresholds (see Methods)

3_Plotting.R - this short R script documents the final scripts used to generate Fig.2, as mapping was completed using ArcGIS (with some prior data processing documented in 1_Trade_Off_Risk_Computation.R)

###########################
# REFERENCES:

DALIN, C., WADA, Y., KASTNER, T. & PUMA, M. J. 2017. Groundwater depletion embedded in international food trade. Nature, 543, 700-704.
KASTNER, T., KASTNER, M. & NONHEBEL, S. 2011. Tracing distant environmental impacts of agricultural products from a consumer perspective. Ecological Economics, 70, 1032-1040.


