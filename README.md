### Alaska_Foodwebs
Eventually this read me will be used to help locate files within the Repo edit

###DATA BIOGRAPHY
Your Name: Catalina Burch

##1. RESOURCE OVERVIEW 
#1.1. Title of the dataset.
Gulf of Alaska Diets Data Network Analysis
Data Collection by the NOAA, Alaska Fisheries Science Center, Resource Ecology Ecosystem Modeling Program
Data Processing by Catalina Burch at University of Washington, School of Marine and Environmental Affairs

#1.2. Abstract. 
Recently, the North Pacific Ocean has experienced unprecedented and extreme marine heat waves (MHWs), which have caused cascading ecological effects within marine food webs. Examining past responses of groundfish feeding ecology to temperature shifts will help illuminate how they may respond in the future. In this study, we investigated the role of temperature in explaining spatiotemporal patterns in the diets of four groundfish predators in the Gulf of Alaska (GOA). The objectives of this research were to 1) characterize spatiotemporal patterns of groundfish predation on forage fish and crustacean prey species; and 2) evaluate effects of temperature on patterns of prey occurrence in predator diets. Prey species were identified for analysis based on hypothesized relationships with temperature, relevance to management, and empirical measures of relative importance in groundfish diets. We used 15 years (1990 - 2022) of groundfish stomach contents data to model prey occurrence as a function of temperature, year, location, depth, predator species, and predator length. Our model results showed seven relationships between temperature and prey occurrence in diets, including a negative effect for euphausiids and pandalids, and a positive effect for pagurids and tanner crab. Euphausiids in particular showed consistent negative trends in response to temperature in the diets of Pacific cod, walleye pollock, and arrowtooth flounder. Euphausiids are a high lipid, energy-rich food source for GOA predators including groundfish, whales, and seabirds. The complex and unprecedented effects of the GOA MHW demonstrate the need to better understand the predator-prey interactions in this system across multiple species and trophic levels. This work can help to inform development of prey abundance and biomass surveys and to supplement data-limited species or years.

1.3. Purpose. 
This Github page contains the data and code for a thesis submitted in partial fulfillment of the requirements for the degree of Master of Marine Affairs at the University of Washington. 

#1.4. Contacts. 
Catalina Burch: masters student at UW School of Marine and Environmental Affairs, data processing and analysis 
catalinaburch@gmail.com

#1.5. Sponsors. 
NOAA, Alaska Fisheries Science Center; North Pacific Research Board; University of Washington, School of Marine and Environmental Affairs

#1.6. Citation for the dataset.
Alaska Fisheries Science Center (2022). REEM Diets Data. Available from: https://apps-afsc.fisheries.noaa.gov/refm/reem/webdietdata/dietdataintro.php

#1.7. Keywords.
Ecosystem based fishery management, food webs, groundfish, marine heat wave, network analysis

##2. TEMPORAL AND SPATIAL EXTENTS 
#2.1. Temporal extent. 
Gulf of Alaska 
Data collection years: 1981 1987 1990 1993 1996 1999 2001 2003 2005 2007 2009 2011 2013 2015 2017 2019 

#2.2. Temporal resolution.  
The AFSC conducts bottom trawl surveys biannually/triannually in the GOA. See above for specific sampling years. Surveys were completed in the summer, and the start and end dates vary.

#2.3. Spatial extent. 
The survey reach includes 320,000 km^2 conducted by a stratefied random sampling method. 
Latitude Range: 52.46, 60.30
Longitude Range: -169.98, -132.68

3. RESOURCE CONTENT (what)
#3.1. Digital context. Names of data file(s), names of tables within data file(s), file format(s), and date the data were last modified. If you have multiple data files, describe any relationships among them (e.g., queried database tables saved as separate files and linked through an identifier?). For each data file and table within a data file, include a brief (1 sentence) narrative description of the contents. 
There are two main data files saved as .csv files in the (data) folder. There are also a .html and a .pdf file that contain metadata information.
Prey Groups.html Is a metadata file that catalogues common groupings for analysis of similar prey species.
PreyCodes.pdf Is a metadata file that catalogues prey identifier codes.
1_Gulf Of Alaska_Raw_PreyLength.csv Is a raw data file that contains prey length diets data.
1_Gulf Of Alaska_Raw_StomachContents.csv Is a raw data file that contains stomach contents data.

3.2. Data components and data table attributes. This section details the contents of each data table and/or data file and might be most effectively organized as a table (but it’s up to you). For each data file/data table, provide the names, definitions, and units of the attributes of any data in tabular format (e.g., column headers in a CSV file). Depending on the nature of the data, this could include: parameter name, measurement units, instrument type, precision, accuracy, taxonomic details, definitions of codes used, and any other important information for an analyst (e.g., quality review notes, missing values). Indicate whether data are raw values (not modified in any way after collection), processed values (corrected or calibrated), or derived values (an index or summarized value calculated based on other data). 
?I think this is something that I can finish later

##4. METHODS (how) 
#4.1. Lineage statement. 
A subsample of the bottom trawl catch is selected for stomach content analysis. The protocol for species selection varied over the years with the goal of rotating through a variety of commercially and ecologically important species. 

At each station, individual fish are chosen for stomach content analysis based on species-specific length stratification; for example, walleye pollock length categories are 1–24 cm, 25–39 cm, 40–54 cm, and 55+ cm. At a given station, no more than five specimens are selected per length category, and samples within a length category are selected to provide as wide a length-range as possible given the available fish. Not all length categories or target species are collected at all stations. 

Individual fish are selected from the catch and their stomachs are removed for analysis. Selected fish showing signs of regurgitation or net feeding are rejected and a substitute fish is selected. Those rejected due to suspected regurgitation are replaced by a stomach containing food to diminish the likelihood of biasing collections towards empty stomachs. Those rejected due to net-feeding are substituted with a randomly selected fish. The stomachs are excised in a manner that retains all of the stomach contents by pinching and slicing through the esophagus and posterior to the pyloric valve (or spiral valve for elasmobranchs). The predator species, length and sex, and the vessel, cruise and haul number are recorded on a Specimen Form and Specimen Label. The individual stomach and its corresponding specimen label are placed in a cloth bag that is tied shut and put into a bucket of a buffered neutral, 10% formalin and seawater solution. These buckets are labeled when full, sealed for transportation, and shipped to the Trophic Interactions Laboratory at the AFSC in Seattle.

?This is copied from livingston. Rephrase?

4.2. Process steps. The general process steps that occurred between data collection and its current form (brief narrative description or bulleted list). Depending on the dataset, processing might include digitization, removing or identifying outliers via computer scripts, file processing, data summarization, or data transformations. This does not need to be exhaustive, but should include information that would be important for an analyst to be aware of when they are using the data for research. Include relevant citations. 

Stomach content analysis in the laboratory follows one of two procedures to provide data on the composition of the contents in an individual stomach. Both procedures result in quantitative diet information, but differ in the level of detail and precision of non-fish or of non-crab prey. The procedure capturing more detail is referred to as "quantitative" analysis, and the procedure capturing less detail on prey other than fish and crab is referred to as
"qualitative" analysis. Both procedures capture the following: total stomach content weight recorded to the decagram (0.01 g), exact prey count, weight (0.001 g) and size (mm) when feasible for all crab and fish, species identification of all fish and crab prey, state of prey digestion, prey life history, and a visual assessment of stomach fullness. The primary difference between the two procedures is the level of sorting, enumerating and weighing of prey other than fish and crab found in a stomach. The quantitative procedure obtains an exact count and weight (0.001 g) for every prey group. The qualitative procedure relies on visual estimates by experienced personnel of the prey composition of the remaining weight after the fish and crabs have been weighed and counted. Prey counts of the remaining prey groups may be estimated but are usually left as null.

A portion of the recent collections were analyzed through detailed stomach content analysis at sea (SCANS). Instead of preserving stomach samples, trained personnel participating in the bottom trawl surveys of the GOA and AI perform SCANS on some or all target species. Motion-compensating scales (accurate to the nearest 0.5 g) make SCANS feasible, although some estimation of stomach contents less than 0.5 g is required. SCANS are conducted using the "qualitative" procedure described above.

Taxonomic prey identification standards have varied over time, ranging from very broad categories (order or
higher) to species level for some benthic and pelagic invertebrates, and were also substantially influenced by the amount of digestion in prey items. However, fish and crabs have always been identified to the species level when digestion state allowed. Some changes in minimum prey identification have occurred over time due to increasing availability and accumulation of reference materials, reference specimens, experience, and taxonomic research. Changes in prey identification standards have also been in response to developments in ecosystem modeling capabilities and increasing focus on the modeling of ecological processes. In addition, stomach content analysis performed in support of specific research projects often requires taxonomic precision that exceeds our minimum prey identification standards.

Stomach fullness is qualitatively estimated during analysis of the stomach contents, but a stomach fullness index can often be calculated from the available data. The stomach fullness is visually estimated and categorized as being empty, trace of prey, 25% full, 50% full, 75% full, 100% full or distended. A quantitative index of stomach fullness can be calculated from the total weight of the stomach contents relative to the estimated body weight of the fish-estimated from length-weight regression.

Prey digestion is an estimate of the state of digestion and is categorized as a percent of the prey remaining. Prey digestion is recorded as traces of prey (often retained hard parts – bones, beaks, shells, jaws, eye lenses, etc.), <50% intact, 50–75% intact, 75–100% intact, or no digestion.

Fish and crab prey are identified to species to the extent allowed by digestion and are measured when feasible. Standard length of prey fish, carapace length of king crab (Lithodidae) and shrimp (Pandalidae) and carapace width of Tanner and snow crab (Majidae) are measured to the nearest millimeter. Walleye pollock standard length can also be derived from the size of its otolith using regression (Frost and Lowry 1981). This technique is only used when the prey’s state of digestion precludes an accurate measurement, the otolith is not degraded and can be determined to be from the prey fish.

4.3. Quality assurance and quality control. Note anything the data creators did to ensure the completeness and accuracy of their dataset (e.g., instrument calibrations, automated procedures, manual/visual tests for outliers). 
I think these two next parts are mostly covered in the above material?

4.4. Data completeness and constraints. Were any data excluded from the dataset? If so, why? What are known cautions or problems, such as sampling bias? Are there ways the data should not be used (according to the data creators)? 
