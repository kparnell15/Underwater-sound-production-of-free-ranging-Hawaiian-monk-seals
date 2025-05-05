# Title of study

Underwater sound production of free-ranging Hawaiian monk seals


# Summary

This repository contains processed data and R scripts supporting a study on the underwater vocal behavior of the endangered Hawaiian monk seal (Neomonachus schauinslandi). Using over 4,500 hours of passive acoustic recordings from five sites across the Hawaiian Archipelago, we detected and classified more than 23,000 underwater vocalizations. We identified 27 distinct call types, including 22 previously undescribed calls and evidence of combinational calls—an unreported communication strategy in pinnipeds. Acoustic analyses and temporal patterns reveal that vocalizations are predominantly low-frequency (<1 kHz), short to medium in duration, and often occur in bouts, with vocalizations produced throughout the day at some sites. These findings establish a foundational understanding of monk seal acoustic communication to support future behavioral research and conservation efforts.


#  Overview of folders/files and their contents: 

This repository includes two folders: 
1. DFA Analysis, which contains the acoustic measurements for each call type titled "DFA_18 call types.csv" and "10 Call Type DFA.R" file used for vocal classification, and
2. rehmsvocaltrends, which includes seven call detection & classification .cvs files from all recording sites, one "ST_active.csv" file used in the scrip, and the .Rmd and .Rproj files for temporal pattern analyses.

While raw acoustic recordings are not publicly archived due to file size (> 20 TB), all processed data used in the analysis are provided.

All .R files were created in RStudio version 2024.09.0+375

Packages used in rmsvocaltrends:
ggplot2,
dplyr,
lubridate,
tidyverse,
readxl,
tidyr

Packages used in DFA Analysis:
corrplot,
MASS,
ggplot2,
caret,
dplyr,
reshape2
