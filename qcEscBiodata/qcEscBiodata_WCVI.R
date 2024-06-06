
# Script to QC WCVI's Escapement Biodata entry file 
# Copied/mirrored from the WCVI_RUn_Recon, but pared down as it doesn't assess stock ID discrepancies 
# June 2024


# Load common libraries --------------------
library(tidyverse)


# Load data file --------------------
esc.biodat.raw <- readxl::read_excel(path=list.files(path = "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/",
                                                     pattern = "^[^~]*_WCVI_Escapement-FSC_BioData*.xlsx",    
                                                     full.names = TRUE), 
                                     sheet=grep("Biodata 2015-", 
                                                readxl::excel_sheets(path=list.files(path = "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/",
                                                                                     pattern = "^[^~]*_WCVI_Escapement-FSC_BioData*.xlsx",    
                                                                                     full.names = TRUE)),
                                                ignore.case=T, value=T),
                                     guess_max=10000) %>%
  #select(Year, `Sample Month`:Species, `Fishery / River`:Gear, Sex, `POF Length (mm)`:`Egg Retention`, Comments) %>%
  mutate(`(R) OTOLITH LBV CONCAT` = case_when(!is.na(`Otolith Lab Number`) & !is.na(`Otolith Box #`) & !is.na(`Otolith Specimen #`) ~ 
                                                paste0(`Otolith Lab Number`, sep="-",`Otolith Box #`, sep="-",`Otolith Specimen #`)),
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`Scale Book #`) & !is.na(`Scale #`) ~ paste0(`Scale Book #`,sep="-",`Scale #`)),
         `Fishery / River` = case_when(`Fishery / River`=="Moheya River" ~ "Moyeha River",
                                       TRUE ~ `Fishery / River`)) %>% 
  print()




################################################################################################################################################


#                                                                           I. QC FLAGS


# SCALE BOOK FLAGS ---------------------------
# Duplicate scale book-cells                                                                                              *****requires Seren sleuthing
qc_dupl_scale_bookcell <- esc.biodat.raw %>% 
  filter(`(R) SCALE BOOK-CELL CONCAT` %in% as.character(esc.biodat.raw %>% 
                                                          filter(!is.na(`(R) SCALE BOOK-CELL CONCAT`)) %>% 
                                                          group_by(`(R) SCALE BOOK-CELL CONCAT`) %>% 
                                                          summarize(n=n()) %>% 
                                                          filter(n>1) %>%
                                                          pull(`(R) SCALE BOOK-CELL CONCAT`))) %>% 
  print()


# Scale books that are not 6 characters and likely were re-named by the lab                                               *****requires Seren sleuthing, then maybe changing
# Would affect joining, but we may just re-label ourselves?? 
qc_nonstd_scale_book <- esc.biodat.raw %>% 
  filter(nchar(`Scale Book #`)<6) %>% 
  group_by(`Scale Book #`) %>%
  summarize(`Scale Book #`=unique(`Scale Book #`)) %>%
  print()


# Scale book contains "SC" or "SP"                                                                                        *****requires Seren sleuthing
# May be fine, we probably just want to quickly do a manual PADS check to ensure joining will work
qc_confirm_scale_book <- esc.biodat.raw %>% 
  filter(grepl("SC|SP", `Scale Book #`)) %>% 
  group_by(`Scale Book #`) %>% 
  summarize(`Scale Book #` = unique(`Scale Book #`)) %>%
  print()


# Scale cell numbers that are probably changed by the lab                                                                 *****requires Seren changing
# Would affect joining, but we may just re-label ourselves?? 
qc_nonstd_scale_cell <- esc.biodat.raw %>%
  filter((grepl("10", `Scale Format (5 down, 2 across, etc)`) & `Scale #` %in% c("11", "21", "31", "41")) |
           grepl("-|,", `Scale #`)) %>% 
  print()







# UNIQUE OTO LAB-BOX-VIAL ---------------------------
qc_dupl_oto_LBV_entries <- esc.biodat.raw %>% 
  filter(`(R) OTOLITH LBV CONCAT` %in% as.character(esc.biodat.raw %>% 
                                                      filter(!is.na(`(R) OTOLITH LBV CONCAT`)) %>% 
                                                      group_by(`(R) OTOLITH LBV CONCAT`) %>% 
                                                      summarize(n=n()) %>% 
                                                      filter(n>1) %>%
                                                      pull(`(R) OTOLITH LBV CONCAT`))) %>% 
  print()



# UNIQUE WHATMANS ---------------------------
dupl_whatman_entries <- esc.biodat.raw %>% 
  filter(`(R) OTOLITH LBV CONCAT` %in% as.character(esc.biodat.raw %>% 
                                                      filter(!is.na(`(R) OTOLITH LBV CONCAT`)) %>% 
                                                      group_by(`(R) OTOLITH LBV CONCAT`) %>% 
                                                      summarize(n=n()) %>% 
                                                      filter(n>1) %>%
                                                      pull(`(R) OTOLITH LBV CONCAT`))) %>% 
  print()















# QC Summary ---------------------------
qc_summary <- data.frame(qc_flagName = c("qc0 - EBwR unCert Oto",
                                         "qc_noOtoID",
                                         "qc_noResults",
                                         "qc_noCWTID",
                                         "qc_noRslvdID",
                                         "qc_unRslvdID",
                                         "qc_unRslvdAge",
                                         "antijoin - PADS",
                                         "antijoin - Otos"),
                         number_records = c(nrow(qc_EBwR_uncertOtoID),
                                            nrow(qc_noOtoID),
                                            nrow(qc_noOtoResults),
                                            nrow(qc_noCWTID),
                                            nrow(qc_noRslvdID),
                                            nrow(qc_unRslvdID),
                                            nrow(qc_unRslvdAge),
                                            nrow(antijoin_PADS),
                                            nrow(antijoin_OM)),
                         description = c("Esc biodata entries where there was no CWT and duplicate otolith hatch codes were applied within one Brood Year resulting in >1 stock ID options, OR where unable to resolve to Stock level and are left making assumptions based on Facility. These records are still retained in the full biodata file as well, and assumptions are made based on likelihood or facility. These are indicated in the (R) OTOLITH ID METHOD column.",
                                         "Otolith hatch code and BY are given but there is no corresponding stock ID in the NPAFC file. Likely due to an error with mark reading.",
                                         "Otolith sample taken and BY available, but no hatchcode (results not processed yet?).",
                                         "There is a CWT available but no Stock ID.",
                                         "There is a CWT or an NPAFC ID but no Resolved stock ID.",
                                         "Otolith, CWT and/or PBT stock ID(s) do not match.",
                                         "Scale, CWT and/or PBT age(s) do not match.",
                                         "All WCVI CN PADS results that did not match to a sample in the Escapement Biodata file. Note they may go elsewhere though, e.g., Barkely Sound Test Fishery likely in FOS. ASSUMPTION: Removed 'WCVI Creel Survey' assumed already in CREST. Purpose here is to make sure there are no missing scales expected (i.e., samples not entered in base esc biodata file).",
                                         "All WCVI otolith results that did not match to a sample in the Escapement Biodata file. Note they may go elsewhere though, e.g., Barkely Sound Test Fishery likely in FOS. ASSUMPTION: Removed 'Sport' assumed already in CREST. Purpose here is to make sure there are no missing otoliths expected (i.e., samples not entered in base esc biodata file).")) %>% 
  print()


# ======================== Create readme ========================
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source escapement file:",
                             "source PADS file:",
                             "source Oto Manager file:",
                             "source NPAFC file:",
                             "!PLACEHOLDER! CWT source:",
                             "",
                             "sheet name:",
                             "Esc biodata w RESULTS",
                             "PBT parent biodata w RESULTS",
                             "QC Report",
                             "qc0 - EBwR unCert Oto",
                             "!NPAFC_dupl!",
                             "QC...",
                             "antijoin - PADS unmatched",
                             "antijoin - OM unmatched"
),
`2` = c(as.character(Sys.Date()), 
        "https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/joins/1-esc_biodata_with_results.R", 
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/SC_BioData_Management/2-Escapement/2015-2023_WCVI_Escapement-FSC_BioData.xlsx",
        "via direct R query to http://pac-salmon.dfo-mpo.gc.ca/CwtDataEntry/#/AgeBatchList",
        "For 2022, query from OtoManager online stored in: https://086gc.sharepoint.com/:x:/r/sites/PAC-SCAStockAssessmentSTAD/Shared%20Documents/WCVI%20STAD/Terminal%20CN%20Run%20Recon/2022/Communal%20data/BiodataResults/OtoManager_RecoverySpecimens_Area20-27_121-127_CN_2022_28Aug2023.xlsx?d=w398c15dd3c9b4ceb84d3083a215e9c6a&csf=1&web=1&e=NAxyjd",
        "//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/All CN Marks from NPAFC Otolith Database to May 1, 2023.xlsx",
        "!NOT IN YET!: http://pac-salmon.dfo-mpo.gc.ca/MRPWeb/#/Notice",  
        "",
        "sheet description:",
        "WCVI Chinook escapement biodata joined to PADS scale age results, OtoManager thermal mark results, NPAFC mark file to give otolith stock ID, CWT recoveries, and PBT up to 2021 return year. Currently does NOT include any GSI results.",
        "Subset of full database, filtered by Whatman IDs of parents contributing to returning fish. I.e., if an adult fish sampled for PBT during broodstock had a hit to a parent in the baseline, here their parents are pulled out of the overall database. This would show parents that contributed to adult recruits. These are parent fish identified through PBT only.",
        "Summary of QC flags and # of entries belonging to that flag.",
        "QC flag 0 tab. Only the Esc biodata w RESULTS ('EBwR') entries that correspond to NPAFC BY-hatchcode duplicates. See QC summary for details.",
        "All duplicate BY-hatchcodes documented by the NPAFC. To inform decisions around QC Flag 0.",
        "QC flag tabs. See QC summary report for details.",
        "PADS Antijoin tab. See QC summary for details.",
        "OtoManager Antijoin tab. See QC summary for details."))




