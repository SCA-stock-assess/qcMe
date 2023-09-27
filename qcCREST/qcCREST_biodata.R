
# QC any CREST biodata w results output, assuming columns are consistent which they should be
# Sept 2023

# there are TWO PLACES users need to enter information, input and output file names. CTRL+F "user entry" to find those places. 


# Set up ----------------
rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

# Load packages ----------------
library(here)
library(tidyverse)
library(readxl)
library(openxlsx)


# Helpers -------------
"%notin%" <- Negate("%in%")



################################################################################################################################################

#                                                                           I. FILE LOAD

crest_biodata_in <- readxl::read_excel(path = paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/test_QC_Reports/1-data-input/",
                                             # <<<< user entry: YOU NEED TO WRITE YOUR OWN FILENAME HERE IN QUOTATION MARKS: 
                                             "2023 ALL AREAS Biological_Data_With_Results (Wednesday, September 27, 2023 11 30 AM).xlsx"
                                             ), 
                               guess_max = 20000,
                               # <<<< user entry: YOU NEED TO WRITE YOUR OWN TAB NAME HERE IN QUOTATION MARKS: 
                               sheet = "Biological_Data_With"
                               ) %>%
  mutate(`(R) SCALE BOOK-NO CONCAT` = case_when(!is.na(SCALE_BOOK) & !is.na(SCALE_NO) ~ paste0(SCALE_BOOK, sep="-", SCALE_NO),
                                                TRUE ~ NA))



#############################################################################################################################################################

#                                                                           II. QC FLAGS, SUMMARY and README


# Add QC flag columns ---------------------------
crest_biodata_qcOUT <- crest_biodata_in %>% 
  mutate(qc1_otoNoSample = case_when(!is.na(OTOLITH_BOX) & !is.na(OTOLITH_SPECIMEN) & THERMALMARK=="No Sample" ~ "FLAG",
                                     TRUE ~ "ok"),
         qc2_scaleNoAge = case_when(is.na(RESOLVED_AGE) & is.na(PART_AGE_CODE) & !is.na(SCALE_BOOK) ~ "FLAG",
                                    TRUE ~ "ok"),
         qc3_CWTnoID = case_when(!is.na(CWT_HEAD_LABEL) & is.na(CWT_RESULT) ~ "FLAG",
                                 TRUE ~ "ok"),
         qc4_CWTzero = case_when(CWT_HEAD_LABEL==0 ~ "FLAG",
                                 TRUE ~ "ok"),
         qc5_WmanNoSample = case_when(!is.na(SPECIMEN_REFERENCE_DNA_NO) & is.na(DNA_RESULTS_STOCK_1) | DNA_RESULTS_STOCK_1=="NO SAMPLE" ~ "FLAG",
                                      TRUE ~ "ok"),
         qc6_CWTDNAdisagree = case_when(RESOLVED_STOCK_SOURCE=="CWT" & PROB_1>=0.8 & RESOLVED_STOCK_ORIGIN!=REGION_1_NAME ~ "FLAG",
                                        TRUE ~ "ok"),
         qc7_otoDNAdisagree = case_when(RESOLVED_STOCK_SOURCE=="Otolith Stock" & PROB_1>=0.8 & RESOLVED_STOCK_ORIGIN!=REGION_1_NAME ~ "FLAG",
                                        TRUE ~ "ok"),
         qc8_DNAuncert = case_when(RESOLVED_STOCK_SOURCE=="DNA" & PROB_1<0.8 ~ "FLAG",
                                   TRUE ~ "ok"),
         qc9_PBTmaybe = case_when(is.na(HATCHERY_ORIGIN) & PROB_1==1 & is.na(DNA_STOCK_2) ~ "FLAG",
                                  TRUE ~ "ok"),
         qc10_susSUS = case_when(RESOLVED_STOCK_ORIGIN=="SUS (assumed)" & 
                                   !is.na(CWT_RESULT) & CWT_RESULT!="No Tag" & 
                                   !is.na(THERMALMARK) & !THERMALMARK%in%c("No Sample","Not Marked") &
                                   !is.na(DNA_RESULTS_STOCK_1) & DNA_RESULTS_STOCK_1!="NO SAMPLE" ~ "FLAG",
                                 TRUE ~ "ok"),
         qc11_ageDisagree = case_when((YEAR-CWT_BROOD_YEAR)!=RESOLVED_AGE ~ "FLAG",
                                      TRUE ~ "ok"),
         qc12_nonstdSex = case_when(SEX %notin% c("M","F") ~ "FLAG",
                                    TRUE ~ "ok"),
         qc13_scalenum10 = case_when(SCALE_NO>10 & SPECIES==124 ~ "FLAG",
                                     TRUE ~ "ok"),
         qc14_otovial100 = case_when(OTOLITH_SPECIMEN>100 ~ "FLAG",
                                     TRUE ~ "ok"),
         qc15_uniqueDNA = case_when(!is.na(SPECIMEN_REFERENCE_DNA_NO) & SPECIMEN_REFERENCE_DNA_NO %in% c(
                                      crest_biodata_in%>%group_by(SPECIMEN_REFERENCE_DNA_NO)%>%summarize(n=n())%>%filter(n>1)%>%pull(SPECIMEN_REFERENCE_DNA_NO))
                                    ~ "FLAG",
                                    TRUE ~ "ok"),
         qc16_uniqueScale = case_when(!is.na(`(R) SCALE BOOK-NO CONCAT`) & `(R) SCALE BOOK-NO CONCAT` %in% c(
                                        crest_biodata_in%>%group_by(`(R) SCALE BOOK-NO CONCAT`)%>%summarize(n=n())%>%filter(n>1)%>%pull(`(R) SCALE BOOK-NO CONCAT`))
                                      ~ "FLAG",
                                      TRUE ~ "ok")
         ) %>% 
  print()


# QC Summary ---------------------------
qc_summary <- data.frame(qc_flagName = c("qc1_otoNoSample",
                                         "qc2_scaleNoAge",
                                         "qc3_CWTnoID",
                                         "qc4_CWTzero",
                                         "qc5_WmanNoSample",
                                         "qc6_CWTDNAdisagree",
                                         "qc7_otoDNAdisagree",
                                         "qc8_DNAuncert",
                                         "qc9_PBTmaybe",
                                         "qc10_susSUS",
                                         "qc11_ageDisagree",
                                         "qc12_nonstdSex",
                                         "qc13_scalenum10",
                                         "qc14_otovial100",
                                         "qc15_uniqueDNA",
                                         "qc16_uniqueScale",
                                         "",
                                         "total number of records in this file:"),
                         number_records = c(nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc1_otoNoSample=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc2_scaleNoAge=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc3_CWTnoID=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc4_CWTzero=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc5_WmanNoSample=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc6_CWTDNAdisagree=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc7_otoDNAdisagree=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc8_DNAuncert=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc9_PBTmaybe=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc10_susSUS=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc11_ageDisagree=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc12_nonstdSex=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc13_scalenum10=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc14_otovial100=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc15_uniqueDNA=="FLAG",]),
                                            nrow(crest_biodata_qcOUT[crest_biodata_qcOUT$qc16_uniqueScale=="FLAG",]),
                                            "",
                                            nrow(crest_biodata_qcOUT)),
                         description = c("Otolith box/vial numbers exist but there is 'No Sample'",
                                         "Scale book and number but no age, and no explanation given (e.g., resorbed etc)",
                                         "A CWT head label was submitted but the stock ID result is blank",
                                         "CWT head label entered as '0'",
                                         "Whatman sheet/DNA tracking numbers exist but no GSI result (blank)",
                                         "Stock ID assigned by CWT but GSI (>=80%) disagrees",
                                         "Stock ID assigned by otolith thermal mark but GSI >=80% disagrees",
                                         "A stock ID assigned by DNA but <80% certainty",
                                         "Hatchery origin given as a blank, but some possibility for PBT (PROB=1.00). Note this should be approached with lots of caution and is stock-specific.",
                                         "SUS (assumed) that have ID methods available - A CWT head label was submitted but the stock ID result is blank",
                                         "Cases where the RESOLVED_AGE does not match the catch YEAR minus the CWT_BROOD_YEAR",
                                         "Sex designation does not fall as M or F - propose changing all other designations to 'unk'",
                                         "Cases where scale numbers are >10 (should not be >10)",
                                         "Cases where otolith vial number is >100 (should not be >100)",
                                         "duplicates in DNA specimen numbers",
                                         "duplicates in scale book-no concatenation",
                                         "",
                                         paste0("for ", paste(unique(crest_biodata_qcOUT$YEAR), collapse = " ") ))) %>% 
  print()



# Create readme ---------------------------
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source CREST file(s):",
                             "",
                             "sheet name:",
                             "CREST Biodata",
                             "QC summary"),
                     `2` = c(as.character(Sys.Date()), 
                             "https://github.com/SCA-stock-assess/qcMe", 
                             "download from CREST stored in SCD_Stad/test_QC_Reports/1-data-input",
                             "",
                             "sheet description:",
                             "CREST Biodata with a number of QC flag columns at the end of the file.",
                             "Summary of QC flag columns and # of entries belonging to that flag."
                             #rep("", 11)
                     ))


#############################################################################################################################################################

#                                                                           III. EXPORT 


# Export ---------------------------
# Create a blank workbook
CRESTbiodata.QC <- openxlsx::createWorkbook()

# Add sheets to the workbook
openxlsx::addWorksheet(CRESTbiodata.QC, "readme")
openxlsx::addWorksheet(CRESTbiodata.QC, "CREST Biodata")
openxlsx::addWorksheet(CRESTbiodata.QC, "QC summary")


# Write data to the sheets
openxlsx::writeData(CRESTbiodata.QC, sheet="readme", x=readme)
openxlsx::writeData(CRESTbiodata.QC, sheet="CREST Biodata", x=crest_biodata_qcOUT)
openxlsx::writeData(CRESTbiodata.QC, sheet="QC summary", x=qc_summary)




# Export to network output folder ---------------------------
openxlsx::saveWorkbook(CRESTbiodata.QC, 
                       file=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/test_QC_Reports/2-qc-output/",
                                   # <<<<< user entry: YOU NEED TO WRITE YOUR OWN OUTPUT FILE NAME HERE IN QUOTATION MARKS: 
                                   "2023 ALL AREAS Biological_Data_With_Results (Wednesday, September 27, 2023 11 30 AM) - QC REPORT.xlsx"),
                       overwrite=T,
                       returnValue=T)




























