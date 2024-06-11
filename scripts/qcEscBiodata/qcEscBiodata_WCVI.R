
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
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`Scale Book #`) & !is.na(`Scale #`) ~ paste0(`Scale Book #`,sep="-",`Scale #`))) %>% 
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







# OTOLITH FLAGS ---------------------------
# Unique otolith lab-box-vial combination number                                                                        *****requires Seren sleuthing
qc_dupl_oto_LBV <- esc.biodat.raw %>% 
  filter(`(R) OTOLITH LBV CONCAT` %in% as.character(esc.biodat.raw %>% 
                                                      filter(!is.na(`(R) OTOLITH LBV CONCAT`)) %>% 
                                                      group_by(`(R) OTOLITH LBV CONCAT`) %>% 
                                                      summarize(n=n()) %>% 
                                                      filter(n>1) %>%
                                                      pull(`(R) OTOLITH LBV CONCAT`))) %>% 
  print()


# Non-standard oto box                                                                                                  *****requires Seren sleuthing/our decision making
qc_nonstd_oto_box <- esc.biodat.raw %>% 
  filter(str_detect(esc.biodat.raw$`Otolith Box #`, "^[:digit:]+$")=="FALSE") %>% 
  print()


# Non-standard oto vial                                                                                                 *****requires Seren sleuthing/our decision making
qc_nonstd_oto_vial <- esc.biodat.raw %>% 
  filter(nchar(`Otolith Specimen #`)>3 | grepl("[^A-Za-z0-9]", `Otolith Specimen #`)) %>% 
  print()



# WHATMAN FLAGS ---------------------------
# Two samples collected per fish - what are we trying to link results to/why collected? 
qc_multi_whatman_entries <- esc.biodat.raw %>% 
  filter(!is.na(`Specimen Reference DNA #...43`) & !is.na(`Specimen Reference DNA #...46`)) %>% 
  print()


# Duplicate DNA #s (both columns)
qc_dupl_whatman_entries <- rbind(
  esc.biodat.raw %>% 
    filter(`Specimen Reference DNA #...43` %in% as.character(esc.biodat.raw %>% 
                                                               filter(!is.na(`Specimen Reference DNA #...43`)) %>% 
                                                               group_by(`Specimen Reference DNA #...43`) %>% 
                                                               summarize(n=n()) %>% 
                                                               filter(n>1) %>%
                                                               pull(`Specimen Reference DNA #...43`))),
  esc.biodat.raw %>% 
    filter(`Specimen Reference DNA #...46` %in% as.character(esc.biodat.raw %>% 
                                                               filter(!is.na(`Specimen Reference DNA #...46`)) %>% 
                                                               group_by(`Specimen Reference DNA #...46`) %>% 
                                                               summarize(n=n()) %>% 
                                                               filter(n>1) %>%
                                                               pull(`Specimen Reference DNA #...46`)))
)%>% 
  print()



# SEX FLAGS ---------------------------
# Inconsistent male/female/jack                                                                                             *****requires Seren changing             
qc_sex_flag <- esc.biodat.raw %>% 
  filter(!is.na(Sex)) %>% 
  group_by(Sex) %>% 
  summarize(n=n()) %>% 
  print()


# Sex listed in two columns: Sex and Life-stage (visual)                                                                    *****requires Seren changing
qc_sex_flag <- esc.biodat.raw %>% 
  filter(!is.na(Sex) & !is.na(`Life-stage (visual)`)) %>% 
  print()


# LENGTH FLAGS ---------------------------
# Illogical lengths (e.g., POH>NF) where one fish has >1 length  measurement                                                *****requires Seren sleuthing
qc_length_flag <- esc.biodat.raw %>% 
  filter(!is.na(`POF Length (mm)`) | !is.na(`POH Length (mm)`) | !is.na(`TOTAL Length`) | !is.na(`Fork Length (mm)`) | !is.na(`Standard Length (mm)`)) %>%
  filter((`POH Length (mm)` > `Fork Length (mm)`) |
           (`POH Length (mm)` > `Standard Length (mm)`) | 
           (`Fork Length (mm)` > `Standard Length (mm)`) |
           (`POF Length (mm)` > `Fork Length (mm)`) |
           (`POF Length (mm)` > `Standard Length (mm)`) |
           (`POH Length (mm)` > `Standard Length (mm)`)) %>%
  print()



# INCONSISTENT SPATIAL DATA ---------------------------
# Sub-area isn't proper format (##-#), there are multiple sub-areas, or no sub area
# (are there details in "Capture Location/River Segment" that can inform?)
qc_subarea_flag <- esc.biodat.raw %>%
  filter(!grepl("-", `Sub-area`) | grepl(",", `Sub-area`) | is.na(`Sub-area`)) %>% 
  print()


# More than 1 stat area given
qc_area_flag <- esc.biodat.raw %>%
  filter(nchar(`Stat Area`)>3) %>% 
  print()

# Typos in river names (manual, not actually QC)
qc_river_typo <- esc.biodat.raw %>% 
  group_by(`Fishery / River`) %>% 
  summarize(n=n()) %>% 
  print()


# INCONSISTENT METHODS/SOURCE ---------------------------
# Gear inconsistencies
qc_gear_typo <- esc.biodat.raw %>% 
  group_by(Gear) %>% 
  summarize(n=n()) %>% 
  print()


# INCONSISTENT TEMPORAL DATA ---------------------------
# End date is before start date
qc_date_flags <- esc.biodat.raw %>% 
  filter(`Sample End Date (DD/MM/YYYY)` < `Sample Start Date (DD/MM/YYYY)`) %>% 
  print()


# "Month" is empty or doesn't align with start and/or end dates
qc_month_flags <- esc.biodat.raw %>% 
  filter(lubridate::month(`Sample Start Date (DD/MM/YYYY)`, label=T, abbr=F) != `Sample Month` | 
           lubridate::month(`Sample End Date (DD/MM/YYYY)`, label=T, abbr=F) != `Sample Month` ) %>% 
  print()



















################################################################################################################################################







# QC Summary ---------------------------
# Create a list of the QC dataframes (exclude anything that doesn't start with "qc")
flags_list <- Filter(function(x) is(x, "data.frame"), mget(ls())) 
flags_list <- flags_list[grep("qc", names(flags_list))]  


# Create as a dataframe
flags_df <- data.frame(qc_flag = names(flags_list))

# Calculate # rows per QC flag dataframe 
for(i in 1:length(flags_list)){
  flag_df$nrow[i] <- nrow(flags_list[[i]])
}


# Export definitions mapping file and manually update - may not have to re-do unless more QC flags are added
  # writexl::write_xlsx(flag_df, path=paste0(here::here("outputs", "Esc biodata qc flag definitions mapping "),
  #                                          Sys.Date(),
  #                                          ".xlsx"))
  # 





# ======================== Create readme ========================
readme_tab <- data.frame(`1` = c( ),
`2` = c( ))



################################################################################################################################################



#                                                                           X. EXPORT 



require(openxlsx)
list_of_datasets <- list("Name of DataSheet1" = dataframe1, "Name of Datasheet2" = dataframe2)
write.xlsx(list_of_datasets, file = "writeXLSX2.xlsx")


library(writexl)
sheets <- list("sheet1Name" = sheet1, "sheet2Name" = sheet2) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets, "path/to/location")




library(openxlsx) # loads library and doesn't require Java installed

your_df_list <- c("df1", "df2", ..., "dfn")

for(name in your_df_list){
  write.xlsx(x = get(name), 
             file = "your_spreadsheet_name.xlsx", 
             sheetName = name)
}



# ==================== Create the Excel file ====================
# Create workbook --------------------
R_OUT_ESC.RES <- openxlsx::createWorkbook()

# Add sheets to the workbook --------------------
openxlsx::addWorksheet(R_OUT_ESC.RES, "readme")
openxlsx::addWorksheet(R_OUT_ESC.RES, "Esc biodata w RESULTS")
openxlsx::addWorksheet(R_OUT_ESC.RES, "Esc biodat w RES - PBT parents")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC summary")
openxlsx::addWorksheet(R_OUT_ESC.RES, "qc0 - EBwR unCert Oto")
openxlsx::addWorksheet(R_OUT_ESC.RES, "!NPAFC_dupl!")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- No Oto stock ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- No Oto result")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- No CWT ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- No Reslvd ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- Unreslvd ID")
openxlsx::addWorksheet(R_OUT_ESC.RES, "QC- Unreslvd age")
openxlsx::addWorksheet(R_OUT_ESC.RES, "antijoin - PADS unmatched")
openxlsx::addWorksheet(R_OUT_ESC.RES, "antijoin - OM unmatched")

# Write data to the sheets --------------------
openxlsx::writeData(R_OUT_ESC.RES, sheet="readme", x=readme)
openxlsx::writeData(R_OUT_ESC.RES, sheet="Esc biodata w RESULTS", x=esc_biodata_w_RESULTS)
openxlsx::writeData(R_OUT_ESC.RES, sheet="Esc biodat w RES - PBT parents", x=PBT_parents)
openxlsx::writeData(R_OUT_ESC.RES, sheet="QC summary", x=qc_summary)
openxlsx::writeData(R_OUT_ESC.RES, sheet="qc0 - EBwR unCert Oto", x=qc_EBwR_uncertOtoID)
openxlsx::writeData(R_OUT_ESC.RES, sheet="!NPAFC_dupl!", x=NPAFC_dupl.df)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- No Oto stock ID", x=qc_noOtoID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- No Oto result", x=qc_noOtoResults)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- No CWT ID", x=qc_noCWTID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- No Reslvd ID", x=qc_noRslvdID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- Unreslvd ID", x=qc_unRslvdID)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "QC- Unreslvd age", x=qc_unRslvdAge)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "antijoin - PADS unmatched", x=antijoin_PADS)
openxlsx::writeData(R_OUT_ESC.RES, sheet = "antijoin - OM unmatched", x=antijoin_OM)











