##### TITLE: UsT3_UoE_spot_exploration #########################################

# Type: Script

# Date: 04/04/2022 
# Version: DRAFT
# Author: EGC
# Maintainer: e.grand-clement@exeter.ac.uk

# Project: UsT3

################################################################################


##### 0.load the data, prep and join
################################################################################

# Load relevant packages ------------------------------------------------

# Load the packages, overall data and summary tables
source("packages_n_plotting_functions.R")


# Load relevant data ------------------------------------------------
# 1.1. load data (all events per site) ####

path <- paste("..","..", "3_Clean_data", "Manganese", sep = "/")
list <- list.files(path) # list of file names in the folder
dataFiles <- lapply(Sys.glob(paste(path,"*.csv", sep ="/")), read.csv, na.strings = c("", "NA"))

# tidy
ALL <- bind_rows(dataFiles)
ALL <- ALL %>% rename("Lab_ID" = Solution.Label,
                      "Mn_257_ppb" = Mn.257.610.nm.ppb,
                      "Mn_259_ppb" = Mn.259.372.nm.ppb)

ALL <- subset(ALL, select = c(Lab_ID, Mn_257_ppb))
ALL$Filtered <- ifelse(grepl("_U",ALL$Lab_ID),'Total_Mn_ug_l',
                       ifelse(grepl("_F", ALL$Lab_ID), "Diss_Mn_ug_l",NA))                 # Isolate Filtered and Unfiltered for future comparison, and remove the label (needs doing now or won't have the time stamp)

ALL$Lab_ID <- str_remove(ALL$Lab_ID,"_U")
ALL$Lab_ID <- str_remove(ALL$Lab_ID,"_F")
ALL$Mn_257_ppb <- str_remove(ALL$Mn_257_ppb,"o")

# remove rows
ALL <- filter(ALL,
              !grepl("Continuing", Lab_ID),
              !grepl("10ppb", Lab_ID),
              !grepl("20ppb", Lab_ID),
              !grepl("40ppb", Lab_ID),
              !grepl("60ppb", Lab_ID),
              !grepl("100ppb", Lab_ID),
              !grepl("30ppb", Lab_ID),
              !grepl("50ppb", Lab_ID),
              !grepl("80ppb", Lab_ID),
              !grepl("Blank", Lab_ID),
              !grepl("BLANK", Lab_ID),
              !grepl("TXX", Lab_ID))

ALL <- ALL[ which(!is.na(ALL$Lab_ID)), ]                                        # will select all rows with NA
ALL$Mn_257_ppb <- as.numeric(ALL$Mn_257_ppb)

# Address dilution factor:
ALL$Mn_257_ppb <- ALL$Mn_257_ppb *1.02

# Deal with LOD ----------------------------------------------------------------
ALL <- ALL %>% 
  mutate(DL_flag = if_else(Mn_257_ppb < 1.8 & Filtered == "Total_Mn_ug_l", '<LOD',"NA"))
ALL <- ALL %>% 
  mutate(DL_flag = if_else(Mn_257_ppb < 2.1 & Filtered == "Diss_Mn_ug_l", '<LOD',"NA"))

# replace by value 1/2 the LOD
ALL <- ALL %>% 
  mutate(Mn_257_ppb = case_when(
    DL_flag == "<LOD" & Filtered == "Diss_Mn_ug_l" ~ 1.05,
    DL_flag == "<LOD" & Filtered == "Total_Mn_ug_l" ~ 0.9,
    TRUE ~ Mn_257_ppb))

n <- length(which(ALL$DL_flag=="<LOD"))


# download the log file ---------------------------------------------------------

path <- paste("..", "..", "..","Logs", sep = "/")
logfile <- list.files(path, pattern = "1_Sample_log_investigation_FINAL_2022-11-11")            # NEEDS CHANGING
path2 <- paste0(path,'/',logfile)
DateTime <- read_excel(path2)
#DateTime <- read.csv(path2)
DateTime <- subset(DateTime, select= c(Sample_code, Lab_ID, datetime_UTC, Routine_spot, code_site, catchment,reservoir_depth_m, waterbody_name ))
DateTime <- filter(DateTime, !grepl("__1900-01-00T00:00_", Sample_code))
DateTime <- DateTime%>%   filter(Lab_ID!="")                                    # removes rows that don't have a Lab ID
DateTime$code_site <- as.factor(DateTime$code_site)
level <- levels(DateTime$code_site)



# create datafiles - merge into one file ---------------------------------------
df <- merge(x = ALL, y = DateTime, by = "Lab_ID", all.x = TRUE)

# add relevant columns (seasons, months, year)
df$year <- as.numeric(format(df$datetime_UTC, "%Y"))
df$month <- format(df$datetime_UTC, "%b")
df$month = factor(df$month, levels = month.abb)
#TO$month <- as.numeric(format(TO$datetime_UTC, "%m"))
df$season <- FUN.season(df$datetime_UTC)
df$datetime_simple <- as.Date(df$datetime_UTC)  # add a column for just the date 
df <- droplevels(df)

# Save file --------------------------------------------------------------------
csvFileName <- paste("../../3_Clean_data/Manganese/All_compiled/ALL_Manganese.csv",sep="")
rdsFileName <- paste("../../3_Clean_data/Manganese/All_compiled/ALL_Manganese.rds",sep="")

write.csv(df, file = csvFileName)
saveRDS(df, file = rdsFileName)

# prep a spatial campaign file -------------------------------------------------
Spatial <- df %>% 
  filter(datetime_simple== as.Date("2022-07-19")|datetime_simple== as.Date("2022-10-04")|datetime_simple== as.Date("2022-05-31")|datetime_simple== as.Date("2022-10-10"))

csvFileName <- paste("../../3_Clean_data/Manganese/All_compiled/SPATIAL_Manganese.csv",sep="")
rdsFileName <- paste("../../3_Clean_data/Manganese/All_compiled/SPATIAL_Manganese.rds",sep="")

write.csv(Spatial, file = csvFileName)
saveRDS(Spatial, file = rdsFileName)


# prep a 'routine only' Meldon data file ---------------------------------------

site <- c("MVC", "MRS", "MFE")
Temporal <- subset(df, (code_site %in% site))           

csvFileName <- paste("../../3_Clean_data/Manganese/All_compiled/ROUTINE_Meldon_Manganese.csv",sep="")
rdsFileName <- paste("../../3_Clean_data/Manganese/All_compiled/ROUTINE_Meldon_Manganese.rds",sep="")

write.csv(Temporal, file = csvFileName)
saveRDS(Temporal, file = rdsFileName)


rm(csvFileName,rdsFileName )


