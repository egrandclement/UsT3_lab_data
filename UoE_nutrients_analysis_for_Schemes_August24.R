#### TITLE: UsT3_UoE_spot_exploration #########################################

# Type: 

# Date: 28/11/2022
# Version: DRAFT
# Author: EGC + JL
# Maintainer: e.grand-clement@exeter.ac.uk

# Project: UsT3 - looking at the nutrient data

## Update in Nov23 
# combined (SEAL + Bluvision) data fpr the analysis 

################################################################################
#setwd("//cles-diamond.ex.ac.uk/brazier/current_projects/UsT3/Data/Data_analysis/UsT3_lab_data")

# Load the packages, overall data and summary tables
source("packages_n_plotting_functions.R")
options("scipen"=100, "digits"=6)


# 1.load the data and tidy -----------------------------------------------------

#1.1 Load the Nutrients (SEAL+Bluvision) data -------------------------------------

df <- read.csv("../ust3_bluvision/05_data/bluvision_als_seal_data_240819.csv")

# start exploring Nutrients data

# add relevant columns (seasons, months, year)
df$datetime_utc <- as.POSIXct(df$datetime_utc, format= "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC") # needs checking the format of initial data
df$year <- as.numeric(format(df$datetime_utc, "%Y"))
df$month <- format(df$datetime_utc, "%b")
df$month = factor(df$month, levels = month.abb)
#df$month <- as.numeric(format(df$datetime_utc, "%m"))
df$season <- FUN.season(df$datetime_utc)
df$datetime_simple <- as.Date(df$datetime_utc)                                  # add a column for just the date 
#df$LOD_flag <- as.factor((df$LOD_flag))
df <- droplevels(df)
df <- subset(df, select = -c(16,18,19,20,22,24,25,26,27,28,29,30,31))

# In the Seal data, the "TP method" was used to measure Orthophosphate. Therefore the results were
# PO4 but automatically labelled as TP.
# so I am moving the data around so that it now labelled as it should (PO4, NOT TP).

df <- df %>%
  mutate(orthophosphate_p_mg_l = if_else(lab == "uoe_seal" & is.na(orthophosphate_p_mg_l), 
                                tp_p_mg_l, orthophosphate_p_mg_l),
    tp_p_mg_l = if_else(lab == "uoe_seal", NA, tp_p_mg_l)
  )



# Create a new column TN by merging tn_mg_l and tn_n_mg_l
df$TN <- ifelse(!is.na(df$tn_mg_l), df$tn_mg_l, df$tn_n_mg_l)

# Create a new column TP by merging tp_mg_l and tp_p_mg_l
df$TP <- ifelse(!is.na(df$tp_mg_l), df$tp_mg_l, df$tp_p_mg_l)

df <- subset(df, select = -c(11,16,17,18))                                      # remove initial cols

# turn the file to a long format to have all the values into test
df_long <- df %>%
  pivot_longer(
    cols = c(10, 11, 12, 13, 14,19,20),  # Columns to pivot to long format (test/results)
    names_to = "test",                     # New column for the names of these columns
    values_to = "results"                  # New column for the corresponding values
  )

#1.2 Load the LOD data and sort ---------------------------------------------------
 
LOD <- read.csv("Data_input/LOD_Nutrients.csv")

# make LOD long format

LOD <- LOD  %>%
  pivot_longer(
    cols = c(2,3,4,5,6,7,8),  # Columns to pivot to long format (test/results)
    names_to = "test",                     # New column for the names of these columns
    values_to = "LOD"                  # New column for the corresponding values
  )

# merge with LOD and remove rows with no value in results
df_long <- merge(x=df_long, y=LOD, by = c("lab","test"))
df_long <- df_long[!is.na(df_long$result), ]

# create qualifier
df_long <- df_long %>%
  mutate(LOD_flag = ifelse(results <= LOD, "<LOD", ""),
         results = ifelse(LOD_flag == "<LOD", LOD/2, results))                  # replace the value by 1/2 LOD


## 1.3 Make a spatial table for mapping ############################################
spatial_WIS <- df_long %>%
  filter(datetime_simple == as.Date("2023-01-16") | datetime_simple == as.Date("2024-07-08"))

# make TP in ug / L
spatial_WIS <- spatial_WIS %>%
  mutate(results = case_when(
    test == "TP" ~ results * 1000,   # Multiply by 1000 if Test is "TP"
    test == "orthophosphate_p_mg_l" ~ results * 1000,    
    TRUE ~ results                                                              # Leave other results unchanged
  ))

# make LOD in ug / L
spatial_WIS <- spatial_WIS %>%
  mutate(LOD = case_when(
    test == "TP" ~ LOD * 1000,   # Multiply by 1000 if Test is "TP"
    test == "orthophosphate_p_mg_l" ~ LOD * 1000,    
    TRUE ~ LOD                                                              # Leave other results unchanged
  ))


# specify the folder where you want to save the file
folder_path <- "../../3_Clean_data/Nutrients/"  # Change this to your desired folder path

# Generate the filename with the current date
current_date <- format(Sys.Date(), "%Y-%m-%d")  # Format date as "YYYY-MM-DD"
file_name <- paste0("WIS_spatial_nutrients_", current_date, ".rds")             # Create the filename
file_name2 <- paste0("WIS_spatial_nutrients_", current_date, ".csv")            # Create the filename
# 4. Save the RDS file
saveRDS(spatial_WIS, file = file.path(folder_path, file_name))
write.csv(spatial_WIS, file = file.path(folder_path, file_name2))



### Finalise the file with all non spatial data

## reorder the sites and remove unused levels
site <- c("BNC", "BHW", "BLB", "BRS",
          "RMM","RWB", "RJW", "RBC", "RRW", "RNE", "RED", "RES", "RRS",
          "STW", "STS","SNS","SPS","SLS","SRS",
          "WRB", "WES","WNS", "WLS","WRS")

df_long <- subset(df_long, (code_site %in% site))
df_long$code_site <- factor(df_long$code_site, levels = c("BNC", "BHW", "BLB", "BRS",
                                                "RMM","RWB", "RJW", "RBC", "RRW", "RNE", "RED", "RES", "RRS",
                                                "STW", "STS","SNS","SPS","SLS","SRS",
                                                "WRB", "WES","WNS", "WLS","WRS"))


############ save an rds file 

# specify the folder where you want to save the file
folder_path <- "../../3_Clean_data/Nutrients/"  # Change this to your desired folder path

# Generate the filename with the current date
current_date <- format(Sys.Date(), "%Y-%m-%d")  # Format date as "YYYY-MM-DD"
file_name <- paste0("All_nutrient_with_LOD_", current_date, ".rds")             # Create the filename
file_name2 <- paste0("All_nutrient_with_LOD_", current_date, ".csv")            # Create the filename

# 4. Save the RDS file
saveRDS(df_long, file = file.path(folder_path, file_name))
write.csv(df_long, file = file.path(folder_path, file_name2))

rm(current_date, file_name, file_name2, folder_path)



# 2. Calculate averages and regular statistics #################################

# 2.1. Regular stats -----------------------------------------------------------
# Add LOD

summary <- df_long %>%
  mutate(results = case_when(
    test == "TP" ~ results * 1000,   # Multiply by 1000 if Test is "TP"
    test == "orthophosphate_p_mg_l" ~ results * 1000,    
    TRUE ~ results                                                              # Leave other results unchanged
  )) %>%
  filter(!is.na(results)) %>%        # Remove rows with NA in results
  group_by(catchment, test) %>%
  summarize(n = sum(!is.na(results)),
            "n <LOD" = sum(LOD_flag == "<LOD"),
            "% Values <LOD" = round(sum(LOD_flag == "<LOD")/n()*100, 1),
            Min = round(min(results, na.rm = TRUE), 3),
            Max = round(max(results, na.rm = TRUE), 3),
            Mean = round(mean(results, na.rm = TRUE), digits = 3),
            Median = round(median(results, na.rm = TRUE), digits = 3),
            SD = round(sd(results, na.rm = TRUE), digits = 3)) %>%
  mutate(Min = case_when(Min < 0 ~ 0, .default = Min)) # Replace negative Min values with 0

# 2.2. split per catchment -----------------------------------------------------
# make one table per catchment (ready for the report)

catch <- c("Stithians", "Roadford", "Wistlandpound", "Burrator") 

for (i in catch){
  list_df <- split(summary, summary$catchment) 
  print(i)
  write.csv(list_df[[i]], paste0("csv_output/Nutrients/Update_August_24/Summary_conc_",i, ".csv"))
}

# 2.3. Split per site ----------------------------------------------------------

summary1 <- df_long %>%  
  mutate(results = case_when(
    test == "TP" ~ results * 1000,   # Multiply by 1000 if test is "TP"
    test == "orthophosphate_p_mg_l" ~ results * 1000,    
    TRUE ~ results                   # Leave other results unchanged
  )) %>% 
  filter(!is.na(results)) %>%
  group_by(catchment, test, code_site) %>%
  summarize(n=sum(!is.na(results)),
            "n <LOD" = sum(LOD_flag == "<LOD"),
            "% Values <LOD" = round(sum(LOD_flag == "<LOD")/n()*100, 1),
            Min = round(min(results, na.rm = TRUE),3),
            Max = round(max(results, na.rm = TRUE),3),
            Mean=round(mean(results,na.rm = T),digits = 3),
            Median = round(median(results,na.rm = T),digits = 3),
            SD = round(sd(results,na.rm = T),digits = 3))  %>%                  #Creates the summary table
  mutate(Min= case_when(Min <0 ~0, .default = Min))          
  

for (i in catch){
  list_df <- split(summary1, summary1$catchment) 
  print(i)
  write.csv(list_df[[i]], paste0("csv_output/Nutrients/Update_August_24/Summary_conc_Site_",i, ".csv"))
}

# 3. Calculate ratios -------------------------------------------------------------

# this is commented out because the values are moslty <LOD so meaningless

# ROUTINE DATA --

# # pivot wider
# df1 <- pivot_wider(
#   rout,
#   id_cols = c(catchment, code_site, Lab_ID, datetime_utc, datetime_simple, month, year, season, source),    # name of the column to put as rows
#   names_from = c(test),                  # name of the different columns, but combine the header with the unit
#   values_from = results,                 # what to put in the cell
#   values_fn = mean                       # this is averages values if there are more than one
# )


# merge with ALS data -

# df2 <- merge(x = df1, y = als, by = c("Lab_ID", "catchment", "code_site", "Lab_ID", "datetime_utc", "source"), all.x = TRUE)

# df2 <- bind_rows(df1, als)                 # could not get this to work with merge, so used bind instead
# 
# 
# df2$NH3_TON <- df2$NH3/df2$TON
# df2$TON_PO4 <- df2$TON/df2$PO4
# 

# ###
# # plot concentrations per catchment --- DOES NOT WORK!  
# 
# # manual split per catchment cos  I can't do the loop within a loop
# 
# Tavy <- rout %>%
#   filter(catchment == "Tavy")
# 
# 
# param <- c("NO3_cal","TP","NH3","NO2","TON","PO4","PO4_low")
# plot_list <- list() 
# 
# unique(Tavy$test)
# 
# 
# for (i in param){
#     
#   print(i)
#   p <- ggplot (data=subset(Tavy, param == i),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
#     geom_boxplot() + 
#     #geom_hline(yintercept = unique(subset(Tavy, param == i)$LOD)[1], linetype= "dashed", colour = "darkred", linewidth = 0.75)+
#     
#     labs(title = i) +
#     theme_coding()+
#     theme(legend.position = "bottom",
#           legend.text = element_text(size = 14),
#           axis.title.y = element_text(size = 14),
#           axis.title.x = element_blank(),
#           axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
#           plot.title = element_text(size = 14, vjust = 1, hjust = 0.5),
#           legend.title = element_blank())+
#     ylab ("mg/L") +
#     #scale_y_continuous(expand = c(0,0), limits =c(-0.5,max(Tavy$Result_corrected)),
#                        #breaks = seq(-0.5,max(Tavy$Result_corrected),by=0.5))+
#     scale_fill_manual(values=coloursUST)
#   
#   plot_list[[i]] <- p
# }
# 
# plot_list[[2]]
# 
# 
# # bodge to make appear the right hlim - add the lines individually
# #NO2
# plot_list[[1]] <- plot_list[[1]] +
#   geom_hline(yintercept = 0.0011, linetype= "dashed", colour = "darkred", linewidth = 0.75)
# # TP
# plot_list[[2]] <- plot_list[[2]] +
#   geom_hline(yintercept = 0.125, linetype= "dashed", colour = "darkred", linewidth = 0.75)
# #NH3
# plot_list[[3]] <- plot_list[[3]] +
#   geom_hline(yintercept = 0.0125, linetype= "dashed", colour = "darkred", linewidth = 0.75)
# #TON
# plot_list[[4]] <- plot_list[[4]] +
#   geom_hline(yintercept = 0.104, linetype= "dashed", colour = "darkred", linewidth = 0.75)
# #NO3_cal
# plot_list[[5]] <- plot_list[[5]] +
#   geom_hline(yintercept = 0.104, linetype= "dashed", colour = "darkred", linewidth = 0.75)
# #PO4
# plot_list[[6]] <- plot_list[[6]] +
#   geom_hline(yintercept =  0.125, linetype= "dashed", colour = "darkred", linewidth = 0.75)
# #PO4_low
# plot_list[[7]] <- plot_list[[7]] +
#   geom_hline(yintercept =  0.125, linetype= "dashed", colour = "darkred", linewidth = 0.75)
# 
# 
# # Save plots to tiff. Makes a separate file for each plot.
# for (i in param){
#   file_name = paste("Plots/Nutrients/Boxplot_TAVY_site_ ", i, ".tiff", sep="")
#   tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
#   )
#   print(plot_list[[i]])
#   dev.off()
# }

  ##############

# BIG ISSUE WITH THE WORK ABOVE SO DOING IT MANUALLY 

# 4. Plotting (manually) -------------------------------------------------------

# slight amendments to the plot style

theme_adjust <- function(){
  theme_bw()+
    theme(legend.position = "none",
          #legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, #angle = 45, 
                                     vjust = 0.5),
          plot.title = element_blank(),
          #plot.title = element_text(size = 14, vjust = 1, hjust = 0.5),
          legend.title = element_blank())
}

# 4.1. Wistlandpound ----------------------------------------------------------------

Wistlandpound <- df_long %>%
  filter(catchment == "Wistlandpound")

Wistlandpound <- Wistlandpound[!is.na(Wistlandpound$code_site),]                # remove NAs from spatial campaign

# remove spatial campaign
wis <- c("WRB", "WES","WNS", "WLS","WRS")
Wistlandpound <- Wistlandpound[Wistlandpound$code_site %in% wis,] 


plot_list <- list()

# NH3
plot_list$NH3 <- ggplot (data=subset(Wistlandpound, test == "ammonia_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.025, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Wistlandpound") +
  theme_adjust()+
  ylab(expression(paste("NH"["3"]~" - N (mg ", L^-1,")"))) + 
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)


# NO2
plot_list$NO2 <- ggplot (data=subset(Wistlandpound, test == "nitrite_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.0022, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Wistlandpound") +
  theme_adjust()+
  ylab(expression(paste("NO"["2"]~" - N (mg ", L^-1,")"))) +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

# TON
plot_list$TON <- ggplot (data=subset(Wistlandpound, test == "ton_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.25, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Wistlandpound") +
  theme_adjust()+
  ylab (expression(paste("TON - N (mg ", L^-1,")")))+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

# TN
plot_list$TN <- ggplot (data=subset(Wistlandpound, test == "TN"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 1.65, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Wistlandpound") +
  theme_adjust()+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab (expression(paste("TN as N (mg ", L^-1,")")))+
  scale_fill_manual(values=coloursUST)

# TP
plot_list$TP <- ggplot (data=subset(Wistlandpound, test == "TP"),aes(x = code_site , y = results*1000, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 16.5, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Wistlandpound") +
  theme_adjust()+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab (expression(paste("TP as P (ug ", L^-1,")")))+
  scale_fill_manual(values=coloursUST)


# NO3_cal
plot_list$NO3_cal <- ggplot (data=subset(Wistlandpound, test == "nitrate_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.0021, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  theme_coding()+
  theme_adjust()+
  labs(title = "Wistlandpound") +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab(expression(paste("NO"["3"]~" - N (mg ", L^-1,") - Calculated"))) +

  scale_fill_manual(values=coloursUST)

# PO4
plot_list$PO4 <- ggplot (data=subset(Wistlandpound, test == "orthophosphate_p_mg_l"),aes(x = code_site , y = results*1000, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 100, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Wistlandpound") +
  theme_adjust()+
  ylab(expression(paste("PO"["4"]~" - P (ug ", L^-1,")"))) +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

param <- c("NO3_cal","TP", "TN","NH3", "NO2","TON" ,"PO4"#,"PO4_low"
           )

# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Nutrients/Update_August_24/Boxplot_Wistlandpound_site_", i, "_n.tiff", sep="")
  tiff(file_name, width = 250, height = 100,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}

## 4.2. Stithians --------------------------------------------------------------

Stithians <- df_long %>%
  filter(catchment == "Stithians")

Stithians <- Stithians[!is.na(Stithians$code_site),]                # remove NAs from spatial campaign

plot_list <- list()

# NH3
plot_list$NH3 <- ggplot (data=subset(Stithians, test == "ammonia_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.0025, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Stithians") +
  theme_adjust()+
  ylab(expression(paste("NH"["3"]~" - N (mg ", L^-1,")"))) + 
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)


# NO2
plot_list$NO2 <- ggplot (data=subset(Stithians, test == "nitrite_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.0022, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Stithians") +
  theme_adjust()+
  ylab(expression(paste("NO"["2"]~" - N (mg ", L^-1,")"))) +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

# TON
plot_list$TON <- ggplot (data=subset(Stithians, test == "ton_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.25, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Stithians") +
  theme_adjust()+
  ylab (expression(paste("TON - N (mg ", L^-1,")")))+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

# TN
plot_list$TN <- ggplot (data=subset(Stithians, test == "TN"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 1.65, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Stithians") +
  theme_adjust()+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab (expression(paste("TN as N (mg ", L^-1,")")))+
  scale_fill_manual(values=coloursUST)

# TP
plot_list$TP <- ggplot (data=subset(Stithians, test == "TP"),aes(x = code_site , y = results*1000, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 16.5, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Stithians") +
  theme_adjust()+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab (expression(paste("TP as P (ug ", L^-1,")")))+
  scale_fill_manual(values=coloursUST)


# NO3_cal
plot_list$NO3_cal <- ggplot (data=subset(Stithians, test == "nitrate_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.0021, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  theme_coding()+
  theme_adjust()+
  labs(title = "Stithians") +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab(expression(paste("NO"["3"]~" - N (mg ", L^-1,") - Calculated"))) +
  
  scale_fill_manual(values=coloursUST)

# PO4
plot_list$PO4 <- ggplot (data=subset(Stithians, test == "orthophosphate_p_mg_l"),aes(x = code_site , y = results*1000, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 100, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Stithians") +
  theme_adjust()+
  ylab(expression(paste("PO"["4"]~" - P (ug ", L^-1,")"))) +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

param <- c("NO3_cal","TP", "TN","NH3", "NO2","TON" ,"PO4")


# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Nutrients/Update_August_24/Boxplot_Stithians_site_", i, "_n.tiff", sep="")
  tiff(file_name, width = 250, height = 100,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}



# 4.3. Roadford -------------------------------------------------------------------------
# 
Roadford <- df_long %>%
  filter(catchment == "Roadford")%>%
  filter(!is.na(code_site) & is.finite(results))

Roadford <- Roadford[!is.na(Roadford$code_site),]                # remove NAs from spatial campaign

plot_list <- list()

# NH3
plot_list$NH3 <- ggplot (data=subset(Roadford, test == "ammonia_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.025, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Roadford") +
  theme_adjust()+
  ylab(expression(paste("NH"["3"]~" - N (mg ", L^-1,")"))) + 
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)


# NO2
plot_list$NO2 <- ggplot (data=subset(Roadford, test == "nitrite_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.0022, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Roadford") +
  theme_adjust()+
  ylab(expression(paste("NO"["2"]~" - N (mg ", L^-1,")"))) +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

# TON
plot_list$TON <- ggplot (data=subset(Roadford, test == "ton_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.25, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Roadford") +
  theme_adjust()+
  ylab (expression(paste("TON - N (mg ", L^-1,")")))+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

# TN
plot_list$TN <- ggplot (data=subset(Roadford, test == "TN"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 1.65, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Roadford") +
  theme_adjust()+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab (expression(paste("TN as N (mg ", L^-1,")")))+
  scale_fill_manual(values=coloursUST)

# TP
plot_list$TP <- ggplot (data=subset(Roadford, test == "TP"),aes(x = code_site , y = results*1000, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 16.5, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
  theme_coding()+
  labs(title = "Roadford") +
  theme_adjust()+
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab (expression(paste("TP as P (ug ", L^-1,")")))+
  scale_fill_manual(values=coloursUST)


# NO3_cal
plot_list$NO3_cal <- ggplot (data=subset(Roadford, test == "nitrate_n_mg_l"),aes(x = code_site , y = results, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 0.0021, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  theme_coding()+
  theme_adjust()+
  labs(title = "Roadford") +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  ylab(expression(paste("NO"["3"]~" - N (mg ", L^-1,") - Calculated"))) +
  
  scale_fill_manual(values=coloursUST)

# PO4
plot_list$PO4 <- ggplot (data=subset(Roadford, test == "orthophosphate_p_mg_l"),aes(x = code_site , y = results*1000, fill = code_site)) +                         # values indicate to use the value in the cell
  geom_boxplot() + 
  geom_hline(yintercept = 100, linetype= "dashed", colour = "grey60", linewidth = 0.75)+
  #scale_y_continuous (limits =c(0,0.11),
                     #breaks = seq(0,0.11, by=0.025))+
    theme_coding()+
  labs(title = "Roadford") +
  theme_adjust()+
  ylab(expression(paste("PO"["4"]~" - P (ug ", L^-1,")"))) +
  stat_summary(fun.data = stat_box_data,                                      # Use function to add n
               geom = "text",#hjust = 0.5, vjust = 0.9,size = 4, 
               colour = "grey40") +
  scale_fill_manual(values=coloursUST)

param <- c("NO3_cal","TP", "TN","NH3", "NO2","TON" ,"PO4"#,"PO4_low"
)


# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Nutrients/Update_August_24/Boxplot_Roadford_site_", i, "_n.tiff", sep="")
  tiff(file_name, width = 250, height = 100,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}


# 5. TIMESERIES OF TP AND TN ---------------------------------------------------

# 5.1. TIMESERIES --------------------------------------------------------------

# plotting function NOT SPATIAL data 

# TP ----------------------------------------------------------------------

catch <- c("Roadford", "Stithians", "Wistlandpound")

plot_list <- list()                                                             # loop to make plots 
Ylab <- expression(paste("TP (ug ", L^-1,")"))
legnd <- c("TP", "Month", Ylab)
#catch <- c(unique(TO2$catchment)) 
#max<- max(TO$geosmin_ng_l, na.rm=TRUE)
lims <- as.POSIXct(strptime(c("2022-02-01 00:00","2024-07-15 00:00"), format = "%Y-%m-%d %H:%M"))  



for (i in catch){
  print(i)
  
  sub <- df_long[df_long$catchment == i,]
  
  p <- ggplot() +
    #geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", linewidth = 0.5)+
    #geom_segment(aes(x=  as.POSIXct("2023-08-07 09:25:00"), xend =  as.POSIXct("2024-07-15 00:00:00"), y=0.5, yend= 0.5), colour = "dimgrey", linetype = "dotted", linewidth = 0.6)+
    #geom_segment(aes(x=  as.POSIXct("2022-04-01 00:00:00"), xend =  as.POSIXct("2023-08-07 09:25:00"), y=2, yend= 2), colour = "dimgrey", linetype = "dotted", linewidth = 0.6)+
    geom_hline(yintercept = 16.5, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
    geom_point(data=subset(sub,test == "TP"), 
               aes(x= datetime_utc,  y = results*1000,colour = code_site), size = 3, position=position_jitter(h=0.15,w=0.1))+
    #geom_point(data= subset(sub[which(!is.na(sub$geosmin_lod)),], catchment == i), aes(x= datetime_utc, y= geosmin_ng_l,
                                                                                       #colour = code_site), shape = 1, size = 3, position=position_jitter(h=0.15,w=0.1))+
    geom_point(size = 9)+
    theme_coding()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5))+
    ylab (Ylab)+
    scale_y_continuous(breaks = pretty_breaks(n = 6)) +  
    scale_x_datetime(name = "Sampling month", date_breaks = "1 month", 
                     limits = lims, date_labels = "%m/%Y") + 
    scale_colour_manual(
      values = grad_UsT(length(unique(sub$code_site)) + 1))
  
  plot_list[[i]] <- p
  
}


# Save plots to tiff. Makes a separate file for each plot.
for (i in catch){
  file_name = paste("Plots/Nutrients/Update_August_24/TS_TP_PerSite_ ", i, "_Jul24.tiff", sep="")
  tiff(file_name, width = 300, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}


# TN ----------------------------------------------------------------------

catch <- c("Roadford", "Stithians", "Wistlandpound")

plot_list <- list()                                                             # loop to make plots 
Ylab <- expression(paste("TN (mg ", L^-1,")"))
legnd <- c("TN", "Month", Ylab)
#catch <- c(unique(TO2$catchment)) 
#max<- max(TO$geosmin_ng_l, na.rm=TRUE)
lims <- as.POSIXct(strptime(c("2022-02-01 00:00","2024-07-15 00:00"), format = "%Y-%m-%d %H:%M"))  



for (i in catch){
  print(i)
  
  sub <- df_long[df_long$catchment == i,]
  
  p <- ggplot() +
    #geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", linewidth = 0.5)+
    #geom_segment(aes(x=  as.POSIXct("2023-08-07 09:25:00"), xend =  as.POSIXct("2024-07-15 00:00:00"), y=0.5, yend= 0.5), colour = "dimgrey", linetype = "dotted", linewidth = 0.6)+
    #geom_segment(aes(x=  as.POSIXct("2022-04-01 00:00:00"), xend =  as.POSIXct("2023-08-07 09:25:00"), y=2, yend= 2), colour = "dimgrey", linetype = "dotted", linewidth = 0.6)+
    geom_hline(yintercept = 1.65, linetype= "dashed", colour = "grey40", linewidth = 0.75)+
    geom_point(data=subset(sub,test == "TN"), 
               aes(x= datetime_utc,  y = results,colour = code_site), size = 3, position=position_jitter(h=0.15,w=0.1))+
    #geom_point(data= subset(sub[which(!is.na(sub$geosmin_lod)),], catchment == i), aes(x= datetime_utc, y= geosmin_ng_l,
    #colour = code_site), shape = 1, size = 3, position=position_jitter(h=0.15,w=0.1))+
    geom_point(size = 9)+
    theme_coding()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5))+
    ylab (Ylab)+
    scale_y_continuous(breaks = pretty_breaks(n = 6)) +  
    scale_x_datetime(name = "Sampling month", date_breaks = "1 month", 
                     limits = lims, date_labels = "%m/%Y") + 
    scale_colour_manual(
      values = grad_UsT(length(unique(sub$code_site)) + 1))
  
  plot_list[[i]] <- p
  
}


# Save plots to tiff. Makes a separate file for each plot.
for (i in catch){
  file_name = paste("Plots/Nutrients/Update_August_24/TS_TN_PerSite_ ", i, "_Jul24.tiff", sep="")
  tiff(file_name, width = 300, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}

# 6. With re reservoir data to see if there is a difference with depth ----------------

#  Load reservoir level 

level <- read.csv("../../SWW_res_levels/reservoir_levels_to_2024-07-10.csv")

#select only res of interest and dates after 1/1/2022
level <- subset(level, select= c(date, burrator_below_twl_m, roadford_below_twl_m, stithians_below_twl_m, wistlandpound_below_twl_m))

level <- level %>%                                                              #transform the date from chr to a date format
  mutate(datetime_simple = as.Date(date, format = "%d/%m/%Y")) %>%
  select(-date)

level <- level %>%
  filter(datetime_simple >as.Date("2022-01-01"))

# rename to match nutrients

# Create a list of data frames, each with 'datetime' and one reservoir level
ResLevel <- map(catch, function(reservoir) {
  level %>%
    select(datetime_simple, all_of(paste0(tolower(reservoir), "_below_twl_m"))) %>%
    rename_with(~ paste0(reservoir, "_below_twl_m"), -datetime_simple)
})

# make into a list of df and name them by catchment
df_list <- list()                                                               # Initialize an empty list to store the resulting dataframes
catch <- c( "Roadford", "Stithians", "Wistlandpound") 

ResLevel <- map(ResLevel, function(df) {
  df %>% rename(ResLevel_m_btwl = 2)  # Renames the second column to 'level_mbtwl'
})

names(ResLevel) <- catch

## 6.1 Roadford -----------------------------------------------------------------

# TP ---------------------------------------------------------------------------

legnd <- c("XX", expression(paste("Depth (m below top water level)")), "Reservoir level (m below TWL)")

dat <- df[!is.na(df$reservoir_depth_m),]
dat <- dat[dat$catchment =="Roadford",]
dat <- dat[!is.na(dat$TP),]
level <- subset(ResLevel[["Roadford"]])
dat <- left_join(dat, level, by="datetime_simple") %>%
  mutate(sample_depth_mBSurf = ResLevel_m_btwl+reservoir_depth_m)


# Define the color gradient
color_gradient <- rev(grad_UsT1(6))  # Adjust the number of colors as needed

p <- ggplot() +
  geom_point(data= dat, aes(x= datetime_simple, y= sample_depth_mBSurf , colour = TP), size = 2, position=position_jitter(h=0.15,w=0.15)) +
  scale_y_continuous(trans = "reverse") +
  geom_line(data= level, aes(x=datetime_simple, y= ResLevel_m_btwl, linetype = "Reservoir level"), 
            colour = "#07597E", linewidth = 0.5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.box.just = "center",
    legend.text = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 9, angle = 45, vjust = 0.5),
    plot.title = element_text(size = 9, vjust = 1),
    axis.title.x = element_blank(),
    axis.line.y.right = element_line(colour="#07597E", linewidth = 0.6),
    axis.line.y.left = element_line(colour="black", linewidth = 0.6),
    axis.ticks.y.right = element_line("#07597E"),
    legend.title = element_text(size = 9),
    panel.grid.major.x = element_line(colour="grey", size = 0.5),
    panel.grid.major.y = element_blank(),
    legend.direction = "horizontal",
    legend.text.align = 0.5,  # Center-align the legend text
    legend.key.height = unit(0.5, "cm"),  # Adjust height for alignment
    legend.box.margin = margin(-10, -10, -10, -10)
  ) +
  ylab(legnd[2]) +
  scale_colour_gradientn(
    colors = color_gradient,
    #limits = c(common_min, common_max),
    name = expression(paste("TP ", "(", "ug ", L^-1, ")"))
  ) +
  guides(linetype = guide_legend(title = NULL))
print(p)


tiff(paste0("Plots/Nutrients/Update_August_24/PLOT_UoEspot_ROA_ts_ResLevel_TP.tiff"),
     width = 240, height = 80, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()


# TN ---------------------------------------------------------------------------

legnd <- c("XX", expression(paste("Depth (m below top water level)")), "Reservoir level (m below TWL)")

dat <- df[!is.na(df$reservoir_depth_m),]
dat <- dat[dat$catchment =="Roadford",]
dat <- dat[!is.na(dat$TN),]
level <- subset(ResLevel[["Roadford"]])
dat <- left_join(dat, level, by="datetime_simple") %>%
  mutate(sample_depth_mBSurf = ResLevel_m_btwl+reservoir_depth_m)


# Define the color gradient
color_gradient <- rev(grad_UsT1(5))  # Adjust the number of colors as needed

p <- ggplot() +
  geom_point(data= dat, aes(x= datetime_simple, y= sample_depth_mBSurf , colour = TN), size = 2, position=position_jitter(h=0.15,w=0.15)) +
  scale_y_continuous(trans = "reverse") +
  geom_line(data= level, aes(x=datetime_simple, y= ResLevel_m_btwl, linetype = "Reservoir level"), 
            colour = "#07597E", linewidth = 0.5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y") +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.box.just = "center",
    legend.text = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 9, angle = 45, vjust = 0.5),
    plot.title = element_text(size = 9, vjust = 1),
    axis.title.x = element_blank(),
    axis.line.y.right = element_line(colour="#07597E", linewidth = 0.6),
    axis.line.y.left = element_line(colour="black", linewidth = 0.6),
    axis.ticks.y.right = element_line("#07597E"),
    legend.title = element_text(size = 9),
    panel.grid.major.x = element_line(colour="grey", size = 0.5),
    panel.grid.major.y = element_blank(),
    legend.direction = "horizontal",
    legend.text.align = 0.5,  # Center-align the legend text
    legend.key.height = unit(0.5, "cm"),  # Adjust height for alignment
    legend.box.margin = margin(-10, -10, -10, -10)
  ) +
  ylab(legnd[2]) +
  scale_colour_gradientn(
    colors = color_gradient,
    #limits = c(common_min, common_max),
    name = expression(paste("TN", "(", "mg ", L^-1, ")"))
  ) +
  guides(linetype = guide_legend(title = NULL))
print(p)

tiff(paste0("Plots/Nutrients/Update_August_24/PLOT_UoEspot_ROA_ts_ResLevel_TN.tiff"),
     width = 240, height = 80, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()
