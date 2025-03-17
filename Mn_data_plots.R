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
options("scipen"=100, "digits"=6)

# load files -------------------------------------------------------------------

routine <- readRDS("../../3_Clean_data/Manganese/All_compiled/ROUTINE_Meldon_Manganese.rds") # this is the routine sampling with pump sampler
Spatial <- readRDS("../../3_Clean_data/Manganese/All_compiled/SPATIAL_Manganese.rds")
# ALL <- readRDS("../3_Clean_data/Manganese/All_compiled/ALL_Manganese.rds")

# event separation (only data for MVC, NOT MFE or MRS)
events <- readRDS("../../3_Clean_data/UoE_RRevents/eventEx_OUTPUTS_timeseries_20221207_1017_value__padding2880_alpha0.95_passes5_BFI0.657.rds")
events <- events %>% rename("datetime_UTC" = datetime)                          # rename to match both 

# reservoir level
reslevel <- read.csv("../Reservoir_depth/Data_Input/selected_res_levels_m_below_top_water.csv")

# Tidy -------------------------------------------------------------------------

# tidy the time
routine <- routine %>%
  mutate(datetime_UTC=round_date(routine$datetime_UTC, unit = "15 mins",
                                 week_start = getOption("lubridate.week.start", 7)))
Spatial <- Spatial %>%
  mutate(datetime_UTC=round_date(Spatial$datetime_UTC, unit = "15 mins",
                                 week_start = getOption("lubridate.week.start", 7)))

# Dates for the reservoir levels files as posixt
reslevel$Date <- as.POSIXct(reslevel$Date, format = "%d/%m/%Y", tz = "UTC") 
reslevel <- subset(reslevel, select= c(Date, Meldon)) 

# merge data with events and select MVC (as this is where the flow is)
MVC <- routine %>% 
  filter(code_site== "MVC")
MVC <- left_join(MVC, events, by= c("datetime_UTC")) 
MVC$event <-as.factor(MVC$event)


# Tidy -------------------------------------------------------------------------

# temp replace <LOD by 0 and delete that col
routine <- routine %>%
  mutate(Mn_257_ppb = if_else(DL_flag =="<LOD", 0, Mn_257_ppb))
routine$Year_Month <- as.yearmon(routine$datetime_UTC, "%Y %m")
routine$Year_Month <- factor(routine$Year_Month)

# remove LOD col
routine <- subset(routine, select = -c(DL_flag))

# change the table format 
temp <- routine %>% pivot_wider(names_from = Filtered, values_from = Mn_257_ppb)
colnames(temp)

# order the sites to make sure that the reservoir always comes last in each catchment
routine$code_site <- factor(routine$code_site, levels = c("MFE", "MVC", "MRS"))
routine <- droplevels(routine)


##### 1. Basic distribution stats ##############################################

# 1.1.on the routine data collection  ------------------------------------------

# most stats
summary <- temp %>%
  dplyr::group_by(catchment, code_site) %>%
  dplyr::summarise("N" = sum(!is.na(Total_Mn_ug_l)),
                   "Nb values <LOD - Total Mn" =sum(Total_Mn_ug_l==0, na.rm=TRUE),
                   "Nb exceedances > 50ug/l - Total Mn" = sum(Total_Mn_ug_l>= 50, na.rm=TRUE),
                   "Min concentration - Total Mn (ug/l)" = round(min(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Max concentration - Total Mn (ug/l)" = round(max(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Mean concentration - Total Mn (ug/l)" = round(mean(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Median concentration - Total Mn (ug/l)" = round(median(Total_Mn_ug_l, na.rm = TRUE),1),
                   "SD - Total Mn (ug/l)" = round(sd(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Nb values <LOD - Dissolved Mn" =sum(Diss_Mn_ug_l==0, na.rm=TRUE),
                   "Nb exceedances > 50ug/l - Dissolved Mn" = sum(Diss_Mn_ug_l>= 50, na.rm=TRUE),
                   "Min concentration - Dissolved Mn (ug/l)" = round(min(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Max concentration - Dissolved Mn (ug/l)" = round(max(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Mean concentration - Dissolved Mn (ug/l)" = round(mean(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Median concentration - Dissolved Mn (ug/l)" = round(median(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "SD - Dissolved Mn (ug/l)" = round(sd(Diss_Mn_ug_l, na.rm = TRUE),1))

write.csv(summary,"UsT3_lab_data/csv_output/Manganese/ROUTINE_MELDON_Manganese_summary_exceedances.csv")


# 2. spatial summary -----------------------------------------------------------

# temp replace <LOD by 0 and delete that col
#Spatial <- Spatial %>%
 # mutate(Mn_257_ppb = if_else(DL_flag =="<LOD", 0,Mn_257_ppb))

# remove LOD col
#spat <- subset(Spatial, select = -c(DL_flag))

# change the table format 
spat <- Spatial %>% pivot_wider(names_from = Filtered, values_from = Mn_257_ppb)

# most stats
summary2 <- spat %>%
  dplyr::group_by(catchment) %>%
  dplyr::summarise("N" = sum(!is.na(Total_Mn_ug_l)),
                   "Nb values <LOD - Total Mn" =sum(Total_Mn_ug_l==0, na.rm=TRUE),
                   "Nb values <LOD - Dissolved Mn" =sum(Diss_Mn_ug_l==0, na.rm=TRUE),
                   "N - Dissolved Mn" = sum(!is.na(Diss_Mn_ug_l)))
catch <- c("Meldon")

# most stats
summary22 <- spat %>%
  dplyr::group_by(catchment) %>%
  filter(Total_Mn_ug_l >0 & Diss_Mn_ug_l>0) %>%
  dplyr::summarise("Nb exceedances > 50ug/l - Total Mn" = sum(Total_Mn_ug_l>= 50, na.rm=TRUE),
                   "Min detectable concentration - Total Mn (ug/l)" = round(min(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Max concentration - Total Mn (ug/l)" = round(max(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Mean concentration - Total Mn (ug/l)" = round(mean(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Median concentration - Total Mn (ug/l)" = round(median(Total_Mn_ug_l, na.rm = TRUE),1),
                   "SD - Total Mn (ug/l)" = round(sd(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Nb exceedances > 50ug/l - Dissolved Mn" = sum(Diss_Mn_ug_l>= 50, na.rm=TRUE),
                   "Min detectable concentration - Dissolved Mn (ug/l)" = round(min(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Max concentration - Dissolved Mn (ug/l)" = round(max(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Mean concentration - Dissolved Mn (ug/l)" = round(mean(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Median concentration - Dissolved Mn (ug/l)" = round(median(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "SD - Dissolved Mn (ug/l)" = round(sd(Diss_Mn_ug_l, na.rm = TRUE),1))

summary222 <- merge(summary2, summary22)

write.csv(summary222,"UsT3_lab_data/csv_output/Manganese/SPATIAL_Generic_MELDON_COLL_Mn_summary_exceedances.csv")

# 3. per subcatchment at Colliford ---------------------------------------------
catch <- c("Colliford")

summary3 <- spat %>%
  filter(catchment %in% catch)%>%
  dplyr::group_by(waterbody_name) %>%
  dplyr::summarise("N - Total Mn" = sum(!is.na(Total_Mn_ug_l)),
                   "Nb values <LOD - Total Mn" =sum(Total_Mn_ug_l==0, na.rm=TRUE),
                   "Nb values <LOD - Dissolved Mn" =sum(Diss_Mn_ug_l==0, na.rm=TRUE),
                   "N - Dissolved Mn" = sum(!is.na(Diss_Mn_ug_l)))

summary33 <- spat %>%
  filter(catchment %in% catch)%>%
  dplyr::group_by(waterbody_name) %>%
  filter(Total_Mn_ug_l >0 & Diss_Mn_ug_l>0) %>%
  dplyr::summarise("Nb exceedances > 50ug/l - Total Mn" = sum(Total_Mn_ug_l>= 50, na.rm=TRUE),
                   "Min concentration - Total Mn (ug/l)" = round(min(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Max concentration - Total Mn (ug/l)" = round(max(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Mean concentration - Total Mn (ug/l)" = round(mean(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Median concentration - Total Mn (ug/l)" = round(median(Total_Mn_ug_l, na.rm = TRUE),1),
                   "SD - Total Mn (ug/l)" = round(sd(Total_Mn_ug_l, na.rm = TRUE),1),
                   "Nb values <LOD - Dissolved Mn" =sum(Diss_Mn_ug_l==0, na.rm=TRUE),
                   "Nb exceedances > 50ug/l - Dissolved Mn" = sum(Diss_Mn_ug_l>= 50, na.rm=TRUE),
                   "Min concentration - Dissolved Mn (ug/l)" = round(min(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Max concentration - Dissolved Mn (ug/l)" = round(max(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Mean concentration - Dissolved Mn (ug/l)" = round(mean(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "Median concentration - Dissolved Mn (ug/l)" = round(median(Diss_Mn_ug_l, na.rm = TRUE),1),
                   "SD - Dissolved Mn (ug/l)" = round(sd(Diss_Mn_ug_l, na.rm = TRUE),1))

summary333 <- merge(summary3, summary33)

write.csv(summary333,"UsT3_lab_data/csv_output/Manganese/SPATIAL_COLL_Mn_summary_exceedances.csv")

rm(summary2, summary22, summary3, summary33)







# 2. PLOTTING ##################################################################

# 2.1. ROUTINE - this is the bit that needs work on!!!! ########################


# Comparison of Tot and Diss Mn for each site (boxplots) -----------------------

# let's make a boxplot for filtered vs Unfiltered
Ylab <- expression(paste(Manganese~(ug/l))) 
legnd <- c("Manganese", "Site", Ylab)
max <- max(routine$Mn_257_ppb)

# plot of the difference between total and unfiltered - OUTLIERS REMOVED

P <-  ggplot (routine, aes(Filtered, Mn_257_ppb))+   # this enables to use strings... don't ask - it works
  geom_boxplot(outlier.shape = NA, fill= "#007A8F") + 
  theme_coding() + 
  xlab(legnd[2]) +
  ylab (Ylab) +
  theme(legend.position = "none") +
  labs(title = legnd [1]) +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(size = 11, vjust = 1, hjust = 0))+
  scale_x_discrete(labels=c("Dissolved Mn", "Total Mn")) +
  scale_y_continuous(expand = c(0,0),
                     limits =c(0,100),
                     breaks = seq(0,100, by = 10))
#theme(axis.title.x = element_blank()
#theme(axis.title.x = element_blank()

tiff(paste0("Plots/Manganese/BOXPLOT_MELDON_Filtered_Unfilt.tiff"),
     width = 200, height = 200, units = 'mm', res = 300, compression = "zip")
print(P)
dev.off()

# comparison between sites  - works --------------------------------------------

Ylab <- expression(paste(Manganese~(ug/l))) 
legnd <- c("Manganese", "Site", Ylab)

# with 'some outliers'
P1 <- ggplot(routine, aes(code_site, Mn_257_ppb, fill= Filtered)) +   # this enables to use strings... don't ask - it works
  geom_boxplot(#outlier.shape = NA
    ) + 
  geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.75)+
  theme_coding() + 
  ylab (Ylab) +
  labs(title = legnd [1]) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14, vjust = 1, hjust = 0))+
  #scale_x_discrete(labels=c("Dissolved Mn", "Total Mn")) +
  scale_fill_manual(values=coloursUST, labels = c("Dissolved Mn", "Total Mn"))+
  scale_y_continuous(expand = c(0,0),
                     limits =c(0,300),
                     breaks = seq(0,300, by = 20))

tiff(paste0("Plots/Manganese/BOXPLOT_MELDON_Filtered_Unfilt_per_site_some_outliers.tiff"),
     width = 200, height = 200, units = 'mm', res = 300, compression = "zip")
print(P1)
dev.off()


# with some outliers: ---------

P2 <- ggplot(routine, aes(code_site, Mn_257_ppb, fill= Filtered)) +   # this enables to use strings... don't ask - it works
  geom_boxplot(outlier.shape = NA) + 
  geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.75)+
  theme_coding() + 
  ylab (Ylab) +
  labs(title = legnd [1]) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14, vjust = 1, hjust = 0))+
  #scale_x_discrete(labels=c("Dissolved Mn", "Total Mn")) +
  scale_fill_manual(values=coloursUST, labels = c("Dissolved Mn", "Total Mn"))+
  scale_y_continuous(expand = c(0,0),
                     limits =c(0,100),
                     breaks = seq(0,100, by = 10))

tiff(paste0("Plots/Manganese/BOXPLOT_MELDON_Filtered_Unfilt_per_site_NoOutliers.tiff"),
     width = 200, height = 200, units = 'mm', res = 300, compression = "zip")
print(P2)
dev.off() 


################################################################################
# concentration per reservoir level - all levels -------------------------------
# this works - keep
res <- routine %>%
  filter(!is.na(reservoir_depth_m))
res <- droplevels(res)

for (i in param){
  print(i)
  p <- ggplot() +
    geom_point(data= subset(res, param == i), 
               aes(x= datetime_simple, y= Mn_257_ppb, colour = factor(reservoir_depth_m)), 
               size = 4, position=position_jitter(h=0.15,w=0.15))+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.75)+
    geom_point(size = 10)+
    labs(title = i) +
    theme_coding()+
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          plot.title = element_text(size = 14, vjust = 1, hjust = 0.5),
          axis.title.x = element_blank(),
          legend.title = element_text(size = 14))+
    ylab ("Mn (ug/L)")+
    scale_color_manual("Reservoir depth (m)", values=grad_UsT(7))
  
  plot_list[[i]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Manganese/Boxplot_MRS_per_depthTime_all_val", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip")
  print(plot_list[[i]])
  dev.off()
}



# now removing the values over 400 ---------------------------------------------

for (i in param){
  print(i)
  p <- ggplot() +
    geom_point(data= subset(res, param == i), 
               aes(x= datetime_simple, y= Mn_257_ppb, colour = factor(reservoir_depth_m)), 
               size = 4, position=position_jitter(h=0.15,w=0.15))+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.75)+
    geom_point(size = 10)+
    labs(title = i) +
    theme_coding()+
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
    scale_y_continuous(expand = c(0,0),
                       limits =c(0,300),
                       breaks = seq(0,300, by = 20))+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          plot.title = element_text(size = 14, vjust = 1, hjust = 0.5),
          legend.title = element_text(size = 14))+
    ylab ("Mn (ug/L)")+
    scale_color_manual("Reservoir depth (m)", values=grad_UsT(7))
  
  plot_list[[i]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Manganese/Boxplot_MRS_per_depthTime_NoOutliers_", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip")
  print(plot_list[[i]])
  dev.off()
}



# Manganese at different reservoir depth (Mn) with reservoir level as 2nd axis---
# NO OUTLIERS ------------------------------------------------------------------

param <- c("Diss_Mn_ug_l", "Total_Mn_ug_l")
ratio <- 300/(max(reslevel$Meldon, na.rm=TRUE))                                 # 300 is the max value to have on the axis (excl outliers)

legnd <- c("XX", expression(paste("Mn (ug ", ~L^-1,")")), "Reservoir level (m below TWL)")
plot_list <- list() 

# before plotting reservoir level, change from posixct to as.date  -------------

reslevel <- reslevel
reslevel[] <- lapply(reslevel, function(x) {
  if (inherits(x, "POSIXt")) as.Date(x) else x
})

res []<- lapply(res, function(x) {
  if (inherits(x, "POSIXt")) as.Date(x) else x
})

# loop 
for (i in param){
  print(i)
  p <- ggplot() +
    geom_point(data= subset(res, param == i), 
               aes(x= datetime_UTC, y= Mn_257_ppb, colour = factor(reservoir_depth_m)), 
               size = 4, position=position_jitter(h=0.15,w=0.15))+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.75)+
    geom_line(data= reslevel, aes(x=Date, y= (14.99-Meldon) * 20, linetype="Reservoir level"), colour = "#07597E", linewidth = 0.75) +  # used 20 based on the ratio between the max conc to be plotted (300 to excl outliers) and max level
    geom_point(size = 10)+
    labs(title = i) +
    theme_coding()+
    ggtitle(as.character(i))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                 limits = as.Date(c("2021-08-01","2022-10-31"))) +
    scale_y_continuous(limits =c(0,300),
                       #
                       breaks = seq(0,300, by = 20),
                       sec.axis = sec_axis(~rev(.)/20,
                                           #limits= c(-1,20),
                                           #breaks = seq(-1,50),
                                           name = legnd[3]))+
    #, breaks = seq(0,8)
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          plot.title = element_text(size = 14, vjust = 1),
          axis.title.x = element_blank(),
          axis.line.y.right = element_line(colour="#07597E", linewidth = 0.75),
          axis.ticks.y.right =element_line( "#07597E"),
          legend.title = element_text(size = 14))+
    ylab ("Mn (ug/L)")+
    scale_color_manual("Sampling depth\n(m below surface)", values=grad_UsT(7))
  
  
  plot_list[[i]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Manganese/MRS_per_depthTime_all_resLevel_", i, ".tiff", sep="")
  tiff(file_name, width = 400, height = 200,  units = "mm", res = 300, compression = "zip")
  print(plot_list[[i]])
  dev.off()
}



# WITH OUTLIERS ------------------------------------------------------------------

param <- c("Diss_Mn_ug_l", "Total_Mn_ug_l")
ratio <- (max(res$Mn_257_ppb, na.rm=TRUE))/(max(reslevel$Meldon, na.rm=TRUE))

legnd <- c("XX", expression(paste("Mn (ug ", ~L^-1,")")), "Reservoir level (m below TWL)")
plot_list <- list() 

# before plotting reservoir level, change from posixct to as.date  -------------


# loop 
for (i in param){
  print(i)
  p <- ggplot() +
    geom_point(data= subset(res, param == i), 
               aes(x= datetime_UTC, y= Mn_257_ppb, colour = factor(reservoir_depth_m)), 
               size = 4, position=position_jitter(h=0.15,w=0.15))+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.75)+
    geom_line(data= reslevel, aes(x=Date, y= (14.99-Meldon) * ratio), colour = "grey49") +   # 14.99 is the total depth (to invert the data)
    geom_point(size = 10)+
    labs(title = i) +
    theme_coding()+
    ggtitle(as.character(i))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                 limits = as.Date(c("2021-08-01","2022-10-31"))) +
    scale_y_continuous(#limits =c(0,300),
                       #breaks = seq(0,300, by = 20),
                       sec.axis = sec_axis(~rev(.)/ratio,
                                           #limits= c(-1,20),
                                           #breaks = seq(-1,50),
                                           name = legnd[3]))+
    #, breaks = seq(0,8)
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          plot.title = element_text(size = 14, vjust = 1),
          axis.title.x = element_blank(),
          legend.title = element_text(size = 14))+
    ylab ("Mn (ug/L)")+
    scale_color_manual("Sampling depth\n(m below surface)", values=grad_UsT(7))
  
  
  plot_list[[i]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Manganese/MRS_per_depthTime_all_outliers_resLevel_", i, ".tiff", sep="")
  tiff(file_name, width = 400, height = 200,  units = "mm", res = 300, compression = "zip")
  print(plot_list[[i]])
  dev.off()
}







################################################################################


param <- c("Diss_Mn_ug_l", "Total_Mn_ug_l")
Ylab <- expression(paste(Dissolved~Mn~(ug/l))) 
plot_list <- list() 


for (i in param){
  print(i)
  p <- ggplot (data=subset(routine, param == i),aes(x = Year_Month, y = Mn_257_ppb, fill=code_site)) +                         # values indicate to use the value in the cell
    geom_boxplot(outlier.shape = NA, outlier.size = 0) + 
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          plot.title = element_text(size = 14, vjust = 1, hjust = 0.5),
          legend.title = element_blank())+
    ylab (Ylab)+
    #scale_y_continuous(expand = c(0,0)#,
                       #breaks = seq(0,max(temp2$value))
    #)+
    scale_y_continuous(expand = c(0,0), limits =c(0,100), breaks = seq(0,100, by=20))+
    # scale_x_datetime(name = "Sampling time", date_breaks = "1 month")+#, 
    #date_labels = "%m/%Y")# + 
    scale_fill_manual(values=coloursUST)
  plot_list[[i]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Manganese/Boxplot_MELDON_routine_perMOnth_site_ ", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}



# Plotting per flow ------------------------------------------------------------

xlim_min <- as.POSIXct("2021-10-01 00:00:00", tz = "UTC")


# Dissolved mn
legnd <- c("XX", expression(paste("Flow"~"(m"^{3}, ~s^-1,")")), expression(paste("Mn (ug ", ~L^-1,")")))
ratio <- (max(events$q_m3_s, na.rm=TRUE))/(max(MVC_diss$Mn_257_ppb, na.rm=TRUE))

p <- ggplot() +
     geom_line(data= events,aes(x=datetime_UTC, y=q_m3_s,colour = "Flow"             )) +                                     # Needs a way to fix and add name
     geom_point(data= MVC_diss, aes(x=datetime_UTC, y= Mn_257_ppb * ratio, 
                                   colour = event), size =2 ) +                            # Needs a way to fix and add name
     ggtitle("Disssolved manganese")+
     #geom_line(aes(x, y3, colour = legnd[4])) +
     xlab (legnd[1]) +
     ylab (legnd[2]) +
     scale_y_continuous (sec.axis = sec_axis(~./ratio,
                                            name = legnd[3]
                                            #, breaks = seq(0,80, by=10
                                            )) +
    theme_coding()+
    guides(colour = guide_legend(override.aes = list(size=1)))+
    theme_bw(base_size = 8) + 
    theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
        plot.title = element_text(size = 14, vjust = 1, hjust = 0.5),
        legend.title = element_blank())+
  scale_x_datetime(limits = c(as.POSIXct("2021-10-01 00:00:00", tz = "UTC"),
                              as.POSIXct("2022-10-01 00:00:00", tz = "UTC")),
                   date_labels = ("%b %Y"),
                   date_breaks = "1 months",
                   minor_breaks = "1 month")+
    scale_colour_manual(values= c("#007A8F","#FFBF00", "grey49"),
                        labels=c('Non event', 'Event', 'Flow'))


tiff(paste0("Plots/Manganese/Timeseries_MVC_DissolvedMn_Flow.tiff"),
     width = 400, height = 200, units = 'mm', res = 300, compression = "zip")
print(p)
dev.off()


# Total Mn ---------------------------------------------------------------------

legnd <- c("XX", expression(paste("Flow"~"(m"^{3}, ~s^-1,")")), expression(paste("Mn (ug ", ~L^-1,")")))
ratio <- (max(events$q_m3_s, na.rm=TRUE))/(max(MVC_tot$Mn_257_ppb, na.rm=TRUE))

p <- ggplot() +
  geom_line(data= events,aes(x=datetime_UTC, y=q_m3_s,colour = "Flow"             )) +                                     # Needs a way to fix and add name
  geom_point(data= MVC_tot, aes(x=datetime_UTC, y= Mn_257_ppb * ratio, 
                                 colour = event), size =2 ) +                            # Needs a way to fix and add name
  ggtitle("Total manganese")+
  #geom_line(aes(x, y3, colour = legnd[4])) +
  xlab (legnd[1]) +
  ylab (legnd[2]) +
  scale_y_continuous (sec.axis = sec_axis(~./ratio,
                                          name = legnd[3]
                                          #, breaks = seq(0,80, by=10
  )) +
  theme_coding()+
  guides(colour = guide_legend(override.aes = list(size=1)))+
  theme_bw(base_size = 8) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14, vjust = 1, hjust = 0.5),
        legend.title = element_blank())+
  scale_x_datetime(limits = c(as.POSIXct("2021-10-01 00:00:00", tz = "UTC"),
                              as.POSIXct("2022-10-01 00:00:00", tz = "UTC")),
                   date_labels = ("%b %Y"),
                   date_breaks = "1 months",
                   minor_breaks = "1 month")+
  scale_colour_manual(values= c("#007A8F","#FFBF00", "grey49"),
                      labels=c('Non event', 'Event', 'Flow'))

tiff(paste0("Plots/Manganese/Timeseries_MVC_TotMn_Flow.tiff"),
     width = 400, height = 200, units = 'mm', res = 300, compression = "zip")
print(p)
dev.off()



##################################

Ylab <- expression(paste(Manganese~(ug/l))) 
legnd <- c("Manganese", "Site", Ylab)

# with 'some outliers'
P1 <- ggplot(MVC, aes(Filtered, Mn_257_ppb, fill= event)) +   # this enables to use strings... don't ask - it works
  geom_boxplot(#outlier.shape = NA
  ) + 
  geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.75)+
  theme_coding() + 
  ylab (Ylab) +
  labs(title = "Meldon - Vellake corner") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14, vjust = 1, hjust = 0))+
  scale_x_discrete(labels=c("Dissolved Mn", "Total Mn")) +
  scale_fill_manual(values=coloursUST, labels = c("Non event", "Event"))+
  scale_y_continuous(expand = c(0,0),
                     limits =c(0,80),
                     breaks = seq(0,80, by = 10))

tiff(paste0("Plots/Manganese/BOXPLOT_MVC_Filtered_Unfilt_Events.tiff"),
     width = 200, height = 200, units = 'mm', res = 300, compression = "zip")
print(P1)
dev.off()


# contribution of Mn from MVC during different flow conditions per season

for (i in param){
  print(i)
  p <- ggplot (data=subset(MVC, param == i),aes(x = Year_Month, y = Mn_257_ppb, fill=code_site)) +                         # values indicate to use the value in the cell
    geom_boxplot(outlier.shape = NA, outlier.size = 0) + 
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          plot.title = element_text(size = 14, vjust = 1, hjust = 0.5),
          legend.title = element_blank())+
    ylab (Ylab)+
    #scale_y_continuous(expand = c(0,0)#,
    #breaks = seq(0,max(temp2$value))
    #)+
    scale_y_continuous(expand = c(0,0), limits =c(0,100), breaks = seq(0,100, by=20))+
    # scale_x_datetime(name = "Sampling time", date_breaks = "1 month")+#, 
    #date_labels = "%m/%Y")# + 
    scale_fill_manual(values=coloursUST)
  plot_list[[i]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in param){
  file_name = paste("Plots/Manganese/Boxplot_MELDON_routine_perMOnth_site_ ", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}















# Statistical testing ##########################################################

# is the difference between Mn during events and non events statistically significant? Mann Whitney

# Mann whitney (other version of Wilcoxon but for not paired data)
MannW <- MVC %>%
  group_by(Filtered) %>%
  do(w = wilcox.test(Mn_257_ppb ~ event, data=., paired=FALSE)) %>% 
  summarise(Filtered, MannW = w$p.value)
    
csvFileName <- paste("csv_output/Manganese/MannWhitney_test_Mn_flow_cond",Sys.Date(),".csv",sep="")
write.csv(MannW, file = csvFileName)







# Differences between sites - Friedman's test ##################################

# let's average the data per month - only gives on value per 
rout_av <- routine %>%
  dplyr::group_by(catchment, code_site, Year_Month, Filtered) %>%
  dplyr::summarize(mean_Mn = mean(Mn_257_ppb, na.rm=TRUE))
rout_av <- as.data.frame(rout_av)

# check data gaps
test <- rout_av %>%
  group_by(catchment, Year_Month, Filtered)%>%
  count(n= n())


rout_av <- as.data.frame(rout_av)
rout_av <- droplevels(rout_av)
rout_av$Filtered <- as.factor(rout_av$Filtered)

for (i in param) {
  res.fried_Mn <- rout_av %>%
    dplyr::group_by(Filtered) %>%
    rstatix::friedman_test(mean_Mn ~ code_site| Year_Month)
}

csvFileName <- paste("csv_output/Manganese/Mn_Friedmans_test_Site_diff",Sys.Date(),".csv",sep="")
write.csv(res.fried_Mn, file = csvFileName)


# post hoc test - can't get it to work for all sites, so doing it individually

source("FUNCTIONS_statistical_analysis.R")         

# Dissolved  -------------------------------------------------------------------

DissMn <- rout_av %>%
  filter(Filtered == "Diss_Mn_ug_l")

DissMn <- as.data.frame(DissMn)
DissMn <- DissMn %>%
  convert_as_factor(catchment,code_site, Year_Month, Filtered)                                           # make as factors
DissMn <- droplevels(DissMn)     

formu <- mean_Mn ~ code_site|Year_Month
post_hoc_Filtered<- friedman.test.with.post.hoc(formu, DissMn)
post_hoc_Filtered2 <- as.data.frame(post_hoc_Filtered[[2]])
post_hoc_Filtered2$Qualif <- c("Diss Mn")

# Total ------------------------------------------------------------------------
TotMn <- rout_av %>%
  filter(Filtered == "Total_Mn_ug_l")

TotMn <- as.data.frame(TotMn)
TotMn <- TotMn %>%
  convert_as_factor(catchment,code_site, Year_Month, Filtered)                                           # make as factors
TotMn <- droplevels(TotMn)     

formu <- mean_Mn ~ code_site|Year_Month
post_hoc_Total<- friedman.test.with.post.hoc(formu, TotMn)
post_hoc_Total2 <- as.data.frame(post_hoc_Total[[2]])
post_hoc_Total2$Qualif <- c("Total Mn")

post_hoc_tests_results <- bind_rows(post_hoc_Filtered2, post_hoc_Total2)                               # make one results table and save
write.csv(post_hoc_tests_results, file="csv_output/Manganese/post_hoc_tests_results_per_site.csv")




################################################################################
# SPATIAL ####### WORKS AND DONE        ########################################


# generic boxplot


catch_spat <- c("Colliford", "Meldon")
Ylab <- expression(paste(Tot~(mg/l)))
legnd <- c("NPOC", "Sampling site", Ylab)
#max <- max(rout$NPOC, na.rm=TRUE)
plot_list <- list()

for (i in catch_spat){
  print(i)

  p_other <- ggplot(data= subset(Spatial, catchment == i), aes(x= month, y= Mn_257_ppb, fill =Filtered)) +
    geom_boxplot()+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 14, vjust = 1, hjust = 0),
          axis.text.x = element_text(size = 14, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_blank(),
          legend.text = element_text(size=14))+
    #scale_y_continuous(expand = c(0,0), limits =c(0,max+1),
    #breaks = seq(0,max+1, by=1))+   
    xlab(legnd[2]) +
    ylab ("Mn (ug/L)")+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
    scale_fill_manual(labels=c("Dissolved Mn", "Total Mn"),
                      values=coloursUST)
 
  
  plot_list[[i]] <- p_other
  
  file_name = paste("Plots/Manganese/Spatial_campaigns/TotMn_Boxplots_SPATIAL_GENERAL_", i, ".tiff", sep="") # saves the 2nd series of plots
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}


# Slightly different one 

catch_spat <- c("Colliford", "Meldon")

plot_list <- list()

for (i in catch_spat){
  print(i)
  
  p_other <- ggplot(data= subset(Spatial, catchment == i), aes(x= Filtered, y= Mn_257_ppb , fill =month)) +
    geom_boxplot()+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", linewidth = 0.75)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 14, vjust = 1, hjust = 0),
          axis.text.x = element_text(size = 14, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_blank(),
          legend.text = element_text(size=14))+
    #scale_y_continuous(expand = c(0,0), limits =c(0,max+1),
    #breaks = seq(0,max+1, by=1))+   
    xlab(legnd[2]) +
    ylab ("Mn (ug/L)")+
    scale_x_discrete(labels= c("Dissolved Mn", "Total Mn")
                     )+
    scale_fill_manual(labels=c("Summer", "Autumn"),
                      values=coloursUST)
  
  
  plot_list[[i]] <- p_other
  
  file_name = paste("Plots/Manganese/Spatial_campaigns/TotMn_Boxplots_SPATIAL_GENERAL_other_", i, ".tiff", sep="") # saves the 2nd series of plots
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}




# Dissolved Mn difference between seasons per site ----------------------------------

id_vars <- c("month", "code_site", "catchment", "waterbody_name")
meas_var <- c("Diss_Mn_ug_l")
Ylab <- expression(paste(Dissolved~Mn~(ug/l))) 
temp <- melt(spat, id.vars=id_vars, measure.vars=meas_var)
max <- max(temp$value)
plot_list <- list() 
catch_spat <- c("Colliford", "Meldon")

for (i in catch_spat){
  print(i)
  p <- ggplot (data=subset(temp, catchment == i),aes(x = waterbody_name, y = value)) +                         # values indicate to use the value in the cell
    geom_bar(aes(fill = month), stat = "identity",position = "dodge", width=0.65)+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", size = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          plot.title = element_text(size = 14, vjust = 1, hjust = 0),
          legend.title = element_blank())+
    ylab (Ylab)+
    scale_y_continuous(expand = c(0,0)#, 
                       #breaks = seq(0,max, by= 20)
                       )+
    #scale_y_continuous(limits =c(0,max+1), breaks = seq(0,max+1, by=1))+
    # scale_x_datetime(name = "Sampling time", date_breaks = "1 month", 
    #date_labels = "%m/%Y")# + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
    scale_fill_manual(labels=c("Summer", "Autumn"),
                      values=coloursUST)
  plot_list[[i]] <- p
}


# Save plots to tiff. Makes a separate file for each plot.
for (i in catch_spat){
  file_name = paste("Plots/Manganese/Spatial_campaigns/BarCharts_DissMn_SPATIAL_", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}


# Total Mn difference between seasons per site---------------------------------------

id_vars <- c("month", "code_site", "catchment", "waterbody_name")
meas_var <- c("Total_Mn_ug_l")
plot_list <- list() 
Ylab <- expression(paste(Total~Mn~(ug/l))) 
temp <- melt(spat, id.vars=id_vars, measure.vars=meas_var)
max <- max(temp$value, na.rm=TRUE)
plot_list <- list() 

for (i in catch_spat){
  print(i)
  
  p <- ggplot (data=subset(temp, catchment == i),aes(x = waterbody_name, y = value)) +                         # values indicate to use the value in the cell
    geom_bar(aes(fill = month),stat = "identity",position = "dodge", width=0.65)+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", size = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
          plot.title = element_text(size = 14, vjust = 1, hjust = 0),
          legend.title = element_blank())+
    ylab (Ylab)+
    scale_y_continuous(expand = c(0,0)#,
                       #limits = c(0,max),
                       #breaks = seq(0,max, by = 20)
                       )+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
    scale_fill_manual(labels=c("Summer", "Autumn"),
                      values=coloursUST)
  
  plot_list[[i]] <- p
}


# Save plots to tiff. Makes a separate file for each plot.
for (i in catch_spat){
  file_name = paste("Plots/Manganese/Spatial_campaigns/BarCharts_TotMn_SPATIAL_", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}


################################################################################
# Where are the hotspots? ######################################################



# can we narrow it down per site? ##############################################

for (i in catch_spat){
  print(i)
  p <- ggplot (data=subset(temp, catchment == i),aes(x = code_site, y = value)) +                         # values indicate to use the value in the cell
    geom_bar(aes(fill = month), stat = "identity",position = "dodge", width=0.65)+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", size = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(vjust = 1, hjust = 0),
          axis.title.x = element_blank())+
    ylab (Ylab)+
    scale_y_continuous(expand = c(0,0), 
                       breaks = seq(0,max, by= 20))+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
    scale_fill_manual(labels=c("Summer", "Autumn"),
                      values=coloursUST)
  plot_list[[i]] <- p
}


# Save plots to tiff. Makes a separate file for each plot.
for (i in catch_spat){
  file_name = paste("Plots/Manganese/Spatial_campaigns/BarCharts_DissMn_SPATIAL_PerSite_", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}



# Total Mn difference between seasons per site---------------------------------------

id_vars <- c("month", "code_site", "catchment", "waterbody_name")
meas_var <- c("Total_Mn_ug_l")
plot_list <- list() 
Ylab <- expression(paste(Total~Mn~(ug/l))) 
temp <- melt(spat, id.vars=id_vars, measure.vars=meas_var)
max <- max(temp$value)
plot_list <- list() 

for (i in catch_spat){
  print(i)
  
  p <- ggplot (data=subset(temp, catchment == i),aes(x = waterbody_name, y = value)) +                         # values indicate to use the value in the cell
    geom_bar(aes(fill = month),stat = "identity",position = "dodge", width=0.65)+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", size = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(vjust = 1, hjust = 0),
          axis.title.x = element_blank())+
    ylab (Ylab)+
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(0,max, by = 20))+
    
    # scale_x_datetime(name = "Sampling time", date_breaks = "1 month", 
    scale_fill_manual(labels=c("Summer", "Autumn"),
                      values=coloursUST)
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18))
  
  plot_list[[i]] <- p
}



# Save plots to tiff. Makes a separate file for each plot.
for (i in catch_spat){
  file_name = paste("UsT3_lab_data/Plots/Manganese/Spatial_campaigns/BarCharts_TotMn_SPATIAL_", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}


# can we narrow it down per site? ##############################################

for (i in catch_spat){
  print(i)
  p <- ggplot (data=subset(temp, catchment == i),aes(x = code_site, y = value)) +                         # values indicate to use the value in the cell
    geom_bar(aes(fill = month), stat = "identity",position = "dodge", width=0.65)+
    geom_hline(aes(yintercept = 50), linetype = "dashed", colour = "darkred", size = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(vjust = 1, hjust = 0),
          axis.title.x = element_blank())+
    ylab (Ylab)+
    scale_y_continuous(expand = c(0,0), 
                       breaks = seq(0,max, by= 20))+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
    scale_fill_manual(labels=c("Summer", "Autumn"),
                      values=coloursUST)
  plot_list[[i]] <- p
}


# Save plots to tiff. Makes a separate file for each plot.
for (i in catch_spat){
  file_name = paste("UsT3_lab_data/Plots/Manganese/Spatial_campaigns/BarCharts_TotMn_SPATIAL_PerSite_", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}


# Statistical testing of difference between campaing 1 and 2! ##################


# Normality test ###############################################################

# are they normally distributed? somehow the table has to have only one grouping var, so repeating it several times for each catchment

#Tot Mn first -----------

test_TotMn <- Spatial %>%
  dplyr::filter(Filtered == "Total_Mn_ug_l") 

# Coll 
Coll <- test_TotMn[test_TotMn$catchment == "Colliford",] 
Coll<- Coll %>% 
  dplyr::select(month, Mn_257_ppb)  # only select grouping var and value

Shapi_TotMn_Coll <- Coll %>%
  group_by(month) %>%
  summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))

Shapi_TotMn_Coll$qualif <- "COLL_TotMn" 

# Meldon 
Mel <- test_TotMn[test_TotMn$catchment == "Meldon",] 
Mel <- Mel %>% 
  dplyr::select(month, Mn_257_ppb)  # only select grouping var and value

Shapi_TotMn_Mel <- Mel %>%
  group_by(month) %>%
  summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))

Shapi_TotMn_Mel$qualif <- "Mel_TotMn" 



# Diss Mn ----------------------------------------------------------------------
test_DissMn <- Spatial %>%
  dplyr::filter(Filtered == "Diss_Mn_ug_l") 

# Coll 
Coll <- test_DissMn[test_DissMn$catchment == "Colliford",] 
Coll<- Coll %>% 
  dplyr::select(month, Mn_257_ppb)  # only select grouping var and value

Shapi_DissMn_Coll <- Coll %>%
  group_by(month) %>%
  summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))

Shapi_DissMn_Coll$qualif <- "COLL_DissMn" 

# Meldon 
Mel <- test_DissMn[test_DissMn$catchment == "Meldon",] 
Mel <- Mel %>% 
  dplyr::select(month, Mn_257_ppb)  # only select grouping var and value

Shapi_DissMn_Mel <- Mel %>%
  group_by(month) %>%
  summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))

Shapi_DissMn_Mel$qualif <- "Mel_DissMn" 


# merge and save
Shapi_test <- bind_rows(Shapi_DissMn_Coll, Shapi_DissMn_Mel, Shapi_TotMn_Coll, Shapi_TotMn_Mel)                               # make one results table and save
write.csv(Shapi_test, file="csv_output/Manganese/Normality_test_Spatial.csv")

### Differences between campaigns ##############################################

test_DissMn <- droplevels(test_DissMn)
test_DissMn$catchment <- as.factor(test_DissMn$catchment)

Other <- test_DissMn %>%
  group_by(catchment, month)%>%
  count(n= n())
Other <- Spatial %>%
  group_by(catchment,code_site)%>%
  count(n= n())

# tidy the tables - uneven number of samples need removing

list <- c("CDR", "CLD", "CWR", "CWW", "MBT", "MFW", "MGK", "MKT", "MVC", "MRS")

test_DissMn <- subset(test_DissMn, !(code_site %in% list))
test_TotMn <- subset(test_TotMn, !(code_site %in% list))

test_Diss <- test_DissMn %>% 
  dplyr::select(catchment, month, Mn_257_ppb)   
test_TotMn <- test_TotMn %>% 
  dplyr::select(catchment, month, Mn_257_ppb)  


Wilcox_Diss <- test_Diss %>%
  group_by(catchment) %>%
  do(w = wilcox.test(Mn_257_ppb ~ month, data=., paired=TRUE)) %>% 
  summarise(catchment, Wilcox = w$p.value)
Wilcox_Diss$qualif <- "DissMn"                                                        # Add a qualif to the result table before merging

Wilcox_Tot <- test_TotMn %>%
  group_by(catchment) %>%
  do(w = wilcox.test(Mn_257_ppb ~ month, data=., paired=TRUE)) %>% 
  summarise(catchment, Wilcox = w$p.value)
Wilcox_Tot$qualif <- "TotMn"

Wilcox <- bind_rows(Wilcox_Diss, Wilcox_Tot)

write.csv(Wilcox, file="csv_output/Manganese/WilcoxTest_Mn_Diff_spatial_campaigns.csv")
rm(Wilcox_Diss, Wilcox_Tot)





############ rough plots #######################################################




# Total and dissolved per site

ylab <-(expression(paste("Manganese (mg ", l^-1,")")))   

P1 <- ggplot (MN, aes(x=Site, y=Mn_257_ppb, fill=Filter))+   # this enables to use strings... don't ask - it works
  geom_boxplot(outlier.size = 0.01, lwd = 0.2) + 
  theme_coding() + 
  ylab (ylab) +
  scale_y_continuous (name = ylab,
                      limits =c(0,140),
                      breaks = seq(0,140, by=20))+
    theme( legend.position = "bottom",
         axis.title.y = element_text(size = 10),
         axis.title.x = element_blank(),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         legend.text = element_text(size = 10),
         plot.title = element_text(size = 10, vjust = 1, hjust = 0),
         axis.ticks = element_line(colour = "black", size = 0.2)) +

  scale_fill_manual(values = c("#00688B", "#B5CB8D"),
                    labels = c("Dissolved Mn","Total Mn"))

tiff(paste0("Data_analysis/UsT3_lab_data/Plots/PLOT_box_UoE_Mn_Meldon_all.tiff"),
     width = 150, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(P1)
dev.off()


# Monthly plot


# separate the sites


MVC <- filter(MN,grepl("MVC", Site)) 
MFE <- filter(MN,grepl("MFE", Site)) 
MRS <- filter(MN,grepl("MRS", Site)) 


# change of Total manganese during an event ####################################

MVC_event <- filter(MVC, month==2)
MVC_event_tot <- filter(MVC_event, Filter == "U")

a <- as.POSIXct("2022-02-13 07:00")
b <- as.POSIXct("2022-02-15 14:00")

lims <- as.POSIXct(strptime(c(a, b), format= "%Y-%m-%d %H:%M", tz = "UTC"))
P2 <- ggplot()+ geom_point(data=MVC_event_tot, aes(x=datetime_UTC, y=Mn_257_ppb, colour = "#00688B"))+
 # geom_point(data=MN, aes(x=datetime_UTC, y=result, colour = 'MIB'))+
  theme_coding()+
  scale_x_datetime(date_breaks = "4 hour",
                   date_labels = "%d-%m-%y %H:%m",
                   limits= lims)+
  scale_y_continuous(limits = c(0,90),
                     breaks = seq(0,80, by =10),
                     expand = c(0,0))+   
  scale_colour_manual (values = pal_UsT) +
  ylab(expression(paste("Total Manganese (mg ", L^-1,")")))+  
  theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 0.75, hjust = 1, colour = "gray28", 
                                   margin = margin(t=-0.5, b=15, l= 2)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r=7)),
        plot.margin=margin(l=0,unit="cm"),
        plot.title = element_text(face = 'bold'), 
        legend.position = "none")

tiff(paste0("Data_analysis/UsT3_lab_data/Plots/Scatter_UoE_totMn_Meldon_Event.tiff"),
     width = 150, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(P2)
dev.off()



# Varation by reservoir level and month
unique(MRS$month)
MRS_10 <- filter(MRS,grepl("10", month)) 
MRS_11 <- filter(MRS,grepl("11", month))
MRS_12 <- filter(MRS,grepl("12", month))

MRS_01 <- filter(MRS,grepl("1", month))                                         # gives months of 1, 11 and 12 so needs separating
MRS_01 <- filter(MRS_01,grepl("(JFM)", season))

MRS_02 <- filter(MRS,grepl("2", month))
MRS_02 <- filter(MRS_02,grepl("(JFM)", season))






p_oct <- ggplot()+ geom_point(data=MRS_10, aes(x=Mn_257_ppb, y=reservoir_depth_m, colour = Filter))+
  geom_point(size = 1)+
 scale_y_reverse(expand = c(0, 0),
                 limits = c(26, 0))+
scale_x_continuous(position = "top",
                   limits = c(20,130),
                   breaks = seq(20,130,by =20))+
  scale_colour_manual (values = pal_UsT) +
  ggtitle("October 2021") +
  xlab(expression(paste("Mn concentration (mg ", L^-1,")")))+  
  ylab(expression(paste("Reservoir depth (m)")))+ 
  theme(axis.text.x = element_text(size = 9),
        axis.title.y = element_text(size = 10, margin = margin(r=5)),
        axis.title.x = element_text(size = 10, margin = margin(r=5)),
        plot.margin=margin(l=0.5,r=0.5, t = 0.5, b=0.5,unit="cm"),
        legend.position = "bottom",
        plot.title =  element_text(size = 10, face = "bold"))

p_nov <- ggplot()+ geom_point(data=MRS_11, aes(x=Mn_257_ppb, y=reservoir_depth_m, colour = Filter))+
  geom_point(size = 1)+
  scale_y_reverse(expand = c(0, 0),
                  limits = c(26, 0))+
  scale_x_continuous(position = "top",
                     limits = c(20,80),
                     breaks = seq(20,80,by =10))+
  scale_colour_manual (values = pal_UsT) +
  ggtitle("November 2021") +
  xlab(expression(paste("Mn concentration (mg ", L^-1,")")))+  
  ylab(expression(paste("Reservoir depth (m)")))+ 
  theme(axis.text.x = element_text(size = 9),
        axis.title.y = element_text(size = 10, margin = margin(r=5)),
        axis.title.x = element_text(size = 10, margin = margin(r=5)),
        plot.margin=margin(l=0,r=0.5,b = 0.5, t=0.5,unit="cm"),
        legend.position = "bottom",
        plot.title =  element_text(size = 10, face = "bold"))

p_dec <- ggplot()+ geom_point(data=MRS_12, aes(x=Mn_257_ppb, y=reservoir_depth_m, colour = Filter))+
  geom_point(size = 1)+
  scale_y_reverse(expand = c(0, 0),
                  limits = c(26, 0))+
  scale_x_continuous(position = "top",
                     limits = c(20,110),
                     breaks = seq(20,110,by =20))+
  scale_colour_manual (values = pal_UsT) +
  ggtitle("December 2021") +
  xlab(expression(paste("Mn concentration (mg ", L^-1,")")))+  
  ylab(expression(paste("Reservoir depth (m)")))+ 
  theme(axis.text.x = element_text(size = 9),
        axis.title.y = element_text(size = 10, margin = margin(r=5)),
        axis.title.x = element_text(size = 10, margin = margin(r=5)),
        plot.margin=margin(l=0,r=0.5,b=0.5, t= 0.5,unit="cm"),
        legend.position = "bottom",
        plot.title =  element_text(size = 10, face = "bold"))

p_jan <- ggplot()+ geom_point(data=MRS_01, aes(x=Mn_257_ppb, y=reservoir_depth_m, colour = Filter))+
  geom_point(size = 1)+
  scale_y_reverse(expand = c(0, 0),
                  limits = c(26, 0))+
  scale_x_continuous(position = "top",
                     limits = c(20,80),
                     breaks = seq(20,80,by =20))+
  scale_colour_manual (values = pal_UsT) +
  ggtitle("January 2022") +
  xlab(expression(paste("Mn concentration (mg ", L^-1,")")))+  
  ylab(expression(paste("Reservoir depth (m)")))+ 
  theme(axis.text.x = element_text(size = 9),
        axis.title.y = element_text(size = 10, margin = margin(r=5)),
        axis.title.x = element_text(size = 10, margin = margin(r=5)),
        plot.margin=margin(l=0.5,r=0.5, b=0.5,unit="cm"),
        legend.position = "bottom",
        plot.title =  element_text(size = 10, face = "bold"))

p_feb <- ggplot()+ geom_point(data=MRS_02, aes(x=Mn_257_ppb, y=reservoir_depth_m, colour = Filter))+
  geom_point(size = 1)+
    scale_y_reverse(expand = c(0, 0),
                  limits = c(26, 0))+
  scale_x_continuous(position = "top",
                     limits = c(20,230),
                     breaks = seq(20,230,by =40))+
  scale_colour_manual (values = pal_UsT, labels = c("Dissolved Mn", "Total Mn")) +
  ggtitle("February 2022") +
  xlab(expression(paste("Mn concentration (mg ", L^-1,")")))+  
  ylab(expression(paste("Reservoir depth (m)")))+ 
  theme(axis.text.x = element_text(size = 9),
        axis.title.y = element_text(size = 10, margin = margin(r=5)),
        axis.title.x = element_text(size = 10, margin = margin(r=5)),
        plot.margin=margin(l=0,r=0.5, b=0.5,unit="cm"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title =  element_text(size = 10, face = "bold"))

legend <- get_legend(p_feb + 
    guides(color = guide_legend(nrow = 2))+
    theme(legend.box.margin = margin(0, 0, 0, 12)))



P <- plot_grid(p_oct + theme(legend.position="none"#,  #axis.title.x = element_blank()
                             ), 
               p_nov + theme(legend.position="none"#,  axis.title.x = element_blank()
                             ), 
               p_dec + theme(legend.position="none"#,  axis.title.x = element_blank()
                             ), 
               p_jan + theme(legend.position="none"#,  axis.title.x = element_blank()
                             ),
               p_feb + theme(legend.position="none"#,  axis.title.x = element_blank()
                             ),
               legend,
               align = "v", nrow = 2, ncol = 3)

tiff(paste0("Data_analysis/UsT3_lab_data/Plots/Lab_UoE_Depth_profiles_Mn_Meldon.tiff"),
     width = 150, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(P)
dev.off()
