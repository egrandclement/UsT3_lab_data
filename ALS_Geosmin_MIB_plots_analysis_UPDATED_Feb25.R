##### TITLE: UsT3_UoE_spot_exploration #########################################

# Type: Script

# Date: 02/06/2022 
# Version: DRAFT - this includes an update of the plots made out of the spatial 
# campaign and a separate analysis for the schemes reporting
# Author: EGC
# Maintainer: e.grand-clement@exeter.ac.uk

# Project: UsT3

# Nov23 version: Updated by JL 
# Specifically produce plots for Geosmin/MIB 
# Geosmin/MIB data sources: ALS - Until July 2023
#                           GCMS- August 2023 onwards 

# Feb24 version: Updated by EGC
# Specifically produce plots for Geosmin/MIB 
# Geosmin/MIB data sources: ALS - Until July 2023
#                           GCMS- August 2023 onwards 



################################################################################

# setwd("//cles-diamond.ex.ac.uk/brazier/current_projects/UsT3/Data/Data_analysis/UsT3_lab_data")                                          # The rest of the code is based on 'data' as the starting folder

# Load the packages, overall data and summary tables
source("packages_n_plotting_functions.R")
getwdlibrary(rstatix)


##### 0.load the data and tidy #################################################


#TO <- readRDS("Data_input/ALS/ALS_geos_MIB_all_Mar23.rds")

TO <- readRDS("../UsT3_gcms/03_data/geos_mib_als_gcms_data_250206.rds")
#TO <- as.data.frame(TO)

# LOD values are 0 in this table

# add relevant columns (seasons, months, year)
TO$year <- as.numeric(format(TO$datetime_utc, "%Y"))
TO$month <- format(TO$datetime_utc, "%b")
TO$month = factor(TO$month, levels = month.abb)
#TO$month <- as.numeric(format(TO$datetime_utc, "%m"))
TO$season <- FUN.season(TO$datetime_utc)
TO$Year_Month <- (as.yearmon(TO$datetime_utc))

# remove Geosmin and MIB - these are samples that don't have 
#TO <- TO[!is.na(TO$geosmin_ng_l), ]

TO <- TO[!is.na(TO$datetime_utc), ]

#remove investigations and spatial sites and order sites

catch <- c("Wistlandpound", "Roadford", "Burrator", "Stithians")
TO <- subset(TO, (catchment %in% catch))




##### 1. Basic distribution stats / Tables #####################################

TO <- subset(TO, (code_site %in% Routine_sites))  # list of routine sites is in function code

# order the sites 
TO$code_site <- as.factor(TO$code_site)
TO$code_site <- factor(TO$code_site, levels = c("BSB", "BSA", "BSE", "BSL", "BNC", "BSN", "BHW", "BLB", "BRS",
                                                "RMM","RWB", "RJW", "RBC", "RRW", "RNE", "RED", "RES", "RRS",
                                                "STW", "STS","SNS","SPS","SLS","SRS",
                                                "WRB", "WES","WNS", "WLS","WRS"))
# 1.1. Geosmin -----------------------------------------------------------------

# most stats
dat <- TO %>%
  dplyr::group_by(catchment, code_site) %>%
  dplyr::summarise(n_geo = sum(!is.na(geosmin_ng_l)),
            n_censored_geo =sum(geosmin_ng_l==0, na.rm=TRUE),
            Censored_percent_Geo = round(n_censored_geo/n_geo*100,0),
            Count_over_3ng_Geo = sum(geosmin_ng_l>= 3, na.rm=TRUE),
            max_Geo = max(geosmin_ng_l, na.rm = TRUE),
            mean_Geo = mean(geosmin_ng_l, na.rm = TRUE),
            Over3ng_percent = round(Count_over_3ng_Geo/n_geo*100,0))


# bodge to get the min positive value 

#temp <- TO[TO$geosmin_lod != "<LOD", ] # didn't work
temp <- TO[is.na(TO$geosmin_lod),]  # remove <LOD values 
temp <- temp[!is.na(temp$geosmin_ng_l),] # remove NA values

dat1 <- temp %>% 
  dplyr::group_by(catchment, code_site) %>%
  dplyr::summarise(min = min(geosmin_ng_l, na.rm = TRUE))

# put together

Summ_tab <- merge(x = dat, y = dat1, by = c("catchment","code_site"), all.x = TRUE, sort =FALSE)
Summ_tab[,'Censored_percent_Geo']=round(Summ_tab[,'Censored_percent_Geo'],0)    # removes the digits after "."
Summ_tab[,'Over3ng_percent']=round(Summ_tab[,'Over3ng_percent'],0)
Summ_tab <- Summ_tab %>% dplyr::rename("Site" = code_site, "n" = n_geo, "Nb values <LOD" = n_censored_geo , 
                    "% Values <LOD" = Censored_percent_Geo, "Nb exceedances > 3 ng/l" = Count_over_3ng_Geo,
                    "% Exceedances" = Over3ng_percent , "Maximum concentration (ng/l)" =max_Geo , 
                    "Minimum concentration over LOD (ng/l)" = min,
                    "Mean concentration (ng/l)"=mean_Geo)
                    
Summ_tab <- Summ_tab[, c(1,2,3,4,5,8,10,7,6,9)]                                    # reorder of the cols

write.csv(Summ_tab,"csv_output/ALS/Feb2025_Update/Geosmin_summary_exceedances_Feb25.csv")

# 1.1. MIB ---------------------------------------------------------------------

# most stats
dat2 <- TO %>%
  dplyr::group_by(catchment, code_site) %>%
  dplyr::summarise(n_MIB = sum(!is.na(mib_ng_l)),
            n_censored_MIB =sum(mib_ng_l==0, na.rm=TRUE),
            Censored_percent = n_censored_MIB/n_MIB*100,
            Count_over_3ng = sum(mib_ng_l>= 3, na.rm=TRUE),
            max_MIB = max(mib_ng_l, na.rm = TRUE),
            mean_MIB = mean(mib_ng_l, na.rm = TRUE),
            Over3ng_percent = Count_over_3ng/n_MIB*100)


# bodge to get the min positive value 

#temp1 <- TO[TO$MIB_LOD != "<LOD", ]
temp1 <- TO[is.na(TO$mib_lod),] 
temp1 <- temp1[!is.na(temp1$mib_ng_l),] # remove NA values

dat3 <- temp1 %>% 
  dplyr::group_by(catchment, code_site) %>%
  dplyr::summarise(min = min(mib_ng_l, na.rm = TRUE))

# put together
Summ_tab1 <- merge(x = dat2, y = dat3, by = c("catchment","code_site"), all.x = TRUE, sort= FALSE)
Summ_tab1[,'Censored_percent']=round(Summ_tab1[,'Censored_percent'],0)    # removes the digits after "."
Summ_tab1[,'Over3ng_percent']=round(Summ_tab1[,'Over3ng_percent'],0)

Summ_tab1 <- Summ_tab1 %>% dplyr::rename("Site" = code_site, "n" = n_MIB, "Nb values <LOD" = n_censored_MIB , 
                                "% Values <LOD" = Censored_percent, "Nb exceedances > 3 ng/l" = Count_over_3ng,
                                "% Exceedances" = Over3ng_percent , "Maximum concentration (ng/l)" =max_MIB , 
                                "Minimum concentration over LOD (ng/l)" = min, "Mean concentration (ng/l)"=mean_MIB)
Summ_tab1 <- Summ_tab1[, c(1,2,3,4,5,8,10,7,6,9)]    

write.csv(Summ_tab1,"csv_output/ALS/Feb2025_Update/MIB_summary_exceedances_Feb25.csv")

rm(dat,dat1, dat2, dat3, temp, temp1)


## make one table per catchment (ready for the report - this only focuses on the schemes)

catch <- c("Stithians", "Wistlandpound", "Roadford", "Burrator") 
 
 for (i in catch){
   list_df <- split(Summ_tab, Summ_tab$catchment) 
   print(i)
   write.csv(list_df[[i]], paste0("csv_output/ALS/Feb2025_Update/GEO_summary_exceedances_",i, "_Feb25.csv"))
 }
# 
# 
 for (i in catch){
   list_df <- split(Summ_tab1, Summ_tab1$catchment) 
   print(i)
   write.csv(list_df[[i]], paste0("csv_output/ALS/Feb2025_Update/MIB_summary_exceedances_",i, "_Feb25.csv"))
 }


############## do some tidying / prep ##########################################

#### replace the <LOD values 1/2 LOD (1 for the ALS data and 0.25 for UoE daTA)) ####################################

TO_original <- TO
TO <- TO %>%
  mutate(geosmin_ng_l = case_when(
    lab=="als" & geosmin_lod == "<LOD"~ 1,
    lab=="uoe" & geosmin_lod == "<LOD"~ 0.25,
    .default = geosmin_ng_l)) %>%
  mutate(mib_ng_l = case_when(
    lab=="als" & mib_lod == "<LOD"~ 1,
    lab=="uoe" & mib_lod == "<LOD"~ 0.25,
    .default = mib_ng_l))

###### Make spatial table ######################################################
# Commented out as it is not in the schemes anyway 


# TO$datetime_simple <- as.Date(TO$datetime_utc)  # add a column for just the date 
# Spatial <- TO %>% 
#   #filter(datetime_simple== as.Date("2022-07-19")|datetime_simple== as.Date("2022-10-04")|datetime_simple== as.Date("2022-07-04")|datetime_simple== as.Date("2022-10-18")) %>%
#   filter(datetime_simple == as.Date("2023-09-21")|catchment=="Tavy"| catchment == "Colliford" | catchment == "Meldon"| catchment == "Avon" | catchment == "Burrator" | catchment == "NA") #%>%
# 

# save tables
#csvFileName <- paste("csv_output/ALS/ALS_geos_MIB_SPATIAL_Feb23.csv",sep="")
#rdsFileName <- paste("csv_output/ALS/ALS_geos_MIB_SPATIAL_Feb23.rds",sep="")

#write.csv(Spatial, file = csvFileName)
#saveRDS(Spatial, file = rdsFileName)

# Save csv just for Tavy
#Tavy <- Spatial %>%
 # filter(catchment=="Tavy")

# save tables
#csvFileName <- paste("csv_output/ALS/ALS_geos_MIB_TAVY_SPATIAL_Feb23.csv",sep="")

#write.csv(Tavy, file = csvFileName)


# remove the spatial campaing from Tavy
# 
# site <- c("TBL", "TBU", "TCH", "TCO", "TDB" ,"THA","TLB", "TML", "TMT", "TOE" ,
#           "TRB" , "TTA", "TTB" ,"TWB" ,"TWG", "TWH", "TWI", "TWM" ,"TWW" ,"TYB")
# TO <- subset(TO, !(code_site %in% site))
# 
# # order the sites to make sure that the reservoir always comes last in each catchment
# #TO$code_site <- factor(TO$code_site, levels = c("ABS", "ARA" ,"ARS" ,"CAB", "CCC", "CCF", "CCM", "CCT" ,"CDR", "CFC", "CFG" ,"CFS", 
#                                                 "CFT", "CLA" ,"CLD" ,"CMA", "CRB" ,"CRD", "CSL", "CSN" ,"CWB", "CWD" ,"CWR", "CWW", 
#                                                 "RED", "RES" ,"RGW", "RJW", "RMM", "RMN" ,"RNE","RRM" ,"RRW" ,"RWB","RRS", "SLS" ,"SNS", "SPS", 
#                                                 "STS" ,"STW" ,"SRS", "TNT","THB" , "TRL", "TRW","TLD" ,"TCR", "WES", "WNS", "WRB" ,"WRS"))
# 
# 
# 
# # Remove the spatial campaign from Wistlandpound
# site <- c("WBB", "WBC", "WBD", "WBE", "WBF", "WBW", "WET", "WRH", "WRP", "WSF")
# TO <- subset(TO, !(code_site %in% site))

# Make a summary table for the spatial campaigns only --------------------------
# Not needed at this stage


# most stats - THE MEAN VALUES ARE WRONG AND WILL NEED AMENDING MANUALLY !!!!
# #dat <- Spatial %>%
#   dplyr::group_by(catchment,month, waterbody_name) %>%
#   dplyr::summarise(n_geo = sum(!is.na(Geosmin_ng_l)),                           # This is the total n
#                    n_over_LOD =sum(Geosmin_ng_l>0.25, na.rm=TRUE),          # N>LOD
#                    max_Geo = max(Geosmin_ng_l, na.rm = TRUE),
#                    mean_Geo = mean(Geosmin_ng_l>0.25),
#                    n_MIB = sum(!is.na(MIB_ng_l)),                           # This is the total n
#                    n_over_LOD_MIB =sum(MIB_ng_l>0.25, na.rm=TRUE),          # N>LOD
#                    max_MIB = max(MIB_ng_l, na.rm = TRUE),
#                    mean_MIB = mean(MIB_ng_l>0.25))

# bodge to get the min positive value 
#temp <- Spatial[Spatial$Geosmin_LOD != "<LOD", ]  # remove <LOD values 
#temp <- temp[!is.na(temp$Geosmin_ng_l),] # remove NA values

#dat1 <- temp %>% 
  # dplyr::group_by(catchment,month, waterbody_name) %>%
  # dplyr::summarise(min_Geo = min(geosmin_ng_l, na.rm = TRUE))                   # this is the mean over LOD


# same for MIB
#temp1 <- Spatial[Spatial$MIB_LOD != "<LOD", ]  # remove <LOD values 
#temp1 <- temp1[!is.na(temp1$MIB_ng_l),] # remove NA values

#dat2 <- temp1 %>% 
  # dplyr::group_by(catchment,month, waterbody_name) %>%
  # dplyr::summarise(min_MIB = min(MIB_ng_l, na.rm = TRUE))                   # this is the mean over LOD

# put together
#Summ_tab <- merge(x = dat, y = dat1, by = c("catchment","month", "waterbody_name"), all.x = TRUE)
#Summ_tab <- merge(x=Summ_tab, y = dat2, by = c("catchment","month", "waterbody_name"), all.x = TRUE)


#Summ_tab <- Summ_tab[, c(1,2,3,4,5,6,8,7,9)]                                    # reorder of the cols

#write.csv(Summ_tab,"csv_output/ALS/ALS_geos_MIB_summary_SPATIAL_Feb23.csv")

  
  



# 2.PLOTS ######################################################################

# 2.1. BOXPLOTS -----------------------------------------------------------------

# 2.1.1. tidying TO data frame for plotting ....................................

TO$datetime_simple <- as.Date(TO$datetime_utc)  # add a column for just the date 

#Spatial <- TO %>% 
  #filter(datetime_simple== as.Date("2022-07-19")|datetime_simple== as.Date("2022-10-04")|datetime_simple== as.Date("2022-07-04")|datetime_simple== as.Date("2022-10-18")) %>%
 # filter(datetime_simple == as.Date("2023-09-21")|catchment=="Tavy"| catchment == "Colliford" | catchment == "Meldon"| catchment == "Avon" | catchment == "Burrator" | catchment == "NA") #%>%

#catch <- c("Wistlandpound", "Roadford", "Burrator", "Stithians")
#TO <- subset(TO, (catchment %in% catch))


#site <- c("RMM","RWB", "RJW", "RBC", "RRW", "RNE", "RED", "RES", "RRS","WRB", "WES","WNS", "WLS","WRS","STW", "STS","SNS","SPS","SLS","SRS","BSB", "BSA", "BSE", "BSL", "BNC", "BSN", "BHW", "BLB", "BRS")
#TO <- subset(TO, (code_site %in% site))

# order the sites 
#TO$code_site <- factor(TO$code_site, levels = c("RMM","RWB", "RJW", "RBC", "RRW", "RNE", "RED", "RES", "RRS","WRB", "WES","WNS", "WLS","WRS","STW", "STS","SNS","SPS","SLS","SRS","BSB", "BSA", "BSE", "BSL", "BNC", "BSN", "BHW", "BLB", "BRS"))


# 2.1.2. Boxplots per catchment ------------------------------------------------




# Including the LOD as 1/2 values

# geosmin
catch <- c("Burrator", "Stithians", "Wistlandpound", "Roadford") 
geo <- TO[which(TO$geosmin_ng_l> 0),]             # defines df and uses values >Lowest LOD
Ylab <-(expression(paste("Geosmin (ng ", L^-1,")")))
legnd <- c("Geosmin", "Catchment", Ylab)
max <- max(geo$geosmin_ng_l)
#LOD <- 2 #max(geo$geosmin_LOD_val)

P1 <- ggBoxplot(geo, geo$catchment, geo$geosmin_ng_l )+
  geom_hline(yintercept = 3, linetype ="dashed", colour = "darkred", linewidth = 0.8)


tiff(paste0("Plots/Taste_odour/Feb2025_Updated/Geosmin_all_sites_boxplots_Feb25.tiff"), width = 200, height = 200, 
       units = "mm", res = 300, compression = "zip"
       ) # creates a link to save a plot
P1
dev.off()


# MIB
MIB <- TO[which(TO$mib_ng_l>0),]                                                # defines df
Ylab <- (expression(paste("MIB (ng ", L^-1,")")))
legnd <- c("MIB", "Catchment", Ylab)
max <- max(MIB$mib_ng_l)

#LOD <- 2#max(MIB$MIB_LOD_val)
P2 <- ggBoxplot(MIB, MIB$catchment, MIB$mib_ng_l)+
  
  geom_boxplot(fill= "#B5CB8D", outlier.size = 1)+
  scale_y_continuous (limits =c(0,max+1),
                      breaks =seq(0,max+1, by=10))+ 
  geom_hline(yintercept = 3, linetype ="dashed", colour = "darkred", linewidth = 0.8)


tiff(paste0("Plots/Taste_odour/Feb2025_Updated/MIB_all_sites_boxplots_Feb25.tiff"), width = 200, height = 200, 
     units = "mm", res = 300, compression = "zip"
     ) # creates a link to save a plot
P2
dev.off()




# 2.1.3.  BOXPLOTS PER SITE PER CATCHMENT (including 0 as different points) ----

# boxplots: separate for Geosmin and MIB, saved separately

# GEOSMIN this works!!! ########################################################

Ylab_g <- expression(paste("Geosmin (ng ", L^-1,")"))
Ylab_m <- (expression(paste("MIB (ng ", L^-1,")")))
legnd <- c("MIB", "Sampling site", Ylab_m)
plot_list <- list()

####

# This uses all the data (incl <LOD)


for (i in catch){
  print(i)
  p <- ggplot(data= subset(TO[which(TO$geosmin_ng_l>0),], catchment == i), 
              aes(x= code_site, y= geosmin_ng_l)) +
    geom_boxplot(outlier.size = 0.3, fill= "#007A8F")+
    geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.75)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 11, vjust = 1, hjust = 0))+
    scale_y_continuous(#expand = c(0,0), #limits =c(0,11),
                       breaks = (seq(0,20, #max(TO$geosmin_ng_l + 10), 
                                     by=1)))+   
    stat_summary(fun.data = stat_box_data,                                      # Use function to add n
                 geom = "text",hjust = 0.5, vjust = 0.9,size = 3) +
    xlab(legnd[2]) +
    ylab (Ylab_g)+
    scale_fill_manual(values=coloursUST)
  
  plot_list[[i]] <- p
  
  file_name = paste("Plots/Taste_odour/Feb2025_Updated/GEOSMIN_Boxplots_PerSite_ ", i, "_Feb25.tiff", sep="") # saves the first series of plots
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip")
  print(plot_list[[i]])
  dev.off()

  p_other <- ggplot(data= subset(TO[which(TO$mib_ng_l>0),], catchment == i), aes(x= code_site, y= mib_ng_l)) +
    geom_boxplot(outlier.size = 0.3, fill= "#C4E046"
                 )+
    geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.75)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 11, vjust = 1, hjust = 0))+
    scale_y_continuous(#expand = c(0,0), #limits =c(0,max+1),
                       breaks = seq(0,20, #max(TO$mib_ng_l + 10), 
                                    by=2)
                       )+
    stat_summary(fun.data = stat_box_data,                                      # Use function to add n
      geom = "text",hjust = 0.5, vjust = 0.9, size = 3, col = "grey") +
    xlab(legnd[2]) +
    ylab (Ylab_m)+
    scale_fill_manual(values=coloursUST)
  
  plot_list[[i]] <- p_other
  
  file_name = paste("Plots/Taste_odour/Feb2025_Updated/MIB_Boxplots_PerSite_ ", i, "_Feb25_2.tiff", sep="") # saves the 2nd series of plots
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
       )
  print(plot_list[[i]])
  dev.off()
  }

print (p_other)



# GEOSMIN AND MIB TOGETHER on the same plot - works ############################

# Updated to increase the size of the outliers

id_vars <- c("datetime_utc", "code_site", "catchment")
meas_var <- c("mib_ng_l", "geosmin_ng_l")
temp <- melt(TO, id.vars=id_vars, measure.vars=meas_var)
Ylab <- (expression(paste("Concentration (ng ", L^-1,")")))
legnd <- c("MIB", "Sampling site", Ylab)

plot_list <- list()

for (i in catch) {
  print(i)
  subset_data <- subset(temp[which(temp$value >= 0.5), ], catchment == i)
  
  # Check for non-finite values and handle them
  if (any(!is.finite(subset_data$value))) {
    warning(paste("Non-finite values found in catchment", i, "and will be removed"))
    subset_data <- subset_data[is.finite(subset_data$value), ]
  }
  
  if (nrow(subset_data) > 0) {
    max_value <- max(subset_data$value, na.rm = TRUE)
    p <- ggplot(data = subset_data, aes(x = code_site, y = value, fill = variable)) +
      geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.75) +
      geom_boxplot(outlier.size = 2, width = 0.5, position = position_dodge(1)) +
      labs(title = i) +
      # scale_fill_manual(values = grad_UsT(length(unique(temp$variable)))) +
      theme_coding() +
      theme(legend.position = "bottom",
            plot.title = element_text(vjust = 1, hjust = 0)) +
      xlab(legnd[2]) +
      ylab(Ylab) +
      scale_y_continuous(breaks = pretty_breaks(n = 6)) + 
      scale_fill_manual(labels = c("MIB", "Geosmin"),
                        values = coloursUST) +
      stat_summary(fun.data = stat_box_data,                                    # Use function to add n
                   geom = "text",
                   #hjust = 0.9, vjust = 0.9, 
                   size = 4, col = "grey52",
                   position = position_dodge(width = 1))
    
    plot_list[[i]] <- p
    
    file_name <- paste("Plots/Taste_odour/Feb2025_Updated/ALL_TO_Boxplots_PerSite_", i, ".tiff", sep = "") 
    
    tiff(file_name, width = 300, height = 200, units = "mm", res = 300, compression = "zip")
    print(plot_list[[i]])
    dev.off()
  } else {
    warning(paste("No valid data to plot for catchment", i))
  }
}


print(p)

#############################

# 2.2. TIMESERIES --------------------------------------------------------------

# plotting function NOT SPATIAL data 

# Geosmin ----------------------------------------------------------------------

plot_list <- list()                                                             # loop to make plots 
Ylab <- expression(paste("Geosmin (ng ", L^-1,")"))
legnd <- c("Geosmin", "Month", Ylab)
#catch <- c(unique(TO2$catchment)) 
max<- max(TO$geosmin_ng_l, na.rm=TRUE)
lims <- as.POSIXct(strptime(c("2022-04-01 00:00","2025-02-01 00:00"), format = "%Y-%m-%d %H:%M"))  

for (i in catch){
  print(i)
  
  sub <- TO[TO$catchment == i,]
  
  p <- ggplot() +
    geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", linewidth = 0.5)+
    geom_segment(aes(x=  as.POSIXct("2023-08-07 09:25:00"), xend =  as.POSIXct("2024-07-15 00:00:00"), y=0.5, yend= 0.5), colour = "dimgrey", linetype = "dotted", linewidth = 0.6)+
    geom_segment(aes(x=  as.POSIXct("2022-04-01 00:00:00"), xend =  as.POSIXct("2023-08-07 09:25:00"), y=2, yend= 2), colour = "dimgrey", linetype = "dotted", linewidth = 0.6)+
   
    geom_point(data= subset(sub[which(is.na(sub$geosmin_lod)),], catchment == i), aes(x= datetime_utc, y= geosmin_ng_l,
                                                             colour = code_site), size = 3, position=position_jitter(h=0.15,w=0.1))+
    geom_point(data= subset(sub[which(!is.na(sub$geosmin_lod)),], catchment == i), aes(x= datetime_utc, y= geosmin_ng_l,
                                                     colour = code_site), shape = 1, size = 3, position=position_jitter(h=0.15,w=0.1))+
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
  file_name = paste("Plots/Taste_odour/Feb2025_Updated/TS_GeosminPerSite_ ", i, "_Feb25.tiff", sep="")
  tiff(file_name, width = 300, height = 200,  units = "mm", res = 300, compression = "zip"
       )
  print(plot_list[[i]])
  dev.off()
}



# MIB -------------------------------------------------------------------------

plot_list <- list()                                                             # loop to make plots 
Ylab <- expression(paste("MIB (ng ", L^-1,")"))
legnd <- c("MIB", "Month", Ylab)
#catch <- c(unique(TO2$catchment)) 
#max<- max(TO$mib_ng_l, na.rm=TRUE)
lims <- as.POSIXct(strptime(c("2022-04-01 00:00","2025-02-01 00:00"), format = "%Y-%m-%d %H:%M"))  

for (i in catch){
  print(i)
  
  sub <- TO[TO$catchment == i,]
  
  p <- ggplot() +
    geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.5)+
    geom_segment(aes(x=  as.POSIXct("2023-08-07 09:25:00"), xend =  as.POSIXct("2024-07-15 00:00:00"), y=0.5, yend= 0.5), colour = "dimgrey", linetype = "dotted", linewidth = 0.6)+
    geom_segment(aes(x=  as.POSIXct("2022-04-01 00:00:00"), xend =  as.POSIXct("2023-08-07 09:25:00"), y=2, yend= 2), colour = "dimgrey", linetype = "dotted", linewidth = 0.6)+
    geom_point(data= subset(sub[which(is.na(sub$mib_lod)),], catchment == i), aes(x= datetime_utc, y= mib_ng_l,
                                                                                  colour = code_site), size = 3, position=position_jitter(h=0.15,w=0.1)
               )+
    geom_point(data= subset(sub[which(!is.na(sub$mib_lod)),], catchment == i), aes(x= datetime_utc, y= mib_ng_l,
                                                                                   colour = code_site), shape = 1, size = 3, position=position_jitter(h=0.15,w=0.1)
               )+
    geom_point(size = 10)+
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
  file_name = paste("Plots/Taste_odour/Feb2025_Updated/TS_MIBPerSite_ ", i, "_Feb25.tiff", sep="")
  tiff(file_name, width = 300, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}



# # 2.3. Boxplots of geosmin/MIB, with TN:TP ratio  as a point - in progress ....
# # This is done on the reservoirs only to start with
# 
# # Select the sites and group by year month
# #dat <- subset(TO, code_site %in% c("RRS", "SRS", "WRS", "WLS")& geosmin_ng_l >0.25)
# dat <- subset(TO, code_site %in% c("RRS", "SRS", "WRS", "WLS"))
# 
# #dat <- dat %>%
#   #group_by(catchment, code_site, Year_Month)%>%
#   #summarise(meanGEO = mean(Geosmin_ng_l, na.rm=TRUE),
#             #meanTN_TP = mean(TN_TP, na.rm=TRUE))
# 
# plot_list <- list()                                                             # loop to make plots 
# Ylab <- expression(paste(Geosmin~(ng/l))) 
# legnd <- c("Geosmin", "Month", Ylab)
# #catch <- c(unique(TO2$catchment)) 
# #max<- max(TO2$Geosmin_ng_l, na.rm=TRUE)
# ratio <- (max(dat$TN_TP, na.rm=TRUE))/(max(dat$Geosmin_ng_l, na.rm=TRUE))
# 
# 
# ################################################################################
# 
# # other test - THISIIIIIIII
# 
# plot_list <- list()    
# 
# for (i in catch){
# 
# print(i)
# p <- ggplot() +
#     geom_point(data= subset(dat, catchment == i), aes(x= datetime_UTC, y=MIB_ng_l , colour = "MIB"),size = 2, shape=17
#                                                       )+
#     geom_point(data= subset(dat, catchment == i), aes(x= datetime_UTC, y= TN_TP*0.35, colour= "TN:TP"), size = 3, shape=1#, position=position_jitter(h=0.15,w=0.15
#                )+
#     geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "#007A8F", size = 0.5)+
#     geom_hline(aes(yintercept = 10.5), linetype = "dotted", colour = "grey23", size = 0.3)+ # 30* 0.35 = scalung the value of the ration below 30 (low ratio / high concentrations)
#    
# 
#     scale_x_datetime(limits = c(),
#                      #date_labels = ("%m"),
#                      date_labels = ("%Y-%m"),
#                      date_breaks = "1 month"#,
#                      #minor_breaks = "1 month"
#     ) +
#    scale_y_continuous(limits =c(0,100),
#                       breaks = seq(0,100, by = 20),
#                       sec.axis = sec_axis(~(.)/0.35,
#                                            #limits= c(0,300),
#                                            breaks = seq(0,300, by =50),
#                                            name = "TN:TP ratio")) +
#     
#     #scale_colour_manual(values = coloursUST) +
#     theme_bw() + theme(axis.title.x=element_blank(),
#                        legend.title=element_blank(),
#                        legend.position = "bottom",
#                        legend.direction = "horizontal",
#                        legend.box = "vertical",
#                        legend.text=element_text(size=12)) +
#     guides(color=guide_legend(nrow=1, byrow=TRUE)) +
#     ylab("MIB concentration (ng/L)")+
#     scale_colour_manual(values=c("#007A8F", "grey23"))
# 
#   plot_list[[i]] <- p
#   
# 
#   
#   file_name = paste("Plots/Taste_odour/Mar2023_Updated/TS_PerSite_MIB_TN_TP_", i, "_Mar23_added_lines.tiff", sep="") # saves the first series of plots
#   tiff(file_name, width = 200, height = 100,  units = "mm", res = 300, compression = "zip")
#   print(plot_list[[i]])
#   dev.off()
#   
#   
# }
# 
# 
# print(p)
# 
# print(plot_list[[2]])
# 
# 
# 
# # other test - THISIIIIIIII=====================
# 
# # define the ratio
# ratio <- (max(dat$Geosmin_ng_l, na.rm=TRUE))/(max(dat$TN_TP, na.rm=TRUE))
# 
# 
# plot_list <- list()    
# 
# for (i in catch){
#   
#   print(i)
#   p <- ggplot() +
#     geom_point(data= subset(dat, catchment == i), aes(x= datetime_UTC, y=Geosmin_ng_l , colour = "Geosmin"),size = 2, shape=17
#     )+
#     geom_point(data= subset(dat, catchment == i), aes(x= datetime_UTC, y= TN_TP*0.065, colour= "TN:TP"), size = 3, shape=1#, position=position_jitter(h=0.15,w=0.15
#     )+
#     
#     geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "#FFC814", size = 0.5)+
#     geom_hline(aes(yintercept = 1.95), linetype = "dotted", colour = "grey23", size = 0.3)+ # 30* 0.35 = scalung the value of the ration below 30 (low ratio / high concentrations)
#     
#     
#     scale_x_datetime(limits = c(),
#                      #date_labels = ("%m"),
#                      date_labels = ("%Y-%m"),
#                      date_breaks = "1 month"#,
#                      #minor_breaks = "1 month"
#     ) +
#     scale_y_continuous(limits =c(0,20),
#                        breaks = seq(0,20, by = 2),
#                        sec.axis = sec_axis(~(.)/0.065,
#                                            #limits= c(0,300),
#                                            breaks = seq(0,300, by =50),
#                                            name = "TN:TP ratio")) +
#     
#     #scale_colour_manual(values = coloursUST) +
#     theme_bw() + theme(axis.title.x=element_blank(),
#                        legend.title=element_blank(),
#                        legend.position = "bottom",
#                        legend.direction = "horizontal",
#                        legend.box = "vertical",
#                        legend.text=element_text(size=12)) +
#     guides(color=guide_legend(nrow=1, byrow=TRUE)) +
#     ylab("Geosmin concentration (ng/L)")+
#     scale_colour_manual(values=c("#FFC814", "grey23"))
#   
#   plot_list[[i]] <- p
#   
#   
#   
#   file_name = paste("Plots/Taste_odour/Mar2023_Updated/TS_PerSite_GEO_TN_TP_", i, "_Mar23_added_lines.tiff", sep="") # saves the first series of plots
#   tiff(file_name, width = 200, height = 100,  units = "mm", res = 300, compression = "zip")
#   print(plot_list[[i]])
#   dev.off()
#   
#   
# }
# 
# 
# print(p)
# 
# 
# 
# # other test - THISIIIIIIII
# 
# plot_list <- list()    
# 
# for (i in catch){
#   
#   print(i)
#   p <- ggplot() +
#     geom_point(data= subset(dat, catchment == i), aes(x= datetime_UTC, y=MIB_ng_l , colour = "MIB"),size = 2, shape=17
#     )+
#     geom_point(data= subset(dat, catchment == i), aes(x= datetime_UTC, y= TN_TP*0.35, colour= "TN:TP"), size = 3, shape=1#, position=position_jitter(h=0.15,w=0.15
#     )+
#     geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "#007A8F", size = 0.5)+
#     #geom_hline(aes(yintercept = 10.5), linetype = "dotted", colour = "grey23", size = 0.3)+ # 30* 0.35 = scalung the value of the ration below 30 (low ratio / high concentrations)
#     
#     
#     scale_x_datetime(limits = c(),
#                      #date_labels = ("%m"),
#                      date_labels = ("%Y-%m"),
#                      date_breaks = "1 month"#,
#                      #minor_breaks = "1 month"
#     ) +
#     scale_y_continuous(limits =c(0,100),
#                        breaks = seq(0,100, by = 20),
#                        sec.axis = sec_axis(~(.)/0.35,
#                                            #limits= c(0,300),
#                                            breaks = seq(0,300, by =50),
#                                            name = "TN:TP ratio")) +
#     
#     #scale_colour_manual(values = coloursUST) +
#     theme_bw() + theme(axis.title.x=element_blank(),
#                        legend.title=element_blank(),
#                        legend.position = "bottom",
#                        legend.direction = "horizontal",
#                        legend.box = "vertical",
#                        legend.text=element_text(size=12)) +
#     guides(color=guide_legend(nrow=1, byrow=TRUE)) +
#     ylab("MIB concentration (ng/L)")+
#     scale_colour_manual(values=c("#007A8F", "grey23"))
#   
#   plot_list[[i]] <- p
#   
#   
#   
#   file_name = paste("Plots/Taste_odour/Mar2023_Updated/TS_PerSite_MIB_TN_TP_", i, "_Mar23.tiff", sep="") # saves the first series of plots
#   tiff(file_name, width = 200, height = 100,  units = "mm", res = 300, compression = "zip")
#   print(plot_list[[i]])
#   dev.off()
#   
#   
# }
# 
# 
# print(p)
# 
# print(plot_list[[2]])
# 
# 
# 
# # other test - THISIIIIIIII=====================
# 
# # define the ratio
# ratio <- (max(dat$Geosmin_ng_l, na.rm=TRUE))/(max(dat$TN_TP, na.rm=TRUE))
# 
# 
# plot_list <- list()    
# 
# for (i in catch){
#   
#   print(i)
#   p <- ggplot() +
#     geom_point(data= subset(dat, catchment == i), aes(x= datetime_UTC, y=Geosmin_ng_l , colour = "Geosmin"),size = 2, shape=17
#     )+
#     geom_point(data= subset(dat, catchment == i), aes(x= datetime_UTC, y= TN_TP*0.065, colour= "TN:TP"), size = 3, shape=1#, position=position_jitter(h=0.15,w=0.15
#     )+
#     
#     geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "#FFC814", size = 0.5)+
#    # geom_hline(aes(yintercept = 1.95), linetype = "dotted", colour = "grey23", size = 0.3)+ # 30* 0.35 = scalung the value of the ration below 30 (low ratio / high concentrations)
#     
#     
#     scale_x_datetime(limits = c(),
#                      #date_labels = ("%m"),
#                      date_labels = ("%Y-%m"),
#                      date_breaks = "1 month"#,
#                      #minor_breaks = "1 month"
#     ) +
#     scale_y_continuous(limits =c(0,20),
#                        breaks = seq(0,20, by = 2),
#                        sec.axis = sec_axis(~(.)/0.065,
#                                            #limits= c(0,300),
#                                            breaks = seq(0,300, by =50),
#                                            name = "TN:TP ratio")) +
#     
#     #scale_colour_manual(values = coloursUST) +
#     theme_bw() + theme(axis.title.x=element_blank(),
#                        legend.title=element_blank(),
#                        legend.position = "bottom",
#                        legend.direction = "horizontal",
#                        legend.box = "vertical",
#                        legend.text=element_text(size=12)) +
#     guides(color=guide_legend(nrow=1, byrow=TRUE)) +
#     ylab("Geosmin concentration (ng/L)")+
#     scale_colour_manual(values=c("#FFC814", "grey23"))
#   
#   plot_list[[i]] <- p
#   
#   
#   
#   file_name = paste("Plots/Taste_odour/Mar2023_Updated/TS_PerSite_GEO_TN_TP_", i, "_Mar23.tiff", sep="") # saves the first series of plots
#   tiff(file_name, width = 200, height = 100,  units = "mm", res = 300, compression = "zip")
#   print(plot_list[[i]])
#   dev.off()
#   
#   
# }
# 
# 
# print(p)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # plot reservoir values only, with TN:TP ratios --------------------------------
# 
# res <- c("WRS", "WLS", "RRS", "SRS")
# TO2 <- subset(TO2, (code_site %in% res))
# TO2[TO2 == Inf] <- 0                        # replaces Inf values by 0
# 
# 
# # Wistlandpound
# Ylab <- expression(paste(Geosmin~(ng/l))) 
# legnd <- c("Geosmin", "Month", Ylab)
# 
# TO3 <- TO2 %>%
#   filter(catchment == "Wistlandpound")
# ratio <- max(TO3$Geosmin_ng_l, na.rm=TRUE)/max(TO3$TN_TP, na.rm=TRUE)
# 
# 
# p <- ggplot(data= TO3)+ 
#   #geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.75)+
#   
#   geom_bar(data= subset(TO3[which(TO3$Geosmin_ng_l>1),]), aes(x= datetime_UTC, y= Geosmin_ng_l#,
#                                                                               #colour = code_site), size = 4, position=position_jitter(h=0.15,w=0.15)
#            ))+
#   geom_line(data= subset(TO3[which(TO3$TN_TP >0),]), aes(x= datetime_UTC, y= TN_TP * ratio#,
#                                                                                #colour = code_site), size = 2, position=position_jitter(h=0.15,w=0.15)
#                                                          ))+
#   
#   
#   
#   labs(title = "Wistlandpound") +
#   # scale_fill_manual(values = grad_UsT(length(unique(temp$variable)))) +
#   theme_coding()+
#   theme(legend.position = "bottom",
#         plot.title = element_text(vjust = 1, hjust = 0))+
#   xlab(legnd[2]) +
#   ylab (Ylab)+
#  # scale_y_continuous(limits = c(0,max(TO3$Geosmin_ng_l + 1),
#                                 #breaks = seq(0,max(TO3$Geosmin_ng_l + 1), by =10)))+
#   scale_y_continuous(sec.axis = sec_axis(~./ratio))
#                      #limits= c(-1,20),
#                      #breaks = seq(-1,50),
#                      #name = legnd[3]))+
#   #)+
#   #scale_fill_manual(labels=c("MIB", "Geosmin"),
#                     #values= coloursUST)
# 
# 
# print(p)
# 
# file_name = paste("Plots/Taste_odour/Mar2023_Updated/ALL_TO_Boxplots_Wistlandpound_Separate.tiff", sep="") 
# 
# tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip")
# print(p)
# dev.off()




# 2.3. Bar charts - more visual and overall better-------------------------------

# 2.3.1. Regular sampling ------------------------------------------------------

# Geosmin ----------------------------------------------------------------------
catch <- c("Stithians", "Wistlandpound", "Roadford") 
id_vars <- c("month", "code_site", "catchment")
meas_var <- c("geosmin_ng_l")
plot_list <- list() 
Ylab <-  expression(paste("Geosmin (ng ", L^-1,")"))
temp <- melt(TO, id.vars=id_vars, measure.vars=meas_var)
any(is.na(temp$value))
temp<- temp[!is.na(temp$value), ] 
max <- max(geo$geosmin_ng_l)
plot_list <- list() 


for (i in catch){
    print(i)
  
  p <- ggplot (data=subset(temp, catchment == i),aes(x = month, y = value)) +                         # values indicate to use the value in the cell
    geom_bar(aes(fill = code_site),stat = "identity",position = "dodge", width=0.65)+
    geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.6)+
    geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "grey", size = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(vjust = 1, hjust = 0))+
    ylab (Ylab)+
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(0,max(temp$value)))+
    # scale_y_continuous(expand = c(0,0), limits =c(0,max+1), breaks = seq(0,max+1, by=1))+
  # scale_x_datetime(name = "Sampling time", date_breaks = "1 month", 
                     #date_labels = "%m/%Y")# + 
  scale_fill_manual(values=coloursUST)
  plot_list[[i]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in catch){
  file_name = paste("Plots/Taste_odour/Feb2025_Updated/BarCharts_GeosminPerSite_Feb25_", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}

catch <- c("Stithians", "Wistlandpound", "Roadford")
id_vars <- c("month", "code_site", "catchment")
meas_var <- c("geosmin_ng_l")
plot_list <- list()
Ylab <- expression(paste("Geosmin (ng ", L^-1,")"))
temp <- melt(TO, id.vars = id_vars, measure.vars = meas_var)

# Check for non-finite values and handle them
if (any(!is.finite(temp$value))) {
  warning("Non-finite values found in temp$value and will be removed")
  temp <- temp[is.finite(temp$value), ]
}

# Ensure we have data after filtering
if (nrow(temp) == 0) {
  stop("No valid data available after filtering non-finite values.")
}

# Determine the maximum value after filtering
max_value <- max(temp$value, na.rm = TRUE)

for (i in catch) {
  print(i)
  
  p <- ggplot(data = subset(temp, catchment == i), aes(x = month, y = value)) +
    geom_bar(aes(fill = code_site), stat = "identity", position = "dodge", width = 0.65) +
    geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.6) +
    geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "grey", size = 0.6) +
    labs(title = i) +
    theme_coding() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(vjust = 1, hjust = 0)) +
    ylab(Ylab) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max_value + 1),   # Set limits with some padding
                       breaks = pretty_breaks(n = 10)) +  # Use pretty breaks for better tick marks
    scale_fill_manual(values = coloursUST)
  
  plot_list[[i]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (i in catch) {
  file_name <- paste("Plots/Taste_odour/Feb2025_Updated/BarCharts_GeosminPerSite_Feb25_", i, ".tiff", sep = "")
  tiff(file_name, width = 200, height = 200, units = "mm", res = 300, compression = "zip")
  print(plot_list[[i]])
  dev.off()
}
# MIB --------------------------------------------------------------------------

id_vars <- c("month", "code_site", "catchment")
meas_var <- c("mib_ng_l")
plot_list <- list() 
Ylab <- expression(paste("MIB (ng ", L^-1,")"))
temp <- melt(TO, id.vars=id_vars, measure.vars=meas_var)
max <- max(MIB$mib_ng_l)
plot_list <- list() 

for (i in catch){
  print(i)

  p <- ggplot (data=subset(temp, catchment == i),aes(x = month, y = value)) +                         # values indicate to use the value in the cell
    geom_bar(aes(fill = code_site),stat = "identity",position = "dodge", width=0.65)+
    geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.6)+
    geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "grey", size = 0.6)+
    labs(title = i) +
    theme_coding()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          plot.title = element_text(vjust = 1, hjust = 0))+
    ylab (Ylab)+
    scale_y_continuous(expand = c(0,0),)+
                      #breaks = seq(0,max(temp$value)))+
    
    # scale_x_datetime(name = "Sampling time", date_breaks = "1 month", 
    #date_labels = "%m/%Y")# + 
    scale_fill_manual(values=coloursUST)
  plot_list[[i]] <- p
}



# Save plots to tiff. Makes a separate file for each plot.
for (i in catch){
  file_name = paste("Plots/Taste_odour/Feb2025_Updated/BarCharts_MIBPerSite_Feb25_", i, ".tiff", sep="")
  tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
  )
  print(plot_list[[i]])
  dev.off()
}

# # 5.2. Spatial campaigns  ######################################################
# 
# # generic boxplot
# 
# #only select the positive values
# 
# #id_vars <- c("month", "code_site", "catchment", "waterbody_name")
# #meas_var <- c("Geosmin_ng_l")
# Ylab <- expression(paste(Geosmin~(ng/l))) 
# Ylab1 <- expression(paste(MIB~(ng/l))) 
# legnd <- c("Geosmin", "Sampling site")
# temp <- melt(Spatial, id.vars=id_vars, measure.vars=meas_var)
# temp <- temp[which(temp$value>1),]      
# max <- max(temp$value)
# plot_list <- list() 
# catch_spat <- c("Tavy", "Colliford")
# 
# for (i in catch_spat){
#   print(i)
#   p <- ggplot(data= subset(Spatial, catchment == i), aes(x= month, y= Geosmin_ng_l)) +
#     geom_bar(outlier.size = 0.1, fill= "#007A8F"
#     )+
#     labs(title = i) +
#     theme_coding()+
#     theme(legend.position = "bottom",
#           plot.title = element_text(size = 11, vjust = 1, hjust = 0),
#           axis.text.x = element_text(angle = 45, vjust = 0.5))+
#     #scale_y_continuous(expand = c(0,0), limits =c(0,11),
#     #breaks = seq(0,11, by=1))+   
#     xlab(legnd[2]) +
#     ylab (Ylab)+
#     scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
#     scale_fill_manual(values=coloursUST)
#   
#   plot_list[[i]] <- p
#   
#   file_name = paste("Plots/Taste_odour/Geo_Boxplots_SPATIAL_GENERAL_", i, ".tiff", sep="") # saves the first series of plots
#   tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip")
#   print(plot_list[[i]])
#   dev.off()
# 
# p_other <- ggplot(data= subset(Spatial, catchment == i), aes(x= month, y= MIB_ng_l)) +
#     geom_boxplot(outlier.size = 0.1)+
#     labs(title = i) +
#     theme_coding()+
#     theme(legend.position = "bottom",
#           plot.title = element_text(size = 11, vjust = 1, hjust = 0),
#           axis.text.x = element_text(angle = 45, vjust = 0.5))+
#     #scale_y_continuous(expand = c(0,0), limits =c(0,max+1),
#     #breaks = seq(0,max+1, by=1))+   
#     xlab(legnd[2]) +
#     ylab (Ylab1)+
#     scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
#     scale_fill_manual(values=coloursUST)
#   
#   plot_list[[i]] <- p_other
#   
#   file_name = paste("Plots/Taste_odour/MIB_Boxplots_SPATIAL_GENERAL_", i, ".tiff", sep="") # saves the 2nd series of plots
#   tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
#   )
#   print(plot_list[[i]])
#   dev.off()
# }
# 


# Geosmin difference between seasons per site ----------------------------------

# id_vars <- c("month", "code_site", "catchment", "waterbody_name")
# meas_var <- c("Geosmin_ng_l")
# Ylab <- expression(paste(Geosmin~(ng/l))) 
# temp <- melt(Spatial, id.vars=id_vars, measure.vars=meas_var)
# max <- max(temp$value)
# plot_list <- list() 
# catch_spat <- c("Tavy", "Colliford")
# 
# 
# for (i in catch_spat){
#   print(i)
#   p <- ggplot (data=subset(temp, catchment == i),aes(x = waterbody_name, y = value)) +                         # values indicate to use the value in the cell
#     geom_col(aes(fill = month), stat = "identity",position = "dodge", width=0.65)+
#     geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.6)+
#     geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "grey", size = 0.6)+
#     labs(title = i) +
#     theme_coding()+
#     theme(legend.position = "bottom",
#           axis.text.x = element_text(angle = 45, vjust = 0.5),
#           plot.title = element_text(vjust = 1, hjust = 0),
#           axis.title.x = element_blank())+
#     ylab (Ylab)+
#     scale_y_continuous(expand = c(0,0), 
#                        breaks = seq(0,max(temp$value)))+
#     #scale_y_continuous(limits =c(0,max+1), breaks = seq(0,max+1, by=1))+
#     # scale_x_datetime(name = "Sampling time", date_breaks = "1 month", 
#     #date_labels = "%m/%Y")# + 
#     scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
#     scale_fill_manual(values=coloursUST)
#   plot_list[[i]] <- p
# }
# 
# 
# # Save plots to tiff. Makes a separate file for each plot.
# for (i in catch_spat){
#   file_name = paste("Plots/Taste_odour/Feb2023_Updated/BarCharts_Geosmin_SPATIAL_", i, "_Zonea_Updated.tiff", sep="")
#   tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
#   )
#   print(plot_list[[i]])
#   dev.off()
# }
# 
# 
# # MIB difference between seasons per site---------------------------------------
# 
# id_vars <- c("month", "code_site", "catchment", "waterbody_name")
# meas_var <- c("MIB_ng_l")
# plot_list <- list() 
# Ylab <- expression(paste(MIB~(ng/l))) 
# temp <- melt(Spatial, id.vars=id_vars, measure.vars=meas_var)
# max <- max(temp$value)
# plot_list <- list() 
# 
# for (i in catch_spat){
#   print(i)
#   
#   p <- ggplot (data=subset(temp, catchment == i),aes(x = waterbody_name, y = value)) +                         # values indicate to use the value in the cell
#     geom_bar(aes(fill = month),stat = "identity",position = "dodge", width=0.65)+
#     geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.6)+
#     geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "grey", size = 0.6)+
#     labs(title = i) +
#     theme_coding()+
#     theme(legend.position = "bottom",
#           axis.text.x = element_text(angle = 45, vjust = 0.5),
#           plot.title = element_text(vjust = 1, hjust = 0),
#           axis.title.x = element_blank())+
#     ylab (Ylab)+
#     scale_y_continuous(expand = c(0,0),
#                        breaks = seq(0,max(temp$value)))+
#     
#     # scale_x_datetime(name = "Sampling time", date_breaks = "1 month", 
#     scale_fill_manual(values=coloursUST)+
#     scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
#   
#   plot_list[[i]] <- p
# }
# 
# 
# 
# # Save plots to tiff. Makes a separate file for each plot.
# for (i in catch_spat){
#   file_name = paste("Data_analysis/UsT3_lab_data/Plots/Taste_odour/BarCharts_MIB_SPATIAL_", i, ".tiff", sep="")
#   tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
#   )
#   print(plot_list[[i]])
#   dev.off()
# }
# 
# 
# 
# # Geosmin difference between groups of sites per month  ------------------------
# 
# id_vars <- c("month", "code_site", "catchment", "waterbody_name")
# meas_var <- c("Geosmin_ng_l")
# Ylab <- expression(paste(Geosmin~(ng/l))) 
# temp <- melt(Spatial, id.vars=id_vars, measure.vars=meas_var)
# max <- max(temp$value)
# plot_list <- list() 
# catch_spat <- c("Tavy", "Colliford")
# 
# for (i in catch_spat){
#   print(i)
#   p <- ggplot (data=subset(temp, catchment == i),aes(x = month, y = value)) +                         # values indicate to use the value in the cell
#     geom_bar(aes(fill = waterbody_name), stat = "identity",position = "dodge", width=0.65)+
#     geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.6)+
#     geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "grey", size = 0.6)+
#     labs(title = i) +
#     theme_coding()+
#     theme(legend.position = "bottom",
#           axis.text.x = element_text(angle = 45, vjust = 0.5),
#           plot.title = element_text(vjust = 1, hjust = 0),
#           axis.title.x = element_blank())+
#     ylab (Ylab)+
#     scale_y_continuous(expand = c(0,0), 
#                        breaks = seq(0,max(temp$value)))+
#     #scale_y_continuous(limits =c(0,max+1), breaks = seq(0,max+1, by=1))+
#     # scale_x_datetime(name = "Sampling time", date_breaks = "1 month", 
#     #date_labels = "%m/%Y")# + 
#     scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
#     scale_fill_manual(values=coloursUST)
#   plot_list[[i]] <- p
# }
# 
# 
# # Save plots to tiff. Makes a separate file for each plot.
# for (i in catch_spat){
#   file_name = paste("Data_analysis/UsT3_lab_data/Plots/Taste_odour/BarCharts_Geosmin_SPATIAL_other_", i, ".tiff", sep="")
#   tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
#   )
#   print(plot_list[[i]])
#   dev.off()
# }
# 
# 
# # MIB difference between seasons per site---------------------------------------
# 
# id_vars <- c("month", "code_site", "catchment", "waterbody_name")
# meas_var <- c("MIB_ng_l")
# plot_list <- list() 
# Ylab <- expression(paste(MIB~(ng/l))) 
# temp <- melt(Spatial, id.vars=id_vars, measure.vars=meas_var)
# max <- max(temp$value)
# plot_list <- list() 
# 
# for (i in catch_spat){
#   print(i)
#   
#   p <- ggplot (data=subset(temp, catchment == i),aes(x = month, y = value)) +                         # values indicate to use the value in the cell
#     geom_bar(aes(fill = waterbody_name),stat = "identity",position = "dodge", width=0.65)+
#     geom_hline(aes(yintercept = 3), linetype = "dashed", colour = "darkred", size = 0.6)+
#     geom_hline(aes(yintercept = 2), linetype = "dashed", colour = "grey", size = 0.6)+
#     labs(title = i) +
#     theme_coding()+
#     theme(legend.position = "bottom",
#           axis.text.x = element_text(angle = 45, vjust = 0.5),
#           plot.title = element_text(vjust = 1, hjust = 0),
#           axis.title.x = element_blank())+
#     ylab (Ylab)+
#     scale_y_continuous(expand = c(0,0),
#                        breaks = seq(0,max(temp$value)))+
#     
#     # scale_x_datetime(name = "Sampling time", date_breaks = "1 month", 
#     scale_fill_manual(values=coloursUST)+
#     scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
#   
#   plot_list[[i]] <- p
# }
# 
# 
# 
# # Save plots to tiff. Makes a separate file for each plot.
# for (i in catch_spat){
#   file_name = paste("Data_analysis/UsT3_lab_data/Plots/Taste_odour/BarCharts_MIB_SPATIAL_other_", i, ".tiff", sep="")
#   tiff(file_name, width = 200, height = 200,  units = "mm", res = 300, compression = "zip"
#   )
#   print(plot_list[[i]])
#   dev.off()
# }
# 
# 
# 
# 
# 
# 

### JL updated -END ###








################################################################################
## Statistical analysis - comparison between sites #############################


# are the data normally distributed? ############################################

# make a long format table (TO2 is the table that doesn't contain Colliford because this is not applicable to that catchment)
TO2_long <- pivot_longer(TO2,
                         cols = c("Geosmin_ng_l"),
                         names_to = "contaminand",
                         values_to = "result",
                         values_drop_na = TRUE)

#test <- TO2_long %>% select(code_site, result)   

# removing all the sites that have either only the same value (ie ARS) or less than 3 datapoints

bb <- c("ARS","RGW", "RRM", "RMM", "RMN", "RWB", "WES")
test <- TO2_long[!( TO2_long$code_site %in% c( bb )),]
rm(bb)


stat.test <- test %>%
  group_by(code_site) %>%
  summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))

# count how many are normally distributed (ie p.value > 0.05) ?
length(which(stat.test$p.value > 0.05)) #5 locations are normally distributed out of 23, so I'm going to consider they are all not normal



### Friedman's test ############################################################
# we want to test if the different sites are significantly different from each other

# Geosmin ######################################################################

# let's average the data per month
TO_av <- TO2_long %>%
  dplyr::group_by(catchment, code_site, month) %>%
  dplyr::summarize(mean_Geo = mean(result),
                   n=n())

TO_av <- as.data.frame(TO_av)
sum(TO_av$n)

# Separate all the df needed for now (quicker than working on the code to be applied to a list)

# Avon -------------------------------------------------------------------------
Avon <- TO_av[TO_av$catchment == "Avon", ]
Avon <- as.data.frame(Avon)                                                     # make as a df
Avon <- Avon[Avon$month != "Aug",]                                              # remove incomplete months
Avon <- Avon %>%
  convert_as_factor(catchment,code_site, month)                                           # make as factors
Avon <- droplevels(Avon)                                                        # drop levels that somehow stay there even if technically removed

# do the friedman's test
res.fried_avon <- Avon %>%
  dplyr::group_by(catchment) %>%
  friedman_test(mean_Geo ~ code_site| month)


# Tavy -------------------------------------------------------------------------
Tavy <- TO_av[TO_av$catchment == "Tavy", ]

Tavy <- as.data.frame(Tavy)                                                     # make as a df
nb <- Tavy %>%
  group_by(catchment,code_site)%>%
  count(n= n())
rm(nb)

Tavy <- Tavy[Tavy$code_site != "TLD",]                                              # remove TLD as it's the only one with incomplete datasets
Tavy <- Tavy %>%
  convert_as_factor(code_site, month)                                           # make as factors
Tavy <- droplevels(Tavy)                                                        # drop levels that somehow stay there even if technically removed

# do the friedman's test
res.fried_tavy <- Tavy %>%
  dplyr::group_by(catchment) %>%
  friedman_test(mean_Geo ~ code_site| month)


# Tavy spatial only ------------------------------------------------------------

Tav_sp <- Spatial[Spatial$catchment == "Tavy", ]
Tav_sp <- pivot_longer(Tav_sp,
                          cols = c("Geosmin_ng_l"),
                          names_to = "contaminand",
                          values_to = "result",
                          values_drop_na = TRUE)
Tav_sp <- Tav_sp %>%
  dplyr::group_by(catchment, code_site, month) %>%
  dplyr::summarize(mean_Geo = mean(result),
                   n=n())

Tav_sp <- as.data.frame(Tav_sp)
Tav_sp <- droplevels(Tav_sp)

res.fried_tav_sp <- Tav_sp %>%
  dplyr::group_by(catchment) %>%
  friedman_test(mean_Geo ~ month|code_site)




# Colliford using the spatial data (comparing only ) ---------------------------

# prep the table
coll <- Spatial[Spatial$catchment == "Colliford", ]
coll_long <- pivot_longer(coll,
                         cols = c("Geosmin_ng_l"),
                         names_to = "contaminand",
                         values_to = "result",
                         values_drop_na = TRUE)

# let's average the data per month # replaced by Wilcox test--------------------
coll_av <- coll_long %>%
  dplyr::group_by(catchment, code_site, month) %>%
  dplyr::summarize(mean_Geo = mean(result),
                   n=n())

# remove unique values
bb <- c("CDR","CLD", "CRW", "CWW", "CWR")
coll_av <- coll_av[!( coll_av$code_site %in% c( bb )),]
rm(bb)

str(coll_av)
coll_av <- as.data.frame(coll_av)
coll_av <- droplevels(coll_av)

res.fried_coll <- coll_av %>%
  dplyr::group_by(catchment) %>%
  friedman_test(mean_Geo ~ month|code_site)

# now merge the results together and save output

Fried_results_table <- bind_rows(res.fried_avon, res.fried_coll, res.fried_tavy, res.fried_tav_sp)
Fried_results_table$Campaign_type <- c("regular", "spatial", "regular", "spatial")          
write.csv(Fried_results_table, file="Data_analysis/UsT3_lab_data/csv_output/ALS/Friedman_test_results_Geosmin.csv")



################################################################################
# MIB  #########################################################################

# make a long format table (TO2 is the table that doesn't contain Colliford because this is not applicable to that catchment)
TO2_long2 <- pivot_longer(TO2,
                         cols = c("MIB_ng_l"),
                         names_to = "contaminand",
                         values_to = "result",
                         values_drop_na = TRUE)





# let's average the data per month
TO_av2 <- TO2_long2 %>%
  dplyr::group_by(catchment, code_site, month) %>%
  dplyr::summarize(mean_MIB = mean(result),
                   n=n())

TO_av2 <- as.data.frame(TO_av2)
sum(TO_av2$n)

# Separate all the df needed for now (quicker than working on the code to be applied to a list)

# Avon -------------------------------------------------------------------------
Avon2 <- TO_av2[TO_av2$catchment == "Avon", ]
Avon2 <- as.data.frame(Avon)                                                     # make as a df
Avon2 <- Avon[Avon$month != "Aug",]                                              # remove incomplete months
Avon2 <- Avon %>%
  convert_as_factor(catchment,code_site, month)                                           # make as factors
Avon2 <- droplevels(Avon)                                                        # drop levels that somehow stay there even if technically removed

# do the friedman's test
res.fried_avon_MIB <- Avon2 %>%
  dplyr::group_by(catchment) %>%
  friedman_test(mean_MIB ~ code_site| month)


# Tavy -------------------------------------------------------------------------
Tavy2 <- TO_av2[TO_av2$catchment == "Tavy", ]

Tavy2 <- as.data.frame(Tavy2)                                                     # make as a df
nb <- Tavy2 %>%
  group_by(catchment,code_site)%>%
  count(n= n())
rm(nb)

Tavy2 <- Tavy2[Tavy2$code_site != "TLD",]                                              # remove TLD as it's the only one with incomplete datasets
Tavy2 <- Tavy2 %>%
  convert_as_factor(code_site, month)                                           # make as factors
Tavy2 <- droplevels(Tavy2)                                                        # drop levels that somehow stay there even if technically removed

# do the friedman's test
res.fried_tavy_MIB <- Tavy2 %>%
  dplyr::group_by(catchment) %>%
  friedman_test(mean_MIB ~ code_site| month)


# Tavy spatial only ------------------------------------------------------------

Tav_sp2 <- Spatial[Spatial$catchment == "Tavy", ]
Tav_sp2 <- pivot_longer(Tav_sp2,
                       cols = c("MIB_ng_l"),
                       names_to = "contaminand",
                       values_to = "result",
                       values_drop_na = TRUE)
Tav_sp2 <- Tav_sp2 %>%
  dplyr::group_by(catchment, code_site, month) %>%
  dplyr::summarize(mean_MIB = mean(result),
                   n=n())

Tav_sp2 <- as.data.frame(Tav_sp2)
Tav_sp2 <- droplevels(Tav_sp2)

res.fried_tav_sp_MIB <- Tav_sp2 %>%
  dplyr::group_by(catchment) %>%
  friedman_test(mean_MIB ~ month|code_site)


# Colliford using the spatial data (comparing only ) ---------------------------

# prep the table
coll2 <- Spatial[Spatial$catchment == "Colliford", ]
coll_long2 <- pivot_longer(coll2,
                          cols = c("MIB_ng_l"),
                          names_to = "contaminand",
                          values_to = "result",
                          values_drop_na = TRUE)

# let's average the data per month
coll_av2 <- coll_long2 %>%
  dplyr::group_by(catchment, code_site, month) %>%
  dplyr::summarize(mean_MIB = mean(result),
                   n=n())

# remove unique values
bb <- c("CDR","CLD", "CRW", "CWW", "CWR")
coll_av2 <- coll_av2[!( coll_av2$code_site %in% c( bb )),]
rm(bb)

str(coll_av2)
coll_av <- as.data.frame(coll_av2)
coll_av <- droplevels(coll_av2)

res.fried_coll_MIB <- coll_av2 %>%
  dplyr::group_by(catchment) %>%
  friedman_test(mean_MIB ~ month|code_site)

# now merge the results together and save output

Fried_results_table_2 <- bind_rows(res.fried_avon_MIB, res.fried_coll_MIB, res.fried_tavy_MIB, res.fried_tav_sp_MIB)
Fried_results_table_2$Campaign_type <- c("regular", "spatial", "regular", "spatial")          
write.csv(Fried_results_table_2, file="Data_analysis/UsT3_lab_data/csv_output/ALS/Friedman_test_results_MIB.csv")




################################################################################

# let's do a post hoc test to see which ones are different!

source("Data_analysis/UsT3_lab_data/FUNCTIONS_statistical_analysis.R")

# test with post hoc for Avon 
formu <- mean_Geo ~ code_site|month
post_hoc_Avon<- friedman.test.with.post.hoc(formu, Avon)
post_hoc_Avon2 <- as.data.frame(post_hoc_Avon[[2]])

# test with post hoc for Tavy 
post_hoc_Tavy<- friedman.test.with.post.hoc(formu, Tavy)
post_hoc_Tavy2 <- as.data.frame(post_hoc_Tavy[[2]])

# compile and save for later

post_hoc_tests_results <- bind_rows(post_hoc_Avon2, post_hoc_Tavy2)
post_hoc_tests_results <- post_hoc_tests_results %>% 
  rename("P Value" = V1)
       
write.csv(post_hoc_tests_results, file="Data_analysis/UsT3_lab_data/csv_output/ALS/post_hoc_tests_results.csv")

# No post hoc test necessary for MIB as none of them are statistically significan!




# detach the package otherwise it causes problems with other data analysis
detach(package:coin) # do this if I am doing something else now!


#### Let's do a Wilcox test to check the difference between spatial campaigns

# Wilcoxon test to test whether spatial campaing #1 is different from #2

#Geosmin first -----------

test_GEo <- Spatial %>% 
  select(catchment, month, Geosmin_ng_l)   


Wilcox_Geo <- test_GEo %>%
  group_by(catchment) %>%
  do(w = wilcox.test(Geosmin_ng_l ~ month, data=., paired=TRUE)) %>% 
  summarise(catchment, Wilcox = w$p.value)
Wilcox_Geo$qualif <- "Geosmin"                                                        # Add a qualif to the result table before merging

#Then MIB -----------

test_MIB <- Spatial %>% 
  select(catchment, month, MIB_ng_l)   

Wilcox_MIB <- test_MIB %>%
  group_by(catchment) %>%
  do(w = wilcox.test(MIB_ng_l ~ month, data=., paired=TRUE)) %>% 
  summarise(catchment, Wilcox = w$p.value)
Wilcox_MIB$qualif <- "MIB"                                                        # Add a qualif to the result table before merging

Wilcox <- bind_rows(Wilcox_Geo, Wilcox_MIB)
write.csv(Wilcox, file="csv_output/ALS/WilcoxTest_spatial_campaigns.csv")
rm(Wilcox_Geo, Wilcox_MIB)





#################################################################################
################################################################################

