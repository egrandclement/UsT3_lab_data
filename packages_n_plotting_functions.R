
# This lists all the packages and plotting functions needed to open the files 
# and make plots needed. 
# It can be used in conjunction with reports for basic stats.

# Packages
library(readr)
#library(plyr)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(unpivotr) # for dealing with the SWW spot data
if(!require('lfstat')) install.packages('lfstat'); library('lfstat')
library(cowplot)
library(readxl)
library(reshape2)
library(tidyverse)
library(rstatix)
library(zoo)
library(scales)  # Load the scales package for pretty_breaks
# Plotting functions
R_grad_UsT <- colorRampPalette(c(
  rgb(229, 109, 11, max=255),   
  rgb(248, 171, 16, max=255),   
  rgb(253, 231, 37, max=255), 
  rgb(180, 222, 80, max=255), 
  rgb(108, 179, 63, max=255),  
  rgb(102, 158, 144, max=255),  
  rgb(0, 132, 145, max=255),  
  rgb(0, 104, 139, max=255)))

# latest colour palette designed by JA (6/3/23)

## UsT gradient: e.g. wet (blue)  to dry (yellow) 

grad_UsT <- colorRampPalette(c(rgb(11, 20, 41, max=255), 
                               rgb(32, 40, 84, max=255),    #darkest bleu (Mires) 
                               rgb(0, 104, 139, max=255),   #deepskyblue4 (Mires)
                               rgb(0, 132, 145, max=255),   #teal (Mires / UsT)
                               rgb(0, 168, 126, max=255),   #Exeter rich deep green
                               rgb(108, 179, 63, max=255),  #bright green (UsT)
                               rgb(180, 222, 80, max=255),  #grad light green (not UsT, tweeked)
                               rgb(253, 231, 37, max=255),  #yellow (not UsT) 
                               rgb(255, 200, 20, max=255)))#,  #very light orange 



grad_UsT1 <- colorRampPalette(c(#rgb(11, 20, 41, max=255),
                               rgb(32, 40, 84, max=255),   #darkest bleu (Mires)
                               rgb(0, 104, 139, max=255),   #deepskyblue4 (Mires)
                               rgb(0, 132, 145, max=255),   #teal (Mires / UsT)
                               rgb(102, 158, 144, max=255), #greeny teal (UsT)
                               rgb(108, 179, 63, max=255),  #bright green (UsT)
                               rgb(180, 222, 80, max=255),  #grad light green (not UsT, tweeked)
                               rgb(253, 231, 37, max=255),  #yellow (not UsT)
                               rgb(255, 200, 20, max=255)))#,  #very light orange


pal_UsT <- c(rgb(11, 20, 41, max=255),
             rgb(32, 40, 84, max=255),    #darkest bleu (Mires)
             rgb(0, 104, 139, max=255),   #deepskyblue4 (Mires)
             rgb(0, 132, 145, max=255),   #teal (Mires / UsT)
             rgb(102, 158, 144, max=255), #greeny teal (UsT)
             rgb(108, 179, 63, max=255),  #bright green (UsT)
             rgb(180, 222, 80, max=255),  #grad light green (not UsT, tweeked)
             rgb(253, 231, 37, max=255),  #yellow (not UsT)
             rgb(255, 200, 20, max= 255))  #,  #very light orange


plot(rep(1,10),col=(pal_UsT),pch=15,cex=4)







coloursUST <- c("#007A8F", "#FFC814", "#1D254F", "#C4E046", "#389290", "#68A76B", "#FDE323", "#07597E", "#83C144", "#0B1429", "#FFBF00", "#6CB33F" )
plot(rep(1,12),col=(coloursUST), pch=15,cex=4)

# df2 <- data.frame(supp=rep(c("A", "B", "C"), each=3),
                  #dose=rep(c("X", "Y", "Z"),3),
                  #len=c(6.8, 15, 33, 4.2, 10, 29.5, 10, 6.4, 2.1))

#head(df2)

#ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  #geom_bar(stat="identity", position=position_dodge())+ 
  #scale_fill_manual(values=coloursUST)




#pal_UsT <- colorRampPalette(c(rgb(0, 104, 139, max=255),   #darkskyblue4 (Mires) - "#00688B"
             #rgb(248, 171, 16, max=255),  #light orange (Mires) - Hex #F8AB10
            #rgb(102, 158, 144, max=255), #greeny teal (UsT) - "#669E90" 
             #rgb(198, 229, 232, max=255), #lightblue (UsT) - "#C6E5E8"
             #rgb(108, 179, 63, max=255),  #bright green (UsT) - "#6CB33F"
             #rgb(181, 203, 141, max=255), #light green (UsT) - Hex #B5CB8D
             #rgb(0, 132, 145, max=255)))   #teal (Mires / UsT) - "#008491"

theme_set(theme_bw(base_size = 9))
theme(panel.grid.minor = element_line(colour="grey"))
update_geom_defaults("line", list(size = 0.2))

theme_set(theme_bw() + theme(#panel.grid.major = element_blank(), 
                             #panel.grid.minor = element_blank(),
                             axis.text = element_text(size = 8),
                             axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, colour = "gray28", margin = margin(t=5)),
                             axis.text.y = element_text(angle = 0, vjust = 0.5, colour = "gray28", margin = margin(r=5)),
                             axis.title.x = element_text(size = 8, margin = margin(t=5)),
                             axis.title.y = element_text(size = 8, margin = margin(r=5)),
                             panel.grid = element_blank(),
                             plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), units = , "cm"),
                             plot.title = element_text(size = 8, vjust = 1, hjust = 0.5),
                             legend.title = element_text(size = 8),
                             legend.text = element_text(size = 8),
                             legend.key.size = unit(1,"line"),
                             legend.position = c(0.85, 0.9)))


theme_coding <- function(){
  theme_bw()+
    theme(axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.text.y = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.title = element_text(size = 12),
          #panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 11, vjust = 1, hjust = 0.5),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1,"line"),
          legend.position = c(0.9, 0.9))
}

# ordered sites used for routine monitoring (schemes catchments)

Routine_sites <- c("BSB", "BSA", "BSE", "BSL", "BNC", "BSN", "BHW", "BLB", "BRS",
                   "RMM","RWB", "RJW", "RBC", "RRW", "RNE", "RED", "RES", "RRS",
                   "STW", "STS","SNS","SPS","SLS","SRS",
                   "WRB", "WES","WNS", "WLS","WRS")


# ordered sites used for spatial monitoring (including routine)

Spatial_sites <- c("BSB","BPD","BPE","BSC","BSA","BPF","BSD","BSI","BSE", 
                   "BSF","BSG","BPA","BPB","BSK","BSJ","BSH","BSL","BPC",
                   "BSM","BPG","BNC","BLW", "BMH","BMW","BSN","BSO","BSP",
                   "BSQ","BSR","BSS","BPH", "BST","BSU","BLB","BSZ","BSV",
                   "BSW","BSX","BSY","BHW", "BRS",
                   
                   "WBC","WBW","WSF","WBF","WBE","WRB","WBB","WES","WET","WNS",
                   "WLS","WRH","WRP","WRS","WBD")





# Data analysis functions

FUN.season <- function(date_time){
  
  ## seasons vector
  season <- ifelse(as.numeric(format(date_time, "%m")) %in% c(10, 11, 12), "Winter (OND)",
                   ifelse(as.numeric(format(date_time, "%m")) %in% c(1, 2, 3), "Winter (JFM)",
                          ifelse(as.numeric(format(date_time, "%m")) %in% c(4, 5, 6), "Summer (AMJ)",
                                 ifelse(as.numeric(format(date_time, "%m")) %in% c(7, 8, 9), "Summer (JAS)",
                                        NA))))
  
  season_f <- factor(season, levels=c("Winter (OND)","Winter (JFM)","Summer (AMJ)","Summer (JAS)"))
  
  return(season_f)
}


FUN.season2 <- function(date_time){
  
  ## seasons vector
  season <- ifelse(as.numeric(format(date_time, "%m")) %in% c(10, 11, 12), "Autumn (OND)",
                   ifelse(as.numeric(format(date_time, "%m")) %in% c(1, 2, 3), "Winter (JFM)",
                          ifelse(as.numeric(format(date_time, "%m")) %in% c(4, 5, 6), "Spring (AMJ)",
                                 ifelse(as.numeric(format(date_time, "%m")) %in% c(7, 8, 9), "Summer (JAS)",
                                        NA))))
  
  season_f <- factor(season, levels=c("Winter (OND)","Winter (JFM)","Summer (AMJ)","Summer (JAS)"))
  
  return(season_f)
}



# boxplots
# needs defining the following:

#Ylab <- expression(paste(Geosmin~(ug/l))) 
#legnd <- c("Geosmin", "Catchment", Ylab)
#max <- max(geo$Geosmin_ug_l)
#LOD <- max(geo$Geosmin_LOD_val)

ggBoxplot <- function(DAT, x, y ){
  
  ggplot (DAT, aes(x, y))+   # this enables to use strings... don't ask - it works
    geom_boxplot(outlier.size = 1, fill= "#007A8F") + 
    theme_coding() + 
    xlab(legnd[2]) +
    ylab (Ylab) +
    theme(legend.position = "none") +
    labs(title = legnd [1]) +
    theme(legend.position = "none",
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 14, vjust = 1, hjust = 0))+
    scale_fill_discrete () +
    scale_y_continuous (limits =c(0,max+1),
                        breaks = seq(0,max+1, by=2))
  #theme(axis.title.x = element_blank()

} 

ggScatter <- function(DAT, x, y1, y2, y3) {
    
    ggplot(DAT) +
      #geom_line(aes(x, y1, colour = legnd[2]))+
      #geom_line(aes(x, y2, colour = legnd[3])) +
      geom_point(aes(x, y3, colour = legnd[4])) +
      xlab (legnd[1]) +
      ylab (legnd[2]) +
      scale_x_datetime(limits = c(lims),                             # get the x axis limits previously set
                       date_labels = ("%Y-%m-%d")) +
      theme_coding()+
      #theme_bw(base_size = 8) + theme(legend.key = element_blank(),
      #legend.title=element_blank(),
      #legend.position = "bottom") +                
      scale_colour_manual(values= c("grey","deepskyblue4", "chartreuse4", "darkorange4"))        # the order applies to the first one in the df - might need changing
    #+
    #scale_linetype_manual(values= c("solid"
    #,"longdash")
    #))
  }


# Function to add n to boxpolot
stat_box_data <- function(y) {
  return( 
    data.frame(
      y = max(y)*1.1,    
      #x = 1 ,# may need to modify this depending on location
      label = paste('n =', length(y), '\n'
                    #,
                    #'mean =', round(mean(y), 1), '\n'
                    )
    )
  )
}

# Function to add n to boxplot with increased spacing
stat_box_data <- function(y) {
  # Increase spacing by adjusting the multiplier factor
  spacing_factor <- 1.2  # Adjust this value to increase/decrease spacing
  return(data.frame(
    y = max(y) * spacing_factor,  # Apply increased spacing
    label = paste('n =', length(y), '\n')
  ))
}


