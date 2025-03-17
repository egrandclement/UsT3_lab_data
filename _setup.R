##### TITLE: _setup #########################################################

# Author: JAshe - amended by EGC 4/4/2023
# Created on: 07/10/2021
# Project: UsT3

##### CODE #####################################################################

#---- Packages in use ----------------------------------------------------------

#load / install+load 
if(!require(readr)) {
  install.packages("readr"); require(readr)} #reading in data
if(!require(dplyr)) {
  install.packages("dplyr"); require(dplyr)} #data wrangleing
if(!require(lubridate)) {
  install.packages("lubridate"); require(lubridate)} #working with dates times
if(!require(purrr)) {
  install.packages("purrr"); require(purrr)}  #applying functions across lists
if(!require(stringr)) {
  install.packages("stringr"); require(stringr)}  #for pulling out strings
if(!require(fs)) {
  install.packages("fs"); require(fs)}  # tidyverse file system operations
if(!require(readxl)) {
  install.packages("readxl"); require(readxl)}  # tidyverse read excel
if(!require(tidyr)) {
  install.packages("tidyr"); require(tidyr)}  # tidy
if(!require(tibble)) {
  install.packages("tibble"); require(tibble)}  # tibble
if(!require(unpivotr)) {
  install.packages("unpivotr"); require(unpivotr)}  # for dealing with the SWW spot data (PB SWWspot wrangling)

if(!require(rlang)) {
  install.packages("rlang"); require(rlang)}  # ggplot and functions {{ }} resolves diff in quote column names

if(!require(ggplot2)) {
  install.packages("ggplot2"); require(ggplot2)}  #load / install+load ggplot2
if(!require(plotly)) {
  install.packages("plotly"); require(plotly)}  #load / install+load plotly
if(!require(cowplot)) {
  install.packages("cowplot"); require(cowplot)}  # grid plot
if(!require(htmlwidgets)) {
  install.packages("htmlwidgets"); require(htmlwidgets)}  # grid plot

if(!require(colorblindcheck)) {
  install.packages("colorblindcheck"); require(colorblindcheck)}  # grid plot


#---- Plot settings ------------------------------------------------------------

theme_set(theme_bw(base_size = 9))
theme(panel.grid.minor = element_line(colour="grey"))
update_geom_defaults("line", list(size = 0.2))

# tested with https://www.color-blindness.com/coblis-color-blindness-simulator/
grad_UsT <- colorRampPalette(c(rgb(11, 20, 41, max=255),
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
                               rgb(255, 200, 20, max=255))  #,  #very light orange

seasonUsT <- c("#016588",
"#3C9390",
"#9FD14B",
"#FFC814")

# as grad
plot(rep(1,50),col=(grad_UsT(50)), pch=15,cex=4)

# as seasons
plot(rep(1,4),col=(grad_UsT(4)), pch=15,cex=4)

coloursUST <- c("#389290", "#FFC814", "#07597E", "#C4E046", "#007A8F", "#68A76B", "#FDE323", "#1D254F", "#83C144", "#0B1429", "#FFBF00", "#6CB33F" )
plot(rep(1,12),col=(coloursUST), pch=15,cex=4)
colourUST <- c("#389290", "#FFC814", "#1D254F", "#C4E046", "#007A8F", "#68A76B", "#FDE323", "#07597E", "#83C144", "#0B1429", "#FFBF00", "#6CB33F" )
plot(rep(1,12),col=(colourUST), pch=15,cex=4)



theme_coding <- function(){
  theme_bw()+
    theme(axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.text.y = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.title = element_text(size = 12),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 11, vjust = 1, hjust = 0.5),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1,"line"),
          legend.position = c(0.9, 0.9))
}




# Functions to be used ---------------------------------------------------------


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

FUN.day_wateryear <- function(date_time){
  
  ## seasons vector
  day_wateryear <- ifelse(yday(date_time) >= 275 &
                            leap_year(date_time) == TRUE,
                          yday(date_time)- 274,
                          
                          ifelse(yday(date_time) >= 275 &
                                   leap_year(date_time) == FALSE,
                                 yday(date_time)- 273,
                                 
                                 yday(date_time) + 92))
  
  return(day_wateryear)
  
}
FUN.wateryear <- function(date_time){
  
  wateryear <- ifelse((date_time >= as.POSIXct(paste0(lubridate::year(date_time), "-10-01 00:00:00"), tz = "UTC") &
                         date_time <  as.POSIXct(paste0(lubridate::year(date_time)+1, "-01-01 00:00:00"), tz = "UTC")),
                      as.character (paste0(year(date_time), "-", year(date_time)+1)),
                      
                      ifelse((date_time >= as.POSIXct(paste0(lubridate::year(date_time), "-01-01 00:00:00"), tz = "UTC") &
                                date_time <  as.POSIXct(paste0(lubridate::year(date_time)+1, "-10-01 00:00:00"), tz = "UTC")),
                             as.character (paste0(year(date_time)-1, "-", year(date_time))),NA))    
  
  return(wateryear)
}


#dat$wateryear_f <- factor(dat$wateryear)

FUN.HYmonth <- function(date_time){
  
  levels(HYmonth) <- c("Oct",    
                       "Nov",    
                       "Dec",      
                       "Jan",     
                       "Feb",    
                       "Mar",  
                       "Apr",
                       "May",
                       "Jun",      
                       "Jul",     
                       "Aug",    
                       "Sep")  
  
  # seasons vector
  HYmonth <- ifelse(format(date_time, "%m")==10, levels(HYmonth)[1],
                    ifelse(format(date_time, "%m")==11, levels(HYmonth)[2],
                           ifelse(format(date_time, "%m")==12, levels(HYmonth)[3],
                                  ifelse(format(date_time, "%m")==1, levels(HYmonth)[4],
                                         ifelse(format(date_time, "%m")==2, levels(HYmonth)[5],
                                                ifelse(format(date_time, "%m")==3, levels(HYmonth)[6],
                                                       ifelse(format(date_time, "%m")==4, levels(HYmonth)[7],
                                                              ifelse(format(date_time, "%m")==5, levels(HYmonth)[8],
                                                                     ifelse(format(date_time, "%m")==6, levels(HYmonth)[9],
                                                                            ifelse(format(date_time, "%m")==7, levels(HYmonth)[10],
                                                                                   ifelse(format(date_time, "%m")==8, levels(HYmonth)[11],
                                                                                          ifelse(format(date_time, "%m")==9, levels(HYmonth)[12],NA))))))))))))
  
  # HYmonth <- ifelse(format(date_time, "%m")>=10 & format(date_time, "%m")<=12, levels(HYmonth)[c(as.numeric(format(date_time, "%m"))-9)],
  #                   ifelse(format(date_time, "%m")>=1 & format(date_time, "%m")<=9, levels(HYmonth)[c(as.numeric(format(date_time, "%m"))+3)],NA))
  
  
  return(HYmonth)
  
}



##### END ######################################################################


