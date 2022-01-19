#======================== Script  Information ==================================
#Created By AMDeines on 2019_07_17
#Loads common pacakges and helper functions, in particular for plotting and P&C watermarks
#Can be run as source
#this file should be added (and modified as needed to a given project)

#Standard libraries (needed to make this script run)
library(tidyverse) # plotting
library(lubridate) # Date formating
#library(extrafont) #https://github.com/wch/extrafont

#default timezone
TZ<-"America/Los_Angeles"


#Creates basic ggplot theme with blank background, black facet strips with white
#letters, Arial font, legend at bottom
expTheme<-theme_bw()+theme(panel.grid=element_blank(),
                          #text=element_text(family="Arial"),
                          legend.position="bottom",
                          strip.background=element_rect(fill="black"),
                          strip.text = element_text(colour = 'white',size=9,face="bold"),
                          axis.title=element_text(size=14,face="bold"),
                          axis.text = element_text(size = 12),  #axis text=9pt  
                          legend.text=element_text(size = 9), 
                          legend.title=element_text(size = 11),
                          panel.border = element_rect(fill = NA, 
                            colour = "grey50",size=.3),
                          axis.ticks = element_line(size = .2)
                           )#updated 20200117

#The standard text text of a confidential watermark 
pandCText = c("DRAFT-Confidential")


#A function that formats the P&CText to be added figure to a figure, including
#the name of the script from which it came and an identifier for the figure Note
#that this "new" P&C bit is a function that variably add the script name & figID
expPandC <- function(scpt=NULL,figID=NULL,is_date=NULL){
  #is_date should be a numeric of the same type as the x axis 
  XPOS<-c(-Inf,Inf)
  if(!is.null(is_date)) XPOS<-is_date
  data.frame(
    xpos = XPOS,
    ypos =  c(Inf,Inf),
    annotateText = c(pandCText,paste(scpt,Sys.Date(),figID,"  ")),
    hjustvar = c(0,1) ,
    vjustvar = c(1,1))
}

#Function that actually added the P&C watermark
expAnnotate<-function(...){
  geom_text(data = expPandC(...) , aes(x=xpos,y=ypos,hjust=hjustvar,
                vjust=vjustvar,label=annotateText),inherit.aes = FALSE,size = 2,
                color="grey")
}
# expAnnotate(scpt=NULL,figID=NULL,is_date=NULL)

# Example
ggplot(data=data.frame(x=1:100,y=rnorm(100)),aes(x,y))+
  geom_point()+
  expAnnotate(scpt="a script name or other text",
              figID="a figure ID or other text",is_date=NULL)+
  expTheme




#function that makes pretier log10 breaks on ggplot y axis 
log10_breaks <- function(n = 5){
    function(x) {
        10^pretty(log10(x), n = n)
    }
}
#see https://stackoverflow.com/questions/18479784/improve-poor-automatic-tick-position-choices-without-explicitly-specifying-break


#function to give the default ggplot color palette, if it's ever needed.
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
#https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette


#Note that for multipanel figures the prefered style is top left lower case in (a)


# A function to convert a date vector into factors, while maintaining the correct order  
date_to_factor<-function(date_seq,format="%Y%m%d"){
  df<-data.frame(dates=unique(date_seq))
  df$label<-format(df$dates,format=format)
  df$order<-order(df$dates)
  df$factor<-factor(df$label,levels=df$label[df$order],ordered=TRUE)
  df$factor[match(date_seq,df$dates)]
}
