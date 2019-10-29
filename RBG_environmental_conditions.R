#Required packages
install.packages("gridExtra")
library(lubridate)
library(questionr)
library(LearnGeom)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")
library(cowplot)
library(gridExtra)

#Import required datasets.
  ##Import the climate data bases.
  ##In those files, "999" stands for NA values.
  ##In those files, the first column contains the [year-month-day hour:minute] time
  ##associated with each recorded measure.
  ##In those files, one column contains all the measured recorded by one weather station.
  ##Those colums are named after corresponding stations.
  
  ##Set the working directory
  setwd("")
  ###Temperature data
  temp<-read.table("Temperature_2015_2017.csv", sep = ";", header=TRUE)
  ####Transform the first column (called "Chrono" here) into a date-time object.
  temp$Chrono2<-ymd_hm(temp$Chrono)
  ###Humidity data
  hum<-read.table("Humidity_2015_2017.csv", sep = ";", header=TRUE)
  hum$Chrono2<-ymd_hm(hum$Chrono)
  ###Wind direction data
  wdir<-read.table("Wind_direction_2015_2017.csv", sep = ";", header=TRUE)
  wdir$Chrono2<-ymd_hm(wdir$Chrono)
  ###Wind speed data
  wspeed<-read.table("Wind_speed_2015_2017.csv", sep = ";", header=TRUE)
  wspeed$Chrono2<-ymd_hm(wspeed$Chrono)
  ##Atmosperic pressure data
  pres<-read.table("Pressure_2015_2017.csv", sep = ";", header=TRUE)
  pres$Chrono2<-ymd_hm(pres$Chrono)
  ##Particulate matter data
  PM<-read.table("PM_2015_2017.csv", sep = ";", header=TRUE)
  PM$Chrono2<-ymd_hm(PM$Date.Heure)

  ##Import the deployment file (dates and hours of deployment and recapture of tracked gulls).
  ##This files contains six columns, one for the individual identification, one for its sex,
  ##one for the date of departure, one for the hour of departure,
  ##one for the date of recapture, and one for the hour of recapture.
  
  setwd("")
  Dep<-read.table("liste_GPS.csv", sep = ";", header=TRUE)
  Dep$Chrono_dep<-ymd_hms(paste(Dep$Date_dep, Dep$Heure_dep))
  Dep$Chrono_rec<-ymd_hms(paste(Dep$Date_rec, Dep$Heure_rec))

  ##Import the home ranges estimates.
  ##This file is obtained through home range estimation performed with adehabitatLT
  ##and adehabitatHR packages.
  ##In this file, each column contains the utilization distribution of one individual
  ##for each pixel of the study area.The last columns descibe the latitude (Northing),
  ##the longitude (Easting), and the identification of each pixel.
  UDvol<-read.table("GBC67_Vol_g1000.csv", sep = ";", header=TRUE)

  ##Import the file storing the closest stations and waste management facilities (WMFs)
  ##from each grid pixel. In this file, there must be two columns for the latitude and 
  ##the longitude of each pixel, one column for the identification of the closest WMF 
  ##(in this case there were several types of WMFs) and one column for the distance between 
  ##the pixel and the closest WMF.
  ##This file was obtained using ArcGIS, with the analysis grid (pixels canevas) overlapping
  ##maped WMFs in the study area.
  grid_sources_stations<-read.table("grid_sources_stations.csv", sep = ";", header=TRUE)
  
  ##Import the file storing WMFs attributes such as coordinates and the closest weather stations.
  WMF_attributes<-read.table("All_WMF_attributes.csv", sep = ";", header=TRUE)

#Create a file where each pixel is linked to the attributes (distance and coordinates0 of the closet WMFs.
grid_sources0<-grid_sources_stations[, c("Northing",
                                           "Easting",
                                           "near_AW",
                                           "AW_dist",
                                           "near_CW",
                                           "CW_dist",
                                           "near_eW",
                                           "eW_dist",
                                           "near_landfill",
                                           "landfill_dist",
                                           "near_mixt",
                                           "mixt_dist",
                                           "near_WW",
                                           "WW_dist",
                                           "near_eW_CW",
                                           "eW_CW_dist")]
  
colnames(grid_sources0)<-c("Pix_N", "Pix_E", 
                             "Auto_waste_fid","Auto_waste_dist",
                             "Const_waste_fid", "Const_waste_dist",
                             "eWaste_fid", "eWaste_dist",
                             "Landfill_fid", "Landfill_dist",
                             "Mixt_waste_fid","Mixt_waste_dist",
                             "Wastewater_fid","Wastewater_dist",
                             "eW_CW_fid", "eW_CW_dist")
  
WMF_coord<-WMF_attributes[,c("Waste_type", "fid", "Northing", "Easting", "Station_SM", "Station_PM")]

Auto_waste_coord<-subset(WMF_coord, Waste_type=="Auto_waste")
colnames(Auto_waste_coord)<-c("Waste_type", "Auto_waste_fid", "Auto_waste_N", "Auto_waste_E", "Station_SM", "Station_PM")
  
Const_waste_coord<-subset(WMF_coord, Waste_type=="Const_waste")
colnames(Const_waste_coord)<-c("Waste_type", "Const_waste_fid", "Const_waste_N", "Const_waste_E", "Station_SM", "Station_PM")
  
eWaste_coord<-subset(WMF_coord, Waste_type=="eWaste")
colnames(eWaste_coord)<-c("Waste_type", "eWaste_fid", "eWaste_N", "eWaste_E", "Station_SM", "Station_PM")
  
Landfill_coord<-subset(WMF_coord, Waste_type=="Landfill")
colnames(Landfill_coord)<-c("Waste_type", "Landfill_fid", "Landfill_N", "Landfill_E","Station_SM", "Station_PM")
  
Mixt_waste_coord<-subset(WMF_coord, Waste_type=="Mixt_waste")
colnames(Mixt_waste_coord)<-c("Waste_type", "Mixt_waste_fid", "Mixt_waste_N", "Mixt_waste_E","Station_SM", "Station_PM")
  
Wastewater_coord<-subset(WMF_coord, Waste_type=="Wastewater")
colnames(Wastewater_coord)<-c("Waste_type", "Wastewater_fid", "Wastewater_N", "Wastewater_E","Station_SM", "Station_PM")

Const_waste_coord2<-subset(WMF_coord, Waste_type=="Const_waste")
eWaste_coord2<-subset(WMF_coord, Waste_type=="eWaste")
eW_CW_coord<-rbind(Const_waste_coord2, eWaste_coord2)
colnames(eW_CW_coord)<-c("Waste_type", "eW_CW_fid", "eW_CW_N", "eW_CW_E","Station_SM", "Station_PM")
eW_CW_coord$eW_CW_fid<-c(1:20)

grid_sources1<-merge(grid_sources0, Auto_waste_coord[,-c(1,5,6)], by="Auto_waste_fid")
grid_sources2<-merge(grid_sources1, Const_waste_coord[,-c(1,5,6)], by="Const_waste_fid")
grid_sources3<-merge(grid_sources2, eWaste_coord[,-c(1,5,6)], by="eWaste_fid")
grid_sources4<-merge(grid_sources3, Landfill_coord[,-c(1,5,6)], by="Landfill_fid")
grid_sources5<-merge(grid_sources4, Mixt_waste_coord[,-c(1,5,6)], by="Mixt_waste_fid")
grid_sources6<-merge(grid_sources5, Wastewater_coord[,-c(1,5,6)], by="Wastewater_fid")
grid_sources7<-merge(grid_sources6, eW_CW_coord[,-c(1,5,6)], by="eW_CW_fid")
grid_sources<-grid_sources7[,c("Pix_N", "Pix_E",
                               "Auto_waste_fid", "Auto_waste_dist","Auto_waste_N", "Auto_waste_E",
                               "Const_waste_fid", "Const_waste_dist", "Const_waste_N", "Const_waste_E",
                               "eWaste_fid", "eWaste_dist","eWaste_N", "eWaste_E",
                               "Landfill_fid", "Landfill_dist", "Landfill_N", "Landfill_E",
                               "Mixt_waste_fid","Mixt_waste_dist", "Mixt_waste_N", "Mixt_waste_E",
                               "Wastewater_fid","Wastewater_dist", "Wastewater_N", "Wastewater_E",
                               "eW_CW_fid","eW_CW_dist", "eW_CW_N", "eW_CW_E")]

#Calculate the angle between WMF-North axis and WMF-Pixel axis (Angle1).
  ##Create an upnorth point for each WMF to create the third point of an angle
  grid_sources$Up_Auto_waste_N<-grid_sources$Auto_waste_N + 1000
  grid_sources$Up_Auto_waste_E<-grid_sources$Auto_waste_E
  
  grid_sources$Up_Const_waste_N<-grid_sources$Const_waste_N + 1000
  grid_sources$Up_Const_waste_E<-grid_sources$Const_waste_E
  
  grid_sources$Up_eWaste_N<-grid_sources$eWaste_N + 1000
  grid_sources$Up_eWaste_E<-grid_sources$eWaste_E
  
  grid_sources$Up_Landfill_N<-grid_sources$Landfill_N + 1000
  grid_sources$Up_Landfill_E<-grid_sources$Landfill_E
  
  grid_sources$Up_Mixt_waste_N<-grid_sources$Mixt_waste_N + 1000
  grid_sources$Up_Mixt_waste_E<-grid_sources$Mixt_waste_E
  
  grid_sources$Up_Wastewater_N<-grid_sources$Wastewater_N + 1000
  grid_sources$Up_Wastewater_E<-grid_sources$Wastewater_E
  
  grid_sources$Up_eW_CW_N<-grid_sources$eW_CW_N + 1000
  grid_sources$Up_eW_CW_E<-grid_sources$eW_CW_E
  
  ##Calculate Angle1 
  ##Angle function only calculate absolute value of angles (between 0 and 180 degrees). 
  ##Here, we want to measure full revolution from the North axis.
  ##Important to type easting before northing in the coordinates suite.
  ##Angle are rounded to tens of degrees.
    ### Function to calculate Angle1 for Auto_waste sites
    Auto_waste_Angle1_f<-function (x){
      pt1_E<-grid_sources[x,"Up_Auto_waste_E"]
      pt1_N<-grid_sources[x,"Up_Auto_waste_N"]
      pt1<-c(pt1_E, pt1_N)
      pt2_E<-grid_sources[x,"Auto_waste_E"]
      pt2_N<-grid_sources[x,"Auto_waste_N"]
      pt2<-c(pt2_E, pt2_N)
      pt3_E<-grid_sources[x,"Pix_E"]
      pt3_N<-grid_sources[x,"Pix_N"]
      pt3<-c(pt3_E, pt3_N)
       if (pt3_E>pt2_E){
         Angle1<-Angle(pt1, pt2, pt3)
       } else{
         Angle1<-360-(Angle(pt1, pt2, pt3))
       }
      
      return (ceiling(Angle1/10)*10)
    }
    grid_list<-c(1:nrow(grid_sources))
    grid_sources$Auto_waste_Angle1<-sapply(grid_list, Auto_waste_Angle1_f)
    
    ### Function to calculate Angle1 for Const_waste sites
    Const_waste_Angle1_f<-function (x){
      pt1_E<-grid_sources[x,"Up_Const_waste_E"]
      pt1_N<-grid_sources[x,"Up_Const_waste_N"]
      pt1<-c(pt1_E, pt1_N)
      pt2_E<-grid_sources[x,"Const_waste_E"]
      pt2_N<-grid_sources[x,"Const_waste_N"]
      pt2<-c(pt2_E, pt2_N)
      pt3_E<-grid_sources[x,"Pix_E"]
      pt3_N<-grid_sources[x,"Pix_N"]
      pt3<-c(pt3_E, pt3_N)
      
      if (pt3_E>pt2_E){
        Angle1<-Angle(pt1, pt2, pt3)
      } else{
        Angle1<-360-(Angle(pt1, pt2, pt3))
      }
      
      return (ceiling(Angle1/10)*10)
    }
    grid_sources$Const_waste_Angle1<-sapply(grid_list, Const_waste_Angle1_f)
  
    ### Function to calculate Angle1 for eWaste sites
    eWaste_Angle1_f<-function (x){
      pt1_E<-grid_sources[x,"Up_eWaste_E"]
      pt1_N<-grid_sources[x,"Up_eWaste_N"]
      pt1<-c(pt1_E, pt1_N)
      pt2_E<-grid_sources[x,"eWaste_E"]
      pt2_N<-grid_sources[x,"eWaste_N"]
      pt2<-c(pt2_E, pt2_N)
      pt3_E<-grid_sources[x,"Pix_E"]
      pt3_N<-grid_sources[x,"Pix_N"]
      pt3<-c(pt3_E, pt3_N)
      
      if (pt3_E>pt2_E){
        Angle1<-Angle(pt1, pt2, pt3)
      } else{
        Angle1<-360-(Angle(pt1, pt2, pt3))
      }
      
      return (ceiling(Angle1/10)*10)
    }
    grid_sources$eWaste_Angle1<-sapply(grid_list, eWaste_Angle1_f)
    
    ### Function to calculate Angle1 for Landfill sites
    Landfill_Angle1_f<-function (x){
      pt1_E<-grid_sources[x,"Up_Landfill_E"]
      pt1_N<-grid_sources[x,"Up_Landfill_N"]
      pt1<-c(pt1_E, pt1_N)
      pt2_E<-grid_sources[x,"Landfill_E"]
      pt2_N<-grid_sources[x,"Landfill_N"]
      pt2<-c(pt2_E, pt2_N)
      pt3_E<-grid_sources[x,"Pix_E"]
      pt3_N<-grid_sources[x,"Pix_N"]
      pt3<-c(pt3_E, pt3_N)
      
      if (pt3_E>pt2_E){
        Angle1<-Angle(pt1, pt2, pt3)
      } else{
        Angle1<-360-(Angle(pt1, pt2, pt3))
      }
      
      return (ceiling(Angle1/10)*10)
    }
    grid_sources$Landfill_Angle1<-sapply(grid_list, Landfill_Angle1_f)
    
    ### Function to calculate Angle1 for Mixt_waste sites
    Mixt_waste_Angle1_f<-function (x){
      pt1_E<-grid_sources[x,"Up_Mixt_waste_E"]
      pt1_N<-grid_sources[x,"Up_Mixt_waste_N"]
      pt1<-c(pt1_E, pt1_N)
      pt2_E<-grid_sources[x,"Mixt_waste_E"]
      pt2_N<-grid_sources[x,"Mixt_waste_N"]
      pt2<-c(pt2_E, pt2_N)
      pt3_E<-grid_sources[x,"Pix_E"]
      pt3_N<-grid_sources[x,"Pix_N"]
      pt3<-c(pt3_E, pt3_N)
      
      if (pt3_E>pt2_E){
        Angle1<-Angle(pt1, pt2, pt3)
      } else{
        Angle1<-360-(Angle(pt1, pt2, pt3))
      }
      
      return (ceiling(Angle1/10)*10)
    }
    grid_sources$Mixt_waste_Angle1<-sapply(grid_list, Mixt_waste_Angle1_f)
    
    ### Function to calculate Angle1 for Wastewater sites
    Wastewater_Angle1_f<-function (x){
      pt1_E<-grid_sources[x,"Up_Wastewater_E"]
      pt1_N<-grid_sources[x,"Up_Wastewater_N"]
      pt1<-c(pt1_E, pt1_N)
      pt2_E<-grid_sources[x,"Wastewater_E"]
      pt2_N<-grid_sources[x,"Wastewater_N"]
      pt2<-c(pt2_E, pt2_N)
      pt3_E<-grid_sources[x,"Pix_E"]
      pt3_N<-grid_sources[x,"Pix_N"]
      pt3<-c(pt3_E, pt3_N)
      
      if (pt3_E>pt2_E){
        Angle1<-Angle(pt1, pt2, pt3)
      } else{
        Angle1<-360-(Angle(pt1, pt2, pt3))
      }
      
      return (ceiling(Angle1/10)*10)
    }
    grid_sources$Wastewater_Angle1<-sapply(grid_list, Wastewater_Angle1_f)
    
    ### Function to calculate Angle1 for eW_CW sites
    eW_CW_Angle1_f<-function (x){
      pt1_E<-grid_sources[x,"Up_eW_CW_E"]
      pt1_N<-grid_sources[x,"Up_eW_CW_N"]
      pt1<-c(pt1_E, pt1_N)
      pt2_E<-grid_sources[x,"eW_CW_E"]
      pt2_N<-grid_sources[x,"eW_CW_N"]
      pt2<-c(pt2_E, pt2_N)
      pt3_E<-grid_sources[x,"Pix_E"]
      pt3_N<-grid_sources[x,"Pix_N"]
      pt3<-c(pt3_E, pt3_N)
      
      if (pt3_E>pt2_E){
        Angle1<-Angle(pt1, pt2, pt3)
      } else{
        Angle1<-360-(Angle(pt1, pt2, pt3))
      }
      
      return (ceiling(Angle1/10)*10)
    }
    grid_sources$eW_CW_Angle1<-sapply(grid_list, eW_CW_Angle1_f)
    
    
#Function to estimate average environmental condition for each gull.
mean_variable_ind<-function(i){

  ##i is the individual gull treated by this function
  ##Select the utilization distribution associated to this individual
  HR0<-UDvol[,c("Northing","Easting",i)]

  ##Select pixels for which UD<99
  HR1<-subset(HR0, HR0[,3]<99 )

  ##Calculate the presence probability
  HR1$fUD<-1/HR1[,3]/sum(1/HR1[,3])
  HR2<-merge(HR1, grid_sources_stations, by= c("Northing", "Easting"))
   pix_list<-c(1:nrow(HR2))
  departure<-Dep[i,"Chrono_dep"]
  recapture<-Dep[i,"Chrono_rec"]

  ##Function to estimate time and space integrated climate parameters in a pixel depending on the closer meteorological station
  mean_climate_pix<-function (p){
    ###p is the pixel treated by this function
    ##Pixel coordinates
    Northing<-HR2[p, "Northing"]
    Easting<- HR2[p, "Easting"]
    
    ###Closest stations
    Stat_Met<-as.character(HR2[p,"near_MS"])
    Stat_PM<-as.character(HR2[p,"near_PM"])
  
  
    ###Temperature
    ###Select the temperature data recorded by the closest station during the gull tracking.
    Temp_pix<-temp[,c("Chrono2",Stat_Met)]
    Temp_pix_dep0<-subset(Temp_pix, Chrono2>=departure & Chrono2 <=recapture)
    ###Get rid of NA values
    Temp_pix_dep<-Temp_pix_dep0[Temp_pix_dep0[,2]<999,]
    ###Calculate the mean temperature associated to the pixel p during the gull tracking.
    mTemp_pix_dep<-mean(Temp_pix_dep[,2])
  
    ###Humidity
    Hum_pix<-hum[,c("Chrono2",Stat_Met)]
    Hum_pix_dep0<-subset(Hum_pix, Chrono2>=departure & Chrono2 <=recapture)
    Hum_pix_dep<-Hum_pix_dep0[Hum_pix_dep0[,2]<999,]
    mHum_pix_dep<-mean(Hum_pix_dep[,2])
  
    ###Pressure
    Pres_pix<-pres[,c("Chrono2",Stat_Met)]
    Pres_pix_dep0<-subset(Pres_pix, Chrono2>=departure & Chrono2 <=recapture)
    Pres_pix_dep<-Pres_pix_dep0[Pres_pix_dep0[,2]<999,]
    mPres_pix_dep<-mean(Pres_pix_dep[,2])
  
    ###Wind speed
    Wspeed_pix<-wspeed[,c("Chrono2",Stat_Met)]
    Wspeed_pix_dep0<-subset(Wspeed_pix, Chrono2>=departure & Chrono2 <=recapture)
    Wspeed_pix_dep<-Wspeed_pix_dep0[Wspeed_pix_dep0[,2]<999,]
    mWspeed_pix_dep<-mean(Wspeed_pix_dep[,2])
  
    ###Main wind direction (Angle 3). The wind direction is expressed in tens of degrees in the original dataset.
    Wdir_pix<-wdir[,c("Chrono2",Stat_Met)]
    Wdir_pix_dep0<-subset(Wdir_pix, Chrono2>=departure & Chrono2 <=recapture)
    Wdir_pix_dep<-Wdir_pix_dep0[Wdir_pix_dep0[,2]<999,]
    ####Multiply the wind direction by ten
    Wdir_pix_dep$Angle3<-Wdir_pix_dep[,2]*10
    ####Convert polar angle to carthesian coordinates angle
    Wdir_pix_dep$cos_angle3<-cos(Wdir_pix_dep$Angle3*pi/180)
    Wdir_pix_dep$sin_angle3<-sin(Wdir_pix_dep$Angle3*pi/180)
    ####Calculate the mean of each coordinate
    mcos_angle3<-mean(Wdir_pix_dep$cos_angle3)
    msin_angle3<-mean(Wdir_pix_dep$sin_angle3)
    ####Calculate the coresponding polar angle
    rad<-atan2(msin_angle3,mcos_angle3)
    radTOdeg<-function(r){
      if(r>=0){
        deg<-r*180/pi
      }else{
        deg<-(r*180/pi)+360
      }
      return(deg)
    }
    mWdir_pix_dep<-radTOdeg(rad)

    ###Proportion of the sampling period when the pixel was exposed to the wind coming from a WMF
    ###A pixel is assumed to be exposed to the wind coming from a WMF when the angle between the WMF and the pixel (Angle1), the orientation of the wind at the WMF (Angle2) and the orientation of the wind on the pixel (Angle3) are equal.
       ####Auto_waste
        #####Wind orientation at a WMF (Angle2) recorded over the sampling period
        WMF_fid<-HR2[p,"near_AW"]
        Stat_Met_WMF<-as.character(Auto_waste_coord[Auto_waste_coord$Auto_waste_fid==WMF_fid,"Station_SM"])
        Wdir_WMF<-wdir[,c("Chrono2",Stat_Met_WMF)]
        Wdir_WMF_dep0<-subset(Wdir_WMF, Chrono2>=departure & Chrono2 <=recapture)
        Wdir_WMF_dep<-Wdir_WMF_dep0[Wdir_WMF_dep0[,2]<999,]
        downW_tab0<-merge(Wdir_pix_dep,Wdir_WMF_dep, by="Chrono2", all = FALSE)
        downW_tab0$Angle2<-downW_tab0[,6]*10
        downW_tab<- downW_tab0[,c("Chrono2", "Angle2", "Angle3")]
      
        #####Gather the measures of Angle 1, 2 and 3 along time
        pix_sources<-subset(grid_sources, Pix_N==Northing & Pix_E==Easting)
        Angle1_pix<-pix_sources$Auto_waste_Angle1
        downW_tab$Angle1<-rep(Angle1_pix, nrow(downW_tab))
        colnames(downW_tab)<-c("Chorno2","Angle3", "Angle2", "Angle1")
        head(downW_tab)
        #####Function to establish the geogaphic orientation quarter of an angle
        oriW_f<- function (X){
          if (X>=0 & X<=90){
            y<-45
          }else if(X>90 & X<=180){
            y<-135
          }else if(X>180 & X<=270){
            y<-225
          }else if(X>270 & X<=360){
            y<-315
          }
          return(y)
        }
        
        oriW_inv_f<- function (X){
          if (X>=0 & X<=90){
            y<-225
          }else if(X>90 & X<=180){
            y<-315
          }else if(X>180 & X<=270){
            y<-45
          }else if(X>270 & X<=360){
            y<-135
          }
          return(y)
        }
      
        #####Function to establish the equality/difference between Angle1, 2 and 3
        #####Important to take into acccount that the orientation recorded by weather stations expresses the direction where the wind comes FROM.
        #####Ex: 90 -> wind blowing FROM east and TOWARD west.
        #####It is necessary to invert the wind orientation (A2 and A3) to express the direction where the wind goes TOWARD.
        #####Otherwise, we can invert only A1.
        #####Also important to take into account that A2 is still expressed in tens of degrees.
        downW_f<- function (x){
          A1<-oriW_inv_f(downW_tab[x,"Angle1"])
          A2<-oriW_f(downW_tab[x,"Angle2"])
          A3<-oriW_f(downW_tab[x,"Angle3"])
          
         if (A1==A2) {
            if(A1==A3){
              y=1
            }else{
              y=0
            }
          }else{
            y=0
          }
          return(y)
        }
       
        downW_tab$DownWind<-sapply(c(1:nrow(downW_tab)), downW_f)
        
        #####Calculate the proportion of measures where the three angles are equal
        mDownW_Auto_waste_pix<-sum(downW_tab$DownWind)/nrow(downW_tab)
      
      ####Const_waste
        #####Wind orientation at a WMF (Angle2) recorded over the sampling period
        WMF_fid<-HR2[p,"near_CW"]
        Stat_Met_WMF<-as.character(Const_waste_coord[Const_waste_coord$Const_waste_fid==WMF_fid,"Station_SM"])
        Wdir_WMF<-wdir[,c("Chrono2",Stat_Met_WMF)]
        Wdir_WMF_dep0<-subset(Wdir_WMF, Chrono2>=departure & Chrono2 <=recapture)
        Wdir_WMF_dep<-Wdir_WMF_dep0[Wdir_WMF_dep0[,2]<999,]
        downW_tab0<-merge(Wdir_pix_dep,Wdir_WMF_dep, by="Chrono2", all = FALSE)
        downW_tab0$Angle2<-downW_tab0[,6]*10
        downW_tab<- downW_tab0[,c("Chrono2", "Angle2", "Angle3")]
        
        #####Gather the measures of Angle 1, 2 and 3 along time
        pix_sources<-subset(grid_sources, Pix_N==Northing & Pix_E==Easting)
        Angle1_pix<-pix_sources$Const_waste_Angle1
        downW_tab$Angle1<-rep(Angle1_pix, nrow(downW_tab))
        colnames(downW_tab)<-c("Chorno2","Angle3", "Angle2", "Angle1")
       
        #####Function to establish the equality/difference between Angle1, 2 and 3
        #####Important to take into acccount that the orientation recorded by weather stations expresses the direction where the wind comes FROM.
        #####Ex: 90 -> wind blowing FROM east and TOWARD west.
        #####It is necessary to invert the wind orientation (A2 and A3) to express the direction where the wind goes TOWARD.
        #####Otherwise, we can invert only A1.
        #####Also important to take into account that A2 is still expressed in tens of degrees.
        downW_f<- function (x){
          A1<-oriW_inv_f(downW_tab[x,"Angle1"])
          A2<-oriW_f(downW_tab[x,"Angle2"])
          A3<-oriW_f(downW_tab[x,"Angle3"])
          
          if (A1==A2) {
            if(A1==A3){
              y=1
            }else{
              y=0
            }
          }else{
            y=0
          }
          return(y)
        }
        
        downW_tab$DownWind<-sapply(c(1:nrow(downW_tab)), downW_f)
        
        #####Calculate the proportion of measures where the three angles are equal
        mDownW_Const_waste_pix<-sum(downW_tab$DownWind)/nrow(downW_tab)
        
      ####eWaste
        #####Wind orientation at a WMF (Angle2) recorded over the sampling period
        WMF_fid<-HR2[p,"near_eW"]
        Stat_Met_WMF<-as.character(eWaste_coord[eWaste_coord$eWaste_fid==WMF_fid,"Station_SM"])
        Wdir_WMF<-wdir[,c("Chrono2",Stat_Met_WMF)]
        Wdir_WMF_dep0<-subset(Wdir_WMF, Chrono2>=departure & Chrono2 <=recapture)
        Wdir_WMF_dep<-Wdir_WMF_dep0[Wdir_WMF_dep0[,2]<999,]
        downW_tab0<-merge(Wdir_pix_dep,Wdir_WMF_dep, by="Chrono2", all = FALSE)
        downW_tab0$Angle2<-downW_tab0[,6]*10
        downW_tab<- downW_tab0[,c("Chrono2", "Angle2", "Angle3")]
        
        #####Gather the measures of Angle 1, 2 and 3 along time
        pix_sources<-subset(grid_sources, Pix_N==Northing & Pix_E==Easting)
        Angle1_pix<-pix_sources$eWaste_Angle1
        downW_tab$Angle1<-rep(Angle1_pix, nrow(downW_tab))
        colnames(downW_tab)<-c("Chorno2","Angle3", "Angle2", "Angle1")
        
        #####Function to establish the equality/difference between Angle1, 2 and 3
        #####Important to take into acccount that the orientation recorded by weather stations expresses the direction where the wind comes FROM.
        #####Ex: 90 -> wind blowing FROM east and TOWARD west.
        #####It is necessary to invert the wind orientation (A2 and A3) to express the direction where the wind goes TOWARD.
        #####Otherwise, we can invert only A1.
        #####Also important to take into account that A2 is still expressed in tens of degrees.
        downW_f<- function (x){
          A1<-oriW_inv_f(downW_tab[x,"Angle1"])
          A2<-oriW_f(downW_tab[x,"Angle2"])
          A3<-oriW_f(downW_tab[x,"Angle3"])
          
          if (A1==A2) {
            if(A1==A3){
              y=1
            }else{
              y=0
            }
          }else{
            y=0
          }
          return(y)
        }
        
        downW_tab$DownWind<-sapply(c(1:nrow(downW_tab)), downW_f)
        
        #####Calculate the proportion of measures where the three angles are equal
        mDownW_eWaste_pix<-sum(downW_tab$DownWind)/nrow(downW_tab)
        
      ####Landfill
        #####Wind orientation at a WMF (Angle2) recorded over the sampling period
        WMF_fid<-HR2[p,"near_landfill"]
        Stat_Met_WMF<-as.character(Landfill_coord[Landfill_coord$Landfill_fid==WMF_fid,"Station_SM"])
        Wdir_WMF<-wdir[,c("Chrono2",Stat_Met_WMF)]
        Wdir_WMF_dep0<-subset(Wdir_WMF, Chrono2>=departure & Chrono2 <=recapture)
        Wdir_WMF_dep<-Wdir_WMF_dep0[Wdir_WMF_dep0[,2]<999,]
        downW_tab0<-merge(Wdir_pix_dep,Wdir_WMF_dep, by="Chrono2", all = FALSE)
        downW_tab0$Angle2<-downW_tab0[,6]*10
        downW_tab<- downW_tab0[,c("Chrono2", "Angle2", "Angle3")]
        
        #####Gather the measures of Angle 1, 2 and 3 along time
        pix_sources<-subset(grid_sources, Pix_N==Northing & Pix_E==Easting)
        Angle1_pix<-pix_sources$Landfill_Angle1
        downW_tab$Angle1<-rep(Angle1_pix, nrow(downW_tab))
        colnames(downW_tab)<-c("Chorno2","Angle3", "Angle2", "Angle1")
        
        #####Function to establish the equality/difference between Angle1, 2 and 3
        #####Important to take into acccount that the orientation recorded by weather stations expresses the direction where the wind comes FROM.
        #####Ex: 90 -> wind blowing FROM east and TOWARD west.
        #####It is necessary to invert the wind orientation (A2 and A3) to express the direction where the wind goes TOWARD.
        #####Otherwise, we can invert only A1.
        #####Also important to take into account that A2 is still expressed in tens of degrees.
        downW_f<- function (x){
          A1<-oriW_inv_f(downW_tab[x,"Angle1"])
          A2<-oriW_f(downW_tab[x,"Angle2"])
          A3<-oriW_f(downW_tab[x,"Angle3"])
          
          if (A1==A2) {
            if(A1==A3){
              y=1
            }else{
              y=0
            }
          }else{
            y=0
          }
          return(y)
        }
        
        downW_tab$DownWind<-sapply(c(1:nrow(downW_tab)), downW_f)
        
        #####Calculate the proportion of measures where the three angles are equal
        mDownW_Landfill_pix<-sum(downW_tab$DownWind)/nrow(downW_tab)
        
      ####Mixt_waste
        #####Wind orientation at a WMF (Angle2) recorded over the sampling period
        WMF_fid<-HR2[p,"near_mixt"]
        Stat_Met_WMF<-as.character(Mixt_waste_coord[Mixt_waste_coord$Mixt_waste_fid==WMF_fid,"Station_SM"])
        Wdir_WMF<-wdir[,c("Chrono2",Stat_Met_WMF)]
        Wdir_WMF_dep0<-subset(Wdir_WMF, Chrono2>=departure & Chrono2 <=recapture)
        Wdir_WMF_dep<-Wdir_WMF_dep0[Wdir_WMF_dep0[,2]<999,]
        downW_tab0<-merge(Wdir_pix_dep,Wdir_WMF_dep, by="Chrono2", all = FALSE)
        downW_tab0$Angle2<-downW_tab0[,6]*10
        downW_tab<- downW_tab0[,c("Chrono2", "Angle2", "Angle3")]
        
        #####Gather the measures of Angle 1, 2 and 3 along time
        pix_sources<-subset(grid_sources, Pix_N==Northing & Pix_E==Easting)
        Angle1_pix<-pix_sources$Mixt_waste_Angle1
        downW_tab$Angle1<-rep(Angle1_pix, nrow(downW_tab))
        colnames(downW_tab)<-c("Chorno2","Angle3", "Angle2", "Angle1")
        
        #####Function to establish the equality/difference between Angle1, 2 and 3
        #####Important to take into acccount that the orientation recorded by weather stations expresses the direction where the wind comes FROM.
        #####Ex: 90 -> wind blowing FROM east and TOWARD west.
        #####It is necessary to invert the wind orientation (A2 and A3) to express the direction where the wind goes TOWARD.
        #####Otherwise, we can invert only A1.
        #####Also important to take into account that A2 is still expressed in tens of degrees.
        downW_f<- function (x){
          A1<-oriW_inv_f(downW_tab[x,"Angle1"])
          A2<-oriW_f(downW_tab[x,"Angle2"])
          A3<-oriW_f(downW_tab[x,"Angle3"])
          
          if (A1==A2) {
            if(A1==A3){
              y=1
            }else{
              y=0
            }
          }else{
            y=0
          }
          return(y)
        }
        
        downW_tab$DownWind<-sapply(c(1:nrow(downW_tab)), downW_f)
        
        #####Calculate the proportion of measures where the three angles are equal
        mDownW_Mixt_waste_pix<-sum(downW_tab$DownWind)/nrow(downW_tab)
        
      ####Wastewater
        #####Wind orientation at a WMF (Angle2) recorded over the sampling period
        WMF_fid<-HR2[p,"near_WW"]
        Stat_Met_WMF<-as.character(Wastewater_coord[Wastewater_coord$Wastewater_fid==WMF_fid,"Station_SM"])
        Wdir_WMF<-wdir[,c("Chrono2",Stat_Met_WMF)]
        Wdir_WMF_dep0<-subset(Wdir_WMF, Chrono2>=departure & Chrono2 <=recapture)
        Wdir_WMF_dep<-Wdir_WMF_dep0[Wdir_WMF_dep0[,2]<999,]
        downW_tab0<-merge(Wdir_pix_dep,Wdir_WMF_dep, by="Chrono2", all = FALSE)
        downW_tab0$Angle2<-downW_tab0[,6]*10
        downW_tab<- downW_tab0[,c("Chrono2", "Angle2", "Angle3")]
        
        #####Gather the measures of Angle 1, 2 and 3 along time
        pix_sources<-subset(grid_sources, Pix_N==Northing & Pix_E==Easting)
        Angle1_pix<-pix_sources$Wastewater_Angle1
        downW_tab$Angle1<-rep(Angle1_pix, nrow(downW_tab))
        colnames(downW_tab)<-c("Chorno2","Angle3", "Angle2", "Angle1")
        
        #####Function to establish the equality/difference between Angle1, 2 and 3
        #####Important to take into acccount that the orientation recorded by weather stations expresses the direction where the wind comes FROM.
        #####Ex: 90 -> wind blowing FROM east and TOWARD west.
        #####It is necessary to invert the wind orientation (A2 and A3) to express the direction where the wind goes TOWARD.
        #####Otherwise, we can invert only A1.
        #####Also important to take into account that A2 is still expressed in tens of degrees.
        downW_f<- function (x){
          A1<-oriW_inv_f(downW_tab[x,"Angle1"])
          A2<-oriW_f(downW_tab[x,"Angle2"])
          A3<-oriW_f(downW_tab[x,"Angle3"])
          
          if (A1==A2) {
            if(A1==A3){
              y=1
            }else{
              y=0
            }
          }else{
            y=0
          }
          return(y)
        }
        
        downW_tab$DownWind<-sapply(c(1:nrow(downW_tab)), downW_f)
        
        #####Calculate the proportion of measures where the three angles are equal
        mDownW_Wastewater_pix<-sum(downW_tab$DownWind)/nrow(downW_tab)
        
      ####eW_CW
        #####Wind orientation at a WMF (Angle2) recorded over the sampling period
        WMF_fid<-HR2[p,"near_eW_CW"]
        Stat_Met_WMF<-as.character(eW_CW_coord[eW_CW_coord$eW_CW_fid==WMF_fid,"Station_SM"])
        Wdir_WMF<-wdir[,c("Chrono2",Stat_Met_WMF)]
        Wdir_WMF_dep0<-subset(Wdir_WMF, Chrono2>=departure & Chrono2 <=recapture)
        Wdir_WMF_dep<-Wdir_WMF_dep0[Wdir_WMF_dep0[,2]<999,]
        downW_tab0<-merge(Wdir_pix_dep,Wdir_WMF_dep, by="Chrono2", all = FALSE)
        downW_tab0$Angle2<-downW_tab0[,6]*10
        downW_tab<- downW_tab0[,c("Chrono2", "Angle2", "Angle3")]
        
        #####Gather the measures of Angle 1, 2 and 3 along time
        pix_sources<-subset(grid_sources, Pix_N==Northing & Pix_E==Easting)
        Angle1_pix<-pix_sources$eW_CW_Angle1
        downW_tab$Angle1<-rep(Angle1_pix, nrow(downW_tab))
        colnames(downW_tab)<-c("Chorno2","Angle3", "Angle2", "Angle1")
        
        #####Function to establish the equality/difference between Angle1, 2 and 3
        #####Important to take into acccount that the orientation recorded by weather stations expresses the direction where the wind comes FROM.
        #####Ex: 90 -> wind blowing FROM east and TOWARD west.
        #####It is necessary to invert the wind orientation (A2 and A3) to express the direction where the wind goes TOWARD.
        #####Otherwise, we can invert only A1.
        #####Also important to take into account that A2 is still expressed in tens of degrees.
        downW_f<- function (x){
          A1<-oriW_inv_f(downW_tab[x,"Angle1"])
          A2<-oriW_f(downW_tab[x,"Angle2"])
          A3<-oriW_f(downW_tab[x,"Angle3"])
          
          if (A1==A2) {
            if(A1==A3){
              y=1
            }else{
              y=0
            }
          }else{
            y=0
          }
          return(y)
        }
        
        downW_tab$DownWind<-sapply(c(1:nrow(downW_tab)), downW_f)
        
        #####Calculate the proportion of measures where the three angles are equal
        mDownW_eW_CW_pix<-sum(downW_tab$DownWind)/nrow(downW_tab)
   
    ###Particuar matter
    PM_pix<-PM[,c("Chrono2",Stat_PM)]
    PM_pix_dep0<-subset(PM_pix, Chrono2>=departure & Chrono2 <=recapture)
    PM_pix_dep<-PM_pix_dep0[PM_pix_dep0[,2]<999,]
    mPM_pix_dep<-mean(PM_pix_dep[,2])
  
    ###Gather all the variables averaged for the pixel during the tracking period in one vector.
  yp<-c(mTemp_pix_dep, mHum_pix_dep, mPres_pix_dep, mWspeed_pix_dep, mWdir_pix_dep, 
       mDownW_Auto_waste_pix, mDownW_Const_waste_pix, mDownW_eWaste_pix,
       mDownW_Landfill_pix, mDownW_Mixt_waste_pix, mDownW_Wastewater_pix,mDownW_eW_CW_pix,
       mPM_pix_dep)
  
  return(yp)
}

  ##Apply the function mean_climate_pix to each pixel of the study area where the gull's
  ##UD > 99
  tab0<-sapply(pix_list, mean_climate_pix)
  
  ##Transpose the dataset so environmental variables are declined in the columns
  tab1<-t(tab0)
  colnames(tab1)<-c("pix_temp", "pix_hum", "pix_pres", "pix_wspeed", "pix_wdir","pix_dw_Auto_waste", "pix_dw_Const_waste", "pix_dw_eWaste", "pix_dw_Landfill", "pix_dw_Mixt_waste", "pix_dw_Wastewater", "pix_dw_eW_CW", "pix_PM")
  
  ##Join UDs 
  HR3<-cbind(HR2[,1:18],tab1)
  
  ##Calculate the average weather variable of the entire home range of the tracked gull
  ##(mean weighted by the presence probability of the gull in its home range).
  mTemp_ind<-wtd.mean(HR3$pix_temp, HR3$fUD)
  mHum_ind<-wtd.mean(HR3$pix_hum, HR3$fUD)
  mPres_ind<-wtd.mean(HR3$pix_pres, HR3$fUD)
  mWspeed_ind<-wtd.mean(HR3$pix_wspeed, HR3$fUD)

  ##calculate cartesian coordinates of wdir angles
  HR3$sin_pix_wdir<-sin(HR3$pix_wdir*pi/180)
  HR3$cos_pix_wdir<-cos(HR3$pix_wdir*pi/180)
  ##calculate the weighted mean of cartesian coordinates
  msin_pix_wdir<-wtd.mean(HR3$sin_pix_wdir, HR3$fUD)
  mcos_pix_wdir<-wtd.mean(HR3$cos_pix_wdir, HR3$fUD)
  ##calculate the coresponding polar angle
  rad2<-atan2(msin_pix_wdir,mcos_pix_wdir)
  mWdir_ind<-ceiling(radTOdeg(rad2)/10)*10
  mPM_ind<-wtd.mean(HR3$pix_PM, HR3$fUD)

  ##Calculate the mean distance of a gull to a 500m-rafius around a WMF.
  mdist_Auto_waste<-wtd.mean(HR3$AW_dist-500, HR3$fUD)
  mdist_Const_waste<-wtd.mean(HR3$CW_dist-500, HR3$fUD)
  mdist_eWaste<-wtd.mean(HR3$eW_dist_dist-500, HR3$fUD)
  mdist_Landfill<-wtd.mean(HR3$landfill_dist-500, HR3$fUD)
  mdist_Mixt_waste<-wtd.mean(HR3$mixt_dist-500, HR3$fUD)
  mdist_Wastewater<-wtd.mean(HR3$WW_dist-500, HR3$fUD)
  mdist_eW_CW<-wtd.mean(HR3$eW_CW_dist-500, HR3$fUD)

  ##Calculate the mean squared distance to a 500m-rafius around a WMF.
  mdist_sq_Auto_waste<-wtd.mean((HR3$AW_dist-500)^2, HR3$fUD)
  mdist_sq_Const_waste<-wtd.mean((HR3$CW_dist-500)^2, HR3$fUD)
  mdist_sq_eWaste<-wtd.mean((HR3$eW_dist_dist-500)^2, HR3$fUD)
  mdist_sq_Landfill<-wtd.mean((HR3$landfill_dist-500)^2, HR3$fUD)
  mdist_sq_Mixt_waste<-wtd.mean((HR3$mixt_dist-500)^2, HR3$fUD)
  mdist_sq_Wastewater<-wtd.mean((HR3$WW_dist-500)^2, HR3$fUD)
  mdist_sq_eW_CW<-wtd.mean((HR3$eW_CW_dist-500)^2, HR3$fUD)

  ##Calculate the mean proportion of the home range exposed to the wind coming from a landfill.
  mdw_Auto_waste<-wtd.mean(HR3$pix_dw_Auto_waste, HR3$fUD)
  mdw_Const_waste<-wtd.mean(HR3$pix_dw_Const_waste, HR3$fUD)
  mdw_eWaste<-wtd.mean(HR3$pix_dw_eWaste, HR3$fUD)
  mdw_Landfill<-wtd.mean(HR3$pix_dw_Landfill, HR3$fUD)
  mdw_Mixt_waste<-wtd.mean(HR3$pix_dw_Mixt_waste, HR3$fUD)
  mdw_Wastewater<-wtd.mean(HR3$pix_dw_Wastewater, HR3$fUD)
  mdw_eW_CW<-wtd.mean(HR3$pix_dw_eW_CW, HR3$fUD)

  ##Calculate the presence probability of a gull within 500m of a WMF
  HR4<-subset(HR3, landfill_dist <=500)
  prob_landfill<-sum(HR4$fUD)

  HR5<-subset(HR3, AW_dist <=500)
  prob_AW<-sum(HR5$fUD)

  HR6<-subset(HR3, CW_dist <=500)
  prob_CW<-sum(HR6$fUD)

  HR7<-subset(HR3, eW_dist <=500)
  prob_eW<-sum(HR7$fUD)

  HR8<-subset(HR3, mixt_dist <=500)
  prob_mixt<-sum(HR8$fUD)

  HR9<-subset(HR3, WW_dist <=500)
  prob_WW<-sum(HR9$fUD)

  HR10<-subset(HR3, eW_CW_dist <=500)
  prob_eW_CW<-sum(HR10$fUD)

  ##Gather all the variables averaged for the home range during the tracking period in one vector.
    yi<-c(mTemp_ind,mHum_ind,mPres_ind,mWspeed_ind,mWdir_ind, mPM_ind, 
      mdist_Auto_waste,mdist_Const_waste,mdist_eWaste,mdist_Landfill,mdist_Mixt_waste,mdist_Wastewater,mdist_eW_CW,
      mdist_sq_Auto_waste,mdist_sq_Const_waste,mdist_sq_eWaste,mdist_sq_Landfill,mdist_sq_Mixt_waste,mdist_sq_Wastewater,mdist_sq_eW_CW,
      mdw_Auto_waste,mdw_Const_waste,mdw_eWaste,mdw_Landfill,mdw_Mixt_waste,mdw_Wastewater, mdw_eW_CW,
      prob_landfill, prob_AW, prob_CW, prob_eW, prob_mixt, prob_WW, prob_eW_CW)

return(yi)
}

#Apply the function to each tracked gull.
RBGs<-c(1:67)
TAB0<-sapply(RBGs,mean_variable_ind)
TAB1<-t(TAB0)
colnames(TAB1)<-c("RBG_temp", "RBG_hum", "RBG_pres", "RBG_wspeed", "RBG_wdir","RBG_PM",
                  "RBG_mdist_Landfill","RBG_mdist_Auto_waste", "RBG_mdist_Const_waste","RBG_mdist_eWaste","RBG_mdist_Mixt_waste","RBG_mdist_Wastewater","RBG_mdist_eW_CW",
                  "RBG_mdw_Auto_waste","RBG_mdw_Const_waste","RBG_mdw_eWaste","RBG_mdw_Landfill","RBG_mdw_Const_waste","RBG_mdw_Wastewater","RBG_mdw_eW_CW",
                  "mdist_sq_Auto_waste","mdist_sq_Const_waste","mdist_sq_eWaste","mdist_sq_Landfill","mdist_sq_Mixt_waste","mdist_sq_Wastewater","mdist_sq_eW_CW",
                  "RBG_Landfill", "RBG_Auto_waste", "RBG_Const_waste", "RBG_eWaste", "RBG_Mixt_waste", "RBG_Wastewater", "RBG_eW_CW")
TAB2<-cbind(Dep[,1:2], TAB1)
write.table(TAB2, "RB_HR_variables2.csv", sep=";", row.names = F)

