rm(list=ls())
gc()
library(raster)
library(rgdal)
library(readxl)
# library(cluster)
# library(classInt)
reclass_df <-function(df, assignvalue, list)
{
  for (i in list) {
    df[df==i] <- assignvalue
  }
  return(df)
}


idkab <- c(3517,3518,3215,3522,3213,3524,3212)
idkab <- sort(idkab,decreasing = FALSE)
#idkab <- c(3518,3517,3522,3524,3212,3215,3213)
#idkab <- sort(idkab,decreasing = TRUE)
#idkab <- c(3215,3213)
#time_list <- c(10,15)
# idkab <- c(3518,3517,3522,3524)


model_ML_list <- c('svmRadial042','svmRadial032','svmRadial02','svmRadial01')
SENSOR_LIST <- c("S2S1AGG","LS8S1AGG","LS8","S2")
SENSOR_pendek_LIST <- c("S2","LS8","LS8","S2")

folder_ML_classify_fmask <- paste("F:/R-Script-DriveF/ML-LS8_S2_S1v1/CLASSIFY-MASK-PADDY",collapse = "",sep = "")
period_list_classify <- sort(list.dirs(folder_ML_classify_fmask,full.names = FALSE,recursive = FALSE),
                             decreasing = FALSE)

model_ML_list <- c('svmRadial01')
SENSOR_LIST <- c("S2")
# SENSOR_LIST <- "LS8S1AGG"
# SENSOR_pendek_LIST <- "LS8"
# 
# SENSOR_LIST <- "S2S1AGG"
# SENSOR_pendek_LIST <- "S2"
min5 <- -20
for (k in 1:length(model_ML_list)){
  model_ML <- model_ML_list[k]
  SENSOR <- SENSOR_LIST[k]
  SENSOR_pendek <- SENSOR_pendek_LIST[k]
  print(SENSOR)
  for (j in 1:length(idkab)){
    region <- idkab[j]
    prov <- substr(region,1,2)
    
    search_str <- paste(region,".*[.]tif$",sep = "")
    band_VH <- paste("F:/R-Script-DriveF/S1-ML-2019/BANDS-LEE-PADDY-VH/",region,collapse = "",sep = "")
    if (dir.exists(band_VH)==FALSE){
      next
    }
    setwd(band_VH)
    rasterVH <- lapply(list.files(pattern=search_str), raster)
    if (ncell(rasterVH)==0 ) {
      next
    }
    files_VH <- list.files(pattern=search_str)
    if (length(files_VH)<1) {
      next
    }
    
    for (it in 1:length(files_VH)) {
      
      period_0 <- substr(files_VH[it],48,55)
      date_0 <- as.Date(period_0,"%Y%m%d")
      #find classify in mask
      if (!(date_0>as.Date("2017-12-31") & date_0<as.Date("2018-10-01"))) {
        next
      }
      df_0_raster <- raster(files_VH[it])
      VH_id <- substr(files_VH[it],48,62)
      S2_0 <- NA
      int_S2_0 <-1
      
      for (int_VHS2 in 1:length(period_list_classify)){
        
        #for (int_classify in seq(1, length(period_list_classify), 3)){
        period_cari <- period_list_classify[int_VHS2]
        
        if (!(period_cari>as.Date("2017-12-31") & period_cari<date_0)) {
          next
        }
        if (period_cari=="") {
        }
        search_str <- paste(region,".*[.]tif$",sep = "")
        band_folder_S2VH <- paste("F:/R-Script-DriveF/ML-LS8_S2_S1v1/CLASSIFY-MASK-PADDY/",period_cari,"/",model_ML,collapse = "",sep = "")
        
        files_S2VH <- list.files(pattern=search_str,path =band_folder_S2VH )
        if (is.na(files_S2VH[1])==TRUE) {
          next
        }
        S2_0[int_S2_0] <- paste(band_folder_S2VH,"/",files_S2VH[1],sep="")
        int_S2_0 <- int_S2_0+1
        
      }
      if (is.na(S2_0)==TRUE) {
        next
      }
      if (length(S2_0)<1) {
        next
      }
      

      S2_0 <- as.data.frame(S2_0,stringsAsFactors = FALSE)
      colnames(S2_0) <- c("fname")
      S2_0$date_str <- substr(S2_0$fname, 55, 55+9)
      S2_0$date1 <- as.Date(S2_0$date_str)
      S2_0$diff <- (as.integer(as.Date(date_0) - S2_0$date1))
      S2_0_close <- S2_0[order(S2_0$diff),]
      
      gap  <- S2_0_close$diff[1]
      if (gap>11 ) {
        next
      }
      if (gap<0 ) {
        next
      }
      folder_S2VH <- paste("F:/R-Script-DriveF/ML-LS8_S2_S1v1/VH_S2/",collapse = "",sep = "")
      dir.create(folder_S2VH, showWarnings = FALSE)
      folder_S2VH <- paste("F:/R-Script-DriveF/ML-LS8_S2_S1v1/VH_S2/",region,"/",collapse = "",sep = "")
      dir.create(folder_S2VH, showWarnings = FALSE)
    
      date_S2  <- S2_0_close$date1[1]
      filename_S2VH <- paste(folder_S2VH,region,"_", VH_id,"_",date_S2 ,"_",SENSOR_pendek,"_VHS2",".tif", sep="")
      if (file.exists(filename_S2VH)==TRUE) {
        next
      }
      print (paste(format(Sys.time(), "%a %b %d %Y %X"),"-->VH MIN - S2 | ","gap:",gap, " | ",filename_S2VH,sep=""))
      
      S2_0_raster <-  raster(S2_0_close$fname[1])
      crs_raster_VH <- projection(df_0_raster,asText = TRUE)
      crs_raster_S2 <- projection(S2_0_raster,asText = TRUE)
      if (crs_raster_VH!=crs_raster_S2) {
        df_0_raster <- projectRaster(df_0_raster, crs=crs_raster_S2)
      }
      if (S2_0_raster@ncols * S2_0_raster@nrows != df_0_raster@ncols * df_0_raster@nrows) {
        S2_0_raster <- resample(S2_0_raster, df_0_raster,method="ngb")
      }
      
      rast_stack_S2VH <- stack(df_0_raster,S2_0_raster)
      fun_raster_S2VH <- function(x) {
        if (is.na(x[1])==FALSE) {
          if (is.na(x[2])==FALSE) {
            if (x[1]<min5) {
              if (x[2]==1) {
                result <- NA
              } else {
                result <- x[1]
              }
            } else {
              result <- x[1]
            }
          } else {
            result <- NA
          }
        } else {
          result <- NA
        }
        return(result)
      }
      raster_S2VH <- calc(rast_stack_S2VH, fun_raster_S2VH)
      #create funciton
      #save raster
      
      writeRaster(raster_S2VH, filename=filename_S2VH, format="GTiff", overwrite=TRUE)
      
    }
  }
}
