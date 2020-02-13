rm(list=ls())
library(raster)
library(rgdal)
library(readxl)
reclass_df <-function(df, assignvalue, list)
{
  for (i in list) {
    df[df==i] <- assignvalue
  }
  return(df)
}


idkab <- c(3517,3518,3215,3522,3213,3524,3212)
idkab <- sort(idkab,decreasing = TRUE)
idkab <- sort(idkab,decreasing = FALSE)
#idkab <- c(3215,3213)
#idkab <- 3215
#time_list <- c(10,15)
Regency_PR_list <- read_excel("C:/MasseyOffice1/Research/R-Script2019/S2_java_pr_2018.xlsx")
model_ML_list <- c('svmRadial042','svmRadial032','svmRadial02','svmRadial01')
SENSOR_LIST <- c("S2S1AGG","LS8S1AGG","LS8","S2")
SENSOR_pendek_LIST <- c("S2","LS8","LS8","S2")
#idkab <-3212
model_ML_list <- c('svmRadialS2')
SENSOR_LIST <- c("S2")
for (k in 1:length(model_ML_list)){
  model_ML <- model_ML_list[k]
  SENSOR <- SENSOR_LIST[k]
  if (SENSOR=="S2" | SENSOR=="S2S1AGG" ) {
    time_list <- c(5,10,15,20,25,30)
  }  else {
    time_list <- c(16)
  }
  print(SENSOR)
  for (j in 1:length(idkab)){
    region <- idkab[j]
    for (time in time_list) {
      folder_ML_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR",collapse = "",sep = "")
      period_list_classify <- sort(list.dirs(folder_ML_classify_fmask,full.names = FALSE,recursive = FALSE),
                                   decreasing = FALSE)
      
      for (int_classify in 1:length(period_list_classify)){
        
        #for (int_classify in seq(1, length(period_list_classify), 3)){  
        period_first<- period_list_classify[int_classify]
        
        if (as.Date(period_first) < as.Date("2018-06-01")) {
          next
        }
        if (period_first=="") {
          next
        }
        
        search_str <- paste(region,".*[.]tif$",sep = "")
        band_folder <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR/",period_first,"/",model_ML,collapse = "",sep = "")
        if (dir.exists(band_folder)==FALSE){
          next
        }
        setwd(band_folder)
        input.rasters.first <- lapply(list.files(pattern=search_str), raster)
        if (ncell(input.rasters.first)==0 ) {
          next
        }
        period_prev <- as.character(as.Date(period_first) - time)
        if ((period_prev %in% period_list_classify)==FALSE) {
          next
        }
        search_str <- paste(region,".*[.]tif$",sep = "")
        band_folder <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR/",period_prev,"/",model_ML,collapse = "",sep = "")
        if (dir.exists(band_folder)==FALSE){
          next
        }
        setwd(band_folder)
        input.rasters.prev <- lapply(list.files(pattern=search_str), raster)
        if (ncell(input.rasters.prev)==0 ) {
          next
        }
        
        period <- paste(period_prev,"_",period_first,sep="")
        period <- gsub("-","",period)
        
        folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR-CD",collapse = "",sep = "")
        dir.create(folder_clip_paddy, showWarnings = FALSE)
        
        folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR-CD/CLASSIFY-MASK-PADDY-CLEAR-CD-",time,"/",collapse = "",sep = "")
        dir.create(folder_clip_paddy, showWarnings = FALSE)
        
        folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR-CD/CLASSIFY-MASK-PADDY-CLEAR-CD-",time,"/",period,"/",collapse = "",sep = "")
        dir.create(folder_clip_paddy, showWarnings = FALSE)
        
        folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR-CD/CLASSIFY-MASK-PADDY-CLEAR-CD-",time,"/",period,"/",model_ML,"/",collapse = "",sep = "")
        dir.create(folder_clip_paddy, showWarnings = FALSE)
        
        filename_result_paddy_cd <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_change_detection_",time,"_",model_ML, sep="")
        filename_tif_paddy_cd <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_change_detection_",time,"_",model_ML,".tif", sep="")
        filename_csv_paddy_cd <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_change_detection_",time,"_",model_ML,"_check.csv", sep="")
        
        filename_result_paddy_cd_reclass <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_change_detection_",time,"_reclass_",model_ML, sep="")
        filename_tif_paddy_cd_reclass <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_change_detection_",time,"_reclass_",model_ML,".tif", sep="")
        filename_csv_paddy_cd_reclass_all <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_change_detection_",time,"_reclass_",model_ML,"_all.csv", sep="")
        
        filename_csv_paddy_cd_reclass <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_change_detection_",time,"_reclass_",model_ML,".csv", sep="")
        print(paste(format(Sys.time(), "%a %b %d %X %Y"),region, int_classify,time,filename_csv_paddy_cd_reclass,sep=" | ")) 
        
        if (file.exists(filename_csv_paddy_cd)) {
          next
        }
        
        if (input.rasters.prev[[1]]@ncols * input.rasters.prev[[1]]@nrows != input.rasters.first[[1]]@ncols * input.rasters.first[[1]]@nrows) {
          e <- intersect(extent(input.rasters.prev[[1]]), extent(input.rasters.first[[1]]))
          input.rasters.prev[[1]] <- crop(input.rasters.prev[[1]], e)
          input.rasters.first[[1]] <- resample(input.rasters.first[[1]], input.rasters.prev[[1]])
          print("extent")
        }
        
        if (extent(input.rasters.prev[[1]]) != extent(input.rasters.first[[1]])) {
          e <- intersect(extent(input.rasters.prev[[1]]), extent(input.rasters.first[[1]]))
          input.rasters.prev[[1]] <- crop(input.rasters.prev[[1]], e)
          input.rasters.first[[1]] <- resample(input.rasters.first[[1]], input.rasters.prev[[1]])
          print("extent")
        }
        
        rast_stack <- stack(input.rasters.prev[[1]],input.rasters.first[[1]])
        fun_raster <- function(x) {
          if (is.na(x[1])==FALSE) {
            if (is.na(x[2])==FALSE) {
                result <- as.integer(paste(x[1],"0",x[2],sep = ""))
            } else {
              result <- NA
            }
          } else {
            result <- NA
          }
          return(result)
        }
        if (file.exists(filename_tif_paddy_cd)==TRUE) {
          raster_mask <- raster(filename_tif_paddy_cd)
        } else {
          raster_mask <- calc(rast_stack, fun_raster)
          writeRaster(raster_mask, filename=filename_result_paddy_cd, format="GTiff", overwrite=TRUE)  
        }
        
        paddy_raster_df <- as.data.frame(raster_mask)
        paddy_raster_df_count <- table(paddy_raster_df)
        area_ha <-paddy_raster_df_count * 10 * 10 / 10000
        write.csv(area_ha, file = filename_csv_paddy_cd)
        
  
        raster_mask<-NA
        paddy_raster_df<-NA
        gc()
        
      }
      
    }
    removeTmpFiles(h=0)
  }
}