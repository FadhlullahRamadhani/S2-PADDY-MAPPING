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
idkab <- sort(idkab,decreasing = TRUE)
#idkab <- c(3518,3517,3522,3524,3212,3215,3213)
#idkab <- sort(idkab,decreasing = TRUE)
#idkab <- c(3215,3213)
#time_list <- c(10,15)
# idkab <- c(3518,3517,3522,3524)
#idkab <- c(3518,3524)
idkab <- c(3517,3522)

folder_ML_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY",collapse = "",sep = "")
period_list_classify <- sort(list.dirs(folder_ML_classify_fmask,full.names = FALSE,recursive = FALSE),
                             decreasing = FALSE)

model_ML_list <- c('svmRadialS2')
SENSOR_LIST <- c("S2")
# SENSOR_LIST <- "LS8S1AGG"
# SENSOR_pendek_LIST <- "LS8"
# 
# SENSOR_LIST <- "S2S1AGG"
# SENSOR_pendek_LIST <- "S2"

for (k in 1:length(model_ML_list)){
  model_ML <- model_ML_list[k]
  SENSOR <- SENSOR_LIST[k]
  print(SENSOR)
  for (j in 1:length(idkab)){
    region <- idkab[j]
    prov <- substr(region,1,2)
    
    for (int_classify in 1:length(period_list_classify)){
      
      #for (int_classify in seq(1, length(period_list_classify), 3)){
      period <- period_list_classify[int_classify]
      
      if (!(period>=as.Date("2018-06-01") & period<as.Date("2018-10-01"))) {
        next
      }
      # if (!(period>=as.Date("2018-07-20") & period<as.Date("2018-08-15"))) {
      #   next
      # }
      if (period=="") {
        next
      }
      
      search_str <- paste(region,".*[.]tif$",sep = "")
      band_folder <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY/",period,"/",model_ML,collapse = "",sep = "")
      if (dir.exists(band_folder)==FALSE){
        next
      }
      setwd(band_folder)
      input.rasters <- lapply(list.files(pattern=search_str), raster)
      if (ncell(input.rasters)==0 ) {
        next
      }
      
      
      folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEARv4/",period,"/",model_ML,"/",collapse = "",sep = "")
  
      filename_result_paddy_clear <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_clearv4_",model_ML, sep="")
      filename_tif_paddy_clear <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_clearv4_",model_ML,".tif", sep="")
      filename_csv_paddy_clear <- paste(folder_clip_paddy,region,"_", period,"_",SENSOR,"_ML_clearv4_",model_ML,".csv", sep="")
      
       
      if (file.exists(filename_tif_paddy_clear)) {
        next
      }
      print(paste(format(Sys.time(), "%a %b %d %X %Y"),region, int_classify,filename_tif_paddy_clear,sep=" | "))
      
      folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEARv4",collapse = "",sep = "")
      dir.create(folder_clip_paddy, showWarnings = FALSE)
      
      folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEARv4/",period,"/",collapse = "",sep = "")
      dir.create(folder_clip_paddy, showWarnings = FALSE)
      
      folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEARv4/",period,"/",model_ML,"/",collapse = "",sep = "")
      dir.create(folder_clip_paddy, showWarnings = FALSE)
      
      
      search_str <- paste(region,".*[.]tif$",sep = "")
      band_VH <- paste("F:/ML-S2_S1v1/VH_S2/",region,collapse = "",sep = "")
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
      VHrasterlist <- NA
      int1 <- 1
      if (prov==32) {
        limit <- 150 
      } else {
        limit <- 60
      }
      for (intVh in 1:length(files_VH)) {
        period1 <- substr(files_VH[intVh],6,13)
        # period1 <- substr(files_VH[intVh],56,63)
        # period1 <- substr(files_VH[intVh],37,44)
        date_VH <- as.Date(period1,"%Y%m%d")
        if ((date_VH>=as.Date(period)-limit & date_VH<=as.Date(period))) {
          #print(date_VH)
          VHrasterlist[int1] <- files_VH[intVh]
          int1 <- int1 + 1
        }
      }
      date_VH_first <- substr(VHrasterlist[1],6,13)
      date_VH_last <- substr(VHrasterlist[length(VHrasterlist)],6,13)
      folder_VH_MIN <- paste("F:/ML-S2_S1v1/VH_CLEAR/",collapse = "",sep = "")
      dir.create(folder_VH_MIN, showWarnings = FALSE)
      folder_VH_MIN <- paste("F:/ML-S2_S1v1/VH_CLEAR/",region,"/",collapse = "",sep = "")
      dir.create(folder_VH_MIN, showWarnings = FALSE)
      
      filename_VH_MIN_clear <- paste(folder_VH_MIN,region,"_", date_VH_first,"_",date_VH_last,"_",SENSOR,"_VHMIN",".tif", sep="")
      
      if (file.exists(filename_VH_MIN_clear)==TRUE) {
        VHrasterlist.raster.min <- raster(filename_VH_MIN_clear)
      } else {
        length_raster <- length(VHrasterlist)
        print(length_raster)
        divider <- round(length_raster/3)
        divided <- (length_raster/divider)
        for (it in 1:divider) {
          size_df_0 <-round( (it-1)*(divided)+1)
          size_df_1 <- round((it)*(divided))
          if (it==divider) {
            size_df_1 <- length(VHrasterlist)
          }
          print (paste(format(Sys.time(), "%a %b %d %Y %X"),"-->VH MIN ",it,"==",size_df_0,"-",size_df_1,sep=""))
          if (it==1){
            VHrasterlist.raster1 <- brick(lapply(VHrasterlist[size_df_0:size_df_1], raster))
            VHrasterlist.raster.min <- stackApply(VHrasterlist.raster1, indices=rep(1,nlayers(VHrasterlist.raster1)), fun = min, na.rm=TRUE)
            VHrasterlist.raster1 <- NA
          } else {
            VHrasterlist.raster1 <- brick(lapply(VHrasterlist[size_df_0:size_df_1], raster))
            VHrasterlist.raster1 <- brick(VHrasterlist.raster.min,VHrasterlist.raster1)
            VHrasterlist.raster.min <- stackApply(VHrasterlist.raster1, indices=rep(1,nlayers(VHrasterlist.raster1)), fun = min, na.rm=TRUE)
            VHrasterlist.raster1 <- NA
          }
          gc()
        }

        gc()
        crs_raster_VH <- projection(VHrasterlist.raster.min,asText = TRUE)
        crs_raster_SENSOR <- projection(input.rasters[[1]],asText = TRUE)
        if (crs_raster_VH!=crs_raster_SENSOR) {
          VHrasterlist.raster.min <- projectRaster(VHrasterlist.raster.min, crs=crs_raster_SENSOR)
        }
        writeRaster(VHrasterlist.raster.min, filename=filename_VH_MIN_clear, format="GTiff", overwrite=TRUE)
        
      }
      
      # zClass <- classIntervals(na.omit(sampleRegular(VHrasterlist.raster.min, 100000)),
      #                          n=5,style="jenks")
      # 
      # min5 <- zClass$brks[5] 
      # if (min5>-20) {
      #   min5 <- zClass$brks[4] 
      # }
      # if (min5>-20) {
      #   min5 <- zClass$brks[3] 
      # }
      # if (min5>-20) {
      #   min5 <- zClass$brks[2] 
      # }
      if (prov==32) {
        min5 <- -19
      } else {
        min5 <- -20
      }
    
      # filename_VH_MIN_min5 <- paste(folder_clip_paddy,region,"_", period,"_VHMIN",min5,".csv", sep="")
      # write.csv(as.character(zClass$brks), file = filename_VH_MIN_min5)
      
      crs_raster_VH <- projection(VHrasterlist.raster.min,asText = TRUE)
      crs_raster_SENSOR <- projection(input.rasters[[1]],asText = TRUE)
      if (crs_raster_VH!=crs_raster_SENSOR) {
        VHrasterlist.raster.min <- projectRaster(VHrasterlist.raster.min, crs=crs_raster_SENSOR)
      }
      
      #plot(VHrasterlist.raster.min, breaks=c(-20,0),col = terrain.colors(2))
      
      if (input.rasters[[1]]@ncols * input.rasters[[1]]@nrows != VHrasterlist.raster.min@ncols * VHrasterlist.raster.min@nrows) {
        input.rasters[[1]] <- resample(input.rasters[[1]], VHrasterlist.raster.min,method="ngb")
      }
      
      if (input.rasters[[1]]@ncols * input.rasters[[1]]@nrows != VHrasterlist.raster.min@ncols * VHrasterlist.raster.min@nrows) {
        e <- intersect(extent(VHrasterlist.raster.min), extent(input.rasters[[1]]))
        print("extent")
        VHrasterlist.raster.min <- crop(VHrasterlist.raster.min, e)
        input.rasters[[1]] <- resample(input.rasters[[1]], VHrasterlist.raster.min,method="ngb")
        
      }
      
      if (input.rasters[[1]]@ncols * input.rasters[[1]]@nrows != VHrasterlist.raster.min@ncols * VHrasterlist.raster.min@nrows) {
        e <- intersect(extent(VHrasterlist.raster.min), extent(input.rasters[[1]]))
        print("extent")
        VHrasterlist.raster.min <- crop(VHrasterlist.raster.min, e)
        input.rasters[[1]] <- resample(input.rasters[[1]], VHrasterlist.raster.min,method="ngb")
      
      }
      
      if (extent(VHrasterlist.raster.min) != extent(input.rasters[[1]])) {
        print("extent")
        e <- intersect(extent(VHrasterlist.raster.min), extent(input.rasters[[1]]))
        VHrasterlist.raster.min <- crop(VHrasterlist.raster.min, e)
        input.rasters[[1]] <- resample(input.rasters[[1]], VHrasterlist.raster.min,method="ngb")
      
      }
      
      if (compareRaster(VHrasterlist.raster.min,input.rasters[[1]],extent=TRUE,stopiffalse=FALSE) == FALSE) {
        print("extent")
           e <- intersect(extent(VHrasterlist.raster.min), extent(input.rasters[[1]]))
        VHrasterlist.raster.min <- crop(VHrasterlist.raster.min, e)
        input.rasters[[1]] <- resample(input.rasters[[1]], VHrasterlist.raster.min,method="ngb")
  
      }
      gc()
      rast_stack <- stack(input.rasters[[1]],VHrasterlist.raster.min)
      fun_raster <- function(x) {
        if (is.na(x[2])==FALSE) {
          if (is.na(x[1])==FALSE) {
            if (x[2]>min5) {
              if (x[1] %in% c(1,2)) {
                result <- x[1]
              } else {
                result <- 7
              }
            } else {
              if (x[1] %in% c(1,2,3,4,5)) {
                result <- x[1]
              } else {
                result <- 6
              }
            }
          } else {
            result <- NA
          }
        } else {
          result <- NA
        }
        return(result)
      }
      raster_clear <- calc(rast_stack, fun_raster)
      writeRaster(raster_clear, filename=filename_result_paddy_clear, format="GTiff", overwrite=TRUE,dataType="INT1U")
      
      paddy_raster_df <- as.data.frame(raster_clear)
      paddy_raster_df_count <- table(paddy_raster_df)
      area_ha <-paddy_raster_df_count * 10 * 10 / 10000
      write.csv(area_ha, file = filename_csv_paddy_clear)
      
      raster_clear <- NA
      paddy_raster_df <- NA
      VHrasterlist.raster <- NA
      VHrasterlist.raster.min <- NA
      input.rasters <- NA
      gc()
      
      
      
    }
    removeTmpFiles(h=0)
  }
}
