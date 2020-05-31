
rm(list=ls())
gc()
memory.size(max=F)

rm(list=ls())
gc()
memory.size(max=F)
rm(list=ls(all=TRUE))
gc(reset=TRUE)
library(raster)
library(rgdal)


library(readxl)

get_free_ram <- function(){
  if(Sys.info()[["sysname"]] == "Windows"){
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    as.integer(x)
  } else {
    stop("Only supported on Windows OS")
  }
}

detachAllPackages <- function() {
  
  basic.packages <- c("package:ggplot2","package:sp","package:rgdal","package:raster","package:lattice","package:caret","package:foreach","package:iterators","package:parallel","package:doParallel","package:reshape","package:plyr","package:utils","package:stats","package:graphics","package:grDMLces","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

library(readxl)


idkab <- "3522"
model_ML <- c('svmRadialMOD13Q1_S1')


for (j in 1:length(idkab)){
  region <- idkab[j]
  
  original_folder_modis <- "F:/RS_images/MOD13Q1-JAVA-250m-utm/"
  folders_ori <- sort(list.files(original_folder_modis,full.names = FALSE,pattern = "\\.tif$"),decreasing = FALSE)
  
  #for (k in 1:1){
  for (k in 1:length(folders_ori)){
    # if (k!=152) {
    #   next
    # }
    file_current <- folders_ori[k]
    if (is.na(file_current)==TRUE) {
      next
    }
    fulldate1 <- substr(file_current, 23, 32)
    fulldate1 <- fulldate1[fulldate1!=""]
    period <- as.Date(fulldate1,"%Y_%m_%d")
    if (length(period)==0) {
      next
    }
    
    # 
    if (!(period>=as.Date("2018-06-01") & period<as.Date("2018-10-01"))) {
      next
    }
    
    
    folder_classify_fmask <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK/",collapse = "",sep = "")
    dir.create(folder_classify_fmask, showWarnings = FALSE)
    folder_classify_fmask <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK/",period,"/",collapse = "",sep = "")
    dir.create(folder_classify_fmask, showWarnings = FALSE)
    
    folder_classify_fmask <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK/",period,"/",model_ML,"/",collapse = "",sep = "")
    dir.create(folder_classify_fmask, showWarnings = FALSE)
    filename_result_classify_fmask <- paste(folder_classify_fmask,region,"_", period, "_MLv6_classify_fmask_",model_ML, sep="")
    filename_tif_classify_fmask <- paste(folder_classify_fmask,region,"_",period,"_MLv6_classify_fmask_",model_ML,".tif", sep="")
    
    
    print(filename_tif_classify_fmask)
    
    folder <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY/",collapse = "",sep = "")
    dir.create(folder, showWarnings = FALSE)
    folder <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY/",period,"",collapse = "",sep = "")
    dir.create(folder, showWarnings = FALSE)
    folder <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY/",period,"/",model_ML,"/",collapse = "",sep = "")
    dir.create(folder, showWarnings = FALSE)
    
    filename_result <- paste(folder,region,"_", period, "_MLv6_classify_",model_ML, sep="")
    filename_tif <- paste(folder,region,"_", period,"_MLv6_classify_",model_ML,".tif", sep="")
    
    if (file.exists(filename_tif)==FALSE) {
      
      
      
      band_VH <- paste(paste("F:/R-Script-DriveF/S1-ML-2019/BANDS-LEE-PADDY-VH/",region,sep=""),collapse = "",sep = "")
      files_VH <- sort(list.files(band_VH,full.names = FALSE,pattern = "\\.tif$"),decreasing = FALSE)

      VHrasterlist <- NA
      limit <- 10
      int1 <- 1
      setwd(band_VH)
      tempDate<- as.Date("2018-01-01")
      for (intVh in 1:length(files_VH)) {
        period1 <- substr(files_VH[intVh],32,39)
        # period1 <- substr(files_VH[intVh],56,63)
        # period1 <- substr(files_VH[intVh],37,44)
        date_VH <- as.Date(period1,"%Y%m%d")
        
        if ((date_VH>=period-20) & (as.Date(date_VH) <= as.Date(period+10))) {
          #print(date_VH)
          check_raster <- raster(files_VH[intVh])
          if (minValue(check_raster)<1.797693e+308 & date_VH!=tempDate & abs(date_VH-tempDate)>10) {
            VHrasterlist[int1] <- files_VH[intVh]
            tempDate <- date_VH
            int1 <- int1 + 1
          }
        }
      }
      print(VHrasterlist)
      
      raster_TIME0 <- raster(VHrasterlist[2])
      raster_TIME1 <- raster(VHrasterlist[1])
   
      #VHrasterlist <- lapply(VHrasterlist, raster)
      # raster_TIME0 <-  merge(VHrasterlist[[4]],VHrasterlist[[5]],VHrasterlist[[6]],tolerance=10)
      # 
      # raster_TIME1 <- merge(VHrasterlist[[1]],VHrasterlist[[2]],VHrasterlist[[3]],tolerance=10)
      # 
      
      raster_TIME0 <- raster(VHrasterlist[2])
      raster_TIME1 <- raster(VHrasterlist[1])
      
      band <- "NDVI"
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-PADDY-10m/MOD13Q1-",band,"/",sep="")
      dir.create(folder, showWarnings = FALSE)
      filename_to <-paste(folder,"PADDY_",region,"_",band,"_10m_",file_current, sep="")
      raster_NDVI <- raster(filename_to)
      
      band <- "EVI"
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-PADDY-10m/MOD13Q1-",band,"/",sep="")
      dir.create(folder, showWarnings = FALSE)
      filename_to <-paste(folder,"PADDY_",region,"_",band,"_10m_",file_current, sep="")
      raster_EVI <- raster(filename_to)
      
      
      
      
      
      if (raster_EVI@ncols * raster_EVI@nrows != raster_TIME0@ncols * raster_TIME0@nrows) {
        print("extent")
        e <- intersect(extent(raster_EVI), extent(raster_TIME0))
        raster_EVI <- crop(raster_EVI, e)
        raster_TIME0 <- resample(raster_TIME0, raster_EVI, method="ngb")
      }
      
      
      
      print("extent")
      if (raster_EVI@ncols * raster_EVI@nrows != raster_TIME1@ncols * raster_TIME1@nrows) {
        e <- intersect(extent(raster_EVI), extent(raster_TIME1))
        raster_EVI <- crop(raster_EVI, e)
        raster_TIME1 <- resample(raster_TIME1, raster_EVI, method="ngb")
        
      }
      
      raster_TIME0[raster_TIME0==0] <- NA
      raster_TIME0_df <- as.data.frame(raster_TIME0[[1]])
      raster_TIME0_df[is.na(raster_TIME0_df)] <- -999
      
      raster_TIME1[raster_TIME1==0] <- NA
      raster_TIME1_df <- as.data.frame(raster_TIME1[[1]])
      raster_TIME1_df[is.na(raster_TIME1_df)] <- -999
      
      
      raster_NDVI[raster_NDVI==0] <- NA
      raster_NDVI_df <- as.data.frame(raster_NDVI[[1]])
      raster_NDVI_df_template <- as.data.frame(raster_NDVI[[1]])
      raster_NDVI_df <- raster_NDVI_df/10000
      raster_NDVI_df[is.na(raster_NDVI_df)] <- -999
      
      raster_EVI[raster_EVI==0] <- NA
      raster_EVI_df <- as.data.frame(raster_EVI[[1]])
      raster_EVI_df <- raster_EVI_df/10000
      raster_EVI_df[is.na(raster_EVI_df)] <- -999
      
      JAVA_classify_df <- data.frame(NDVI=raster_NDVI_df,EVI=raster_EVI_df,
                                     TIME0=raster_TIME0_df,TIME1=raster_TIME1_df,
                                     stringsAsFactors=FALSE)
      
      colnames(JAVA_classify_df) <- c("NDVI","EVI","TIME0","TIME1")
      
      
      
      raster_NDVI_df <- NA
      
      raster_EVI <- NA
      raster_EVI_df <- NA
      raster_TIME0_df<-NA
      raster_TIME1_df<-NA
      gc()
      
      
      
      folder_classify_fmask <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK/",collapse = "",sep = "")
      dir.create(folder_classify_fmask, showWarnings = FALSE)
      folder_classify_fmask <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK/",period,"/",collapse = "",sep = "")
      dir.create(folder_classify_fmask, showWarnings = FALSE)
      folder_classify_fmask <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK/",period,"/",model_ML,"/",collapse = "",sep = "")
      dir.create(folder_classify_fmask, showWarnings = FALSE)
      filename_result_classify_fmask <- paste(folder_classify_fmask,region,"_", period, "_MLv6_classify_fmask_",model_ML, sep="")
      filename_tif_classify_fmask <- paste(folder_classify_fmask,region,"_",period,"_MLv6_classify_fmask_",model_ML,".tif", sep="")
      
      # if (file.exists(filename_tif_classify_fmask)) {
      #   next
      # }
      
      
      mainDir1 <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/S1_MOD13Q1_ML_v6/svmRadial/",collapse = "",sep = "")
      
      filename <- paste(mainDir1,"WithMasking_LOOCV_S1_MOD13Q1_ML_v6_svmRadial_noProc_0014.rds", sep="")
      
      allModelsResults <- readRDS(filename)
      print ("predicting")
      
      
      size_df <-  dim(JAVA_classify_df)[1]
      
      #check memory
      
      if (get_free_ram()<1000000) {
        print(get_free_ram())
        print("sleep 60s")
        Sys.sleep(60)
        gc()
        next
      }
      
      size_df <-  dim(JAVA_classify_df)[1]
      divided <- as.integer(size_df/30)
      for (it in 1:30) {
        size_df_0 <- (it-1)*(divided)+1
        size_df_1 <- (it)*(divided)
        #print (paste(format(Sys.time(), "%a %b %d %Y %X"),"-->predicting",it,"==",size_df_0,"-",size_df_1,sep=""))
        if (it==1){
          result1 <- predict(allModelsResults,JAVA_classify_df[size_df_0:size_df_1,])
          allModelsResults_predictions <- result1
        } else {
          result1 <- predict(allModelsResults,JAVA_classify_df[size_df_0:size_df_1,])
          allModelsResults_predictions[size_df_0:size_df_1] <- predict(allModelsResults,JAVA_classify_df[size_df_0:size_df_1,])
        }
      }
      
      JAVA_classify_df <- NA
      gc()
      
      #allModelsResults_predictions_coded <- match(allModelsResults_predictions, cbind("Non rice","Vegetative","Reproductive","Ripening","Flooding"))
      allModelsResults_predictions_coded <- allModelsResults_predictions
      #encoded
      
      #unique(allModelsResults_predictions)
      
      allModelsResults_predictions_filter <- raster_NDVI_df_template
      
      allModelsResults_predictions_filter[!is.na(raster_NDVI_df_template)] <- allModelsResults_predictions_coded[!is.na(raster_NDVI_df_template)]
      allModelsResults_predictions_coded <- NA
      
      allModelsResults_predictions_filter <- as.matrix(allModelsResults_predictions_filter)
      
      result_temp <- raster(raster_NDVI[[1]])
      
      nrows=raster_NDVI[[1]]@nrows
      ncols=raster_NDVI[[1]]@ncols
      
      temp1<-matrix(allModelsResults_predictions_filter,nrows,ncols,byrow=TRUE)
      allModelsResults_predictions_filter <- NA
      temp2 <- raster(temp1)
      extent(temp2) <- raster_NDVI[[1]]
      projection(temp2) <- projection(raster_NDVI[[1]])
      raster_classify <- temp2
      
      
      temp1 <- NA
      temp2 <- NA
      writeRaster(raster_classify, filename=filename_result, format="GTiff", overwrite=TRUE)
      
    } else {
      raster_classify <- raster(filename_tif)
    }
    
    if (file.exists(filename_tif_classify_fmask)==FALSE){
      gc()
      print("mask")
      band <- "QA"
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-PADDY-10m/MOD13Q1-",band,"/",sep="")
      dir.create(folder, showWarnings = FALSE)
      filename_to <-paste(folder,"PADDY_",region,"_",band,"_10m_",file_current, sep="")
      fmask_clip_paddy1 <- raster(filename_to)
      
      
      
      if (raster_classify@ncols * raster_classify@nrows != fmask_clip_paddy1@ncols * fmask_clip_paddy1@nrows) {
        e <- intersect(extent(raster_classify), extent(fmask_clip_paddy1))
        raster_classify <- crop(raster_classify, e)
        fmask_clip_paddy1 <- resample(fmask_clip_paddy1, raster_classify)
        print("extent")
      }
      
      if (extent(raster_classify) != extent(fmask_clip_paddy1)) {
        e <- intersect(extent(raster_classify), extent(fmask_clip_paddy1))
        raster_classify <- crop(raster_classify, e)
        fmask_clip_paddy1 <- resample(fmask_clip_paddy1, raster_classify)
        print("extent")
      }
      
      rast_stack <- stack(raster_classify,fmask_clip_paddy1)
      
      fun_raster <- function(x) {
        if (is.na(x[1])==FALSE) {
          if (is.na(x[2])==FALSE) {
            if (x[2]!=3) {
              result <- x[1]
            } else {
              result <- 99
            }
          } else {
            result <- NA
          }
        } else {
          result <- NA
        }
        return(result)
      }
      #fun_raster(c(1,322))
      raster_classify_fmask <- calc(rast_stack, fun_raster)
      raster_classify <- NA
      rast_stack <- NA
      writeRaster(raster_classify_fmask, filename=filename_result_classify_fmask, format="GTiff", overwrite=TRUE, dataType=INT2U)
      
      gc()
    } else {
      raster_classify_fmask <- raster(filename_tif_classify_fmask)
      print(filename_tif_classify_fmask)
    }
    
    #clear
    folder_classify_fmask_clear <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK-CLEAR/",collapse = "",sep = "")
    dir.create(folder_classify_fmask_clear, showWarnings = FALSE)
    folder_classify_fmask_clear <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK-CLEAR/",period,"/",collapse = "",sep = "")
    dir.create(folder_classify_fmask_clear, showWarnings = FALSE)
    folder_classify_fmask_clear <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK-CLEAR/",period,"/",model_ML,"/",collapse = "",sep = "")
    dir.create(folder_classify_fmask_clear, showWarnings = FALSE)
    filename_result_classify_fmask_clear <- paste(folder_classify_fmask_clear,region,"_", period, "_MLv6_classify_fmask_clear_",model_ML, sep="")
    filename_tif_classify_fmask_clear <- paste(folder_classify_fmask_clear,region,"_",period,"_MLv6_classify_fmask_clear_",model_ML,".tif", sep="")
    
    if (file.exists(filename_tif_classify_fmask_clear)==FALSE){
      gc()
      print("clear")
      
      model_ML_S2 <- "svmRadialS2"
      findS2 <- 0
      for (intS2 in -4:4) {
        period_S2 <- period + intS2
        folder_clip_paddy <- paste("F:/R-Script-DriveF/ML-LS8_S2_S1v1/CLASSIFY-MASK-PADDY-CLEARv4/",period_S2,"/",model_ML_S2,"/",collapse = "",sep = "")
        temp <- paste(folder_clip_paddy,region,"_", period_S2,"_S2_ML_clearv4_",model_ML_S2,".tif", sep="")
        if (file.exists(temp)==TRUE) {
          findS2 <-temp
        }
      }
      
      filename_tif_paddy_clear <- findS2
      print(filename_tif_paddy_clear)
      clear_S2 <- raster(filename_tif_paddy_clear)
      
      raster_classify_fmask <- projectRaster(raster_classify_fmask, crs=crs(clear_S2), method = "ngb")
      
      if (clear_S2@ncols * clear_S2@nrows != raster_classify_fmask@ncols * raster_classify_fmask@nrows) {
        e <- intersect(extent(clear_S2), extent(raster_classify_fmask))
        clear_S2 <- crop(clear_S2, e)
        raster_classify_fmask <- resample(raster_classify_fmask, clear_S2,method = "ngb")
        print("extent")
      }
      
      
      rast_stack_clear <- stack(clear_S2,raster_classify_fmask)
      
      fun_raster_clear <- function(x) {
        if (is.na(x[1])==FALSE) {
          if (is.na(x[2])==FALSE) {
            if (x[1]==6 & x[2]<=5) {
              result <- x[2]
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
      
      raster_classify_fmask_clear <- calc(rast_stack_clear, fun_raster_clear)
      rast_stack_clear <-NA
      clear_S2 <- NA
      raster_classify_fmask <- NA
      writeRaster(raster_classify_fmask_clear, filename=filename_result_classify_fmask_clear, format="GTiff", overwrite=TRUE, dataType=INT2U)
    }
  }
}



