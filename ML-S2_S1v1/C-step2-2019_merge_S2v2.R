rm(list=ls())
library(raster)
library(rgdal)
library(readxl)
multisources <- read_excel("C:/MasseyOffice1/Research/R-Script/multisources.xlsx")
multisources$period <- gsub("'", '', multisources$period)
temp_s2 <- multisources[multisources$satellite=="S2",]
period_list <- unique(temp_s2$period)
idkab <- unique(temp_s2$regency)
idkab <- sort(idkab,decreasing = TRUE)
#idkab<-3213
#idkab <- c(3517,3518,3215,3522,3213,3524,3212)
#idkab <- c(3518,3215,3522,3213,3524,3212,3517)
#idkab<-3518
Regency_PR_list <- read_excel("C:/MasseyOffice1/Research/R-Script2019/S2_java_pr_2018.xlsx")
model_ML <- c('svmRadialS2') 
for (j in 1:length(idkab)){ 
  region <- idkab[j]
  folder_ML_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK",collapse = "",sep = "")
  period_list_classify <- sort(list.dirs(folder_ML_classify_fmask,full.names = FALSE,recursive = FALSE),
                               decreasing = FALSE)

  for (int_classify in 1:length(period_list_classify)){
    
  #for (int_classify in seq(1, length(period_list_classify), 3)){  
    period<- period_list_classify[int_classify]
    if (period=="") {
      next
    }
    # if (!(period>as.Date("2018-07-01") & period<as.Date("2018-09-15"))) {
    #   next
    # }
    
    folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY/",collapse = "",sep = "")
    dir.create(folder_clip_paddy, showWarnings = FALSE)
    
    folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY/",period,"/",collapse = "",sep = "")
    dir.create(folder_clip_paddy, showWarnings = FALSE)
    
    folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY/",period,"/",model_ML,"/",collapse = "",sep = "")
    dir.create(folder_clip_paddy, showWarnings = FALSE)
    
    filename_result_paddy <- paste(folder_clip_paddy,region,"_", period,"_S2_ML_clip_paddy_mask_",model_ML, sep="")
    filename_tif_paddy <-    paste(folder_clip_paddy,region,"_", period,"_S2_ML_clip_paddy_mask","_",model_ML,".tif", sep="")
    
    if (file.exists(filename_tif_paddy)) {
      next
    }
    
    
    scenes <- Regency_PR_list$PR[Regency_PR_list$IDKAB==region]
    mainDir <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",period,"/",model_ML,"/",sep="")
    dir.create(mainDir, showWarnings = FALSE)
    
    setwd(mainDir)
    
    iserror=FALSE
    

    #3524_T49MFN_08-29_S2_classify_fmask_rf_noProc.tif
    search_str <- paste(region,"_",scenes[1],".*[.]tif$",sep = "")
    input.rasters <- lapply(list.files(pattern=search_str), raster)
    if (ncell(input.rasters)==0 ) {
      next
    }
    print("loading scenes")
    print(list.files(pattern=search_str))
    
    int_k<-2
    if (length(scenes)>1) {
      for (int_j in 2:length(scenes)){
        search_str <- paste(region,"_",scenes[int_j],".*[.]tif$",sep = "")
        print(list.files(pattern=search_str))
        if (length(list.files(pattern=search_str))>0) {
          input.rasters[int_k] <- lapply(list.files(pattern=search_str), raster)
          int_k <- int_k + 1 
          if (ncell(input.rasters[int_j])==0 ) {
            iserror=TRUE
          }
        }
      }
    }
    
    if (length(scenes)!=ncell(input.rasters)) {
      next
    }
    
    input.rasters<- Filter(Negate(is.null), input.rasters)
    
    if (iserror==TRUE) {
      next
    }
    
    
    if (length(input.rasters)==0) {
      next
    }
    
    if (length(input.rasters)==1) {
      mosaic1 <- input.rasters[[1]]
    } else if (length(input.rasters)==2) {
      mosaic1 <- merge(input.rasters[[1]],input.rasters[[2]],tolerance=10)
    } else if (length(input.rasters)==3) {
      mosaic1 <- merge(input.rasters[[1]],input.rasters[[2]],input.rasters[[3]],tolerance=10)
    } else  {
      mosaic1 <- merge(input.rasters[[1]],input.rasters[[2]],input.rasters[[3]],input.rasters[[4]],tolerance=10)
    }
    
    
    #paddy
    folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY/",collapse = "",sep = "")
    dir.create(folder_clip_paddy, showWarnings = FALSE)
    
    folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY/",period,"/",collapse = "",sep = "")
    dir.create(folder_clip_paddy, showWarnings = FALSE)
    
    folder_clip_paddy <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY/",period,"/",model_ML,"/",collapse = "",sep = "")
    dir.create(folder_clip_paddy, showWarnings = FALSE)
    
    filename_result_paddy <- paste(folder_clip_paddy,region,"_", period,"_S2_ML_clip_paddy_mask_",model_ML, sep="")
    filename_tif_paddy <-    paste(folder_clip_paddy,region,"_", period,"_S2_ML_clip_paddy_mask","_",model_ML,".tif", sep="")
    
    print(format(Sys.time(), "%a %b %d %Y %X"))       
    print(filename_tif_paddy)
    
    writeRaster(mosaic1, filename=filename_result_paddy, format="GTiff", overwrite=TRUE)  
    
    filename_csv_paddy <- paste(folder_clip_paddy,region,"-",period,"_S2_ML_stats_region_utm","_",model_ML,".csv", sep="")
    #
    paddy_raster_df <- as.data.frame(mosaic1)
    paddy_raster_df_count <- table(paddy_raster_df)
    area_ha <-paddy_raster_df_count * 10 * 10 / 10000
    write.csv(area_ha, file = filename_csv_paddy)
    mosaic1 <-NA
    gc()
    # #paddy subdistrict
    # print("Mask subdistrict")
    # folder_clip_paddy_subdistrict <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-SUBDISTRICT/",collapse = "",sep = "")
    # dir.create(folder_clip_paddy_subdistrict, showWarnings = FALSE)
    # folder_clip_paddy_subdistrict <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-SUBDISTRICT/",period,"/",collapse = "",sep = "")
    # dir.create(folder_clip_paddy_subdistrict, showWarnings = FALSE)
    # folder_clip_paddy_subdistrict <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK-PADDY-SUBDISTRICT/",period,"/",model_ML,"/",collapse = "",sep = "")
    # dir.create(folder_clip_paddy_subdistrict, showWarnings = FALSE)
    # filename_csv_paddy_subdistrict <- paste(folder_clip_paddy_subdistrict,region,"-",period,"_S2_ML_stats_region_subdistrict_utm","_",model_ML,".csv", sep="")
    # 
    # #raster subdistrict
    # input.rasters.subdisctrict_resample_filename <-  paste("F:/ML-S2_S1v1/SHP/regency_utm_paddy_raster/regency_utm_paddy_raster_resample_",region,"_S2.tif",collapse = "",sep = "")
    # if (file.exists(input.rasters.subdisctrict_resample_filename)==TRUE){
    #   input.rasters.subdisctrict_resample <-raster(input.rasters.subdisctrict_resample_filename)
    # } else {
    #   input.rasters.subdisctrict <- raster(paste("F:/ML-S2_S1v1/SHP/regency_utm_paddy_raster/regency_utm_paddy_raster_",region,".tif",collapse = "",sep = ""))
    #   input.rasters.subdisctrict_resample <- projectRaster(input.rasters.subdisctrict,mosaic1,method = 'ngb')
    #   writeRaster(input.rasters.subdisctrict_resample, filename=input.rasters.subdisctrict_resample_filename, format="GTiff", overwrite=TRUE)  
    # }
    # 
    # 
    # rast_stack1 <- stack(mosaic1,input.rasters.subdisctrict_resample)
    # 
    # fun_raster1 <- function(x) {
    #   if (is.na(x[1])==FALSE) {
    #     if (is.na(x[2])==FALSE) {
    #       result <-  as.integer(paste(substr(x[2],4,7),x[1],sep = ""))
    #     } else {
    #       result <-  NA
    #     }
    #   } else {
    #     result <-  NA
    #   }
    #   return(result)
    # }
    # 
    # fmask_clip_paddy1_subdistrict <- calc(rast_stack1, fun_raster1)
    # 
    # paddy_raster_df_count_subdistrict <- table(na.omit(as.data.frame(fmask_clip_paddy1_subdistrict)))
    # temp <-paddy_raster_df_count_subdistrict * 10 * 10 / 10000
    # temp2 <- as.data.frame(temp,stringsAsFactors = FALSE)
    # temp2$subdistrict <- as.integer(paste(region,substr(temp2$Var1,2,4),sep=""))
    # temp2$code <- as.integer(substr(temp2$Var1,5,length(temp2$Var1)))
    # temp2$area_ha <- temp2$Freq
    # temp2$Var1 <- NULL
    # temp2$Freq <- NULL
    # write.csv(temp2, file = filename_csv_paddy_subdistrict)
    # paddy_raster_df_subdistrict<-NA
    # 
    gc()
    
    
    
  }
  removeTmpFiles(h=0)
}
