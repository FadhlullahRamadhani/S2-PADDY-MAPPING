
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

#idkab <- c(3517,3518,3215,3522,3213,3524,3212)
# idkab <- c(3518,3215,3522,3213,3524,3212)
idkab <- c(3212,3213,3215,3518,3517,3522,3524)
idkab <- sort(idkab,decreasing = TRUE)
#idkab <- c(3212,3213,3215)
#idkab <- 3524
Regency_PR_list <- read_excel("C:/MasseyOffice1/Research/R-Script2019/S2_java_pr_2018.xlsx")
model_ML <- c('svmRadialS2')
preName_current ='noProc'
preName_current_string <- "noProc"

for (j in 1:length(idkab)){
  region <- idkab[j]
  PR_list <- sort(Regency_PR_list$PR[Regency_PR_list$IDKAB==region],decreasing = TRUE)
  for (i in 1:length(PR_list)){
    PR <- PR_list[i]
    original_folder <- paste("G:/RS_images/S2Pre/L2A-PADDY/",region,"/",PR,collapse = "",sep = "")
    folders_ori <- sort(list.dirs(original_folder,full.names = FALSE),decreasing = FALSE)
    
    for (k in 1:length(folders_ori)){
      # if (k!=152) {
      #   next
      # }
      folder_current <- folders_ori[k]
      fulldate1 <- substr(folder_current, 12, 12+7)
      fulldate1 <- fulldate1[fulldate1!=""]
      period <- as.Date(fulldate1,"%Y%m%d")
      if (length(period)==0) {
        next
      }
      # 
      if (!(period>as.Date("2017-12-31") & period<as.Date("2018-10-01"))) {
        next
      }

      # if (!(period>=as.Date("2018-07-06") & period<as.Date("2018-07-10"))) {
      #   next
      # }
      
      # if (region==3212) {
      #   if (!(period>=as.Date("2018-08-15") & period<=as.Date("2018-08-20"))) {
      #     next
      #   }
      # } else  if (region==3213) {
      #   if (!(period>=as.Date("2018-08-23") & period<=as.Date("2018-09-02"))) {
      #     next
      #   }
      # } else  if (region==3215) {
      #   if (!(period>=as.Date("2018-08-23") & period<=as.Date("2018-09-07"))) {
      #     next
      #   }
      # } else  if (region==3517) {
      #   if (!(period>=as.Date("2018-07-28") & period<=as.Date("2018-08-07"))) {
      #     next
      #   }
      # } else  if (region==3518) {
      #   if (!(period>=as.Date("2018-08-02") & period<=as.Date("2018-08-07"))) {
      #     next
      #   }
      # } else  if (region==3522) {
      #   if (!(period>=as.Date("2018-07-17") & period<=as.Date("2018-08-02"))) {
      #     next
      #   }
      # } else  if (region==3524) {
      #   if (!(period>=as.Date("2018-07-23") & period<=as.Date("2018-08-02"))) {
      #     next
      #   }
      # }
        
      # if (as.Date(period) < as.Date("2018-06-01")) {
      #   next
      # }
      #check files 8311
      sizeok<-TRUE
      for (checkit in 1:length(PR_list)){
        PRcheckit <- PR_list[checkit]
        folder_1183  <- paste("F:/R-Script-DriveF/S2-JAVA-EVI-UTM/BANDS-1183/",period,"/",region,"/",collapse = "",sep = "")
        filename_tif_1183 <- paste(folder_1183 ,region,"_",PRcheckit, "_", period, "_S2_1183.tif", sep="")
        
        if (file.exists(filename_tif_1183)==TRUE) {
          size <- file.size(filename_tif_1183)/(1000*1000)
          if (size<10) {
            sizeok<-FALSE
            print(paste("error",region," | ",period," | ",PR," | ",size," | ",sep=""))
            break
          }
        } else {
          sizeok<-FALSE
          break
        }
        
      }
      
      if (sizeok==FALSE) {
        next
      }
      
      
      band_folder <- paste("G:/RS_images/S2Pre/L2A-PADDY/",region,"/",PR,"/",folder_current,"/",collapse = "",sep = "")
      setwd(band_folder)
      search_str <- paste("*", PR,".*SCL*",".*[.]tif$",sep = "")
      
      if (length(list.files(pattern=search_str)) == 0) {
        next
      }
      
      # folder_ML_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",collapse = "",sep = "")
      # dir.create(folder_ML_fmask, showWarnings = FALSE)
      # folder_ML_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",period,"/",collapse = "",sep = "")
      # dir.create(folder_ML_fmask, showWarnings = FALSE)
      # 
      # filename_result_ML_fmask <- paste(folder_ML_fmask,region,"_",PR, "_", period, "_S2_ML_LS8_S2_S1v1_classify_fmask.tif", sep="")
      # filename_tif_ML_fmask <- paste(folder_ML_fmask,region,"_", PR, "_",period,"_S2_ML_LS8_S2_S1v1_classify_fmask.tif", sep="")
      # 
      # if (file.exists(filename_tif_ML_fmask)) {
      #   next
      # }
      # 
      # 
      fmask_clip_paddy1 <- lapply(list.files(pattern=search_str), raster)[[1]]
      
      fmask_clip_paddy1_table <- as.data.frame(table(na.omit(as.data.frame(fmask_clip_paddy1))))
      # data_total <- sum(fmask_clip_paddy1_table$Freq)
      # data_ok<-0
      # if (length(fmask_clip_paddy1_table$Freq[fmask_clip_paddy1_table$Var1==4])!=0) {
      #   data_ok <- data_ok + fmask_clip_paddy1_table$Freq[fmask_clip_paddy1_table$Var1==4]
      # }
      # if (length(fmask_clip_paddy1_table$Freq[fmask_clip_paddy1_table$Var1==5])!=0) {
      #   data_ok <- data_ok + fmask_clip_paddy1_table$Freq[fmask_clip_paddy1_table$Var1==5]
      # }
      # if (length(fmask_clip_paddy1_table$Freq[fmask_clip_paddy1_table$Var1==6])!=0) {
      #   data_ok <- data_ok + fmask_clip_paddy1_table$Freq[fmask_clip_paddy1_table$Var1==6]
      # }
      # if (length(fmask_clip_paddy1_table$Freq[fmask_clip_paddy1_table$Var1==7])!=0) {
      #   data_ok <- data_ok + fmask_clip_paddy1_table$Freq[fmask_clip_paddy1_table$Var1==7]
      # }
      
      #      if (x[2]==4 | x[2]==5 | x[2]==6 | x[2]==7) {
      
      # if (data_total>0 ) {
      #   data_avg<- data_ok/data_total * 100  
      #   print(paste(format(Sys.time(), "%a %b %d %Y %X")," | ",region," | ",period," | ",PR," | ",data_avg," | ",sep=""))
      #   if (data_avg<80) {
      #     next
      #   }
      # } else {
      #   next
      # }
      
      
      folder_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",collapse = "",sep = "")
      dir.create(folder_classify_fmask, showWarnings = FALSE)
      folder_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",period,"/",collapse = "",sep = "")
      dir.create(folder_classify_fmask, showWarnings = FALSE)
      
      folder_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",period,"/",model_ML,"/",collapse = "",sep = "")
      dir.create(folder_classify_fmask, showWarnings = FALSE)
      filename_result_classify_fmask <- paste(folder_classify_fmask,region,"_",PR, "_", period, "_S2_ML_LS8_S2_S1v1_classify_fmask_",model_ML,"_","noProc", sep="")
      filename_tif_classify_fmask <- paste(folder_classify_fmask,region,"_", PR, "_",period,"_S2_ML_LS8_S2_S1v1_classify_fmask_",model_ML,"_","noProc",".tif", sep="")
      
      if (file.exists(filename_tif_classify_fmask)) {
        next
      }
      print(filename_tif_classify_fmask)
      
      folder <- paste("F:/ML-S2_S1v1/CLASSIFY/",collapse = "",sep = "")
      dir.create(folder, showWarnings = FALSE)
      folder <- paste("F:/ML-S2_S1v1/CLASSIFY/",period,"",collapse = "",sep = "")
      dir.create(folder, showWarnings = FALSE)
      folder <- paste("F:/ML-S2_S1v1/CLASSIFY/",period,"/",model_ML,"/",collapse = "",sep = "")
      dir.create(folder, showWarnings = FALSE)
      
      filename_result <- paste(folder,region,"_", PR, "_", period, "_S2_ML_LS8_S2_S1v1_classify_",model_ML,"_",preName_current_string, sep="")
      filename_tif <- paste(folder,region,"_", PR, "_", period,"_S2_ML_LS8_S2_S1v1_classify_",model_ML,"_",preName_current_string,".tif", sep="")
      
      if (file.exists(filename_tif)==FALSE) {
        
        band_folder <- paste("G:/RS_images/S2Pre/L2A-PADDY/",region,"/",PR,"/",folder_current,"/",collapse = "",sep = "")
        setwd(band_folder)
        search_str <- paste("*", PR,".*_B04_*",".*[.]tif$",sep = "")
        S2_B04_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B04_raster)==0) {
          next
        }
        
        search_str <- paste("*", PR,".*_B03_*",".*[.]tif$",sep = "")
        S2_B03_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B03_raster)==0) {
          next
        }
        search_str <- paste("*", PR,".*_B02_*",".*[.]tif$",sep = "")
        S2_B02_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B02_raster)==0) {
          next
        }
        search_str <- paste("*", PR,".*_B08_*",".*[.]tif$",sep = "")
        S2_B08_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B08_raster)==0) {
          next
        }
        search_str <- paste("*", PR,".*_B11_*",".*[.]tif$",sep = "")
        S2_B11_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B11_raster)==0) {
          next
        }
        search_str <- paste("*", PR,".*_B12_*",".*[.]tif$",sep = "")
        S2_B12_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B12_raster)==0) {
          next
        }
        
        S2_B04_NA <- S2_B04_raster[[1]]
        S2_B04_NA[S2_B04_NA==0] <- NA
        
        S2_B03_NA <- S2_B03_raster[[1]]
        S2_B03_NA[S2_B03_NA==0] <- NA
        
        
        if (S2_B03_NA@ncols * S2_B03_NA@nrows != S2_B04_NA@ncols * S2_B04_NA@nrows) {
          e <- intersect(extent(S2_B03_NA), extent(S2_B04_NA))
          S2_B03_NA <- crop(S2_B03_NA, e)
          S2_B04_NA <- resample(S2_B04_NA, S2_B03_NA)
          print("extent")
        }
        
        S2_B02_NA <- S2_B02_raster[[1]]
        S2_B02_NA[S2_B02_NA==0] <- NA
        
        if (S2_B03_NA@ncols * S2_B03_NA@nrows != S2_B02_NA@ncols * S2_B02_NA@nrows) {
          e <- intersect(extent(S2_B03_NA), extent(S2_B02_NA))
          S2_B03_NA <- crop(S2_B03_NA, e)
          S2_B02_NA <- resample(S2_B02_NA, S2_B03_NA)
          print("extent")
        }
        
        S2_B08_NA <- S2_B08_raster[[1]]
        S2_B08_NA[S2_B08_NA==0] <- NA
        
        if (S2_B08_NA@ncols * S2_B08_NA@nrows != S2_B02_NA@ncols * S2_B02_NA@nrows) {
          e <- intersect(extent(S2_B08_NA), extent(S2_B02_NA))
          S2_B08_NA <- crop(S2_B08_NA, e)
          S2_B02_NA <- resample(S2_B02_NA, S2_B08_NA)
          print("extent")
        }
        
        S2_B11_NA <- S2_B11_raster[[1]]
        S2_B11_NA[S2_B11_NA==0] <- NA
        if (S2_B08_NA@ncols * S2_B08_NA@nrows != S2_B11_NA@ncols * S2_B11_NA@nrows) {
          e <- intersect(extent(S2_B08_NA), extent(S2_B11_NA))
          S2_B08_NA <- crop(S2_B08_NA, e)
          S2_B11_NA <- resample(S2_B11_NA, S2_B08_NA)
          print("extent")
        }
        
        S2_B12_NA <- S2_B12_raster[[1]]
        S2_B12_NA[S2_B12_NA==0] <- NA
        if (S2_B12_NA@ncols * S2_B12_NA@nrows != S2_B11_NA@ncols * S2_B11_NA@nrows) {
          e <- intersect(extent(S2_B12_NA), extent(S2_B11_NA))
          S2_B12_NA <- crop(S2_B12_NA, e)
          S2_B11_NA <- resample(S2_B11_NA, S2_B12_NA)
          print("extent")
        }
        
        S2_B04_raster <- S2_B04_NA
        S2_B04_raster_df <- as.data.frame(S2_B04_raster)
        S2_B04_raster_df_template <- as.data.frame(S2_B04_raster)
        S2_B04_raster_df[is.na(S2_B04_raster_df)] <- -999
        
        S2_B03_raster <- S2_B03_NA
        S2_B03_raster_df <- as.data.frame(S2_B03_raster)
        S2_B03_raster_df_template <- as.data.frame(S2_B03_raster)
        S2_B03_raster_df[is.na(S2_B03_raster_df)] <- -999
        
        S2_B02_raster <- S2_B02_NA
        S2_B02_raster_df <- as.data.frame(S2_B02_raster)
        S2_B02_raster_df_template <- as.data.frame(S2_B02_raster)
        S2_B02_raster_df[is.na(S2_B02_raster_df)] <- -999
        
        S2_B08_raster <- S2_B08_NA
        S2_B08_raster_df <- as.data.frame(S2_B08_raster)
        S2_B08_raster_df_template <- as.data.frame(S2_B08_raster)
        S2_B08_raster_df[is.na(S2_B08_raster_df)] <- -999
        
        S2_B11_raster <- S2_B11_NA
        S2_B11_raster_df <- as.data.frame(S2_B11_raster)
        S2_B11_raster_df_template <- as.data.frame(S2_B11_raster)
        S2_B11_raster_df[is.na(S2_B11_raster_df)] <- -999
        
        S2_B12_raster <- S2_B12_NA
        S2_B12_raster_df <- as.data.frame(S2_B12_raster)
        S2_B12_raster_df_template <- as.data.frame(S2_B12_raster)
        S2_B12_raster_df[is.na(S2_B12_raster_df)] <- -999
        
        search_str <- paste("*", PR,".*_B05_*",".*[.]tif$",sep = "")
        S2_B05_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B05_raster)==0) {
          next
        }
        
        
        S2_B05_NA <- S2_B05_raster[[1]]
        S2_B05_NA[S2_B05_NA==0] <- NA
        if (S2_B12_NA@ncols * S2_B12_NA@nrows != S2_B05_NA@ncols * S2_B05_NA@nrows) {
          e <- intersect(extent(S2_B12_NA), extent(S2_B05_NA))
          S2_B12_NA <- crop(S2_B12_NA, e)
          S2_B05_NA <- resample(S2_B05_NA, S2_B12_NA)
          print("extent")
        }
        
        
        S2_B05_raster <- S2_B05_NA
        S2_B05_raster_df <- as.data.frame(S2_B05_raster)
        S2_B05_raster_df_template <- as.data.frame(S2_B05_raster)
        S2_B05_raster_df[is.na(S2_B05_raster_df)] <- -999
        
        search_str <- paste("*", PR,".*_B06_*",".*[.]tif$",sep = "")
        S2_B06_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B06_raster)==0) {
          next
        }
        
        
        S2_B06_NA <- S2_B06_raster[[1]]
        S2_B06_NA[S2_B06_NA==0] <- NA
        
        if (S2_B06_NA@ncols * S2_B06_NA@nrows != S2_B05_NA@ncols * S2_B05_NA@nrows) {
          e <- intersect(extent(S2_B06_NA), extent(S2_B05_NA))
          S2_B06_NA <- crop(S2_B06_NA, e)
          S2_B05_NA <- resample(S2_B05_NA, S2_B06_NA)
          print("extent")
        }
        
        S2_B06_raster <- S2_B06_NA
        S2_B06_raster_df <- as.data.frame(S2_B06_raster)
        S2_B06_raster_df_template <- as.data.frame(S2_B06_raster)
        S2_B06_raster_df[is.na(S2_B06_raster_df)] <- -999
        
        search_str <- paste("*", PR,".*_B07_*",".*[.]tif$",sep = "")
        S2_B07_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B07_raster)==0) {
          next
        }
        
        
        S2_B07_NA <- S2_B07_raster[[1]]
        S2_B07_NA[S2_B07_NA==0] <- NA
        if (S2_B06_NA@ncols * S2_B06_NA@nrows != S2_B07_NA@ncols * S2_B07_NA@nrows) {
          e <- intersect(extent(S2_B06_NA), extent(S2_B07_NA))
          S2_B06_NA <- crop(S2_B06_NA, e)
          S2_B07_NA <- resample(S2_B07_NA, S2_B06_NA)
          print("extent")
        }
        
        
        S2_B07_raster <- S2_B07_NA
        S2_B07_raster_df <- as.data.frame(S2_B07_raster)
        S2_B07_raster_df_template <- as.data.frame(S2_B07_raster)
        S2_B07_raster_df[is.na(S2_B07_raster_df)] <- -999
        
        search_str <- paste("*", PR,".*_B8A_*",".*[.]tif$",sep = "")
        S2_B8A_raster <- lapply(list.files(pattern=search_str), raster)
        if (length(S2_B05_raster)==0) {
          next
        }
        
        
        S2_B8A_NA <- S2_B8A_raster[[1]]
        S2_B8A_NA[S2_B8A_NA==0] <- NA
        if (S2_B8A_NA@ncols * S2_B8A_NA@nrows != S2_B07_NA@ncols * S2_B07_NA@nrows) {
          e <- intersect(extent(S2_B8A_NA), extent(S2_B07_NA))
          S2_B8A_NA <- crop(S2_B8A_NA, e)
          S2_B07_NA <- resample(S2_B07_NA, S2_B8A_NA)
          print("extent")
        }
        
        
        S2_B8A_raster <- S2_B8A_NA
        S2_B8A_raster_df <- as.data.frame(S2_B8A_raster)
        S2_B8A_raster_df_template <- as.data.frame(S2_B8A_raster)
        S2_B8A_raster_df[is.na(S2_B8A_raster_df)] <- -999
        
        
        JAVA_classify_df <- data.frame(S2_B02=S2_B02_raster_df,S2_B03=S2_B03_raster_df,S2_B04=S2_B04_raster_df,
                                       S2_B05=S2_B05_raster_df,S2_B06=S2_B06_raster_df,S2_B07=S2_B07_raster_df,
                                       S2_B08=S2_B08_raster_df,S2_B11=S2_B11_raster_df,S2_B12=S2_B12_raster_df,S2_B8A=S2_B8A_raster_df,
                                       stringsAsFactors=FALSE)
        
        colnames(JAVA_classify_df) <- c("S2_B02","S2_B03","S2_B04","S2_B05","S2_B06","S2_B07","S2_B08","S2_B11","S2_B12","S2_B8A")
        # S2_B02	S2_B03	S2_B04	S2_B05	S2_B06	S2_B07	S2_B08	S2_B11	S2_B12	S2_B8A
        
        
        S2_B04_NA <- NA
        S2_B04_raster_df <- NA
        
        S2_B03_NA <- NA
        S2_B03_raster <- NA
        S2_B03_raster_df <- NA
        S2_B03_raster_df_template <- NA
        
        S2_B02_NA <- NA
        S2_B02_raster <- NA
        S2_B02_raster_df <- NA
        S2_B02_raster_df_template <- NA
        
        S2_B08_NA <- NA
        S2_B08_raster <- NA
        S2_B08_raster_df <- NA
        S2_B08_raster_df_template <- NA
        
        S2_B11_NA <- NA
        
        S2_B11_raster_df <- NA
        S2_B11_raster_df_template <- NA
        
        S2_B12_NA <- NA
        S2_B12_raster <- NA
        S2_B12_raster_df <- NA
        S2_B12_raster_df_template <- NA
        
        gc()

       

        folder_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",collapse = "",sep = "")
        dir.create(folder_classify_fmask, showWarnings = FALSE)
        folder_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",period,"/",collapse = "",sep = "")
        dir.create(folder_classify_fmask, showWarnings = FALSE)
        folder_classify_fmask <- paste("F:/ML-S2_S1v1/CLASSIFY-MASK/",period,"/",model_ML,"/",collapse = "",sep = "")
        dir.create(folder_classify_fmask, showWarnings = FALSE)
        filename_result_classify_fmask <- paste(folder_classify_fmask,region,"_",PR, "_", period, "_S2_ML_LS8_S2_S1v1_classify_fmask_",model_ML,"_",preName_current_string, sep="")
        filename_tif_classify_fmask <- paste(folder_classify_fmask,region,"_", PR, "_",period,"_S2_ML_LS8_S2_S1v1_classify_fmask_",model_ML,"_",preName_current_string,".tif", sep="")
        
        if (file.exists(filename_tif_classify_fmask)) {
          next
        }
        
        
        mainDir1 <- paste("F:/ML-S2_S1v1/ML_LS8_S1_LEE_S2_2019_5class_surveyV2/svmRadial/",collapse = "",sep = "")
        
        filename <- paste(mainDir1,"WithMasking_LOOCV_ML_LS8_S1_LEE_S2_2019_5class_surveyV2_svmRadial_noProc_0001.rds", sep="")
        
        allModelsResults <- readRDS(filename)
        print ("predicting")
        
        
        size_df <-  dim(JAVA_classify_df)[1]
        
        #check memory
        
        if (get_free_ram()<1000000) {
          print(get_free_ram())
          print("sleep 60s")
          Sys.sleep(60)
          next
        }
        
        size_df <-  dim(JAVA_classify_df)[1]
        divided <- as.integer(size_df/10)
        for (it in 1:10) {
          size_df_0 <- (it-1)*(divided)+1
          size_df_1 <- (it)*(divided)
          print (paste(format(Sys.time(), "%a %b %d %Y %X"),"-->predicting",it,"==",size_df_0,"-",size_df_1,sep=""))
          if (it==1){
            result1 <- predict(allModelsResults,JAVA_classify_df[size_df_0:size_df_1,])
            allModelsResults_predictions <- result1
          } else {
            result1 <- predict(allModelsResults,JAVA_classify_df[size_df_0:size_df_1,])
            allModelsResults_predictions[size_df_0:size_df_1] <- predict(allModelsResults,JAVA_classify_df[size_df_0:size_df_1,])
          }
        }
        
        JAVA_classify_df <- NA
        
        
        #allModelsResults_predictions_coded <- match(allModelsResults_predictions, cbind("Non rice","Vegetative","Reproductive","Ripening","Flooding"))
        allModelsResults_predictions_coded <- allModelsResults_predictions
        #encoded
        
        #unique(allModelsResults_predictions)
        
        allModelsResults_predictions_filter <- S2_B8A_raster_df_template
        
        allModelsResults_predictions_filter[!is.na(S2_B8A_raster_df_template)] <- allModelsResults_predictions_coded[!is.na(S2_B8A_raster_df_template)]
        allModelsResults_predictions_coded <- NA
        
        allModelsResults_predictions_filter <- as.matrix(allModelsResults_predictions_filter)
        
        result_temp <- raster(S2_B8A_raster[[1]])
        
        nrows=S2_B04_raster[[1]]@nrows
        ncols=S2_B04_raster[[1]]@ncols
        
        temp1<-matrix(allModelsResults_predictions_filter,nrows,ncols,byrow=TRUE)
        allModelsResults_predictions_filter <- NA
        temp2 <- raster(temp1)
        extent(temp2) <- S2_B04_raster[[1]]
        projection(temp2) <- projection(S2_B8A_raster[[1]])
        raster_classify <- temp2
        
        
        temp1 <- NA
        temp2 <- NA
        writeRaster(raster_classify, filename=filename_result, format="GTiff", overwrite=TRUE)
        
      } else {
        raster_classify <- raster(filename_tif)
      }
      gc()
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
        if (is.na(x[2])==FALSE) {
          if (x[2]==4 | x[2]==5 | x[2]==6 | x[2]==2 | x[2]==7) {
            result <- x[1]
          } else {
            result <- x[2] * 10
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
      raster_classify_fmask <- NA
      gc()
      
    }
  }
}



