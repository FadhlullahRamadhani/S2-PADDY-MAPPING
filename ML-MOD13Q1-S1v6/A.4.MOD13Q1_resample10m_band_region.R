
rm(list=ls())
library(raster)
library(sp)
library(rgdal)


#'B4','B3','B2','B5'
mainDir <- "F:/RS_images/MOD13Q1-JAVA-250m-utm/"
setwd(mainDir)
setwd(file.path(mainDir))

files <- sort(list.files(mainDir,full.names = FALSE,pattern = "\\.tif$"),decreasing = FALSE)

if (length(files)<1) {
  next
}
idkab <- c(3517,3518,3215,3522,3213,3524,3212)
idkab <- sort(idkab,decreasing = FALSE)
model_ML <- c('svmRadialMOD13Q1_S1')


for (j in 1:length(idkab)){
  region <- idkab[j]
  region_kab  <- readOGR(paste0('F:/R-Script-DriveF/ML-LS8_S2_S1v1/SHP/regency_utm/regency_',region,".shp"),verbose = FALSE)
  
  for (i in 1:length(files)){
    filename_original <-files[i]
    fulldate1 <- substr(filename_original, 23, 32)
    fulldate1 <- fulldate1[fulldate1!=""]
    period <- as.Date(fulldate1,"%Y_%m_%d")
    
    
    if (is.na(period)) {
      next
    }
    
    print(format(Sys.time(), "%a %b %d %X %Y"))
    print(filename_original)
    
    bands<- c("NDVI","EVI","DOY","QA")
    
    for (j in 1:4) {
      
      band<- bands[j]
      print(band)
      folder_band <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-REGION/MOD13Q1-",band,"/",sep="")
      filename_band <-paste(folder_band,region,"_",band,"_",filename_original, sep="")
      
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/",sep="")
      dir.create(folder, showWarnings = FALSE)
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-REGION-10m/",sep="")
      dir.create(folder, showWarnings = FALSE)
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-REGION-10m/MOD13Q1-",band,"/",sep="")
      dir.create(folder, showWarnings = FALSE)
      filename_to <-paste(folder,region,"_",band,"_10m_",filename_original, sep="")
      
      if (file.exists(filename_to)) {
        next
      }
      
      setwd(folder_band)
      print(format(Sys.time(), "%a %b %d %Y %X"))

      temp <- paste("\"C:/OSGeo4W64/bin/gdalwarp.exe\" -tr 10 10 ",filename_band," ", filename_to, "  -co COMPRESS=DEFLATE -co TILED=YES --config GDAL_CACHEMAX 1000 --config GDAL_NUM_THREADS 2 ", "\r\n",sep="")
      shell(temp)
      
         
    }
  }
}

rm(list=ls())
gc()
memory.size(max=F)
Sys.sleep(30)
rm(list=ls())
gc()
memory.size(max=F)