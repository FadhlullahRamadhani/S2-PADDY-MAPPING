
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
    folder <-paste("F:/R-Script-DriveF/MOD13Q1/",sep="")
    dir.create(folder, showWarnings = FALSE)
    folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS/",sep="")
    dir.create(folder, showWarnings = FALSE)
    folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS/MOD13Q1-",band,"/",sep="")
    dir.create(folder, showWarnings = FALSE)
    filename <-paste(folder,band,"_",filename_original, sep="")
    if (file.exists(filename)) {
      next
    }
    
    temp_raster <- raster(filename_original,band=j) #B4: Red (0.64 - 0.67 µm)
    
    
    #select(['B1','B2','B3','B4','B5','B6','B7']);
    
    writeRaster(temp_raster, filename=filename, format="GTiff", overwrite=TRUE)
   
  }
}


rm(list=ls())
gc()
memory.size(max=F)
Sys.sleep(30)
rm(list=ls())
gc()
memory.size(max=F)