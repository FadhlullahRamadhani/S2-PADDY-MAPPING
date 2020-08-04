
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
mainDir_BAT <- paste("F:/R-Script-DriveF/MOD13Q1/",sep="")
mainDir_SHP_PADDY <- paste("F:/R-Script-DriveF/ML-LS8_S2_S1v1/SHP/paddy_utm",sep="")
temp<-"import arcpy, arcinfo\r\nimport time\r\nimport os\r\nimport math\r\nfrom datetime import datetime\r\n"
temp <- paste(temp, "arcpy.CheckOutExtension(\"spatial\")", "\r\n",sep="")

for (j in 1:length(idkab)){
  region <- idkab[j]
  print(region)
  pr_shp <- paste(mainDir_SHP_PADDY, "/paddy_",region,".shp",sep="")  
  pr_shp <- gsub("/","\\\\\\",pr_shp)
  temp <- paste(temp, "S2_paddy_",region," = \"",pr_shp, "\"\r\n",sep="")
  
  for (i in 1:length(files)){
    filename_original <-files[i]
    fulldate1 <- substr(filename_original, 23, 32)
    fulldate1 <- fulldate1[fulldate1!=""]
    period <- as.Date(fulldate1,"%Y_%m_%d")
    
    
    if (is.na(period)) {
      next
    }
    
    # print(format(Sys.time(), "%a %b %d %X %Y"))
    # print(filename_original)
    
    bands<- c("NDVI","EVI","DOY","QA")
    
    for (j in 1:4) {
      
      band<- bands[j]
      #print(band)
      folder_band <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-REGION-10m/MOD13Q1-",band,"/",sep="")
      filename_band <-paste(folder_band,region,"_",band,"_10m_",filename_original, sep="")
      
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/",sep="")
      dir.create(folder, showWarnings = FALSE)
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-PADDY-10m/",sep="")
      dir.create(folder, showWarnings = FALSE)
      folder <-paste("F:/R-Script-DriveF/MOD13Q1/BANDS-PADDY-10m/MOD13Q1-",band,"/",sep="")
      dir.create(folder, showWarnings = FALSE)
      filename_to <-paste(folder,"PADDY_",region,"_",band,"_10m_",filename_original, sep="")
      
      # setwd(folder_band)
      # print(format(Sys.time(), "%a %b %d %Y %X"))
      
      if (file.exists(filename_to)) {
        next
      }
      
      tif_ori <- paste(filename_band,sep="")  
      tif_ori <- gsub("/","\\\\\\",tif_ori)
      band_tif  <- gsub("\\.","_",band)
      temp <- paste(temp,"from_",band_tif," = \"",tif_ori, "\"\r\n",sep="")
      tif_to <- paste(filename_to,sep="")  
      tif_to <- gsub("/","\\\\\\",tif_to)
      temp <- paste(temp,"to_", band_tif," = \"",tif_to, "\"\r\n",sep="")
      temp <- paste(temp,"print datetime.now().strftime('%Y-%m-%d %H:%M:%S') + ' ",region," BANDS2PADDY creating ->  ' + '", band,"'\r\n",sep="")
      temp <- paste(temp,"try:\r\n",sep="")
      temp <- paste(temp, "\tarcpy.gp.ExtractByMask_sa(","from_",band_tif,", ","S2_paddy_", region ,", ","to_", band_tif ,")", "\r\n",sep="")
      temp <- paste(temp,"except:\r\n",sep="")
      temp <- paste(temp,"\tprint(\"error\")\r\n",sep="")     
    
    }
  }
}

temp <- gsub("\\", "\\\\", temp, fixed = TRUE)
filename <- paste(mainDir_BAT, "/","run2PADDY.py",sep="")
write(temp,file = filename)

rm(list=ls())
gc()
memory.size(max=F)
Sys.sleep(30)
rm(list=ls())
gc()
memory.size(max=F)