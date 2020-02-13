
rm(list=ls())
library(raster)
library(sp)
library(rgdal)
library(ggplot2)

setwd("C:/MasseyOffice1/Research/R-Script2019/")
#'B4','B3','B2','B5'
library(readxl)
idkab <- c(3212,3213,3215,3518,3517,3522,3524)
idkab <- sort(idkab,decreasing = TRUE)
#idkab <-3215
Regency_PR_list <- read_excel("C:/MasseyOffice1/Research/R-Script2019/S2_java_pr_2018.xlsx")

mainDir_L2A_TIF <- paste("G:/RS_images/S2Pre/L2A-TIF",sep="")
mainDir_L2A_PADDY <- paste("G:/RS_images/S2Pre/L2A-PADDY",sep="")
mainDir_BAT <- paste("G:/RS_images/S2Pre/bat",sep="")
mainDir_SHP_PADDY <- paste("G:/RS_images/S2Pre/shp/paddy_utm",sep="")
temp<-"import arcpy, arcinfo\r\nimport time\r\nimport os\r\nimport math\r\nfrom datetime import datetime\r\n"
temp <- paste(temp, "arcpy.CheckOutExtension(\"spatial\")", "\r\n",sep="")

for (j in 1:length(idkab)){
  regency <- idkab[j]
  PR_list <- sort(Regency_PR_list$PR[Regency_PR_list$IDKAB==regency],decreasing = TRUE)
  folder <-paste(mainDir_L2A_PADDY, "/", regency ,sep="")
  dir.create(folder, showWarnings = FALSE)
  #PR_list <- "T48MYT"
  for (PR in PR_list) {
    print(PR)  
    mainDir_L2A_TIF_PR <- paste(mainDir_L2A_TIF,"/",PR,sep="")
    L2A_TIF_dir <- as.data.frame(list.dirs(mainDir_L2A_TIF_PR,recursive = FALSE), stringsAsFactors = FALSE)
    L2A_TIF_dir_short <- as.data.frame(list.dirs(mainDir_L2A_TIF_PR,recursive = FALSE,full.names = FALSE), stringsAsFactors = FALSE)
    L2A_TIF_dir_short$name <- L2A_TIF_dir_short$`list.dirs(mainDir_L2A_TIF_PR, recursive = FALSE, full.names = FALSE)`
    L2A_TIF_dir_short$PR <- substring(L2A_TIF_dir_short$name,6,10)
  
    pr_shp <- paste(mainDir_SHP_PADDY, "/paddy_",regency,".shp",sep="")  
    pr_shp <- gsub("/","\\\\\\",pr_shp)
    temp <- paste(temp, "S2_paddy_",PR," = \"",pr_shp, "\"\r\n",sep="")
    folder <-paste(mainDir_L2A_PADDY, "/", regency, "/", PR ,sep="")
    dir.create(folder, showWarnings = FALSE)
    
    for (dir in L2A_TIF_dir_short$name) {
      fulldate1 <- substr(dir,12,19)
      period <- as.Date(fulldate1,"%Y%m%d")
      if (!(period>as.Date("2019-05-31") & period<as.Date("2020-10-01"))) {
        next
      }
      print(period)
      folder <-paste(mainDir_L2A_PADDY, "/", regency, "/", PR , "/", dir ,sep="")
      dir.create(folder, showWarnings = FALSE)
      
      folder_ori <- paste(mainDir_L2A_TIF, "/" , PR, "/", dir ,sep="")
      files <- list.files(pattern = glob2rx(paste("*.tif",sep = "")),path=folder_ori)
      list_ok <- c("B02","B03","B04","B05","B06","B07","B08","B8A","B11","B12","SCL","TCI")

      for (band in files) {
        name_band <-substring(band, 28,30)
        if (name_band %in% list_ok) {
        setwd(folder)
        if (file.exists(band)==FALSE) {
          print(folder_ori)
          print(format(Sys.time(), "%a %b %d %Y %X"))
          
          tif_ori <- paste(folder_ori, "/",band,sep="")  
          tif_ori <- gsub("/","\\\\\\",tif_ori)
          band_tif  <- gsub("\\.","_",band)
          temp <- paste(temp,"from_",band_tif," = \"",tif_ori, "\"\r\n",sep="")
          tif_to <- paste(folder, "/",band,sep="")  
          tif_to <- gsub("/","\\\\\\",tif_to)
          temp <- paste(temp,"to_", band_tif," = \"",tif_to, "\"\r\n",sep="")
          temp <- paste(temp,"print datetime.now().strftime('%Y-%m-%d %H:%M:%S') + ' BANDS2PADDY creating ->  ' + '", band,"'\r\n",sep="")
          
          temp <- paste(temp, "arcpy.gp.ExtractByMask_sa(","from_",band_tif,", ","S2_paddy_", PR ,", ","to_", band_tif ,")", "\r\n",sep="")      }
        }
      }
    }
  }
}
temp <- gsub("\\", "\\\\", temp, fixed = TRUE)
filename <- paste(mainDir_BAT, "/","runTIF2PADDY.py",sep="")
write(temp,file = filename)


