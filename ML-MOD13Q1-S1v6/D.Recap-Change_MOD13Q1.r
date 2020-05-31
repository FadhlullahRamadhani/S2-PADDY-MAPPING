
rm(list=ls())
assign("last.warning", NULL, envir = baseenv())
setwd("C:/MasseyOffice1/Research/R-Script2020/")
#  
#
library(readxl)
area_STATS <- read_excel("C:/MasseyOffice1/Research/R-Script2020/area_kab.xlsx")

idkab <- c(3517,3518,3215,3522,3213,3524,3212)
idkab <- sort(idkab,decreasing = FALSE)
results <- data.frame(
  sensor=character(),
  model=character(),
  period=character(),
  period_first=character(),
  period_prev=character(),
  region=character(),
  prov=character(),
  time_lag=character(),
  area_ha_101=double(),
  area_ha_102=double(),
  area_ha_103=double(),
  area_ha_104=double(),
  area_ha_105=double(),
  area_ha_106=double(),
  area_ha_107=double(),
  area_ha_201=double(),
  area_ha_202=double(),
  area_ha_203=double(),
  area_ha_204=double(),
  area_ha_205=double(),
  area_ha_206=double(),
  area_ha_207=double(),
  area_ha_301=double(),
  area_ha_302=double(),
  area_ha_303=double(),
  area_ha_304=double(),
  area_ha_305=double(),
  area_ha_306=double(),
  area_ha_307=double(),
  area_ha_401=double(),
  area_ha_402=double(),
  area_ha_403=double(),
  area_ha_404=double(),
  area_ha_405=double(),
  area_ha_406=double(),
  area_ha_407=double(),
  area_ha_501=double(),
  area_ha_502=double(),
  area_ha_503=double(),
  area_ha_504=double(),
  area_ha_505=double(),
  area_ha_506=double(),
  area_ha_507=double(),
  area_ha_601=double(),
  area_ha_602=double(),
  area_ha_603=double(),
  area_ha_604=double(),
  area_ha_605=double(),
  area_ha_606=double(),
  area_ha_607=double(),
  area_ha_701=double(),
  area_ha_702=double(),
  area_ha_703=double(),
  area_ha_704=double(),
  area_ha_705=double(),
  area_ha_706=double(),
  area_ha_707=double(),
  area_ha_total_nochange=double(),
  area_ha_total_correct=double(),
  area_ha_total_true=double(),
  area_ha_total_incorrect=double(),
  area_ha_total_all=double(),
  area_RS=double(),
  area_BPS=double(),
  compare_total_RS=double(),
  compare_total_BPS=double(),
  pct_101=double(),
  pct_102=double(),
  pct_103=double(),
  pct_104=double(),
  pct_105=double(),
  pct_106=double(),
  pct_107=double(),
  pct_201=double(),
  pct_202=double(),
  pct_203=double(),
  pct_204=double(),
  pct_205=double(),
  pct_206=double(),
  pct_207=double(),
  pct_301=double(),
  pct_302=double(),
  pct_303=double(),
  pct_304=double(),
  pct_305=double(),
  pct_306=double(),
  pct_307=double(),
  pct_401=double(),
  pct_402=double(),
  pct_403=double(),
  pct_404=double(),
  pct_405=double(),
  pct_406=double(),
  pct_407=double(),
  pct_501=double(),
  pct_502=double(),
  pct_503=double(),
  pct_504=double(),
  pct_505=double(),
  pct_506=double(),
  pct_507=double(),
  pct_601=double(),
  pct_602=double(),
  pct_603=double(),
  pct_604=double(),
  pct_605=double(),
  pct_606=double(),
  pct_607=double(),
  pct_701=double(),
  pct_702=double(),
  pct_703=double(),
  pct_704=double(),
  pct_705=double(),
  pct_706=double(),
  pct_707=double(),
  pct_total_nochange=double(),
  pct_total_correct=double(),
  pct_total_true=double(),
  pct_total_incorrect=double(),
  stringsAsFactors = FALSE
)

time_list <- c(16)

model_ML_list <- c('svmRadialMOD13Q1_S1')
SENSOR_LIST <- c("S2_MOD13Q1_S1")
for (k in 1:length(model_ML_list)){
  model_ML <- model_ML_list[k]
  SENSOR <- SENSOR_LIST[k]
  print(SENSOR)
  for (j in 1:length(idkab)){
    region <- idkab[j]
    print(region)
    area_RS <- area_STATS$area_RS[area_STATS$idkab==region]
    area_BPS <- area_STATS$area_BPS[area_STATS$idkab==region]
    for (time in time_list) {
      print(time)

      folder_clip_paddy <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK-CLEAR-CD/CLASSIFY-MASK-CLEAR-CD-",time,"/",collapse = "",sep = "")
      
      period_list <- sort(list.dirs(folder_clip_paddy,full.names = FALSE,recursive = FALSE),
                          decreasing = FALSE)
      for (period in period_list) {
        print(period)
        period_first <- as.Date(substr(period, 1, 8),"%Y%m%d")
        period_prev <- as.Date(substr(period, 10, 18),"%Y%m%d")
        
        
        if (!(period_first>as.Date("2018-05-31") & period_prev<as.Date("2018-10-01"))) {
          next
        }
        
        
        search_str <- paste(region,".*",model_ML,".*[.]csv$",sep = "")
        mainDir_results_S2_csv <- paste("F:/R-Script-DriveF/ML-MOD13Q1-S1v6/CLASSIFY-MASK-CLEAR-CD/CLASSIFY-MASK-CLEAR-CD-",time,"/",period,"/",model_ML,"/",collapse = "",sep = "")
        file_csv <- list.files(pattern=search_str,path = mainDir_results_S2_csv,full.names = TRUE )
        if (length(file_csv)>0){
          #print(file_csv)
          dat = as.data.frame(read.csv(file_csv, header = TRUE))
          
          area_ha_101 <- NA
          area_ha_102 <- NA
          area_ha_103 <- NA
          area_ha_104 <- NA
          area_ha_105 <- NA
          area_ha_106 <- NA
          area_ha_107 <- NA
          area_ha_201 <- NA
          area_ha_202 <- NA
          area_ha_203 <- NA
          area_ha_204 <- NA
          area_ha_205 <- NA
          area_ha_206 <- NA
          area_ha_207 <- NA
          area_ha_301 <- NA
          area_ha_302 <- NA
          area_ha_303 <- NA
          area_ha_304 <- NA
          area_ha_305 <- NA
          area_ha_306 <- NA
          area_ha_307 <- NA
          area_ha_401 <- NA
          area_ha_402 <- NA
          area_ha_403 <- NA
          area_ha_404 <- NA
          area_ha_405 <- NA
          area_ha_406 <- NA
          area_ha_407 <- NA
          area_ha_501 <- NA
          area_ha_502 <- NA
          area_ha_503 <- NA
          area_ha_504 <- NA
          area_ha_505 <- NA
          area_ha_506 <- NA
          area_ha_507 <- NA
          area_ha_601 <- NA
          area_ha_602 <- NA
          area_ha_603 <- NA
          area_ha_604 <- NA
          area_ha_605 <- NA
          area_ha_606 <- NA
          area_ha_607 <- NA
          area_ha_701 <- NA
          area_ha_702 <- NA
          area_ha_703 <- NA
          area_ha_704 <- NA
          area_ha_705 <- NA
          area_ha_706 <- NA
          area_ha_707 <- NA
          
          area_ha_101<- dat$Freq[dat$paddy_raster_df==101]
          area_ha_102<- dat$Freq[dat$paddy_raster_df==102]
          area_ha_103<- dat$Freq[dat$paddy_raster_df==103]
          area_ha_104<- dat$Freq[dat$paddy_raster_df==104]
          area_ha_105<- dat$Freq[dat$paddy_raster_df==105]
          area_ha_106<- dat$Freq[dat$paddy_raster_df==106]
          area_ha_107<- dat$Freq[dat$paddy_raster_df==107]
          area_ha_201<- dat$Freq[dat$paddy_raster_df==201]
          area_ha_202<- dat$Freq[dat$paddy_raster_df==202]
          area_ha_203<- dat$Freq[dat$paddy_raster_df==203]
          area_ha_204<- dat$Freq[dat$paddy_raster_df==204]
          area_ha_205<- dat$Freq[dat$paddy_raster_df==205]
          area_ha_206<- dat$Freq[dat$paddy_raster_df==206]
          area_ha_207<- dat$Freq[dat$paddy_raster_df==207]
          area_ha_301<- dat$Freq[dat$paddy_raster_df==301]
          area_ha_302<- dat$Freq[dat$paddy_raster_df==302]
          area_ha_303<- dat$Freq[dat$paddy_raster_df==303]
          area_ha_304<- dat$Freq[dat$paddy_raster_df==304]
          area_ha_305<- dat$Freq[dat$paddy_raster_df==305]
          area_ha_306<- dat$Freq[dat$paddy_raster_df==306]
          area_ha_307<- dat$Freq[dat$paddy_raster_df==307]
          area_ha_401<- dat$Freq[dat$paddy_raster_df==401]
          area_ha_402<- dat$Freq[dat$paddy_raster_df==402]
          area_ha_403<- dat$Freq[dat$paddy_raster_df==403]
          area_ha_404<- dat$Freq[dat$paddy_raster_df==404]
          area_ha_405<- dat$Freq[dat$paddy_raster_df==405]
          area_ha_406<- dat$Freq[dat$paddy_raster_df==406]
          area_ha_407<- dat$Freq[dat$paddy_raster_df==407]
          area_ha_501<- dat$Freq[dat$paddy_raster_df==501]
          area_ha_502<- dat$Freq[dat$paddy_raster_df==502]
          area_ha_503<- dat$Freq[dat$paddy_raster_df==503]
          area_ha_504<- dat$Freq[dat$paddy_raster_df==504]
          area_ha_505<- dat$Freq[dat$paddy_raster_df==505]
          area_ha_506<- dat$Freq[dat$paddy_raster_df==506]
          area_ha_507<- dat$Freq[dat$paddy_raster_df==507]
          area_ha_601<- dat$Freq[dat$paddy_raster_df==601]
          area_ha_602<- dat$Freq[dat$paddy_raster_df==602]
          area_ha_603<- dat$Freq[dat$paddy_raster_df==603]
          area_ha_604<- dat$Freq[dat$paddy_raster_df==604]
          area_ha_605<- dat$Freq[dat$paddy_raster_df==605]
          area_ha_606<- dat$Freq[dat$paddy_raster_df==606]
          area_ha_607<- dat$Freq[dat$paddy_raster_df==607]
          area_ha_701<- dat$Freq[dat$paddy_raster_df==701]
          area_ha_702<- dat$Freq[dat$paddy_raster_df==702]
          area_ha_703<- dat$Freq[dat$paddy_raster_df==703]
          area_ha_704<- dat$Freq[dat$paddy_raster_df==704]
          area_ha_705<- dat$Freq[dat$paddy_raster_df==705]
          area_ha_706<- dat$Freq[dat$paddy_raster_df==706]
          area_ha_707<- dat$Freq[dat$paddy_raster_df==707]
          
          if (length(area_ha_101)==0) { area_ha_101<- 0 }
          if (length(area_ha_102)==0) { area_ha_102<- 0 }
          if (length(area_ha_103)==0) { area_ha_103<- 0 }
          if (length(area_ha_104)==0) { area_ha_104<- 0 }
          if (length(area_ha_105)==0) { area_ha_105<- 0 }
          if (length(area_ha_106)==0) { area_ha_106<- 0 }
          if (length(area_ha_107)==0) { area_ha_107<- 0 }
          if (length(area_ha_201)==0) { area_ha_201<- 0 }
          if (length(area_ha_202)==0) { area_ha_202<- 0 }
          if (length(area_ha_203)==0) { area_ha_203<- 0 }
          if (length(area_ha_204)==0) { area_ha_204<- 0 }
          if (length(area_ha_205)==0) { area_ha_205<- 0 }
          if (length(area_ha_206)==0) { area_ha_206<- 0 }
          if (length(area_ha_207)==0) { area_ha_207<- 0 }
          if (length(area_ha_301)==0) { area_ha_301<- 0 }
          if (length(area_ha_302)==0) { area_ha_302<- 0 }
          if (length(area_ha_303)==0) { area_ha_303<- 0 }
          if (length(area_ha_304)==0) { area_ha_304<- 0 }
          if (length(area_ha_305)==0) { area_ha_305<- 0 }
          if (length(area_ha_306)==0) { area_ha_306<- 0 }
          if (length(area_ha_307)==0) { area_ha_307<- 0 }
          if (length(area_ha_401)==0) { area_ha_401<- 0 }
          if (length(area_ha_402)==0) { area_ha_402<- 0 }
          if (length(area_ha_403)==0) { area_ha_403<- 0 }
          if (length(area_ha_404)==0) { area_ha_404<- 0 }
          if (length(area_ha_405)==0) { area_ha_405<- 0 }
          if (length(area_ha_406)==0) { area_ha_406<- 0 }
          if (length(area_ha_407)==0) { area_ha_407<- 0 }
          if (length(area_ha_501)==0) { area_ha_501<- 0 }
          if (length(area_ha_502)==0) { area_ha_502<- 0 }
          if (length(area_ha_503)==0) { area_ha_503<- 0 }
          if (length(area_ha_504)==0) { area_ha_504<- 0 }
          if (length(area_ha_505)==0) { area_ha_505<- 0 }
          if (length(area_ha_506)==0) { area_ha_506<- 0 }
          if (length(area_ha_507)==0) { area_ha_507<- 0 }
          if (length(area_ha_601)==0) { area_ha_601<- 0 }
          if (length(area_ha_602)==0) { area_ha_602<- 0 }
          if (length(area_ha_603)==0) { area_ha_603<- 0 }
          if (length(area_ha_604)==0) { area_ha_604<- 0 }
          if (length(area_ha_605)==0) { area_ha_605<- 0 }
          if (length(area_ha_606)==0) { area_ha_606<- 0 }
          if (length(area_ha_607)==0) { area_ha_607<- 0 }
          if (length(area_ha_701)==0) { area_ha_701<- 0 }
          if (length(area_ha_702)==0) { area_ha_702<- 0 }
          if (length(area_ha_703)==0) { area_ha_703<- 0 }
          if (length(area_ha_704)==0) { area_ha_704<- 0 }
          if (length(area_ha_705)==0) { area_ha_705<- 0 }
          if (length(area_ha_706)==0) { area_ha_706<- 0 }
          if (length(area_ha_707)==0) { area_ha_707<- 0 }
          
          area_ha_total_nochange <- area_ha_101 + area_ha_202 + area_ha_303 + area_ha_404 + area_ha_505 + area_ha_606 + area_ha_707
          area_ha_total_correct <- 
            area_ha_102 + area_ha_106 + area_ha_107 +
            area_ha_201 + area_ha_203 + area_ha_206 + 
            area_ha_304 + area_ha_306 +
            area_ha_405 + area_ha_406 +
            area_ha_501 + area_ha_502 + area_ha_506 + area_ha_507 +
            area_ha_601 + area_ha_602 + area_ha_603 + area_ha_604 + area_ha_605 + area_ha_607 +
            area_ha_701 + area_ha_702
          area_ha_total_incorrect <- 
            area_ha_103 + area_ha_104 + area_ha_105 + 
            area_ha_204 + area_ha_205 + area_ha_207 + 
            area_ha_301 + area_ha_302 + area_ha_305 + area_ha_307 + 
            area_ha_401 + area_ha_402 + area_ha_403 + area_ha_407 +
            area_ha_503 + area_ha_504 + 
            area_ha_703 + area_ha_704 + area_ha_705 + area_ha_706
          area_ha_total_true <- area_ha_total_nochange + area_ha_total_correct
          area_ha_total_all <- area_ha_total_nochange + area_ha_total_correct + area_ha_total_incorrect
          
          compare_total_RS <- area_ha_total_all/area_RS*100
          compare_total_BPS <- area_ha_total_all/area_BPS*100
          
          pct_101 <- area_ha_101 / area_ha_total_all * 100
          pct_102 <- area_ha_102 / area_ha_total_all * 100
          pct_103 <- area_ha_103 / area_ha_total_all * 100
          pct_104 <- area_ha_104 / area_ha_total_all * 100
          pct_105 <- area_ha_105 / area_ha_total_all * 100
          pct_106 <- area_ha_106 / area_ha_total_all * 100
          pct_107 <- area_ha_107 / area_ha_total_all * 100
          pct_201 <- area_ha_201 / area_ha_total_all * 100
          pct_202 <- area_ha_202 / area_ha_total_all * 100
          pct_203 <- area_ha_203 / area_ha_total_all * 100
          pct_204 <- area_ha_204 / area_ha_total_all * 100
          pct_205 <- area_ha_205 / area_ha_total_all * 100
          pct_206 <- area_ha_206 / area_ha_total_all * 100
          pct_207 <- area_ha_207 / area_ha_total_all * 100
          pct_301 <- area_ha_301 / area_ha_total_all * 100
          pct_302 <- area_ha_302 / area_ha_total_all * 100
          pct_303 <- area_ha_303 / area_ha_total_all * 100
          pct_304 <- area_ha_304 / area_ha_total_all * 100
          pct_305 <- area_ha_305 / area_ha_total_all * 100
          pct_306 <- area_ha_306 / area_ha_total_all * 100
          pct_307 <- area_ha_307 / area_ha_total_all * 100
          pct_401 <- area_ha_401 / area_ha_total_all * 100
          pct_402 <- area_ha_402 / area_ha_total_all * 100
          pct_403 <- area_ha_403 / area_ha_total_all * 100
          pct_404 <- area_ha_404 / area_ha_total_all * 100
          pct_405 <- area_ha_405 / area_ha_total_all * 100
          pct_406 <- area_ha_406 / area_ha_total_all * 100
          pct_407 <- area_ha_407 / area_ha_total_all * 100
          pct_501 <- area_ha_501 / area_ha_total_all * 100
          pct_502 <- area_ha_502 / area_ha_total_all * 100
          pct_503 <- area_ha_503 / area_ha_total_all * 100
          pct_504 <- area_ha_504 / area_ha_total_all * 100
          pct_505 <- area_ha_505 / area_ha_total_all * 100
          pct_506 <- area_ha_506 / area_ha_total_all * 100
          pct_507 <- area_ha_507 / area_ha_total_all * 100
          pct_601 <- area_ha_601 / area_ha_total_all * 100
          pct_602 <- area_ha_602 / area_ha_total_all * 100
          pct_603 <- area_ha_603 / area_ha_total_all * 100
          pct_604 <- area_ha_604 / area_ha_total_all * 100
          pct_605 <- area_ha_605 / area_ha_total_all * 100
          pct_606 <- area_ha_606 / area_ha_total_all * 100
          pct_607 <- area_ha_607 / area_ha_total_all * 100
          pct_701 <- area_ha_701 / area_ha_total_all * 100
          pct_702 <- area_ha_702 / area_ha_total_all * 100
          pct_703 <- area_ha_703 / area_ha_total_all * 100
          pct_704 <- area_ha_704 / area_ha_total_all * 100
          pct_705 <- area_ha_705 / area_ha_total_all * 100
          pct_706 <- area_ha_706 / area_ha_total_all * 100
          pct_707 <- area_ha_707 / area_ha_total_all * 100
          
          pct_total_nochange <- area_ha_total_nochange / area_ha_total_all * 100
          pct_total_correct <- area_ha_total_correct / area_ha_total_all * 100
          pct_total_true <- area_ha_total_true / area_ha_total_all * 100
          pct_total_incorrect <- area_ha_total_incorrect / area_ha_total_all * 100
          
          results[nrow(results) + 1, ] <- c(
            as.character(SENSOR),
            as.character(model_ML),
            as.character(period),
            as.character(period_first),
            as.character(period_prev),
            as.character(region),
            as.character(substr(region,0,2)),
            as.character(time),
            as.double(area_ha_101),
            as.double(area_ha_102),
            as.double(area_ha_103),
            as.double(area_ha_104),
            as.double(area_ha_105),
            as.double(area_ha_106),
            as.double(area_ha_107),
            as.double(area_ha_201),
            as.double(area_ha_202),
            as.double(area_ha_203),
            as.double(area_ha_204),
            as.double(area_ha_205),
            as.double(area_ha_206),
            as.double(area_ha_207),
            as.double(area_ha_301),
            as.double(area_ha_302),
            as.double(area_ha_303),
            as.double(area_ha_304),
            as.double(area_ha_305),
            as.double(area_ha_306),
            as.double(area_ha_307),
            as.double(area_ha_401),
            as.double(area_ha_402),
            as.double(area_ha_403),
            as.double(area_ha_404),
            as.double(area_ha_405),
            as.double(area_ha_406),
            as.double(area_ha_407),
            as.double(area_ha_501),
            as.double(area_ha_502),
            as.double(area_ha_503),
            as.double(area_ha_504),
            as.double(area_ha_505),
            as.double(area_ha_506), 
            as.double(area_ha_507),
            as.double(area_ha_601),
            as.double(area_ha_602),
            as.double(area_ha_603),
            as.double(area_ha_604),
            as.double(area_ha_605),
            as.double(area_ha_606), 
            as.double(area_ha_607),
            as.double(area_ha_701),
            as.double(area_ha_702),
            as.double(area_ha_703),
            as.double(area_ha_704),
            as.double(area_ha_705),
            as.double(area_ha_706), 
            as.double(area_ha_707),
            as.double(area_ha_total_nochange),
            as.double(area_ha_total_correct),
            as.double(area_ha_total_true),
            as.double(area_ha_total_incorrect),
            as.double(area_ha_total_all),
            as.double(area_RS),
            as.double(area_BPS),
            as.double(compare_total_RS),
            as.double(compare_total_BPS),
            as.double(pct_101),
            as.double(pct_102),
            as.double(pct_103),
            as.double(pct_104),
            as.double(pct_105),
            as.double(pct_106),
            as.double(pct_107),
            as.double(pct_201),
            as.double(pct_202),
            as.double(pct_203),
            as.double(pct_204),
            as.double(pct_205),
            as.double(pct_206),
            as.double(pct_207),
            as.double(pct_301),
            as.double(pct_302),
            as.double(pct_303),
            as.double(pct_304),
            as.double(pct_305),
            as.double(pct_306),
            as.double(pct_307),
            as.double(pct_401),
            as.double(pct_402),
            as.double(pct_403),
            as.double(pct_404),
            as.double(pct_405),
            as.double(pct_406), 
            as.double(pct_407),
            as.double(pct_501),
            as.double(pct_502),
            as.double(pct_503),
            as.double(pct_504),
            as.double(pct_505),
            as.double(pct_506),
            as.double(pct_507),
            as.double(pct_601),
            as.double(pct_602),
            as.double(pct_603),
            as.double(pct_604),
            as.double(pct_605),
            as.double(pct_606), 
            as.double(pct_607),
            as.double(pct_701),
            as.double(pct_702),
            as.double(pct_703),
            as.double(pct_704),
            as.double(pct_705),
            as.double(pct_706), 
            as.double(pct_707),
            as.double(pct_total_nochange),
            as.double(pct_total_correct),
            as.double(pct_total_true),
            as.double(pct_total_incorrect)
          )
          
        }
      }
    }
  }
}

mainDir_results<- paste("C:/MasseyOffice1/Research/R-Script2020/",sep="")


filename_csv_paddy_subdistrict <- paste(mainDir_results,"Recap-Change-MOD13Q1_result_kab.csv", sep="")
write.csv(results, file = filename_csv_paddy_subdistrict)

#PROVINSI
results_prov <- data.frame(
  sensor=character(),
  model=character(),
  period_start=character(),
  start_first=character(),
  start_last=character(),
  period_end=character(),
  end_first=character(),
  end_last=character(),
  rows=character(),
  regions=character(),
  prov=character(),
  time_lag=character(),
  area_ha_101=double(),
  area_ha_102=double(),
  area_ha_103=double(),
  area_ha_104=double(),
  area_ha_105=double(),
  area_ha_106=double(),
  area_ha_107=double(),
  area_ha_201=double(),
  area_ha_202=double(),
  area_ha_203=double(),
  area_ha_204=double(),
  area_ha_205=double(),
  area_ha_206=double(),
  area_ha_207=double(),
  area_ha_301=double(),
  area_ha_302=double(),
  area_ha_303=double(),
  area_ha_304=double(),
  area_ha_305=double(),
  area_ha_306=double(),
  area_ha_307=double(),
  area_ha_401=double(),
  area_ha_402=double(),
  area_ha_403=double(),
  area_ha_404=double(),
  area_ha_405=double(),
  area_ha_406=double(),
  area_ha_407=double(),
  area_ha_501=double(),
  area_ha_502=double(),
  area_ha_503=double(),
  area_ha_504=double(),
  area_ha_505=double(),
  area_ha_506=double(),
  area_ha_507=double(),
  area_ha_601=double(),
  area_ha_602=double(),
  area_ha_603=double(),
  area_ha_604=double(),
  area_ha_605=double(),
  area_ha_606=double(),
  area_ha_607=double(),
  area_ha_701=double(),
  area_ha_702=double(),
  area_ha_703=double(),
  area_ha_704=double(),
  area_ha_705=double(),
  area_ha_706=double(),
  area_ha_707=double(),
  area_ha_total_nochange=double(),
  area_ha_total_correct=double(),
  area_ha_total_true=double(),
  area_ha_total_incorrect=double(),
  area_ha_total_all=double(),
  area_RS=double(),
  area_BPS=double(),
  compare_total_RS=double(),
  compare_total_BPS=double(),
  pct_101=double(),
  pct_102=double(),
  pct_103=double(),
  pct_104=double(),
  pct_105=double(),
  pct_106=double(),
  pct_107=double(),
  pct_201=double(),
  pct_202=double(),
  pct_203=double(),
  pct_204=double(),
  pct_205=double(),
  pct_206=double(),
  pct_207=double(),
  pct_301=double(),
  pct_302=double(),
  pct_303=double(),
  pct_304=double(),
  pct_305=double(),
  pct_306=double(),
  pct_307=double(),
  pct_401=double(),
  pct_402=double(),
  pct_403=double(),
  pct_404=double(),
  pct_405=double(),
  pct_406=double(),
  pct_407=double(),
  pct_501=double(),
  pct_502=double(),
  pct_503=double(),
  pct_504=double(),
  pct_505=double(),
  pct_506=double(),
  pct_507=double(),
  pct_601=double(),
  pct_602=double(),
  pct_603=double(),
  pct_604=double(),
  pct_605=double(),
  pct_606=double(),
  pct_607=double(),
  pct_701=double(),
  pct_702=double(),
  pct_703=double(),
  pct_704=double(),
  pct_705=double(),
  pct_706=double(),
  pct_707=double(),
  pct_total_nochange=double(),
  pct_total_correct=double(),
  pct_total_true=double(),
  pct_total_incorrect=double(),
  stringsAsFactors = FALSE
)



prov_list <- c(32,35)
for (sensor in SENSOR_LIST) {
  result_temp <- results
  
  time_period <- 16

  for (time in time_list) {
    date_LS8 <- sort(unique(result_temp$period_first[which(result_temp$sensor==sensor & result_temp$time_lag == time)]),decreasing = FALSE)
    
    for (i in 1:2) {
      prov_temp <- prov_list[i]
      for (j in 1:length(date_LS8)) {
        date_temp1 <- date_LS8[j]
        if (j>1) {
          if (date_temp1 < date_temp2) {
            next
          }
        }
        date_temp2 <- as.character(as.Date(date_temp1) +  time_period)
        
        result_date <- result_temp[which(result_temp$sensor==sensor & result_temp$prov==prov_temp & 
                                           result_temp$time_lag == time & result_temp$period_first>=date_temp1
                                         & result_temp$period_first<date_temp2),]
        result_date <- result_date[order(result_date$period_first),]
        no_rows <- nrow(result_date)
        if (no_rows<3) {
          next
        }
        if (no_rows==12) {
          next
        }
        model_ML <- unique(result_date$model)
        regions <- paste( unlist(result_date$region), collapse='|')
        start_first <- result_date$period_first[1]
        start_last <- result_date$period_first[no_rows]
        end_first <- result_date$period_prev[1]
        end_last <- result_date$period_prev[no_rows]
        period_start <- paste(start_first,"_",start_last,sep="")
        period_end <- paste(end_first,"_",end_last,sep="")
        period_start <- gsub("-","",period_start)
        period_end <- gsub("-","",period_end)
        # if (period_first == period_last) {
        #   next
        # }
        
        area_ha_101<- sum(as.numeric(result_date$area_ha_101), na.rm = TRUE)
        area_ha_102<- sum(as.numeric(result_date$area_ha_102), na.rm = TRUE)
        area_ha_103<- sum(as.numeric(result_date$area_ha_103), na.rm = TRUE)
        area_ha_104<- sum(as.numeric(result_date$area_ha_104), na.rm = TRUE)
        area_ha_105<- sum(as.numeric(result_date$area_ha_105), na.rm = TRUE)
        area_ha_106<- sum(as.numeric(result_date$area_ha_106), na.rm = TRUE)
        area_ha_107<- sum(as.numeric(result_date$area_ha_107), na.rm = TRUE)
        area_ha_201<- sum(as.numeric(result_date$area_ha_201), na.rm = TRUE)
        area_ha_202<- sum(as.numeric(result_date$area_ha_202), na.rm = TRUE)
        area_ha_203<- sum(as.numeric(result_date$area_ha_203), na.rm = TRUE)
        area_ha_204<- sum(as.numeric(result_date$area_ha_204), na.rm = TRUE)
        area_ha_205<- sum(as.numeric(result_date$area_ha_205), na.rm = TRUE)
        area_ha_206<- sum(as.numeric(result_date$area_ha_206), na.rm = TRUE)
        area_ha_207<- sum(as.numeric(result_date$area_ha_207), na.rm = TRUE)
        area_ha_301<- sum(as.numeric(result_date$area_ha_301), na.rm = TRUE)
        area_ha_302<- sum(as.numeric(result_date$area_ha_302), na.rm = TRUE)
        area_ha_303<- sum(as.numeric(result_date$area_ha_303), na.rm = TRUE)
        area_ha_304<- sum(as.numeric(result_date$area_ha_304), na.rm = TRUE)
        area_ha_305<- sum(as.numeric(result_date$area_ha_305), na.rm = TRUE)
        area_ha_306<- sum(as.numeric(result_date$area_ha_306), na.rm = TRUE)
        area_ha_307<- sum(as.numeric(result_date$area_ha_307), na.rm = TRUE)
        area_ha_401<- sum(as.numeric(result_date$area_ha_401), na.rm = TRUE)
        area_ha_402<- sum(as.numeric(result_date$area_ha_402), na.rm = TRUE)
        area_ha_403<- sum(as.numeric(result_date$area_ha_403), na.rm = TRUE)
        area_ha_404<- sum(as.numeric(result_date$area_ha_404), na.rm = TRUE)
        area_ha_405<- sum(as.numeric(result_date$area_ha_405), na.rm = TRUE)
        area_ha_406<- sum(as.numeric(result_date$area_ha_406), na.rm = TRUE)
        area_ha_407<- sum(as.numeric(result_date$area_ha_107), na.rm = TRUE)
        area_ha_501<- sum(as.numeric(result_date$area_ha_501), na.rm = TRUE)
        area_ha_502<- sum(as.numeric(result_date$area_ha_502), na.rm = TRUE)
        area_ha_503<- sum(as.numeric(result_date$area_ha_503), na.rm = TRUE)
        area_ha_504<- sum(as.numeric(result_date$area_ha_504), na.rm = TRUE)
        area_ha_505<- sum(as.numeric(result_date$area_ha_505), na.rm = TRUE)
        area_ha_506<- sum(as.numeric(result_date$area_ha_506), na.rm = TRUE)
        area_ha_507<- sum(as.numeric(result_date$area_ha_507), na.rm = TRUE)
        area_ha_601<- sum(as.numeric(result_date$area_ha_601), na.rm = TRUE)
        area_ha_602<- sum(as.numeric(result_date$area_ha_602), na.rm = TRUE)
        area_ha_603<- sum(as.numeric(result_date$area_ha_603), na.rm = TRUE)
        area_ha_604<- sum(as.numeric(result_date$area_ha_604), na.rm = TRUE)
        area_ha_605<- sum(as.numeric(result_date$area_ha_605), na.rm = TRUE)
        area_ha_606<- sum(as.numeric(result_date$area_ha_606), na.rm = TRUE)
        area_ha_607<- sum(as.numeric(result_date$area_ha_607), na.rm = TRUE)
        area_ha_702<- sum(as.numeric(result_date$area_ha_702), na.rm = TRUE)
        area_ha_703<- sum(as.numeric(result_date$area_ha_703), na.rm = TRUE)
        area_ha_704<- sum(as.numeric(result_date$area_ha_704), na.rm = TRUE)
        area_ha_705<- sum(as.numeric(result_date$area_ha_705), na.rm = TRUE)
        area_ha_706<- sum(as.numeric(result_date$area_ha_706), na.rm = TRUE)
        area_ha_707<- sum(as.numeric(result_date$area_ha_707), na.rm = TRUE)
        
        if (length(area_ha_101)==0) { area_ha_101<- 0 }
        if (length(area_ha_102)==0) { area_ha_102<- 0 }
        if (length(area_ha_103)==0) { area_ha_103<- 0 }
        if (length(area_ha_104)==0) { area_ha_104<- 0 }
        if (length(area_ha_105)==0) { area_ha_105<- 0 }
        if (length(area_ha_106)==0) { area_ha_106<- 0 }
        if (length(area_ha_107)==0) { area_ha_107<- 0 }
        if (length(area_ha_201)==0) { area_ha_201<- 0 }
        if (length(area_ha_202)==0) { area_ha_202<- 0 }
        if (length(area_ha_203)==0) { area_ha_203<- 0 }
        if (length(area_ha_204)==0) { area_ha_204<- 0 }
        if (length(area_ha_205)==0) { area_ha_205<- 0 }
        if (length(area_ha_206)==0) { area_ha_206<- 0 }
        if (length(area_ha_207)==0) { area_ha_207<- 0 }
        if (length(area_ha_301)==0) { area_ha_301<- 0 }
        if (length(area_ha_302)==0) { area_ha_302<- 0 }
        if (length(area_ha_303)==0) { area_ha_303<- 0 }
        if (length(area_ha_304)==0) { area_ha_304<- 0 }
        if (length(area_ha_305)==0) { area_ha_305<- 0 }
        if (length(area_ha_306)==0) { area_ha_306<- 0 }
        if (length(area_ha_307)==0) { area_ha_307<- 0 }
        if (length(area_ha_401)==0) { area_ha_401<- 0 }
        if (length(area_ha_402)==0) { area_ha_402<- 0 }
        if (length(area_ha_403)==0) { area_ha_403<- 0 }
        if (length(area_ha_404)==0) { area_ha_404<- 0 }
        if (length(area_ha_405)==0) { area_ha_405<- 0 }
        if (length(area_ha_406)==0) { area_ha_406<- 0 }
        if (length(area_ha_407)==0) { area_ha_407<- 0 }
        if (length(area_ha_501)==0) { area_ha_501<- 0 }
        if (length(area_ha_502)==0) { area_ha_502<- 0 }
        if (length(area_ha_503)==0) { area_ha_503<- 0 }
        if (length(area_ha_504)==0) { area_ha_504<- 0 }
        if (length(area_ha_505)==0) { area_ha_505<- 0 }
        if (length(area_ha_506)==0) { area_ha_506<- 0 }
        if (length(area_ha_507)==0) { area_ha_507<- 0 }
        if (length(area_ha_601)==0) { area_ha_601<- 0 }
        if (length(area_ha_602)==0) { area_ha_602<- 0 }
        if (length(area_ha_603)==0) { area_ha_603<- 0 }
        if (length(area_ha_604)==0) { area_ha_604<- 0 }
        if (length(area_ha_605)==0) { area_ha_605<- 0 }
        if (length(area_ha_606)==0) { area_ha_606<- 0 }
        if (length(area_ha_607)==0) { area_ha_607<- 0 }
        if (length(area_ha_701)==0) { area_ha_701<- 0 }
        if (length(area_ha_702)==0) { area_ha_702<- 0 }
        if (length(area_ha_703)==0) { area_ha_703<- 0 }
        if (length(area_ha_704)==0) { area_ha_704<- 0 }
        if (length(area_ha_705)==0) { area_ha_705<- 0 }
        if (length(area_ha_706)==0) { area_ha_706<- 0 }
        if (length(area_ha_707)==0) { area_ha_707<- 0 }
        
        area_ha_total_nochange <- area_ha_101 + area_ha_202 + area_ha_303 + area_ha_404 + area_ha_505 + area_ha_606 + area_ha_707
        area_ha_total_correct <- 
          area_ha_102 + area_ha_106 + area_ha_107 +
          area_ha_201 + area_ha_203 + area_ha_206 + 
          area_ha_304 + area_ha_306 +
          area_ha_405 + area_ha_406 +
          area_ha_501 + area_ha_502 + area_ha_506 + area_ha_507 +
          area_ha_601 + area_ha_602 + area_ha_603 + area_ha_604 + area_ha_605 + area_ha_607 +
          area_ha_701 + area_ha_702
        area_ha_total_incorrect <- 
          area_ha_103 + area_ha_104 + area_ha_105 + 
          area_ha_204 + area_ha_205 + area_ha_207 + 
          area_ha_301 + area_ha_302 + area_ha_305 + area_ha_307 + 
          area_ha_401 + area_ha_402 + area_ha_403 + area_ha_407 +
          area_ha_503 + area_ha_504 + 
          area_ha_703 + area_ha_704 + area_ha_705 + area_ha_706
        area_ha_total_true <- area_ha_total_nochange + area_ha_total_correct
        area_ha_total_all <- area_ha_total_nochange + area_ha_total_correct + area_ha_total_incorrect
        
        compare_total_RS <- area_ha_total_all/area_RS*100
        compare_total_BPS <- area_ha_total_all/area_BPS*100
        
        pct_101 <- area_ha_101 / area_ha_total_all * 100
        pct_102 <- area_ha_102 / area_ha_total_all * 100
        pct_103 <- area_ha_103 / area_ha_total_all * 100
        pct_104 <- area_ha_104 / area_ha_total_all * 100
        pct_105 <- area_ha_105 / area_ha_total_all * 100
        pct_106 <- area_ha_106 / area_ha_total_all * 100
        pct_107 <- area_ha_107 / area_ha_total_all * 100
        pct_201 <- area_ha_201 / area_ha_total_all * 100
        pct_202 <- area_ha_202 / area_ha_total_all * 100
        pct_203 <- area_ha_203 / area_ha_total_all * 100
        pct_204 <- area_ha_204 / area_ha_total_all * 100
        pct_205 <- area_ha_205 / area_ha_total_all * 100
        pct_206 <- area_ha_206 / area_ha_total_all * 100
        pct_207 <- area_ha_207 / area_ha_total_all * 100
        pct_301 <- area_ha_301 / area_ha_total_all * 100
        pct_302 <- area_ha_302 / area_ha_total_all * 100
        pct_303 <- area_ha_303 / area_ha_total_all * 100
        pct_304 <- area_ha_304 / area_ha_total_all * 100
        pct_305 <- area_ha_305 / area_ha_total_all * 100
        pct_306 <- area_ha_306 / area_ha_total_all * 100
        pct_307 <- area_ha_307 / area_ha_total_all * 100
        pct_401 <- area_ha_401 / area_ha_total_all * 100
        pct_402 <- area_ha_402 / area_ha_total_all * 100
        pct_403 <- area_ha_403 / area_ha_total_all * 100
        pct_404 <- area_ha_404 / area_ha_total_all * 100
        pct_405 <- area_ha_405 / area_ha_total_all * 100
        pct_406 <- area_ha_406 / area_ha_total_all * 100
        pct_407 <- area_ha_407 / area_ha_total_all * 100
        pct_501 <- area_ha_501 / area_ha_total_all * 100
        pct_502 <- area_ha_502 / area_ha_total_all * 100
        pct_503 <- area_ha_503 / area_ha_total_all * 100
        pct_504 <- area_ha_504 / area_ha_total_all * 100
        pct_505 <- area_ha_505 / area_ha_total_all * 100
        pct_506 <- area_ha_506 / area_ha_total_all * 100
        pct_507 <- area_ha_507 / area_ha_total_all * 100
        pct_601 <- area_ha_601 / area_ha_total_all * 100
        pct_602 <- area_ha_602 / area_ha_total_all * 100
        pct_603 <- area_ha_603 / area_ha_total_all * 100
        pct_604 <- area_ha_604 / area_ha_total_all * 100
        pct_605 <- area_ha_605 / area_ha_total_all * 100
        pct_606 <- area_ha_606 / area_ha_total_all * 100
        pct_607 <- area_ha_607 / area_ha_total_all * 100
        pct_701 <- area_ha_701 / area_ha_total_all * 100
        pct_702 <- area_ha_702 / area_ha_total_all * 100
        pct_703 <- area_ha_703 / area_ha_total_all * 100
        pct_704 <- area_ha_704 / area_ha_total_all * 100
        pct_705 <- area_ha_705 / area_ha_total_all * 100
        pct_706 <- area_ha_706 / area_ha_total_all * 100
        pct_707 <- area_ha_707 / area_ha_total_all * 100
        
        pct_total_nochange <- area_ha_total_nochange / area_ha_total_all * 100
        pct_total_correct <- area_ha_total_correct / area_ha_total_all * 100
        pct_total_true <- area_ha_total_true / area_ha_total_all * 100
        pct_total_incorrect <- area_ha_total_incorrect / area_ha_total_all * 100
        
        
        results_prov[nrow(results_prov) + 1, ] <- c(
          as.character(sensor),
          as.character(model_ML),
          as.character(period_start),
          as.character(start_first),
          as.character(start_last),
          as.character(period_end),
          as.character(end_first),
          as.character(end_last),
          as.character(no_rows),
          as.character(regions),
          as.character(prov_temp),
          as.character(time),
          as.double(area_ha_101),
          as.double(area_ha_102),
          as.double(area_ha_103),
          as.double(area_ha_104),
          as.double(area_ha_105),
          as.double(area_ha_106),
          as.double(area_ha_107),
          as.double(area_ha_201),
          as.double(area_ha_202),
          as.double(area_ha_203),
          as.double(area_ha_204),
          as.double(area_ha_205),
          as.double(area_ha_206),
          as.double(area_ha_207),
          as.double(area_ha_301),
          as.double(area_ha_302),
          as.double(area_ha_303),
          as.double(area_ha_304),
          as.double(area_ha_305),
          as.double(area_ha_306),
          as.double(area_ha_307),
          as.double(area_ha_401),
          as.double(area_ha_402),
          as.double(area_ha_403),
          as.double(area_ha_404),
          as.double(area_ha_405),
          as.double(area_ha_406),
          as.double(area_ha_407),
          as.double(area_ha_501),
          as.double(area_ha_502),
          as.double(area_ha_503),
          as.double(area_ha_504),
          as.double(area_ha_505),
          as.double(area_ha_506), 
          as.double(area_ha_507),
          as.double(area_ha_601),
          as.double(area_ha_602),
          as.double(area_ha_603),
          as.double(area_ha_604),
          as.double(area_ha_605),
          as.double(area_ha_606), 
          as.double(area_ha_607),
          as.double(area_ha_701),
          as.double(area_ha_702),
          as.double(area_ha_703),
          as.double(area_ha_704),
          as.double(area_ha_705),
          as.double(area_ha_706), 
          as.double(area_ha_707),
          as.double(area_ha_total_nochange),
          as.double(area_ha_total_correct),
          as.double(area_ha_total_true),
          as.double(area_ha_total_incorrect),
          as.double(area_ha_total_all),
          as.double(area_RS),
          as.double(area_BPS),
          as.double(compare_total_RS),
          as.double(compare_total_BPS),
          as.double(pct_101),
          as.double(pct_102),
          as.double(pct_103),
          as.double(pct_104),
          as.double(pct_105),
          as.double(pct_106),
          as.double(pct_107),
          as.double(pct_201),
          as.double(pct_202),
          as.double(pct_203),
          as.double(pct_204),
          as.double(pct_205),
          as.double(pct_206),
          as.double(pct_207),
          as.double(pct_301),
          as.double(pct_302),
          as.double(pct_303),
          as.double(pct_304),
          as.double(pct_305),
          as.double(pct_306),
          as.double(pct_307),
          as.double(pct_401),
          as.double(pct_402),
          as.double(pct_403),
          as.double(pct_404),
          as.double(pct_405),
          as.double(pct_406), 
          as.double(pct_407),
          as.double(pct_501),
          as.double(pct_502),
          as.double(pct_503),
          as.double(pct_504),
          as.double(pct_505),
          as.double(pct_506),
          as.double(pct_507),
          as.double(pct_601),
          as.double(pct_602),
          as.double(pct_603),
          as.double(pct_604),
          as.double(pct_605),
          as.double(pct_606), 
          as.double(pct_607),
          as.double(pct_701),
          as.double(pct_702),
          as.double(pct_703),
          as.double(pct_704),
          as.double(pct_705),
          as.double(pct_706), 
          as.double(pct_707),
          as.double(pct_total_nochange),
          as.double(pct_total_correct),
          as.double(pct_total_true),
          as.double(pct_total_incorrect)
        )
   
      }
    }
  }
} 

filename_csv_paddy_subdistrict <- paste(mainDir_results,"Recap-Change-MOD13Q1_result_prov.csv", sep="")
write.csv(results_prov, file = filename_csv_paddy_subdistrict)


results_root <- data.frame(
  sensor=character(),
  model=character(),
  rows=character(),
  prov=character(),
  time_lag=character(),
  area_ha_101=double(),
  area_ha_102=double(),
  area_ha_103=double(),
  area_ha_104=double(),
  area_ha_105=double(),
  area_ha_106=double(),
  area_ha_107=double(),
  area_ha_201=double(),
  area_ha_202=double(),
  area_ha_203=double(),
  area_ha_204=double(),
  area_ha_205=double(),
  area_ha_206=double(),
  area_ha_207=double(),
  area_ha_301=double(),
  area_ha_302=double(),
  area_ha_303=double(),
  area_ha_304=double(),
  area_ha_305=double(),
  area_ha_306=double(),
  area_ha_307=double(),
  area_ha_401=double(),
  area_ha_402=double(),
  area_ha_403=double(),
  area_ha_404=double(),
  area_ha_405=double(),
  area_ha_406=double(),
  area_ha_407=double(),
  area_ha_501=double(),
  area_ha_502=double(),
  area_ha_503=double(),
  area_ha_504=double(),
  area_ha_505=double(),
  area_ha_506=double(),
  area_ha_507=double(),
  area_ha_601=double(),
  area_ha_602=double(),
  area_ha_603=double(),
  area_ha_604=double(),
  area_ha_605=double(),
  area_ha_606=double(),
  area_ha_607=double(),
  area_ha_701=double(),
  area_ha_702=double(),
  area_ha_703=double(),
  area_ha_704=double(),
  area_ha_705=double(),
  area_ha_706=double(),
  area_ha_707=double(),
  area_ha_total_nochange=double(),
  area_ha_total_correct=double(),
  area_ha_total_true=double(),
  area_ha_total_incorrect=double(),
  area_ha_total_all=double(),
  area_RS=double(),
  area_BPS=double(),
  compare_total_RS=double(),
  compare_total_BPS=double(),
  pct_101=double(),
  pct_102=double(),
  pct_103=double(),
  pct_104=double(),
  pct_105=double(),
  pct_106=double(),
  pct_107=double(),
  pct_201=double(),
  pct_202=double(),
  pct_203=double(),
  pct_204=double(),
  pct_205=double(),
  pct_206=double(),
  pct_207=double(),
  pct_301=double(),
  pct_302=double(),
  pct_303=double(),
  pct_304=double(),
  pct_305=double(),
  pct_306=double(),
  pct_307=double(),
  pct_401=double(),
  pct_402=double(),
  pct_403=double(),
  pct_404=double(),
  pct_405=double(),
  pct_406=double(),
  pct_407=double(),
  pct_501=double(),
  pct_502=double(),
  pct_503=double(),
  pct_504=double(),
  pct_505=double(),
  pct_506=double(),
  pct_507=double(),
  pct_601=double(),
  pct_602=double(),
  pct_603=double(),
  pct_604=double(),
  pct_605=double(),
  pct_606=double(),
  pct_607=double(),
  pct_701=double(),
  pct_702=double(),
  pct_703=double(),
  pct_704=double(),
  pct_705=double(),
  pct_706=double(),
  pct_707=double(),
  pct_total_nochange=double(),
  pct_total_correct=double(),
  pct_total_true=double(),
  pct_total_incorrect=double(),
  stringsAsFactors = FALSE
)

for (sensor in SENSOR_LIST) {
  
    time_list <- c(16)
    time_period <- 16
  
  for (time in time_list) {
    
    for (i in 1:2) {
      prov_temp <- prov_list[i]
      
      result_date <- result_temp[which(result_temp$sensor==sensor & result_temp$prov==prov_temp & 
                                         result_temp$time_lag == time),]
      
      no_rows <- nrow(result_date)
      if (no_rows<3) {
        next
      }
      # if (no_rows==12) {
      #   next
      # }
      model_ML <- unique(result_date$model)
      regions <- paste( unlist(result_date$region), collapse='|')
      
      # if (period_first == period_last) {
      #   next
      # }
      
      area_ha_101<- sum(as.numeric(result_date$area_ha_101), na.rm = TRUE)
      area_ha_102<- sum(as.numeric(result_date$area_ha_102), na.rm = TRUE)
      area_ha_103<- sum(as.numeric(result_date$area_ha_103), na.rm = TRUE)
      area_ha_104<- sum(as.numeric(result_date$area_ha_104), na.rm = TRUE)
      area_ha_105<- sum(as.numeric(result_date$area_ha_105), na.rm = TRUE)
      area_ha_106<- sum(as.numeric(result_date$area_ha_106), na.rm = TRUE)
      area_ha_107<- sum(as.numeric(result_date$area_ha_107), na.rm = TRUE)
      area_ha_201<- sum(as.numeric(result_date$area_ha_201), na.rm = TRUE)
      area_ha_202<- sum(as.numeric(result_date$area_ha_202), na.rm = TRUE)
      area_ha_203<- sum(as.numeric(result_date$area_ha_203), na.rm = TRUE)
      area_ha_204<- sum(as.numeric(result_date$area_ha_204), na.rm = TRUE)
      area_ha_205<- sum(as.numeric(result_date$area_ha_205), na.rm = TRUE)
      area_ha_206<- sum(as.numeric(result_date$area_ha_206), na.rm = TRUE)
      area_ha_207<- sum(as.numeric(result_date$area_ha_207), na.rm = TRUE)
      area_ha_301<- sum(as.numeric(result_date$area_ha_301), na.rm = TRUE)
      area_ha_302<- sum(as.numeric(result_date$area_ha_302), na.rm = TRUE)
      area_ha_303<- sum(as.numeric(result_date$area_ha_303), na.rm = TRUE)
      area_ha_304<- sum(as.numeric(result_date$area_ha_304), na.rm = TRUE)
      area_ha_305<- sum(as.numeric(result_date$area_ha_305), na.rm = TRUE)
      area_ha_306<- sum(as.numeric(result_date$area_ha_306), na.rm = TRUE)
      area_ha_307<- sum(as.numeric(result_date$area_ha_307), na.rm = TRUE)
      area_ha_401<- sum(as.numeric(result_date$area_ha_401), na.rm = TRUE)
      area_ha_402<- sum(as.numeric(result_date$area_ha_402), na.rm = TRUE)
      area_ha_403<- sum(as.numeric(result_date$area_ha_403), na.rm = TRUE)
      area_ha_404<- sum(as.numeric(result_date$area_ha_404), na.rm = TRUE)
      area_ha_405<- sum(as.numeric(result_date$area_ha_405), na.rm = TRUE)
      area_ha_406<- sum(as.numeric(result_date$area_ha_406), na.rm = TRUE)
      area_ha_407<- sum(as.numeric(result_date$area_ha_107), na.rm = TRUE)
      area_ha_501<- sum(as.numeric(result_date$area_ha_501), na.rm = TRUE)
      area_ha_502<- sum(as.numeric(result_date$area_ha_502), na.rm = TRUE)
      area_ha_503<- sum(as.numeric(result_date$area_ha_503), na.rm = TRUE)
      area_ha_504<- sum(as.numeric(result_date$area_ha_504), na.rm = TRUE)
      area_ha_505<- sum(as.numeric(result_date$area_ha_505), na.rm = TRUE)
      area_ha_506<- sum(as.numeric(result_date$area_ha_506), na.rm = TRUE)
      area_ha_507<- sum(as.numeric(result_date$area_ha_507), na.rm = TRUE)
      area_ha_601<- sum(as.numeric(result_date$area_ha_601), na.rm = TRUE)
      area_ha_602<- sum(as.numeric(result_date$area_ha_602), na.rm = TRUE)
      area_ha_603<- sum(as.numeric(result_date$area_ha_603), na.rm = TRUE)
      area_ha_604<- sum(as.numeric(result_date$area_ha_604), na.rm = TRUE)
      area_ha_605<- sum(as.numeric(result_date$area_ha_605), na.rm = TRUE)
      area_ha_606<- sum(as.numeric(result_date$area_ha_606), na.rm = TRUE)
      area_ha_607<- sum(as.numeric(result_date$area_ha_607), na.rm = TRUE)
      area_ha_702<- sum(as.numeric(result_date$area_ha_702), na.rm = TRUE)
      area_ha_703<- sum(as.numeric(result_date$area_ha_703), na.rm = TRUE)
      area_ha_704<- sum(as.numeric(result_date$area_ha_704), na.rm = TRUE)
      area_ha_705<- sum(as.numeric(result_date$area_ha_705), na.rm = TRUE)
      area_ha_706<- sum(as.numeric(result_date$area_ha_706), na.rm = TRUE)
      area_ha_707<- sum(as.numeric(result_date$area_ha_707), na.rm = TRUE)
      
      if (length(area_ha_101)==0) { area_ha_101<- 0 }
      if (length(area_ha_102)==0) { area_ha_102<- 0 }
      if (length(area_ha_103)==0) { area_ha_103<- 0 }
      if (length(area_ha_104)==0) { area_ha_104<- 0 }
      if (length(area_ha_105)==0) { area_ha_105<- 0 }
      if (length(area_ha_106)==0) { area_ha_106<- 0 }
      if (length(area_ha_107)==0) { area_ha_107<- 0 }
      if (length(area_ha_201)==0) { area_ha_201<- 0 }
      if (length(area_ha_202)==0) { area_ha_202<- 0 }
      if (length(area_ha_203)==0) { area_ha_203<- 0 }
      if (length(area_ha_204)==0) { area_ha_204<- 0 }
      if (length(area_ha_205)==0) { area_ha_205<- 0 }
      if (length(area_ha_206)==0) { area_ha_206<- 0 }
      if (length(area_ha_207)==0) { area_ha_207<- 0 }
      if (length(area_ha_301)==0) { area_ha_301<- 0 }
      if (length(area_ha_302)==0) { area_ha_302<- 0 }
      if (length(area_ha_303)==0) { area_ha_303<- 0 }
      if (length(area_ha_304)==0) { area_ha_304<- 0 }
      if (length(area_ha_305)==0) { area_ha_305<- 0 }
      if (length(area_ha_306)==0) { area_ha_306<- 0 }
      if (length(area_ha_307)==0) { area_ha_307<- 0 }
      if (length(area_ha_401)==0) { area_ha_401<- 0 }
      if (length(area_ha_402)==0) { area_ha_402<- 0 }
      if (length(area_ha_403)==0) { area_ha_403<- 0 }
      if (length(area_ha_404)==0) { area_ha_404<- 0 }
      if (length(area_ha_405)==0) { area_ha_405<- 0 }
      if (length(area_ha_406)==0) { area_ha_406<- 0 }
      if (length(area_ha_407)==0) { area_ha_407<- 0 }
      if (length(area_ha_501)==0) { area_ha_501<- 0 }
      if (length(area_ha_502)==0) { area_ha_502<- 0 }
      if (length(area_ha_503)==0) { area_ha_503<- 0 }
      if (length(area_ha_504)==0) { area_ha_504<- 0 }
      if (length(area_ha_505)==0) { area_ha_505<- 0 }
      if (length(area_ha_506)==0) { area_ha_506<- 0 }
      if (length(area_ha_507)==0) { area_ha_507<- 0 }
      if (length(area_ha_601)==0) { area_ha_601<- 0 }
      if (length(area_ha_602)==0) { area_ha_602<- 0 }
      if (length(area_ha_603)==0) { area_ha_603<- 0 }
      if (length(area_ha_604)==0) { area_ha_604<- 0 }
      if (length(area_ha_605)==0) { area_ha_605<- 0 }
      if (length(area_ha_606)==0) { area_ha_606<- 0 }
      if (length(area_ha_607)==0) { area_ha_607<- 0 }
      if (length(area_ha_701)==0) { area_ha_701<- 0 }
      if (length(area_ha_702)==0) { area_ha_702<- 0 }
      if (length(area_ha_703)==0) { area_ha_703<- 0 }
      if (length(area_ha_704)==0) { area_ha_704<- 0 }
      if (length(area_ha_705)==0) { area_ha_705<- 0 }
      if (length(area_ha_706)==0) { area_ha_706<- 0 }
      if (length(area_ha_707)==0) { area_ha_707<- 0 }
      
      area_ha_total_nochange <- area_ha_101 + area_ha_202 + area_ha_303 + area_ha_404 + area_ha_505 + area_ha_606 + area_ha_707
      area_ha_total_correct <- 
        area_ha_102 + area_ha_106 + area_ha_107 +
        area_ha_201 + area_ha_203 + area_ha_206 + 
        area_ha_304 + area_ha_306 +
        area_ha_405 + area_ha_406 +
        area_ha_501 + area_ha_502 + area_ha_506 + area_ha_507 +
        area_ha_601 + area_ha_602 + area_ha_603 + area_ha_604 + area_ha_605 + area_ha_607 +
        area_ha_701 + area_ha_702
      area_ha_total_incorrect <- 
        area_ha_103 + area_ha_104 + area_ha_105 + 
        area_ha_204 + area_ha_205 + area_ha_207 + 
        area_ha_301 + area_ha_302 + area_ha_305 + area_ha_307 + 
        area_ha_401 + area_ha_402 + area_ha_403 + area_ha_407 +
        area_ha_503 + area_ha_504 + 
        area_ha_703 + area_ha_704 + area_ha_705 + area_ha_706
      area_ha_total_true <- area_ha_total_nochange + area_ha_total_correct
      area_ha_total_all <- area_ha_total_nochange + area_ha_total_correct + area_ha_total_incorrect
      
      compare_total_RS <- area_ha_total_all/area_RS*100
      compare_total_BPS <- area_ha_total_all/area_BPS*100
      
      pct_101 <- area_ha_101 / area_ha_total_all * 100
      pct_102 <- area_ha_102 / area_ha_total_all * 100
      pct_103 <- area_ha_103 / area_ha_total_all * 100
      pct_104 <- area_ha_104 / area_ha_total_all * 100
      pct_105 <- area_ha_105 / area_ha_total_all * 100
      pct_106 <- area_ha_106 / area_ha_total_all * 100
      pct_107 <- area_ha_107 / area_ha_total_all * 100
      pct_201 <- area_ha_201 / area_ha_total_all * 100
      pct_202 <- area_ha_202 / area_ha_total_all * 100
      pct_203 <- area_ha_203 / area_ha_total_all * 100
      pct_204 <- area_ha_204 / area_ha_total_all * 100
      pct_205 <- area_ha_205 / area_ha_total_all * 100
      pct_206 <- area_ha_206 / area_ha_total_all * 100
      pct_207 <- area_ha_207 / area_ha_total_all * 100
      pct_301 <- area_ha_301 / area_ha_total_all * 100
      pct_302 <- area_ha_302 / area_ha_total_all * 100
      pct_303 <- area_ha_303 / area_ha_total_all * 100
      pct_304 <- area_ha_304 / area_ha_total_all * 100
      pct_305 <- area_ha_305 / area_ha_total_all * 100
      pct_306 <- area_ha_306 / area_ha_total_all * 100
      pct_307 <- area_ha_307 / area_ha_total_all * 100
      pct_401 <- area_ha_401 / area_ha_total_all * 100
      pct_402 <- area_ha_402 / area_ha_total_all * 100
      pct_403 <- area_ha_403 / area_ha_total_all * 100
      pct_404 <- area_ha_404 / area_ha_total_all * 100
      pct_405 <- area_ha_405 / area_ha_total_all * 100
      pct_406 <- area_ha_406 / area_ha_total_all * 100
      pct_407 <- area_ha_407 / area_ha_total_all * 100
      pct_501 <- area_ha_501 / area_ha_total_all * 100
      pct_502 <- area_ha_502 / area_ha_total_all * 100
      pct_503 <- area_ha_503 / area_ha_total_all * 100
      pct_504 <- area_ha_504 / area_ha_total_all * 100
      pct_505 <- area_ha_505 / area_ha_total_all * 100
      pct_506 <- area_ha_506 / area_ha_total_all * 100
      pct_507 <- area_ha_507 / area_ha_total_all * 100
      pct_601 <- area_ha_601 / area_ha_total_all * 100
      pct_602 <- area_ha_602 / area_ha_total_all * 100
      pct_603 <- area_ha_603 / area_ha_total_all * 100
      pct_604 <- area_ha_604 / area_ha_total_all * 100
      pct_605 <- area_ha_605 / area_ha_total_all * 100
      pct_606 <- area_ha_606 / area_ha_total_all * 100
      pct_607 <- area_ha_607 / area_ha_total_all * 100
      pct_701 <- area_ha_701 / area_ha_total_all * 100
      pct_702 <- area_ha_702 / area_ha_total_all * 100
      pct_703 <- area_ha_703 / area_ha_total_all * 100
      pct_704 <- area_ha_704 / area_ha_total_all * 100
      pct_705 <- area_ha_705 / area_ha_total_all * 100
      pct_706 <- area_ha_706 / area_ha_total_all * 100
      pct_707 <- area_ha_707 / area_ha_total_all * 100
      
      pct_total_nochange <- area_ha_total_nochange / area_ha_total_all * 100
      pct_total_correct <- area_ha_total_correct / area_ha_total_all * 100
      pct_total_true <- area_ha_total_true / area_ha_total_all * 100
      pct_total_incorrect <- area_ha_total_incorrect / area_ha_total_all * 100
      
      
      results_root[nrow(results_root) + 1, ] <- c(
        as.character(sensor),
        as.character(model_ML),
        as.character(no_rows),
        as.character(prov_temp),
        as.character(time),
        as.double(area_ha_101),
        as.double(area_ha_102),
        as.double(area_ha_103),
        as.double(area_ha_104),
        as.double(area_ha_105),
        as.double(area_ha_106),
        as.double(area_ha_107),
        as.double(area_ha_201),
        as.double(area_ha_202),
        as.double(area_ha_203),
        as.double(area_ha_204),
        as.double(area_ha_205),
        as.double(area_ha_206),
        as.double(area_ha_207),
        as.double(area_ha_301),
        as.double(area_ha_302),
        as.double(area_ha_303),
        as.double(area_ha_304),
        as.double(area_ha_305),
        as.double(area_ha_306),
        as.double(area_ha_307),
        as.double(area_ha_401),
        as.double(area_ha_402),
        as.double(area_ha_403),
        as.double(area_ha_404),
        as.double(area_ha_405),
        as.double(area_ha_406),
        as.double(area_ha_407),
        as.double(area_ha_501),
        as.double(area_ha_502),
        as.double(area_ha_503),
        as.double(area_ha_504),
        as.double(area_ha_505),
        as.double(area_ha_506), 
        as.double(area_ha_507),
        as.double(area_ha_601),
        as.double(area_ha_602),
        as.double(area_ha_603),
        as.double(area_ha_604),
        as.double(area_ha_605),
        as.double(area_ha_606), 
        as.double(area_ha_607),
        as.double(area_ha_701),
        as.double(area_ha_702),
        as.double(area_ha_703),
        as.double(area_ha_704),
        as.double(area_ha_705),
        as.double(area_ha_706), 
        as.double(area_ha_707),
        as.double(area_ha_total_nochange),
        as.double(area_ha_total_correct),
        as.double(area_ha_total_true),
        as.double(area_ha_total_incorrect),
        as.double(area_ha_total_all),
        as.double(area_RS),
        as.double(area_BPS),
        as.double(compare_total_RS),
        as.double(compare_total_BPS),
        as.double(pct_101),
        as.double(pct_102),
        as.double(pct_103),
        as.double(pct_104),
        as.double(pct_105),
        as.double(pct_106),
        as.double(pct_107),
        as.double(pct_201),
        as.double(pct_202),
        as.double(pct_203),
        as.double(pct_204),
        as.double(pct_205),
        as.double(pct_206),
        as.double(pct_207),
        as.double(pct_301),
        as.double(pct_302),
        as.double(pct_303),
        as.double(pct_304),
        as.double(pct_305),
        as.double(pct_306),
        as.double(pct_307),
        as.double(pct_401),
        as.double(pct_402),
        as.double(pct_403),
        as.double(pct_404),
        as.double(pct_405),
        as.double(pct_406), 
        as.double(pct_407),
        as.double(pct_501),
        as.double(pct_502),
        as.double(pct_503),
        as.double(pct_504),
        as.double(pct_505),
        as.double(pct_506),
        as.double(pct_507),
        as.double(pct_601),
        as.double(pct_602),
        as.double(pct_603),
        as.double(pct_604),
        as.double(pct_605),
        as.double(pct_606), 
        as.double(pct_607),
        as.double(pct_701),
        as.double(pct_702),
        as.double(pct_703),
        as.double(pct_704),
        as.double(pct_705),
        as.double(pct_706), 
        as.double(pct_707),
        as.double(pct_total_nochange),
        as.double(pct_total_correct),
        as.double(pct_total_true),
        as.double(pct_total_incorrect)
      )
      
    }
  }
} 
filename_csv_paddy_subdistrict <- paste(mainDir_results,"Recap-Change-MOD13Q1_result_root.csv", sep="")
write.csv(results_root, file = filename_csv_paddy_subdistrict)
