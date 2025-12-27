#### Color scale ####
My_color <- c("TraCE Armenia"= "#49708A", "Armenia" = "#49708A", "TraCE Caucasus" = "#9A66D1", "Caucasus" = "#9A66D1", "GDGT" = "#9A66D1", "Speleo" = "#00BA38", "MPCOQ" = "#49708A", "MTCOQ" = "#49708A", "MPWAQ" = "#AF332E", "MTWAQ" = "#AF332E",
              "Azerbaijan" = "#9A66D1", "Uzbekistan" = "#EB9122", "Georgia" = "#FF6E91", "CRUTS" = "#FF6E91", "Caspian" = "#387E97", "Black sea" = "grey30", "Pollen-CWM" = "#72a72bff",
              "Pollen" = "#EB9122", "TraCE" = "#49708A", "Climate model" = "#664174", 
              "Leaf Area" = "#72a72bff", "Height" = "#00BA38", "Leaf N" = "#49708A", "Seed mass" = "#BEA33A","SSD" = "#EB9122",
              "Leaf area" = "#72a72bff", "Plant height" = "#00BA38", "Nleaf" = "#49708A"
)

#### Calculation Functions ####

Clim.param.extraction <- function(M, Clim.cal, All.param, Season, Seasonality, Map.display, 
                                  Altitude, Chelsa, Aridity, Biome, MAF, Csv.sep = ",", Soil.temp = F,
                                  Clim.display, Land.cover, Nb.map, Save.path, Save.plot, H, W){
  #### Initialisation variable ####
  library(raster) # import data raster
  library(maps)   # carte simple
  
  Save.tab = T
  if(missing(Map.display)){Map.display = F}
  if(missing(Clim.cal)){Clim.cal = T}
  if(missing(Chelsa)){Chelsa = F}
  if(missing(Season)){Season = F}
  if(missing(Seasonality)){Seasonality = F}
  if(missing(Biome)){Biome = F}
  if(missing(Land.cover)){Land.cover = F}
  if(missing(Aridity)){Aridity = F}
  if(missing(MAF)){MAF = F}
  if(missing(All.param)){All.param = F}
  if(missing(Clim.display)){Clim.display = T}
  if(missing(Nb.map)){Nb.map = 1}
  if(missing(Save.path)){Save.tab = F}
  if(missing(Altitude)){Altitude = F}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  
  #### Check if data.frame or list of data.frame ####
  if(length(M) == 3 & any(names(M) %in% c("Elevation", "Altitude", "Site")) == F){M2 <- M$Msites}
  else{M2 <- t(M)}
  Lat <- c(grep("lat", row.names(M2)), grep("Lat", row.names(M2)), grep("LAT", row.names(M2)))
  Long <- c(grep("lon", row.names(M2)), grep("Lon", row.names(M2)),grep("LON", row.names(M2)))
  
  #### Recuperations des coordonnées GPS ####
  DB.coord <- data.frame(t(M2))
  Lat <- as.numeric(M2[Lat,])
  Long <- as.numeric(M2[Long,])
  DB.coord.num <- data.frame(cbind(Longitude = Long, Latitude = Lat))
  row.names(DB.coord.num) <- row.names(DB.coord)
  
  #### Clean NAs ####
  Row.NA <- DB.coord.num[is.na(DB.coord.num$Longitude) | is.na(DB.coord.num$Latitude),]
  if(dim(Row.NA)[1] > 0){
    print("**** Warning ! These sites have NA as GPS coordinates and have been removed. ****")
    print(Row.NA)
    Keep.row.order <- row.names(DB.coord.num)
    DB.coord.num <- DB.coord.num[!row.names(DB.coord.num) %in% row.names(Row.NA),]
    Lat <- Lat[!is.na(Lat)]
    Long <- Long[!is.na(Long)]
  }
  DB.coord.SP <- SpatialPoints(DB.coord.num, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  
  #### Clim parameters ####
  if(Chelsa == F){
    # Clim.path = "/media/lucas.dugerdil/Maximator/Documents/Recherche/SIG/Data_climat/WorldClim/Actual2.0/"
    Clim.path = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Data_climat/WorldClim/Actual2.0/"
    if(file.exists(Clim.path) == F){warning("The disk dure n'a pas ete branche.")}
    Clim.param = list(
      MAAT.path = "Bio_index/wc2.1_30s_bio/wc2.1_bio_30s_01.tif",
      MAP.path = "Bio_index/wc2.1_30s_bio/wc2.1_bio_30s_12.tif",
      MTWAQ.path = "Bio_index/wc2.1_30s_bio/wc2.1_bio_30s_10.tif",
      MTCOQ.path = "Bio_index/wc2.1_30s_bio/wc2.1_bio_30s_11.tif",
      MPWAQ.path = "Bio_index/wc2.1_30s_bio/wc2.1_bio_30s_18.tif",
      MPCOQ.path = "Bio_index/wc2.1_30s_bio/wc2.1_bio_30s_19.tif"
    )
    #### Seasonality ####
    if(Seasonality == T){
      Clim.param2 = list(
        TS.path = "Bio_index/wc2.1_30s_bio/wc2.1_bio_30s_04.tif",     # Temperature Seasonality [standard deviation]
        PS.path = "Bio_index/wc2.1_30s_bio/wc2.1_bio_30s_15.tif"     # Precipitation Seasonality [coefficient of variation]
      )
      Clim.param <- c(Clim.param, Clim.param2)
    }
    
    #### Clim param part2 ####
    if(All.param == T | MAF == T){
      Clim.param2 = list(
        T.jan.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_01.tif",
        T.fev.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_02.tif",
        T.mar.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_03.tif",
        T.avr.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_04.tif",
        T.mai.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_05.tif",
        T.juin.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_06.tif",
        T.juil.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_07.tif",
        T.aou.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_08.tif",
        T.sep.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_09.tif",
        T.oct.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_10.tif",
        T.nov.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_11.tif",
        T.dec.path = "Mean_temp/wc2.1_30s_tavg/wc2.1_30s_tavg_12.tif",
        P.jan.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_01.tif",
        P.fev.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_02.tif",
        P.mar.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_03.tif",
        P.avr.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_04.tif",
        P.mai.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_05.tif",
        P.juin.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_06.tif",
        P.juil.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_07.tif",
        P.aou.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_08.tif",
        P.sep.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_09.tif",
        P.oct.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_10.tif",
        P.nov.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_11.tif",
        P.dec.path = "Precipitation/wc2.1_30s_prec/wc2.1_30s_prec_12.tif"
      )
      Clim.param <- c(Clim.param, Clim.param2)
    }}
  
  if(Chelsa == T){
    # Clim.path = "/media/lucas.dugerdil/Maximator/Documents/Recherche/SIG/Data_climat/Chelsa_1.2/"
    Clim.path = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Data_climat/Chelsa_1.2/"
    if(file.exists(Clim.path) == F){warning("The disk dure n'a pas ete branche.")}
    Clim.param = list(
      MAAT.path = "CHELSA_bio10_01.tif",     # Annual Mean Temperature [°C*10]
      MAP.path = "CHELSA_bio10_12.tif",      # Annual Precipitation [mm/year]
      MTWAQ.path = "CHELSA_bio10_10.tif",    # Mean Temperature of Warmest Quarter [°C*10]
      MTCOQ.path = "CHELSA_bio10_11.tif",    # Mean Temperature of Coldest Quarter [°C*10]
      MPWAQ.path = "CHELSA_bio10_18.tif",    # Precipitation of Warmest Quarter [mm/quarter]
      MPCOQ.path = "CHELSA_bio10_19.tif"     # Precipitation of Coldest Quarter [mm/quarter]
    )
    
    #### Seasonality ####
    if(Seasonality == T){
      Clim.param2 = list(
        TS.path = "CHELSA_bio10_04.tif",     # Temperature Seasonality [standard deviation]
        PS.path = "CHELSA_bio10_15.tif"      # Precipitation Seasonality [coefficient of variation]
      )
      Clim.param <- c(Clim.param, Clim.param2)
    }
    
    #### Clim param part2 ####
    if(All.param == T){
      Clim.param2 = list(
        MTDR.path = "CHELSA_bio10_02.tif",   # Mean Diurnal Range [°C]
        IsoTh.path = "CHELSA_bio10_03.tif",  # Isothermality
        MxT.path = "CHELSA_bio10_05.tif",    # Max Temperature of Warmest Month [°C*10]
        MnT.path = "CHELSA_bio10_06.tif",    # Min Temperature of Coldest Month [°C*10]
        TAR.path = "CHELSA_bio10_07.tif",    # Temperature Annual Range [°C*10]
        MTWeQ.path = "CHELSA_bio10_08.tif",  # Mean Temperature of Wettest Quarter [°C*10]
        MTDrQ.path = "CHELSA_bio10_09.tif",  # Mean Temperature of Driest Quarter [°C*10]
        MPWeM.path = "CHELSA_bio10_13.tif",  # Precipitation of Wettest Month [mm/month]
        MPDrM.path = "CHELSA_bio10_14.tif",  # Precipitation of Driest Month [mm/month]
        MPWeQ.path = "CHELSA_bio10_16.tif",  # Precipitation of Wettest Quarter [mm/quarter] 
        MPDrQ.path = "CHELSA_bio10_17.tif"   # Precipitation of Driest Quarter [mm/quarter]
      )
      Clim.param <- c(Clim.param, Clim.param2)
    }}
  
  
  #### Altitude ####
  if(Altitude == T){
    Alt.path = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/MNT/"
    Alt.param = list(
      Alt.WC2 = "WorldClim2_MNT/wc2.1_30s_elev.tif",
      Alt.Mong = "Mongolia_ASTER/WGS84/DEM_Mongolia.tif",
      Alt.Baikal = "Russie/Baikal_ASTER/Baikal_ASTER.tif",
      Alt.Az = "Azerbaijan_SRTM/merge/SRTM_azer_merge.tif",
      Alt.Iran = "Iran/DEM_Golestan_PN_full",
      Alt.Europe1 = "Europe/EUD_CP-DEMS_3500025000-AA.tif",
      Alt.Europe2 = "Europe/EUD_CP-DEMS_3500035000-AA.tif",
      Alt.Europe3 = "Europe/EUD_CP-DEMS_3500045000-AA.tif",
      Alt.Europe4 = "Europe/EUD_CP-DEMS_3500055000-AA.tif",
      Alt.Europe5 = "Europe/EUD_CP-DEMS_4500015000-AA.tif"
    )
    
    #### Extraction des valeurs altitude ATTENTION il fait la moyenne !!!! ####
    for(j in 1:length(Alt.param)){
      Alt.map = raster::raster(paste(Alt.path, Alt.param[[j]], sep = ""))
      DB.coord.SP <- spTransform(DB.coord.SP, crs(Alt.map))
      Alt.extract <- raster::extract(Alt.map, DB.coord.SP)
      Name.var = gsub(".path","", names(Alt.param)[[j]])
      DB.coord.num <- cbind(DB.coord.num, Alt.extract)
      colnames(DB.coord.num)[ncol(DB.coord.num)] = Name.var
    }
  }
  
  #### Soil temperature ####
  if(Soil.temp == T){
    Soil.path = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Data_climat/GSB_2.0/"
    Soil.param = list(
      Soil.Europe5 = "Europe/EUD_CP-DEMS_4500015000-AA.tif"
    )
    
    #### Extraction des valeurs altitude ATTENTION il fait la moyenne !!!! ####
    for(j in 1:length(Soil.param)){
      Soil.map = raster::raster(paste(Soil.path, Soil.param[[j]], sep = ""))
      DB.coord.SP <- spTransform(DB.coord.SP, crs(Soil.map))
      Soil.extract <- raster::extract(Soil.map, DB.coord.SP)
      Name.var = gsub(".path","", names(Soil.param)[[j]])
      DB.coord.num <- cbind(DB.coord.num, Soil.extract)
      colnames(DB.coord.num)[ncol(DB.coord.num)] = Name.var
    }
  }
  
  #### Evapotranspiration ####
  if(Aridity == T){
    # Aridity.path = "/media/lucas.dugerdil/Maximator/Documents/Recherche/SIG/Data_Water/CGAR/"
    Aridity.path = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Data_Water/CGAR/"
    Aridity.param = list(AI.path = "ai_et0/ai_et0.tif")
    #### Extraction des valeurs aridity ####
    for(j in 1:length(Aridity.param)){
      Aridity.map = raster::raster(paste(Aridity.path, Aridity.param[[j]], sep = ""))
      crs(Aridity.map) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      DB.coord.SP <- spTransform(DB.coord.SP, crs(Aridity.map))
      Aridity.extract <- raster::extract(Aridity.map, DB.coord.SP)
      Name.var = gsub(".path","", names(Aridity.param)[[j]])
      DB.coord.num <- cbind(DB.coord.num, Aridity.extract)
      colnames(DB.coord.num)[ncol(DB.coord.num)] = Name.var
    }
  }
  
  #### Biome ####
  if(Biome == T){
    if(exists("Pth.Dinerstein") == F){
      # Pth.Dinerstein <- "/media/lucas.dugerdil/Maximator/Documents/Recherche/SIG/Data_vegetation/Ecoregions2017/Ecoregions2017.shp"
      Pth.Dinerstein <- "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Data_vegetation/Ecoregions2017/Ecoregions2017.shp"
      biom = readOGR(Pth.Dinerstein)
      proj4string(biom) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    }
    Biome.extract <- data.frame(over(DB.coord.SP, biom))
    Biome.extract <- Biome.extract[c(2,3,4)]
    names(Biome.extract) <- c("Ecosystem", "Biome.no", "Biome")
    DB.coord.num <- cbind(DB.coord.num, Biome.extract)
  }
  
  #### Land Cover ####
  if(Land.cover == T){
    Pth.GLC2000 <- "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Global_Land_Cover_2000/glc2000_v1_1_tif/"
    GLC2000.param <- list(GLC2000.path = "glc2000_v1_1.tif")
    GLC2000.label <- data.frame(read.csv(paste(Pth.GLC2000, "leg_glc2000_v1_1.csv", sep = ""), sep = "\t" ,dec=".",header=T,row.names=1), stringsAsFactors = T)
    for(j in 1:length(GLC2000.param)){
      GLC2000.map = raster::raster(paste(Pth.GLC2000, GLC2000.param[[j]], sep = ""))
      crs(GLC2000.map) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      DB.coord.SP <- spTransform(DB.coord.SP, crs(GLC2000.map))
      GLC2000.extract <- raster::extract(GLC2000.map, DB.coord.SP)
      GLC2000.names <- GLC2000.label$CLASSNAMES[GLC2000.extract]
      Name.var = gsub(".path","", names(GLC2000.param)[[j]])
      DB.coord.num <- cbind(DB.coord.num, GLC2000.extract, GLC2000.names)
      Name.var = c(Name.var, "GLC2000.lab")
      print(Name.var)
      print(colnames(DB.coord.num)[c(ncol(DB.coord.num)-1,ncol(DB.coord.num))])
      colnames(DB.coord.num)[c(ncol(DB.coord.num)-1,ncol(DB.coord.num))] = Name.var
    }
  }
  
  
  #### Extraction des valeurs clim ####
  if(Clim.cal == T){
    for(i in 1:length(Clim.param)){
      Clim.map = raster(paste(Clim.path, Clim.param[[i]], sep = ""))
      crs(Clim.map) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      
      DB.coord.SP <- spTransform(DB.coord.SP, crs(Clim.map))
      Clim.extract <- raster::extract(Clim.map, DB.coord.SP)
      Name.var = gsub(".path","", names(Clim.param)[[i]])
      
      if(Chelsa == T){
        Param.divide <- c("MAAT", "MxT", "MnT", "TAR", "MTWeQ", "MTDrQ", "MTWAQ", "MTCOQ")
        if(Name.var %in% Param.divide){Clim.extract <- Clim.extract/10}
      }
      
      DB.coord.num <- cbind(DB.coord.num, Clim.extract)
      colnames(DB.coord.num)[ncol(DB.coord.num)] = Name.var
    }}
  
  #### Map display ####
  if(Map.display == T){
    #### Save plots ####
    if(is.null(Save.plot) == F){
      if(is.null(W) == F & is.null(H) == F){
        pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
      else{pdf(file = Save.plot)}}
    
    #### Panel settings ####
    if (Nb.map <= 3){par(mfrow = c(1, Nb.map), mai = c(0.6, 0.6, 0.2, 0.2))}
    if (Nb.map == 4){par(mfrow = c(2,2), mai = c(0.6, 0.6, 0.2, 0.2))}
    if (Nb.map >= 5 & Nb.map <= 6){par(mfrow = c(2,3), mai = c(0.6, 0.6, 0.2, 0.2))}
    if (Nb.map >= 7 & Nb.map <= 9){par(mfrow = c(3,3), mai = c(0.6, 0.6, 0.2, 0.2))}
    if (Nb.map >= 10 & Nb.map <= 12){par(mfrow = c(3,4), mai = c(0.6, 0.6, 0.2, 0.2))}
    if (Nb.map >= 13 & Nb.map <= 16){par(mfrow = c(4,4), mai = c(0.6, 0.6, 0.2, 0.2))}
    
    #### Map settings ####
    Xlim <- c(min(Long)-.05*min(Long), max(Long)+.05*max(Long))
    Ylim <- c(min(Lat)-.05*min(Lat), max(Lat)+.05*max(Lat))
    
    #### Main loop ####
    for(j in 1:Nb.map){
      if(Clim.display == T){
        Add.P = T
        Clim.map = raster(paste(Clim.path, Clim.param[[j]], sep = ""))
        Name.var = gsub(".path","", names(Clim.param)[[j]])
        
        plot(Clim.map, 
             xlim = Xlim, 
             ylim = Ylim,
             main = Name.var,
             col = rev(heat.colors(9, alpha = 1)),
             xlab = "Longitude (°)",
             ylab = "Latitude (°)"
        )}
      
      if(Clim.display == F){
        Add.P = F
        plot.new()}
      map("world",# regions = c("Mongolia", "China", "Japan", "Laos", "Vietnam", "India", "Russia", "Corea"), 
          col="black", lty = "dashed", add = Add.P,
          xlim = Xlim,
          ylim = Ylim
      )
      points(DB.coord.SP,           
             pch = 19,
             cex = 1.5,
             col = "gray20"
      )
    }
  }
  #### Season calculation ####
  if(All.param == T & Season == T & Chelsa == F){
    DB.coord.num <- data.frame(DB.coord.num)
    DB.coord.num[["Tsum"]] <- (DB.coord.num$T.juil + DB.coord.num$T.juin + DB.coord.num$T.aou)/3
    DB.coord.num[["Twin"]] <- (DB.coord.num$T.dec + DB.coord.num$T.jan + DB.coord.num$T.fev)/3
    DB.coord.num[["Tfal"]] <- (DB.coord.num$T.sep + DB.coord.num$T.oct + DB.coord.num$T.nov)/3
    DB.coord.num[["Tspr"]] <- (DB.coord.num$T.mar + DB.coord.num$T.avr + DB.coord.num$T.mai)/3
    
    DB.coord.num[["Psum"]] <- (DB.coord.num$P.juil + DB.coord.num$P.juin + DB.coord.num$P.aou)
    DB.coord.num[["Pwin"]] <- (DB.coord.num$P.dec + DB.coord.num$P.jan + DB.coord.num$P.fev)
    DB.coord.num[["Pfal"]] <- (DB.coord.num$P.sep + DB.coord.num$P.oct + DB.coord.num$P.nov)
    DB.coord.num[["Pspr"]] <- (DB.coord.num$P.mar + DB.coord.num$P.avr + DB.coord.num$P.mai)
  }
  #### MAF ####
  if(MAF == T){
    if(Chelsa == T){
      print("We need WorldClim2 to calculate MAF. Please remove Chelsa = T.")
    }
    else{
      MAF <- DB.coord.num[grep("T\\.", names(DB.coord.num))]
      MAF[MAF < 0] <- NA
      MAF <- rowMeans(MAF, na.rm = T)
      DB.coord.num[["MAF"]] <- MAF
      
      if(All.param == F){
        DB.coord.num <- DB.coord.num[setdiff(names(DB.coord.num), gsub(".path","", names(Clim.param2)))]
      }
    }
  }
  
  #### Return ####
  par(mfrow = c(1,1))
  
  if(dim(Row.NA)[1] > 0){
    DB.coord.num <- DB.coord.num[Keep.row.order, ]
    row.names(DB.coord.num) <- Keep.row.order
  }
  
  if(length(M) == 3 & any(names(M) %in% "Elevation") == F){M[["MClim"]] <- DB.coord.num}
  else{M <- DB.coord.num}
  
  #### Altitudes merge ####
  if(Altitude == T){
    M <- data.frame(M)
    A <- M[,grep(paste("^Alt.", ".", sep = ""), colnames(M))]
    M <- M[, setdiff(1:ncol(M), grep(paste("^Alt.", ".", sep = ""), colnames(M)))]
    #A[is.na(A)] <- 0
    A <- round(rowMeans(A, na.rm = T), digits = 0) # Fait la moyenne entre les différents MNT
    A[is.nan(A)] <- NA
    M <- cbind(M, Altitude = A)}
  
  #### Export datas ####
  if(Save.tab == T){
    Path.to.create <- gsub("(.*/).*\\.csv.*","\\1", Save.path)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    write.table(M, file = Save.path, row.names=T, col.names=NA, sep = Csv.sep, dec = ".")}
  if(is.null(Save.plot) == F){dev.off()}
  # print(M)
  return(M)
}

MC.extract.pca.prep <- function(MSample.Clim, MSample.Biom, Pclim){
  MC <- cbind(MSample.Clim, MSample.Biom)
  MC <- cbind(MC, Type = "MPS")
  row.names(MC) <- paste("MPS", row.names(MC), sep = "-")
  PV <- data.frame(cbind(Pclim, Type = "PV"))
  row.names(PV) <- paste("PV", row.names(PV), sep = "-")
  PV <- PV[names(MC)]
  MCPV <- rbind(PV, MC)
  MCPV <- MCPV[MCPV$Biome.no %in% c(13,8,10,5,4,6),]
  return(MCPV)
}


CWT.calculation.2 <- function(MT, MP, Mclim = NULL, MPS.ACA.Biom = NULL, Accep.seuil, Remove.biom, Add.CWV = F){
  #### Settings ####
  if(missing(Remove.biom)){Remove.biom = NULL}
  
  #### Clean matrix ####
  if(any(colSums(MP) < 95)){
    print("Recalculate the FA for the species matrix.")
    Keep.real.names <- row.names(MP)
    MP <- data.frame(t(MP))
    MP <- MP/rowSums(MP)*100
    MP <- data.frame(t(MP))
    row.names(MP) <- Keep.real.names
  }
  
  MP$id <- row.names(MP)
  names(MT) <- gsub("X", "", names(MT))
  if(any(grepl("TRY", names(MT)) == T) == F){names(MT) <- paste("TRY", names(MT), sep = "_")}
  names(MT)[1] <- "id"
  MT <- MT[which(MT$id %in% MP$id),]
  # /!\ missing taxa ! 
  Missing.taxa <- setdiff(MP$id, MT$id)
  if(length(Missing.taxa) > 0){
    print("The following taxa are missing from the trait matrix:")
    print(Missing.taxa)
  }
  
  Missing.raw <- MT[setdiff(MP$id, MT$id),]
  Missing.raw$id <- setdiff(MP$id, MT$id)
  MT <- rbind(MT, Missing.raw)
  MT <- as_tibble(lapply(MT, function(x){x[is.nan(x)] <- NA ; x}))
  
  #### Merge MP + MT ####
  MPT <- merge(MT, MP, by = "id", all = T)
  MPT <- melt(MPT, id = names(MT))
  names(MPT)[names(MPT) == "variable"] <- "Site"
  names(MPT)[names(MPT) == "value"] <- "FA"
  
  #### Calculation of the CWT ####
  MCWT <- data.frame(Site = (unique(MPT["Site"])))
  MCWT.stat <- setNames(data.frame(NA,NA,NA), c("Trait", "Pour.sites.kept", "N.site"))
  MCWT.stat <- MCWT.stat[-1,]
  Tot.site.nb <- length(levels(MPT$Site))
  
  if(Add.CWV == T){MCWV <- MCWT; MCWV.stat = MCWT.stat}
  
  #### Verbose ####
  pb = txtProgressBar(min = 1, 
                      max = length(grep("TRY", names(MPT))),
                      width = 40,
                      initial = 0,  style = 3) 
  
  init <- numeric(length(grep("TRY", names(MPT))))
  end <- numeric(length(grep("TRY", names(MPT))))
  
  #### Main loop ####
  for(i in grep("TRY", names(MPT))){
    init[i] <- Sys.time()
    Trait.treat.i <- names(MPT)[i]
    A <- MPT[c("id", Trait.treat.i, "Site", "FA")]
    A$FA[is.na(A[[Trait.treat.i]])] <- NA
    if(all(is.na(A$FA)) == T){next}
    all_abund = aggregate(FA ~ Site, A, sum)
    colnames(all_abund)[2] = "tot_abund"
    Keep.sites <- all_abund[all_abund[2] > Accep.seuil,]
    
    #### Calculation CWM ####
    if(nrow(Keep.sites) > 0){
      N.site <- length(Keep.sites$tot_abund)
      Pourc.site.up.seuil <- round(length(Keep.sites$tot_abund)/Tot.site.nb, digits = 2)
      MCWT.stat[i,] <- c(Trait.treat.i, Pourc.site.up.seuil, N.site)
      A <- A[which(A$Site %in% Keep.sites$Site),]
      A <- merge(A, all_abund, by = "Site")
      A$FA <- A$FA/A$tot_abund
      XX <- aggregate(FA * eval(parse(text = Trait.treat.i)) ~ Site, A, sum, na.rm = T)
    }
    else{
      XX <- data.frame(Site = NA, X = "")
    }
    names(XX)[2] <- Trait.treat.i
    MCWT <- left_join(MCWT, XX, by = "Site")
    
    
    #### Add CWV ####
    if(Add.CWV == T){
      if(nrow(Keep.sites) > 0){
        
        A.var <- full_join(A, setNames(XX, c("Site", "CWM")), by = "Site")
        A.var$T.var <- (A.var[[names(A.var)[grep("TRY_", names(A.var))]]] - A.var$CWM)^2
        XX.var <- aggregate(FA * T.var ~ Site, A.var, sum, na.rm = T)
      }
      else{
        XX.var <- data.frame(Site = NA, X = "")
      }
      names(XX.var)[2] <- Trait.treat.i
      MCWV <- left_join(MCWV, XX.var, by = "Site")
    }
    
    #### Barre avancement ####
    end[i] <- Sys.time()
    setTxtProgressBar(pb, i)
    time <- round(seconds_to_period(sum(end - init)), 0)
    est <- length(grep("TRY", names(MPT))) * (mean(end[end != 0] - init[init != 0])) - time
    remainining <- round(seconds_to_period(est), 0)
    cat(paste(" // Execution time:", time,
              " // Estimated time remaining:", remainining), "")
  }
  close(pb)
  
  MCWT.stat <- MCWT.stat[-1,]
  MCWT.stat[nrow(MCWT.stat)+1,] <- c("Average", round(mean(as.numeric(MCWT.stat$Pour.sites.kept)), digits = 2), round(mean(as.numeric(MCWT.stat$N.site)), digits = 0))
  
  #### Merge CWT + climat ####
  if(is.null(Mclim) == F){
    Mclim[["Site"]] <- row.names(Mclim)
    MCWT.clim = merge(MCWT, Mclim, by = "Site")
    if(Add.CWV == T){MCWV.clim = merge(MCWV, Mclim, by = "Site")}
    
    if(is.null(MPS.ACA.Biom) == F){
      MPS.ACA.Biom[["Site"]] <- row.names(MPS.ACA.Biom)
      MCWT.clim = merge(MCWT.clim, MPS.ACA.Biom, by = intersect(names(MCWT.clim), names(MPS.ACA.Biom)))
      if(Add.CWV == T){MCWV.clim = merge(MCWV.clim, MPS.ACA.Biom, by = intersect(names(MCWV.clim), names(MPS.ACA.Biom)))}
    }
    if(is.null(Remove.biom) == F){
      MCWT.clim <- MCWT.clim[setdiff(seq(1,nrow(MCWT.clim)), which(MCWT.clim$Biome %in% Remove.biom)),]
      if(Add.CWV == T){MCWV.clim <- MCWV.clim[setdiff(seq(1,nrow(MCWV.clim)), which(MCWV.clim$Biome %in% Remove.biom)),]}
    }
  }
  
  #### Export ####
  if(Add.CWV == F){
    if(is.null(Mclim) == F){
      Lexport <- list(MCWT = MCWT.clim, MCWT.stat = MCWT.stat, Missing.taxon = Missing.taxa)
      if(is.null(Mclim) == T){
        Lexport <- list(MCWT = MCWT, MCWT.stat = MCWT.stat, Missing.taxon = Missing.taxa)}
    }}
  else{
    if(is.null(Mclim) == F){
      Lexport <- list(MCWT = MCWT.clim, MCWV = MCWV.clim, MCWT.stat = MCWT.stat, Missing.taxon = Missing.taxa)
      if(is.null(Mclim) == T){
        Lexport <- list(MCWT = MCWT, MCWV = MCWV, MCWT.stat = MCWT.stat, Missing.taxon = Missing.taxa)}
      
    }}
  return(Lexport)
}

Trait.aggregate.by.class <- function(M){
  M <- M[which((!is.na(M[[1]]))),]
  M <- aggregate(M, list(M[[1]]), FUN = mean, na.action = na.pass, na.rm = T)
  names(M)[1] <- names(M)[2]
  M <- M[-2]
  return(M)
}

Trait.aggregate.by.type <- function(TP, MT, name.var){
  Fuck.off <- c("species", "family", "genus", "PT_ss", "PT_sl", "Subreign", "kingdom", "order", "Other.clade")
  Col.to.keep <- setdiff(names(MT), Fuck.off)
  Row.to.keep <- setdiff(names(TP), Fuck.off)
  
  TP.work <- setNames(data.frame(matrix(NA, ncol = length(Col.to.keep)+1, nrow = length(Row.to.keep))), c(name.var,Col.to.keep))
  TP.work[[name.var]] <- Row.to.keep
  names(TP.work)[1] <- "species"
  
  T.m <- TP.work
  T.sd <- TP.work
  TP <- TP[c("species",Row.to.keep)]
  MT <- data.frame(MT[c("species",Col.to.keep)])
  pb = txtProgressBar(min = 0, max = ncol(TP), initial = 0) 
  print(paste("Aggregation with", name.var))
  
  for(i in 2:ncol(TP)){
    setTxtProgressBar(pb,i)
    S.to.pick <- TP$species[which(TP[i] == T)]
    for(j in 2:ncol(MT)){
      Val <- MT[which(MT$species %in% S.to.pick), j]
      Trait.mean <- mean(Val, na.rm = T)
      Trait.sd <- sd(Val, na.rm = T)
      T.m[i-1,j] <- Trait.mean
      T.sd[i-1,j] <- Trait.sd
    }
  }
  close(pb)
  return(list(Mean = T.m, SD = T.sd))
}

Stacking.quantif <- function(Imput.list, Keep.clim = NULL, Scaling = F, Plot.x = "Age", Type.name = NULL, Return.list = F, Add.bin.count = F, Keep.empty.bin = F, Bin.sd = F,
                             Limits = NULL, Binning = F, Anomaly = F, Detrending = F, Windows.length = NULL, Stacking = T,
                             Interpolation = F, Interp.step = 10, Filter.cut.off = 500, New.param.names = NULL, Calib.name = NULL) {
  
  #### Inner functions ####
  Scaling.fun <- function(x = NULL, a = -1, b = 1){x <- a + (b-a)*(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)); return(x)}
  
  Bin.by.TW <- function(x, Anomaly = F, Add.bin.count = F){
    Nb.param <- ncol(x)
    x <- dplyr::mutate(x, TimeWind = cut(Age, breaks = TW.x, include.lowest = F))
    
    if(Add.bin.count == T){
      counts <- as.data.frame(table(x$TimeWind))
      colnames(counts) <- c("Group.1", "N")
    }
    
    if(any(colSums(is.na(x[-c(1, ncol(x))])) == nrow(x))){
      Keep.order.col <- names(x) 
      Keep.col.na <- names(x[which(colSums(is.na(x)) == nrow(x))])
      x <- x[which(colSums(is.na(x)) != nrow(x))]
    }
    else{Keep.col.na <- NULL}
    
    if(Bin.sd == T){
      y <- aggregate(. ~ TimeWind, data = x[-c(1)], FUN = sd, na.rm = T, na.action = na.pass)
      names(y) <- c("Group.1", paste("SD", names(y)[-c(1)], sep = "_"))
    }
    
    x <- aggregate(x, list(x$TimeWind), FUN = mean, na.action = na.pass, na.rm = T)
    
    if(Keep.empty.bin == T){
      Add.missing.TW <- levels(x$Group.1)[which(!levels(x$Group.1) %in% unique(x$Group.1))]
      MNA <- data.frame(matrix(nrow = length(Add.missing.TW), ncol = ncol(x)))
      MNA[c(1)] <- Add.missing.TW
      names(MNA) <- names(x)
      x <- rbind(x, MNA)
    }
    
    if(Add.bin.count == T & Keep.empty.bin == F){counts <- counts[which(counts$Group.1 %in% unique(as.character(x$Group.1))),]}
    
    x$Age <- (as.numeric(gsub(",.*", "", gsub("\\]", "", gsub("\\(", "", x$Group.1))))+as.numeric(gsub(".*,", "", gsub("\\]", "", gsub("\\(", "", x$Group.1)))))/2
    x <- x[order(x$Age),]
    
    if(Bin.sd == T){
      x <- dplyr::full_join(x, y, by = "Group.1")
      
    }
    
    x <- x[!names(x) %in% c("TimeWind", "Group.1")]
    
    if(Anomaly == T){
      if(Bin.sd == F){x <- cbind(Age = x$Age, data.frame(apply(x[2:Nb.param], 2, function(x) x <- x - x[1])))}
      else{
        Off.set <- as.vector(unlist(x[1,2:Nb.param]))
        x <- cbind(Age = x$Age,
                   data.frame(apply(x[which(names(x) != "Age" & ! grepl("^SD_", names(x)))], 2, function(x) x <- x - x[1])),
                   x[grepl("^SD_", names(x))])
      }}
    if(Anomaly == F){x <- cbind(Age = x$Age, x[-c(1)])}
    
    if(Add.bin.count == T){x <- cbind(x, N = counts$N)}
    
    if(is.null(Keep.col.na) == F){
      x[Keep.col.na] <- NA
      if(Bin.sd == T){
        x[paste("SD", Keep.col.na, sep = "_")] <- NA
      }
    }
    return(x)
  }
  
  
  Corit.simple <- function (timser, detr = Detrending, method = c("InterpolationMethod", "DirectFiltering", "IntegrandInterpolationMethod", "NoFilter"), 
                            appliedFilter = c("gauss", "runmean", "lowpass"), fc, tn = seq(from = min(index(timser)), to = max(index(timser)), by = 10), dt,
                            int.method = c("linear", "nearest"), k = 5){
    library(corit)
    library(zoo)
    n <- max(index(timser))
    if(method == "InterpolationMethod"){res <- InterpolationMethod(detrTimser(timser, detr), fc, dt, n, int.method, appliedFilter, k)}
    if(method == "DirectFiltering"){res <- DirectFiltering(detrTimser(timser, detr), fc, tn, appliedFilter, k)}
    if(method == "IntegrandInterpolationMethod"){res <- IntegrandInterpolationMethod(detrTimser(timser, detr), fc, tn, appliedFilter, k)}
    if(method == "NoFilter"){res <- detrTimser(timser, detr)}
    return(res)
  }
  
  #### Preparation ####
  Imput.list <- Filter(Negate(is.null), Imput.list)
  if(is.null(New.param.names) == F){New.param.names <- c(Plot.x, New.param.names)}
  if(is.null(names(Imput.list)) == F){Keep.order <- names(Imput.list)}
  
  Imput.list <- lapply(Imput.list, function(x) x[!duplicated(x),])
  Imput.list <- lapply(Imput.list, function(x) x[intersect(Reduce(intersect, lapply(Imput.list, names)), names(x))])
  if(is.null(Calib.name) == F){
    for(i in 1:length(Imput.list)){
      Imput.list[[i]] <- Imput.list[[i]][c(Plot.x, names(Imput.list[[i]])[grep(paste(Calib.name, "$", sep = ""), names(Imput.list[[i]]))])]
      names(Imput.list[[i]]) <- gsub(paste(Calib.name, "$", sep = ""), "", names(Imput.list[[i]]))
    }
  }
  if(is.null(Keep.clim) == F){Imput.list <- lapply(Imput.list, function(x) x[intersect(c(Plot.x, Keep.clim), names(x))])}
  if(is.null(Limits) == F){
    Imput.list <- lapply(Imput.list, function(x){
      x <- x[x[Plot.x] >= Limits[1] & x[Plot.x] <= Limits[length(Limits)],]
      if(nrow(x) == 0){x <- NULL}
      return(x)
    })
    Imput.list <- Filter(Negate(is.null), Imput.list)
  }
  if(Scaling == T){Imput.list <- lapply(Imput.list, function(x) cbind(Age = x[Plot.x], data.frame(apply(x[-c(1)], 2, Scaling.fun))))}
  if(Anomaly == T & Binning == F){Imput.list <- lapply(Imput.list, function(y){cbind(y[1], apply(y[2:ncol(y)], 2, function(x) x <- x - x[1]))})}
  
  if(length(Imput.list) == 0){
    print("**** Not of the input data.frame is within the settled limits ! ****")
    return(NULL)}
  
  #### Corit cleaning ####
  if(Interpolation == T){Interpolation.method <- "InterpolationMethod"}
  else{Interpolation.method <- "NoFilter"}
  
  Imput.list <- lapply(Imput.list, function(x){
    if(Interpolation == T){
      x2 <- seq(min(x$Age, na.rm = T), max(x$Age, na.rm = T), by = Interp.step)
      x2 <- data.frame(Age = x2)
    }
    else{x2 <- x}
    
    for(i in 2:ncol(x)){
      if(all(is.na(x[i])) == F){
        P1 <- zoo::zoo(as.vector(unlist(x[i])), order.by = as.vector(unlist(x[1])))
        Cor <- Corit.simple(
          timser = P1,
          detr = Detrending,                       #remove linear trend time series
          method = Interpolation.method,
          appliedFilter = "gauss",
          fc = 1/Filter.cut.off, #cut-off frequency
          dt = Interp.step,    #time step for the interpolation
          int.method = "linear",  #kind of interpolation
        )
        
        x2[1] <- zoo::fortify.zoo(Cor)[1]
        x2[i] <- zoo::fortify.zoo(Cor)[2]
      }
      else{
        x2[1] <- x2[1]
        x2[i] <- NA
        
      }
    }
    names(x2) <- names(x)
    return(x2)
  }
  )
  
  #### Stacking and binning ####
  if(Stacking == T){
    Imput.list <- do.call(rbind, Imput.list)
    if(nrow(Imput.list)>1){
      if(Binning == T){
        if(is.null(Limits) == T){
          print(paste("**** No limits provided. We binned the data from the max(", Plot.x, ") to the min(", Plot.x, "). Use 'Limits' argument for manual limits. ****", sep = ""))
          Limits <- c(min(Imput.list, na.rm = T), max(Imput.list, na.rm = T))
        }
        if(is.null(Windows.length) == T){
          Windows.length <- round((Limits[2] - Limits[1])/15, digits = -1)
          print(paste("**** No 'Windows.length' provide, we binned the data by ", Windows.length, ". Use 'Windows.length' argument else. ****", sep = ""))
        }
        TW.x <- seq(Limits[1], Limits[2], Windows.length)
        Imput.list <- Bin.by.TW(Imput.list, Anomaly = Anomaly, Add.bin.count = Add.bin.count)
        
        if(Bin.sd == T & is.null(New.param.names) == F){New.param.names <- c(New.param.names, paste("SD", New.param.names[-c(1)], sep = "_"))}
        if(Add.bin.count == T & is.null(New.param.names) == F){New.param.names <- c(New.param.names, "N")}
      }
      
      #### Add type (or zone) name and change param.clim names ####
      if(is.null(Type.name) == F){
        Imput.list$Zone <- Type.name
        New.param.names <- c(New.param.names, "Zone")
      }
      
      if(is.null(New.param.names) == F){names(Imput.list) <- New.param.names}
      
      
      #### If not binned, return as list ####
      if(Return.list == T){
        if(Binning == F){
          Site.names <- gsub("\\..*", "", row.names(Imput.list))
          row.names(Imput.list) <- paste0(Site.names, ".Ech_", ave(seq_along(Site.names), Site.names, FUN = seq_along))
          Imput.list <- split(Imput.list, Site.names)
          Imput.list <- Imput.list[Keep.order]
          Imput.list <- Filter(Negate(is.null), Imput.list)
          
        }
        else{print("**** Be carefull! 'Return.list = T' is only for 'Binning = F'. ****")}
      }
      return(Imput.list)
    }
    else{
      print(paste("**** Be carefull! The limits do not contain any data in", Type.name, "****", sep = " "))
      return(NULL)
    }
  }
  
  #### No stacking and binning ####
  if(Stacking == F){
    if(Binning == T){
      for(i in 1:length(Imput.list)){
        Imput.list.i <- Imput.list[[i]]
        Limits.i <- Limits
        Windows.length.i <- Windows.length
        
        if(is.null(Limits.i) == T){
          print(paste("**** No limits provided. We binned the data from the max(", Plot.x, ") to the min(", Plot.x, "). Use 'Limits' argument for manual limits. ****", sep = ""))
          Limits.i <- c(min(Imput.list.i, na.rm = T), max(Imput.list.i, na.rm = T))
        }
        if(is.null(Windows.length.i) == T){
          Windows.length.i <- round((Limits.i[2] - Limits.i[1])/15, digits = -1)
          print(paste("**** No 'Windows.length' provide, we binned the data by ", Windows.length, ". Use 'Windows.length' argument else. ****", sep = ""))
        }
        TW.x <- seq(Limits.i[1], Limits.i[2], Windows.length.i)
        
        Imput.list.i <- Bin.by.TW(Imput.list.i, Anomaly = Anomaly, Add.bin.count = Add.bin.count)
        if(Bin.sd == T & is.null(New.param.names) == F){New.param.names <- c(New.param.names, paste("SD", New.param.names[-c(1)], sep = "_"))}
        if(Add.bin.count == T & is.null(New.param.names) == F){New.param.names <- c(New.param.names, "N")}
        if(is.null(New.param.names) == F){names(Imput.list.i) <- New.param.names}
        
        Imput.list[[i]] <- Imput.list.i
      }
      return(Imput.list)
    }
    else{
      if(Return.list == T){
        Imput.list <- lapply(Imput.list, function(df){
          Site.names <- gsub("\\..*", "", row.names(df))
          row.names(df) <- paste0(Site.names, ".Ech_", ave(seq_along(Site.names), Site.names, FUN = seq_along))
          return(df)
        })
        Imput.list <- Imput.list[Keep.order]
        Imput.list <- Filter(Negate(is.null), Imput.list)
        
        if(is.null(Type.name) == F){
          Imput.list <- lapply(Imput.list, function(df){
            df$Zone <- Type.name
            return(df)})
          New.param.names <- c(New.param.names, "Zone")
        }
        
        if(is.null(New.param.names) == F){
          Imput.list <- lapply(Imput.list, function(df){
            names(df) <- New.param.names
            return(df)})
        }
        
        return(Imput.list)
      }
      else{
        Imput.list <- do.call(rbind, Imput.list)
        
        if(is.null(Type.name) == F){
          Imput.list$Zone <- Type.name
          New.param.names <- c(New.param.names, "Zone")}
        
        if(is.null(New.param.names) == F){names(Imput.list) <- New.param.names}
        
        return(Imput.list)
      }
    }
  }
  
}

Mat.corel.CWT.clim <- function(Mclim, Mtrait, I.confiance = NULL, Display.pval, Disp.R = "pie", Display = "full", ggplot.version = F,
                               Title, Label, Save.path = NULL, return.pick, Bar.pos = "b", my_color, Use.cor = "pairwise.complete.obs", Xlab.pos = NULL,
                               Good.tiles = NULL,  Bad.tiles = NULL, Xlab.rot = 45, Return.slope = F, Print.result = T, No.axis.lab = F,
                               Permutation.test = F, Nb.permutations = 1000, Save.pval = NULL, Bold = NULL, Order.hclust = F,
                               Method.cor, Save.plot = NULL, W, H, Order.by.alphabet, Average){
  #### Settings ####
  Save.tab = T
  library(corrplot)
  if(missing(return.pick)){return.pick = F}
  if(missing(Order.by.alphabet)){Order.by.alphabet = F}
  if(missing(Label)){Label = T}
  if(missing(Average)){Average = T}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Method.cor)){Method.cor = "pearson"}
  if(missing(Display.pval)){Display.pval = "pch"}
  if(missing(Mtrait)){Mtrait = NULL}
  if(missing(Title)){Title = NULL}
  if(missing(my_color)){my_color = colorRampPalette(c("royalblue", "white", "darkorange"))(50)}
  if(Display.pval == "blank"){Subt = paste("*The cells are blanked when ever the p-value is over the", I.confiance, "significant level.")}
  if(Display.pval == "pch"){Subt = paste("*The cells are crossed when ever the p-value is over the", I.confiance, "significant level.")}
  if(Label == F){Subt <- NULL}
  
  #### Calculations ####
  if(identical(row.names(Mclim), row.names(Mtrait)) == F){print("****Be carefull !!! The 2 matrixes may be not sorted or similar shape !****")}
  if(is.null(Mtrait) == F){names(Mtrait) <- gsub("TRY_", "", names(Mtrait))}
  
  if(Order.by.alphabet == T){Mtrait <- Mtrait[order(names(Mtrait))]}
  
  Mcor <- cor(Mclim, Mtrait, use = Use.cor, method = Method.cor)
  Mvar <- var(Mclim, Mtrait)
  Mtot <- cbind(Mclim, Mtrait)
  
  if(is.null(I.confiance) == F){
    Resid <- cor.mtest(Mtot, conf.level = I.confiance)
    PV <- Resid$p
  }
  else{PV <- NULL}
  
  if(is.null(PV) == F){
    if(is.null(Mtrait) == F){PV <- PV[(ncol(Mtrait)+1):(ncol(Mtrait)+ncol(Mclim)), 1:ncol(Mtrait)]} 
    row.names(PV) <- row.names(Mcor)
    colnames(PV) <- colnames(Mcor)
  }
  
  if(Return.slope == T){
    Mtrait2 <- Mtrait
    Mclim2 <- Mclim
    slope_mat <- sapply(1:ncol(Mtrait2), function(j) {apply(Mclim2, 2, function(y) coef(lm(y ~ Mtrait2[, j]))[2])})
    colnames(slope_mat) <- colnames(Mcor)
  }
  
  #### Permutations test ####
  if(Permutation.test == T){
    library("jmuOutlier")
    print(paste("p-value calculated with permutation test made on ", Nb.permutations, " permutations.", sep = ""))
    PV.perm <- setNames(data.frame(matrix(NA, nrow = length(Mclim), length(Mtrait))), names(Mtrait))
    row.names(PV.perm) <- names(Mclim) 
    for(i in 1:length(names(Mclim))){
      for(j in 1:length(names(Mtrait))){
        set.seed(0)
        M.perm <- na.omit(cbind(Mclim[i], Mtrait[j]))
        PV.perm[i, j] <- perm.cor.test(M.perm[[1]], M.perm[[2]], num.sim = Nb.permutations)$p.value
      }
    }
    PV <- as.matrix(PV.perm)
  }
  
  #### Save p-values ####
  if(is.null(Save.pval) == F & is.null(PV) == F){
    Path.to.create <- gsub("(.*/).*\\.csv.*","\\1", Save.pval)
    dir.create(file.path(Path.to.create), showWarnings = F)
    write.table(PV, file = Save.pval, row.names=T, col.names = NA, sep=",", dec = ".")
    Save.path.RDS <- gsub("\\.csv", "\\.Rds", Save.pval)
    saveRDS(PV, Save.path.RDS)
  }
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Graphic param ####
  if(is.null(Bold) == F & ggplot.version == T){
    names(Bold) <- c("Var1", "Var2")
    Bold.case <- geom_tile(data = Bold, width = .95, height = .95, linewidth = 1, color = "grey10", fill = NA, alpha = .5)
  }
  else{Bold.case <- NULL}
  
  #### Highlight some tiles ####
  if(is.null(Good.tiles) == F){
    heat_data <- reshape2::melt(Mcor)
    heat_data$highlight <- apply(heat_data[, c("Var1", "Var2")], 1, function(row) {any(mapply(function(x) all(row == x), Good.tiles))})
    Bold.tiles <- geom_tile(data = subset(heat_data, highlight), mapping = aes(x = Var1, y = Var2), color = "green", size = 1.2, fill = NA)
  }
  else{Bold.tiles <- NULL}
  
  if(is.null(Bad.tiles) == F){
    heat_data <- melt(Mcor)
    heat_data$highlight <- apply(heat_data[, c("Trait1", "Trait2")], 1, function(row) {any(mapply(function(x) all(row == x), Bad.tiles))})
    Bold.tiles.2 <- geom_tile(data = subset(heat_data, highlight), color = "black", size = 1.2, fill = NA)
  }
  else{Bold.tiles.2 <- NULL}
  
  #### Plots (coorplor) ####
  if(ggplot.version == F){
    CP <- corrplot(Mcor, type = Display, col = my_color, tl.pos = Xlab.pos,
                   tl.col="black", tl.srt = Xlab.rot, tl.cex = .7, method = Disp.R, cl.align.text = "l", cl.pos = Bar.pos, 
                   p.mat = PV, sig.level = (1-I.confiance), insig = Display.pval, pch.cex = 2)
    
    title(main = Title, sub = Subt)
  }
  
  #### Plots (ggplot) ####
  if(ggplot.version == T){
    library(ggcorrplot)
    if(Disp.R == "pie"|Disp.R == "circle"){Disp.R.gg <- "circle"}
    if(Disp.R == "number"|Disp.R == "square"){Disp.R.gg <- "square"}
    
    if(Xlab.rot == 0){Xjust <- 0.5}
    if(Xlab.rot != 0){Xjust <- 1}
    
    Bar.pos.gg <- "top"
    if(Bar.pos == "b" | Bar.pos == "bottom"){Bar.pos.gg <- "bottom"}
    if(Bar.pos == "n" | Bar.pos == "none"){Bar.pos.gg <- "none"}
    
    if(No.axis.lab == T){Xlab.opt <- element_blank(); Ylab.opt <- element_blank()}
    else{Xlab.opt <- element_text(hjust = Xjust); Ylab.opt <- element_text()}
    
    CP <- ggcorrplot(Mcor, hc.order = Order.hclust, type = Display, title = Title, colors = c("royalblue", "white", "darkorange"),
                     tl.col="black", tl.srt = Xlab.rot, tl.cex = 10,
                     method = Disp.R.gg,
                     p.mat = PV,
                     sig.level = (1-I.confiance), insig = Display.pval, pch.cex = 5,
                     lab = TRUE)+
      Bold.case+ Bold.tiles.2 + Bold.tiles +
      scale_x_discrete(expand = c(0,0))+
      scale_y_discrete(expand = c(0,0))+
      theme(legend.position = Bar.pos.gg, panel.border = element_rect(NA, "black"),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = Xlab.opt,
            axis.text.y = Ylab.opt,
            plot.margin=unit(c(0,0,0,0),"cm"), plot.background = element_blank(), panel.background = element_rect(fill = NA, colour = "black"))
    
    print(CP)
  }
  
  #### Average values ####
  if(Average == T){
    type.correl <- gsub(".*ACA", "", Title)
    type.correl <- gsub("\\s.*", "", type.correl)
    
    MeanR2 <- round((Mcor)^2, digits = 2)
    MeanR2 <- cbind(MeanR2, MeansCol = rowMeans(MeanR2, na.rm = T))
    MeanR2 <- rbind(MeanR2, MeansRow = colMeans(MeanR2, na.rm = T))
    
    if(is.null(PV) == F & Print.result == T){print(PV)}
    
    R.mean.ACAV.t.chel <- round(mean(MeanR2["MeansRow", grepl("_chel", colnames(MeanR2))], na.rm = T), digits = 2)
    R.mean.ACAV.t.wc <- round(mean(MeanR2["MeansRow", grepl("_wc", colnames(MeanR2))]), digits = 2)
    R.mean.ACAV.gl.clim <- round(mean(MeanR2[grepl("_gf", rownames(MeanR2)), "MeansCol"]), digits = 2)
    R.mean.ACAV.clim <- round(mean(MeanR2[!grepl("_gf", rownames(MeanR2)), "MeansCol"]), digits = 2)
    
    if(Print.result == T){
      print(paste("R² mean ACA-", type.correl, " vs. Climat-CHELSA:", R.mean.ACAV.t.chel, sep = ""))
      print(paste("R² mean ACA-", type.correl, " vs. Climat-WORLDCLIM:", R.mean.ACAV.t.wc, sep = ""))
      print(paste("R² mean ACA-", type.correl, "-NORMAL vs. Climat :", R.mean.ACAV.clim, sep = ""))
      print(paste("R² mean ACA-", type.correl, "-GAPFILLING vs. Climat :", R.mean.ACAV.gl.clim, sep = ""))
    }
  }
  
  #### Export datas ####
  if(is.null(PV) == F){Mfull <- list(R2 = round(Mcor, digits = 2), Var = round(Mvar, digits = 2),p.val = round(PV, digits = 5))}
  else{Mfull <- list(R2 = round(Mcor, digits = 2), Var = round(Mvar, digits = 2))}
  
  if(Return.slope == T){Mfull <- append(Mfull, list("Slope" = slope_mat))}
  
  if(is.null(Save.path) == F){
    Path.to.create <- gsub("(.*/).*\\.csv.*","\\1", Save.path)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    write.table(Mfull, file = Save.path, row.names=T, col.names=NA, sep=",", dec = ".")}
  
  #### Return ####
  if(is.null(Save.plot) == F){dev.off()}
  if(return.pick == T){return(CP)}
  else{return(Mfull)}
}

Matcor.by.core.litho <- function(XRF, GDGT, Pollen, MMS, Plot.x, Cluster = NULL, H = 500, W = 500, Display.info = F, return.plot = F, Title = NULL, 
                                 Display = "full", Display.pval = "pch", Bar.pos = "b", ggplot.version = T, Permutation.test = F, Nb.permutations = 1000,
                                 Use.cor = "pairwise.complete.obs", Xlab.rot = 45, I.confiance = NULL, Disp.R = "number", Order.hclust = F, No.axis.lab = F,
                                 Proxy2proxy = F, Manual.col.grad = NULL, Seuil.min = 8, Show.inner = F, Select.interv = 500, Good.tiles = NULL, Bad.tiles = NULL,
                                 Segmt.width = 2, Limites = NULL, Gaussian.filtering = F, Accumulation.Rate = NULL, Save.path = NULL, Save.plot = NULL){
  #### Settings ####
  if(missing(Plot.x)){Plot.x = "MCD"}
  if(missing(GDGT)){GDGT = NULL}
  if(missing(MMS)){MMS = NULL}
  if(missing(XRF)){XRF = NULL}
  if(missing(Pollen)){Pollen = NULL}
  library(dplyr)
  library("corit")
  library("zoo")
  
  #### Accumulation rate cleaning ####
  if(is.null(Accumulation.Rate) == F){
    Accumulation.Rate <- Accumulation.Rate[names(Accumulation.Rate) %in% c("ages", "mean")]
    Accumulation.Rate <- Accumulation.Rate[!is.na(Accumulation.Rate$mean),]
    Accumulation.Rate <- round(Accumulation.Rate, digits = 0)
    Accumulation.Rate$Tw.min <- NA
    Accumulation.Rate$Tw.max <- NA
    
    Accumulation.Rate$Tw.min[1] <- Accumulation.Rate$ages[1]
    Accumulation.Rate$Tw.max[1] <- Accumulation.Rate$ages[1] + Accumulation.Rate$mean[1]
    for(i in 1:(nrow(Accumulation.Rate)-1)){
      if(Accumulation.Rate$mean[i] < (Accumulation.Rate$ages[i+1] - Accumulation.Rate$ages[i])){
        Accumulation.Rate$mean[i] <- (Accumulation.Rate$ages[i+1] - Accumulation.Rate$ages[i])
      }
    }
    
    for(i in 2:(nrow(Accumulation.Rate))){
      Accumulation.Rate$Tw.min[i] <- Accumulation.Rate$Tw.max[i-1]
      
      if(Accumulation.Rate$ages[i] < Accumulation.Rate$Tw.max[i-1]){
        Accumulation.Rate$Tw.max[i] <- Accumulation.Rate$Tw.max[i-1]
      }
      else{
        Accumulation.Rate$Tw.max[i] <- Accumulation.Rate$Tw.min[i] + Accumulation.Rate$mean[i]  
      }
    }
  }
  
  #### Cleaning data ####
  Remove.abiot.full <- c("Age", "Bottom", "Top", "MCD", "Depth", "Litho", "Zone")
  Remove.abiot <- setdiff(Remove.abiot.full, Plot.x)
  G1 <- NULL; G2 <- NULL; G3 <- NULL; G4 <- NULL
  
  #### Merging data ####
  if(is.null(XRF) == F){
    XRF <- XRF[setdiff(names(XRF), Remove.abiot)]
    G1 <- setdiff(names(XRF), Remove.abiot.full)
    
    if(is.null(Accumulation.Rate) == F & Gaussian.filtering == F){
      print("**** Timewindows size calculated based on the Accumulation Rate ! ****")
      XRF  <-  mutate(XRF, TimeWind = cut(XRF[[Plot.x]], breaks = unique(Accumulation.Rate$Tw.min)))
      Tw.min <- gsub(",.*", "", XRF$TimeWind)
      Tw.min <- as.numeric(gsub("\\(", "", Tw.min))
      XRF$Tw.min <- Tw.min
      XRF <- aggregate(XRF, list(XRF$TimeWind), FUN = mean, na.action = na.pass, na.rm = T)
      XRF <- XRF[setdiff(names(XRF), c("Age","TimeWind"))]
      names(XRF)[names(XRF) == "Group.1"] <- "Age"
    }
    M <- XRF
    
    if(is.null(GDGT) == F){
      GDGT <- GDGT[setdiff(names(GDGT), Remove.abiot)]
      G2 <- setdiff(names(GDGT), Remove.abiot.full)
      
      if(is.null(Accumulation.Rate) == F & Gaussian.filtering == F){
        GDGT  <-  dplyr::mutate(GDGT, TimeWind = cut(GDGT[[Plot.x]], breaks = unique(Accumulation.Rate$Tw.min)))
        Tw.min <- gsub(",.*", "", GDGT$TimeWind)
        Tw.min <- as.numeric(gsub("\\(", "", Tw.min))
        GDGT$Tw.min <- Tw.min
        GDGT <- aggregate(GDGT, list(GDGT$TimeWind), FUN = mean, na.action = na.pass, na.rm = T)
        GDGT <- GDGT[setdiff(names(GDGT), c("Age","TimeWind"))]
        names(GDGT)[names(GDGT) == "Group.1"] <- "Age"}
      
      
      if(is.null(M) == F){M <- full_join(M, GDGT, by = intersect(names(GDGT), names(M)))}
      else{M <- GDGT}
    }
    if(is.null(MMS) == F){
      MMS <- MMS[setdiff(names(MMS), Remove.abiot)]
      G3 <- setdiff(names(MMS), Remove.abiot.full)
      
      if(is.null(Accumulation.Rate) == F & Gaussian.filtering == F){
        MMS  <-  dplyr::mutate(MMS, TimeWind = cut(MMS[[Plot.x]], breaks = unique(Accumulation.Rate$Tw.min)))
        Tw.min <- gsub(",.*", "", MMS$TimeWind)
        Tw.min <- as.numeric(gsub("\\(", "", Tw.min))
        MMS$Tw.min <- Tw.min
        MMS <- aggregate(MMS, list(MMS$TimeWind), FUN = mean, na.action = na.pass, na.rm = T)
        MMS <- MMS[setdiff(names(MMS), c("Age","TimeWind"))]
        names(MMS)[names(MMS) == "Group.1"] <- "Age"}
      
      M <- full_join(M, MMS, by = intersect(names(MMS), names(M)))}
    if(is.null(Pollen) == F){
      Pollen <- Pollen[setdiff(names(Pollen), Remove.abiot)]
      G4 <- setdiff(names(Pollen), Remove.abiot.full)
      
      if(is.null(Accumulation.Rate) == F & Gaussian.filtering == F){
        Pollen  <-  mutate(Pollen, TimeWind = cut(Pollen[[Plot.x]], breaks = unique(Accumulation.Rate$Tw.min)))
        Tw.min <- gsub(",.*", "", Pollen$TimeWind)
        Tw.min <- as.numeric(gsub("\\(", "", Tw.min))
        Pollen$Tw.min <- Tw.min
        Pollen <- aggregate(Pollen, list(Pollen$TimeWind), FUN = mean, na.action = na.pass, na.rm = T)
        Pollen <- Pollen[setdiff(names(Pollen), c("Age","TimeWind"))]
        names(Pollen)[names(Pollen) == "Group.1"] <- "Age"}
      
      M <- full_join(M, Pollen, by = intersect(names(Pollen), names(M)))}
  }
  
  if(is.null(Accumulation.Rate) == F & Gaussian.filtering == F){M <- M[setdiff(names(M), Plot.x)]}
  
  M.tot <- M[sapply(M, is.numeric)]
  M.tot <- M.tot[setdiff(names(M.tot), Plot.x)]
  
  #### Cor plot total (Gaussian Filtering) ####
  if(Gaussian.filtering == T){
    #### Calculation ####
    Nb.param <- setdiff(names(M), Plot.x)
    All.relations <- data.frame(combn(Nb.param, 2, simplify = T))
    Mtot.plot <- data.frame(xName = as.vector(unlist(All.relations[1,])), 
                            yName = as.vector(unlist(All.relations[2,])), 
                            x = NA, y = NA, corr = NA)
    
    for(i in 1:ncol(All.relations)){
      P1 <- zoo(c(M[All.relations[1,i]])[[1]], order.by = c(M[Plot.x])[[1]])
      P2 <- zoo(c(M[All.relations[2,i]])[[1]], order.by = c(M[Plot.x])[[1]])
      
      Cor <- CorIrregTimser(
        timser1 = P1,
        timser2 = P2,
        detr = F,   #remove linear trend time series
        method = "InterpolationMethod",
        appliedFilter = "gauss",
        fc = 1/200, #cut-off frequency
        dt = 100,    #time step for the interpolation
        int.method = "linear",  #kind of interpolation
        filt.output = T)    #return filtered time series
      
      Mtot.plot[i, 5] <- round(Cor$cor, digits = 2)
    }
    
    #### Display info ####
    if(Display.info == T){
      Mtot.displot <- Mtot.plot
      
      if(Show.inner == T){
        if(is.null(G1) == F){Mtot.displot <- Mtot.displot[!(Mtot.displot$xName %in% G1 & Mtot.displot$yName %in% G1),]}
        if(is.null(G2) == F){Mtot.displot <- Mtot.displot[!(Mtot.displot$xName %in% G2 & Mtot.displot$yName %in% G2),]}
        if(is.null(G3) == F){Mtot.displot <- Mtot.displot[!(Mtot.displot$xName %in% G3 & Mtot.displot$yName %in% G3),]}
        if(is.null(G4) == F){Mtot.displot <- Mtot.displot[!(Mtot.displot$xName %in% G4 & Mtot.displot$yName %in% G4),]}
        
      }
      Mtot.displot <- Mtot.displot[c(1,2,5)]
      Mtot.displot$R2 <- round(Mtot.displot$corr^2, digits = 2)
      print("**** Result full correlation Matrix. ****")
      print(Mtot.displot)}
    
    #### Plots (ggplot) ####
    library(ggcorrplot)
    Mcor.gg <- Mtot.plot[c("xName", "yName", "corr")]
    Mcor.gg2 <- Mcor.gg[c("yName", "xName", "corr")]
    names(Mcor.gg2) <- names(Mcor.gg)
    Mcor.gg <- rbind(Mcor.gg2, Mcor.gg)
    Mcor.gg <- dcast(Mcor.gg, yName ~ xName, value.var = "corr")
    Mcor.gg[is.na(Mcor.gg)] <- 1
    row.names(Mcor.gg) <- Mcor.gg$yName
    Mcor.gg <- subset(Mcor.gg, select = - c(yName))
    
    if(is.null(Title) == T){Title <- ""}
    
    CP <- ggcorrplot(Mcor.gg, hc.order = TRUE, type = Display, title = Title, colors = c("royalblue", "white", "darkorange"),
                     tl.col="black", tl.srt=45, tl.cex = 10,
                     insig = Display.pval, #pch.cex = 2,
                     lab = TRUE)+
      theme(legend.position = Bar.pos, panel.border = element_rect(NA, "black", linewidth = 1),
            panel.grid = element_line(linetype = "dashed"),
            plot.margin=unit(c(0,0,0,0),"cm"), plot.background = element_blank(), panel.background = element_blank())
    
    #### Save plot ####
    if(is.null(Save.plot) == F){
      if(is.null(W) == F & is.null(H) == F){ggsave(CP, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
      else{ggsave(Save.plot)}}
    
  }
  
  #### Cor plot total (Matcor) ####
  if(Gaussian.filtering == F){
    if(is.null(Title) == T && ggplot.version == T){Title <- ""}
    
    if(Proxy2proxy == F){
      M1 <- M.tot[names(M.tot) %in% names(XRF)]
      M2 <- M.tot[names(M.tot) %in% names(GDGT)]
    }
    else{M1 <- M.tot; M2 <- M.tot}
    
    print(Save.path)
    
    Mtot.plot <- Mat.corel.CWT.clim(M1, M2,
                                    I.confiance = I.confiance, Display = Display,
                                    Bar.pos = Bar.pos, No.axis.lab = No.axis.lab,
                                    Use.cor = Use.cor,
                                    Display.pval = Display.pval, Disp.R = Disp.R, Title = Title, 
                                    return.pick = T, ggplot.version = ggplot.version,
                                    Xlab.rot = Xlab.rot, Order.hclust = Order.hclust,
                                    Permutation.test = Permutation.test, Nb.permutations = Nb.permutations,
                                    Good.tiles = Good.tiles, Bad.tiles = Bad.tiles,
                                    Save.path = Save.path, Average = F, 
                                    Save.plot = Save.plot,
                                    H = H, W = W)
    CP <- Mtot.plot
    Mtot.plot <- Mtot.plot$corrPos
  }
  
  #### Cor plot by units ####
  if(is.null(Cluster) == F){
    #### Mise-en-place des units ####
    Name.Units <- names(Cluster)
    if(is.null(Accumulation.Rate) == F & Gaussian.filtering == F){Clu.cor <- "Tw.min"}
    else{Clu.cor <- Plot.x}
    Units.L <- c()
    for(i in 1:ncol(Cluster)){Units.L[which(M[[Clu.cor]] >= Cluster[1,i] & M[[Clu.cor]] <= Cluster[2,i])] <- names(Cluster)[i]}
    Units.L[which(is.na(Units.L))] <- Units.L[which(is.na(Units.L))+1]
    R.change <- Mtot.plot
    R.change$corr <- round(R.change$corr, digits = 2)
    names(R.change)[names(R.change) == "corr"] <- "r_mn"
    
    #### Boucle pour chaque mat.cor par units ####
    for(i in 1:length(unique(Units.L))){
      if(Display.info == T){print(paste("** Mat. cor. pour L", Name.Units[i], " **", sep = ""))}
      Units.L.i <- which(Units.L == Name.Units[i])
      M.i <- M[Units.L.i,]
      Save.plot.i <- gsub("\\.pdf", paste("_", i, "\\.pdf", sep = ""), Save.plot)
      Save.path.i <- gsub("\\.csv", paste("_", i, "\\.csv", sep = ""), Save.path)
      Temporal.res <- round((max(M.i[Clu.cor], na.rm = T) - min(M.i[Clu.cor], na.rm = T))/10, digits = -1)
      if(Temporal.res <= 10){Temporal.res = 10}
      
      #### Version corplot ####
      if(Gaussian.filtering == F){
        Mtot.plot.i <- Mat.corel.CWT.clim(M.i, M.i,
                                          # I.confiance = 0.95, #Display = "upper", 
                                          Display.pval = "pch", Disp.R = "number", Title = paste("Cor. Mat. units: L", i, sep = ""), return.pick = T, ggplot.version = F,
                                          Save.path = Save.path.i, Average = F,
                                          Save.plot = Save.plot.i,
                                          H = H, W = W)
        
        Mtot.plot.i$corrPos$corr <- round(Mtot.plot.i$corrPos$corr, digits = 2)
        names(Mtot.plot.i$corrPos)[names(Mtot.plot.i$corrPos) == "corr"] <- paste("L", i, sep = "")
        R.change <- left_join(R.change, Mtot.plot.i$corrPos, by = join_by(xName, yName, x, y))
      }
      #### Version Gaussian  ####
      if(Gaussian.filtering == T){
        Nb.param <- setdiff(names(M.i), Plot.x)
        All.relations <- data.frame(combn(Nb.param, 2, simplify = T))
        
        Mtot.plot.i <- data.frame(xName = as.vector(unlist(All.relations[1,])), 
                                  yName = as.vector(unlist(All.relations[2,])), 
                                  x = NA, y = NA, corr = NA)
        
        for(j in 1:ncol(All.relations)){
          P1 <- zoo(c(M.i[All.relations[1,j]])[[1]], order.by = c(M.i[Plot.x])[[1]])
          P2 <- zoo(c(M.i[All.relations[2,j]])[[1]], order.by = c(M.i[Plot.x])[[1]])
          
          if(Display.info == T){print(paste(All.relations[2,j], "vs.", All.relations[1,j]))}
          
          if(length(P1) < Seuil.min){next}
          
          Cor.i <- CorIrregTimser(
            timser1 = P1,
            timser2 = P2,
            detr = F,   #remove linear trend time series
            method = "InterpolationMethod",
            appliedFilter = "gauss",
            fc = 1/Temporal.res, #cut-off frequency
            dt = Temporal.res,    #time step for the interpolation
            int.method = "linear",  #kind of interpolation
            filt.output = T)    #return filtered time series
          Mtot.plot.i[j, 5] <- round(Cor.i$cor, digits = 2)
          
          names(Mtot.plot.i)[names(Mtot.plot.i) == "corr"] <- Name.Units[i]
        }
        R.change <- left_join(R.change, Mtot.plot.i, by = join_by(xName, yName, x, y))
      }
    }
    if(Display.info == T){print(R.change)}
    
    #### Clean matrice totale ####
    R.change <- R.change[R.change$xName != R.change$yName,]
    R.change <- R.change[R.change$xName != Clu.cor,]
    R.change <- R.change[R.change$yName != Clu.cor,]
    R.change$Lab <- paste(R.change$xName, "/", R.change$yName)
    
    R.change <- R.change[setdiff(names(R.change), c("xName", "yName", "x", "y"))]
    R.change <- R.change[c(ncol(R.change), 1:(ncol(R.change)-1))]
    
    R.change <- R.change[!duplicated(R.change[c(2:ncol(R.change))]),]
    R.change[R.change == 1] <- NA
    R.change[R.change == -1] <- NA
    
    #### Strat plot ####
    R.change.plot <- R.change
    Save.plot.strat <- gsub("\\.pdf", "_strat\\.pdf", Save.plot)
    
    if(Proxy2proxy == T){
      R.change.plot$Param1 <- gsub(".*/", "", R.change.plot$Lab)
      R.change.plot$Param1 <- gsub("_.*", "", R.change.plot$Param1)
      R.change.plot$Param1 <- gsub(" ", "", R.change.plot$Param1)
      R.change.plot$Param2 <- gsub("/.*", "", R.change.plot$Lab)
      R.change.plot$Param2 <- gsub("_.*", "", R.change.plot$Param2)
      R.change.plot <- R.change.plot[which(R.change.plot$Param1 == R.change.plot$Param2),]
      R.change.plot <- subset(R.change.plot, select = -c(Param1, Param2))
    }
    
    R.change.plot <- melt(R.change.plot, c("Lab","r_mn"))
    R.change.plot$Ymin <- unlist(Cluster[1,])[match(R.change.plot$variable, names(Cluster))]
    R.change.plot$Ymax <- unlist(Cluster[2,])[match(R.change.plot$variable, names(Cluster))]
    
    if(is.null(Limites)){Limites <- c(min(R.change.plot$Ymin, na.rm = T), max(R.change.plot$Ymax, na.rm = T))}
    
    pstrat <- ggplot(data = R.change.plot, aes(y = 0, x = Ymin, color = value))+
      geom_segment(aes(yend = 0, xend = Ymax), linewidth = Segmt.width)+#ylim(-1,1)+ 
      scale_x_continuous(breaks = round(seq(0, Limites[2], by = Select.interv)), expand = c(0,0))+
      scale_color_gradientn(colors = c("darkblue", "#82b9edff", "#82b9edff", 'white', "#f1b05fff","#f1b05fff", "#B83426"),
                            values = Manual.col.grad,
                            na.value = "white", name = "r-value")+
      facet_wrap(vars(Lab), ncol = 1, strip.position = "left")+
      theme_void() + theme(strip.text = element_text(hjust = 1), 
                           axis.line.x = element_line(), axis.ticks.x = element_line(),
                           axis.ticks.length.x = unit(2, "mm"),
                           axis.text.x = element_text())
    
    ggsave(filename = Save.plot.strat, pstrat, width = W*0.026458333, height = H*0.026458333, units = "cm")
    
    if(return.plot == T){return(Mtot.plot)}
    else{return(R.change)}
  }
  
  if(return.plot == T){return(CP)}
  
}

#### Plots Functions ####
PCA.bioclim <- function(MP, transp_OK, Site.name, Type.samples, Ellipse, Shape = NULL, Show.centroid, Show.arrow, Show.site.lab = F,
                        Csv.sep, Scale.PCA, Groupes, Cluster.core, Cluster.core.lab, return.pick, Contrib, Size.MPS = 3, Alpha.MPS = 0.5,
                        Save.path, Manu.lim.x, Manu.lim.y, Dot.size, Dot.opac, Ellipse.opa, Density.contour, Doz.size.leg = NULL,
                        Opa.range, Reverse.dim, Show.annot, Show.Plotly, PCA.site, Marg.density.plot, Leg.nrow = NULL, GDGT = F,
                        Helinger.trans = F, Variable.inactive = NULL, No.arrow = F, Hide.MPS = F, Display.only.MPS = F,
                        VIF = F, VIF.seuil = NULL, Display.VIF = F, Manual.color.scale = NULL, return.PCA.variable = F,
                        Symbol.path = NULL, Symbol.pos = NULL, Legend.position, Density.type, Save.plot, H, W, Num.facet, Legend.size){
  #### Settings ####
  library(vegan)
  library(ggrepel)
  library("FactoMineR") # FactoMineR pour la PCA (Le et al. 2008)
  library("factoextra")
  if(missing(Csv.sep)){Csv.sep = "\t"}
  if(missing(Site.name)){Site.name = "Site1"}
  if(missing(Cluster.core)){Cluster.core = NULL}
  if(missing(Num.facet)){Num.facet = NULL}
  if(missing(Ellipse.opa)){Ellipse.opa = 0.4}
  if(missing(Cluster.core.lab)){Cluster.core.lab = "Biome (Dinerstein et al., 2017)"}
  if(missing(PCA.site)){PCA.site = F}
  if(missing(Marg.density.plot)){Marg.density.plot = F}
  if(missing(Reverse.dim)){Reverse.dim = F}
  if(missing(Show.arrow)){Show.arrow = T}
  if(missing(Show.annot)){Show.annot = T}
  if(missing(Ellipse)){Ellipse = F}
  if(missing(Contrib)){Contrib = T}
  if(missing(return.pick)){return.pick = F}
  if(missing(Show.centroid)){Show.centroid = F}
  if(missing(transp_OK)){transp_OK = T}
  if(missing(Legend.position)){Legend.position = "right"}
  if(Show.centroid == F){Centroide = "quali"}
  if(Show.centroid == T){Centroide = NULL}
  if(missing(Legend.size)){Legend.size = 1}
  if(missing(Scale.PCA)){Scale.PCA = 1}
  if(missing(Save.path)){Save.path = NULL}
  if(missing(Groupes)){Groupes = NULL}
  if(missing(Manu.lim.x)){Manu.lim.x = NULL}
  if(missing(Manu.lim.y)){Manu.lim.y = NULL}
  if(missing(Type.samples)){Type.samples = NULL}
  if(missing(Show.Plotly)){Show.Plotly = F}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Density.contour)){Density.contour = F}
  if(missing(Density.type)){Density.type = "polygon"}
  if(missing(Dot.size)){Dot.size = NULL}
  if(missing(Dot.opac)){Dot.opac = NULL}
  if(missing(Opa.range)){Opa.range = c(0.01,0.1)}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Data pulishing ####
  print("Lets PCA bioclimatic !")
  Remove.name <- c("Biome", "Biome.no", "Ecosystem", "Bioclim", "Latitude", "Longitude", "Cluster", "Age",
                   "Aridity", "Aridity2", "Type", "AP_NAP", "PFT", "GrowthForm", "Country")
  if(is.null(Groupes) == F){Remove.name <- setdiff(Remove.name, unlist(Groupes))}
  Keep.xdata <- MP[intersect(names(MP), Remove.name)]
  MP <- MP[setdiff(names(MP), Remove.name)]
  names(MP) <- gsub("TRY_", "", names(MP))
  Save.names <- names(MP)
  
  #### Pour les GDGTs ####
  if(GDGT == T){
    # if(Remove.7Me == T){MP <- MP[setdiff(names(MP), names(MP)[grepl("7Me", names(MP))]),]}
    names(MP) <- gsub("f.", "", names(MP))
    names(MP) <- gsub("_5Me", "", names(MP))
    names(MP) <- gsub("_6Me", "\\'", names(MP))
    names(MP) <- gsub("_7Me", "\\''", names(MP))
  }
  
  #### Groupes ####
  if(is.null(Groupes) == F){
    Groupes <- melt(Groupes)
    Groupes <- Groupes$L1[match(names(MP), Groupes$value)]
    Groupes[is.na(Groupes)] <- "Unknown"
    Groupes <- factor(Groupes)
  }
  
  #### Transforming the data + PCA calcul ####
  if(nlevels(as.factor(is.na(MP))) >= 2){
    library(missMDA)
    nb <- estim_ncpPCA(MP, ncp.max = 5) ## Time consuming, nb = 2
    MP.comp <- imputePCA(MP, ncp = nb[[1]])
    MP <- MP.comp$completeObs
  }
  
  Sites.to.rm <- names(which(sapply(MP, function(x)all(is.na(x)))))
  if(length(Sites.to.rm) > 0){MP <- MP[!names(MP) %in% Sites.to.rm]}
  
  if(Helinger.trans == T){MP <- vegan::decostand(MP, method = "hellinger")}
  
  if(is.null(Variable.inactive) == F){
    Variable.active <- which(!colnames(MP) %in% Variable.inactive)
    Variable.inactive <- which(colnames(MP) %in% Variable.inactive)
    # MP.pca <- PCA(MP[,Variable.active], quanti.sup = Variable.inactive, graph = FALSE, scale.unit = transp_OK)
    MP.pca <- PCA(MP, quanti.sup = Variable.inactive, graph = FALSE, scale.unit = transp_OK)
  }
  else{MP.pca <- PCA(MP, graph = FALSE, scale.unit = transp_OK)}
  
  PCA <- data.frame(MP.pca$ind$coord)
  PCA <- PCA[,1:2]
  names(PCA) <- c("PC1", "PC2")
  
  if(is.null(Keep.xdata$Type) == F){
    PCA <- cbind(PCA, Type = Keep.xdata$Type, Biome = Keep.xdata[[Cluster.core]])
    PCA <- PCA[PCA$Type == "MPS",]
    if(is.null(Shape) == T){Shape <- 21}
    
    if(Display.only.MPS == F){
      Point.surf <- geom_point(inherit.aes = F, PCA, mapping = aes(x = PC1, y = PC2, fill = Biome), shape = Shape, colour = "grey10", alpha = Alpha.MPS, na.rm = T, size = Size.MPS)
    }
    else{
      Point.surf <- geom_point(inherit.aes = F, PCA, mapping = aes(x = PC1, y = PC2, color = Biome), shape = Shape, alpha = Alpha.MPS, na.rm = T, size = Size.MPS)
    }
    # Point.surf <- geom_point(inherit.aes = F, PCA, mapping = aes(x = PC1, y = PC2, fill = Biome), shape = Shape, colour = "grey10", alpha = Alpha.MPS, na.rm = T, size = 0.1)
    Opac <- 0.3
    Ptaille <- 2
  }
  else{
    Point.surf <- NULL
    Opac <- 0.7
    Ptaille <- 1.5 
  }
  if(is.null(Dot.size) == F){Ptaille <- Dot.size}
  if(is.null(Dot.opac) == F){Opac <- Dot.opac}
  
  #### Color settings ####
  if(is.null(Groupes) == F){
    my_orange = c("Water" = "darkblue",
                  "Altitude" = "brown",
                  "Temperature" = "darkred")
    
    Scale.color.vec <- scale_color_manual(values = my_orange, name = "Proxies")
    Show.leg.arrow = T
    
  }
  else{
    Groupes <- "Unique"
    Show.leg.arrow = F
    my_orange <- data.frame(Unique = "royalblue")
    Scale.color.vec <- scale_color_manual(values = my_orange, guide = "none")
  }
  
  if(is.null(Cluster.core) == F){
    if(is.numeric(Keep.xdata[[Cluster.core]]) == T){
      my_orange2 = brewer.pal(n = 11, "RdYlBu")[Keep.col2[-c(3,4,5,7,8,9)]] 
      orange_palette2 = colorRampPalette(my_orange2)
      my_orange2 = rev(orange_palette2(length(seq(min(Keep.xdata[[Cluster.core]]), max(Keep.xdata[[Cluster.core]]), by = 200))))
      Scale.fill <- scale_fill_gradientn(colours = my_orange2, guide = "colourbar", 
                                         name = Cluster.core.lab,
                                         breaks = seq(round(min(Keep.xdata[[Cluster.core]]),digits = -3), max(Keep.xdata[[Cluster.core]]), by = 1000),
                                         na.value = "white")}
    else{
      if(is.null(Manual.color.scale) == T){
        values.bi = c("Deserts & Xeric Shrublands" = "#C88282",
                      "Temperate Grasslands, Savannas & Shrublands" = "#ECED8A",
                      "Montane Grasslands & Shrublands" = "#D0C3A7",
                      "Temperate Conifer Forests" = "#6B9A88",
                      "Temperate Broadleaf & Mixed Forests" = "#3E8A70",
                      "N/A" = "#FFEAAF",
                      "Tundra" = "#A9D1C2",
                      "Boreal Forests/Taiga" = "#8FB8E6",
                      "Tropical & Subtropical Coniferous Forests" = "#99CA81",
                      "Mangroves" = "#FE01C4",
                      "Flooded Grasslands & Savannas" = "#BEE7FF",
                      "Tropical & Subtropical Moist Broadleaf Forests" = "#38A700",
                      "Plant_height" = "royalblue",
                      "Leaf_thickness" = "darkorange",
                      "Photosynthesis_pathway" = "purple",
                      "TUSDB sites" = "#323232",
                      "Woodyness" = "#323232",
                      "Leaf_size" = "darkred",
                      "Variable" = "#aa373aff",
                      "Algal" = "#4666E9",
                      "NAP" = "#b5ab32ff",
                      "Herb" = "#b5ab32ff",
                      "Shrub" = "#aa373aff",
                      # "Other" = "grey90",
                      "Unknown" = "grey90",
                      "AP" = "#0f6b31ff",
                      "Tree" = "#0f6b31ff",
                      # "Hg-Rich./Herb." = "#c67f05", "Hg-Rich." = "#004266", "Hg-Even" = "#75AADB", "Other" = "grey40", "Herb." = "#E76D51",
                      "Hg-Rich./Herb." = "goldenrod1", "Hg-Rich." = "royalblue", "Hg-Even" = "#54a697", "Other" = "grey40", "Herb." = "firebrick3",
                      "1_Hyper-arid" = "#8c510a", "2_Arid" = "#bf812e", "3_Semi-arid" = "#dfc27e", "4_Dry sub-humid" = "#f5e9bf", "5_Humid" = "#80cec1",
                      "1. Hyper-arid" = "#8c510a", "2. Arid" = "#bf812e", "3. Semi-arid" = "#dfc27e", "4. Dry sub-humid" = "#f5e9bf", "5. Humid" = "#80cec1",
                      Mongolia = "#3e96bdff", Chine = "#f02a26", Uzbekistan = "#6fb440", Armenia = "#54a697", "China, Tibet" = "#8c510a", "Northern Iran" = "#176E5B",
                      Tajikistan = "#e4af08", Russia = "#0035a9", Azerbaijan = "#094227", China = "#bb0202", "ACA lakes" =  "purple",
                      "Ch'ol cold desert-steppes" = "#7916C4", 
                      "Tugai riparian forest" = "#BB0268", 
                      "Ch'ol warm deserts" = "#bb0202", 
                      "Adyr desert-steppes" = "#ff5400", 
                      "Adyr steppes" = "#e6c607", 
                      "Tau riparian forest" = "#2C9740", 
                      "Tau thermophilous woodlands" = "#85682D", 
                      "Tau juniper steppe-forest" = "#176E5B",
                      "Tau steppes" = "#bab133",
                      "Alau cryophilous steppe-forest" = "#54a697",
                      "Alau meadows" = "#197CDA",
                      "Tugai riparian forests" = "#7916C4", 
                      "Ch'ol deserts" = "#bb0202", 
                      "Adyr pseudosteppes" = "#ff5400", 
                      "Adyr steppes" = "#ECED8A", 
                      "Tau xeric shrublands" = "#85682D", 
                      "Tau open woodlands" = "#1e8736",
                      "Tau steppes" = "#b0a62e",
                      "Alau cryophilous open woodlands" = "#54a697",
                      "Alau mesic grasslands" = "#197CDA" 
        )
      }
      else{values.bi <- Manual.color.scale}
      
      if(Display.only.MPS == F){values.bi <- values.bi[which(names(values.bi) %in% unique(Keep.xdata[[Cluster.core]]))]}
      
      
      Scale.fill <- scale_fill_manual(values = values.bi, name = Cluster.core.lab, drop = T)
      Scale.color <- scale_color_manual(values = values.bi, name = Cluster.core.lab, drop = T, guide = "none")
    }
    Col.select <- Keep.xdata[[Cluster.core]]
    Fill.select <- Keep.xdata[[Cluster.core]]
  }
  else{
    Scale.fill <- scale_fill_manual(values = "brown", name = "Species density")
    Scale.color <- scale_color_manual(values = "brown", name = "Species density")
    Opac = 0.2
    Col.select = "brown"
    Fill.select = "brown"
  }
  
  if(Marg.density.plot == F){
    My_title <- paste(Num.facet, Site.name, Type.samples, sep = " ")
  }
  else{My_title <- NULL}
  
  if(is.null(Leg.nrow) == F){
    Guide.color <- guides(fill = guide_legend(nrow = Leg.nrow), color = guide_legend(nrow = Leg.nrow))
  }
  else{Guide.color <- NULL}
  
  if(is.null(Doz.size.leg)){Doz.size.leg <- Ptaille+2}
  
  #### Density contours #### 
  if(Density.contour == T){
    Data.contour <- data.frame(MP.pca$ind$coord)
    Data.contour$Col.select <- Col.select
    Data.contour <- unique(Data.contour)
    if(Density.type == "contour"){
      Density.color.line <- "grey20"
      Scale.opa <- NULL
      Point.surf <- NULL
      Dot.up <- "point"
    }
    if(Density.type == "polygon"){
      Density.color.line <- NA
      Scale.opa <- scale_alpha_continuous(range = Opa.range)
      if(is.null(Shape) == T){Shape <- 16}
      Point.surf <- geom_point(inherit.aes = F, Data.contour, mapping = aes(x = Dim.1, y = Dim.2, colour = Col.select), shape = Shape, alpha = Opac, na.rm = T, size = Ptaille)
      Dot.up <- "none"
    }
    Density.contour <- stat_density_2d(data = Data.contour, mapping = aes(x = Dim.1, y = Dim.2, alpha = ..level..),
                                       geom = Density.type, colour = Density.color.line, show.legend = F,
                                       bins = 6)
    
  }
  else{
    Dot.up <- "point"
    Scale.opa <- NULL
    Density.contour <- NULL}
  
  if(Hide.MPS == T){Point.surf <- NULL}
  if(Display.only.MPS == T){
    Fill.select <- rep("Unknown", length(Fill.select))
    Col.select <- rep("Unknown", length(Col.select))
  }
  
  #### Labels ####
  if(Show.site.lab == T){
    PCA$Site <- row.names(PCA)
    Site.lab <- geom_text(inherit.aes = F, PCA, mapping = aes(x = PC1, y = PC2, label = Site), alpha = 0.5, na.rm = T, size = Size.MPS)
  }
  else{Site.lab <- NULL}
  
  
  #### Reverse dim ####
  A = round(MP.pca$eig[1,2], digits = 0)
  B = round(MP.pca$eig[2,2], digits = 0)
  
  if(Reverse.dim == T){
    if(Show.annot == T){
      Note.n <- annotate("text", x = min(min(MP.pca$ind$coord[,2]), min(MP.pca$var$coord[,2])), y = min(MP.pca$ind$coord[,1]), label = paste("n = ", nrow(MP), sep = ""), size = 5, hjust = 0)}
    else{Note.n <- NULL}
    Axes <- c(2,1)
    X.arrow <- c(2)
    Y.arrow <- c(1)
    Xlab <- labs(x = substitute(paste("PCA"[2], ~ "(", B, " %)", sep = " " )),
                 y = substitute(paste("PCA"[1], ~ "(", A, " %)", sep = " " )))
  }
  else{
    if(Show.annot == T){
      # Note.n <- annotate("text", x = min(min(MP.pca$ind$coord[,1]), min(MP.pca$var$coord[,1])),  y = min(min(MP.pca$ind$coord[,2]), min(MP.pca$var$coord[,2])), label = paste("n = ", nrow(MP), sep = ""), size = 5, hjust = 0, vjust = 0.5)}
      Note.n <- annotate("text", x = Inf, y = -Inf, label = paste("n = ", nrow(MP), sep = ""), size = 4, hjust = 1.1, vjust = -0.5)}
    else{Note.n <- NULL}
    
    Axes = c(1,2)
    X.arrow <- c(1)
    Y.arrow <- c(2)
    Xlab <- labs(x = substitute(paste("PCA"[1],~"(", A," %)", sep = " " )),
                 y = substitute(paste("PCA"[2],~"(", B," %)", sep = " " )))
  }
  if(is.null(Manu.lim.x)==F){
    Lim.x <- xlim(Manu.lim.x)
    if(Show.annot == T){Note.n <- annotate("text", x = Manu.lim.x[1], y = min(MP.pca$ind$coord[,2]), label = paste("n = ", nrow(MP), sep = ""), size = 5, hjust = 0, vjust = 0.5)}
    else{Note.n <- NULL}  
  }
  else{Lim.x <- NULL}
  if(is.null(Manu.lim.y)==F){Lim.y <- ylim(Manu.lim.y)}
  else{Lim.y <- NULL}
  
  #### Variables inactives ####
  if(is.null(Variable.inactive) == F){
    MP.pca$quanti.sup$contrib <- MP.pca$quanti.sup$coord
    MP.pca$quanti.sup$contrib[,] <- min(MP.pca$var$contrib[,X.arrow] + MP.pca$var$contrib[,Y.arrow])/2
    
    MP.pca$var$coord <- rbind(MP.pca$var$coord, MP.pca$quanti.sup$coord)
    MP.pca$var$cor <- rbind(MP.pca$var$cor, MP.pca$quanti.sup$cor)
    MP.pca$var$cos2 <- rbind(MP.pca$var$cos2, MP.pca$quanti.sup$cos2)
    MP.pca$var$contrib <- rbind(MP.pca$var$contrib, MP.pca$quanti.sup$contrib)
    
    MP.pca$var$coord <- MP.pca$var$coord[match(Save.names, row.names(MP.pca$var$coord)),]
    MP.pca$var$cor <- MP.pca$var$cor[match(Save.names, row.names(MP.pca$var$cor)),]
    MP.pca$var$cos2 <- MP.pca$var$cos2[match(Save.names, row.names(MP.pca$var$cos2)),]
    MP.pca$var$contrib <- MP.pca$var$contrib[match(Save.names, row.names(MP.pca$var$contrib)),]
  }
  
  #### Ajout vectors ####
  Pouet <- data.frame(X0 = 0,
                      Y0 = 0,
                      X1 = MP.pca$var$coord[,X.arrow]*Scale.PCA,
                      Y1 = MP.pca$var$coord[,Y.arrow]*Scale.PCA,
                      Lab = row.names(MP.pca$var$coord),
                      Contrib = MP.pca$var$contrib[,X.arrow] + MP.pca$var$contrib[,Y.arrow],
                      Groupes = Groupes)
  
  Pouet$Contrib <- Pouet$Contrib/sum(Pouet$Contrib)
  
  Arrow.lab <- geom_text_repel(data = Pouet, aes(x = X1, y = Y1, label = Lab, color = Groupes), min.segment.length = 10, force = 20, show.legend = F)
  if(Contrib == T & Show.arrow == T){
    Arrow <- geom_segment(data = Pouet, aes(x = X0, y = Y0, xend = X1, yend = Y1, color = Groupes, size = Contrib), arrow = arrow(length=unit(0.2,"cm")), show.legend = Show.leg.arrow)}
  if(Contrib == F & Show.arrow == T){
    Arrow <- geom_segment(data = Pouet, aes(x = X0, y = Y0, xend = X1, yend = Y1, color = Groupes), arrow = arrow(length=unit(0.2,"cm")), show.legend = Show.leg.arrow)}
  if(Show.arrow == F){
    Arrow <- NULL
    Arrow.lab <- NULL
  }
  Scale.size <- scale_size_continuous(range = c(0.2,1), guide = "none")
  
  if(No.arrow == T){Arrow.lab <- NULL; Arrow <- NULL}
  
  #### Ajout symbole ####
  if(is.null(Symbol.path) == F){
    if(is.null(Symbol.pos)== T){Symbol.pos <- c(.9, .9, .16)}
    if(grepl("\\.png", Symbol.path)){
      library(png)
      library(grid)
      img <- readPNG(Symbol.path)
      g <- rasterGrob(x = Symbol.pos[1], y = Symbol.pos[2], width = Symbol.pos[3], height = Symbol.pos[3], img, interpolate = T)
    }
    
    if(grepl("\\.xml", Symbol.path)){
      library(grImport)
      img <- readPicture(Symbol.path)
      g <- pictureGrob(x = Symbol.pos[1], y = Symbol.pos[2], width = Symbol.pos[3], height = Symbol.pos[3], img)
    }
    
    Logo <- annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  }
  else{Logo <- NULL}
  
  #### PLOT  ####
  p <- fviz_pca_biplot(MP.pca, na.rm = T,
                       geom.ind = Dot.up, axes = Axes,
                       pointshape = 16,
                       pointsize = Ptaille,
                       fill.ind = Fill.select,
                       alpha.ind = Opac, #alpha.var ="contrib",
                       col.ind = Col.select,
                       repel = T, arrowsize = 0.5,
                       invisible = Centroide, # enlève ou ajoute le centroïde
                       addEllipses = Ellipse, ellipse.level = 0.75, ellipse.type = "norm", # convex or norm
                       ellipse.alpha = Ellipse.opa, 
                       title = My_title, 
                       # col.var = Groupes #a enleve parfois
                       col.var = NA,
                       col.quanti.sup = NA
  ) +
    guides(color = guide_legend(override.aes = list(size = Doz.size.leg, alpha = 1), title.position = "top"))+
    Guide.color +
    Xlab+ #Ylab+ 
    Lim.x+ Lim.y+
    # Axes+
    Scale.opa + 
    Density.contour +
    Scale.fill +
    Scale.color +
    Point.surf +
    Site.lab +
    new_scale_color() + 
    Arrow + Arrow.lab + 
    Scale.size + Scale.color.vec +
    Note.n + Logo + 
    guides(fill = guide_legend(title.position = "top")) +
    guides(color = guide_legend(title.position = "top")) +
    theme(plot.background = element_blank(),
          legend.position = Legend.position,
          legend.title = element_text(size = (Legend.size+2)),
          legend.text = element_text(size = Legend.size),
          legend.key = element_blank(),
          plot.margin=unit(c(0,0,0,0),"cm"),
          panel.grid = element_blank(),
          panel.border = element_rect(NA, "black", linewidth = 1),
          # panel.grid = element_line(linetype = "dashed"),
          axis.line = element_blank())
  
  
  #### Add margin density ####
  if(Marg.density.plot == T){
    MMarg <- data.frame(MP.pca$ind$coord)
    MMarg <- cbind(MMarg, Biome = Col.select)
    
    x_limits <- ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
    y_limits <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
    if(is.null(Lim.x) == F){x_limits <- c(Lim.x$limits[1]-0.05*abs(min(Lim.x$limits) - max(Lim.x$limits)), Lim.x$limits[2]+0.05*abs(min(Lim.x$limits) - max(Lim.x$limits)))}
    if(is.null(Lim.y) == F){y_limits <- c(Lim.y$limits[1]-0.05*abs(min(Lim.y$limits) - max(Lim.y$limits)), Lim.y$limits[2]+0.05*abs(min(Lim.y$limits) - max(Lim.y$limits)))}
    
    List.of.NA <- which(MMarg$Dim.1 < 1e-12 & MMarg$Dim.1 > -1e-12 & MMarg$Dim.2 < 1e-12 & MMarg$Dim.2 > -1e-12)
    if(length(List.of.NA) > 0){
      print("Remove NA from density.")
      MMarg <- MMarg[-List.of.NA,]}
    
    #### Density plots up ####
    plot_top <- ggplot(MMarg, aes(x = Dim.1, fill = Biome)) + 
      geom_density(alpha = 0.6, size = 0.1) + Scale.fill + 
      ggtitle(paste(Num.facet, Site.name, Type.samples, sep = " "))+ 
      scale_x_continuous(limits = x_limits, expand = c(0,0))+
      #### Theme ####
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(), axis.text.y = element_text(hjust = 1, size = 6),
      axis.ticks.x.bottom = element_blank(),
      axis.title = element_blank(),
      axis.line.y = element_line(colour = "grey"),
      axis.ticks.y = element_line(colour = "grey"),
      #panel.border = element_rect(fill = NA, colour = "grey"),
      legend.title = element_text(),
      legend.key = element_blank(),
      legend.justification = c("center"),               # left, top, right, bottom
      legend.text = element_text(size = 8),
      panel.background = element_blank(),
      panel.spacing = unit(0.7, "lines"),
      legend.position = "none",
      strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
      strip.placement = "outside",
      # strip.background = element_rect(color = "white", fill = "white"),
      strip.background = element_blank(), panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"cm")
    )
    
    #### Density plots right ####
    plot_right <- ggplot(MMarg, aes(x = Dim.2, fill = Biome)) + 
      geom_density(alpha = 0.6, size = 0.1) + Scale.fill +
      scale_x_continuous(limits = y_limits, expand = c(0,0))+
      coord_flip() + 
      #### Theme ####
    theme(
      axis.line.y = element_blank(),
      axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      axis.line.x = element_line(colour = "grey"),
      axis.ticks.x = element_line(colour = "grey"),
      #panel.border = element_rect(fill = NA),
      legend.title = element_text(),
      legend.key = element_blank(),
      legend.justification = c("center"),               # left, top, right, bottom
      legend.position = "none",
      legend.text = element_text(size = 8),
      panel.background = element_blank(),
      panel.spacing = unit(0.7, "lines"),
      strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
      strip.placement = "outside",
      # strip.background = element_rect(color = "white", fill = "white"),
      strip.background = element_blank(), panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"cm")
    )
    
    
    #### Export ####
    layout <- "AAAAAAA#
             CCCCCCCB
             CCCCCCCB
             CCCCCCCB
             CCCCCCCB
             CCCCCCCB
             "
    p <- plot_top + plot_right + p + plot_layout(design = layout)
  }
  
  if(is.null(Save.plot) == F){print(p)}
    
  #### Save html ####
  if(Show.Plotly == T){
    library(plotly)
    library(htmlwidgets)
    
    #### 3D PCA chart ####
    Pouet <- data.frame(MP.pca$ind$coord)
    Pouet$Col <- Col.select
    fig <- plot_ly(Pouet, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, color = ~Col, 
                   colors = values.bi, text = ~paste('Site: ', row.names(Pouet), "\n Biome:", Pouet$Col, sep = "")) #%>%
    options(warn = - 1)
    # add_markers(size = 60)
    # 
    # fig <- fig %>%
    #   layout(
    #     # title = tit,
    #     xaxis = list(title = "PCA1"),
    #     scene = list(bgcolor = "#e5ecf6")
    #   )
    
    #### Export ####
    Save.plot.html <- gsub("pdf", "html", Save.plot)
    Keep.name <- gsub(".*\\/", "", Save.plot.html)
    Path.root <- paste(gsub(Keep.name, "", Save.plot.html), "HTML_files/", sep = "")
    if(file.exists(Path.root) == F){dir.create(Path.root)}
    Save.plot.html <- paste(Path.root, Keep.name, sep = "")
    # # p1_ly <- ggplotly(p)
    # # p1_ly <- p1_ly %>% layout()
    options(warn = - 1)
    saveWidget(fig, file = Save.plot.html)
    options(warn = - 1)
    # saveWidget(p1_ly, file = Save.plot.html)
  }
  
  #### Export data ####
  if(is.null(Save.path) == F){
    Site.name <- gsub(" ","_",Site.name)
    Save.path.Site <- gsub("\\.csv", "_PCA_site.csv", Save.path)
    Save.path.Taxon <- gsub("\\.csv", "_PCA_clim_param.csv", Save.path)
    write.table(data.frame(MP.pca$ind$coord), file = Save.path.Site, col.names = T, sep = ",")
    write.table(data.frame(MP.pca$var$coord), file = Save.path.Taxon, col.names = T, sep = ",")}
  if(is.null(Save.plot) == F){
    dev.off()}
  
  if(return.pick == T){return(p)}
  if(return.pick == F){
    if(PCA.site == T){return(data.frame(MP.pca$ind$coord))}
    if(return.PCA.variable == T){return(data.frame(MP.pca$var$coord))}
    if(Contrib == T){return(MP.pca$var$contrib)}
  }
}

Mantel.plot <- function(Keep.trait = NULL, Mtrait = NULL, Scaling = T, Npermutation = 999, Display.info = F, Mantel.test = T, 
                        Highlight.tiles = NULL,
                        Nb.voisin = 4, Save.plot = NULL, W = NULL, H = NULL, Moran.I = F, Moran.cross.I = F, Return.plot = T){
  #### Clean data ####
  names(Mtrait) <- gsub("TRY_", "", names(Mtrait))
  
  if(is.null(Keep.trait) == F){traits_df <- Mtrait[Keep.trait]}
  else{traits_df <- Mtrait[grep("^TRY_", names(Mtrait))]}
  
  Lat <- Mtrait[,grep("LAT|lat|Lat", names(Mtrait))]
  Long <- Mtrait[,grep("LONG|long|Long", names(Mtrait))]
  Mcoord <- cbind(Longitude = Long, Latitude = Lat)
  if(is.null(Mtrait$Site) == F){
    row.names(Mcoord) <- Mtrait$Site
    row.names(traits_df) <- Mtrait$Site
  }
  
  if(Scaling == T){traits_df <- scale(traits_df)}
  
  Mcoord <- data.frame(Mcoord[complete.cases(traits_df), ])
  traits_df <- traits_df[complete.cases(traits_df), ]
  
  #### Distance matrix compilation ####
  trait_dists <- lapply(1:ncol(traits_df), function(i) dist(traits_df[, i]))
  names(trait_dists) <- colnames(traits_df)
  trait_names <- colnames(traits_df)
  
  #### Mantel tests ####
  if(Mantel.test == T){
    results <- data.frame()
    
    for (i in 1:(length(trait_dists) - 1)) {
      for (j in (i + 1):length(trait_dists)) {
        mantel_res <- vegan::mantel(trait_dists[[i]], trait_dists[[j]], method = "pearson", permutations = Npermutation)
        results <- rbind(results, data.frame(
          trait1 = trait_names[i],
          trait2 = trait_names[j],
          r = mantel_res$statistic,
          p = mantel_res$signif
        ))
      }
    }
    
    if(Display.info == T){print(results)}
    Save.results <- results
    
    #### Reshape results ####
    results$stars <- cut(results$p,
                         breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                         labels = c("***", "**", "*", "")
    )
    results$stars <- as.character(results$stars)  # Convert to character to avoid 1/2/3 output
    trait_names <- unique(c(results$trait1, results$trait2))
    mantel_mat <- matrix(NA, nrow = length(trait_names), ncol = length(trait_names),
                         dimnames = list(trait_names, trait_names))
    stars_mat <- mantel_mat  # same structure for significance stars
    
    for (row in 1:nrow(results)) {
      i <- results$trait1[row]
      j <- results$trait2[row]
      mantel_mat[i, j] <- results$r[row]
      stars_mat[i, j] <- results$stars[row]
    }
    
    diag(mantel_mat) <- 1
    diag(stars_mat) <- ""
    
    mantel_df <- reshape2::melt(mantel_mat, na.rm = TRUE)
    stars_df <- reshape2::melt(stars_mat, na.rm = TRUE)
    
    plot_df <- merge(mantel_df, stars_df, by = c("Var1", "Var2"))
    colnames(plot_df) <- c("Trait1", "Trait2", "r", "stars")
    Title.A <- "(A) Mantel r"
  }
  else{plot_df <- NULL}
  
  #### Moran's cross-I ####
  if(Moran.cross.I == T){
    #### Prepare Spatial Data ####
    dups <- duplicated(Mcoord)
    
    if(sum(dups) > 0){
      Mcoord$Longitude <- jitter(Mcoord$Longitude, amount = 1e-5)
      Mcoord$Latitude  <- jitter(Mcoord$Latitude, amount = 1e-5)
    }
    
    library(spdep)
    coords <- as.matrix(Mcoord)
    nb <- knn2nb(knearneigh(coords, k = Nb.voisin))
    listw <- nb2listw(nb, style = "W")
    
    #### Cross Moran function ####
    Moran.cross.I.perm <- function(x, y, listw, n_perm = Npermutation) {
      n <- length(x)
      x_dev <- x - mean(x, na.rm = T)
      y_dev <- y - mean(y, na.rm = T)
      
      # Observed Moran's I
      lag_y <- lag.listw(listw, y_dev)
      numerator_obs <- sum(x_dev * lag_y)
      denom <- sqrt(sum(x_dev^2) * sum(y_dev^2))
      S0 <- sum(unlist(listw$weights))
      obs_I <- (n / S0) * numerator_obs / denom
      
      # Permutation distribution
      perm_I <- numeric(n_perm)
      for (i in 1:n_perm) {
        y_perm <- sample(y)
        y_dev_perm <- y_perm - mean(y_perm, na.rm = T)
        lag_y_perm <- lag.listw(listw, y_dev_perm)
        numerator_perm <- sum(x_dev * lag_y_perm)
        perm_I[i] <- (n / S0) * numerator_perm / sqrt(sum(y_dev_perm^2) * sum(x_dev^2))
      }
      
      # Two-sided p-value
      p_value <- (sum(abs(perm_I) >= abs(obs_I)) + 1) / (n_perm + 1)
      
      return(list(I = obs_I, p = p_value))
    }
    
    Moran.cross.I.perm.symmetric <- function(x, y, listw, n_perm = Npermutation) {
      n <- length(x)
      x_dev <- x - mean(x)
      y_dev <- y - mean(y)
      S0 <- sum(unlist(listw$weights))
      
      # Get neighbor indices and weights
      W <- listw$weights
      N <- listw$neighbours
      
      # Compute observed numerator: sum over i and j of w_ij * x_i' * y_j'
      obs_numerator <- 0
      for (i in 1:n) {
        for (j in seq_along(N[[i]])) {
          neighbor <- N[[i]][j]
          weight <- W[[i]][j]
          obs_numerator <- obs_numerator + weight * x_dev[i] * y_dev[neighbor]
        }
      }
      
      denom <- sqrt(sum(x_dev^2) * sum(y_dev^2))
      obs_I <- (n / S0) * obs_numerator / denom
      
      # Permutations
      perm_I <- numeric(n_perm)
      for (p in 1:n_perm) {
        y_perm <- sample(y_dev)  # permute residuals (already centered)
        numerator <- 0
        for (i in 1:n) {
          for (j in seq_along(N[[i]])) {
            neighbor <- N[[i]][j]
            weight <- W[[i]][j]
            numerator <- numerator + weight * x_dev[i] * y_perm[neighbor]
          }
        }
        perm_I[p] <- (n / S0) * numerator / sqrt(sum(x_dev^2) * sum(y_perm^2))
      }
      
      p_value <- (sum(abs(perm_I) >= abs(obs_I)) + 1) / (n_perm + 1)
      return(list(I = obs_I, p = p_value))
    }
    
    #### Compute Matrix of Moran's Cross-I and P-values ####
    traits <- as.data.frame(traits_df)
    results_I <- matrix(NA, nrow = length(trait_names), ncol = length(trait_names))
    results_p <- matrix(NA, nrow = length(trait_names), ncol = length(trait_names))
    rownames(results_I) <- rownames(results_p) <- trait_names
    colnames(results_I) <- colnames(results_p) <- trait_names
    
    for (i in seq_along(trait_names)) {
      for (j in seq_along(trait_names)) {
        x <- traits[[i]]
        y <- traits[[j]]
        # res <- Moran.cross.I.perm(x, y, listw, n_perm = Npermutation)
        # res <- Moran.cross.I.perm.symmetric(x, y, listw, n_perm = Npermutation)
        # results_I[i, j] <- res$I
        # results_p[i, j] <- res$p
        
        res1 <- Moran.cross.I.perm.symmetric(traits[[i]], traits[[j]], listw, n_perm = Npermutation)
        res2 <- Moran.cross.I.perm.symmetric(traits[[j]], traits[[i]], listw, n_perm = Npermutation)
        results_I[i, j] <- results_I[j, i] <- mean(c(res1$I, res2$I), na.rm = T)
        results_p[i, j] <- results_p[j, i] <- mean(c(res1$p, res2$p), na.rm = T)  # optional
        
      }
    }
    if(Display.info == T){print(results_I)}
    
    #### Melt data ####
    crossI_df <- reshape2::melt(results_I, na.rm = T)
    stars_df <- reshape2::melt(results_p, na.rm = T)
    
    plot_df <- merge(crossI_df, stars_df, by = c("Var1", "Var2"))
    colnames(plot_df) <- c("Trait1", "Trait2", "r", "stars")
    
    plot_df <- plot_df %>%
      dplyr::filter(as.numeric(factor(Trait1, levels = trait_names)) <= as.numeric(factor(Trait2, levels = trait_names)))
    
    plot_df$stars <- cut(plot_df$stars,
                         breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                         labels = c("***", "**", "*", ""))
    
    Title.A <- "(A) Moran's cross-I"
    
    #### Highlight some tiles ####
    if(is.null(Highlight.tiles) == F){
      heat_data <- plot_df
      print(heat_data)
      heat_data$highlight <- apply(heat_data[, c("Trait1", "Trait2")], 1, function(row) {any(mapply(function(x) all(row == x), Highlight.tiles))})
      print(heat_data)
      Bold.tiles <- geom_tile(data = subset(heat_data, highlight), color = "black", size = 1.2, fill = NA)
    }
    else{Bold.tiles <- NULL}
  }
  
  #### Plot principal ####
  p <- ggplot(plot_df, aes(Trait1, Trait2, fill = r)) +
    geom_tile(color = "white", width = 1) +
    Bold.tiles +
    geom_text(aes(label = paste0(round(r, 2), stars)), color = "black") +
    scale_fill_gradient2(low = "royalblue", mid = "white", high = "darkorange", midpoint = 0, limits = c(-1, 1), name = "") +
    theme_minimal() +
    theme(legend.position = "none", axis.title = element_blank(),
          axis.ticks = element_blank())+
    ggtitle(Title.A) +
    coord_fixed()
  
  #### Moran's I ####
  if(Moran.I == T){
    source("Scripts/Load_func.R")
    # source("Scripts/Pollen_fun_trans.R")
    print("Moran's I Calculation")
    traits_df <- data.frame(traits_df)
    Mcoord <- data.frame(Mcoord)
    Moiran.result <- data.frame()
    for(i in 1:ncol(traits_df)){
      Res.i <- Spatial.autocor.check(Mcoord = Mcoord, Mvariable = traits_df[names(traits_df)[i]], Show.dup.sites = F)
      Moiran.result <- rbind(Moiran.result, data.frame(Res.i))
    }
    Moiran.result$Trait <- row.names(Moiran.result)
    Moiran.result <- as.data.frame(lapply(Moiran.result, unlist))
    Moiran.result$Trait <- factor(Moiran.result$Trait, levels = trait_names)
    
    p2 <- ggplot(Moiran.result, aes(x = 1, y = Trait, fill = I)) +
      geom_tile(color = "white", width = 1) +
      # geom_text(aes(label = round(I, 2)), size = 3) +
      geom_text(aes(label = round(I, 2))) +
      scale_fill_gradient2(low = "royalblue", mid = "white", high = "darkorange", midpoint = 0, limits = c(-1, 1), name = "") +
      theme_minimal(base_size = 12) +
      ggtitle("(B) Moran's I")+
      coord_fixed() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            legend.position = "none",
            axis.ticks = element_blank(),
            # plot.margin = unit(c(0,1.5,0,0), "lines"),
            panel.grid = element_blank()#,
            # axis.text.y = element_text(angle = 0)
      )
    
    if(Mantel.test == T | Moran.cross.I == T){p <- p + p2 + plot_layout(widths = c(length(trait_names), 1), guides = "collect")}
    else{p <- p2}
  }
  
  #### Save plot ####
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){ggsave(p, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
    else{ggsave(Save.plot)}}
  if(Return.plot == T){return(p)}
}

Map.biogeo.CWM <- function(MCWT = NULL, MCWT2 = NULL, Select.trait = NULL, Type1 = "Type1", Type2 = "Type2", Crop.zone = NULL, Leg.pos = "bottom",
                           Vertical = F, Show.diff = F, Strip.lab = T, Show.type.lab = T, Show.trait.lab = T, Hex.size = 40, H = NULL, W = NULL, Save.plot = NULL){
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Only one CWM ####
  if(is.null(MCWT) == T & is.null(MCWT2) == T){print("**** Please prodive at least one CWM data.frame(). ****"); return()}
  if(is.null(MCWT) == T & is.null(MCWT2) == F){MCWT <- MCWT2; MCWT2 <- NULL}
  
  #### Crop area Map ####
  if(is.null(Crop.zone) == F){
    crs.select <- 3857
    Select.map <- st_as_sf(map('world', fill = TRUE, plot = FALSE, region = Crop.zone))
    Select.map <- st_transform(Select.map, crs = crs.select) #32647, 3857
    M.surf <- st_as_sf(MCWT, coords = 3:2, crs = 4326) #4326
    M.surf <- st_transform(M.surf, crs = crs.select) # CRS pseudo-mercador -> coord plane
    M.surf <- st_intersection(M.surf, Select.map)
    M.surf <- st_transform(M.surf, crs = crs.select) # CRS Mongolia 
    M.surf$Longitude <- st_coordinates(M.surf)[,1]
    M.surf$Latitude <- st_coordinates(M.surf)[,2]
    MCWT <- as.data.frame(M.surf)
    
    if(is.null(MCWT2) == F){
      M.surf2 <- st_as_sf(MCWT2, coords = 3:2, crs = 4326) #4326
      M.surf2 <- st_transform(M.surf2, crs = crs.select) # CRS pseudo-mercador -> coord plane
      M.surf2 <- st_intersection(M.surf2, Select.map)
      print(M.surf2)
      M.surf2 <- st_transform(M.surf2, crs = crs.select) # CRS Mongolia 
      M.surf2$Longitude <- st_coordinates(M.surf2)[,1]
      M.surf2$Latitude <- st_coordinates(M.surf2)[,2]
      MCWT2 <- as.data.frame(M.surf2)
    }
    Select.map <- st_transform(Select.map, crs = crs.select) 
    Fond.carte <- geom_sf(data = Select.map, alpha = 0, color = "black", size = .5)
    Fond.carte.1 <- NULL
  }
  else{
    Fond.carte.1 <- geom_polygon(data = ACA.bo.proj, aes(x=long, y=lat),colour="black", fill = NA, size = .5)
    Fond.carte <- geom_polygon(data = ACA.bo.co.proj, aes(x=long, y=lat, group = group), colour = "grey40", fill = NA, size = .25, linetype = 2)}
  
  #### Plot maps CWT trait biogeography ####
  MCWT <- MCWT[c(grep("Lat", names(MCWT)), grep("Lon", names(MCWT)), which(names(MCWT) %in% Select.trait))]
  MCWT <- melt(MCWT, id = c("Latitude","Longitude"))
  names(MCWT)<- c("Latitude", "Longitude", "CWM_Traits", "Value")
  MCWT$Type <- Type1
  
  if(is.null(MCWT2) == F){
    MCWT2 <- MCWT2[c(grep("Lat", names(MCWT2)), grep("Lon", names(MCWT2)), which(names(MCWT2) %in% Select.trait))]
    MCWT2 <- melt(MCWT2, id = c("Latitude","Longitude"))
    names(MCWT2)<- c("Latitude", "Longitude", "CWM_Traits", "Value")
    MCWT2$Type <- Type2
    MCWT <- rbind(MCWT, MCWT2)}
  
  #### Difference between the two maps ####
  if(Show.diff == T & is.null(MCWT) == F & is.null(MCWT2) == F){
    library(hexbin)
    library(ggplot2)
    
    set.seed(2)
    xA <- rnorm(1000)
    set.seed(3)
    yA <- rnorm(1000)
    set.seed(4)
    zA <- sample(c(1, 0), 20, replace = TRUE, prob = c(0.2, 0.8))
    hbinA <- hexbin(xA, yA, xbins = 40, IDs = TRUE)
    
    A <- data.frame(x = xA, y = yA, z = zA)
    
    set.seed(5)
    xB <- rnorm(1000)
    set.seed(6)
    yB <- rnorm(1000)
    set.seed(7)
    zB <- sample(c(1, 0), 20, replace = TRUE, prob = c(0.4, 0.6))
    hbinB <- hexbin(xB, yB, xbins = 40, IDs = TRUE)
    
    B <- data.frame(x = xB, y = yB, z = zB)
    
    
    ggplot(A, aes(x, y, z = z)) + stat_summary_hex(fun = function(z) sum(z)/length(z), alpha = 0.8) +
      scale_fill_gradientn(colours = c("blue","red")) +
      guides(alpha = FALSE, size = FALSE)
    
    ggplot(B, aes(x, y, z = z)) + stat_summary_hex(fun = function(z) sum(z)/length(z), alpha = 0.8) +
      scale_fill_gradientn (colours = c("blue","red")) +
      guides(alpha = FALSE, size = FALSE)
    
    ## find the bounds for the complete data 
    xbnds <- range(c(A$x, B$x))
    ybnds <- range(c(A$y, B$y))
    nbins <- 30
    
    #  function to make a data.frame for geom_hex that can be used with stat_identity
    makeHexData <- function(df) {
      h <- hexbin(df$x, df$y, nbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 z = tapply(df$z, h@cID, FUN = function(z) sum(z)/length(z)),
                 cid = h@cell)
    }
    
    Ahex <- makeHexData(A)
    Bhex <- makeHexData(B)
    
    ##  not all cells are present in each binning, we need to merge by cellID
    byCell <- merge(Ahex, Bhex, by = "cid", all = T)
    
    ##  when calculating the difference empty cells should count as 0
    byCell$z.x[is.na(byCell$z.x)] <- 0
    byCell$z.y[is.na(byCell$z.y)] <- 0
    
    ##  make a "difference" data.frame
    Diff <- data.frame(x = ifelse(is.na(byCell$x.x), byCell$x.y, byCell$x.x),
                       y = ifelse(is.na(byCell$y.x), byCell$y.y, byCell$y.x),
                       z = byCell$z.x - byCell$z.y)
    
    ##  plot the results
    
    ggplot(Ahex) +
      geom_hex(aes(x = x, y = y, fill = z),
               stat = "identity", alpha = 0.8) +
      scale_fill_gradientn (colours = c("blue","red")) +
      guides(alpha = FALSE, size = FALSE)
    
    ggplot(Bhex) +
      geom_hex(aes(x = x, y = y, fill = z),
               stat = "identity", alpha = 0.8) +
      scale_fill_gradientn (colours = c("blue","red")) +
      guides(alpha = FALSE, size = FALSE)
    
    ggplot(Diff) +
      geom_hex(aes(x = x, y = y, fill = z),
               stat = "identity", alpha = 0.8) +
      scale_fill_gradientn (colours = c("blue","red")) +
      guides(alpha = FALSE, size = FALSE)
  }
  # print(MCWT)
  
  n.trait <- nlevels(MCWT$CWM_Traits)
  MCWT$CWM_Traits <- as.factor(gsub("TRY_", "", MCWT$CWM_Traits))
  MCWT$Type <- as.factor(MCWT$Type)
  n.type <- nlevels(MCWT$Type)
  
  MCWT$Longitude <- round(MCWT$Longitude, digits = 0)
  MCWT$Latitude <- round(MCWT$Latitude, digits = 0)
  
  #### Annotations names Strig.lab = F ####
  if(Strip.lab == F){
    Strip.lab.disp <- element_blank()
    S.trait <- setNames(data.frame(as.factor(unique(MCWT$CWM_Traits)), rep(1,nlevels(MCWT$CWM_Traits)), rep(1,nlevels(MCWT$CWM_Traits))), c("Lab", "x","y"))
    S.clim <- setNames(data.frame(as.factor(unique(MCWT$Type)), rep(1,nlevels(MCWT$Type)), rep(1,nlevels(MCWT$Type))), c("Lab", "x","y"))
    
    Theme.null <- theme(axis.line = element_blank(), axis.title = element_blank(),
                        strip.text = element_blank(), axis.text = element_blank(),
                        axis.ticks = element_blank(), plot.background = element_blank(),
                        panel.grid = element_blank(), panel.background = element_blank())
    
    if(Vertical == T){print("Verticalisation !")
      p.up <- ggplot(S.clim, mapping = aes(x = x, y = y))+ 
        facet_wrap(vars(Lab), scales = "free_x", ncol = n.trait) + 
        geom_text(aes(label = Lab))+ Theme.null
      
      p.right <- ggplot(S.trait, mapping = aes(x = x, y = y))+ 
        facet_wrap(vars(Lab), scales = "free_x", nrow = n.trait) +
        geom_text(aes(label = Lab), angle = 270,  hjust=0.5, vjust=1)+ Theme.null}
    else{
      p.up <- ggplot(S.trait, mapping = aes(x = x, y = y))+ 
        facet_wrap(vars(Lab), scales = "free_x", ncol = n.trait) + 
        geom_text(aes(label = Lab))+ Theme.null
      
      p.right <- ggplot(S.clim, mapping = aes(x = x, y = y))+ 
        facet_wrap(vars(Lab), scales = "free_x", nrow = n.trait) +
        geom_text(aes(label = Lab), angle = 270,  hjust=0.5, vjust=1)+ Theme.null
    }
    
    
    
  }
  else{Strip.lab.disp <- element_text(hjust = 0)}
  
  
  #### Graphical settings ####
  Map_theme <- ggplot2::theme(
    plot.background = element_blank(), panel.grid = element_blank(),
    axis.line = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
    legend.title = element_text(),
    legend.key = element_blank(),
    legend.justification = c("center"),
    legend.text = element_text(size = 8),
    legend.position = Leg.pos,
    panel.background = element_blank(),
    strip.text = Strip.lab.disp,
    plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm")
  )
  
  if(Vertical == T){
    My_facet <- facet_wrap(CWM_Traits ~ Type, ncol = n.type)
  }
  else{My_facet <- facet_wrap(Type ~ CWM_Traits, nrow = n.type)}
  
  #### Plots ####
  p <- ggplot() +
    scale_fill_gradientn(
      colors = c("#40004b", "grey80", "#00441b"),# label = c("a", "b", "c", "d", "e"),
      values = scales::rescale(c(-5,-0.7,0.5,1.3,5), from = c(-5,5)),
      name = "CWM-traits (z-scores)")+
    Fond.carte +
    Fond.carte.1 +
    stat_summary_hex(data = MCWT, mapping = aes(x = Longitude, y = Latitude, z = Value), 
                     fun = function(x) mean(x), position = "jitter",
                     bins = Hex.size, color = NA) +
    My_facet +
    Map_theme
  
  #### Export plot map ####
  if(Show.diff == T & is.null(MCWT) == F & is.null(MCWT2) == F){
    #### Methode 1 ####  
    # print(summary(MCWT$Value)[[1]])
    # print(summary(MCWT$Value)[[6]])
    d1 <- ggplot_build(p)$data[[3]][, 2:7]
    d1$x <- round(d1$x, digits = 0)
    d1$y <- round(d1$y, digits = 0)
    d1 <- reshape2::dcast(d1, x + y ~ PANEL, fun.aggregate = mean)
    names(d1) <- c("Longitude", "Latitude", "Veg", "Pol")
    d1 <- na.omit(d1)                                 # Apply na.omit function
    d1$Veg <- (d1$Veg-min(d1$Veg))/(max(d1$Veg)-min(d1$Veg))
    d1$Pol <- (d1$Pol-min(d1$Pol))/(max(d1$Pol)-min(d1$Pol))
    d1$diff <-abs(d1$Veg - d1$Pol)
    print(d1)
    
    p2 <- ggplot() +
      geom_polygon(data = ACA.bo.co.proj, aes(x=long, y=lat, group = group), colour = "grey40", fill = NA, size = .25, linetype = 2) +
      geom_polygon(data = ACA.bo.proj, aes(x=long, y=lat),colour="black", fill = NA, size = .5) +
      stat_summary_hex(data = d1, mapping = aes(x = Longitude, y = Latitude, z = diff), fun = function(x) mean(x), bins = Hex.size, color = NA) +
      scalebar(ACA.bo.proj, dist = 1000, dist_unit = "km", st.size = 1.7, border.size = 0.2,
               transform = TRUE, model = "WGS84", st.bottom = FALSE, st.dist = 0.03,
               facet.lev = c("TRY_SSD")) +
      north(ACA.bo.proj, symbol = 6, location = "topright", scale = 0.1)+ 
      Map_theme
    
    #### Methode 2 ####
    Save.fit <- ggplot_build(p)$data[[3]][c(2:7)]
    my_digits = 0
    Trait.1 <- Save.fit[Save.fit$PANEL == 1,] 
    Trait.1p <- Save.fit[Save.fit$PANEL == 4,]
    
    Trait.1$x <- round(Trait.1$x, digits = my_digits)
    Trait.1$y <- round(Trait.1$y, digits = my_digits)
    Trait.1p$x <- round(Trait.1p$x, digits = my_digits)
    Trait.1p$y <- round(Trait.1p$y, digits = my_digits)
    
    names(Trait.1p)[c(3:6)] <- paste(names(Trait.1p)[c(3:6)], "p", sep = "_")
    Trait.1 <-full_join(Trait.1, Trait.1p, by = c("x", "y"))
    # Trait.1$Erreur <- abs(Trait.1$value_p - Trait.1$value)/Trait.1$value*100
    Trait.1$Erreur <- abs(Trait.1$value_p - Trait.1$value)
    
    # p2 <- ggplot(Trait.1, aes(x = x, y = y, color = Erreur)) +
    #             geom_point(na.rm = T, size = 3)+
    #             scale_color_gradientn(
    #               # colors = c("#40004b", "#c2a5cf", "#a6dba0","#00441b"),# label = c("a", "b", "c", "d", "e"),
    #               # colors = c("royalblue", "grey80", "darkorange"),# label = c("a", "b", "c", "d", "e"),
    #               colors = c("#40004b", "grey80", "#00441b"),# label = c("a", "b", "c", "d", "e"),
    #               # values = scales::rescale(c(summary(MCWT$Value)[[1]],summary(MCWT$Value)[[2]],summary(MCWT$Value)[[4]],summary(MCWT$Value)[[5]],summary(MCWT$Value)[[6]]), from = c(summary(MCWT$Value)[[1]],summary(MCWT$Value)[[6]])),
    #               # values = scales::rescale(c(summary(MCWT$Value)[[1]],-0.2,0.4,summary(MCWT$Value)[[6]]), from = c(summary(MCWT$Value)[[1]],summary(MCWT$Value)[[6]])),
    #               values = scales::rescale(c(-5,-0.7,0.5,1.3,5), from = c(-5,5)), na.value = NA,
    #               name = "CWM-traits (z-scores)")+ Map_theme
    # 
    
    
  }
  
  #### Patchwork ####
  if(Strip.lab == F & Show.trait.lab == T & Show.type.lab == T){p <- p.up + plot_spacer() + p + p.right + plot_layout(nrow = 2, heights = c(1/40,39/40), widths = c(39/40,1/40))}
  if(Strip.lab == F & Show.trait.lab == F & Show.type.lab == T){p <- p.up + p + plot_layout(nrow = 2, heights = c(1/40,39/40))}
  if(Strip.lab == F & Show.trait.lab == T & Show.type.lab == F){p <- p + p.right + plot_layout(nrow = 2, widths = c(39/40,1/40))}
  if(Show.diff == T & Strip.lab == T){p <- p + p2}
  
  #### Export ####
  print(p)
  if(is.null(Save.plot) == F){dev.off()}
  return(p)
}

LRelation.CWT.clim <- function(CWT, Select.trait, Select.Pclim, Select.eco, Trait.lim, Add.n, Pearson.r = F,
                               Tit.x.axis = "Climate parameters", Pearson.r.pos = "topright", Transform.Pclim = NULL,
                               Transformation.method = "log", Strip.pos = "left",
                               Strip.lab, Bit.map, Leg.pos, Facet.scale, Add.linear, Alpha, Save.plot, H, W){
  #### Settings ####
  if(missing(Alpha)){Alpha = 1}
  if(missing(Trait.lim)){Trait.lim = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Select.trait)){Select.trait = NULL}
  if(missing(Select.Pclim)){Select.Pclim = NULL}
  if(missing(Select.eco)){Select.eco = NULL}
  if(missing(Add.linear)){Add.linear = NULL}
  if(missing(Add.linear)){Add.linear = NULL}
  if(missing(Strip.lab)){Strip.lab = T}
  if(missing(Bit.map)){Bit.map = F}
  if(missing(Add.n)){Add.n = F}
  if(missing(Leg.pos)){Leg.pos = "right"}
  if(missing(Facet.scale)){Facet.scale = "free_x"}
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  
  #### Transforme data ####
  if(is.null(Transform.Pclim) == F){
    if(Transformation.method == "log"){CWT[Transform.Pclim] <- log1p(CWT[Transform.Pclim])}
    if(Transformation.method == "sqrt"){CWT[Transform.Pclim] <- sqrt(CWT[Transform.Pclim])}
  }
  
  #### Select facets ####
  if(is.null(Select.trait) == T){Select.trait <- grep("TRY", names(CWT))}
  else{Select.trait <- which(names(CWT) %in% Select.trait)}
  
  if(is.null(Select.Pclim) == T){Select.Pclim <- setdiff(seq(1, length(names(CWT))), Trait)}
  else{Select.Pclim <- which(names(CWT) %in% Select.Pclim)}
  
  CWT.m1 <- melt(CWT[c(1, Select.trait)], id = "Site")
  names(CWT.m1) <- c("Site", "Trait", "Trait.Val")
  CWT.m2 <- melt(CWT[c(1, Select.Pclim)], id = "Site")
  names(CWT.m2) <- c("Site", "Clim", "Clim.Val")
  CWT.m <- merge(CWT.m1, CWT.m2, by = "Site")
  
  if(is.null(Select.eco) == T){CWT.m$Eco.Val <- "OK"}
  else{Select.eco <- which(names(CWT) %in% Select.eco)
  CWT.m3 <- melt(CWT[c(1, Select.eco)], id = "Site")
  names(CWT.m3) <- c("Site", "Eco", "Eco.Val")
  CWT.m <- merge(CWT.m, CWT.m3, by = "Site")
  }
  
  n.trait <- nlevels(CWT.m$Trait)
  n.clim <- nlevels(CWT.m$Clim)
  CWT.m$Trait <- as.factor(gsub("TRY_", "", CWT.m$Trait))
  CWT.m$Clim <- as.factor(gsub("_wc", "", CWT.m$Clim))
  CWT.m$Clim <- as.factor(gsub("_chel", "", CWT.m$Clim))
  
  #### Regression ####
  if(is.null(Add.linear) == F){
    if(Pearson.r.pos == "topright"){rx <- "right"; ry <- "top"}
    if(Pearson.r.pos == "topleft"){rx <- "left"; ry <- "top"}
    if(Pearson.r.pos == "bottomleft"){rx <- "left"; ry <- "bottom"}
    if(Pearson.r.pos == "bottomright"){rx <- "right"; ry <- "bottom"}
    
    Add.linear <- stat_poly_line(method='lm', se = T, color='turquoise4', fill = "turquoise4", size = .7, linetype = "dashed")
    
    if(Add.n == F){
      Add.r2 <- stat_poly_eq(label.y = ry, label.x = rx, color = "turquoise4", size = 3.5, small.r = F,
                             aes(label =  sprintf("%s*\", \"*%s" ,
                                                  after_stat(rr.label),
                                                  after_stat(p.value.label))))
      # Pearson r
      if(Pearson.r == T){
        Add.r2 <- stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, cor.coef.name = c("r"), 
                           label.x.npc = rx, label.y.npc = ry, hjust = 1, vjust = 0)}
    }
    else{
      Add.r2 <- stat_poly_eq(label.y = ry, label.x = rx, color = "turquoise4", size = 3, small.r = F,
                             aes(label =  sprintf("%s*\", \"*%s*\", \"*%s" ,
                                                  after_stat(rr.label),
                                                  after_stat(p.value.label),
                                                  after_stat(n.label))))
      if(Pearson.r == T){
        Add.r2 <- stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, cor.coef.name = c("r"), 
                           label.x.npc = rx, label.y.npc = ry, hjust = 1, vjust = 0)}
      
    }
  }
  
  #### Color def + lab ####
  Colors.biomes <- c("Deserts & Xeric Shrublands" = "#C88282",
                     "Temperate Grasslands, Savannas & Shrublands" = "#ECED8A",
                     "Montane Grasslands & Shrublands" = "#D0C3A7",
                     "Temperate Conifer Forests" = "#6B9A88",
                     "Temperate Broadleaf & Mixed Forests" = "#3E8A70",
                     "N/A" = "#FFEAAF",
                     "Tundra" = "#A9D1C2",
                     "Boreal Forests/Taiga" = "#8FB8E6",
                     "Tropical & Subtropical Coniferous Forests" = "#99CA81",
                     "Mangroves" = "#FE01C4",
                     "Flooded Grasslands & Savannas" = "#BEE7FF",
                     "Tropical & Subtropical Moist Broadleaf Forests" = "#38A700",
                     "Pollen" = "#bea33a",
                     "Vegetation" = "#6d956f")
  if(Bit.map == F){
    Plot.dot <- geom_point(aes(fill = Eco.Val, color = Eco.Val), size = 1, shape = 16, alpha = Alpha)
    
    Fill.scale <- scale_fill_manual(values = Colors.biomes, name = "Biomes (Dinerstein et al., 2017)")
    Col.scale <- scale_color_manual(values = Colors.biomes, name = "Biomes (Dinerstein et al., 2017)")}
  else{
    Plot.dot <- geom_hex(data = CWT.m, mapping = aes(x = Clim.Val, y = Trait.Val, fill = ..count..), bins = 60, color = NA)
    if(unique(CWT.m$Eco.Val) == "Pollen"){col.scale.hex <- c("#bea03733", "#a07e09f6")}
    if(unique(CWT.m$Eco.Val) == "Vegetation"){col.scale.hex <- c("#6289620d", "#3a723af6")}
    Fill.scale <- scale_fill_gradientn(colors = col.scale.hex,
                                       guide = "legend",
                                       values = scales::rescale(c(1,5,10,30,100,1000), from = c(1,1000)),
                                       # values = scales::rescale(c(1,1000), from = c(1,1000)),
                                       name = "Count")
    Col.scale <- NULL
  }
  
  if(is.null(Trait.lim) == F){Trait.lim <- ylim(Trait.lim)}
  else{Trait.lim <- NULL}
  
  #### Annotations names Strig.lab = F ####
  if(Strip.lab == F){
    Strip.lab.disp <- element_blank()
    S.trait <- setNames(data.frame(as.factor(unique(CWT.m$Trait)), rep(1,nlevels(CWT.m$Trait)), rep(1,nlevels(CWT.m$Trait))), c("Lab", "x","y"))
    S.clim <- setNames(data.frame(as.factor(unique(CWT.m$Clim)), rep(1,nlevels(CWT.m$Clim)), rep(1,nlevels(CWT.m$Clim))), c("Lab", "x","y"))
    
    Theme.null <- theme(axis.line = element_blank(), axis.title.x = element_blank(),
                        strip.text = element_blank(), axis.text = element_blank(),
                        axis.ticks = element_blank(), plot.background = element_blank(), strip.clip = "off",
                        panel.grid = element_blank(), panel.background = element_blank())
    
    p.up <- ggplot(S.clim, mapping = aes(x = x, y = y))+
      facet_wrap(vars(Lab), scales = "free_x", ncol = n.clim)+
      geom_text(aes(label = Lab))+ Theme.null + theme(axis.title.y = element_blank())
    
    p.right <- ggplot(S.trait, mapping = aes(x = x, y = y))+
      facet_wrap(vars(Lab), scales = "free_x", nrow = n.trait)+
      coord_cartesian(clip = 'off') +
      ylab(substitute(CWM~traits~(italic(z)-score)))+
      geom_text(aes(label = Lab), angle = 90,  hjust=0.5, vjust=1)+ Theme.null
    
    Y.tit <- NULL
    Tit.x.param <- element_blank()
  }
  else{
    Strip.lab.disp <- element_text()
    Y.tit <- ylab(substitute(CWM~traits~(italic(z)-score)))
    Tit.x.param <- element_text()
  }
  
  #### PLOT ####
  p <- ggplot(data = CWT.m, mapping = aes(x = Clim.Val, y = Trait.Val))+
    Plot.dot +
    Add.linear +
    Add.r2 +
    xlab(Tit.x.axis)+
    Y.tit +
    facet_wrap(Trait~Clim, scales = Facet.scale, ncol = n.clim, strip.position = Strip.pos)+
    Trait.lim +
    Fill.scale +
    Col.scale +
    #### Theme ####
  theme(
    axis.line = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA),
    plot.background = element_blank(), 
    legend.position = Leg.pos, axis.title.y = Tit.x.param,
    panel.grid = element_blank(), 
    strip.text = Strip.lab.disp,
    strip.placement = "outside",
    strip.background = element_blank(),
  )
  
  if(Strip.lab == F & Strip.pos == "right"){p <- p.up + plot_spacer() + p + p.right + plot_layout(nrow = 2, heights = c(1/40,39/40), widths = c(39/40,1/40))}
  if(Strip.lab == F & Strip.pos == "left"){p <- plot_spacer() + p.up + p.right + p + plot_layout(nrow = 2, heights = c(1/40,39/40), widths = c(1/40,39/40))}
  
  #### Export ####
  print(p)
  if(is.null(Save.plot) == F){dev.off()}
  return(p)
}

R2.compar <- function(MP, MV, Xlab, Ylab, Bisectrice.area, Leg.pos, shade.areas, Pourc.coerrent = T, Display.message = T,
                      RMA = F, Avg.r.Fisher.z = T,
                      p.val.show = T, Compare.slopes = F, Panel.lab, Show.Plotly, Repel.outliers, Nb.labs, H, W, Save.plot){
  #### Settings ####
  library(ggrepel)
  if(missing(Leg.pos)){Leg.pos = "top"}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Panel.lab)){Panel.lab = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Xlab)){Xlab = NULL}
  if(missing(Ylab)){Ylab = NULL}
  if(missing(Nb.labs)){Nb.labs = NULL}
  if(missing(Bisectrice.area)){Bisectrice.area = NULL}
  if(missing(shade.areas)){shade.areas = F}
  if(missing(Repel.outliers)){Repel.outliers = T}
  if(missing(Show.Plotly)){Show.Plotly = F}
  library(RColorBrewer)
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Calc ####
  if(Compare.slopes == F){
    P1 <- suppressWarnings(melt(MP$R2))
    P2 <- suppressWarnings(melt(MV$R2))
  }
  else{ # ATTENTION PB 
    P1 <- suppressWarnings(melt(MP$Slope))
    P2 <- suppressWarnings(melt(MV$Slope))
  }
  
  names(P1) <- c("X1", "X2", "R2_pol") 
  names(P2) <- c("X1", "X2", "R2_veg")
  P1 <- left_join(P1, P2, by = c("X1", "X2"))
  P1$Func <- paste(P1$X2, " vs. ", P1$X1, "", sep = "")
  
  #### Average Pearson's r (Fisher z) ####
  if(Avg.r.Fisher.z == T){
    Mean.r.Fisher.z <- function(r, na.rm = T, n = NULL) {
      if(na.rm == T){r <- r[!is.na(r)]}
      
      if(is.null(n) == T){
        z <- atanh(r)
        z <- tanh(mean(z))
      }
      else{
        z <- atanh(r)
        z <- tanh(weighted.mean(z, w = n - 3))  
      }
      return(z)
    }
    
    r_mean <- numeric(nrow(P1))
    
    for (i in seq_len(nrow(P1))) {
      r_mean[i] <- Mean.r.Fisher.z(r = c(P1$R2_pol[i], P1$R2_veg[i]))
    }
    
    P1$Mean <- r_mean
  }
  else{P1$Mean <- rowMeans(P1[c(3,4)])}
  
  #### Keep only one combination possible ####
  if(identical(row.names(MP$R2), names(MP$R2)) == T){
    Comb <- data.frame(gtools::combinations(nrow(MP$R2), 2, row.names(MP$R2)))
    Comb$Func <- paste(Comb$X2, " vs. ", Comb$X1, "", sep = "")
    P1 <- P1[P1$Func %in% Comb$Func,]
    P1 <- P1[P1$R2_pol !=1,]
  }
  
  #### p-value ####
  if(p.val.show == T){
    Mpval1 <- suppressWarnings(melt(MP$p.val))
    names(Mpval1) <- c("X1", "X2", "pval_pol") 
    Mpval2 <- suppressWarnings(melt(MV$p.val))
    names(Mpval2) <- c("X1", "X2", "pval_veg") 
    Mpval1 <- left_join(Mpval1, Mpval2, by = c("X1", "X2"))
    Mpval1$Func <- paste(Mpval1$X2, " vs. ", Mpval1$X1, "", sep = "")
    if(identical(row.names(MP$R2), names(MP$R2)) == T){Mpval1 <- Mpval1[Mpval1$Func %in% Comb$Func,]}
    Mpval1 <- Mpval1[Mpval1$pval_veg !=1,]
    Mpval1$Significant <- Mpval1$pval_pol < 0.001 & Mpval1$pval_veg < 0.001
    Mpval1$Significant[Mpval1$Significant == T] <- "p < 0.001"
    Mpval1$Significant[Mpval1$Significant == F] <- "p > 0.001"
    P1 = cbind(P1, Signific = Mpval1$Significant)
  }
  else{P1$Signific = "All dots"}
  
  #### Signe coherence ####
  if(Pourc.coerrent == T){
    print("**** r direction consistence between vegetation and pollen : ****")
    Tab.coer <- P1
    Tab.coer$Pos.pol <- P1$R2_pol >= 0
    Tab.coer$Pos.veg <- P1$R2_veg >= 0
    Tab.coer$Coerence <- Tab.coer$Pos.pol == Tab.coer$Pos.veg
    Pourc.coerence <- 100 - round(length(which(Tab.coer$Coerence == F))/nrow(Tab.coer)*100, digits = 0)
    print(paste(Pourc.coerence, "% of the relations are consistent in direction."))
  }
  
  #### Graph settings ####
  if(is.null(Xlab) == F){Xlab <- xlab(Xlab)}
  if(is.null(Ylab) == F){Ylab <- ylab(Ylab)}
  if(is.null(Panel.lab) == F){
    if(Compare.slopes == F){Xan <- -1; Yan <- 1}
    else{Xan <- min(P1$R2_veg, na.rm = T); Yan <- max(P2$R2_veg, na.rm = T)}
    Panel.name <- annotate("text", x = Xan, y = Yan, label = Panel.lab, colour="grey30", hjust = -0.2, vjust = 1.5, na.rm = T)}
  else{Panel.name <- NULL}
  
  if(is.null(Bisectrice.area) == F){Bisectrice.area <- geom_abline(aes(slope = 1, intercept = 0), linewidth = Bisectrice.area, color = "grey70", alpha = 0.2, linetype = "solid")}
  
  if(shade.areas == T){
    Shade.box <- geom_rect(inherit.aes = F, data = NULL, xmin = 0, xmax = 1, ymin = -1, ymax = 0, fill = "#FAE6DD", colour = NA, na.rm = T)
    Shade.box2 <- geom_rect(inherit.aes = F, xmin = 0, xmax = -1, ymin = 1, ymax = 0, fill = "#FAE6DD", colour = NA, na.rm = T)
  }
  else{Shade.box = Shade.box2 <- NULL}
  
  if(Compare.slopes == F){
    My_lims <- lims(x = c(-1,1), y = c(-1,1))
    My_yaxis <- scale_y_continuous(limits = c(-1,1), expand = c(0, 0))
    My_xaxis <- scale_x_continuous(limits = c(-1,1), expand = c(0, 0))
  }
  else{
    My_lims <- lims(x = c(min(P1$R2_veg, na.rm = T), max(P1$R2_veg, na.rm = T)), y = c(min(P2$R2_veg, na.rm = T), max(P2$R2_veg, na.rm = T)))
    My_xaxis <- scale_x_log10()
    My_yaxis <- scale_y_log10()
  }
  
  #### Repel selection ####
  if(is.null(Nb.labs) == F){
    dist2d <- function(a) {
      b = c(0,0)
      c = c(1,1)
      
      v1 <- b - c
      v2 <- a - b
      m <- cbind(v1,v2)
      d <- abs(det(m))/sqrt(sum(v1*v1))
    } 
    
    P1$Dist <- NA
    for(i in 1:nrow(P1)){
      a <- c(P1$R2_pol[i], P1$R2_veg[i])
      P1$Dist[i] <- dist2d(a)
    }
    
    
    if(Repel.outliers == T){
      P1 <- P1[!is.na(P1$Dist),]
      Val4 <- sort(P1$Dist)[length(P1$Dist)-Nb.labs]
      Sub.DB <- subset(P1, Dist > Val4)
    }
    else{
      Val4 <- sort(P1$Dist)[Nb.labs]
      Sub.DB <- subset(P1, Dist <= Val4)
    }
    
    Annot <- geom_text_repel(data = Sub.DB, aes(label = Func), size = 2.5, force = 100, max.overlaps = 100,
                             force_pull = 10,
                             nudge_x = 0.3,
                             nudge_y = 0.3,
                             segment.size = 0.3,
                             segment.curvature = 0.4,
                             segment.angle = 35,
                             show.legend = F)
  }
  else{Annot <- NULL}
  
  #### Best table of r ####
  P1$Pond <- 1-abs(P1$Dist)
  P1$Pond <- abs(P1$R2_pol)*P1$Pond
  P1 <- P1[order(P1$Pond, decreasing = T),]
  Table.best <- P1[c(1:10), c(5,3,4)]
  
  #### Linear regression or Reduced major axis regression (RMA) ####
  if(RMA == T){
    library(lmodel2)
    mod <- lmodel2(R2_pol ~ R2_veg, data = P1,"interval", "interval", 999)
    reg <- cbind(mod$regression.results[c(1,2,3,5)], mod$confidence.intervals[c(3,5)])
    names(reg) <- c("method", "intercept", "slope", "p_value", "CI_inter", "CI_slope")
    
    Lab.R2 <- paste0("italic(R)^2==", signif(mod$rsquare, 2),"*','~italic(p)<=", signif(reg$p_value[1], 2),"*','~n==", nrow(P1))
    Add.r2 <- annotate("text", x = +Inf, y = -Inf, label = Lab.R2, hjust = 1.07, vjust = -0.5, na.rm = T, parse = T, size = 3, color = "turquoise4")
    reg <- reg[reg$method == "SMA",]
    
    xmin <- min(P1$R2_pol, na.rm = T)
    xmax <- max(P1$R2_pol, na.rm = T)
    ymin <- reg$slope * xmin + reg$intercept
    ymax <- reg$slope * xmax + reg$intercept
    line_df <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    
    LR <- geom_line(inherit.aes = F, data = line_df, aes(x, y), linetype = "longdash", linewidth = 0.5, color = "turquoise4")
    
    x_seq <- seq(min(P1$R2_pol, na.rm = T), max(P1$R2_pol, na.rm = T), length.out = 100)
    ci <- mod$confidence.intervals[3,2:5]
    
    fit_df <- data.frame(
      x = x_seq,
      y = reg$slope * x_seq + reg$intercept,
      ymin = ci[[3]] * x_seq + ci[[1]],
      ymax = ci[[4]] * x_seq + ci[[2]]
    )
    
    My_CI <-  geom_ribbon(inherit.aes = F, data = fit_df, aes(x = x, y = y, ymin = ymin, ymax = ymax), alpha = 0.2, fill = "turquoise4")
  }
  else{
    Add.r2 <- stat_poly_eq(label.y = "bottom", label.x = "right", color = "turquoise4", size = 3, small.r = F, small.p = T,
                           aes(label =  sprintf("%s*\", \"*%s" ,
                                                after_stat(rr.label),
                                                after_stat(p.value.label))))
    LR <- geom_smooth(method = "lm", se = F, span = 1000, linetype = "longdash", linewidth = 0.5, color = "turquoise4",
                      formula = y ~ x)
    
    My_CI <- NULL
  }
  
  #### Plot ####
  p <- ggplot(P1, aes(x = R2_pol, y = R2_veg, color = Mean))+
    My_CI +
    Xlab + Ylab + Shade.box + Shade.box2 +
    geom_abline(aes(slope = 1, intercept = 0), linewidth = .8, color = "grey60", linetype = "dashed")+
    Bisectrice.area +
    geom_point(aes(shape = Signific))+
    My_lims + My_xaxis + My_yaxis + 
    LR +
    Add.r2 +
    scale_color_gradientn(colors = c("royalblue", "grey50", "darkorange", "red"),
                          values = c(0,0.26,0.38,0.5,1),
                          name = substitute(Pearson*minute*s~italic(r)))+
    scale_shape(name = NULL)+
    Annot +
    Panel.name +
    theme(axis.line = element_line(colour = "grey30"), legend.background = element_blank(), 
          plot.background = element_blank(), panel.background = element_blank(),
          panel.grid = element_blank(), legend.direction = "horizontal", legend.position = Leg.pos,
          legend.key = element_blank(), legend.text = element_text(size = 4, angle = 0),
          legend.key.size = unit(4, "mm"), legend.title = element_text(size = 8, vjust = 0.75),
          panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 1)
          
    )
  
  #### Save html ####
  if(Show.Plotly == T){
    library(plotly)
    library(htmlwidgets)
    Save.plot.html <- gsub("pdf", "html", Save.plot)
    Keep.name <- gsub(".*\\/", "", Save.plot.html)
    Path.root <- paste(gsub(Keep.name, "", Save.plot.html), "HTML_files/", sep = "")
    if(file.exists(Path.root) == F){dir.create(Path.root)}
    Save.plot.html <- paste(Path.root, Keep.name, sep = "")
    p1_ly <- ggplotly(p)
    p1_ly <- p1_ly %>% layout(boxmode = "group", boxpoints = F)
    options(warn = - 1) 
    saveWidget(p1_ly, file = Save.plot.html)
  }
  #### Export ####
  print(p)
  if(is.null(Save.plot) == F){dev.off()}
  return(p)
}

Biplot.bioclim <- function(MC.area, MC.samp, Same.mat, PClim1, PClim2, Show.density, Emprise.alpha, Add.reg, R2.pos,
                           Emprise.bin, Emprise.size, return.pick, Save.plot, H, W, Leg.pos, Mean.cal, Limites){
  #### Settings ####
  if(missing(Add.reg)){Add.reg = F}
  if(missing(Emprise.bin)){Emprise.bin = F}
  if(missing(return.pick)){return.pick = F}
  if(missing(Emprise.alpha)){Emprise.alpha = 1}
  if(missing(Emprise.size)){Emprise.size = 3}
  if(missing(Leg.pos)){Leg.pos = c(0.15, 0.85)}
  if(missing(Limites)){Limites = NULL}
  if(missing(Show.density)){Show.density = T}
  if(missing(Same.mat)){Same.mat = F}
  if(missing(Mean.cal)){Mean.cal = F}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(R2.pos)){R2.pos = "bottomleft"}
  
  
  MC.area <- MC.area[!is.na(MC.area$Biome),] 
  MC.samp <- MC.samp[!is.na(MC.samp$Biome),] 
  
  #### Stats ####
  if(Mean.cal == T){
    mu2 <- aggregate(MC.samp[[PClim2]], list(MC.samp$Biome), FUN=mean) 
    names(mu2)[1]<- "Biome"
    mu1 <- aggregate(MC.samp[[PClim1]], list(MC.samp$Biome), FUN=mean) 
    names(mu1)[1]<- "Biome"
    Mean.line1 <- geom_vline(data = mu1, aes(xintercept = x, color = Biome), linetype="dashed")
    Mean.line2 <- geom_vline(data = mu2, aes(xintercept = x, color = Biome), linetype="dashed")
  }
  else{
    Mean.line1 <- NULL
    Mean.line2 <- NULL
  }
  
  #### Colours settings ####
  Col.vec <- c("Deserts & Xeric Shrublands" = "#C88282",
               "Temperate Grasslands, Savannas & Shrublands" = "#ECED8A",
               "Montane Grasslands & Shrublands" = "#D0C3A7",
               "Temperate Conifer Forests" = "#6B9A88",
               "Temperate Broadleaf & Mixed Forests" = "#3E8A70",
               "N/A" = "#FFEAAF",
               "Tundra" = "#A9D1C2",
               "Boreal Forests/Taiga" = "#8FB8E6",
               "Tropical & Subtropical Coniferous Forests" = "#99CA81",
               "Mangroves" = "#FE01C4",
               "Flooded Grasslands & Savannas" = "#BEE7FF",
               "Tropical & Subtropical Moist Broadleaf Forests" = "#38A700"
  )
  
  #### Limits ####
  if(is.null(Limites) == F){
    Lim.x <- xlim(c(Limites[1], Limites[2]))
    Lim.y <- ylim(c(Limites[3], Limites[4]))
    Lim.up <- Lim.x 
    Lim.right <- xlim(c(Limites[3], Limites[4])) 
  }
  else{
    Lim.x <- NULL
    Lim.y <- NULL
    Lim.up <- NULL
    Lim.right <- NULL
  }
  
  #### Add regression lines ####
  if(Add.reg == T){print(R2.pos)
    if(R2.pos == "bottomleft"){
      R2.y = "bottom"
      R2.x = "left"}
    if(R2.pos == "bottomright"){
      R2.y = "bottom"
      R2.x = "right"} 
    if(R2.pos == "topright"){
      R2.y = "top"
      R2.x = "right"}
    if(R2.pos == "topleft"){
      R2.y = "top"
      R2.x = "left"}
    if(R2.pos == "none"){
      R2.y = "none"
      R2.x = "none"}
    print(R2.x)
    Add.reg.line <- geom_smooth(data = MC.samp, mapping = aes(x = eval(parse(text = PClim1)), y = eval(parse(text = PClim2))),
                                method = "lm", se = F, span = 1000, size = 0.7, linetype = "dashed", colour = "grey20",
                                formula = y ~ x)  
    
    Add.r2 <- stat_poly_eq(data = MC.samp, 
                           label.y = R2.y, label.x = R2.x, 
                           size = 3, small.r = F, vstep = 0.07, p.digits = 1, na.rm = T,  
                           aes(x = eval(parse(text = PClim1)), y = eval(parse(text = PClim2)),
                               label =  sprintf("%s*\", \"*%s" ,
                                                after_stat(rr.label),
                                                # after_stat(r.squared),
                                                after_stat(p.value.label)
                               )))
  }
  else{
    Add.r2 <- NULL
    Add.reg.line <- NULL
    
  }
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Same matrice 1 et 2 ####
  if(Same.mat == F){
    if(Emprise.bin == F){
      Point.grey <- geom_point(data = MC.area, mapping = aes(x = eval(parse(text = PClim1)), y = eval(parse(text = PClim2))), 
                               size = Emprise.size, shape = 15, alpha = Emprise.alpha, colour = "grey70")}
    else{Point.grey <- geom_hex(data = MC.area, mapping = aes(x = eval(parse(text = PClim1)), y = eval(parse(text = PClim2)), 
                                                              fill = Biome),
                                bins = Emprise.size, 
                                alpha = Emprise.alpha, fill = "grey70"
    )}
    
  } 
  else{Point.grey <- NULL}
  
  #### PLOT ####
  p <- ggplot()+
    Point.grey + 
    stat_density_2d(data = MC.samp, mapping = aes(x = eval(parse(text = PClim1)), y = eval(parse(text = PClim2)), alpha = ..level.., fill = Biome),
                    geom = "polygon", #colour = "grey90",
                    bins = 8) +
    geom_point(data = MC.samp, mapping = aes(x = eval(parse(text = PClim1)), y = eval(parse(text = PClim2)), fill = Biome), size = 1.5, shape = 21, alpha = 1, colour = "grey40")+
    scale_fill_manual(values = Col.vec, name = "Biomes (Dinerstein et al., 2017)")+
    Add.r2 + Add.reg.line + 
    scale_alpha_continuous(limits=c(0,0.6)) +
    scale_color_manual(values = Col.vec, name = "Biomes (Dinerstein et al., 2017)")+
    guides(colour = FALSE, alpha = F)+
    xlab(PClim1)+
    ylab(PClim2)+
    Lim.x +
    Lim.y +
    #### Theme ####
  theme(
    axis.line= element_blank(),
    axis.ticks.x.bottom = element_line(colour = "grey"),
    panel.border = element_rect(fill = NA, colour = "grey"),
    legend.title = element_text(),
    legend.key = element_blank(),
    legend.justification = c("center"),               # left, top, right, bottom
    legend.text = element_text(size = 8),
    panel.background = element_blank(),
    panel.spacing = unit(0.7, "lines"),
    strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
    strip.placement = "outside",
    # legend.position = "none",
    legend.position = Leg.pos,
    strip.background = element_rect(color = "white", fill = "white"),
    plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm")
  )
  
  
  #### Density plots up ####
  plot_top <- ggplot(MC.samp, aes(x = eval(parse(text = PClim1)), fill = Biome)) + 
    geom_density(alpha = 0.6, size = 0.2) +
    Mean.line1 +
    Lim.up +
    scale_color_manual(values = Col.vec) + 
    scale_fill_manual(values = Col.vec)+
    #### Theme ####
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(), axis.text.y = element_text(hjust = 1, size = 6),
    axis.ticks.x.bottom = element_blank(),
    axis.title = element_blank(),
    axis.line.y = element_line(colour = "grey"),
    axis.ticks.y = element_line(colour = "grey"),
    #panel.border = element_rect(fill = NA, colour = "grey"),
    legend.title = element_text(),
    legend.key = element_blank(),
    legend.justification = c("center"),               # left, top, right, bottom
    legend.text = element_text(size = 8),
    panel.background = element_blank(),
    panel.spacing = unit(0.7, "lines"),
    legend.position = "none",
    strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
    strip.placement = "outside",
    strip.background = element_rect(color = "white", fill = "white"),
    plot.margin=unit(c(0,0,0,0),"cm")
  )
  
  #### Density plots right ####
  plot_right <- ggplot(MC.samp, aes(x = eval(parse(text = PClim2)), fill = Biome)) + 
    geom_density(alpha = 0.6, size = 0.2) +
    Mean.line2 +
    Lim.right +
    coord_flip() + 
    scale_color_manual(values = Col.vec) + 
    scale_fill_manual(values = Col.vec)+
    #### Theme ####
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.line.x = element_line(colour = "grey"),
    axis.ticks.x = element_line(colour = "grey"),
    legend.title = element_text(),
    legend.key = element_blank(),
    legend.justification = c("center"),               # left, top, right, bottom
    legend.position = "none",
    legend.text = element_text(size = 8),
    panel.background = element_blank(),
    panel.spacing = unit(0.7, "lines"),
    strip.text.x = element_text(size = 12, angle = 0, face = "bold"),
    strip.placement = "outside",
    strip.background = element_rect(color = "white", fill = "white"),
    plot.margin=unit(c(0,0,0,0),"cm")
  )
  
  
  #### Export ####
  if(Show.density == T){
    layout <- "AAAAAAA#
               CCCCCCCB
               CCCCCCCB
               CCCCCCCB
               CCCCCCCB
               CCCCCCCB
               "
    pfull <- plot_top + plot_right + p + plot_layout(design = layout)
    print(pfull)
  }
  if(Show.density == F){
    pfull <- p
    print(pfull)}
  
  
  if(is.null(Save.plot) == F){dev.off()}
  return(pfull)
}


Merge.local.global.trait <- function(Pollen, Trait.ACA.pol, Trait.ACA.veg = NULL, Trait.local = NULL, 
                                     Type = "ss", Gap.filling = T, Display.warning = T) {
  #### Select the good tables ####
  Trait.ACA.pol <- Trait.ACA.pol[names(Trait.ACA.pol) == Type][[1]]
  
  if(Gap.filling == F){Trait.ACA.veg <- Trait.ACA.veg[!grepl("gf", names(Trait.ACA.veg))][[1]]}
  if(Gap.filling == T){Trait.ACA.veg <- Trait.ACA.veg[grepl("gf", names(Trait.ACA.veg))][[1]]}
  
  if(Type == "ss"){
    if(Gap.filling == F){Trait.local <- Trait.local[Trait.local$Rank == "PT_ss", c(2,4:ncol(Trait.local))]}
    # if(Gap.filling == T){Trait.local <- Trait.local[Trait.local$Rank == "PT_ss", c(2,4:ncol(Trait.local))]
    if(Gap.filling == T){Trait.local <- Trait.local[Trait.local$Rank %in% c("PT_ss", "genus", "family", "species"), c(2,4:ncol(Trait.local))]
    }
  }
  if(Type == "sl"){
    if(Gap.filling == F){Trait.local <- Trait.local[Trait.local$Rank == "PT_sl", c(2,4:ncol(Trait.local))]}
    # if(Gap.filling == T){Trait.local <- Trait.local[Trait.local$Rank == "PT_sl", c(2,4:ncol(Trait.local))]}
    if(Gap.filling == T){Trait.local <- Trait.local[Trait.local$Rank %in% c("PT_sl", "family"), c(2,4:ncol(Trait.local))]}
  }
  
  #### Clean tables ####
  Trait.ACA.pol$species[!grepl("eae$", Trait.ACA.pol$species) & !grepl(" ", Trait.ACA.pol$species)] <- paste(Trait.ACA.pol$species[!grepl("eae$", Trait.ACA.pol$species) & !grepl(" ", Trait.ACA.pol$species)], "spp.")
  Trait.ACA.pol$species[grep("Cerealia", Trait.ACA.pol$species)] <- "Cerealia"
  Trait.ACA.veg$species[!grepl("eae$", Trait.ACA.veg$species) & !grepl(" ", Trait.ACA.veg$species)] <- paste(Trait.ACA.veg$species[!grepl("eae$", Trait.ACA.veg$species) & !grepl(" ", Trait.ACA.veg$species)], "spp.")
  Trait.ACA.veg$species[grep("Cerealia", Trait.ACA.veg$species)] <- "Cerealia"
  names(Trait.ACA.pol) <- paste("TRY", names(Trait.ACA.pol), sep = "_")
  names(Trait.ACA.veg) <- paste("TRY", names(Trait.ACA.veg), sep = "_")
  names(Trait.ACA.pol)[names(Trait.ACA.pol) == "TRY_species"] <- "taxa"
  names(Trait.ACA.veg)[names(Trait.ACA.veg) == "TRY_species"] <- "taxa"
  
  #### Merge tables ####
  Trait.tab <- data.frame(Trait.local[Trait.local$taxa %in% row.names(Pollen),])
  Trait.tab.2 <- Trait.ACA.pol[Trait.ACA.pol$taxa %in% row.names(Pollen) & ! Trait.ACA.pol$taxa %in% Trait.tab$taxa,]
  Trait.tab.3 <- Trait.ACA.veg[Trait.ACA.veg$taxa %in% row.names(Pollen) & ! Trait.ACA.veg$taxa %in% Trait.tab$taxa & ! Trait.ACA.veg$taxa %in% Trait.tab.2$taxa,]
  
  if(length(Trait.tab.2) > 0){Trait.tab <- suppressMessages(full_join(Trait.tab, Trait.tab.2))}
  if(length(Trait.tab.3) > 0){Trait.tab <- suppressMessages(full_join(Trait.tab, Trait.tab.3))}
  
  Trait.tab <- Trait.tab[!duplicated(Trait.tab),]
  if(Display.warning == T & length(row.names(Pollen)[! row.names(Pollen) %in% Trait.tab$taxa]) > 0){print(row.names(Pollen)[! row.names(Pollen) %in% Trait.tab$taxa])}
  return(Trait.tab)
}
