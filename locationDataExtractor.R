locationDataExtractor_f = function(
  customerInputTable = customerInputTable, 
  customerInputTable_sf = customerInputTable_sf, 
  availableGages_Q = availableGages_Q, 
  mapData = mapData){
  
    # static vars
  thisYear = lubridate::year(Sys.Date())
  lastYear = thisYear - 1
  lastYear2 = thisYear - 2
    # initializing list
  gageDoyAvgs_ls = list()
  allGageIDs = NULL
#  gageAvgs = data.frame(STAID = NA, Q10 = NA, Q25 = NA, Q50 = NA, Q75 = NA, Q90 = NA, min = NA, max = NA)
  
  
  for(thisRow in 1:nrow(customerInputTable))  { 
    # check to see if we are using USGS or bespoke (e.g., usace) data
    if(customerInputTable$Use_USGS[thisRow]){
      # check to see is the customer has chosen a usgs gage
      if(!is.na(customerInputTable$Gage_ID[thisRow])){
        customerGage = strsplit(customerInputTable$Gage_ID[thisRow],"_")[[1]][2]
      } else {
        # if not we choose the nearest river reach
        nearestRiverReach = sf::st_nearest_feature(customerInputTable_sf[thisRow,], mapData)
        if(!is.na(mapData$Annual_Avg_Pct[nearestRiverReach])) {
          gageLocation = mapData$nearestNeighbor_Q[nearestRiverReach]
          customerGage = availableGages_Q$site_no[gageLocation]
        } else {
          # but if that river reach has null data, then we search for the nearest neighbor usgs gage
          thisRiverReach = which.min(sqrt(
            (availableGages_Q$dec_lat_va - customerInputTable$Lat[thisRow])^2 + 
              (availableGages_Q$dec_long_va - customerInputTable$Lon[thisRow])^2))
          customerGage = availableGages_Q$site_no[thisRiverReach]
        }
      }
      
      thisData = dataRetrieval::readNWISdv(siteNumber = customerGage,
                                           parameterCd = '00060', #00065 stage; 00060 is Q
                                           startDate = '1980-01-01')
      thisData$year = lubridate::year(thisData$Date)
      
      # and now we double check to ensure that the selected usgs gage actually has recent data
      if(all(is.na(subset(thisData, year == thisYear)[,4]))) {
        print("oops, usgs gage is empty this year")
        badData = TRUE
        iter = 2
        while(badData)  {
          thisRiverReach = order(sqrt(
            (availableGages_Q$dec_lat_va - customerInputTable$Lat[thisRow])^2 + 
              (availableGages_Q$dec_long_va - customerInputTable$Lon[thisRow])^2))[iter]
          customerGage = availableGages_Q$site_no[thisRiverReach]
          
          thisData = dataRetrieval::readNWISdv(siteNumber = customerGage,
                                               parameterCd = '00060', #00065 stage; 00060 is Q
                                               startDate = '1980-01-01')
          thisData$year = lubridate::year(thisData$Date)
          if(any(!is.na(subset(thisData, year == thisYear)[,4]))) {badData = FALSE}
          iter = iter + 1
        }
      }
      
    } else {
      # for use cases where the customer should rely on alternative (e.g., usace) data instead of live usgs api pulls
      usaceData = data.table::fread(paste0("./Data/", customerInputTable$File_name[thisRow], ".csv"), data.table=FALSE)
      usaceData$Date = as.Date(lubridate::mdy_hm(usaceData$Date))
      thisData = usaceData[order(usaceData$Date), c("Date", "Stage")]
      thisData$year = lubridate::year(thisData$Date)
      # bad coding practice: setting stage data to column for to conform to previous workflow
      thisData$stageCol4 = as.numeric(thisData$Stage)

      customerGage = customerInputTable$NWS_Gage_Name[thisRow]
   }
    
   allGageIDs = c(allGageIDs, customerGage)    
  
    # ensuring there is a long record of data for quantile analysis
    #  if(length(!is.na(thisData$X_00060_00003)) > 365 * 10) {
    if(any(!is.na(thisData$year %in% thisYear))) {
      thisData$DOY = lubridate::yday(thisData$Date)
      
      justNAs = rep(NA, 365)
      dataDoyQuantilesAndRecent = data.frame(DOY = justNAs, Q10 = justNAs, Q25 = justNAs, Q50 = justNAs, Q75 = justNAs, Q90 = justNAs, min = justNAs, max = justNAs)
      #    dataDoyQuantilesAndRecent = data.table::as.data.table(matrix(NA, nrow = 365, ncol = 8))
      for(j in 1:365) {
        # looping doy for smoothing data
        loopedDoys = seq(j - 30, j + 30, 1)
        # catching days at beginning / end of season
        if(any(loopedDoys <= 0)) {
          loopedDoys[which(loopedDoys <= 0)] = loopedDoys[which(loopedDoys <= 0)] + 365
        }
        if(any(loopedDoys > 365)) {
          loopedDoys[which(loopedDoys > 365)] = loopedDoys[which(loopedDoys > 365)] - 365
        }
        
        dataDoyQuantilesAndRecent[j, ] = c(j,
                                           quantile(subset(thisData, DOY %in% loopedDoys)[,4],
                                                    c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
      }
#      gageAvgs[thisRow, ] = c(customerGage, 
#                        #                      quantile(thisData$X_00065_00003, c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
#                        quantile(thisData[,4], c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
      
      
      thisYearDF = data.frame(DOY = thisData[which(thisData$year == thisYear)[1:365], "DOY"], thisYear = thisData[which(thisData$year == thisYear)[1:365], 4]) 
      lastYearDF = data.frame(DOY = thisData[which(thisData$year == lastYear)[1:365], "DOY"], lastYear = thisData[which(thisData$year == lastYear)[1:365], 4]) 
      lastYear2DF = data.frame(DOY = thisData[which(thisData$year == lastYear2)[1:365], "DOY"], lastYear2 = thisData[which(thisData$year == lastYear2)[1:365], 4]) 
      
      dataDoyQuantilesAndRecent = merge(dataDoyQuantilesAndRecent, thisYearDF, by = "DOY", all.x=TRUE)
      dataDoyQuantilesAndRecent = merge(dataDoyQuantilesAndRecent, lastYearDF, by = "DOY", all.x=TRUE)
      dataDoyQuantilesAndRecent = merge(dataDoyQuantilesAndRecent, lastYear2DF, by = "DOY", all.x=TRUE)
      
      dataDoyQuantilesAndRecent[366, ] = c(NA,
                                         quantile(thisData[ , 4],
                                                  c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE),
                                         NA, NA, NA)
      
      
      # compiling the data
      gageDoyAvgs_ls[[thisRow]] = dataDoyQuantilesAndRecent
    }
  }
  
  gageDoyAvgs_ls[[thisRow + 1]] = allGageIDs
  return(gageDoyAvgs_ls)
}