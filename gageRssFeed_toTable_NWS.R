gageRssFeed_f = function(customerInputTable = customerInputTable, userDataLocation = userDataLocation) {
  #	secsPerDay = 60*60*24
  theDate = Sys.Date()
  theDay = lubridate::day(theDate)
  theMonth = lubridate::month(theDate)
  theYear = lubridate::year(theDate)
  myStart = Sys.time()
  for(thisRow in 1:nrow(customerInputTable))	{
    if(!is.na(customerInputTable$File_name[thisRow])) {
      recentObserved = tidyRSS::tidyfeed(
        paste0("https://water.weather.gov/ahps2/rss/obs/",
               customerInputTable$NWS_Gage_Name[thisRow],
               ".rss")
      )
      recentValue_ugh = unlist(strsplit(unlist(recentObserved)[13], "Latest Observation: "))[2]
      recentValue = as.numeric(unlist(strsplit(recentValue_ugh, " ft"))[1])
      
      histVals = data.table::fread(paste0(
        "C:/Users/arik/Documents/GitHub/RiverLevelMonitor_Simplot_Beta/Data/",
        customerInputTable$File_name[thisRow],
        ".csv")
      )
      #		histVals$Date = lubridate::mdy_hm(histVals$Date)
      newVals = histVals[1,]
      newVals$Date = paste0(theMonth, "/", theDay, "/", theYear, " 8:00")
      newVals$Stage = recentValue
      concatVals = rbind(newVals, histVals)
      
      data.table::fwrite(concatVals, 
                         paste0(
                           userDataLocation,
                           customerInputTable$File_name[thisRow],
                           ".csv"
                         )
      )
    }
  }
  
  ###################### pulling in and processing USACE data
  #jj = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/obs/heea4.rss")
  #kk = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/fcst/heea4.rss")
  #ll = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/alert/heea4.rss")
}


