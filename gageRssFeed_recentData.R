gageRssFeed_recentData_f = function(customerInputTable = customerInputTable, userDataLocation = userDataLocation) {
	recentObserved = tidyRSS::tidyfeed(
		paste0("https://water.weather.gov/ahps2/rss/obs/",
		customerInputTable$NWS_Gage_Name[userDataLocation],
		".rss")
	)
	recentValue_ugh = unlist(strsplit(unlist(recentObserved)[13], "Latest Observation: "))[2]
	recentValue = as.numeric(unlist(strsplit(recentValue_ugh, " ft"))[1])

	 return(recentValue)
	###################### pulling in and processing USACE data
	#jj = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/obs/heea4.rss")
	#kk = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/fcst/heea4.rss")
	#ll = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/alert/heea4.rss")
}
		

