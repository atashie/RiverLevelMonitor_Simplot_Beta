portfolioTableMaker_f = function(customerGageData = customerGageData, customerInputTable = customerInputTable) {
    portfolioTable = data.frame(
      Location = customerInputTable$Location_Name, 
      Raw_Value = NA,
      Current_Pct_Annual = NA,
      Current_Pct_Season = NA,
      Last_Year_Pct = NA,
      Two_Years_Ago_Pct = NA,
      Stage = NA,
      Last_Year = NA,
      Two_Years_Ago = NA,
      High = customerInputTable$High_Level_ft, 
      Low = customerInputTable$Low_Level_ft,
      USGS_Gage_ID = customerGageData[[length(customerGageData)]]
    ) 
    
    
    for(thisGage in 1:(length(customerGageData) - 1)){
      theseData = customerGageData[[thisGage]]
      lastNoNaDay = data.table::last(which(!is.na(theseData$thisYear)))
      portfolioTable$Raw_Value[thisGage] = theseData$thisYear[lastNoNaDay]
      
      quantSeq_tot = rep(NA, 100)
      quantSeq_tot[c(1,11,26,51,76,91,100)] = as.numeric(theseData[366, c(7,2,3,4,5,6,8)])
      quantSeq_tot = zoo::na.fill(quantSeq_tot, 'extend')
      portfolioTable$Current_Pct_Annual[thisGage] = which.min(abs(theseData$thisYear[lastNoNaDay] - quantSeq_tot))
      
      quantSeq_seas = rep(NA, 100)
      quantSeq_seas[c(1,11,26,51,76,91,100)] = as.numeric(theseData[lastNoNaDay, c(7,2,3,4,5,6,8)])
      quantSeq_seas = zoo::na.fill(quantSeq_seas, 'extend')
      portfolioTable$Current_Pct_Season[thisGage] = which.min(abs(theseData$thisYear[lastNoNaDay] - quantSeq_seas))
      portfolioTable$Last_Year_Pct[thisGage] = which.min(abs(zoo::na.fill(theseData$lastYear, 'extend')[lastNoNaDay] - quantSeq_seas))
      portfolioTable$Two_Years_Ago_Pct[thisGage] = which.min(abs(zoo::na.fill(theseData$lastYear2, 'extend')[lastNoNaDay] - quantSeq_seas))
    

      portfolioTable$Stage[thisGage] = round(theseData$thisYear[lastNoNaDay], 1)
      portfolioTable$Last_Year[thisGage] = round(zoo::na.fill(theseData$lastYear, 'extend')[lastNoNaDay], 1)
      portfolioTable$Two_Years_Ago[thisGage] = round(zoo::na.fill(theseData$lastYear2, 'extend')[lastNoNaDay], 1)

  }


 
  
  
 return(portfolioTable)
}