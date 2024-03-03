portfolioTablePlotter_f = function(portfolioTable = portfolioTable) {
  df = DT::datatable(portfolioTable,
                     options=list(
                       pageLength=100,
                       buttons = list('copy', 
                                      list(
                                        extend = 'csv', 
                                        filename = "riverLevels"
                                      ),
                                      list(
                                        extend = 'excel',
                                        filename = 'riverLevels',
                                        title = NULL
                                      )
                       ),
                       dom = "Bfrtip",
                       filename = "riverLevels",
                       scrollX = TRUE,
                       scrollY = "350px"#,
                       # columnDefs = list(list(targets=c(2,3,4,5,7,9), visible=FALSE))
                     ),
                     extensions="Buttons",
                     selection = "single",
                     rownames = TRUE#,
                     #              server = FALSE # change to TRUE if the datatable function is slowing down the entire page
  ) 
  df# %>% DT::formatStyle(
  #c("Local","Regional"), c("Local_Direction", "Regional_Direction"),
  #backgroundColor = styleEqual(c(-1,0,1), c("tomato","beige","royalblue")),
  #fontSize = "150%",
  #fontweight = "bold"
  #)
}
