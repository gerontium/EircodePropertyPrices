rm(list = ls())
cat("\014")

hp <- read.csv("C:/Users/loughnge/Dropbox/Maps/Excel files/CSO House Value by Eircode.csv", 
                     header = FALSE, stringsAsFactors = FALSE)
hp <- hp[, 4:90]

# Want a data frame with Columns: 
# Eircode, Filings/Executions, Year, Month, 
# Vol Sales, Value Sales, Avg Sale Price, Median Sale Price

# Eircode names
eircode_names <- hp$V4
eircode_index <- which(nchar(eircode_names) > 1)
eircode_names <- unique(eircode_names[eircode_index])
eircode_names <- eircode_names[2:length(eircode_names)]

property_prices <- data.frame(Eircode=character(),
                              AreaName=character(),
                              FileExec=integer(),
                              Year=integer(),
                              Month=integer(),
                              VolSales=integer(),
                              ValSales=integer(),
                              AvgPrice=integer(),
                              MedPrice=integer(),
                              stringsAsFactors=FALSE)

passed_exec = 0
for (i in 1:length(eircode_index)) {
  print(i)
  if (hp$V4[eircode_index[i]] != 'All') {
    # get Eircode
    eircode_temp <- substr(hp$V4[eircode_index[i]], 1, 3)
    # get Name
    area_name_temp <- substr(hp$V4[eircode_index[i]], 6, nchar(hp$V4[eircode_index[i]]))
    # get FileExec
    if (passed_exec == 1) {
      file_exec_temp <- 1
    } else {
      file_exec_temp <- 2
    }
    for (j in 3:ncol(hp)) {
      year_temp <- as.numeric(substr(hp[4, j], 1, 4))
      month_temp <- as.numeric(substr(hp[4, j], 6, 7))
      vol_temp <- as.numeric(hp[eircode_index[i]+1, j])
      val_temp <- as.numeric(hp[eircode_index[i]+2, j])*1000000
      avg_temp <- as.numeric(hp[eircode_index[i]+3, j])
      med_temp <- as.numeric(hp[eircode_index[i]+4, j])
      df_temp <- data.frame(eircode_temp, area_name_temp, file_exec_temp, year_temp, month_temp, 
                            vol_temp, val_temp, avg_temp, med_temp, 
                            stringsAsFactors=FALSE)
      colnames(df_temp) <- 
        c('Eircode', 'AreaName', 'FileExec', 'Year', 'Month', 'VolSales', 'ValSales', 'AvgPrice', 'MedPrice')
      property_prices <- rbind(property_prices, df_temp)
    }
  } else {
    passed_exec = passed_exec+1
  }
}

# save data
save(property_prices, file = "C:/Users/loughnge/Dropbox/Maps/Eircode Prices.Rda")
