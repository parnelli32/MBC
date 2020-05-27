### Montgomery County FOP CPI Analysis, 25 Year Perspective

### Notes: Series refers to cpi series being pulled; Month refers to the last month in client fiscal year

workforce_cpi_25 <- function(gwas, cpi.series, month) {
  
  ### Set-up  
  require(devtools)
  require(blsAPI)
  require(rjson)
  require(ggplot2)
  require(scales)
  
  ### List the CPI series that I'd like to examine
  relevant.series <- cpi.series                               ### single or multiple CPI series can be pulled
  
  ### Generate today's year
  year.today <- lubridate::year(Sys.Date())               ### Look into making the sys.date() the default and inputting a start and end year parameter
  
  ### Download the CPI data from the BLS API.  Note: BLS can only pull 10 years worth of data at a time.
  payload.1 <- list('seriesid' = relevant.series, 'startyear' = year.today - 9, 'endyear' = year.today + 1, 
                    'registrationKey' = 'de1da84d964e475bb4c04ec71b4895f4')
  payload.1 <- fromJSON(blsAPI(payload.1, 2))
  
  payload.2 <- list('seriesid' = relevant.series, 'startyear' = year.today - 20, 'endyear' = year.today - 10,
                    'registrationKey' = 'de1da84d964e475bb4c04ec71b4895f4')
  payload.2 <- fromJSON(blsAPI(payload.2, 2))
  
  payload.3 <- list('seriesid' = relevant.series, 'startyear' = year.today - 30, 'endyear' = year.today - 21,
                    'registrationKey' = 'de1da84d964e475bb4c04ec71b4895f4')
  payload.3 <- fromJSON(blsAPI(payload.3, 2))
  
  payload <<- list("first" = payload.1, "second" = payload.2, "third" = payload.3)
  rm(payload.1, payload.2, payload.3)
  
  ### Combine the data into vectors by year, month, and value.  After, organize vectors in data frame
  year.chain.cpi <- c()      ### Make change to vector names because the first object might not always be chained cpi
  month.chain.cpi <- c()
  value.chain.cpi <- c()
  
  for (a in 1:3) {
    
    for (b in 1:length(payload[[a]]$Results$series[[1]]$data)) {
      
      boom <- length(year.chain.cpi)
      year.chain.cpi[boom+1] <- payload[[a]]$Results$series[[1]]$data[[b]]$year
      month.chain.cpi[boom+1] <- payload[[a]]$Results$series[[1]]$data[[b]]$periodName
      value.chain.cpi[boom+1] <- payload[[a]]$Results$series[[1]]$data[[b]]$value
      
    }
  }
  rm(a,b)
  
  chain.cpi <- tibble::tibble(year.chain.cpi, month.chain.cpi, value.chain.cpi)
  
  ### Filter data frame to only contain data from a particular month
  ### Note: the chosen month should correspond with the last month in the client's fiscal year
  revised.chain.cpi <- chain.cpi[chain.cpi$month.chain.cpi == month & chain.cpi$year.chain.cpi > (year.today -26),]
  revised.chain.cpi <- revised.chain.cpi[nrow(revised.chain.cpi):1,]
  
  
  ### Combine the data into vectors by year, month, and value.  After, organize vectors in data frame
  year.cpi.w <- c()
  month.cpi.w <- c()
  value.cpi.w <- c()
  
  for (a in 1:3) {
    
    for (b in 1:length(payload[[a]]$Results$series[[2]]$data)) {
      
      boom.2 <- length(year.cpi.w)
      year.cpi.w[boom.2+1] <- payload[[a]]$Results$series[[2]]$data[[b]]$year
      month.cpi.w[boom.2+1] <- payload[[a]]$Results$series[[2]]$data[[b]]$periodName
      value.cpi.w[boom.2+1] <- payload[[a]]$Results$series[[2]]$data[[b]]$value
      
    }
  }
  rm(a,b)
  
  cpi.w <- tibble::tibble(year.cpi.w, month.cpi.w, value.cpi.w)
  
  ### Filter data frame to only contain data from a particular month
  ### Note: the chosen month should correspond with the last month in the client's fiscal year
  revised.cpi.w <- cpi.w[cpi.w$month.cpi.w == month & cpi.w$year.cpi.w > (year.today - 26),]
  revised.cpi.w <- revised.cpi.w[nrow(revised.cpi.w):1,]
  
  
  ### Combine the data into vectors by year, month, and value.  After, organize vectors in data frame
  year.region.cpi.w <- c()
  month.region.cpi.w <- c()
  value.region.cpi.w <- c()
  
  for (a in 1:3) {
    
    for (b in 1:length(payload[[a]]$Results$series[[3]]$data)) {
      
      boom.3 <- length(year.region.cpi.w)
      year.region.cpi.w[boom.3+1] <- payload[[a]]$Results$series[[3]]$data[[b]]$year
      month.region.cpi.w[boom.3+1] <- payload[[a]]$Results$series[[3]]$data[[b]]$periodName
      value.region.cpi.w[boom.3+1] <- payload[[a]]$Results$series[[3]]$data[[b]]$value
      
    }
  }
  rm(a,b)
  
  region.cpi.w <- tibble::tibble(year.region.cpi.w, month.region.cpi.w, value.region.cpi.w)
  
  ### Filter data frame to only contain data from a particular month
  ### Note: the chosen month should correspond with the last month in the client's fiscal year
  revised.region.cpi.w <- region.cpi.w[region.cpi.w$month.region.cpi.w == "May" & region.cpi.w$year.region.cpi.w > (year.today-26),]
  revised.region.cpi.w <- revised.region.cpi.w[nrow(revised.region.cpi.w):1,] 
  
  ### Create a vector of MontCo FOP GWAs (historical)
  montco.fop.gwas <- gwas
  
  ### Create a vector of "Fiscal Year" labels
  fiscal.year <- paste0("FY", as.character(revised.cpi.w$year.cpi.w), sep = "")
  new.year <- as.numeric(revised.cpi.w$year.cpi.w)
  
  ### Generate new table that holds raw CPI figures for the months and years in question
  final <- tibble::tibble(fiscal.year, montco.fop.gwas[1:length(fiscal.year)], 
                          as.numeric(revised.cpi.w$value.cpi.w), 
                          as.numeric(revised.region.cpi.w$value.region.cpi.w))
  colnames(final) <- c("FY", "raw.gwas", "raw.cpi.w", "raw.region.cpi.w")
  
  ### Create 3 new columns and make the first row values equal to 1.000
  final <- dplyr::mutate(final, indexed.gwas = final$raw.gwas/final$raw.gwas)
  final <- dplyr::mutate(final, indexed.cpi.w = as.numeric(final$raw.cpi.w)/as.numeric(final$raw.cpi.w))
  final <- dplyr::mutate(final, indexed.region.cpi.w = as.numeric(final$raw.region.cpi.w)/as.numeric(final$raw.region.cpi.w))
  
  ### Load in indexed values for the added 3 columns after the first row 
  for (c in 2:length(final$FY)) {
    
    final$indexed.gwas[c] <-  round((final$indexed.gwas[c-1] * (1 + final$raw.gwas[c])), digits = 3)
    final$indexed.cpi.w[c] <- round((final$raw.cpi.w[c]/final$raw.cpi.w[1]), digits = 3)
    final$indexed.region.cpi.w[c] <- round((final$raw.region.cpi.w[c]/final$raw.region.cpi.w[1]), digits = 3)
    
  }
  rm(c)
  
  ### Create 3 new columns and make the first row values equal to 0.000
  final <- dplyr::mutate(final, `FOP GWAs` = indexed.cpi.w - indexed.region.cpi.w)
  final <- dplyr::mutate(final, `National CPI-W` = indexed.cpi.w - indexed.region.cpi.w)
  final <- dplyr::mutate(final, `Regional CPI-W` = indexed.cpi.w - indexed.region.cpi.w)
  
  ### Load in compounded values for the added 3 columns after the first row 
  for (d in 2:length(final$FY)) {
    
    final$`FOP GWAs`[d] <-  round(((1+final$raw.gwas[d])*(1+final$`FOP GWAs`[d-1])-1), digits = 3)
    final$`National CPI-W`[d] <- round(((final$raw.cpi.w[d]/final$raw.cpi.w[1])-1), digits = 3)
    final$`Regional CPI-W`[d] <- round(((final$raw.region.cpi.w[d]/final$raw.region.cpi.w[1])-1), digits = 3)
    
  }
  rm(d)
  
  ### Return final table to Global Environment
  cpi.analysis <<- final
  
  ### Plot a Line Graph
  theme_set(theme_classic())
  ggplot(final[,c(1,8:10)], aes(x = new.year)) +
    geom_line(aes(y = final$`FOP GWAs`, col = names(final$`FOP GWAs`), color = "blue"), linetype = "dashed", size = 1.4) +
    geom_line(aes(y = final$`National CPI-W`, col = names(final$`National CPI-W`), color = "red"), size = 1.2) +
    geom_line(aes(y = final$`Regional CPI-W`, col = names(final$`Regional CPI-W`), color = "green"), size = 1.2) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks = new.year, labels = fiscal.year) +
    theme(axis.text.x = element_text(angle = 25)) +
    labs(title = "FOP General Wage Growth vs. CPI",
         subtitle = "25-Year Perspective",
         caption = "Source: Bureau of Labor Statistics",
         y = "Compounded Growth",
         x = "")
  
}
