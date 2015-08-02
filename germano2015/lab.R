buildLabTable <- function(reftime) {
  tformat <- "%m/%d/%Y %H:%M:%S"
  
  labWeeks <- paste(c("01/12/2015", "01/19/2015", "01/26/2015",
                      "02/02/2015", "02/09/2015", "02/16/2015",
                      "02/23/2015", "03/02/2015", "03/16/2015",
                      "03/23/2015", "03/30/2015", "04/06/2015",
                      "04/13/2015", "04/20/2015"),
                    "12:00:00")
  
  labLabels <- c("Th 2:00PM - 2:50PM",  "Th 3:00PM - 3:50PM", "Th 4:00PM - 4:50PM",
                 "Fr 11:00AM - 11:50AM", "Fr 12:00PM - 12:50PM", "Fr 1:00PM - 1:50PM",
                 "Fr 2:00PM - 2:50PM", "Fr 3:00PM - 3:50PM","Fr 4:00PM - 4:50PM")
  
  labStart <- c("01/15/2015 14:00:00", "01/15/2015 15:00:00", "01/15/2015 16:00:00",
                "01/16/2015 11:00:00", "01/16/2015 12:00:00", "01/16/2015 13:00:00",
                "01/16/2015 14:00:00", "01/16/2015 15:00:00", "01/16/2015 16:00:00")
  
  labGrid <- sapply(labWeeks, function(lw)
    as.numeric(strptime(lw, tformat) + (strptime(labStart, tformat) - reftime) - reftime))
  rownames(labGrid) <- labLabels
  labGrid
}

