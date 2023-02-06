find.neighborhood <- function(row, gps.data, Swath, Multiplier=2) {
  Easting.UL = row$Easting + Multiplier*Swath
  if(Easting.UL > max(gps.data$Easting)) Easting.UL <- max(gps.data$Easting)
  
  Northing.UL = row$Northing + Multiplier*Swath
  if(Northing.UL > max(gps.data$Northing)) Northing.UL <- max(gps.data$Northing)
  
  Easting.LL = row$Easting - Multiplier*Swath
  if(Easting.LL < min(gps.data$Easting)) Easting.LL <- min(gps.data$Easting)
  
  Northing.LL = row$Northing - Multiplier*Swath
  if(Northing.LL < min(gps.data$Northing)) Northing.LL <- min(gps.data$Northing)
  
  ret = gps.data[gps.data$Easting<Easting.UL,]
  ret = ret[ret$Easting>Easting.LL,]
  ret = ret[ret$Northing>Northing.LL,]
  ret = ret[ret$Northing<Northing.UL,]
  return(ret)
}