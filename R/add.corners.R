add.corners <- function(row) {
  corners <- find.corners(Easting = row$Easting,
                          Northing = row$Northing,
                          Heading = row$Heading, 
                          Distance = row$Distance,
                          SwathWidth = row$SwathWidth)
  row$FRx <- corners["FR", "Easting"]
  row$FRy <- corners["FR", "Northing"]
  row$FLx <- corners["FL", "Easting"]
  row$FLy <- corners["FL", "Northing"]
  
  row$BRx <- corners["BR", "Easting"]
  row$BRy <- corners["BR", "Northing"]
  row$BLx <- corners["BL", "Easting"]
  row$BLy <- corners["BL", "Northing"]
  return(list(Corners = corners,
              Row = row))
}