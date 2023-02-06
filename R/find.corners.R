find.corners <- function(Easting, Northing, Heading, Distance, SwathWidth) {
  ret = data.frame(Y = rep(0,4), X = rep(0,4))
  row.names(ret) = c("FR", "FL", "BR", "BL")
  if(0<=Heading && Heading < 90) {
    #Quadrant A
    DistanceAngle <- Heading*pi/180
    SwathAngle <- (Heading+90)*pi/180
    
    DistanceNorthing <- cos(DistanceAngle)*Distance
    DistanceEasting <- sin(DistanceAngle)*Distance
    SwathNorthing <- -cos(SwathAngle)*SwathWidth/2
    SwathEasting <- sin(SwathAngle)*SwathWidth/2
    
    ret["FR", "Easting"] = Easting + SwathEasting 
    ret["FR", "Northing"] = Northing - SwathNorthing
    
    ret["FL", "Easting"] = Easting - SwathEasting 
    ret["FL", "Northing"] = Northing + SwathNorthing
    
    ret["BR", "Easting"] = Easting + SwathEasting - DistanceEasting
    ret["BR", "Northing"] = Northing - SwathNorthing - DistanceNorthing 
    
    ret["BL", "Easting"] = Easting - SwathEasting - DistanceEasting
    ret["BL", "Northing"] = Northing + SwathNorthing - DistanceNorthing 
    
  } else if(90 <= Heading && Heading < 180 ) {
    #Quadrant B
    DistanceAngle <- Heading*pi/180
    SwathAngle <- (Heading+90)*pi/180
    DistanceNorthing <- -cos(DistanceAngle)*Distance
    DistanceEasting <- sin(DistanceAngle)*Distance
    SwathNorthing <- cos(SwathAngle)*SwathWidth/2
    SwathEasting <- -sin(SwathAngle)*SwathWidth/2
    
    ret["FR", "Easting"] = Easting - SwathEasting 
    ret["FR", "Northing"] = Northing + SwathNorthing
    
    ret["FL", "Easting"] = Easting + SwathEasting 
    ret["FL", "Northing"] = Northing - SwathNorthing
    
    ret["BR", "Easting"] = Easting - SwathEasting - DistanceEasting
    ret["BR", "Northing"] = Northing + SwathNorthing + DistanceNorthing 
    
    ret["BL", "Easting"] = Easting + SwathEasting - DistanceEasting
    ret["BL", "Northing"] = Northing - SwathNorthing + DistanceNorthing 
  } else if(180 <= Heading && Heading < 270) {
    #Quadrant C
    DistanceAngle <- Heading*pi/180
    SwathAngle <- (Heading+90)*pi/180
    DistanceNorthing <- -cos(DistanceAngle)*Distance
    DistanceEasting <- -sin(DistanceAngle)*Distance
    SwathNorthing <- cos(SwathAngle)*SwathWidth/2
    SwathEasting <- -sin(SwathAngle)*SwathWidth/2
    
    ret["FR", "Easting"] = Easting + SwathEasting 
    ret["FR", "Northing"] = Northing - SwathNorthing
    
    ret["FL", "Easting"] = Easting - SwathEasting 
    ret["FL", "Northing"] = Northing + SwathNorthing
    
    ret["BR", "Easting"] = Easting + SwathEasting + DistanceEasting
    ret["BR", "Northing"] = Northing - SwathNorthing + DistanceNorthing 
    
    ret["BL", "Easting"] = Easting - SwathEasting + DistanceEasting
    ret["BL", "Northing"] = Northing + SwathNorthing + DistanceNorthing 
  } else {
    #Quadrant D
    DistanceAngle <- Heading*pi/180
    SwathAngle <- (Heading-90)*pi/180
    DistanceNorthing <- cos(DistanceAngle)*Distance
    DistanceEasting <- -sin(DistanceAngle)*Distance
    SwathNorthing <- -cos(SwathAngle)*SwathWidth/2
    SwathEasting <- -sin(SwathAngle)*SwathWidth/2
    
    ret["FR", "Easting"] = Easting - SwathEasting 
    ret["FR", "Northing"] = Northing - SwathNorthing
    
    ret["FL", "Easting"] = Easting + SwathEasting 
    ret["FL", "Northing"] = Northing + SwathNorthing
    
    ret["BR", "Easting"] = Easting - SwathEasting + DistanceEasting
    ret["BR", "Northing"] = Northing - SwathNorthing - DistanceNorthing
    
    ret["BL", "Easting"] = Easting + SwathEasting + DistanceEasting
    ret["BL", "Northing"] = Northing + SwathNorthing - DistanceNorthing 
  }
  return(ret)
}