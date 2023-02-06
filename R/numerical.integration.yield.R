numerical.integration.yield <- function(point, gam.model, nsims=1000, max_tries=10000) {
  minx = min(point[,c("FRx","FLx","BRx","BLx")])
  miny = min(point[,c("FRy","FLy","BRy","BLy")])
  maxx = max(point[,c("FRx","FLx","BRx","BLx")])
  maxy = max(point[,c("FRy","FLy","BRy","BLy")])
  
  Estimates  = data.frame(Easting = rep(NA,nsims),
                          Northing = rep(NA, nsims))
  idx=1
  tries = 1
  while (idx <= nsims) {
    if(tries>max_tries) break;
    newx = runif(1, minx,maxx)
    newy = runif(1, miny,maxy)
    if(point.in.rectangle(c(newx,newy), point)) {
      Estimates$Easting[idx] = newx
      Estimates$Northing[idx] =  newy
      idx=idx+1
    }
    tries = tries+ 1
  }
  Estimates = Estimates[!is.na(Estimates$Easting),]
  Estimates$Estimate = predict(gam.model, newdata=Estimates)
  return(list(Estimates=Estimates, 
              Estimate = mean(Estimates$Estimate),
              N= dim(Estimates)[1]))
}