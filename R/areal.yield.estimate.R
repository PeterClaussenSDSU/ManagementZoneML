areal.yield.estimate <- function(row, dat, swathwidth) {
  neighborhood <- find.neighborhood(row$Row, dat, swathwidth)
  surface.gam <- NULL
  res <- NA
  tryCatch(surface.gam <- gam(Yield ~ s(Easting,Northing), data=neighborhood), error= function(e){e})
  if(!is.null(surface.gam)) {
    res = numerical.integration.yield(row$Row,surface.gam)$Estimate
  }
  return(res)
}