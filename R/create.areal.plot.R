create.areal.plot <- function(row, Multiplier=1) {
  res <- add.corners(row)
  base.plot <- ggplot(res$Row, aes(Easting, Northing)) + geom_point(color=cbPalette[6],size = 2)
  base.plot = base.plot+geom_point(data=res$Corners,color= cbPalette[4])
  #Leading edge
  base.plot <- base.plot + geom_segment(aes(x = FLx, xend = FRx, y = FLy, yend = FRy), 
                                        data = res$Row,color = cbPalette[1],linewidth = 2)
  #trailing edge
  base.plot <- base.plot + geom_segment(aes(x = BLx, xend = BRx, y = BLy, yend = BRy), 
                                        data = res$Row,color = cbPalette[3], linewidth = 1,linetype = "dotted")
  #left edge
  base.plot <- base.plot + geom_segment(aes(x = FLx, xend = BLx, y = FLy, yend = BLy), 
                                        data = res$Row,color = cbPalette[2],linewidth = 1)
  #Right edge
  base.plot <- base.plot + geom_segment(aes(x = FRx,  xend = BRx, y = FRy, yend = BRy), 
                                        data = res$Row,color = cbPalette[4],linewidth = 1)
  center = res$Row$Northing
  base.plot= base.plot + ylim(c(center-Multiplier*res$Row$SwathWidth,center+Multiplier*res$Row$SwathWidth))
  center = res$Row$Easting
  base.plot= base.plot + xlim(c(center-Multiplier*res$Row$SwathWidth,center+Multiplier*res$Row$SwathWidth))
  return(base.plot)
}