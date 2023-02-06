#p is a pair of values representing `Northing` and `Easting`
#corners is a data frame defining the four corners of a rectangle, with columns `Northing` and `Easting`
point.in.rectangle <- function(NewPoint, CurrentRow) {
  #P(x,y), and rectangle A(x1,y1), B(x2,y2), C(x3,y3), D(x4,y4).
  
  NewPoint.x = NewPoint[1]
  NewPoint.y = NewPoint[2]
  
  A.x = CurrentRow$FRx
  A.y = CurrentRow$FRy
  B.x = CurrentRow$FLx
  B.y = CurrentRow$FLy
  C.x = CurrentRow$BRx
  C.y = CurrentRow$BRy
  D.x = CurrentRow$BLx
  D.y = CurrentRow$BLy
  
  #Calculate the sum of areas of APD, DPC, CPB, PBA.
  APD = triangle.area(c(A.x,A.y), NewPoint, c(D.x,D.y))
  DPC = triangle.area(c(D.x,D.y), NewPoint, c(C.x,C.y))
  CPB = triangle.area(c(C.x,C.y), NewPoint, c(B.x,B.y))
  PBA = triangle.area(c(B.x,B.y), NewPoint, c(A.x,A.y))
  Triangle.Sums = sum(APD, DPC, CPB, PBA)
  if(Triangle.Sums < CurrentRow$Distance * CurrentRow$SwathWidth) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

triangle.area <- function(A,B,C) {
  Ax = A[1]
  Ay = A[2]
  Bx = B[1]
  By = B[2]
  Cx = C[1]
  Cy = C[2]
  return (abs((Bx * Ay - Ax * By) + (Cx * By - Bx * Cy) + (Ax * Cy - Cx * Ay) ) / 2)
}