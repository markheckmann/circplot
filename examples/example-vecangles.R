v <- c(1,0)   # point vector

rad <- seq(0, 2*pi, length=10)    # radians to rotate
m <- cbind(cos(rad), sin(rad))    # 2d m matrix 

# mode 1: between 0 and 180 degrees, minimal angle / radian
vec_angles(v, c(1,0))
vec_angles(m, c(1,0))

# mode 2: between 0 and 360 degrees, anti-clockwise 
vec_angles(v, c(1,0), mode=2)
vec_angles(m, c(1,0), mode=2)

