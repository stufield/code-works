# --------------------
# Fancy 3D Plots in R
# --------------------
# Stu Field
# --------------------
require(rgl)
require(emdbook)
# --------------------
demo(hist3d)
rgl.bg(col = "darkblue") # Choosing background
hist3d(rnorm(2500), rnorm(2500), alpha = 0.4, nclass = 7, scale = 30)

#####################
# 3d cloud for fun
# pretty cool with col = rainbow
#####################
require(scatterplot3d)
x <- rnorm(5000)
y <- rnorm(5000)
z <- rnorm(5000)
scatterplot3d(sort(x), y, z)
plot3d(sort(x), y, z, col = rainbow(5000))
plot3d(sort(x), y, z, col = rainbow(5000), type = "s", size = 0.5)

scatter3d(wt, disp, mpg)


# -------------------------
# Cool plot; Sinc function
# -------------------------
# demo(persp)
# -------------------------
x <- y <- seq(-10, 10, length = 50)

rotsinc <- function(x, y) {
  sinc <- function(x) {
    y <- sin(x) / x 
    y[is.na(y)] <- 1
    y
  }
  10 * sinc(sqrt(x^2 + y^2))
}
#sinc.exp <- expression(z == Sinc(sqrt(x^2 + y^2)))
z <- outer(x, y, rotsinc)

# -------------------------
# Make Plot using persp()
# -------------------------
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, ltheta = 0,
      shade = 0.4, col = "navy", axes = FALSE)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "navy",
      ltheta = 120, shade = 0.5, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Z")

# -------------------------
# Make Plot using curve3d()
# -------------------------
curve3d(outer(x, y, rotsinc), from = c(-10, -10), to = c(10, 10),
        zlab = "Z", ylab = "Y", xlab = "X", col = rainbow(100))
curve3d(outer(x, y, rotsinc), from = c(-10, -10), to = c(10, 10),
        zlab = "Z", ylab = "Y", xlab = "X", sys3d = "rgl", col = "navy")
curve3d(outer(x, y, rotsinc), from = c(-10, -10), to = c(10, 10),
        zlab = "", ylab = "", xlab = "", sys3d = "rgl", col = rainbow(40),
        axes = FALSE, box = FALSE)

# Save 3D image ###
rgl.snapshot(filename = "rgl_Image.png", fmt = "png")

