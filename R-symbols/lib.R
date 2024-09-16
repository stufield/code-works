
plotSymbols <- function(f, h = 7, w = 10, s = 1.2) {
  pdf(file = f, height = h * s, width = w * s, useDingbats = FALSE,
      title = basename(f))
  par(bg = "mistyrose2")

  # Titles #
  main_name <- expression(paste(plain(sin), " ", phi^2))
  xlab_name <- expression(paste(italic(vti), " ", Delta[3]))
  ylab_name <- expression(gamma^3*"  x  "*mu*"L")

  # Make Plot #
  plot(0, 0, type = "n", xlab = xlab_name, ylab = ylab_name, main = main_name,
      xlim = c(-pi, pi), ylim = c(-1.5, 1.5), axes = FALSE)
  on.exit(dev.off())
  box()
  usr <- par("usr")
  rect(usr[1L], usr[3L], usr[2L], usr[4L], col = "thistle")

  # Axes #
  axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
       labels = expression(-pi, -pi/2, 0, pi/2, pi))
  axis(2)

  # Add Text #
  text(-pi/2, 0.5,
       expression(hat(alpha)==(Sigma^tau*Chi)^{-1} * X^theta * y^delta),
       cex = 2)
  text(pi/2, -0.5,
      expression(
        paste(frac(beta^4, sigma*sqrt(2*pi*Omega)), " ",
              italic(e)^{frac(-(x-mu)^2, 2*sigma^2)})),
       cex = 2
  )
  text(0, 1.25,
       expression(
         paste("greek = ", alpha*beta*gamma*delta*epsilon*zeta*eta*
               theta*vartheta*iota*kappa*mu*nu*xi*pi*rho*sigma*
               varsigma*tau*upsilon*phi*varphi*chi*psi*omega))
  )
  text(0, -1.25,
       expression(
         paste("GREEK = ", Gamma*Lambda*Sigma*Psi*Delta*Xi*
               Upsilon*Omega*Theta*Pi*Phi))
  )
}

plotPCHsymbols <- function(f, h = 7, w = 10, s = 1) {
  pdf(file = f, height = h * s, width = w * s, useDingbats = FALSE,
      title = basename(f))
  on.exit(dev.off())
  b <- 25L
  for (i in 1:b) {
    if ( i == 1L ) {
      plot(i, i, ylim = c(1, b), xlim = c(1, b), xlab = "", ylab = "", type = "n")
      grid(lty = 1, col = "gray90")
      title("Plotting Symbol, Line Type, & Color Codes in R")
      legend("topleft", legend = 1:6, lty = 1:6, lwd = 1.5, ncol = 2, bg = "gray95")
      legend("bottomright", legend = 1:8, col = 1:8, ncol = 3, pch = 19, bg = "gray95")
    }
    points(i, i, pch = i, col = i, cex = 2)
  }
}

print2file <- function(..., file, width = 250) {
  usethis::ui_done("Creating text file ... {usethis::ui_value(file)}")
  withr::with_output_sink(file, {
    withr::with_options(list(width = width), {
      invisible(lapply(list(...), print))
    })
  })
}
