#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

LSeffectSizes   <- function(jaspResults, dataset, options, state = NULL){

  if (options[["effectSize"]] == "delta") {

    .tsDeltaPopulationPlot(jaspResults, options)
    .tsDeltaPopulationTable(jaspResults, options)

    if (options[["simulateData"]]) {

      .tsDeltaSimulateData(jaspResults, options)
      .tsDeltaSimulationPlot(jaspResults, options)
      .tsDeltaSimulationTable(jaspResults, options)

    }


  } else if (options[["effectSize"]] == "rho") {

    .tsRhoPopulationPlot(jaspResults, options)
    .tsRhoPopulationTable(jaspResults, options)

    if (options[["simulateData"]]) {

      .tsRhoSimulateData(jaspResults, options)
      .tsRhoSimulationPlot(jaspResults, options)
      .tsRhoSimulationTable(jaspResults, options)

    }


  } else if (options[["effectSize"]] == "phi") {

    .tsPhiPopulationPlot(jaspResults, options)
    .tsPhiPopulationTable(jaspResults, options)

    if (options[["simulateData"]]) {

      .tsPhiSimulateData(jaspResults, options)
      .tsPhiSimulationPlot(jaspResults, options)
      .tsPhiSimulationTable(jaspResults, options)

    }


  }

  return()
}

.tsDeltaPopulationTable    <- function(jaspResults, options) {

  if (!is.null(jaspResults[["deltaPopulationTable"]]))
    return()

  deltaTable <- createJaspTable(title = gettext("Population Characteristics"))
  deltaTable$position <- 2
  deltaTable$dependOn(c("effectSize", "effectSizeValueDelta", "eventRate"))

  jaspResults[["deltaPopulationTable"]] <- .tsDeltaFillTable(deltaTable, options[["effectSizeValueDelta"]], options[["eventRate"]])

  return()
}
.tsDeltaSimulationTable    <- function(jaspResults, options) {

  if (!is.null(jaspResults[["deltaSimulationTable"]]))
    return()

  deltaTable <- createJaspTable(title = gettext("Simulation Summary"))
  deltaTable$position <- 4
  deltaTable$dependOn(c("effectSize", "effectSizeValueDelta", "simulateData", "simulateDataN", "eventRate", "mu", "sigma", "n"))

  data <- jaspResults[["simulatedData"]]$object

  delta <- psych::t2d(stats::t.test(data$x ~ data$Group)$stat, n1 = options[["simulateDataN"]], n2 = options[["simulateDataN"]])
  delta <- unname(psych::d.ci(-delta, n1 = options[["simulateDataN"]], n2 = options[["simulateDataN"]])[1,c(2,1,3)])

  jaspResults[["deltaSimulationTable"]] <- .tsDeltaFillTable(deltaTable, delta, options[["eventRate"]])

  return()
}
.tsDeltaPopulationPlot     <- function(jaspResults, options) {

  if (!is.null(jaspResults[["deltaPopulationPlot"]]))
    return()

  deltaPopulationPlot   <- createJaspPlot(title = gettext("Population distributibution"), width = 500, height = 350)
  deltaPopulationPlot$position <- 1
  deltaPopulationPlot$dependOn(c("effectSize", "effectSizeValueDelta", "mu", "sigma", "simulateData"))
  jaspResults[["deltaPopulationPlot"]] <- deltaPopulationPlot

  deltaPopulationPlot$plotObject <- .tsDeltaMakePopulationPlot(
    delta = options[["effectSizeValueDelta"]],
    mu    = options[["mu"]],
    sigma = options[["sigma"]])

  return()
}
.tsDeltaSimulationPlot     <- function(jaspResults, options) {

  if (!is.null(jaspResults[["deltaSimulationPlot"]]))
    return()

  deltaSimulationPlot   <- createJaspPlot(title = gettext("Simulation distributibution"), width = 660, height = 350)
  deltaSimulationPlot$position <- 3
  deltaSimulationPlot$dependOn(c("effectSize", "effectSizeValueDelta", "simulateData", "simulateDataN", "mu", "sigma"))
  jaspResults[["deltaSimulationPlot"]] <- deltaSimulationPlot

  deltaSimulationPlot$plotObject <- .tsDeltaMakeSimulationPlot(jaspResults[["simulatedData"]]$object)

  return()
}
.tsDeltaSimulateData       <- function(jaspResults, options) {

  if (is.null(jaspResults[["simulatedData"]])) {

    simulatedData <- createJaspState()
    simulatedData$dependOn(c("effectSize", "effectSizeValueDelta", "simulateDataN", "mu", "sigma"))
    simulatedData$object <- data.frame(
      x     = c(
        stats::rnorm(options[["simulateDataN"]], options[["mu"]],  options[["sigma"]]),
        stats::rnorm(options[["simulateDataN"]], options[["mu"]] + options[["effectSizeValueDelta"]] * options[["sigma"]], options[["sigma"]])
      ),
      Group = c(
        rep("Control",      options[["simulateDataN"]]),
        rep("Experimental", options[["simulateDataN"]])
      )
    )
    jaspResults[["simulatedData"]] <- simulatedData

  }

  return()

}
.tsDeltaMakePopulationPlot <- function(delta, mu, sigma) {

  dens1 <- function(x) stats::dnorm(x, mu, sigma)
  dens2 <- function(x) stats::dnorm(x, mu + delta * sigma, sigma)

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(
    stats::qnorm(c(0.01, 0.99), mu, sigma),
    stats::qnorm(c(0.01, 0.99), mu + delta * sigma, sigma)
  )))
  yTicks <- jaspGraphs::getPrettyAxisBreaks(c(0, dens1(mu)))
  xlim   <- range(xTicks)
  ylim   <- range(yTicks)


  # based on: https://rpsychologist.com/cohend/
  plot <- ggplot2::ggplot() +
    ggplot2::stat_function(
      fun   = dens1,
      geom  = "area",
      xlim  = xlim,
      alpha = .2,
      fill  = "blue") +
    ggplot2::stat_function(
      fun   = dens1,
      geom  = "line",
      xlim  = xlim,
      color = "blue",
      size  = 1) +
    ggplot2::geom_line(
      mapping  = ggplot2::aes(
        x = c(mu, mu),
        y = c(0, dens1(mu))),
      linetype = 3
    ) +
    ggplot2::geom_line(
      mapping  = ggplot2::aes(
        x = c(mu + delta * sigma, mu + delta * sigma),
        y = c(0, dens2(mu + delta * sigma))),
      linetype = 3
    ) +
    ggplot2::stat_function(
      fun   = dens2,
      geom  = "area",
      xlim  = xlim,
      alpha = .2,
      fill  = "red") +
    ggplot2::stat_function(
      fun   = dens2,
      geom  = "line",
      xlim  = xlim,
      color = "red",
      size  = 1) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        x     = mu,
        y     = ylim[2] * 1.10,
        label = gettext("Control"),
        hjust = 1,
        vjust = 1),
      size = 5) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        x     = mu + delta * sigma,
        y     = if (delta < 0) ylim[2] * 1.05 else ylim[2] * 1.10,
        label = gettext("Experimental"),
        hjust = if (delta < 2) 0 else 1,
        vjust = 1),
      size = 5) +
    ggplot2::scale_x_continuous(
      expression(X),
      breaks = xTicks,
      limits = xlim) +
    ggplot2::scale_y_continuous(
      gettext("Density"),
      breaks = yTicks,
      limits = ylim * 1.10) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plot)
}
.tsDeltaMakeSimulationPlot <- function(data) {

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(data$x))

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data     = data,
      mapping  = ggplot2::aes(x = x, fill = Group, color = Group),
      alpha    = 0.5,
      position = "identity") +
    ggplot2::scale_color_manual(values = c("Control" = "blue", "Experimental" = "red")) +
    ggplot2::scale_fill_manual(values = c("Control" = "blue", "Experimental" = "red")) +
    ggplot2::scale_x_continuous(expression(X), breaks = xTicks, limits = range(xTicks)) +
    ggplot2::scale_y_continuous(gettext("Count")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  return(plot)
}
.tsDeltaFillTable          <- function(deltaTable, delta, eventRate) {

  # add columns
  deltaTable$addColumnInfo(name = "variable",    title = "",  type = "string")
  deltaTable$addColumnInfo(name = "value",       title = if (length(delta) > 1) gettext("Estimate") else "",  type = "number")
  if (length(delta) > 1) {
    deltaTable$addColumnInfo(name = "lower", title = gettext("Lower"),  type = "number", overtitle = gettext("95% CI"))
    deltaTable$addColumnInfo(name = "upper", title = gettext("Upper"),  type = "number", overtitle = gettext("95% CI"))
  }

  # based on: https://rpsychologist.com/cohend/
  row1 <- list(
    variable = gettext("Cohen's d"),
    value    = delta[1])
  row2 <- list(
    variable = gettext("Cohen's U3"),
    value    = stats::pnorm(delta[1]))
  row3 <- list(
    variable = gettext("Overlap"),
    value    = 2*stats::pnorm(-abs(delta[1])/2))
  row4 <- list(
    variable = gettext("Probability of superiority"),
    value    = stats::pnorm(delta[1]/sqrt(2)))
  row5 <- list(
    variable = gettext("Number needed to treat*"),
    value    = 1 / (stats::pnorm(delta[1] + stats::qnorm(eventRate))-eventRate))

  if (length(delta) > 1) {
    row1 <- c(row1, c(
      lower = delta[2],
      upper = delta[3]
    ))
    row2 <- c(row2, c(
      lower = stats::pnorm(delta[2]),
      upper = stats::pnorm(delta[3])
    ))
    row3 <- c(row3, c(
      lower = 2*stats::pnorm(-abs(max(delta[2:3]))/2),
      upper = if(sum(sign(delta[2:3])) == 0) 1 else 2*stats::pnorm(-abs(min(delta[2:3]))/2)
    ))
    row4 <- c(row4, c(
      lower = stats::pnorm(delta[2]/sqrt(2)),
      upper = stats::pnorm(delta[3]/sqrt(2))
    ))
    row5 <- c(row5, c(
      lower = if(delta[3] < 0) Inf else 1 / (stats::pnorm(delta[3] + stats::qnorm(eventRate))-eventRate),
      upper = if(delta[2] < 0) Inf else 1 / (stats::pnorm(delta[2] + stats::qnorm(eventRate))-eventRate)
    ))
  }

  deltaTable$addRows(row1)
  deltaTable$addRows(row2)
  deltaTable$addRows(row3)
  deltaTable$addRows(row4)
  deltaTable$addRows(row5)

  deltaTable$addFootnote(gettextf("The number needed to treat is based on a %1$s event rate.", eventRate))

  return(deltaTable)
}

.tsRhoPopulationTable    <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rhoPopulationTable"]]))
    return()

  rhoTable <- createJaspTable(title = gettext("Population Characteristics"))
  rhoTable$position <- 2
  rhoTable$dependOn(c("effectSize", "effectSizeValueRho"))

  jaspResults[["rhoPopulationTable"]] <- .tsRhoFillTable(rhoTable, options[["effectSizeValueRho"]])

  return()
}
.tsRhoSimulationTable    <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rhoSimulationTable"]]))
    return()

  rhoTable <- createJaspTable(title = gettext("Simulation Characteristics"))
  rhoTable$position <- 4
  rhoTable$dependOn(c("effectSize", "effectSizeValueRho", "simulateData", "simulateDataN", "mu1", "mu2", "sigma1", "sigma2"))

  data <- jaspResults[["simulatedData"]]$object
  rho  <- cor.test(data$x, data$y)
  rho  <- unname(c(rho$estimate, rho$conf.int))

  jaspResults[["rhoSimulationTable"]] <- .tsRhoFillTable(rhoTable, rho)

  return()
}
.tsRhoPopulationPlot     <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rhoPopulationPlot"]]))
    return()

  rhoPopulationPlot   <- createJaspPlot(title = gettext("Population distributibution"), width = 500, height = 350)
  rhoPopulationPlot$position <- 1
  rhoPopulationPlot$dependOn(c("effectSize", "effectSizeValueRho", "mu1", "mu2", "sigma1", "sigma2"))
  jaspResults[["rhoPopulationPlot"]] <- rhoPopulationPlot

  rhoPopulationPlot$plotObject <- .tsRhoMakePopulationPlot(
    rho    = options[["effectSizeValueRho"]],
    mu1    = options[["mu1"]],
    mu2    = options[["mu2"]],
    sigma1 = options[["sigma1"]],
    sigma2 = options[["sigma2"]])

  return()
}
.tsRhoSimulationPlot     <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rhoSimulationPlot"]]))
    return()

  rhoSimulationPlot   <- createJaspPlot(title = gettext("Simulation distributibution"), width = 500, height = 500)
  rhoSimulationPlot$position <- 3
  rhoSimulationPlot$dependOn(c("effectSize", "effectSizeValueRho", "simulateData", "simulateDataN", "mu1", "mu2", "sigma1", "sigma2"))
  jaspResults[["rhoSimulationPlot"]] <- rhoSimulationPlot

  rhoSimulationPlot$plotObject <- .tsRhoMakeSimulationPlot(jaspResults[["simulatedData"]]$object)

  return()
}
.tsRhoSimulateData       <- function(jaspResults, options) {

  if (is.null(jaspResults[["simulatedData"]])) {

    simulatedData <- createJaspState()
    simulatedData$dependOn(c("effectSize", "effectSizeValueDelta", "simulateDataN", "mu1", "mu2", "sigma1", "sigma1", "effectSizeValueRho"))

    data <- data.frame(MASS::mvrnorm(
      options[["simulateDataN"]],
      c(options[["mu1"]], options[["mu2"]]),
      matrix(c(
        options[["sigma1"]]^2, options[["sigma1"]]*options[["sigma2"]]*options[["effectSizeValueRho"]],
        options[["sigma1"]]*options[["sigma2"]]*options[["effectSizeValueRho"]], options[["sigma2"]]^2),
        ncol = 2, nrow = 2)))
    colnames(data) <- c("x", "y")

    simulatedData$object <- data
    jaspResults[["simulatedData"]] <- simulatedData

  }

  return()

}
.tsRhoMakePopulationPlot <- function(rho, mu1, mu2, sigma1, sigma2, nPoints = 100) {

  if (isTRUE(all.equal(abs(rho), 1))) {

    plot <- ggplot2::ggplot() +
      ggplot2::geom_line(
        mapping = ggplot2::aes(x, y),
        data    = data.frame(
          x = c(mu1 * c(-3, 3) * sigma1),
          y = c(mu2 * c(-3, 3) * sigma2)
        ),
        color   = "red",
        size    = 1.2)

    xTicks <- jaspGraphs::getPrettyAxisBreaks(c(mu1 * c(-3, 3) * sigma1))
    yTicks <- jaspGraphs::getPrettyAxisBreaks(c(mu2 * c(-3, 3) * sigma2))

  } else {

    # based on: https://stackoverflow.com/questions/25718363/how-to-plot-bivariate-normal-distribution-with-expanding-ellipses
    theta <- c(mu1, mu2)
    sigma <- matrix(c(sigma1^2, sigma1*sigma2*rho, sigma1*sigma2*rho, sigma2^2), ncol = 2, nrow = 2)

    xy <- cbind(
      sin(seq(0, 2 * pi, length.out = nPoints)),
      cos(seq(0, 2 * pi, length.out = nPoints))
    )

    # then we scale the dimensions
    ev <- eigen(sigma)
    xy[, 1] <- xy[, 1] * 1
    xy[, 2] <- xy[, 2] * sqrt(min(ev$values) / max(ev$values))

    # then rotate
    phi <- atan(ev$vectors[2, 1] / ev$vectors[1, 1])
    R   <- matrix(c(cos(phi), sin(phi), -sin(phi), cos(phi)), 2)
    xy  <- tcrossprod(R, xy)

    xTicks <- jaspGraphs::getPrettyAxisBreaks(sqrt(qchisq(.90, df = 2) * max(ev$values)) * range(xy[1, ]) + theta[1])
    yTicks <- jaspGraphs::getPrettyAxisBreaks(sqrt(qchisq(.90, df = 2) * max(ev$values)) * range(xy[2, ]) + theta[2])

    gridData <- expand.grid(
      x = seq(max(xTicks), min(xTicks), length.out = 201),
      y = seq(min(yTicks), max(yTicks), length.out = 201)
    )

    gridData$d <- mvtnorm::dmvnorm(x = gridData, mean = c(mu1, mu2), sigma = sigma, log = FALSE)

    plot <- ggplot2::ggplot()+
      ggplot2::geom_raster(data = gridData, mapping = ggplot2::aes(x = x, y = y, fill = d), alpha = 1) +
      colorspace::scale_fill_continuous_sequential(palette = "Blues")

    for (lvl in rev(seq(.10, .90, .10)))
      plot <- plot + ggplot2::geom_path(
        mapping = ggplot2::aes(x, y),
        data    = data.frame(
          x = sqrt(stats::qchisq(lvl, df = 2) * max(ev$values)) * xy[1, ] + theta[1],
          y = sqrt(stats::qchisq(lvl, df = 2) * max(ev$values)) * xy[2, ] + theta[2]),
        alpha   = 1 - lvl)

  }

  plot <- plot +
    ggplot2::scale_x_continuous(
      expression(X[1]),
      breaks = xTicks,
      limits = range(xTicks),
      oob    = scales::oob_keep) +
    ggplot2::scale_y_continuous(
      expression(X[2]),
      breaks = yTicks,
      limits = range(yTicks),
      oob    = scales::oob_keep) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plot)
}
.tsRhoMakeSimulationPlot <- function(data) {

  xlab <- jaspGraphs::getPrettyAxisBreaks(range(data$x))
  ylab <- jaspGraphs::getPrettyAxisBreaks(range(data$y))

  plot <- ggplot2::ggplot() +
    jaspGraphs::geom_point(
      data     = data,
      mapping  = ggplot2::aes(x = x, y = y),
      color    = "red") +
    ggplot2::scale_x_continuous(expression(X[1]), breaks = xlab, limits = range(xlab)) +
    ggplot2::scale_y_continuous(expression(X[2]), breaks = ylab, limits = range(ylab)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plot)
}
.tsRhoFillTable          <- function(rhoTable, rho) {

  # add columns
  rhoTable$addColumnInfo(name = "variable",    title = "",  type = "string")
  rhoTable$addColumnInfo(name = "value",       title = if (length(rho) > 1) gettext("Estimate") else "",  type = "number")
  if (length(rho) > 1) {
    rhoTable$addColumnInfo(name = "lower", title = gettext("Lower"),  type = "number", overtitle = gettext("95% CI"))
    rhoTable$addColumnInfo(name = "upper", title = gettext("Upper"),  type = "number", overtitle = gettext("95% CI"))
  }

  # based on: https://rpsychologist.com/cohend/
  row1 <- list(
    variable = gettext("Rho coefficient"),
    value    = rho[1])
  row2 <- list(
    variable = gettext("Shared variance"),
    value    = rho[1]^2)

  if (length(rho) > 1) {
    row1 <- c(row1, c(
      lower = rho[2],
      upper = rho[3]
    ))
    row2 <- c(row2, c(
      lower = rho[2]^2,
      upper = rho[3]^2
    ))
  }

  rhoTable$addRows(row1)
  rhoTable$addRows(row2)

  return(rhoTable)
}

.tsPhi2Frequencies        <- function(phi, pX, pY) {

  A <- phi * sqrt(pX*pY*(1-pX)*(1-pY)) + pX*pY
  B <- pX - A
  C <- pY - A
  D <- 1 - A - B - C

  output <- list(A = A, B = B, C = C, D = D)

  if (any(output < 0) || any(output > 1))
    .quitAnalysis(gettext("The specified phi, P(X), and P(Y) do not yield a valid contingency table. One of the frequencies would need to be negative."))

  return(output)
}
.tsPhiCi                  <- function(f00, f01, f10, f11) {

  # based on statpsych::ci.phi
  alpha <- 0.05
  z     <- qnorm(1 - alpha/2)
  n     <- f00 + f01 + f10 + f11

  p00 <- f00/n; p01 <- f01/n; p10 <- f10/n; p11 <- f11/n;
  p0x <- (f00 + f01)/n; p1x <- (f10 + f11)/n
  px0 <- (f00 + f10)/n; px1 <- (f01 + f11)/n
  phi <- (p11*p00 - p10*p01)/sqrt(p1x*p0x*px1*px0)

  v1 <- 1 - phi^2
  v2 <- phi + .5*phi^3
  v3 <- (p0x - p1x)*(px0 - px1)/sqrt(p0x*p1x*px0*px1)
  v4 <- (.75*phi^2)*((p0x - p1x)^2/(p0x*p1x) + (px0 - px1)^2/(px0*px1))
  se <- sqrt((v1 + v2*v3 + v4)/n)

  return(c(phi, phi - 1.96*se, phi + 1.96*se))
}
.tsPhiPopulationTable     <- function(jaspResults, options) {

  if (!is.null(jaspResults[["phiPopulationTable"]]))
    return()

  phiTable <- createJaspTable(title = gettext("Population Frequencies"))
  phiTable$position <- 2
  phiTable$dependOn(c("effectSize", "effectSizeValuePhi", "pX", "pY"))

  # based on https://en.wikipedia.org/wiki/Phi_coefficient and some math by Frantisek
  frequencies <- .tsPhi2Frequencies(
    phi = options[["effectSizeValuePhi"]],
    pX  = options[["pX"]],
    pY  = options[["pY"]]
  )

  jaspResults[["phiPopulationTable"]] <- .tsPhiFillFrequencies(phiTable, frequencies)


  phiTable2 <- createJaspTable(title = gettext("Population Characteristics"))
  phiTable2$position <- 3
  phiTable2$dependOn(c("effectSize", "effectSizeValuePhi", "pX", "pY"))

  phi <- matrix(c(options[["pX"]], options[["pY"]], options[["effectSizeValuePhi"]]), ncol = 1, nrow = 3)

  jaspResults[["phiPopulationTable2"]] <- .tsPhiFillTable(phiTable2, phi)

  return()
}
.tsPhiSimulationTable     <- function(jaspResults, options) {

  if (!is.null(jaspResults[["phiSimulationTable"]]))
    return()

  phiTable <- createJaspTable(title = gettext("Simulation Frequencies"))
  phiTable$position <- 5
  phiTable$dependOn(c("effectSize", "effectSizeValuePhi", "pX", "pY", "simulateData", "simulateDataN"))

  data        <- jaspResults[["simulatedData"]]$object
  frequencies <- as.list(data / sum(data))

  jaspResults[["phiSimulationTable"]] <- .tsPhiFillFrequencies(phiTable, frequencies)

  phiTable2 <- createJaspTable(title = gettext("Simulation Characteristics"))
  phiTable2$position <- 6
  phiTable2$dependOn(c("effectSize", "effectSizeValuePhi", "pX", "pY", "simulateData", "simulateDataN"))

  phi <- c((data["A"] + data["B"]) / sum(data), (data["A"] + data["C"]) / sum(data), psych::phi(rbind(data[c("A", "B")], data[c("C", "D")])))
  phi <- unname(rbind(
    c(phi[1], phi[1] - 1.96 * sqrt(phi[1]*(1-phi[1])/sum(data)), phi[1] + 1.96 * sqrt(phi[1]*(1-phi[1])/sum(data))),
    c(phi[2], phi[2] - 1.96 * sqrt(phi[2]*(1-phi[2])/sum(data)), phi[2] + 1.96 * sqrt(phi[2]*(1-phi[2])/sum(data))),
    .tsPhiCi(data["A"], data["B"], data["C"], data["D"])
  ))

  # fix CIs
  phi[1:2,][phi[1:2,] < 0] <- 0
  phi[1:2,][phi[1:2,] > 1] <- 1
  phi[3,][phi[3,] < -1] <- -1
  phi[3,][phi[3,] >  1] <-  1

  jaspResults[["phiSimulationTable2"]] <- .tsPhiFillTable(phiTable2, phi)

  return()
}
.tsPhiPopulationPlot      <- function(jaspResults, options) {

  if (!is.null(jaspResults[["phiPopulationPlots"]]))
    return()

  phiPopulationPlots <- createJaspContainer(title = gettext("Population distributibution"))
  phiPopulationPlots$position <- 1
  phiPopulationPlots$dependOn(c("effectSize", "effectSizeValuePhi", "pX", "pY"))
  jaspResults[["phiPopulationPlots"]] <- phiPopulationPlots


  phiPopulationPlot   <- createJaspPlot(title = gettext(""), width = 650, height = 350)
  phiPopulationPlot$position <- 1
  phiPopulationPlots[["phiPopulationPlot"]] <- phiPopulationPlot

  phiMosaicPlot   <- createJaspPlot(title = gettext(""), width = 650, height = 450)
  phiMosaicPlot$position <- 2
  phiPopulationPlots[["phiMosaicPlot"]] <- phiMosaicPlot

  phiPopulationPlot$plotObject <- .tsPhiMakePopulationDPlot(
    phi = options[["effectSizeValuePhi"]],
    pX  = options[["pX"]],
    pY  = options[["pY"]])

  phiMosaicPlot$plotObject <- .tsPhiMakePopulationMPlot(
    phi = options[["effectSizeValuePhi"]],
    pX  = options[["pX"]],
    pY  = options[["pY"]])

  return()
}
.tsPhiSimulationPlot      <- function(jaspResults, options) {

  if (!is.null(jaspResults[["phiSimulationPlots"]]))
    return()

  phiSimulationPlots <- createJaspContainer(title = gettext("Simulation distributibution"))
  phiSimulationPlots$position <- 4
  phiSimulationPlots$dependOn(c("effectSize", "effectSizeValuePhi", "simulateData", "simulateDataN", "pX", "pY"))
  jaspResults[["phiSimulationPlots"]] <- phiSimulationPlots


  phiPopulationPlot   <- createJaspPlot(title = gettext(""), width = 650, height = 350)
  phiPopulationPlot$position <- 1
  phiSimulationPlots[["phiPopulationPlot"]] <- phiPopulationPlot

  phiMosaicPlot   <- createJaspPlot(title = gettext(""), width = 650, height = 450)
  phiMosaicPlot$position <- 2
  phiSimulationPlots[["phiMosaicPlot"]] <- phiMosaicPlot

  phiPopulationPlot$plotObject <- .tsPhiMakeSimulationDPlot(jaspResults[["simulatedData"]]$object)

  phiMosaicPlot$plotObject <- .tsPhiMakeSimulationMPlot(jaspResults[["simulatedData"]]$object)

  return()
}
.tsPhiSimulateData        <- function(jaspResults, options) {

  if (is.null(jaspResults[["simulatedData"]])) {

    simulatedData <- createJaspState()
    simulatedData$dependOn(c("effectSize", "effectSizeValueDelta", "simulateDataN", "pX", "pY", "effectSizeValuePhi"))

    frequencies <- .tsPhi2Frequencies(phi = options[["effectSizeValuePhi"]], pX = options[["pX"]], pY = options[["pY"]])
    data        <- sample(names(frequencies), options[["simulateDataN"]], replace = TRUE, prob = unlist(frequencies))
    data        <- factor(data, levels = names(frequencies))
    data        <- table(data)

    simulatedData$object <- data
    jaspResults[["simulatedData"]] <- simulatedData

  }

  return()

}
.tsPhiMakePopulationDPlot <- function(phi, pX, pY) {

  frequencies <- .tsPhi2Frequencies(phi = phi, pX = pX, pY = pY)
  yTicks      <- jaspGraphs::getPrettyAxisBreaks(range(c(0, unlist(frequencies))))

  plot <- ggplot2::ggplot(
    data    = data.frame(
      X = factor(c(1, 1, 0, 0)),
      Y = factor(c(1, 0, 1, 0)),
      p = c(frequencies[["A"]], frequencies[["C"]], frequencies[["B"]], frequencies[["D"]])
    ),
    mapping = ggplot2::aes(fill = Y, y = p, x = X)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::scale_y_continuous(
      expression(P(X,Y)),
      breaks = yTicks,
      limits = range(yTicks)
      ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  return(plot)
}
.tsPhiMakePopulationMPlot <- function(phi, pX, pY) {

  frequencies <- .tsPhi2Frequencies(phi = phi, pX = pX, pY = pY)
  ticks       <- jaspGraphs::getPrettyAxisBreaks(range(c(0, 1)))

  plot <- ggplot2::ggplot(data = data.frame(
    x_start = c(0,    0,    1-pX, 1-pX),
    x_stop  = c(1-pX, 1-pX, 1,    1),
    y_start = c(0,                    frequencies$C/(1-pX), 0,                    1-frequencies$B/(pX)),
    y_stop  = c(frequencies$C/(1-pX), 1,                    1-frequencies$B/(pX), 1),
    Outcome = c("P(X=0,Y=0)", "P(X=0,Y=1)", "P(X=1,Y=0)", "P(X=1,Y=1)")
  )) +
    ggplot2::scale_x_continuous(name = "", breaks = ticks, limits = range(ticks)) +
    ggplot2::scale_y_continuous(name = "", breaks = ticks, limits = range(ticks)) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = x_start,
        xmax = x_stop,
        ymin = y_start,
        ymax = y_stop,
        fill = Outcome),
      color="black", alpha = 0.5)  +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  return(plot)
}
.tsPhiMakeSimulationDPlot <- function(data) {

  yTicks      <- jaspGraphs::getPrettyAxisBreaks(c(0, range(unlist(data))))

  plot <- ggplot2::ggplot(
    data    = data.frame(
      X = factor(c(1, 1, 0, 0)),
      Y = factor(c(1, 0, 1, 0)),
      p = c(data[["A"]], data[["C"]], data[["B"]], data[["D"]])
    ),
    mapping = ggplot2::aes(fill = Y, y = p, x = X)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::scale_y_continuous(
      "Counts",
      breaks = yTicks,
      limits = range(yTicks)
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  return(plot)
}
.tsPhiMakeSimulationMPlot <- function(data) {

  data  <- data / sum(data)
  pX    <- data[["A"]] + data[["B"]]
  ticks <- jaspGraphs::getPrettyAxisBreaks(range(c(0, 1)))

  plot <- ggplot2::ggplot(data = data.frame(
    x_start = c(0,    0,    1-pX, 1-pX),
    x_stop  = c(1-pX, 1-pX, 1,    1),
    y_start = c(0,             data[["C"]]/(1-pX), 0,              1-data[["B"]]/(pX)),
    y_stop  = c(data[["C"]]/(1-pX), 1,              1-data[["B"]]/(pX), 1),
    Outcome = c("P(X=0,Y=0)", "P(X=0,Y=1)", "P(X=1,Y=0)", "P(X=1,Y=1)")
  )) +
    ggplot2::scale_x_continuous(name = "", breaks = ticks, limits = range(ticks)) +
    ggplot2::scale_y_continuous(name = "", breaks = ticks, limits = range(ticks)) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = x_start,
        xmax = x_stop,
        ymin = y_start,
        ymax = y_stop,
        fill = Outcome),
      color="black", alpha = 0.5)  +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  return(plot)

  return(plot)
}
.tsPhiFillFrequencies     <- function(phiTable, frequencies) {

  # add columns
  phiTable$addColumnInfo(name = "x",   title = "",       type = "string")
  phiTable$addColumnInfo(name = "Y1",  title = "P(Y=1)", type = "number")
  phiTable$addColumnInfo(name = "Y0",  title = "P(Y=0)", type = "number")

  phiTable$addRows(list(
    x  = "P(X=1)",
    Y1 = frequencies[["A"]],
    Y0 = frequencies[["B"]]))
  phiTable$addRows(list(
    x  = "P(X=0)",
    Y1 = frequencies[["C"]],
    Y0 = frequencies[["D"]]))

  return(phiTable)
}
.tsPhiFillTable           <- function(phiTable2, phi) {

  # add columns
  phiTable2$addColumnInfo(name = "variable",    title = "",  type = "string")
  phiTable2$addColumnInfo(name = "value",       title = if (ncol(phi) > 1) gettext("Estimate") else "",  type = "number")
  if (ncol(phi) > 1) {
    phiTable2$addColumnInfo(name = "lower", title = gettext("Lower"),  type = "number", overtitle = gettext("95% CI"))
    phiTable2$addColumnInfo(name = "upper", title = gettext("Upper"),  type = "number", overtitle = gettext("95% CI"))
  }

  row1 <- list(
    variable = gettext("P(X=1)"),
    value    = phi[1,1])
  row2 <- list(
    variable = gettext("P(Y=1)"),
    value    = phi[2,1])
  row3 <- list(
    variable = gettext("Phi coefficient"),
    value    = phi[3,1])

  if (ncol(phi) > 1) {
    row1 <- c(row1, c(
      lower = phi[1, 2],
      upper = phi[1, 3]
    ))
    row2 <- c(row2, c(
      lower = phi[2, 2],
      upper = phi[2, 3]
    ))
    row3 <- c(row3, c(
      lower = phi[3, 2],
      upper = phi[3, 3]
    ))
  }

  phiTable2$addRows(row1)
  phiTable2$addRows(row2)
  phiTable2$addRows(row3)

  return(phiTable2)
}
