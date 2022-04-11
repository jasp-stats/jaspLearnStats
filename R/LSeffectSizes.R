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

# know bugs:
# - upper index for R^2 does not work in the table
# - aspect ratio on correlation does not work

LSeffectSizes   <- function(jaspResults, dataset, options, state = NULL){

  if (options[["effectSize"]] == "delta") {

    if (options[["simulateData"]])
      .tsDeltaSimulateData(jaspResults, options)

    .tsDeltaPlot(jaspResults, options)
    .tsDeltaTable(jaspResults, options)

  } else if (options[["effectSize"]] == "rho") {

    if (options[["simulateData"]])
      .tsRhoSimulateData(jaspResults, options)

    .tsRhoPlot(jaspResults, options)
    .tsRhoTable(jaspResults, options)

  } else if (options[["effectSize"]] == "phi") {

    if (options[["simulateData"]])
      .tsPhiSimulateData(jaspResults, options)

    .tsPhiTable(jaspResults, options)

    .tsPhiPopulationPlot(jaspResults, options)
    if (options[["simulateData"]]) {
      .tsPhiSimulationPlot(jaspResults, options)
    }


  }

  return()
}

.tsDeltaTable              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["deltaTable"]]))
    return()

  deltaTable <- createJaspTable(title = gettext("Statistics Summary"))
  deltaTable$position <- 3
  deltaTable$dependOn(c("effectSize", "effectSizeValueDelta", "simulateData", "simulateDataN", "eventRate", "mu", "sigma", "n",
                        "deltaCohensU3", "deltaOverlap", "deltaProbabilityOfSuperiority", "deltaNumberNeededToTreat"))

  # get population characteristics
  delta <- options[["effectSizeValueDelta"]]

  # add simulation characteristics
  if (options[["simulateData"]]){

    data <- jaspResults[["simulatedData"]]$object

    deltaSim <- psych::t2d(stats::t.test(data$x ~ data$Group)$stat, n1 = options[["simulateDataN"]], n2 = options[["simulateDataN"]])
    deltaSim <- unname(psych::d.ci(-deltaSim, n1 = options[["simulateDataN"]], n2 = options[["simulateDataN"]])[1,c(2,1,3)])

    delta <- c(delta, deltaSim)
  }

  jaspResults[["deltaTable"]] <- .tsDeltaFillTable(deltaTable, options, delta, options[["eventRate"]])

  return()
}
.tsDeltaPlot               <- function(jaspResults, options) {

  if (!is.null(jaspResults[["deltaPlot"]]))
    return()

  deltaPlot <-  createJaspContainer()
  deltaPlot$position <- 1
  deltaPlot$dependOn(c("effectSize", "effectSizeValueDelta", "mu", "sigma", "simulateData", "simulateDataN", "plotCombine"))
  jaspResults[["deltaPlot"]] <- deltaPlot


  if (options[["plotCombine"]] && options[["simulateData"]]){

    deltaCombinedPlot <- createJaspPlot(title = gettext("Population and simulation distributibution"), width = 500, height = 350)
    deltaCombinedPlot$position <- 1
    deltaPlot[["deltaCombinedPlot"]] <- deltaCombinedPlot

    deltaCombinedPlot$plotObject <- .tsDeltaMakeCombinedPlot(
      delta = options[["effectSizeValueDelta"]],
      mu    = options[["mu"]],
      sigma = options[["sigma"]],
      data  = jaspResults[["simulatedData"]]$object)

  } else {

    deltaPopulationPlot <- createJaspPlot(title = gettext("Population distributibution"), width = 500, height = 350)
    deltaPopulationPlot$position <- 1
    deltaPlot[["deltaPopulationPlot"]] <- deltaPopulationPlot

    deltaPopulationPlot$plotObject <- .tsDeltaMakePopulationPlot(
      delta = options[["effectSizeValueDelta"]],
      mu    = options[["mu"]],
      sigma = options[["sigma"]])

    if (options[["simulateData"]]) {

      deltaSimulationPlot <- createJaspPlot(title = gettext("Simulation distributibution"), width = 660, height = 350)
      deltaSimulationPlot$position <- 2
      deltaPlot[["deltaSimulationPlot"]] <- deltaSimulationPlot

      deltaSimulationPlot$plotObject <- .tsDeltaMakeSimulationPlot(jaspResults[["simulatedData"]]$object)
    }
  }

  return()
}
.tsDeltaSimulateData       <- function(jaspResults, options) {

  .setSeedJASP(options)

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
.tsDeltaMakeCombinedPlot   <- function(delta, mu, sigma, data) {

  dens1 <- function(x) stats::dnorm(x, mu, sigma)
  dens2 <- function(x) stats::dnorm(x, mu + delta * sigma, sigma)

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(
    stats::qnorm(c(0.01, 0.99), mu, sigma),
    stats::qnorm(c(0.01, 0.99), mu + delta * sigma, sigma),
    data$x
  )))
  yTicks <- jaspGraphs::getPrettyAxisBreaks(c(0, dens1(mu)))
  xlim   <- range(xTicks)
  ylim   <- range(yTicks)


  # based on: https://rpsychologist.com/cohend/
  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data     = data,
      mapping  = ggplot2::aes(x = x, fill = Group, color = Group, y = ..density..),
      alpha    = 0.5,
      position = "identity") +
    ggplot2::scale_color_manual(values = c("Control" = "blue", "Experimental" = "red")) +
    ggplot2::scale_fill_manual(values = c("Control" = "blue", "Experimental" = "red")) +
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
      gettext("Density")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()


  return(plot)
}
.tsDeltaFillTable          <- function(deltaTable, options, delta, eventRate) {

  # add columns
  deltaTable$addColumnInfo(name = "variable",    title = "",                      type = "string")
  deltaTable$addColumnInfo(name = "population",  title = gettext("Population") ,  type = "number")
  if (length(delta) > 1) {
    deltaTable$addColumnInfo(name = "simulation", title = gettext("Simulation"),  type = "number")
    deltaTable$addColumnInfo(name = "lower",      title = gettext("Lower"),       type = "number", overtitle = gettext("95% CI"))
    deltaTable$addColumnInfo(name = "upper",      title = gettext("Upper"),       type = "number", overtitle = gettext("95% CI"))
  }

  # based on: https://rpsychologist.com/cohend/
  row1 <- list(
    variable   = gettext("Cohen's d"),
    population = delta[1])

  if (options[["deltaCohensU3"]])
    row2 <- list(
      variable   = gettext("Cohen's U3"),
      population = stats::pnorm(delta[1]))

  if (options[["deltaOverlap"]])
    row3 <- list(
      variable   = gettext("Overlap"),
      population = 2*stats::pnorm(-abs(delta[1])/2))

  if (options[["deltaProbabilityOfSuperiority"]])
    row4 <- list(
      variable   = gettext("Probability of superiority"),
      population = stats::pnorm(delta[1]/sqrt(2)))

  if (options[["deltaNumberNeededToTreat"]])
    row5 <- list(
      variable   = gettext("Number needed to treat*"),
      population = 1 / (stats::pnorm(delta[1] + stats::qnorm(eventRate))-eventRate))

  if (length(delta) > 1) {

    row1 <- c(row1, c(
      simulation = delta[2],
      lower      = delta[3],
      upper      = delta[4]
    ))

    if (options[["deltaCohensU3"]])
      row2 <- c(row2, c(
        simulation = stats::pnorm(delta[2]),
        lower      = stats::pnorm(delta[3]),
        upper      = stats::pnorm(delta[4])
      ))

    if (options[["deltaOverlap"]])
      row3 <- c(row3, c(
        simulation = 2*stats::pnorm(-abs(delta[2])/2),
        lower      = 2*stats::pnorm(-abs(max(delta[3:4]))/2),
        upper      = if(sum(sign(delta[3:4])) == 0) 1 else 2*stats::pnorm(-abs(min(delta[3:4]))/2)
      ))

    if (options[["deltaProbabilityOfSuperiority"]])
      row4 <- c(row4, c(
        simulation = stats::pnorm(delta[2]/sqrt(2)),
        lower      = stats::pnorm(delta[3]/sqrt(2)),
        upper      = stats::pnorm(delta[4]/sqrt(2))
      ))

    if (options[["deltaNumberNeededToTreat"]])
      row5 <- c(row5, c(
        simulation = if(delta[2] < 0) Inf else 1 / (stats::pnorm(delta[2] + stats::qnorm(eventRate))-eventRate),
        lower      = if(delta[4] < 0) Inf else 1 / (stats::pnorm(delta[4] + stats::qnorm(eventRate))-eventRate),
        upper      = if(delta[3] < 0) Inf else 1 / (stats::pnorm(delta[3] + stats::qnorm(eventRate))-eventRate)
      ))
  }

  deltaTable$addRows(row1)

  if (options[["deltaCohensU3"]])
    deltaTable$addRows(row2)

  if (options[["deltaOverlap"]])
    deltaTable$addRows(row3)

  if (options[["deltaProbabilityOfSuperiority"]])
    deltaTable$addRows(row4)

  if (options[["deltaNumberNeededToTreat"]])
    deltaTable$addRows(row5)

  if (options[["deltaNumberNeededToTreat"]])
    deltaTable$addFootnote(gettextf("The number needed to treat is based on a %1$s event rate.", eventRate))

  return(deltaTable)
}

.tsRhoTable              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rhoTable"]]))
    return()

  rhoTable <- createJaspTable(title = gettext("Statistics Summary"))
  rhoTable$position <- 2
  rhoTable$dependOn(c("effectSize", "effectSizeValueRho", "simulateData", "simulateDataN",
                      "rhoSharedVariance"))

  # get population characteristics
  rho <- options[["effectSizeValueRho"]]

  # add simulation characteristics
  if (options[["simulateData"]]){

    data <- jaspResults[["simulatedData"]]$object

    rhoSim  <- cor.test(data$x, data$y)
    rhoSim  <- unname(c(rhoSim$estimate, rhoSim$conf.int))

    rho <- c(rho, rhoSim)
  }

  jaspResults[["rhoTable"]] <- .tsRhoFillTable(rhoTable, options, rho)

  return()
}
.tsRhoPlot               <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rhoPlot"]]))
    return()

  rhoPlot <-  createJaspContainer()
  rhoPlot$position <- 1
  rhoPlot$dependOn(c("effectSize", "effectSizeValueRho", "simulateData", "simulateDataN", "mu1", "mu2", "sigma1", "sigma2", "plotCombine", "plotRhoRegression"))
  jaspResults[["rhoPlot"]] <- rhoPlot


  if (options[["plotCombine"]] && options[["simulateData"]]){

    rhoCombinedPlot <- createJaspPlot(title = gettext("Population and simulation distributibution"), height = 500, aspectRatio = 1)
    rhoCombinedPlot$position <- 1
    rhoPlot[["rhoCombinedPlot"]] <- rhoCombinedPlot

    rhoCombinedPlot$plotObject <- .tsRhoMakeCombinedPlot(
      rho    = options[["effectSizeValueRho"]],
      mu1    = options[["mu1"]],
      mu2    = options[["mu2"]],
      sigma1 = options[["sigma1"]],
      sigma2 = options[["sigma2"]],
      data   = jaspResults[["simulatedData"]]$object)

  } else {

    rhoPopulationPlot   <- createJaspPlot(title = gettext("Population distributibution"), height = 500, aspectRatio = 1)
    rhoPopulationPlot$position <- 1
    rhoPlot[["rhoPopulationPlot"]] <- rhoPopulationPlot

    rhoPopulationPlot$plotObject <- .tsRhoMakePopulationPlot(
      rho    = options[["effectSizeValueRho"]],
      mu1    = options[["mu1"]],
      mu2    = options[["mu2"]],
      sigma1 = options[["sigma1"]],
      sigma2 = options[["sigma2"]])

    if (options[["simulateData"]]) {

      rhoSimulationPlot   <- createJaspPlot(title = gettext("Simulation distributibution"), height = 500, aspectRatio = 1)
      rhoSimulationPlot$position <- 2
      rhoPlot[["rhoSimulationPlot"]] <- rhoSimulationPlot

      rhoSimulationPlot$plotObject <- .tsRhoMakeSimulationPlot(jaspResults[["simulatedData"]]$object)
    }
  }

  return()
}
.tsRhoSimulateData       <- function(jaspResults, options) {

  .setSeedJASP(options)

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
      color    = "blue") +
    ggplot2::scale_x_continuous(expression(X[1]), breaks = xlab, limits = range(xlab)) +
    ggplot2::scale_y_continuous(expression(X[2]), breaks = ylab, limits = range(ylab)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plot)
}
.tsRhoMakeCombinedPlot   <- function(rho, mu1, mu2, sigma1, sigma2, data, nPoints = 100) {

  plot <- ggplot2::ggplot()

  if (isTRUE(all.equal(abs(rho), 1))) {

    plot <- plot +
      ggplot2::geom_line(
        mapping = ggplot2::aes(x, y),
        data    = data.frame(
          x = c(mu1 * c(-3, 3) * sigma1),
          y = c(mu2 * c(-3, 3) * sigma2)
        ),
        color   = "red",
        size    = 1.2)

    xTicks <- jaspGraphs::getPrettyAxisBreaks(c(mu1 * c(-3, 3) * sigma1, range(data$x)))
    yTicks <- jaspGraphs::getPrettyAxisBreaks(c(mu2 * c(-3, 3) * sigma2, range(data$y)))

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

    plot <- plot +
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
    jaspGraphs::geom_point(
      data     = data,
      mapping  = ggplot2::aes(x = x, y = y),
      color    = "blue") +
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
.tsRhoFillTable          <- function(rhoTable, options, rho) {

  # add columns
  rhoTable$addColumnInfo(name = "variable",    title = "",  type = "string")
  rhoTable$addColumnInfo(name = "population",  title = gettext("Population") ,  type = "number")
  if (length(rho) > 1) {
    rhoTable$addColumnInfo(name = "simulation", title = gettext("Simulation"),  type = "number")
    rhoTable$addColumnInfo(name = "lower",      title = gettext("Lower"),       type = "number", overtitle = gettext("95% CI"))
    rhoTable$addColumnInfo(name = "upper",      title = gettext("Upper"),       type = "number", overtitle = gettext("95% CI"))
  }

  # based on: https://rpsychologist.com/cohend/
  row1 <- list(
    variable   = gettext("Pearson correlation"),
    population = rho[1])

  if (options[["rhoSharedVariance"]])
    row2 <- list(
      variable   = gettext("Shared variance (R²)"),
      population = rho[1]^2)

  if (length(rho) > 1) {

    row1 <- c(row1, c(
      simulation = rho[2],
      lower      = rho[3],
      upper      = rho[4]
    ))

    if(options[["rhoSharedVariance"]])
      row2 <- c(row2, c(
        simulation = rho[2]^2,
        lower      = rho[3]^2,
        upper      = rho[4]^2
      ))
  }

  rhoTable$addRows(row1)

  if(options[["rhoSharedVariance"]])
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
.tsPhiTable               <- function(jaspResults, options) {

  if (!is.null(jaspResults[["phiPopulationTable"]]))
    return()

  ### Frequencies table
  phiTable <- createJaspTable(title = gettext("Frequencies"))
  phiTable$position <- 2
  phiTable$dependOn(c("effectSize", "effectSizeValuePhi", "simulateData", "simulateDataN", "pX", "pY"))

  # based on https://en.wikipedia.org/wiki/Phi_coefficient and some math by Frantisek
  frequencies <- list(.tsPhi2Frequencies(
    phi = options[["effectSizeValuePhi"]],
    pX  = options[["pX"]],
    pY  = options[["pY"]]
  ))

  # add simulation characteristics
  if (options[["simulateData"]]){

    data <- jaspResults[["simulatedData"]]$object

    frequencies[[2]] <- as.list(data / sum(data))
  }

  jaspResults[["phiTable"]] <- .tsPhiFillFrequencies(phiTable, frequencies)


  ### Characteristics table
  phiTable2 <- createJaspTable(title = gettext("Statistics Summary"))
  phiTable2$position <- 3
  phiTable2$dependOn(c("effectSize", "effectSizeValuePhi", "pX", "pY"))

  phi <- list(matrix(c(options[["pX"]], options[["pY"]], options[["effectSizeValuePhi"]]), ncol = 1, nrow = 3))

  # add simulation characteristics
  if (options[["simulateData"]]){

    data <- jaspResults[["simulatedData"]]$object

    phiSim <- c((data["A"] + data["B"]) / sum(data), (data["A"] + data["C"]) / sum(data), psych::phi(rbind(data[c("A", "B")], data[c("C", "D")])))
    phiSim <- unname(rbind(
      c(phiSim[1], phiSim[1] - 1.96 * sqrt(phiSim[1]*(1-phiSim[1])/sum(data)), phiSim[1] + 1.96 * sqrt(phiSim[1]*(1-phiSim[1])/sum(data))),
      c(phiSim[2], phiSim[2] - 1.96 * sqrt(phiSim[2]*(1-phiSim[2])/sum(data)), phiSim[2] + 1.96 * sqrt(phiSim[2]*(1-phiSim[2])/sum(data))),
      .tsPhiCi(data["A"], data["B"], data["C"], data["D"])
    ))

    # fix CIs
    phiSim[1:2,][phiSim[1:2,] < 0] <- 0
    phiSim[1:2,][phiSim[1:2,] > 1] <- 1
    phiSim[3,][phiSim[3,] < -1] <- -1
    phiSim[3,][phiSim[3,] >  1] <-  1

    phi[[2]] <- phiSim
  }

  jaspResults[["phiTable2"]] <- .tsPhiFillTable(phiTable2, phi)


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

  .setSeedJASP(options)

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
  phiTable$addColumnInfo(name = "x",             title = "",       type = "string")
  phiTable$addColumnInfo(name = "populationY1",  title = "P(Y=1)", type = "number", overtitle = gettext("Population"))
  phiTable$addColumnInfo(name = "populationY0",  title = "P(Y=0)", type = "number", overtitle = gettext("Population"))
  if (length(frequencies) > 1) {
    phiTable$addColumnInfo(name = "simulationY1",  title = "P(Y=1)", type = "number", overtitle = gettext("Simulation"))
    phiTable$addColumnInfo(name = "simulationY0",  title = "P(Y=0)", type = "number", overtitle = gettext("Simulation"))
  }

  row1 <- list(
    x            = "P(X=1)",
    populationY1 = frequencies[[1]][["A"]],
    populationY0 = frequencies[[1]][["B"]])
  row2 <- list(
    x            = "P(X=0)",
    populationY1 = frequencies[[1]][["C"]],
    populationY0 = frequencies[[1]][["D"]])

  if (length(frequencies) > 1) {
    row1 <- c(row1, c(
      simulationY1 = frequencies[[2]][["A"]],
      simulationY0 = frequencies[[2]][["B"]]
    ))
    row2 <- c(row2, c(
      simulationY1 = frequencies[[2]][["C"]],
      simulationY0 = frequencies[[2]][["D"]]
    ))
  }

  phiTable$addRows(row1)
  phiTable$addRows(row2)

  return(phiTable)
}
.tsPhiFillTable           <- function(phiTable2, phi) {

  # add columns
  phiTable2$addColumnInfo(name = "variable",    title = "",  type = "string")
  phiTable2$addColumnInfo(name = "population",  title = gettext("Population") ,  type = "number")
  if (length(phi) > 1) {
    phiTable2$addColumnInfo(name = "simulation", title = gettext("Simulation"),  type = "number")
    phiTable2$addColumnInfo(name = "lower",      title = gettext("Lower"),       type = "number", overtitle = gettext("95% CI"))
    phiTable2$addColumnInfo(name = "upper",      title = gettext("Upper"),       type = "number", overtitle = gettext("95% CI"))
  }

  row1 <- list(
    variable   = gettext("P(X=1)"),
    population = phi[[1]][1,1])
  row2 <- list(
    variable   = gettext("P(Y=1)"),
    population = phi[[1]][2,1])
  row3 <- list(
    variable   = gettext("Phi coefficient"),
    population = phi[[1]][3,1])

  if (length(phi) > 1) {
    row1 <- c(row1, c(
      simulation = phi[[2]][1, 1],
      lower      = phi[[2]][1, 2],
      upper      = phi[[2]][1, 3]
    ))
    row2 <- c(row2, c(
      simulation = phi[[2]][2, 1],
      lower      = phi[[2]][2, 2],
      upper      = phi[[2]][2, 3]
    ))
    row3 <- c(row3, c(
      simulation = phi[[2]][3, 1],
      lower      = phi[[2]][3, 2],
      upper      = phi[[2]][3, 3]
    ))
  }

  phiTable2$addRows(row1)
  phiTable2$addRows(row2)
  phiTable2$addRows(row3)

  return(phiTable2)
}
