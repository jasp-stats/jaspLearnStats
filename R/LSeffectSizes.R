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

  options <- switchOptions(options)

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

    .tsPhiPlot(jaspResults, options)
    .tsPhiTable(jaspResults, options)

  }

  return()
}

switchOptions <- function(options) {

  if (options[["inputPopulation"]]) {
    if (options[["effectSize"]] == "delta") {
      # recompute effect size
      options[["effectSizeValueDelta"]] <- (options[["muE"]] - options[["muC"]])/options[["sigma"]]
    } else if (options[["effectSize"]] == "phi") {
      # standardize input
      sumP <- options[["pX1Y1"]] + options[["pX1Y0"]] + options[["pX0Y1"]] + options[["pX0Y0"]]
      options[["pX1Y1"]] <- options[["pX1Y1"]] / sumP
      options[["pX1Y0"]] <- options[["pX1Y0"]] / sumP
      options[["pX0Y1"]] <- options[["pX0Y1"]] / sumP
      options[["pX0Y0"]] <- options[["pX0Y0"]] / sumP
      # recompute effect size
      options[["effectSizeValuePhi"]] <- (options[["pX1Y1"]]*options[["pX0Y0"]] - options[["pX1Y0"]]*options[["pX0Y1"]]) /
        sqrt( (options[["pX1Y1"]]+options[["pX1Y0"]]) * (options[["pX0Y1"]]+options[["pX0Y0"]]) * (options[["pX1Y1"]]+options[["pX0Y1"]]) * (options[["pX1Y0"]]+options[["pX0Y0"]]))
      options[["pX"]] <- options[["pX1Y1"]] + options[["pX1Y0"]]
      options[["pY"]] <- options[["pX1Y1"]] + options[["pX0Y1"]]
    }
  }

  return(options)
}

.tsDeltaTable              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["deltaTable"]]))
    return()

  deltaTable <-  createJaspContainer()
  deltaTable$position <- 3
  deltaTable$dependOn(c("effectSize", "effectSizeValueDelta", "simulateData", "simulateDataN", "eventRate", "muC", "muE", "inputPopulation", "sigma", "n",
                       "deltaCohensU3", "deltaOverlap", "deltaProbabilityOfSuperiority", "deltaNumberNeededToTreat", "setSeed", "seed", "explanatoryTexts"))
  jaspResults[["deltaTable"]] <- deltaTable


  # summary statistics table
  statisticsTable <- createJaspTable(title = gettext("Statistics Summary"))
  statisticsTable$position <- 1

  # get population characteristics
  delta <- options[["effectSizeValueDelta"]]

  # add simulation characteristics
  if (options[["simulateData"]]){

    data <- jaspResults[["simulatedData"]]$object

    deltaSim <- psych::t2d(stats::t.test(data$x ~ data$Group)$stat, n1 = options[["simulateDataN"]], n2 = options[["simulateDataN"]])
    deltaSim <- unname(psych::d.ci(-deltaSim, n1 = options[["simulateDataN"]], n2 = options[["simulateDataN"]])[1,c(2,1,3)])

    delta <- c(delta, deltaSim)
  }

  deltaTable[["statisticsTable"]] <- .tsDeltaFillTable(statisticsTable, options, delta, options[["eventRate"]])


  # explanatory texts
  if (options[["explanatoryTexts"]]) {

    explanatoryText <- createJaspHtml()
    explanatoryText$position <- 2

    explanatoryText[["text"]] <- .tsDeltaTableText(options)

    deltaTable[["explanatoryText"]] <- explanatoryText
  }

  return()
}
.tsDeltaPlot               <- function(jaspResults, options) {

  if (!is.null(jaspResults[["deltaPlot"]]))
    return()

  deltaPlot <-  createJaspContainer()
  deltaPlot$position <- 1
  deltaPlot$dependOn(c("effectSize", "effectSizeValueDelta", "muC", "muE", "inputPopulation", "sigma", "simulateData", "simulateDataN", "plotCombine", "setSeed", "seed", "plotDeltaRaincloud"))
  jaspResults[["deltaPlot"]] <- deltaPlot


  if (options[["plotCombine"]] && options[["simulateData"]]){

    deltaCombinedPlot <- createJaspPlot(title = gettext("Population and simulation distribution"), width = 500, height = 350)
    deltaCombinedPlot$position <- 1
    deltaPlot[["deltaCombinedPlot"]] <- deltaCombinedPlot

    deltaCombinedPlot$plotObject <- .tsDeltaMakeCombinedPlot(
      delta = options[["effectSizeValueDelta"]],
      mu    = options[["muC"]],
      sigma = options[["sigma"]],
      data  = jaspResults[["simulatedData"]]$object)

  } else {

    deltaPopulationPlot <- createJaspPlot(title = gettext("Population distribution"), width = 500, height = 350)
    deltaPopulationPlot$position <- 1
    deltaPlot[["deltaPopulationPlot"]] <- deltaPopulationPlot

    deltaPopulationPlot$plotObject <- .tsDeltaMakePopulationPlot(
      delta = options[["effectSizeValueDelta"]],
      mu    = options[["muC"]],
      sigma = options[["sigma"]])

    if (options[["simulateData"]]) {

      deltaSimulationPlot <- createJaspPlot(title = gettext("Simulation distribution"), width = 660, height = 350)
      deltaSimulationPlot$position <- 2
      deltaPlot[["deltaSimulationPlot"]] <- deltaSimulationPlot

      deltaSimulationPlot$plotObject <- .tsDeltaMakeSimulationPlot(jaspResults[["simulatedData"]]$object)
    }
  }

  if (options[["simulateData"]] && options[["plotDeltaRaincloud"]]) {
    deltaRaincloudPlot <- createJaspPlot(title = gettext("Raincloud plot"), width = 660, height = 350)
    deltaRaincloudPlot$position <- 3
    deltaPlot[["deltaRaincloudPlot"]] <- deltaRaincloudPlot

    deltaRaincloudPlot$plotObject <- jaspTTests::.descriptivesPlotsRainCloudFill(
      dataset  = jaspResults[["simulatedData"]]$object,
      variable = "x",
      groups   = "Group",
      yLabel   = "X",
      xLabel   = gettext("Group"),
      addLines = FALSE,
      horiz    = FALSE
    )
  }

  return()
}
.tsDeltaSimulateData       <- function(jaspResults, options) {

  .setSeedJASP(options)

  if (is.null(jaspResults[["simulatedData"]])) {

    simulatedData <- createJaspState()
    simulatedData$dependOn(c("effectSize", "effectSizeValueDelta", "simulateDataN", "muC", "muE", "inputPopulation", "sigma", "setSeed", "seed"))
    simulatedData$object <- data.frame(
      x     = c(
        stats::rnorm(options[["simulateDataN"]], options[["muC"]],  options[["sigma"]]),
        stats::rnorm(options[["simulateDataN"]], options[["muC"]] + options[["effectSizeValueDelta"]] * options[["sigma"]], options[["sigma"]])
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
      fill  = jaspGraphs::JASPcolors("colorblind")[1]) +
    ggplot2::stat_function(
      fun   = dens1,
      geom  = "line",
      xlim  = xlim,
      color = jaspGraphs::JASPcolors("colorblind")[1],
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
      fill  = jaspGraphs::JASPcolors("colorblind")[2]) +
    ggplot2::stat_function(
      fun   = dens2,
      geom  = "line",
      xlim  = xlim,
      color = jaspGraphs::JASPcolors("colorblind")[2],
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
    ggplot2::scale_color_manual(values = c("Control" = jaspGraphs::JASPcolors("colorblind")[1], "Experimental" = jaspGraphs::JASPcolors("colorblind")[2])) +
    ggplot2::scale_fill_manual(values = c("Control" = jaspGraphs::JASPcolors("colorblind")[1], "Experimental" = jaspGraphs::JASPcolors("colorblind")[2])) +
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
    ggplot2::scale_color_manual(values = c("Control" = jaspGraphs::JASPcolors("colorblind")[1], "Experimental" = jaspGraphs::JASPcolors("colorblind")[2])) +
    ggplot2::scale_fill_manual(values = c("Control" = jaspGraphs::JASPcolors("colorblind")[1], "Experimental" = jaspGraphs::JASPcolors("colorblind")[2])) +
    ggplot2::stat_function(
      fun   = dens1,
      geom  = "area",
      xlim  = xlim,
      alpha = .2,
      fill  = jaspGraphs::JASPcolors("colorblind")[1]) +
    ggplot2::stat_function(
      fun   = dens1,
      geom  = "line",
      xlim  = xlim,
      color = jaspGraphs::JASPcolors("colorblind")[1],
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
      fill  = jaspGraphs::JASPcolors("colorblind")[2]) +
    ggplot2::stat_function(
      fun   = dens2,
      geom  = "line",
      xlim  = xlim,
      color = jaspGraphs::JASPcolors("colorblind")[2],
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
    deltaTable$addColumnInfo(name = "lower",      title = gettext("Lower"),       type = "number", overtitle = gettextf("95%% CI"))
    deltaTable$addColumnInfo(name = "upper",      title = gettext("Upper"),       type = "number", overtitle = gettextf("95%% CI"))
  }

  # based on: https://rpsychologist.com/cohend/
  row1 <- list(
    variable   = gettext("Cohen's d"),
    population = delta[1])

  if (options[["deltaCohensU3"]])
    row2 <- list(
      variable   = gettextf("Cohen's U%1$s", "\u2083"),
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
.tsDeltaTableText          <- function(options) {

  text <- gettextf("Cohen's %1$s is a measure of the standardized difference between two groups. The rules of thumbs for interpreting the measure are:<ul><li>Small: %1$s = 0.20 </li> <li>Medium: %1$s = 0.50</li><li>Large: %1$s = 0.80</li></ul>", "\u03B4")

  if (options[["deltaCohensU3"]])
    text <- paste0(text, gettextf("- Cohen's U%1$s corresponds to the proportion of the control group that is surpassed by the upper upper half of the experimental group.\n", "\u2083"))

  if (options[["deltaOverlap"]])
    text <- paste0(text, gettext("- Overlap corresponds to the common are of the control and experimental density.\n"))

  if (options[["deltaProbabilityOfSuperiority"]])
    text <- paste0(text, gettext("- Probability of superiority corresponds to the probability that a randomly chosen participant from the experimental group will have higher value than a randomly chosen participant from the control group.\n"))

  if (options[["deltaNumberNeededToTreat"]])
    text <- paste0(text, gettext("- Number needed to treat corresponds to the number of participants in the experimental group that would be required to observe one more successful outcome than in the control group.\n"))

  return(text)
}

.tsRhoTable              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rhoTable"]]))
    return()


  rhoTable <- createJaspContainer()
  rhoTable$position <- 3
  rhoTable$dependOn(c("effectSize", "effectSizeValueRho", "simulateData", "simulateDataN",
                      "rhoSharedVariance", "setSeed", "seed", "explanatoryTexts"))
  jaspResults[["rhoTable"]] <- rhoTable


  # summary statistics table
  statisticsTable <- createJaspTable(title = gettext("Statistics Summary"))
  statisticsTable$position <- 1

  # get population characteristics
  rho <- options[["effectSizeValueRho"]]

  # add simulation characteristics
  if (options[["simulateData"]]){

    data <- jaspResults[["simulatedData"]]$object

    rhoSim  <- cor.test(data$x, data$y)
    rhoSim  <- unname(c(rhoSim$estimate, rhoSim$conf.int))

    rho <- c(rho, rhoSim)
  }

  rhoTable[["statisticsTable"]] <- .tsRhoFillTable(statisticsTable, options, rho)


  # explanatory texts
  if (options[["explanatoryTexts"]]) {

    explanatoryText <- createJaspHtml()
    explanatoryText$position <- 2

    explanatoryText[["text"]] <- .tsRhoTableText(options)

    rhoTable[["explanatoryText"]] <- explanatoryText
  }

  return()
}
.tsRhoPlot               <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rhoPlot"]]))
    return()

  rhoPlot <-  createJaspContainer()
  rhoPlot$position <- 1
  rhoPlot$dependOn(c("effectSize", "effectSizeValueRho", "simulateData", "simulateDataN", "mu1", "mu2", "sigma1", "sigma2", "plotCombine", "plotRhoRegression", "plotRhoRegression", "setSeed", "seed"))
  jaspResults[["rhoPlot"]] <- rhoPlot


  if (options[["plotCombine"]] && options[["simulateData"]]){

    rhoCombinedPlot <- createJaspPlot(title = gettext("Population and simulation distribution"), height = 500, aspectRatio = 1)
    rhoCombinedPlot$position <- 1
    rhoPlot[["rhoCombinedPlot"]] <- rhoCombinedPlot

    rhoCombinedPlot$plotObject <- .tsRhoMakeCombinedPlot(
      rho     = options[["effectSizeValueRho"]],
      mu1     = options[["mu1"]],
      mu2     = options[["mu2"]],
      sigma1  = options[["sigma1"]],
      sigma2  = options[["sigma2"]],
      data    = jaspResults[["simulatedData"]]$object,
      options = options)

  } else {

    rhoPopulationPlot   <- createJaspPlot(title = gettext("Population distribution"), height = 500, aspectRatio = 1)
    rhoPopulationPlot$position <- 1
    rhoPlot[["rhoPopulationPlot"]] <- rhoPopulationPlot

    rhoPopulationPlot$plotObject <- .tsRhoMakePopulationPlot(
      rho     = options[["effectSizeValueRho"]],
      mu1     = options[["mu1"]],
      mu2     = options[["mu2"]],
      sigma1  = options[["sigma1"]],
      sigma2  = options[["sigma2"]],
      options = options)

    if (options[["simulateData"]]) {

      rhoSimulationPlot   <- createJaspPlot(title = gettext("Simulation distribution"), height = 500, aspectRatio = 1)
      rhoSimulationPlot$position <- 2
      rhoPlot[["rhoSimulationPlot"]] <- rhoSimulationPlot

      rhoSimulationPlot$plotObject <- .tsRhoMakeSimulationPlot(jaspResults[["simulatedData"]]$object, options)
    }
  }

  return()
}
.tsRhoSimulateData       <- function(jaspResults, options) {

  .setSeedJASP(options)

  if (is.null(jaspResults[["simulatedData"]])) {

    simulatedData <- createJaspState()
    simulatedData$dependOn(c("effectSize", "effectSizeValueDelta", "simulateDataN", "mu1", "mu2", "sigma1", "sigma1", "effectSizeValueRho", "setSeed", "seed"))

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
.tsRhoMakePopulationPlot <- function(rho, mu1, mu2, sigma1, sigma2, options, nPoints = 100) {

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


  if (options[["plotRhoRegression"]]) {
    plot <- plot + ggplot2::geom_abline(
      intercept = mu2 - (rho * (sigma2/sigma1)) * mu1,
      slope     = rho * (sigma2/sigma1)
    )
  }

  return(plot)
}
.tsRhoMakeSimulationPlot <- function(data, options) {

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

  if (options[["plotRhoRegression"]]) {
    plot <- plot + ggplot2::geom_smooth(
      data    = data,
      mapping = ggplot2::aes(x = x, y = y),
      method  = "lm",
      formula = y ~ x)
  }


  return(plot)
}
.tsRhoMakeCombinedPlot   <- function(rho, mu1, mu2, sigma1, sigma2, data, options, nPoints = 100) {

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

  if (options[["plotRhoRegression"]]) {
    plot <- plot  + ggplot2::geom_smooth(
      data    = data,
      mapping = ggplot2::aes(x = x, y = y),
      method  = "lm",
      formula = y ~ x) +
    ggplot2::geom_abline(
      intercept = mu2,
      slope     = rho * (sigma2/sigma1)
    )
  }

  return(plot)
}
.tsRhoFillTable          <- function(rhoTable, options, rho) {

  # add columns
  rhoTable$addColumnInfo(name = "variable",    title = "",  type = "string")
  rhoTable$addColumnInfo(name = "population",  title = gettext("Population") ,  type = "number")
  if (length(rho) > 1) {
    rhoTable$addColumnInfo(name = "simulation", title = gettext("Simulation"),  type = "number")
    rhoTable$addColumnInfo(name = "lower",      title = gettext("Lower"),       type = "number", overtitle = gettextf("95%% CI"))
    rhoTable$addColumnInfo(name = "upper",      title = gettext("Upper"),       type = "number", overtitle = gettextf("95%% CI"))
  }

  # based on: https://rpsychologist.com/cohend/
  row1 <- list(
    variable   = gettext("Pearson correlation"),
    population = rho[1])

  if (options[["rhoSharedVariance"]])
    row2 <- list(
      variable   = gettextf("Shared variance (R%1$s)", "\u00B2"),
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
.tsRhoTableText          <- function(options) {

  text <- gettextf("Pearson correlation coefficient %1$s is a measure of the relationship between two continuous variables. The rules of thumbs for interpreting the measure are:<ul><li>Small: %1$s = 0.10 </li> <li>Medium: %1$s = 0.30</li><li>Large: %1$s = 0.50</li></ul>", "\u03C1")

  if (options[["rhoSharedVariance"]])
    text <- paste0(text, gettextf("- Shared variance (R%1$s) corresponds to proportion of variance shared common between the variables.\n", "\u00B2"))

  return(text)
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

  phiTable <-  createJaspContainer()
  phiTable$position <- 3
  phiTable$dependOn(c("effectSize", "effectSizeValuePhi", "simulateData", "simulateDataN", "pX", "pY", "inputPopulation", "pX1Y1", "pX1Y0", "pX0Y1", "pX0Y0", "setSeed", "seed", "phiOR", "phiRR", "phiRD", "explanatoryTexts"))
  jaspResults[["phiTable"]] <- phiTable


  # frequencies table
  frequenciesTable <- createJaspTable(title = gettext("Frequencies"))
  frequenciesTable$position <- 1

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

  phiTable[["frequenciesTable"]] <- .tsPhiFillFrequencies(frequenciesTable, frequencies)


  # summary statistics table
  statisticsTable <- createJaspTable(title = gettext("Statistics Summary"))
  statisticsTable$position <- 2

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

  frequencies <- list(.tsPhi2Frequencies(
    phi = options[["effectSizeValuePhi"]],
    pX  = options[["pX"]],
    pY  = options[["pY"]]
  ))

  # add simulation characteristics
  if (options[["simulateData"]]){

    data <- jaspResults[["simulatedData"]]$object

    frequencies[[2]] <- as.list(data)
  }

  phiTable[["statisticsTable"]] <- .tsPhiFillTable(statisticsTable, phi, frequencies, options)


  # explanatory texts
  if (options[["explanatoryTexts"]]) {

    explanatoryText <- createJaspHtml()
    explanatoryText$position <- 3

    explanatoryText[["text"]] <- .tsPhiTableText(options)

    phiTable[["explanatoryText"]] <- explanatoryText
  }

  return()
}
.tsPhiPlot                <- function(jaspResults, options) {

  if (!is.null(jaspResults[["phiPlot"]]))
    return()

  phiPlot <-  createJaspContainer()
  phiPlot$position <- 1
  phiPlot$dependOn(c("effectSize", "effectSizeValuePhi", "pX", "pY", "simulateData", "simulateDataN", "plotCombine", "plotPhiMosaic", "plotPhiProportions", "inputPopulation", "pX1Y1", "pX1Y0", "pX0Y1", "pX0Y0", "setSeed", "seed"))
  jaspResults[["phiPlot"]] <- phiPlot


  if (options[["plotCombine"]] && options[["simulateData"]]){

    phiCombinedPlot <- createJaspPlot(title = gettext("Population and simulation distribution"), height = 500, aspectRatio = 1)
    phiCombinedPlot$position <- 1
    phiPlot[["phiCombinedPlot"]] <- phiCombinedPlot

    phiCombinedPlot$plotObject <- .tsPhiMakeCombinedPlot(
      phi     = options[["effectSizeValuePhi"]],
      pX      = options[["pX"]],
      pY      = options[["pY"]],
      data    = jaspResults[["simulatedData"]]$object,
      options = options)

  } else {

    phiPopulationPlot   <- createJaspPlot(title = gettext("Population distribution"), height = 500, aspectRatio = 1)
    phiPopulationPlot$position <- 1
    phiPlot[["phiPopulationPlot"]] <- phiPopulationPlot

    phiPopulationPlot$plotObject <- .tsPhiMakePopulationPlot(
      phi     = options[["effectSizeValuePhi"]],
      pX      = options[["pX"]],
      pY      = options[["pY"]],
      options = options)

    if (options[["simulateData"]]) {

      phiSimulationPlot   <- createJaspPlot(title = gettext("Simulation distribution"), height = 500, aspectRatio = 1)
      phiSimulationPlot$position <- 2
      phiPlot[["phiSimulationPlot"]] <- phiSimulationPlot

      phiSimulationPlot$plotObject <- .tsPhiMakeSimulationPlot(
        frequencies = jaspResults[["simulatedData"]]$object,
        options     = options
      )
    }
  }

  return()
}
.tsPhiSimulateData        <- function(jaspResults, options) {

  .setSeedJASP(options)

  if (is.null(jaspResults[["simulatedData"]])) {

    simulatedData <- createJaspState()
    simulatedData$dependOn(c("effectSize", "effectSizeValueDelta", "simulateDataN", "pX", "pY", "effectSizeValuePhi", "inputPopulation", "pX1Y1", "pX1Y0", "pX0Y1", "pX0Y0", "setSeed", "seed"))

    frequencies <- .tsPhi2Frequencies(phi = options[["effectSizeValuePhi"]], pX = options[["pX"]], pY = options[["pY"]])
    data        <- sample(names(frequencies), options[["simulateDataN"]], replace = TRUE, prob = unlist(frequencies))
    data        <- factor(data, levels = c("A", "B", "C", "D"))
    data        <- table(data)

    simulatedData$object <- data
    jaspResults[["simulatedData"]] <- simulatedData

  }

  return()

}
.tsPhiMakePopulationPlot  <- function(phi, pX, pY, options) {

  frequencies <- .tsPhi2Frequencies(phi = phi, pX = pX, pY = pY)

  if (options[["plotPhiMosaic"]]){
    ticks <- jaspGraphs::getPrettyAxisBreaks(range(c(0, 1)))
    data  <- with(frequencies, data.frame(
      x_start = c(     0,        0,       pX,       pX),
      x_stop  = c(    pX,       pX,        1,        1),
      y_start = c(     0,   C/(pX),        0, D/(1-pX)),
      y_stop  = c(C/(pX),        1, D/(1-pX),        1),
      Outcome = c("P(X=1,Y=0)", "P(X=1,Y=1)", "P(X=0,Y=0)", "P(X=0,Y=1)")
    ))
    lab   <- with(frequencies, data.frame(
      x     = c(      pX/2,           pX/2,     (pX+1)/2,       (pX+1)/2),
      y     = c((C/(pX))/2, (C/(pX) + 1)/2, (D/(1-pX)/2), (D/(1-pX)+1)/2),
      label = round(c(C, A, D, B), 2)
    ))
  } else {
    data  <- with(frequencies, data.frame(
      x_start = c(-sqrt(A),       0, -sqrt(C),        0),
      x_stop  = c(       0, sqrt(B),        0,  sqrt(D)),
      y_start = c(       0,       0, -sqrt(C), -sqrt(D)),
      y_stop  = c( sqrt(A), sqrt(B),        0,        0),
      Outcome = c("P(X=1,Y=1)", "P(X=1,Y=0)", "P(X=0,Y=1)", "P(X=0,Y=0)")
    ))
    lab   <- with(frequencies, data.frame(
      x     = c(-sqrt(A)/2, sqrt(B)/2, -sqrt(C)/2,  sqrt(D)/2),
      y     = c( sqrt(A)/2, sqrt(B)/2, -sqrt(C)/2, -sqrt(D)/2),
      label = round(c(A, B, C, D), 2)
    ))
    ticks <- jaspGraphs::getPrettyAxisBreaks(range(data[,-5]))
  }

  plot <- ggplot2::ggplot(data = data) +
    ggplot2::scale_x_continuous(name = "", breaks = scales::pretty_breaks(n = 3)) +
    ggplot2::scale_y_continuous(name = "", breaks = scales::pretty_breaks(n = 3)) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = x_start,
        xmax = x_stop,
        ymin = y_start,
        ymax = y_stop,
        fill = Outcome),
      color = "black", alpha = 0.5)  +
    ggplot2::scale_fill_discrete(type = jaspGraphs::JASPcolors("colorblind")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  if (options[["plotPhiProportions"]]) {
    plot <- plot + ggplot2::geom_text(
      data    = lab,
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        label = label),
      hjust = 0.5,
      vjust = 0.5
    )
  }

  return(plot)
}
.tsPhiMakeSimulationPlot  <- function(frequencies, options) {

  if (options[["plotPhiMosaic"]]){
    ticks <- jaspGraphs::getPrettyAxisBreaks(range(c(0, 1)))
    data   <- with(as.list(frequencies/sum(frequencies)), data.frame(
      x_start = c(     0,        0,      A+B,      A+B),
      x_stop  = c(   A+B,      A+B,        1,        1),
      y_start = c(     0,  C/(A+B),        0,  D/(C+D)),
      y_stop  = c(C/(A+B),        1, D/(C+D),        1),
      Outcome = c("P(X=1,Y=0)", "P(X=1,Y=1)", "P(X=0,Y=0)", "P(X=0,Y=1)")
    ))
    lab   <- with(as.list(frequencies/sum(frequencies)), data.frame(
      x     = c(    (A+B)/2,       (A+B)/2,   (A+B+1)/2,     (A+B+1)/2),
      y     = c((C/(A+B))/2, (C/(A+B)+1)/2, (D/(C+D))/2, (D/(C+D)+1)/2),
      label = round(c(C, A, D, B), 2)
    ))
  } else {
    data  <- with(as.list(frequencies/sum(frequencies)), data.frame(
      x_start = c(-sqrt(A),       0, -sqrt(C),        0),
      x_stop  = c(       0, sqrt(B),        0,  sqrt(D)),
      y_start = c(       0,       0, -sqrt(C), -sqrt(D)),
      y_stop  = c( sqrt(A), sqrt(B),        0,        0),
      Outcome = c("P(X=1,Y=1)", "P(X=1,Y=0)", "P(X=0,Y=1)", "P(X=0,Y=0)")
    ))
    lab   <- with(as.list(frequencies/sum(frequencies)), data.frame(
      x     = c(-sqrt(A)/2, sqrt(B)/2, -sqrt(C)/2,  sqrt(D)/2),
      y     = c( sqrt(A)/2, sqrt(B)/2, -sqrt(C)/2, -sqrt(D)/2),
      label = round(c(A, B, C, D), 2)
    ))
    ticks <- jaspGraphs::getPrettyAxisBreaks(range(data[,-5]))
  }

  plot <- ggplot2::ggplot(data = data) +
    ggplot2::scale_x_continuous(name = "", breaks = scales::pretty_breaks(n = 3)) +
    ggplot2::scale_y_continuous(name = "", breaks = scales::pretty_breaks(n = 3)) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = x_start,
        xmax = x_stop,
        ymin = y_start,
        ymax = y_stop,
        fill = Outcome),
      color = "black", alpha = 0.5) +
    ggplot2::scale_fill_discrete(type = jaspGraphs::JASPcolors("colorblind")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  if (options[["plotPhiProportions"]]) {
    plot <- plot + ggplot2::geom_text(
      data    = lab,
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        label = label),
      hjust = 0.5,
      vjust = 0.5
    )
  }

  return(plot)
}
.tsPhiMakeCombinedPlot    <- function(phi, pX, pY, data, options) {

  frequencies <- .tsPhi2Frequencies(phi = phi, pX = pX, pY = pY)

  if (options[["plotPhiMosaic"]]){
    ticks <- jaspGraphs::getPrettyAxisBreaks(range(c(0, 1)))
    dataP   <- with(frequencies, data.frame(
      x_start = c(     0,        0,       pX,       pX),
      x_stop  = c(    pX,       pX,        1,        1),
      y_start = c(     0,   C/(pX),        0, D/(1-pX)),
      y_stop  = c(C/(pX),        1, D/(1-pX),        1),
      Outcome = c("P(X=1,Y=0)", "P(X=1,Y=1)", "P(X=0,Y=0)", "P(X=0,Y=1)")
    ))
    dataS   <- with(as.list(data/sum(data)), data.frame(
      x_start = c(      0,        0,     A+B,     A+B),
      x_stop  = c(    A+B,      A+B,       1,       1),
      y_start = c(      0,  C/(A+B),       0, D/(C+D)),
      y_stop  = c(C/(A+B),        1, D/(C+D),       1),
      Outcome = c("P(X=1,Y=0)", "P(X=1,Y=1)", "P(X=0,Y=0)", "P(X=0,Y=1)")
    ))
  } else {
    dataP  <- with(frequencies, data.frame(
      x_start = c(-sqrt(A),       0, -sqrt(C),        0),
      x_stop  = c(       0, sqrt(B),        0,  sqrt(D)),
      y_start = c(       0,       0, -sqrt(C), -sqrt(D)),
      y_stop  = c( sqrt(A), sqrt(B),        0,        0),
      Outcome = c("P(X=1,Y=1)", "P(X=1,Y=0)", "P(X=0,Y=1)", "P(X=0,Y=0)")
    ))
    dataS  <- with(as.list(data/sum(data)), data.frame(
      x_start = c(-sqrt(A),       0, -sqrt(C),        0),
      x_stop  = c(       0, sqrt(B),        0,  sqrt(D)),
      y_start = c(       0,       0, -sqrt(C), -sqrt(D)),
      y_stop  = c( sqrt(A), sqrt(B),        0,        0),
      Outcome = c("P(X=1,Y=1)", "P(X=1,Y=0)", "P(X=0,Y=1)", "P(X=0,Y=0)")
    ))
    ticks <- jaspGraphs::getPrettyAxisBreaks(range(rbind(dataP[,-5], dataS[,-5])))
  }

  plot <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(name = "", breaks = scales::pretty_breaks(n = 3)) +
    ggplot2::scale_y_continuous(name = "", breaks = scales::pretty_breaks(n = 3)) +
    ggplot2::geom_rect(
      data    = dataS,
      mapping = ggplot2::aes(
        xmin = x_start,
        xmax = x_stop,
        ymin = y_start,
        ymax = y_stop,
        fill = Outcome),
      color = "black", alpha = 0.25, linetype = 2)  +
    ggplot2::geom_rect(
      data    = dataP,
      mapping = ggplot2::aes(
        xmin = x_start,
        xmax = x_stop,
        ymin = y_start,
        ymax = y_stop,
        fill = Outcome),
      color = "black", alpha = 0.50)  +
    ggplot2::scale_fill_discrete(type = jaspGraphs::JASPcolors("colorblind")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

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
.tsPhiFillTable           <- function(phiTable2, phi, frequencies, options) {

  # add columns
  phiTable2$addColumnInfo(name = "variable",    title = "",  type = "string")
  phiTable2$addColumnInfo(name = "population",  title = gettext("Population") ,  type = "number")
  if (length(phi) > 1) {
    phiTable2$addColumnInfo(name = "simulation", title = gettext("Simulation"),  type = "number")
    phiTable2$addColumnInfo(name = "lower",      title = gettext("Lower"),       type = "number", overtitle = gettextf("95%% CI"))
    phiTable2$addColumnInfo(name = "upper",      title = gettext("Upper"),       type = "number", overtitle = gettextf("95%% CI"))
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

  if (options[["phiOR"]])
    row4 <- list(
      variable   = gettext("Odds ratio"),
      population = (frequencies[[1]][["A"]] * frequencies[[1]][["D"]]) / (frequencies[[1]][["B"]] * frequencies[[1]][["C"]]))

  if (options[["phiRR"]])
    row5 <- list(
      variable   = gettext("Risk ratio"),
      population = (frequencies[[1]][["A"]] / (frequencies[[1]][["A"]] + frequencies[[1]][["B"]])) /
        (frequencies[[1]][["C"]] / (frequencies[[1]][["C"]] + frequencies[[1]][["D"]])))

  if (options[["phiRD"]])
    row6 <- list(
      variable   = gettext("Risk difference"),
      population = (frequencies[[1]][["A"]] / (frequencies[[1]][["A"]] + frequencies[[1]][["B"]])) -
        (frequencies[[1]][["C"]] / (frequencies[[1]][["C"]] + frequencies[[1]][["D"]])))

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

    if (options[["phiOR"]]){
      tempOR <- metafor::escalc(
        measure = "OR",
        ai      = frequencies[[2]][["A"]],
        bi      = frequencies[[2]][["B"]],
        ci      = frequencies[[2]][["C"]],
        di      = frequencies[[2]][["D"]])
      tempOR <- exp(c(tempOR[["yi"]], tempOR[["yi"]] - 1.96*sqrt(tempOR[["vi"]]), tempOR[["yi"]] + 1.96*sqrt(tempOR[["vi"]])))
      row4 <- c(row4, c(
        simulation = tempOR[1],
        lower      = tempOR[2],
        upper      = tempOR[3]
      ))
    }

    if (options[["phiRR"]]){
      tempRR <- metafor::escalc(
        measure = "RR",
        ai      = frequencies[[2]][["A"]],
        bi      = frequencies[[2]][["B"]],
        ci      = frequencies[[2]][["C"]],
        di      = frequencies[[2]][["D"]])
      tempRR <- exp(c(tempRR[["yi"]], tempRR[["yi"]] - 1.96*sqrt(tempRR[["vi"]]), tempRR[["yi"]] + 1.96*sqrt(tempRR[["vi"]])))
      row5 <- c(row5, c(
        simulation = tempRR[1],
        lower      = tempRR[2],
        upper      = tempRR[3]
      ))
    }

    if (options[["phiRD"]]){
      tempRD <- metafor::escalc(
        measure = "RD",
        ai      = frequencies[[2]][["A"]],
        bi      = frequencies[[2]][["B"]],
        ci      = frequencies[[2]][["C"]],
        di      = frequencies[[2]][["D"]])
      tempRD <- c(tempRD[["yi"]], tempRD[["yi"]] - 1.96*sqrt(tempRD[["vi"]]), tempRD[["yi"]] + 1.96*sqrt(tempRD[["vi"]]))
      row6 <- c(row6, c(
        simulation = tempRD[1],
        lower      = tempRD[2],
        upper      = tempRD[3]
      ))
    }
  }

  phiTable2$addRows(row1)
  phiTable2$addRows(row2)
  phiTable2$addRows(row3)

  if (options[["phiOR"]])
    phiTable2$addRows(row4)

  if (options[["phiRR"]])
    phiTable2$addRows(row5)

  if (options[["phiRD"]])
    phiTable2$addRows(row6)

  return(phiTable2)
}
.tsPhiTableText           <- function(options) {

  text <- gettextf("Contingency coefficient %1$s is a measure of the relationship between two nominal variables. The rules of thumbs for interpreting the measure are:<ul><li>Weak: %1$s = 0.20 </li> <li>Moderate: %1$s = 0.30</li><li>Strong: %1$s = 0.40</li><li>Very Strong: %1$s = 0.70</li></ul>", "\u03C6")

  if (options[["phiOR"]])
    text <- paste0(text, gettext("- Odds ratio corresponds to the ratio of the odds of an event occuring in the experimental and control groups.\n"))

  if (options[["phiRR"]])
    text <- paste0(text, gettext("- Risk ratio corresponds to the ratio of the risk of an event occuring in the experimental and control groups.\n"))

  if (options[["phiRD"]])
    text <- paste0(text, gettext("- Risk difference corresponds to the difference in the risk of the event occuring between the experimental and control groups.\n"))

  return(text)
}
# to be deleted once deemed useless
# .tsPhiMakePopulationDPlot <- function(phi, pX, pY) {
#
#   frequencies <- .tsPhi2Frequencies(phi = phi, pX = pX, pY = pY)
#   yTicks      <- jaspGraphs::getPrettyAxisBreaks(range(c(0, unlist(frequencies))))
#
#   plot <- ggplot2::ggplot(
#     data    = data.frame(
#       X = factor(c(1, 1, 0, 0)),
#       Y = factor(c(1, 0, 1, 0)),
#       p = c(frequencies[["A"]], frequencies[["C"]], frequencies[["B"]], frequencies[["D"]])
#     ),
#     mapping = ggplot2::aes(fill = Y, y = p, x = X)) +
#     ggplot2::geom_bar(position = "dodge", stat = "identity") +
#     ggplot2::scale_y_continuous(
#       expression(P(X,Y)),
#       breaks = yTicks,
#       limits = range(yTicks)
#       ) +
#     jaspGraphs::geom_rangeframe() +
#     jaspGraphs::themeJaspRaw(legend.position = "right")
#
#   return(plot)
# }
# .tsPhiMakeSimulationDPlot <- function(data) {
#
#   yTicks      <- jaspGraphs::getPrettyAxisBreaks(c(0, range(unlist(data))))
#
#   plot <- ggplot2::ggplot(
#     data    = data.frame(
#       X = factor(c(1, 1, 0, 0)),
#       Y = factor(c(1, 0, 1, 0)),
#       p = c(data[["A"]], data[["C"]], data[["B"]], data[["D"]])
#     ),
#     mapping = ggplot2::aes(fill = Y, y = p, x = X)) +
#     ggplot2::geom_bar(position = "dodge", stat = "identity") +
#     ggplot2::scale_y_continuous(
#       "Counts",
#       breaks = yTicks,
#       limits = range(yTicks)
#     ) +
#     jaspGraphs::geom_rangeframe() +
#     jaspGraphs::themeJaspRaw(legend.position = "right")
#
#   return(plot)
# }
