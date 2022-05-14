#
# Copyright (C) 2013-2022 University of Amsterdam
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

pValues <- function(jaspResults, dataset = NULL, options) {
  #save(options, file = "~/Downloads/opts.Rdata")
  if(options[["introText"]])
    .pvIntroText(jaspResults, options, position = 1)

  nullDistribution <- .pvDistribution(options, null = TRUE)
  .pvTheoreticalDistribution (jaspResults, nullDistribution, options, position = 2)
  .pvNullHypothesisSimulation(jaspResults, nullDistribution, options, position = 3)

  altDistribution <- .pvDistribution(options, null = FALSE)
  .pvAlternativeHypothesisSimulation(jaspResults, nullDistribution, altDistribution, options, position = 4)
}

# jaspResults helpers ----
.pvGetContainer <- function(jaspContainer, name, title = "", dependencies = NULL, position = NULL, initCollapsed = FALSE) {
  if(is.null(jaspContainer[[name]])) {
    container <- createJaspContainer(
      title = title, dependencies = dependencies, position = position, initCollapsed = initCollapsed
    )
    jaspContainer[[name]] <- container
  } else {
    container <- jaspContainer[[name]]
  }

  return(container)
}

.pvIntroText <- function(container, options, text = gettext("to be added..."), position = 1) {
  if(isFALSE(options[["introText"]]) || !is.null(container[["introText"]])) return()

  introText <- createJaspHtml(dependencies = "introText", position = position)
  container[["introText"]] <- introText

  introText$text <- text
}

# Theoretical distribution ----
.pvTheoreticalDistribution <- function(jaspResults, distribution, options, position) {
  if(isFALSE(options[["plotTheoretical"]])) return()

  container <- .pvGetContainer(
    jaspContainer = jaspResults,
    name          = "theoreticalDistribution",
    title         = gettext("Theoretical distribution of the test statistic under H<sub>0</sub>"),
    dependencies  = "plotTheoretical",
    position      = position
  )

  .pvIntroText(container, options, position = 1)
  .pvPlotTheoreticalDistribution(container, distribution, options, position = 2)

  return()
}


.pvPlotTheoreticalDistribution <- function(container, distribution, options, position) {
  if(!is.null(container[["plot"]])) return()

  plotContainer <- createJaspPlot(
    title        = gettext("Distribution plot"),
    dependencies = c(.pvDistributionDependencies,
                     "alternative", "alpha",
                     "plotTheoretical", "plotTheoreticalCriticalRegion",
                     "plotTheoreticalStatistic", "plotTheoreticalTestStatistic",
                     "introText"),
    width        = 500,
    position     = position
  )
  container[["plot"]] <- plotContainer

  plotContainer$plotObject <- .pvFillPlotTheoreticalDistribution(distribution, options)
}

.pvFillPlotTheoreticalDistribution <- function(distribution, options) {
  plot <- ggplot2::ggplot()

  if(options[["plotTheoreticalCriticalRegion"]]) {
    criticalRegions <- .pvGetCriticalRegions(distribution, options[["alternative"]], options[["alpha"]])
    for(criticalRegion in criticalRegions) {
      if(is.null(criticalRegion)) next
      plot <- .pvAddCurveToPlot(plot, fun = distribution$pdf, xlim = criticalRegion,
                                plotLine = FALSE, plotArea = TRUE,
                                areaColor = "steelblue", areaAlpha = 0.5)
    }
  }

  if(options[["plotTheoreticalStatistic"]]) {
    pValue       <- .pvGetPValue(distribution, options[["alternative"]], options[["plotTheoreticalTestStatistic"]])
    colorRegions <- .pvGetCriticalRegions(distribution, options[["alternative"]], pValue)

    for(region in colorRegions) {
      if(is.null(region)) next
      plot <- .pvAddCurveToPlot(plot, fun = distribution$pdf, xlim = region,
                                plotLine = FALSE, plotArea = TRUE,
                                areaColor = "red", areaAlpha = 0.5)
    }
  }

  plot <- .pvAddCurveToPlot(plot, fun = distribution$pdf, xlim = distribution$limits)

  plot <- plot +
    ggplot2::xlab(gettext("Test statistic")) +
    ggplot2::ylab(gettext("Density")) +
    ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(distribution$limits)) +
    jaspGraphs::scale_y_continuous() +
    jaspGraphs::themeJaspRaw()

  return(plot)
}

# Dance under the null ----
.pvNullHypothesisSimulation <- function(jaspResults, distribution, options, position) {
  container <- .pvGetContainer(
    jaspContainer = jaspResults,
    name          = "nullHypothesisSimulation",
    title         = gettext("Simulation under H<sub>0</sub>"),
    dependencies  = c(.pvDistributionDependencies,
                      "alternative", "alpha", "nullHypothesisReset")
  )

  # Determine whether or not to draw samples (workaround for mimicking an action button with a checkbox)
  if(!is.null(container[["simulate"]])) {
    simulate <- FALSE
  } else {
    simulate <- TRUE
    container[["simulate"]] <- createJaspState(object = options[["nullHypothesisSimulate"]], dependencies = "nullHypothesisSimulate")
  }

  # Simulate data and add them to a state
  if(!is.null(container[["nullHypothesisData"]])) {
    data <- container[["nullHypothesisData"]]$object
  } else {
    data <- numeric(0)
    #simulate <- FALSE
  }

  if(simulate) {
    data <- c(data, distribution$rng(options[["nullHypothesisStudiesToSimulate"]]))
  }
  container[["nullHypothesisData"]] <- createJaspState(object = data)

  # Output
  .pvIntroText         (container,                     options, position = 1)
  if (options[["nullHypothesisPlotTestStatistics"]])
    .pvPlotTestStatistics(container, data, distribution, options = options, position = 2,
                          plotNullCurve = options[["nullHypothesisPlotTestStatisticsOverlayTheoretical"]])
  if (options[["nullHypothesisPlotPValues"]])
    .pvPlotPValues       (container, data, distribution, options, position = 3,
                          plotUniform = options[["nullHypothesisPlotPValuesOverlayUniform"]])
  if (options[["nullHypothesisFrequencyTable"]])
    .pvFrequencyTable(container, data, distribution, options, position = 4)
}

.pvPlotTestStatistics <- function(container, data, nullDistribution, altDistribution = NULL, options, position,
                                  plotNullCurve = FALSE, plotAltCurve = FALSE) {
  if(!is.null(container[["plotTestStatistics"]])) return()

  plot <- createJaspPlot(
    title    = gettext("Observed test statistics"),
    position = position,
    width    = 500,
    dependencies = c("nullHypothesisSimulate", "alternativeHypothesisSimulate", "nullHypothesisPlotTestStatistics",
                     "alternativeHypothesisPlotTestStatistics", "nullHypothesisPlotTestStatisticsOverlayTheoretical",
                     "alternativeHypothesisPlotTestStatisticsOverlayNull", "alternativeHypothesisPlotTestStatisticsOverlayAlternative")
    )
  container[["plotTestStatistics"]] <- plot

  if(length(data) == 0) return()
  dataForLowerLimits <- c(data, nullDistribution$limits["lower"])
  dataForUpperLimits <- c(data, nullDistribution$limits["upper"])
  if (!is.null(altDistribution)) {
    dataForLowerLimits <- c(dataForLowerLimits, altDistribution$limits["lower"])
    dataForUpperLimits <- c(dataForUpperLimits, altDistribution$limits["upper"]) 
  }
  xlim <- c(
    min(dataForLowerLimits),
    max(dataForUpperLimits)
    )
  df   <- data.frame(data = data)

  pp <- ggplot2::ggplot(df, ggplot2::aes(x = data)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), col = "black", fill = "gray") +
    ggplot2::geom_rug() +
    jaspGraphs::scale_x_continuous(limits = xlim) +
    jaspGraphs::scale_y_continuous() +
    ggplot2::xlab(gettext("Test statistic")) +
    ggplot2::ylab(gettext("Density")) +
    jaspGraphs::themeJaspRaw()

  if(plotNullCurve) {
    pp <- .pvAddCurveToPlot(pp, fun = nullDistribution$pdf, xlim = xlim)
  }
  if(plotAltCurve) {
    pp <- .pvAddCurveToPlot(pp, fun = altDistribution$pdf, xlim = xlim)
  }

  plot$plotObject <- pp
}

.pvPlotPValues <- function(container, data, distribution, options, position, plotUniform = FALSE) {
  if(!is.null(container[["plotPValues"]])) return()

  plot <- createJaspPlot(
    title        = gettext("Observed p-values"),
    position     = position,
    width        = 500,
    dependencies = c("nullHypothesisSimulate", "nullHypothesisPlotPValues", "nullHypothesisPlotPValuesOverlayUniform"))
  container[["plotPValues"]] <- plot

  if(length(data) == 0) return()

  df <- data.frame(data = vapply(data, function(x) .pvGetPValue(distribution, options[["alternative"]], x), numeric(1)))

  pp <- ggplot2::ggplot(df, ggplot2::aes(x = data)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), col = "black", fill = "gray", breaks = seq(0, 1, by = 0.05)) +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05)) +
    jaspGraphs::scale_y_continuous() +
    ggplot2::xlab(gettext("p-values"))+
    ggplot2::ylab(gettext("Density")) +
    jaspGraphs::themeJaspRaw()

  if(plotUniform) {
    pp <- pp + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 1), size = 1.5)
  }

  plot$plotObject <- pp
}

.pvFrequencyTable <- function(container, data, distribution, options, position) {
  if(!is.null(container[["frequencyTable"]])) return()

  table <- createJaspTable(
    title        = gettext("Frequency Table"),
    position     = position,
    dependencies = c("nullHypothesisSimulate", "nullHypothesisFrequencyTable",
                     "alternativeHypothesisSimulate","alternativeHypothesisFrequencyTable")
    )

  table$addColumnInfo(name = "label",   title = "",                 type = "string")
  table$addColumnInfo(name = "count",   title = gettext("Count"),   type = "integer")
  table$addColumnInfo(name = "percent", title = gettext("Percent"), type = "number")
  container[["frequencyTable"]] <- table

  significant <- switch(
    options[["alternative"]],
    "greater"  = data > distribution$qf(options[["alpha"]],   lower.tail = FALSE),
    "less"     = data < distribution$qf(options[["alpha"]],   lower.tail = TRUE),
    "twoSided" = data > distribution$qf(options[["alpha"]]/2, lower.tail = FALSE) | data < distribution$qf(options[["alpha"]]/2, lower.tail = TRUE)
  )

  total <- length(data)
  table$addRows(
    list(
      label = gettext("Significant"),
      count = sum(significant),
      percent = if(total == 0) NA else 100*sum(significant)/total
    )
  )
  table$addRows(
    list(
      label = gettext("Not significant"),
      count = sum(!significant),
      percent = if(total == 0) NA else 100*sum(!significant)/total
    )
  )
  table$addRows({
    list(
      label = gettext("Total"),
      count = total,
      percent = if(total == 0) NA else 100
    )
  })
}

# Dance under the alternative ----
.pvAlternativeHypothesisSimulation <- function(jaspResults, nullDistribution, altDistribution, options, position = 4) {
  container <- .pvGetContainer(
    jaspContainer = jaspResults,
    name          = "alternativeHypothesisSimulation",
    title         = gettext("Simulation under H<sub>a</sub>"),
    dependencies  = c(.pvDistributionDependencies,
                      "alternative", "alpha", "alternativeHypothesisReset")
  )
  
  
  # Determine whether or not to draw samples (workaround for mimicking an action button with a checkbox)
  if(!is.null(container[["simulate"]])) {
    simulate <- FALSE
  } else {
    simulate <- TRUE
    container[["simulate"]] <- createJaspState(object = options[["alternativeHypothesisSimulate"]], dependencies = "alternativeHypothesisSimulate")
  }
  
  # Simulate data and add them to a state
  if(!is.null(container[["alternativeHypothesisData"]])) {
    data <- container[["alternativeHypothesisData"]]$object
  } else {
    data <- numeric(0)
    #simulate <- FALSE
  }
  
  if(simulate) {
    data <- c(data, altDistribution$rng(options[["alternativeHypothesisStudiesToSimulate"]]))
  }
  container[["alternativeHypothesisData"]] <- createJaspState(object = data)
  
  # Output
  .pvIntroText         (container,                     options, position = 5)
  if (options[["alternativeHypothesisPlotTestStatistics"]])
    .pvPlotTestStatistics(container, data, nullDistribution, altDistribution, options, position = 6,
                          plotNullCurve = options[["alternativeHypothesisPlotTestStatisticsOverlayNull"]],
                          plotAltCurve = options[["alternativeHypothesisPlotTestStatisticsOverlayAlternative"]])
  if (options[["alternativeHypothesisPlotPValues"]])
    .pvPlotPValues       (container, data, nullDistribution, options, position = 7)
  if (options[["alternativeHypothesisFrequencyTable"]])
    .pvFrequencyTable(container, data, nullDistribution, options, position = 8)
}

# Helpers ----
## Distributions ----
.pvDistribution <- function(options, null = TRUE) {
  distribution <- switch (options[["distribution"]],
    "tDistribution" = .pvTDistribution(df = options[["tDf"]], ncp = if(null) 0 else options[["tNcp"]]),
    "normal"        = .pvNormalDistribution(mean = if(null) 0 else options[["normalMean"]], sd = 1)
  )

  return(distribution)
}

.pvTDistribution <- function(df = 1, ncp = 0) {
  output <- list(
    pdf = function(x)                    dt(x, df = df, ncp = ncp),
    cdf = function(q, lower.tail = TRUE) pt(q, df = df, ncp = ncp, lower.tail = lower.tail),
    qf  = function(p, lower.tail = TRUE) qt(p, df = df, ncp = ncp, lower.tail = lower.tail),
    rng = function(n)                    rt(n, df = df, ncp = ncp),
    limits = c(lower = ncp - 5, upper = ncp + 5)
  )

  return(output)
}

.pvNormalDistribution <- function(mean = 0, sd = 1) {
  output <- list(
    pdf = function(x)                    dnorm(x, mean = mean, sd = sd),
    cdf = function(q, lower.tail = TRUE) pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail),
    qf  = function(p, lower.tail = TRUE) qnorm(p, mean = mean, sd = sd, lower.tail = lower.tail),
    rng = function(n)                    rnorm(n, mean = mean, sd = sd),
    limits = c(lower = mean - 5 * sd, upper = mean + 5 * sd)
  )

  return(output)
}

### Dependencies ----
.pvDistributionDependencies <- c("distribution", "tDf", "tNcp", "normalMean")

### Get distribution quantities ----
.pvGetPValue <- function(distribution, hypothesis = c("twoSided", "greater", "less"), statistic) {
  hypothesis <- match.arg(hypothesis)

  pValue <- switch(
    hypothesis,
    "twoSided" = 2*distribution$cdf(q = min(c(statistic, -1*statistic)), lower.tail = TRUE), # this won't work for e.g., binomial -> fix this
    "greater"  =   distribution$cdf(q = statistic,                       lower.tail = FALSE),
    "less"     =   distribution$cdf(q = statistic,                       lower.tail = TRUE)
  )

  return(pValue)
}

.pvGetCriticalRegions <- function(distribution, hypothesis = c("twoSided", "greater", "less"), alpha = 0.05) {
  hypothesis <- match.arg(hypothesis)

  regions <- list(
    lower = switch(hypothesis,
                   "twoSided" = c(distribution$limits[["lower"]], distribution$qf(alpha/2)),
                   "greater"  = NULL,
                   "less"     = c(distribution$limits[["lower"]], distribution$qf(alpha))
    ),
    upper = switch(hypothesis,
                   "twoSided" = c(distribution$qf(1-alpha/2), distribution$limits[["upper"]]),
                   "greater"  = c(distribution$qf(1-alpha),   distribution$limits[["upper"]]),
                   "less"     = NULL
    )
  )

  return(regions)
}

### Plotting ----
.pvAddCurveToPlot <- function(plot, fun, xlim, n = 201,
                              plotLine = TRUE, plotArea = FALSE,
                              lineSize = 1.5, lineColor = "black", lineAlpha = 1,
                              areaColor = "steelblue", areaAlpha = 0.5) {
  x <- seq(xlim[1], xlim[2], length.out = n)
  df <- data.frame(x = x, y = fun(x))

  if(plotArea) {
    plot <- plot +
      ggplot2::geom_area(data = df, mapping = ggplot2::aes(x = x, y = y), fill = areaColor, alpha = areaAlpha)
  }

  if(plotLine) {
    plot <- plot +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = y), color = lineColor, size = lineSize, alpha = lineAlpha)
  }

  return(plot)
}

