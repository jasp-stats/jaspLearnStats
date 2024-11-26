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

LSTdescriptives <- function(jaspResults, dataset, options, state = NULL) {
  inputType <- options[["lstDescDataType"]]
  colors <- .getColors(options[["descColorPalette"]])
  ready <- (inputType == "dataRandom" |
              (inputType == "dataSequence" && options[["lstDescDataSequenceInput"]] != "") |
              (inputType == "dataVariable" && options[["selectedVariable"]] != ""))
  data <- .getDataLSTdesc(jaspResults, options, inputType)

  #checking whether data is discrete or continuous, whereas only integers are treated as discrete
  discrete <- ifelse(all(data$x == as.integer(data$x)), TRUE, FALSE)

  stats <- switch(options[["LSdescStatistics"]],
                            "LSdescMean" = "ct", "LSdescMedian" = "ct", "LSdescMode" = "ct", "LSdescMMM" = "ct",
                            "LSdescRange" = "spread", "LSdescQR" = "spread", "LSdescSD" = "spread",
                            "none" = "none")


  if (options[["LSdescHistBar"]])
    .lstDescCreateHistogramOrBarplot(jaspResults, options, data, ready, discrete, stats = stats, colors)
  if (options[["LSdescDotPlot"]])
    .lstDescCreateDotplot(jaspResults, options, data, ready, discrete, stats = stats, colors)
  if (options[["LSdescExplanation"]])
    .descExplanation(jaspResults, options, colors, stats = stats)
}


.descExplanation <- function(jaspResults, options, colors, stats = c("ct", "spread", "none")) {
  jaspResults[["descExplanation"]] <- createJaspContainer(gettext("Explanation"))
  jaspResults[["descExplanation"]]$position <- 1
  jaspResults[["descExplanation"]]$dependOn(options = c("LSdescStatistics",
                                                        "LSdescExplanation"))

  if (stats == "none") {
    return()
  } else if (stats == "ct") {
    plot1 <- createJaspPlot(title = gettext("Theoretical example distribution"), width = 700, height = 400)
    plot1$position <- 1

    mean <- 0
    sd <- 1
    skew <- 1000
    distLimits <- c(mean - 4 * sd, mean + 4 * sd)

    data <- data.frame(x = .scaledSkewedNormal(100000, xi = mean, omega = sd, alpha = skew))
    data <- subset(data, data$x > distLimits[1] & data$x < distLimits[2]) # remove values outside limits

    plot1Object <- .plotCTexampleDistribution(data)

    allCTs <- options[["LSdescStatistics"]] == "LSdescMMM"

    if (options[["LSdescStatistics"]] == "LSdescMedian" | allCTs) {
      plot1Object <- .visualExplanationMedian(plot1Object, data, options, colors)
      text <- "Text for Median:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    }
    if (options[["LSdescStatistics"]] == "LSdescMode" | allCTs) {
      plot1Object <- .visualExplanationMode(plot1Object, data, options, colors)
      text <- "Text for Mode:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    }
    if (options[["LSdescStatistics"]] == "LSdescMean" | allCTs) {
      plot1Object <- .visualExplanationMean(plot1Object, data, options, colors)
      text <- "Text for Mean : Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    }
    if (allCTs) {
      text <- "Text for comparison:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    }
    plot1$plotObject <- plot1Object
  } else if(stats == "spread") {
    data <- data.frame(index = 1:21, x = rnorm(21, 10, 2.5))
    plot1 <- createJaspPlot(title = gettext("Visual Explanation"), width = 700, height = 700)

    if (options[["LSdescStatistics"]] == "LSdescRange") {
      plot1$plotObject <- .visualExplanationRange(data, colors)
    } else if (options[["LSdescStatistics"]] == "LSdescQR") {
      plot1$plotObject <- .visualExplanationQuartiles(data, colors)
    } else if (options[["LSdescStatistics"]] == "LSdescSD") {
      plot1$plotObject <- .visualExplanationStdDev(data, colors)$plot1
      plot2 <- createJaspPlot(title = gettext("Visual Explanation 2"), width = 700, height = 700)
      plot2$plotObject <- .visualExplanationStdDev(data, colors)$plot2
      jaspResults[["descExplanation"]][["Plot2"]] <- plot2
      jaspResults[["descExplanation"]][["Plot2"]]$position <- 3
      plot3 <- createJaspPlot(title = gettext("Visual Explanation 3"), width = 700, height = 700)
      plot3$plotObject <- .visualExplanationStdDev(data, colors)$plot3
      jaspResults[["descExplanation"]][["Plot3"]] <- plot3
      jaspResults[["descExplanation"]][["Plot3"]]$position <- 4
      plot4 <- createJaspPlot(title = gettext("Visual Explanation 4"), width = 700, height = 700)
      plot4$plotObject <- .visualExplanationStdDev(data, colors)$plot4
      jaspResults[["descExplanation"]][["Plot4"]] <- plot4
      jaspResults[["descExplanation"]][["Plot4"]]$position <- 5
    }
    text <- "Text for comparison:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  }
  jaspResults[["descExplanation"]][["Plot"]] <- plot1
  jaspResults[["descExplanation"]][["Plot"]]$position <- 1
  jaspResults[["descExplanation"]][["Text"]] <- createJaspHtml(text, "p")
  jaspResults[["descExplanation"]][["Text"]]$position <- 2
}

.visualExplanationRange <- function(data, colors) {
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  yBreaks <- c(1, 6, 11, 16, 21)
  sortedDf <- data[order(data$x),]
  sortedDf <- cbind(sortedDf, list("extreme" = c("min", rep("normal", 19), "max")))

  plotObject <- ggplot2::ggplot() +
    ggplot2::geom_point(data = sortedDf, mapping = ggplot2::aes(x = x, y = index, fill = extreme),
                        size = 6, color = "black", shape = 21) +
    ggplot2::scale_fill_manual(values = c("min" = colors[2], "normal" = "gray", "max" = colors[3]))

  minY <- which.min(data$x)
  minX <- min(data$x)
  minLineData <- data.frame(x = rep(minX, 2), y = c(minY, -.5))
  maxY <- which.max(data$x)
  maxX <- max(data$x)
  maxLineData <- data.frame(x = rep(maxX, 2), y = c(maxY, -.5))
  rangeLineData <- data.frame(x = c(minX, maxX), y = rep(-.5, 2))
  arrowHeadData1 <- data.frame(x = c(minX, minX + .3, minX + .3), y = c(-.5, 0, -1))
  arrowHeadData2 <- data.frame(x = c(maxX, maxX - .3, maxX - .3), y = c(-.5, 0, -1))
  rangeLabelData <- data.frame(x = (maxX + minX) / 2, y = -.5, label = gettext("Range"))
  minLabelData <- data.frame(x = minX, y = (minY - .5) / 2, label = gettext("Min"))
  maxLabelData <- data.frame(x = maxX, y = (maxY - .5) / 2, label = gettext("Max"))

  plotObject <- plotObject +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = minLineData, color = colors[2], size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = maxLineData, color = colors[3], size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = rangeLineData, color = colors[4], size = 1.5) +
    ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = arrowHeadData1, fill = colors[4]) +
    ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = arrowHeadData2, fill = colors[4]) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = rangeLabelData, color = colors[4], size = 6) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = minLabelData, color = colors[2], size = 6) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = maxLabelData, color = colors[3], size = 6) +
    ggplot2::scale_y_continuous(name = "Observation No.", breaks = yBreaks, limits = c(-1, 21)) +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = range(xBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  return(plotObject)
}

.visualExplanationQuartiles <- function(data, colors) {
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  yBreaks <- c(1, 6, 11, 16, 21)
  quartiles <- quantile(data$x, type = 2)
  twentyfivePercentLabels <- data.frame(x = c(sum(quartiles[1:2])/2, sum(quartiles[2:3])/2,
                                              sum(quartiles[3:4])/2, sum(quartiles[4:5])/2),
                                        y = rep(11, 4), label = rep("25%", 4))
  q1LineData <- data.frame(x = rep(quartiles[2], 2), y = c(21, -.5))
  q2LineData <- data.frame(x = rep(quartiles[3], 2), y = c(21, 1))
  q3LineData <- data.frame(x = rep(quartiles[4], 2), y = c(21, -.5))
  iqrLineData <- data.frame(x = c(quartiles[2], quartiles[4]), y = rep(-.5, 2))
  arrowHeadData1 <- data.frame(x = c(quartiles[2], quartiles[2] + .3, quartiles[2] + .3), y = c(-.5, 0, -1))
  arrowHeadData2 <- data.frame(x = c(quartiles[4], quartiles[4] - .3, quartiles[4] - .3), y = c(-.5, 0, -1))
  labelData <- data.frame(x = c(rep(max(xBreaks)+2, 3), mean(data$x)), y = c(20, 18, 16, -.5),
                          label = gettext("1st quartile", "2nd quartile / \n Median", "3rd quartile", "IQR"))
  plotObject <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x, y = index)) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = min(data$x), xmax = quartiles[2]), fill = "grey", alpha = .2) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = quartiles[2], xmax = quartiles[3]), fill = "grey", alpha = .8) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = quartiles[3], xmax = quartiles[4]), fill = "grey", alpha = .4) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = quartiles[4], xmax = quartiles[5]), fill = "grey", alpha = .6) +
    ggplot2::geom_point(size = 6, fill = "grey", color = "black", shape = 21) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q1LineData, color = colors[6], size = 1, alpha = .6) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q2LineData, color = colors[1], size = 1, alpha = .6) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q3LineData, color = colors[5], size = 1, alpha = .6) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = iqrLineData, color = colors[4], size = 1) +
    ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = arrowHeadData1, fill = colors[4]) +
    ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = arrowHeadData2, fill = colors[4]) +
    ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label), data = twentyfivePercentLabels, size = 6) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData,
                        color = colors[c(6, 1, 5, 4)], size = 6) +
    ggplot2::scale_y_continuous(name = "Observation No.", breaks = yBreaks, limits = c(-1, 21)) +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = c(min(xBreaks), max(xBreaks) + 5)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  return(plotObject)
}

.visualExplanationVariance <- function(data, colors, plotVarLabel = TRUE) {
  xBreaks1 <- jaspGraphs::getPrettyAxisBreaks(data$x)
  yBreaks <- c(1, 6, 11, 16, 21)
  meanPoint <- mean(data$x)
  meanLineData <- data.frame(x = rep(meanPoint, 2), y = c(22, 0))
  labelDataMean <- data.frame(x = meanPoint, y = 22, label = gettext("Mean"))
  plotObject1 <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x, y = index)) +
    ggplot2::geom_point(size = 6, fill = "grey", color = "black", shape = 21) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = colors[3], alpha = .7) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelDataMean, size = 6, color = colors[3]) +
    ggplot2::scale_y_continuous(name = "Observation No.", breaks = yBreaks, limits = c(-1, 22)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plotObject2 <- plotObject1
  plotObject1 <- plotObject1 +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks1, limits = range(xBreaks1))

  for (i in seq_along(data$x)) {
    devLineData <- data.frame(x = c(data$x[i], meanPoint), y = rep(data$index[i], 2))
    plotObject1 <- plotObject1 +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = devLineData, size = 1, color = colors[4])
  }

  #plot with squared distances to mean
  distances <- data$x - meanPoint
  distancesSquared <- distances^2

  for (i in seq_along(distancesSquared)) {
    devLineData <- data.frame(x = c(meanPoint, meanPoint + distancesSquared[i]), y = rep(i, 2))
    plotObject2 <- plotObject2 +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = devLineData, size = 1, color = colors[4])
  }
  varLineData <- data.frame(x = c(meanPoint, meanPoint + var(data$x)), y = rep(0, 2))
  xBreaks2 <- jaspGraphs::getPrettyAxisBreaks(c(data$x, meanPoint + distancesSquared))
  plotObject2 <- plotObject2 +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = varLineData, size = 2, color = colors[6]) +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks2, limits = range(xBreaks2))

  if (plotVarLabel) {
    labelDataVar <- data.frame(x = meanPoint + var(data$x)/2, y = -1, label = gettext("Variance"))
    plotObject2 <- plotObject2 +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelDataVar, size = 6, color = colors[6])
  }

  return(list("plot1" = plotObject1,
              "plot2" = plotObject2))
}

.visualExplanationStdDev <- function(data, colors) {
  # plot 1
  plotObject1 <- .visualExplanationVariance(data, colors)$plot1

  #Plot 2
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  yBreaks <- c(1, 6, 11, 16, 21)
  stdDev <- sd(data$x)
  meanPoint <- mean(data$x)
  meanLineData <- data.frame(x = rep(meanPoint, 2), y = c(21, 1))
  labelData <- data.frame(x = meanPoint, y = 21, label = gettext("Mean"))
  sdLabels <- data.frame(x = c(sum(min(xBreaks), meanPoint - 2*stdDev)/2, sum(meanPoint - stdDev, meanPoint - 2*stdDev)/2,
                               sum(meanPoint, meanPoint - stdDev)/2, sum(meanPoint, meanPoint + stdDev)/2,
                               sum(meanPoint + stdDev, meanPoint + 2*stdDev)/2, sum(meanPoint + 2*stdDev, meanPoint + 3*stdDev)/2),
                         y = rep(11, 6), label = gettext("-3 SD", "-2 SD", "-1 SD", "+1 SD", "+2 SD", "+3 SD"))
  plotObject2 <- .visualExplanationVariance(data, colors, plotVarLabel = FALSE)$plot2
  labelData2 <- data.frame(x = rep(meanPoint + 1.5 * var(data$x), 2), y = c(0, -1),
                           label = c(gettext("Variance"), gettext("Std. Dev.")))
  meanLineExtensionData <- data.frame(x = rep(meanPoint, 2), y = c(0, -1))
  stdDevLineData <- data.frame(x = c(meanPoint, meanPoint + stdDev), y = rep(-1, 2))
  plotObject2 <- plotObject2 +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData2, size = 6,
                        color = colors[c(6, 2)]) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = stdDevLineData, size = 2, color = colors[2]) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineExtensionData, size = 1, color = colors[3], alpha = .7)

  #Plot 3
  plotObject3 <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x, y = index)) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = min(xBreaks), xmax = max(xBreaks)), fill = colors[6]) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = meanPoint - 2 * stdDev, xmax = meanPoint + 2 * stdDev),
                         fill = colors[5]) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = meanPoint - stdDev, xmax = meanPoint + stdDev), fill = colors[4]) +
    ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label), data = sdLabels, size = 6) +
    ggplot2::geom_point(size = 6, fill = "grey", color = "black", shape = 21) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = colors[3], alpha = .7) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = 6, color = colors[3]) +
    ggplot2::scale_y_continuous(name = "Observation No.", breaks = yBreaks, limits = c(0, 21)) +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = range(xBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  #plot 4
  xBreaks2 <- -4:4
  yBreaks2 <- seq(0, .5, .1)
  yLabels2 <- rep("", length(yBreaks2))
  percentLabelData <- data.frame(x = c(-2.3, -1.5, -.5, .5, 1.5, 2.3), y = c(.01, .05, .2, .2, .05, .01),
                                 label = c("2.1%", "13.6%", "34.1%", "34.1%", "13.6%", "2.1%"))
  sdLabelsData2 <- data.frame(x = c(-3.3, -2.3, -1.4, 0,  1.4,  2.3,  3.3), y = c(.02, .07, .25, .41, .25, .07, .02),
                              label = c(paste(-3:-1, "SD"), "Mean", paste("+", 1:3, " SD", sep = "")))
  colorPalette <- c(colors[6:3], colors[4:6])

  plotObject4 <- ggplot2::ggplot(data = data.frame(x = xBreaks2), mapping = ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), geom = "area",
                           color = "black", fill = "white") +
    ggplot2::geom_area(stat = "function", fun = dnorm, fill = colors[6], color = "black", xlim = c(-3, 3)) +
    ggplot2::geom_area(stat = "function", fun = dnorm, fill = colors[5], color = "black", xlim = c(-2, 2)) +
    ggplot2::geom_area(stat = "function", fun = dnorm, fill = colors[4], color = "black", xlim = c(-1, 1)) +
    ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label), data = percentLabelData, size = 6) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = sdLabelsData2, size = 6,
                        color = colorPalette) +
    ggplot2::scale_x_continuous(breaks = xBreaks2, limits = range(xBreaks2), name =) +
    ggplot2::scale_y_continuous(breaks = yBreaks2, limits = range(yBreaks2), labels = yLabels2, name = "") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.line.x = ggplot2::element_line(color = "black", size = .5))

  # plot lines
  plotData <- ggplot2::ggplot_build(plotObject4)$data[[1]]
  xPosLines <- -3:3
  colors <- colorPalette
  for (i in seq_along(xPosLines)) {
    indexOfyMax <- .indexOfNearestValue(xPosLines[i], plotData$x)
    lineData <- data.frame(x = rep(xPosLines[i], 2), y = c(0, plotData$ymax[indexOfyMax]))
    plotObject4 <- plotObject4 +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = lineData, size = 1, color = colors[i], alpha = .7)
  }



  return(list("plot1" = plotObject1,
              "plot2" = plotObject2,
              "plot3" = plotObject3,
              "plot4" = plotObject4))
}

.indexOfNearestValue <- function(targetValue, valueSet) {
  nearestPointIndex <- which.min(abs(valueSet - targetValue))
  return(nearestPointIndex)
}

.plotCTexampleDistribution <- function(data) {
  mean <- mean(data$x)
  sd <- sd(data$x)

  xLimits <- c(mean - 5 * sd, mean + 5 * sd)
  yLimits <- c(-.1 , .5)
  distLimits <- c(mean - 4 * sd, mean + 4 * sd)

  xAxisData <- data.frame(x = distLimits, y = rep(0, 2))

  pdPlotObject <-  ggplot2::ggplot(data, ggplot2::aes(x = x)) +
    ggplot2::geom_density(mapping = ggplot2::aes(y = ..density..), n = 2^10, bw = sd/3, size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = xAxisData, size = 1) +
    ggplot2::ylim(yLimits) +
    ggplot2::xlim(xLimits) +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())

  #to make density line stop a dist limits
  densityOverlayData1 <- data.frame(x = c(xLimits[1], distLimits[1]), y = rep(0, 2))
  densityOverlayData2 <- data.frame(x = c(xLimits[2], distLimits[2]), y = rep(0, 2))
  pdPlotObject <- pdPlotObject +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = densityOverlayData1, color = "white", size = 4) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = densityOverlayData2, color = "white", size = 4)

  return(pdPlotObject)
}

.visualExplanationMedian <- function(plotObject, data, options, colors) {
  median <- median(data$x)
  mean <- mean(data$x)
  sd <- sd(data$x)
  xLimits <- c(mean - 5 * sd, mean + 5 * sd)
  yLimits <- c(-.1 , .5)

  plotData <- ggplot2::ggplot_build(plotObject)$data[[1]]

  medianLineHeight <- plotData$y[.indexOfNearestValue(median, plotData$x)]
  medianLineData <- data.frame(x = c(rep(median, 2)), y = c(0, medianLineHeight))
  fiftyPercentLabels <- data.frame(x = c(median + .85, median - .85), y = rep(.1, 2), label = rep("50%", 2))
  labelXPos <- ifelse(options[["LSdescStatistics"]] == "LSdescMMM", 4, median)
  labelYPos <- ifelse(options[["LSdescStatistics"]] == "LSdescMMM", max(yLimits) * .85, max(yLimits) * .55)

  plotObject <- plotObject +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = 0, ymax = y), data = subset(plotData, plotData$x > median),
                         fill = "grey") +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = 0, ymax = y), data = subset(plotData, plotData$x < median),
                         fill = "grey", alpha = .5) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = medianLineData, size = 1, color = colors[1]) +
    ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Median")),
                        mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[1], size = 6) +
    ggplot2::geom_text(data = fiftyPercentLabels, mapping = ggplot2::aes(x = x, y = y, label = label), size = 7)

  return(plotObject)
}

.visualExplanationMode <- function(plotObject, data, options, colors) {
  mean <- mean(data$x)
  sd <- sd(data$x)
  xLimits <- c(mean - 5 * sd, mean + 5 * sd)
  yLimits <- c(-.1 , .5)
  plotData <- ggplot2::ggplot_build(plotObject)$data[[1]]
  mode <- plotData$x[plotData$y == max(plotData$y)]

  modeVLineData <- data.frame(x = c(rep(mode, 2)), y = c(0, max(plotData$y)))
  modeHLineData <- data.frame(x = c(mode - .7, mode + .7), y = rep(max(plotData$y) + 0.003, 2))
  labelXPos <- ifelse(options[["LSdescStatistics"]] == "LSdescMMM", 4, mode)
  labelYPos <- ifelse(options[["LSdescStatistics"]] == "LSdescMMM", max(yLimits) * .75, max(yLimits) * .45)
  plotObject <- plotObject +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = modeVLineData, size = 1, color = colors[2]) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = modeHLineData, size = 1, color = colors[2]) +
    ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Mode")),
                        mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[2], size = 6)

  return(plotObject)
}

.visualExplanationMean <- function(plotObject, data, options, colors) {
  mean <- mean(data$x)
  sd <- sd(data$x)
  xLimits <- c(mean - 5 * sd, mean + 5 * sd)
  yLimits <- c(-.1 , .5)
  plotData <- ggplot2::ggplot_build(plotObject)$data[[1]]

  meanLineHeight <- plotData$y[.indexOfNearestValue(mean, plotData$x)]
  meanLineData <- data.frame(x = c(rep(mean, 2)), y = c(0, meanLineHeight))
  triangleData <- data.frame(x = c(mean, mean + .4, mean - .4), y = c(0, -.05, -.05))
  balanceBaseData <- data.frame(x = rep(c(-2, 2), each = 2), y = c(-.05, -.1, -.1, -.05))
  balanceBaseLineData <- data.frame(x = c(-2, 2), y = rep(-.05, 2))
  balanceLineData1 <- data.frame(x = c(seq(-3.8, -4.2, length.out = 40), seq(-4.2, -4.1, length.out = 60)),
                                 y = seq(-.04, .04, length.out = 100))
  balanceLineData2 <- data.frame(x = c(seq(-4, -4.3, length.out = 40), seq(-4.3, -4.25, length.out = 60)),
                                 y = seq(-.04, .02, length.out = 100))
  balanceLineData3 <- data.frame(x = balanceLineData1$x * -1, y = balanceLineData1$y)
  balanceLineData4 <- data.frame(x = balanceLineData2$x * -1, y = balanceLineData2$y)
  labelXPos <- ifelse(options[["LSdescStatistics"]] == "LSdescMMM", 4, mean)
  labelYPos <- ifelse(options[["LSdescStatistics"]] == "LSdescMMM", max(yLimits) * .95, max(yLimits) * .2)
  plotObject <- plotObject +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = colors[3]) +
    ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = triangleData, fill = colors[3]) +
    ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = balanceBaseData, fill = colors[3], alpha = .3) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceBaseLineData, size = 1.2, color = colors[3], alpha = .3) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData1, size = 1.2, color = colors[3], alpha = .3) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData2, size = 1, color = colors[3], alpha = .3) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData3, size = 1.2, color = colors[3], alpha = .3) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData4, size = 1, color = colors[3], alpha = .3) +
    ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Mean")),
                        mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[3], size = 6)

  return(plotObject)
}

.getDataLSTdesc <- function(jaspResults, options, inputType) {
  if (inputType == "dataRandom") {
    data <- .sampleRandomDataForLSTdesc(jaspResults, options)
  } else if (inputType == "dataSequence") {
    data <- .readInputSequenceForLSTdesc(jaspResults, options)
  } else if (inputType == "dataVariable") {
    data <- .readDataLSTdesc(jaspResults, options)
  }
  return(data)
}

.sampleRandomDataForLSTdesc <- function(jaspResults, options) {
  set.seed(options[["lstDescSampleSeed"]])
  n <- options[["lstDescSampleN"]]
  if (options[["lstDescSampleDistType"]] == "lstSampleDistDiscrete") {
    if (options[["LSdescDiscreteDistributions"]] == "binomialDist") {
      k <- options[["binomialDistributionNumberOfTrials"]]
      p <- options[["binomialDistributionSuccessProbability"]]
      data <- rbinom(n, k, prob = p)
    } else if (options[["LSdescDiscreteDistributions"]] == "poissonDist") {
      lambda <- options[["poissonDistributionLambda"]]
      data <- rpois(n, lambda)
    }
  } else if (options[["lstDescSampleDistType"]] == "lstSampleDistCont") {
    if (options[["LSdescContinuousDistributions"]] == "skewedNormal") {
      xi <- options[["skewedNormalDistributionLocation"]]
      omega <- options[["skewedNormalDistributionScale"]]
      alpha <- options[["skewedNormalDistributionShape"]]
      data <- sn::rsn(n, alpha = alpha, xi = xi, omega = omega)
    } else if (options[["LSdescContinuousDistributions"]] == "uniform") {
      min <- options[["uniformDistributionLowerBound"]]
      max <- options[["uniformDistributionUpperBound"]]
      data <- runif(n = n, min = min, max = max)
    } else if (options[["LSdescContinuousDistributions"]] == "normal") {
      mu <- options[["normalDistributionMean"]]
      sigma <- options[["normalDistributionStdDev"]]
      data <- rnorm(n, mu, sigma)
    }
  }
  df <- data.frame(x = data)
  return(df)
}

.readInputSequenceForLSTdesc <- function(jaspResults, options) {
  inputSequence <- options[["lstDescDataSequenceInput"]]
  inputSequence <- unlist(strsplit(inputSequence, split = ","))
  inputSequence <- gsub(" ", "", inputSequence, fixed = TRUE)
  df <- data.frame(x = as.numeric(inputSequence))
  return(df)
}

.readDataLSTdesc <- function(jaspResults, options) {
  return( data.frame(x = dataset[[ options[["selectedVariable"]] ]]) )
}

.drawSpreadVisualization <- function(jaspResults, options, data, plotObject, yMax, xLimits, xBreaks, labelSize, colors) {
  xLimitUpper <- max(xLimits)
  xBreakLimit <- max(xBreaks)
  if (options[["LSdescStatistics"]] == "LSdescRange") {
    minX <- min(data$x)
    maxX <- max(data$x)
    range <- maxX - minX
    minLineData <- data.frame(x = rep(minX, 2), y = c(0, yMax))
    maxLineData <- data.frame(x = rep(maxX, 2), y = c(0, yMax))
    rangeLineData <- data.frame(x = c(minX, maxX), y = rep(yMax * .95, 2))
    labelData <- data.frame(x = c(minX, maxX, (minX + maxX)/2),
                            y = c(yMax * .9, yMax * .9, yMax * .95),
                            label = c(gettextf("Min: %.2f", minX), gettextf("Max: %.2f", maxX), gettextf("Range: %.2f", range)))
    plotObject <- plotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = minLineData, size = 1, color = colors[2]) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = maxLineData, size = 1, color = colors[3]) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = rangeLineData, size = 1, color = colors[4]) +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = labelSize,
                          color = colors[2:4])
  } else if (options[["LSdescStatistics"]] == "LSdescQR") {
    quartiles <- quantile(data$x, type = 2)
    iqr <- quartiles[4] - quartiles[2]
    maxX <- max(data$x)
    q1LineData <- data.frame(x = rep(quartiles[2], 2), y = c(0, yMax * .95))
    q2LineData <- data.frame(x = rep(quartiles[3], 2), y = c(0, yMax * .95))
    q3LineData <- data.frame(x = rep(quartiles[4], 2), y = c(0, yMax * .95))
    iqrLineData <- data.frame(x = c(quartiles[2], quartiles[4]), y = rep(yMax * .95, 2))
    xPosQuarLabels <- xBreakLimit + (xLimitUpper - xBreakLimit)/2
    labelData <- data.frame(x = c(rep(xPosQuarLabels, 3), sum(quartiles[2], quartiles[4])/2),
                            y = yMax * c(.95, .75, .55, 1),
                            label = c(gettextf("1st quar. = %.2f", quartiles[2]), gettextf("2nd quar. / \n Median = %.2f", quartiles[3]),
                                      gettextf("3rd quar. = %.2f", quartiles[4]), gettextf("IQR = %.2f", iqr)))

    plotObject <- plotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q1LineData, color = colors[4], size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q2LineData, color = colors[1], size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q3LineData, color = colors[5], size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = iqrLineData, color = colors[6], size = 1) +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData,
                          color = colors[c(4, 1, 5, 6)], size = labelSize)
  }else if (options[["LSdescStatistics"]] == "LSdescSD") {
    stdDev <- sd(data$x)
    meanPoint <- mean(data$x)
    meanLineData <- data.frame(x = rep(meanPoint, 2), y = c(0, yMax * .95))
    minX <- min(xBreaks)
    maxX <- max(xBreaks)
    sdsMin <- round((abs(meanPoint - minX) / stdDev) + .5)
    sdsMax <- round((abs(meanPoint - maxX) / stdDev) + .5)
    colorPalette <- colors[4:8]
    for (i in seq_len(sdsMin)) {
      sdLineMax <- meanPoint - stdDev * (i-1)
      if (i != max(sdsMin)){
        sdLineMin <- meanPoint - stdDev * (i)
        sdLineData <- data.frame(x = c(sdLineMax, sdLineMin, sdLineMin),
                                 y = c(yMax * .85, yMax * .85, 0))
      } else {
        sdLineMin <- minX
        sdLineData <- data.frame(x = c(sdLineMax, sdLineMin),
                                 y = rep(yMax * .85, 2))
      }
      sdLineLength <- sdLineMax - sdLineMin
      if (sdLineLength >= stdDev/2) {
        xPosSdLabel <- sdLineMax - (sdLineMax - sdLineMin)/2
        sdLabelData <- data.frame(x = xPosSdLabel, y = yMax * .88, label = gettextf("-%i SD", i))
        plotObject <- plotObject +
          ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label), data = sdLabelData, size = labelSize - .5,
                             color = colorPalette[i])
      }
      plotObject <- plotObject +
        ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = sdLineData, color = colorPalette[i], size = 1)
    }
    for (i in seq_len(sdsMax)) {
      sdLineMin <- meanPoint + stdDev * (i - 1)
      if (i != max(sdsMax)) {
        sdLineMax <- meanPoint + stdDev * (i)
        sdLineData <- data.frame(x = c(sdLineMin, sdLineMax, sdLineMax),
                                 y = c(yMax * .85, yMax * .85, 0))
      } else {
        sdLineMax <- maxX
        sdLineData <- data.frame(x = c(sdLineMax, sdLineMin),
                                 y = rep(yMax * .85, 2))
      }
      sdLineLength <- sdLineMax - sdLineMin
      if (sdLineLength >= stdDev/2) {
        xPosSdLabel <- sdLineMin + (sdLineMax - sdLineMin)/2
        sdLabelData <- data.frame(x = xPosSdLabel, y = yMax * .88, label = gettextf("+%i SD", i))
        plotObject <- plotObject +
          ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label), data = sdLabelData, size = labelSize - .5,
                             color = colorPalette[i])
      }
      plotObject <- plotObject +
        ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = sdLineData, color = colorPalette[i], size = 1)
    }
    xPosLabels <- xBreakLimit + (xLimitUpper - xBreakLimit)/2
    labelData <- data.frame(x = rep(xPosLabels, 2), y = yMax * c(.95, .75),
                            label = c(gettextf("Mean = %.2f", meanPoint), gettextf("Std. Dev. = %.2f", stdDev)))
    plotObject <- plotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = colors[3]) +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = labelSize,
                          color = colors[3:2])
  }
  return(plotObject)
}

.lstDescCreateHistogramOrBarplot <- function(jaspResults, options, data, ready, discrete, stats = c("ct", "spread", "none"), colors) {
  title <- ifelse(discrete, "Barplot", "Histogram")
  jaspResults[["descHistogramOrBarplot"]] <- createJaspContainer(gettext(title))
  p <- createJaspPlot(title = gettext(title), width = 800, height = 700)
  p$position <- 2
  p$dependOn(options = c("LSdescStatistics",
                         "lstDescDataType",
                         "lstDescSampleN",
                         "lstDescSampleSeed",
                         "lstDescSampleDistType",
                         "LSdescDiscreteDistributions",
                         "LSdescContinuousDistributions",
                         "lstDescDataSequenceInput",
                         "selectedVariable",
                         "LSdescStatistics",
                         "LSdescStatistics",
                         "LSdescHistBar",
                         "LSdescHistCountOrDens",
                         "LSdescHistBarRugs",
                         "descBinWidthType",
                         "descNumberOfBins"))
  errors <- .plotErrors(p, data, options, stats)$errors

  if (ready && !errors) {
    if (discrete) {
      xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(data$x)))
      xStep <- xBreaks[2] - xBreaks[1]
      xLimits <- c(min(xBreaks) - xStep, max(xBreaks) + xStep * 3)
      yBreaks <- unique(round(jaspGraphs::getPrettyAxisBreaks(c(0, table(data$x)))))
      yLimits <- range(yBreaks) * 1.3
      yMax <- max(yLimits)
      plotObject <- ggplot2::ggplot(data, ggplot2::aes(x = x)) + ggplot2::geom_bar(fill = "grey",
                                                                                   col = "black", size = .3)
      plotObject <- plotObject +
        ggplot2::scale_y_continuous(name = "Counts", breaks = yBreaks, limits = yLimits) +
        ggplot2::scale_x_continuous(name = "Observations", breaks = xBreaks, limits = xLimits) +
        jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
    } else {
      displayDensity <- options[["LSdescHistCountOrDens"]] == "LSdescHistDens"
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
      xStep <- xBreaks[2] - xBreaks[1]
      xLimits <- c(min(xBreaks) - xStep/2, max(xBreaks) + xStep * 3)
      binWidthType <- options[["descBinWidthType"]]
      numberOfBins <- options[["descNumberOfBins"]]
      plotObject <- jaspDescriptives:::.plotMarginal(data$x, variableName = "Observations", displayDensity = displayDensity,
                                                     binWidthType = binWidthType, rugs = options[["LSdescHistBarRugs"]],
                                                     numberOfBins = numberOfBins)
      yMax <- max(ggplot2::ggplot_build(plotObject)$data[[1]]$y) * 1.3
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(ggplot2::ggplot_build(plotObject)$data[[1]]$y)
      yLimits <- c(0, yMax)
      plotObject <- plotObject + ggplot2::scale_y_continuous(limits = yLimits, breaks = yBreaks) +
        ggplot2::scale_x_continuous(name = "Observations", breaks = xBreaks, limits = xLimits)
    }
    if (stats == "ct") {
      plotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, plotObject, yMax = yMax, xMax = max(xLimits),
                                              xBreaks, discrete = discrete, colors = colors)
    } else if (stats == "spread") {
      plotObject <- .drawSpreadVisualization(jaspResults, options, data, plotObject, yMax = yMax, xLimits, xBreaks,
                                             labelSize = 6, colors = colors)
    }
    p$plotObject <- plotObject
  } else if (errors){
    p <- .plotErrors(p, data, options, stats)$errorMessage
  }
  jaspResults[["descHistogramOrBarplot"]] <- p
}


.lstDescCreateDotplot <- function(jaspResults, options, data, ready, discrete, stats = c("ct", "spread", "none"), colors) {
  jaspResults[["descDotplot"]] <- createJaspContainer(gettext("Dotplot"))
  height <- 700 + (length(data$x) / 50) * 25
  dp <- createJaspPlot(title = gettext("Dotplot"), width = 800, height = height)
  dp$position <- 3
  dp$dependOn(options = c("LSdescStatistics",
                          "lstDescDataType",
                          "lstDescSampleN",
                          "lstDescSampleSeed",
                          "lstDescSampleDistType",
                          "LSdescDiscreteDistributions",
                          "LSdescContinuousDistributions",
                          "lstDescDataSequenceInput",
                          "selectedVariable",
                          "LSdescStatistics",
                          "LSdescStatistics",
                          "LSdescDotPlot",
                          "LSdescDotPlotRugs"))
  errors <- .plotErrors(dp, data, options, stats)$errors
  if (ready && !errors) {
    dp$plotObject <- .lstDescCreateDotPlotObject(data, options, stats = stats, discrete, rugs = options[["LSdescDotPlotRugs"]],
                                                 colors = colors)
  }else if (errors) {
    dp <- .plotErrors(dp, data, options, stats)$errorMessage
  }
  jaspResults[["descDotplot"]] <- dp
}

.plotErrors <- function(plot, data, options, stats) {
  errors <- FALSE
  if (stats == "spread"){
    if (options[["LSdescStatistics"]] == "LSdescSD") {
      if (sd(data$x) == 0 | is.na(sd(data$x))){
        plot$setError(gettext("No variance: All values are equal."))
        errors <- TRUE
      }
    }
  }
  return(list("errorMessage" = plot,
              "errors" = errors))
}

.drawMeanMedianOrModeLine <- function(jaspResults, options, data, plot, yMax, xMax, xBreaks, lines = TRUE, discrete, colors) {
  n <- length(data$x)
  labelSize <- .getLabelSize(n)
  if (options[["LSdescStatistics"]] == "LSdescMode"|| options[["LSdescStatistics"]] == "LSdescMMM") {
    xPos <- median(xBreaks)
    modeLines <- lines
    noMode <- length(unique(data$x)) == length(data$x)
    if (noMode) {
      modeLabelData <- data.frame(x = xPos, y = yMax * .95, label = gettext("All values unique, \n no mode defined."))
      modeLines <- FALSE # also plot no line then
    } else {
      tableData <- table(data$x)
      modeCol <- tableData[tableData == max(tableData)]
      mode <- as.numeric(names(modeCol))
      modeHeight <- as.numeric(modeCol)
      plotData <- ggplot2::ggplot_build(plot)$data
      if (length(mode) == 1) {
        modeLabelText <- ifelse(discrete, gettextf("Mode = %1$i \n (Count = %2$i)", mode, modeHeight),
                                gettextf("Mode = %.2f \n (Count = %i)", mode, modeHeight))
      } else {
        firstMode <- ifelse(discrete, gettextf("Mode = {%i", mode[1]), gettextf("Mode = /n {%.2f", mode[1]))
        if (discrete) {
          otherModes <- gettextf(rep(", %i", length(mode) - 1), mode[2:length(mode)])
        } else {
          otherModes <- gettextf(rep(", %.2f", length(mode) - 1), mode[2:length(mode)])
        }
        otherModes <- paste(otherModes, collapse = "")
        modeHeightString <- gettextf( "} \n (Count = %i)", modeHeight)
        modeLabelText <- paste(firstMode, otherModes, modeHeightString, sep = "")
      }
      labelYPos <- yMax * .95
      modeLabelData <- data.frame(x = xPos, y = labelYPos, label = modeLabelText)
    }
    if (modeLines) {
      if (discrete) {
        modeLineYPos <- max(plotData[[1]]$ymax)
        for (i in seq_along(mode)) {
          modeLineXPos <- plotData[[1]]$x[plotData[[1]]$xmin < mode[i] & plotData[[1]]$xmax > mode[i]][1]
          lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos * 1.05))
          plot <- plot + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = colors[2], size = 3)
        }
      } else {
        modeLineYPos <- max(plotData[[2]]$ymax)
        for (i in seq_along(mode)) {
          modeLineXPos <- plotData[[2]]$x[plotData[[2]]$xmin < mode[i] & plotData[[2]]$xmax > mode[i]][1]
          lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos * 1.05))
          plot <- plot + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = colors[2], size = 3)
        }
      }
      plot <- plot + ggplot2::geom_hline(yintercept = modeLineYPos, size = 1, color = colors[2])
    }
    if (options[["LSdescStatistics"]] == "LSdescMode") {
      plot <- plot + ggplot2::geom_label(data = modeLabelData,
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[2], size = labelSize)
    } else if (options[["LSdescStatistics"]] == "LSdescMMM") {
      modeLabelData$x <- max(xBreaks) + (xMax - max(xBreaks)) / 2
      modeLabelData$y <- yMax * .55
      plot <- plot + ggplot2::geom_label(data = modeLabelData,
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[2], size = labelSize)
    }
  }
  if (options[["LSdescStatistics"]] == "LSdescMean" || options[["LSdescStatistics"]] == "LSdescMMM") {
    mean <- mean(data$x)
    if (lines)
      plot <- plot + ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y),
                                        data = data.frame(x = rep(mean, 2), y = c(0, yMax)), size = 1, color = colors[3])
    if (options[["LSdescStatistics"]] == "LSdescMean") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = mean, y = yMax, label = gettextf("Mean = %.2f", mean)),
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[3], size = labelSize)
    } else if (options[["LSdescStatistics"]] == "LSdescMMM") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = max(xBreaks) + (xMax - max(xBreaks)) / 2, y = yMax * .95, label = gettextf("Mean = %.2f", mean)),
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[3], size = labelSize)
    }
  }
  if (options[["LSdescStatistics"]] == "LSdescMedian" || options[["LSdescStatistics"]] == "LSdescMMM") {
    median <- median(data$x)
    if (lines)
      plot <- plot + ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y),
                                        data = data.frame(x = rep(median, 2), y = c(0, yMax)), size = 1, color = colors[1])
    if (options[["LSdescStatistics"]] == "LSdescMedian") {
      plot <- plot +  ggplot2::geom_label(data = data.frame(x = median, y = yMax, label = gettextf("Median = %.2f", median)),
                                          mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[1], size = labelSize)
    } else if (options[["LSdescStatistics"]] == "LSdescMMM") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = max(xBreaks) + (xMax - max(xBreaks)) / 2, y = yMax * .75, label = gettextf("Median = %.2f", median)),
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[1], size = labelSize)
    }
  }
  return(plot)
}


.lstDescCreateDotPlotObject <- function(data, options, stats = c("ct", "spread", "none"), discrete, rugs, colors) {
  n <- length(data$x)

  dotsize <- .getDotSize(n)
  labelSize <- .getLabelSize(n)

  if (discrete) {
    xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(data$x)))
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  }
  xStep <- xBreaks[2] - xBreaks[1]
  xLimits <- c(min(xBreaks) - xStep/2, max(xBreaks) + xStep * 3)

  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = x)) +
    ggplot2::geom_dotplot(binaxis = 'x', stackdir = 'up', dotsize = dotsize, fill = "grey") +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = xLimits) +
    ggplot2::coord_fixed()

  if (rugs)
    p <- p + ggplot2::geom_rug(data = data, mapping = ggplot2::aes(x = x), sides = "b")

  pData <- ggplot2::ggplot_build(p)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
  yBreaks <- yLabels * dotWidth
  yStep <- yBreaks[2] - yBreaks[1]
  yMax <- ifelse(max(yBreaks) < (10 * dotWidth), (10 * dotWidth) + yStep*2, max(yBreaks) + yStep*2)
  yLimits <-  c(0, yMax)

  p <- p + ggplot2::scale_y_continuous(name = "Counts", limits = yLimits, breaks = yBreaks, labels = yLabels) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (stats == "ct") {
    allCTs <- options[["LSdescStatistics"]] == "LSdescMMM"
    if (options[["LSdescStatistics"]] == "LSdescMedian" || allCTs) {
      p <- .dotPlotVisualizeQuartiles(data, p, dotsize, labelSize, yLimits, xLimits, xBreaks, labels = !allCTs, labelText = gettext("Median"), quartile = 2,
                                      lines = !allCTs, color = colors[1])
    }
    if (options[["LSdescStatistics"]] == "LSdescMean" || allCTs) {
      mean <- mean(data$x)
      y0 <- dotWidth / 2
      circleData <- data.frame(x0 = mean,
                               y0 = y0,
                               r = dotWidth / 2)
      p <- p + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                    inherit.aes = FALSE, fill = colors[3], alpha = .3, color = colors[3], n = 4)
      if (!allCTs) {
        meanLineData <- data.frame(x = c(mean, mean),
                                   y = c(y0 + dotWidth / 2,  max(yLimits)))
        p <- p + ggplot2::geom_path(data = meanLineData, mapping = ggplot2::aes(x = x, y = y), color = colors[3], size = 1)
      }
      p <- .drawMeanMedianOrModeLine(jaspResults, options, data, p, yMax = max(yLimits), xMax = max(xLimits), xBreaks,
                                     lines = FALSE, discrete = discrete, colors = colors)
    }
    if (options[["LSdescStatistics"]] == "LSdescMode" || allCTs) {
      if (length(unique(data$x)) != n) {
        modeTable <- table(data$x)
        mode <- as.numeric(names(modeTable[modeTable == max(modeTable)]))
        modeLineYPos <- dotWidth * max(pData[[1]]$count)
        for (i in seq_along(mode)) {
          modeLineXPos <- pData[[1]]$x[pData[[1]]$xmin < mode[i] & pData[[1]]$xmax > mode[i]][1]
          lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos + dotWidth / 2))
          p <- p + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = colors[2], size = 3)
        }
        p <- p + ggplot2::geom_hline(yintercept = modeLineYPos, color = colors[2], size = 1)
      }
      p <- .drawMeanMedianOrModeLine(jaspResults, options, data, p, yMax = max(yLimits), xMax = max(xLimits), xBreaks,
                                     lines = FALSE, discrete = discrete, colors = colors)
    }
  } else if (stats == "spread") {
    if (options[["LSdescStatistics"]] == "LSdescRange") {
      p <- .dotPlotVisualizeRange(data, p, dotsize, yLimits, colors)
    } else if (options[["LSdescStatistics"]] == "LSdescQR") {
      p <- .dotPlotVisualizeQuartiles(data, p, dotsize, labelSize, yLimits, xLimits, xBreaks, labelText = gettext("2nd quar. / \n Median"),
                                      quartile = 2, labelsInCorner = TRUE, color = colors[1])
      p <- .dotPlotVisualizeQuartiles(data, p, dotsize, labelSize, yLimits, xLimits, xBreaks, labelText = gettext("1st quar."), quartile = 1,
                                      labelsInCorner = TRUE, color = colors[6])
      p <- .dotPlotVisualizeQuartiles(data, p, dotsize, labelSize, yLimits, xLimits, xBreaks, labelText = gettext("3rd quar."), quartile = 3,
                                      labelsInCorner = TRUE, color = colors[5])
      p <- .dotPlotIQRLine(data, p, yLimits, labelSize, colors)
    } else if (options[["LSdescStatistics"]] == "LSdescSD") {
      p <- .drawSpreadVisualization(jaspResults, options, data, p, yMax = max(yLimits), xLimits, xBreaks, labelSize, colors = colors)
    }
  }
  return(p)
}

.dotPlotVisualizeRange <- function(data, plotObject, dotsize, yLimits, colors) {
  pData <- ggplot2::ggplot_build(plotObject)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  range <- max(data$x) - min(data$x)
  minDot <- pData[[1]][1,]
  maxDot <- pData[[1]][length(data$x),]
  minY0 <- dotWidth/2
  maxY0 <- ifelse(maxDot$countidx == 1, dotWidth/2, dotWidth/2 + (maxDot$countidx - 1) * dotWidth)
  circleData  <- data.frame(x0 = c(minDot$x, maxDot$x),
                            y0 = c(minY0, maxY0),
                            r = dotWidth/2)
  minLineData <- data.frame(x = rep(minDot$x, 2), y = c(0, max(yLimits)))
  maxLineData <-  data.frame(x = rep(maxDot$x, 2), y = c(0, max(yLimits)))
  rangeLineData <- data.frame(x = c(minDot$x, maxDot$x), y = rep(max(yLimits) * .95, 2))
  labelData <- data.frame(x = c(minDot$x, maxDot$x, (minDot$x + maxDot$x) /2),
                          y = c(max(yLimits) * .9, max(yLimits) * .9, max(yLimits) * .95),
                          label = c(gettextf("Min: %.2f", min(data$x)), gettextf("Max: %.2f", max(data$x)),
                                    gettextf("Range: %.2f", range)))
  plotObject <- plotObject +
    ggforce::geom_circle(data = circleData[1,], mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                         inherit.aes = FALSE, fill = colors[2]) +
    ggforce::geom_circle(data = circleData[2,], mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                         inherit.aes = FALSE, fill = colors[3]) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = minLineData, color = colors[2], size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = maxLineData, color = colors[3], size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = rangeLineData, color = colors[4], size = 1) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = 6,
                        color = colors[2:4])
  return(plotObject)
}

.dotPlotVisualizeQuartiles <- function(data, plotObject, dotsize, labelSize, yLimits, xLimits, xBreaks, labels = TRUE, labelText,
                                       quartile, lines = TRUE, labelsInCorner = FALSE, color) {
  n <- length(data$x)
  pData <- ggplot2::ggplot_build(plotObject)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  sortedDf <- data.frame(x = sort(data$x))
  location <- as.integer(quantile(as.numeric(rownames(sortedDf)), type = 2) * 2) / 2 # round to nearest half
  location <- location[quartile + 1]
  quartileValue <- quantile(data$x, type = 2)[quartile + 1]
  if (location == as.integer(location)) {   # if quartile is a single datapoint
    halfwayDot <- pData[[1]][location,]
    y0 <- ifelse(halfwayDot$countidx == 1, dotWidth/2, dotWidth/2 + (halfwayDot$countidx - 1) * dotWidth)
    x0 <- halfwayDot$x
    circleData <- data.frame(x0 = x0,
                             y0 = y0,
                             r = dotWidth / 2)
    chairData <- data.frame(x = c(x0 - dotWidth * .65, x0 - dotWidth * .65,
                                  x0 + dotWidth * .65, x0 + dotWidth * .65),
                            y = c(y0 + dotWidth/2, y0, y0, y0 - dotWidth/2))
    plotObject <- plotObject + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                                    inherit.aes = FALSE, fill = color) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = chairData, size = 1, color = color)
    if (lines) {
      lineData <- data.frame(x = c(x0, quartileValue),
                             y = c(y0, max(yLimits) * .95))
      plotObject <- plotObject + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = color,
                                                    size = 1)
    }
  } else {  # if quartile is the average of two points
    halfwayDots <- list("lowerDot" = pData[[1]][location - .5,],
                        "upperDot" = pData[[1]][location + .5,])
    y0lower <- ifelse(halfwayDots$lowerDot$countidx == 1, dotWidth / 2, dotWidth / 2 + (halfwayDots$lowerDot$countidx - 1) * dotWidth)
    y0upper <- ifelse(halfwayDots$upperDot$countidx == 1, dotWidth / 2, dotWidth / 2 + (halfwayDots$upperDot$countidx - 1) * dotWidth)
    if (all(c(halfwayDots$lowerDot$count, halfwayDots$upperDot$count) == 1)) {
      start <- c(pi * 2, pi * 2)
      end <-c(pi * 3, pi)
    } else {
      start <- c(pi * 1.5, pi / 2)
      end <- c(pi * 2.5, pi * 1.5)
      chairData <- data.frame(x = c(halfwayDots$lowerDot$x - dotWidth * .65, halfwayDots$lowerDot$x - dotWidth * .65,
                                    halfwayDots$lowerDot$x + dotWidth * .65, halfwayDots$lowerDot$x + dotWidth * .65),
                              y = c(y0lower + dotWidth, y0lower + dotWidth/2, y0lower + dotWidth/2, y0lower))
      plotObject <- plotObject + ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = chairData, size = 1, color = color)
    }
    circleData <- data.frame(x0 = c(halfwayDots$lowerDot$x, halfwayDots$upperDot$x),
                             y0 = c(y0lower, y0upper),
                             r = rep(dotWidth/2, 2),
                             r0 = rep(0, 2),
                             start = start,
                             end = end)
    plotObject <- plotObject +
      ggforce::geom_arc_bar(data = circleData,
                            mapping = ggplot2::aes(x0 = x0, y0 = y0, r0 = r0, r = r,  start = start, end = end),
                            inherit.aes = FALSE, fill = color)
    if (lines) {
      lineData1 <- data.frame(x = c(halfwayDots$lowerDot$x, quartileValue),
                              y = c(y0lower, max(yLimits) * .95))
      lineData2 <- data.frame(x = c(halfwayDots$upperDot$x, quartileValue),
                              y = c(y0upper, max(yLimits) * .95))
      plotObject <- plotObject +
        ggplot2::geom_path(data = lineData1, mapping = ggplot2::aes(x = x, y = y), color = color, size = 1) +
        ggplot2::geom_path(data = lineData2, mapping = ggplot2::aes(x = x, y = y), color = color, size = 1)
    }
  }
  if (labels) {
    label <- paste(labelText, "= %.2f")
    if (labelsInCorner) {
      labelXPos <- max(xBreaks) + (max(xLimits) - max(xBreaks))/2
      labelYPos <- switch(quartile, max(yLimits) * .95, max(yLimits) * .75, max(yLimits) * .55)
    } else {
      labelXPos <- quartileValue
      labelYPos <- max(yLimits) * .95
    }
    plotObject <- plotObject + ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettextf(label, quartileValue)),
                                                   mapping = ggplot2::aes(x = x, y = y, label = label), color = color, size = labelSize)
  }
  return(plotObject)
}

.dotPlotIQRLine <- function(data, plotObject, yLimits, labelSize, colors) {
  quartiles <- quantile(data$x, type = 2, names = FALSE)
  iqr <- quartiles[4] - quartiles[2]
  lineData <- data.frame(x = c(quartiles[2], quartiles[4]), y = rep(max(yLimits)* .95, 2))
  labelXPos <- (quartiles[4] + quartiles[2]) / 2
  labelData <- data.frame(x = labelXPos, y = max(yLimits), label = gettextf("IQR = %.2f", iqr))
  plotObject <- plotObject +
    ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = colors[4], size = 1) +
    ggplot2::geom_label(data = labelData, mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[4],
                        size = labelSize)
}


.getLabelSize <- function(n){
  if (n <=  100) {
    labelSize <- 6
  } else if (n > 100 && n <= 200){
    labelSize <- 6 - 2 *((n - 100) / 100)
  } else if (n > 200 && n <= 300){
    labelSize <- 4 - ((n - 200) / 100)
  } else if (n > 300 && n < 400){
    labelSize <- 3 - ((n - 300) / 100)
  } else {
    labelSize <- 3
  }
  return(labelSize)
}

.getDotSize <- function(n) {
  if (n > 50){
    dotsize <- 1 - (log(n)) / 30
  } else {
    dotsize <- 1
  }
  return(dotsize)
}
