#
# Copyright (C) 2013-2018 University of Amsterdam
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

LSTconfidenceIntervals <- function(jaspResults, dataset = NULL, options) {

  confidenceContainer <- .getConfidenceContainer(jaspResults)

  .simulateDatasets(confidenceContainer, options)

  .computeConfidenceIntervals(confidenceContainer, options)

  .makeSummaryTable(confidenceContainer, options)

  .plotTreePlot(confidenceContainer, options)

  .makeTreeTable(confidenceContainer, options)

  .plotDatasets(confidenceContainer, options)

  .plotConvergencePlot(confidenceContainer, options)

  return()
}

.getConfidenceContainer <- function(jaspResults) {
  confidenceContainer <- jaspResults[["confidenceContainer"]] %setOrRetrieve% createJaspContainer(dependencies = c("mu", "sigma", "n", "confidenceIntervalInterval", "nReps", "treePlot", "setSeed", "seed"))
  return(confidenceContainer)
}

.simulateDatasets <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["simulatedDatasets"]]))
    return()

  jaspBase::.setSeedJASP(options)
  listWithData <- lapply(seq_len(options[["nReps"]]), function(x) rnorm(options$n,
                                                            options$mu,
                                                            options$sigma))

  jaspContainer[["simulatedDatasets"]] <- createJaspState(object = listWithData)

  return()
}

.computeConfidenceIntervals <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["computedConfidenceIntervals"]]))
    return()

  listWithData <- jaspContainer[["simulatedDatasets"]][["object"]]

  meanCI <- t(sapply(listWithData, function(x) c(mean(x),
                                                 t.test(x, conf.level = options$confidenceIntervalInterval)$conf.int[1:2])))
  meanCI <- as.data.frame(meanCI)
  colnames(meanCI) <- c("mean", "lower", "upper")
  meanCI[["Repetition"]] <- 1:options$nReps
  meanCI[["successfulCI"]] <- options$mu > meanCI[["lower"]] & options$mu < meanCI[["upper"]]
  meanCI[["colorCI"]] <- ifelse(meanCI[["successfulCI"]], "A", "B")
  meanCI[["sd"]] <- sapply(listWithData, sd)
  meanCI[["n"]] <- sapply(listWithData, length)


  jaspContainer[["computedConfidenceIntervals"]] <- createJaspState(object = meanCI)

  return()
}

.makeSummaryTable <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["resultTable"]]))
    return()

  resultTable <- createJaspTable(title = "Results Table")
  jaspContainer[["resultTable"]] <- resultTable

  resultTable$addColumnInfo(name = "Repetitions", type = "integer")
  resultTable$addColumnInfo(name = "mu", type = "number", title="μ")
  resultTable$addColumnInfo(name = "sigma", title="σ", type = "number")
  resultTable$addColumnInfo(name = "n", type = "integer")
  resultTable$addColumnInfo(name = "coverage", type = "integer",title=gettext("Coverage"))
  resultTable$addColumnInfo(name = "coveragePercentage", type = "number", format = "pc", title=gettextf("Coverage %%"))

  meanCI <- jaspContainer[["computedConfidenceIntervals"]][["object"]]
  tableDat <- data.frame(mu = options$mu,
                         sigma = options$sigma,
                         Repetitions = options$nReps,
                         n = options$n,
                         coverage = sum(meanCI[["successfulCI"]]),
                         coveragePercentage = mean(meanCI[["successfulCI"]]))

  resultTable$setData(tableDat)

  return()
}

.makeTreeTable <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["treeTable"]]) || !options$tableTreePlot)
    return()

  treeTable <- createJaspTable(title = "Confidence Interval Table", dependencies = "tableTreePlot")
  jaspContainer[["containerTreePlot"]][["treeTable"]] <- treeTable

  treeTable$addColumnInfo(name = "Repetition", type = "integer")
  treeTable$addColumnInfo(name = "mean", type = "number", title=gettext("Mean"))
  treeTable$addColumnInfo(name = "n", title=gettext("Estimate"), type = "integer")
  treeTable$addColumnInfo(name = "sd", type = "number")

  thisOverTitle <- gettextf("%s%% CI for Mean Difference", options$confidenceIntervalInterval * 100)
  treeTable$addColumnInfo(name="lower", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
  treeTable$addColumnInfo(name="upper", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
  treeTable$addColumnInfo(name = "successfulCI", type = "string", title=gettextf("CI contains %s?", "μ"))

  meanCI <- jaspContainer[["computedConfidenceIntervals"]][["object"]]
  meanCI[["successfulCI"]] <- ifelse(meanCI[["successfulCI"]], "Yes", "No")
  meanCI <- meanCI[1:options$dataPlotShowN,]  # Show data only for CIs that are also plotted

  treeTable$showSpecifiedColumnsOnly <- TRUE
  treeTable$setData(meanCI)

  return()
}

.plotTreePlot <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerTreePlot"]]) || !options$treePlot)
    return()

  treePlotContainer <- createJaspContainer(title = gettext("Tree Plot"))
  jaspContainer[["containerTreePlot"]] <- treePlotContainer
  treePlotContainer$dependOn(c("treePlot", "treePlotAdditionalInfo", "fixAxisTreePlot",
                               "fixAxisLower", "fixAxisUpper", "dataPlotShowN"))
  treePlotContainer[["treePlot"]] <- createJaspPlot(title = "", width = 480,
                                                    height = 320 * min((options$nReps %/% 50 + 1), 5))


  if (options$fixAxisLower >= options$fixAxisUpper) {
    treePlotContainer$setError(gettext("Make sure to specify a lower bound that is lower than the upper bound for the x-axis."))
    return()
  }

  meanCI <- jaspContainer[["computedConfidenceIntervals"]][["object"]]
  meanCI$Repetition <- rev(meanCI$Repetition)
  meanCItrimmed <- meanCI[seq_len(options$dataPlotShowN), ]

  myTicks <- jaspGraphs::getPrettyAxisBreaks(1:options$nReps)
  myTicks[1] <- 1

  p <- ggplot2::ggplot(meanCItrimmed,
                       ggplot2::aes(x = mean,
                                    y = Repetition)) +
    ggplot2::geom_point(ggplot2::aes(color = colorCI), size = 6, shape = 18) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower,
                                         xmax = upper,
                                         color = colorCI)) +
    ggplot2::scale_color_manual(values = c("A" = "black", "B" = "red")) +
    ggplot2::geom_vline(data = data.frame(options$mu),
                        ggplot2::aes(xintercept = options$mu),
                        linetype = "dashed") +
    ggplot2::scale_y_continuous(limits = c(0, options$nReps+1),
                                breaks = jaspGraphs::getPrettyAxisBreaks(1:options$nReps),
                                labels = rev(myTicks)) +
    ggplot2::xlab(gettextf("Observed mean and %s%% CI", options$confidenceIntervalInterval * 100))

  if (options$fixAxisTreePlot) {
    p <- p + ggplot2::scale_x_continuous(limits = c(options$fixAxisLower, options$fixAxisUpper))
  }

  p <- p +
    jaspGraphs::geom_rangeframe() + # add lines on the x-axis and y-axis
    jaspGraphs::themeJaspRaw()      # add the JASP theme

  if(isTryError(p))
    treePlotContainer[["treePlot"]]$setError(.extractErrorMessage(p))
  else
    treePlotContainer[["treePlot"]]$plotObject <- p

  return()
}


.plotDatasets <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerRainCloudPlots"]]) || !options$dataPlot)
    return()

  jaspContainer[["containerRainCloudPlots"]] <- createJaspContainer(gettext("Data plots"),
                                                                    dependencies = c("dataPlotShowN", "dataPlot", "ciSampleShowType",
                                                                                     "ciFirstOrLastSamples", "ciFromSample",
                                                                                     "ciToSample"))

  rainData <- jaspContainer[["simulatedDatasets"]][["object"]]
  maxSamples <- length(rainData)
  fromTo <- .getFromToSampleShow(options[["ciSampleShowType"]], maxSamples, singleValue = options[["ciFirstOrLastSamples"]],
                                 start = options[["ciFromSample"]], stop = options[["ciToSample"]])
  from <- fromTo[1]
  to <- fromTo[2]
  visibleRainData <- rainData[from:to]
  rows <- .getPlotMatDetails(length(visibleRainData))$rows
  plotHeight <- rows * 250
  matrixPlot <- createJaspPlot(title = gettext("Samples"), width = 1200,
                               height = plotHeight)

  plotList <- list()
  for (i in seq_along(visibleRainData)){
    thisRainData <- data.frame(y = visibleRainData[[i]],
                               group = rep(1, options$n))
    repetitionNumber <- from + i - 1
    plotList[[i]] <- try(jaspTTests::.descriptivesPlotsRainCloudFill(thisRainData, "y", "group",
                                                                     yLabel = gettext("Dependent"),
                                                                     xLabel = gettextf("Repetition %d", repetitionNumber),
                                                                     testValue = options$mu,
                                                                     addLines = FALSE, horiz = FALSE))
     if(isTryError(plotList[[i]]))
       matrixPlot$setError(.extractErrorMessage(plotList[[i]]))
  }
  tbcString <- ifelse(to == maxSamples, "", gettextf("... until Sample Nr. %i", maxSamples))
  plotMat <- .arrangePlotMat(plotList, tbc = tbcString)
  matrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
  jaspContainer[["containerRainCloudPlots"]][["plotObject"]] <- matrixPlot
  return()
}

.plotConvergencePlot <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerConvergencePlot"]]) || !options$convergencePlot)
    return()

  containerConvergencePlot <- createJaspContainer(title = gettext("Convergence Plot"))
  jaspContainer[["containerConvergencePlot"]] <- containerConvergencePlot
  containerConvergencePlot$dependOn(c("convergencePlot", "convergenceZoomIn", "convergenceZoomMargin"))
  containerConvergencePlot[["convergencePlot"]] <- createJaspPlot(title = "", width = 480, height = 320)

  meanCI <- jaspContainer[["computedConfidenceIntervals"]][["object"]]

  myTicks <- jaspGraphs::getPrettyAxisBreaks(1:options$nReps)
  myTicks[1] <- 1

  if (options$convergenceZoomIn) {
    myYlim <- c(max(0, options$confidenceIntervalInterval - options$convergenceZoomMargin),
                min(1, options$confidenceIntervalInterval + options$convergenceZoomMargin))
  } else {
    myYlim <- c(0, 1)
  }

  cumSuccessProportion <- cumsum(meanCI$successfulCI)/1:options$nReps
  simulMatrix <- matrix(0, nrow = 1000, ncol = options$nReps)

  for (i in 1:options$nReps){
    simulMatrix[ , i] <- rbeta(1e3, cumSuccessProportion[i]*i+1, i-cumSuccessProportion[i]*i+1)
  }
  credInt <- apply(simulMatrix, 2, HDInterval::hdi) # record the credibility interval
  yUpper <- credInt[1,]
  yLower <- credInt[2,]

  p <- ggplot2::ggplot(data= NULL) +
    ggplot2::xlab(gettext("Number of Replications")) +
    ggplot2::ylab(gettext("p(Coverage)")) +
    ggplot2::coord_cartesian(xlim = c(0, options$nReps), ylim = myYlim) +
    ggplot2::geom_polygon(ggplot2::aes(x = c(1:options$nReps,options$nReps:1), y = c(yUpper, rev(yLower))),
                          fill = "lightsteelblue") +  # CI
    ggplot2::geom_line(color = "darkred", ggplot2::aes(x = 1:options$nReps,
                                                       y = rep(options$confidenceIntervalInterval, options$nReps))) +  # confidence level
    ggplot2::geom_line(data= NULL, ggplot2::aes(x = 1:options$nReps,
                                                y = cumsum(meanCI$successfulCI)/1:options$nReps)) + # observed coverage
    ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(1:options$nReps), labels = myTicks)

  p <- p +
    jaspGraphs::geom_rangeframe() + # add lines on the x-axis and y-axis
    jaspGraphs::themeJaspRaw()      # add the JASP theme

  if(isTryError(p))
    containerConvergencePlot[["convergencePlot"]]$setError(.extractErrorMessage(p))
  else
    containerConvergencePlot[["convergencePlot"]]$plotObject <- p

  return()
}

.plotTreePlotOLD <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerTreePlot"]]) || !options$treePlot)
    return()

  treePlotContainer <- createJaspContainer(title = gettext("Tree Plot"))
  jaspContainer[["containerTreePlot"]] <- treePlotContainer
  treePlotContainer$dependOn(c("treePlot", "treePlotAdditionalInfo", "fixAxisTreePlot",
                               "fixAxisLower", "fixAxisUpper"))
  treePlotContainer[["treePlot"]] <- createJaspPlot(title = "", width = 480, height = 320)

  if (options$fixAxisLower >= options$fixAxisUpper) {
    treePlotContainer$setError(gettext("Make sure to specify a lower bound that is lower than the upper bound for the x-axis."))
    return()
  }


  meanCI <- jaspContainer[["computedConfidenceIntervals"]][["object"]]

  p <- ggplot2::ggplot(meanCI,
                       ggplot2::aes(x = mean,
                                    y = Repetition)) +
    ggplot2::geom_point(ggplot2::aes(color = colorCI), size = 6, shape = 18) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower,
                                         xmax = upper,
                                         color = colorCI)) +
    ggplot2::scale_color_manual(values = c("A" = "black", "B" = "red")) +
    ggplot2::geom_vline(data = data.frame(options$mu),
                        ggplot2::aes(xintercept = options$mu),
                        linetype = "dashed") +
    ggplot2::scale_y_continuous(breaks= jaspGraphs::getPrettyAxisBreaks(1:options$nReps)) +
    ggplot2::xlab(gettextf("Observed mean and %s%% CI", options$confidenceIntervalInterval * 100))

  if (options$fixAxisTreePlot) {
    p <- p + ggplot2::scale_x_continuous(limits = c(options$fixAxisLower, options$fixAxisUpper))
  }

  p <- p +
    jaspGraphs::geom_rangeframe() + # add lines on the x-axis and y-axis
    jaspGraphs::themeJaspRaw()      # add the JASP theme

  if(isTryError(p))
    treePlotContainer[["treePlot"]]$setError(.extractErrorMessage(p))
  else
    treePlotContainer[["treePlot"]]$plotObject <- p

  return()
}

.plotDatasetsOLD <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerRainCloudPlots"]]) || !options$dataPlot)
    return()

  rainCloudPlotsContainer <- createJaspContainer(title = gettext("Data plots"))
  # rainCloudPlotsContainer$position <- 3
  jaspContainer[["containerRainCloudPlots"]] <- rainCloudPlotsContainer
  rainCloudPlotsContainer$dependOn(c("dataPlotShowN", "dataPlot"))

  rainData <- data.frame(y = unlist(jaspContainer[["simulatedDatasets"]][["object"]]),
                         group = rep(1:options$nReps, each = options$n))

  singlePlot <- createJaspPlot(title = "", width = 480, height = 320)
  rainCloudPlotsContainer[["rainCloudPlotAll"]] <- singlePlot
  p <- try(jaspTTests::.descriptivesPlotsRainCloudFill(rainData, "y", "group",
                                                       yLabel = gettext("Dependent"),
                                                       xLabel = gettext("Rep"),
                                                       testValue = options$mu,
                                                       addLines = FALSE, horiz = FALSE))
  if(isTryError(p))
    singlePlot$setError(.extractErrorMessage(p))
  else
    singlePlot$plotObject <- p

  for (i in seq_len(options$dataPlotShowN)) {


    thisRainData <- data.frame(
      y = unlist(jaspContainer[["simulatedDatasets"]][["object"]][[I]]),
      group = rep(1, options$n)
    )

    singlePlot <- createJaspPlot(title = "", width = 480, height = 320)
    rainCloudPlotsContainer[[paste0("rainCloudPlotSingle", i)]] <- singlePlot
    p <- try(jaspTTests::.descriptivesPlotsRainCloudFill(thisRainData, "y", "group",
                                                         yLabel = "Dependent",
                                                         xLabel = "Rep",
                                                         testValue = options$mu,
                                                         addLines = FALSE, horiz = FALSE))
    if(isTryError(p))
      singlePlot$setError(.extractErrorMessage(p))
    else
      singlePlot$plotObject <- p

  }

  return()
}

.plotDatasetsOLD2 <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerRainCloudPlots"]]) || !options$dataPlot)
    return()

  jaspContainer[["containerRainCloudPlots"]] <- createJaspContainer(gettext("Data plots"),
                                                                    dependencies = c("dataPlotShowN", "dataPlot"))

  rainData <- jaspContainer[["simulatedDatasets"]][["object"]]

  nRows <- min((options$dataPlotShowN %/% 3) + 1, 3)
  nCols <- ifelse(nRows == 1, options$dataPlotShowN, 3)
  addDots <- ifelse(options$dataPlotShowN > 9, TRUE, FALSE)

  matrixPlot <- createJaspPlot(title = gettext("Samples"), width = 960/(3/nCols),
                               height = 800/(3/nRows))

  plotList <- list()

  if (addDots) {
    loopingVector <- c(1, 2, (options$dataPlotShowN-6):options$dataPlotShowN)
  } else {
    loopingVector <- 1:options$dataPlotShowN
  }

  listCounter <- 1
  for (i in loopingVector) {

    if (addDots && listCounter == 2) {
      plotList[[listCounter]] <- ggplot2::ggplot(data = data.frame(x = c(-1,1), y = c(-1,1))) + ggplot2::theme_void() +
        ggplot2::annotate("text", x = -3, y = -2.5,
                          label = "...",
                          size = 60)
    } else {

      thisRainData <- data.frame(y = rainData[[i]],
                                 group = rep(1, options$n))

      plotList[[listCounter]] <- try(jaspTTests::.descriptivesPlotsRainCloudFill(thisRainData, "y", "group",
                                                                                 yLabel = gettext("Dependent"),
                                                                                 xLabel = gettextf("Repetition %d", i),
                                                                                 testValue = options$mu,
                                                                                 addLines = FALSE, horiz = FALSE))


      if(isTryError(plotList[[listCounter]]))
        matrixPlot$setError(.extractErrorMessage(plotList[[listCounter]]))

    }
    listCounter <- listCounter + 1
  }

  p <- jaspGraphs::ggMatrixPlot(matrix(plotList, nrow = nRows, ncol = nCols, byrow = TRUE))
  matrixPlot$plotObject <- p
  jaspContainer[["containerRainCloudPlots"]][["plotObject"]] <- matrixPlot

  return()
}
