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

LSTcentralLimitTheorem <- function(jaspResults, dataset, options) {
  set.seed(options[["cltSampleSeed"]])
  parentData <- .generateParentData(options)
  samples <- .cltTakeSamples(jaspResults, options = options, data = parentData)
  
  if (options[["parentShow"]])
    jaspResults[["cltParentDistribution"]] <- .cltParentDistribution(jaspResults, options = options)
  
  if (options[["samplesShow"]])
    jaspResults[["cltSamples"]] <- .cltPlotSamples(jaspResults, options = options, samples = samples)
  
  if (options[["samplingDistShow"]])
    jaspResults[["cltSamplingDistribution"]] <- .cltSamplingDistribution(jaspResults, options = options, samples = samples)
  
  return()
}

.cltParentDistribution <- function(jaspResults, options) {
  distribution <- options[["cltParentDistribution"]]
  mean <- options[["cltMean"]]
  sd <- options[["cltStdDev"]]
  range <- options[["cltRange"]]
  skewDirection <- options[["cltSkewDirection"]]
  if (options[["cltSkewIntensity"]] == "low"){
    skew <- 2
  } else if (options[["cltSkewIntensity"]] == "medium") {
    skew <- 5
  } else if (options[["cltSkewIntensity"]] == "high") {
    skew <- 50
  }

  if (distribution == "binomial") {
    prob <- options[["binomProb"]]
    showMean <- FALSE
  } else {
    showMean <- TRUE
    prob <- NA
  }

  pdPlot <- createJaspPlot(title = gettext("Parent Distribution"), width = 700, height = 400)
  pdPlot$position <- 1
  pdPlot$dependOn(options = c("cltParentDistribution",
                              "parentShow",
                              "cltMean",
                              "cltStdDev",
                              "cltRange",
                              "cltSkewDirection",
                              "cltSkewIntensity"))
  pdPlot$plotObject <- .distributionPlotFunction(distribution = distribution, mean = mean, sd = sd, range = range,
                                                 skew = skew, prob = prob, skewDirection = skewDirection,
                                                 fillColor = "coral", showMean = showMean)
  return(pdPlot)
}


.cltTakeSamples <- function(jaspResults, options, data, replace = TRUE) {
  n <- options[["cltSampleSize"]]
  k <- options[["cltSampleAmount"]]
  sampleList <- list()
  if (replace) {
    for (i in 1:k) {
      sampleList[[i]] <- sample(x = data[["x"]], size = n, replace = TRUE)
    }
    return(sampleList)
  } else {
    population <- data
    if (options[["cltParentDistribution"]] == "binomial") {
      sampleIndiceList <- list()
      indices <- 1:length(population[["x"]])
      for(i in 1:k){
        sampleIndices <- sample(indices, size = n, replace = FALSE)
        sampleIndiceList[[i]] <- sampleIndices
        indicesWithoutSamples <- indices[-.getIndices(sampleIndices, indices)]
        indices <- indicesWithoutSamples
      }
      sampleList <- .getSampleValuesFromIndices(sampleIndiceList, population)
    } else {
    for(i in 1:k){
      indices <- 1:length(population[["x"]])
      sampleIndices <- sample(indices, size = n, replace = FALSE)
      sampleList[[i]] <- population[["x"]][sampleIndices]
      populationWithoutSamples <- population[["x"]][-sampleIndices]
      population <- data.frame(x = populationWithoutSamples)
    }
    orderedData <- data.frame(x = sort(data$x))
    sampleIndiceList <- lapply(sampleList, .getIndices, population = orderedData)
    }
    return(list("samples" = sampleList,
                "indices" = sampleIndiceList))
  }
}

.getIndices <- function(sampleVector, population){
  sampleIndices <- c()
  for (i in 1:length(sampleVector)){
    sampleIndices <- c(sampleIndices, which(sampleVector[i] == population))
  }
  return(sampleIndices)
}

.getSampleValuesFromIndices <- function(sampleIndiceList, population){
  sampleList <- list()
  for (i in 1:length(sampleIndiceList)) {
    sampleList[[i]] <- population$x[sampleIndiceList[[i]]]
  }
  return(sampleList)
}


.cltPlotSamples <- function(jaspResults, options, samples) {
  samples <- samples[1:7]
  plotMat <- matrix(list(), 4, 2)

  allCounts <- vector()
  for (i in 1:7) {
    sample <- samples[[i]]
    binWidthType1 <- .getBinWidth(variable = sample, options = options)
    h1 <- hist(x = sample, plot = FALSE, right = FALSE, breaks = binWidthType1)
    counts <- h1$counts
    allCounts <- c(allCounts, counts)
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(allCounts)))
  yLimits <- range(yBreaks)
  if (options[["cltParentDistribution"]] == "binomial") {
    xBreaks <- c(0 , 1)
    xLimits <- c(-.5, 1.5)
  } else {
    binWidthType2 <- .getBinWidth(unlist(samples), options)
    h2 <- hist(unlist(samples), plot = FALSE, right = FALSE, breaks = binWidthType2)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h2$breaks))
    xLimits <- range(xBreaks)
    binWidth <- (h2$breaks[2] - h2$breaks[1])
  }

  index <- 0
  for (i in 1:4) {
    for (j in 1:2) {
      index <-  index + 1
      if (index == 8) {
        samplePlot <- ggplot2::ggplot() + ggplot2::theme_void() +
          ggplot2::annotate(geom = "text", x = 0, y = 0, label = gettextf("... until Sample Nr. %i",
                                                                          options[["cltSampleAmount"]]), size = 10)
      } else {
        if (options[["cltParentDistribution"]] == "binomial") {
          data <- factor(samples[[index]], level = c(0, 1))
          countTable <- table(data)
          df <- data.frame("biValues" = c(0, 1), "counts" = as.vector(countTable), colors = c("c1", "c2"))
          samplePlot <- ggplot2::ggplot() +
            ggplot2::geom_bar(data = df, mapping = ggplot2::aes(x = biValues, y = counts, fill = colors),
                              size = .3, stat = "identity", color = "black") +
            ggplot2::scale_fill_manual(values = c("grey20", "grey90"))
        } else {
          mean <- mean(samples[[index]])
          samplePlot <- ggplot2::ggplot(data.frame(x = samples[[index]]), ggplot2::aes(x = x)) +
            ggplot2::geom_histogram(mapping = ggplot2::aes(y =..count..),closed = "left", fill = "coral", col = "black",
                                    size = .7, binwidth = binWidth, center = binWidth/2) +
            ggplot2::geom_vline(xintercept = mean, color = "cornflowerblue", size = 1) +
            ggplot2::geom_label(data = data.frame(x = mean, y = max(yLimits)*0.95, label = gettextf("Mean: %.2f", mean)),
                                mapping = ggplot2::aes(x = x, y = y, label = label), color = "cornflowerblue", size = 6)
          if (options[["samplesShowRugs"]])
            samplePlot <- samplePlot + ggplot2::geom_rug()
        }
        samplePlot <- samplePlot + ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits, name = "Count") +
          ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
          ggplot2::ggtitle(gettextf("Sample Nr. %i", index)) +
          jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
      }
      plotMat[[i,j]] <- samplePlot
    }
  }
  sampleMatrixPlot <- createJaspPlot(title = gettext("Samples"), width = 1200, height = 1000)
  sampleMatrixPlot$position <- 2
  sampleMatrixPlot$dependOn(options = c("cltParentDistribution",
                                        "cltMean",
                                        "cltStdDev",
                                        "cltRange",
                                        "cltSkewDirection",
                                        "cltSkewIntensity",
                                        "cltSampleSize",
                                        "cltSampleAmount",
                                        "samplesShow",
                                        "samplesShowRugs"))

  sampleMatrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)

  return(sampleMatrixPlot)
}

.generateParentData <- function(options, finite = FALSE) {
  if (finite) {
    n <- finite
  } else {
    n <- 100000
  }
  distribution <- options[["cltParentDistribution"]]
  mean <- options[["cltMean"]]
  sd <- options[["cltStdDev"]]
  range <- options[["cltRange"]]

  if (distribution == "normal") {
    data <- rnorm(n, mean, sd)
  } else if (distribution == "uniform") {
    min <- mean - range / 2
    max <- mean + range / 2
    data <- runif(n = n, min = min, max = max)
  } else if (distribution == "skewed") {
    if (options[["cltSkewIntensity"]] == "low") {
      skew <- 1.5
    } else if (options[["cltSkewIntensity"]] == "medium") {
      skew <- 3
    } else if (options[["cltSkewIntensity"]] == "high") {
      skew <- 10
    }
    if (options[["cltSkewDirection"]] == "left") {
      skew <- skew * -1
    }
    data <- .scaledSkewedNormal(n, xi = mean, omega = sd,  alpha = skew)
  } else if (distribution == "binomial") {
    prob <- options[["binomProb"]]
    data <- c(rep(1, as.integer(n * prob)), rep(2, as.integer(n * (1 - prob))))
  }
  df <- data.frame(x = data)
  return(df)
}


.cltSamplingDistribution <- function(jaspResults, options, samples) {
  means <- lapply(X = samples, FUN = mean)
  meanDf <- data.frame(x = unlist(means))
  n <- length(samples)
  meanOfMeans <- mean(meanDf[["x"]])
  sdOfMeans <- sd(meanDf[["x"]])

  binWidthType <- .getBinWidth(variable = meanDf[["x"]], options = options)
  h <- hist(meanDf[["x"]], plot = FALSE, breaks = binWidthType)
  counts <- h$counts
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(counts)*1.1))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h$breaks), min.n = 4)
  xLimits <- range(xBreaks)
  binWidth <- (h$breaks[2] - h$breaks[1])

  sdPlotObject <- ggplot2::ggplot(meanDf, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(mapping = ggplot2::aes(y =..count..),
                            closed = "left", fill = "cornflowerblue", col = "black", size = .7,
                            binwidth = binWidth, center = binWidth/2)
  if (options[["samplingDistShowRugs"]])
    sdPlotObject <- sdPlotObject + ggplot2::geom_rug()
  sdPlotObject <- sdPlotObject +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
    ggplot2::geom_vline(xintercept = meanOfMeans, color = "darkgoldenrod1", size = 1)
  if (options[["samplingDistShowNormal"]]) {
    sdPlotObject <- sdPlotObject +
      ggplot2::stat_function(fun = function(x)dnorm(x, mean = meanOfMeans, sd = sdOfMeans) * binWidth * n, size = 1)
  }
  yPos <- max(ggplot2::ggplot_build(sdPlotObject)$data[[1]]$y) * 1.1
  yLimits <- range(c(yBreaks, yPos))
  sdPlotObject <- sdPlotObject + ggplot2::geom_label(data = data.frame(x = meanOfMeans, y = yPos,
                                                                       label = gettextf("Mean: %.2f", meanOfMeans)),
                                                     mapping = ggplot2::aes(x = x, y = y, label = label),
                                                     color = "darkgoldenrod1", size = 6) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits, name = "Count") +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()

  sdPlot <- createJaspPlot(title = gettext("Sampling Distribution of the Mean"), width = 700, height = 400)
  sdPlot$position <- 3
  sdPlot$dependOn(options = c("cltParentDistribution",
                              "cltMean",
                              "cltStdDev",
                              "cltRange",
                              "cltSkewDirection",
                              "cltSkewIntensity",
                              "cltSampleSize",
                              "cltSampleAmount",
                              "samplingDistShow",
                              "samplingDistShowNormal",
                              "samplingDistShowRugs"))

  sdPlot$plotObject <- sdPlotObject

  return(sdPlot)
}

.scaledSkewedNormal <- function(n, xi= 0 , omega = 1, alpha = 0, tau = 0) {
  y <- xi + omega*scale(sn::rsn(n, xi = xi, omega = omega, alpha = alpha, tau = tau))
  return(y)
}


.distributionPlotFunction <- function(distribution = c("normal", "uniform", "skewed"), mean, sd = NA, range = NA, skew = NA, prob = NA,
                                      skewDirection = "right", fillColor,
                                      showMean = TRUE, returnData = FALSE) {
  if (distribution == "normal") {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(mean - 3 * sd, mean + 3 * sd))
    xLimits <- range(xBreaks)
    pdPlotObject <-  ggplot2::ggplot(data.frame(x = xBreaks), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = dnorm, n = 10000, args = list(mean = mean, sd = sd), geom = "area", fill = fillColor,
                             color = "black")
    if (returnData)
      df <- data.frame(x = rnorm(n = 100000, mean = mean, sd = sd))
  } else if (distribution == "uniform") {
    min <- mean - range / 2
    max <- mean + range / 2
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min - range / 4, max + range / 4))
    xLimits <- range(xBreaks)
    pdPlotObject <-  ggplot2::ggplot(data.frame(x = xBreaks), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = dunif, n = 100, args = list(min = min, max = max), geom = "area", fill = fillColor) +
      ggplot2::stat_function(fun = dunif, n = 100, args = list(min = min, max = max))
    if (returnData)
      df <- data.frame(x = runif(n = 100000, min = min, max = max))
  } else if (distribution == "skewed") {
    if (skewDirection == "left")
      skew <- skew * -1
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(mean - 3*sd, mean + 3*sd ))
    xLimits <- range(xBreaks)
    df <- data.frame(x = .scaledSkewedNormal(100000, xi = mean, omega = sd, alpha = skew))
    pdPlotObject <-  ggplot2::ggplot(df, ggplot2::aes(x = x)) + ggplot2::geom_density(mapping = ggplot2::aes(y = ..density..),
                                                                                      n = 2^7, bw = sd/3, fill = fillColor)
  } else if (distribution == "binomial") {
    df <- data.frame("biValues" = 0:1, "counts" = c(prob, 1 - prob), "colors" = c("c1", "c2"))
    xBreaks <- c(0, 1)
    xLimits <- c(-.5, 1.5)
    pdPlotObject <- ggplot2::ggplot() + ggplot2::geom_bar(data = df, mapping = ggplot2::aes(x = biValues, y = counts, fill = colors),
                                                          size = .3, stat = "identity", color = "black") +
      ggplot2::scale_fill_manual(values = c("grey20", "grey90"))
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, ggplot2::ggplot_build(pdPlotObject)$data[[1]]$y))
  yLimits <- range(yBreaks)
  if (distribution == "binomial") {
    yName <- "Probability"
  } else {
    yName <- "Density"
  }
  if (showMean) {
    yPos <- max(ggplot2::ggplot_build(pdPlotObject)$data[[1]]$y) * 1.1
    pdPlotObject <- pdPlotObject + ggplot2::geom_vline(xintercept = mean, color = "darkmagenta", size = 1) +
      ggplot2::geom_label(data = data.frame(x = mean, y = yPos, label = gettextf("Mean: %.2f", mean)),
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "darkmagenta", size = 6)
    yLimits <- range(c(yLimits, yPos))
  }
  pdPlotObject <- pdPlotObject + ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits, name = yName) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()

  if (!returnData){
    return(pdPlotObject)
  } else {
    return(list(plotobject = pdPlotObject, data = df))
  }
}

.getBinWidth <- function(variable, options) {
  binWidthType <- options[["cltBinWidthType"]]
  if (binWidthType == "doane") {  # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6*(length(variable) - 2)) / ((length(variable) + 1)*(length(variable) + 3)))
    g1 <- mean(abs(variable)^3)
    k <- 1 + log2(length(variable)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k
  } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
    binWidthType <- 10000
  } else if (binWidthType == "manual") {
    binWidthType <- options[["cltNumberOfBins"]]
  }
  return(binWidthType)
}
