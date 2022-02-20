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
  ready <- (inputType == "dataRandom" |
              (inputType == "dataSequence" && options[["lstDescDataSequenceInput"]] != "") | 
              (inputType == "dataVariable" && options[["selectedVariable"]] != ""))
  data <- .getDataLSTdesc(jaspResults, options, inputType)
  
  #checking whether data is discrete or continuous, whereas only integers are treated as discrete
  discrete <- ifelse(all(data$x == as.integer(data$x)), TRUE, FALSE)
  
  if(options[["LSdescCentralOrSpread"]] == "LSdescCentralTendency"){
    if (options[["LSdescExplanationC"]])
      .descExplanation(jaspResults, options)
    if (options[["LSdescHistBar"]]) {
      .lstDescCreateHistogramOrBarplot(jaspResults, options, data, ready, discrete)
    }
    if (options[["LSdescDotPlot"]])
      .lstDescCreateDotplot(jaspResults, options, data, ready, discrete)
  }
  
  if(options[["LSdescCentralOrSpread"]] == "LSdescCentralTendency"){
    #all spread plots go here
  }
  
}


.descExplanation <- function(jaspResults, options) {
  jaspResults[["descExplanation"]] <- createJaspContainer(gettext("Explanation"))
  jaspResults[["descExplanation"]]$position <- 1
  
  mean <- 0
  sd <- 1
  skew <- 1000
  
  pdPlot <- createJaspPlot(title = gettext("Theoretical example distribution"), width = 700, height = 400)
  pdPlot$position <- 1
  
  
  xLimits <- c(mean - 5 * sd, mean + 5 * sd)
  yLimits <- c(-.1 , .5)
  distLimits <- c(mean - 4 * sd, mean + 4 * sd)
  hLineData <- data.frame(x = distLimits, y = rep(0, 2))
  
  df <- data.frame(x = .scaledSkewedNormal(100000, xi = mean, omega = sd, alpha = skew))
  df <- subset(df, df$x > distLimits[1] & df$x < distLimits[2]) # remove values outside limits
  pdPlotObject <-  ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_density(mapping = ggplot2::aes(y = ..density..), n = 2^10, bw = sd/3, size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = hLineData, size = 1) +
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
  
  plotData <- ggplot2::ggplot_build(pdPlotObject)$data[[1]]
  allCTs <- options[["LSdescCT"]] == "LSdescMMM"
  
  if(options[["LSdescCT"]] == "LSdescMedian"| allCTs){
    median <- median(df$x)
    medianLineHeight <- plotData$y[which.min(abs(plotData$x - median))]
    medianLineData <- data.frame(x = c(rep(median, 2)), y = c(0, medianLineHeight))
    fiftyPercentLabels <- data.frame(x = c(median + .85, median - .85), y = rep(.1, 2), label = rep("50%", 2))
    labelXPos <- ifelse(allCTs, 4, median)
    labelYPos <- ifelse(allCTs, max(yLimits) * .85, max(yLimits) * .55)
    
    pdPlotObject <- pdPlotObject +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = 0, ymax = y), data = subset(plotData, plotData$x > median),
                           fill = "grey") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = 0, ymax = y), data = subset(plotData, plotData$x < median),
                           fill = "grey", alpha = .5) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = medianLineData, size = 1, color = "green") +
      ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Median")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = 6) +
      ggplot2::geom_text(data = fiftyPercentLabels, mapping = ggplot2::aes(x = x, y = y, label = label), size = 7)
    text <- gettext("Text for Median:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  if(options[["LSdescCT"]] == "LSdescMode"| allCTs){
    mode <- plotData$x[plotData$y == max(plotData$y)]
    modeVLineData <- data.frame(x = c(rep(mode, 2)), y = c(0, max(plotData$y)))
    modeHLineData <- data.frame(x = c(mode - .7, mode + .7), y = rep(max(plotData$y) + 0.003, 2))
    labelXPos <- ifelse(allCTs, 4, mode)
    labelYPos <- ifelse(allCTs, max(yLimits) * .75, max(yLimits) * .45)
    pdPlotObject <- pdPlotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = modeVLineData, size = 1, color = "blue") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = modeHLineData, size = 1, color = "lightblue") +
      ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Mode")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = 6)
    text <- gettext("Text for Mode:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  if(options[["LSdescCT"]] == "LSdescMean" | allCTs){
    meanLineHeight <- plotData$y[which.min(abs(plotData$x - mean))]
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
    labelXPos <- ifelse(allCTs, 4, mean)
    labelYPos <- ifelse(allCTs, max(yLimits) * .95, max(yLimits) * .2)
    
    pdPlotObject <- pdPlotObject + 
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = "red") +
      ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = triangleData, fill = "red") +
      ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = balanceBaseData, fill = "rosybrown2", alpha = .3) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceBaseLineData, size = 1.2, color = "rosybrown2") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData1, size = 1.2, color = "rosybrown2") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData2, size = 1, color = "rosybrown2") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData3, size = 1.2, color = "rosybrown2") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData4, size = 1, color = "rosybrown2") +
      ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Mean")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = 6)
    text <- gettext("Text for Mean : Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  pdPlot$plotObject <- pdPlotObject
  
  
  if (options[["LSdescCT"]] == "LSdescMMM"){
    text <- gettext("Text for comparison:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  
  jaspResults[["descExplanation"]][["Plot"]] <- pdPlot
  jaspResults[["descExplanation"]][["Text"]] <- createJaspHtml(text, "p")
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

.sampleRandomDataForLSTdesc <- function(jaspResults, options){
  set.seed(options[["lstDescSampleSeed"]])
  n <- options[["lstDescSampleN"]]
  if (options[["lstDescSampleDistType"]] == "lstSampleDistDiscrete") {
    if (options[["LSdescDiscreteDistributions"]] == "binomialDist") {
      data <- rbinom(n, 10, prob = .5)
    } else if (options[["LSdescDiscreteDistributions"]] == "poissonDist") {
      data <- rpois(n, 1)
    }
  } else if (options[["lstDescSampleDistType"]] == "lstSampleDistCont") {
    if (options[["LSdescContinuousDistributions"]] == "skewedNormal") {
      data <- sn::rsn(n, alpha = 100)
    } else if (options[["LSdescContinuousDistributions"]] == "uniform") {
      data <- runif(n = n, min = 0, max = 5)
    } else if (options[["LSdescContinuousDistributions"]] == "normal"){
      data <- rnorm(n, 0, 10)
    }
  }
  df <- data.frame(x = data)
  return(df)
}


.readInputSequenceForLSTdesc <- function(jaspResults, options){
  inputSequence <- options[["lstDescDataSequenceInput"]]
  inputSequence <- unlist(strsplit(inputSequence, split = ","))
  inputSequence <- gsub(" ", "", inputSequence, fixed = TRUE)
  df <- data.frame(x = as.numeric(inputSequence))
  return(df)
}


.readDataLSTdesc <- function(jaspResults, options){
  variable <- unlist(options[["selectedVariable"]])
  variable <- variable[variable != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric = variable)
  df <- data.frame(x = unlist(dataset))
  return(df)
}

.lstDescCreateHistogramOrBarplot <- function(jaspResults, options, data, ready, discrete){
  title <- ifelse(discrete, "Barplot", "Histogram")
  jaspResults[["descHistogramOrBarplot"]] <- createJaspContainer(gettext(title))
  p <- createJaspPlot(title = gettext(title), width = 700, height = 400)
  p$position <- 2
  allCTs <- options[["LSdescCT"]] == "LSdescMMM"
  
  if (ready) {
    if (discrete){
      xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(data$x)))
      if (allCTs){
        xLimits <- c(xBreaks[1] - abs(xBreaks[2] - xBreaks[1])/2, xBreaks[length(xBreaks)] * 1.4)
      } else {
        xLimits <- range(xBreaks[1] - abs(xBreaks[2] - xBreaks[1])/2, max(xBreaks))
      }
      yBreaks <- unique(round(jaspGraphs::getPrettyAxisBreaks(c(0, table(data$x)))))
      yLimits <- range(yBreaks) * 1.3
      yMax <- max(yLimits)
      plotObject <- ggplot2::ggplot(data, ggplot2::aes(x = x)) + ggplot2::geom_bar(fill = "grey",
                                                                                   col = "black", size = .3)
      if (options[["LSdescHistBarRugs"]])
        plotObject <- plotObject + ggplot2::geom_rug(data = data, mapping = ggplot2::aes(x = x), sides = "b")
      plotObject <- plotObject + 
        ggplot2::scale_y_continuous(name = "Counts", breaks = yBreaks, limits = yLimits) + 
        ggplot2::scale_x_continuous(name = "Observations", breaks = xBreaks, limits = xLimits) + 
        jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
    } else{
      displayDensity <- options[["LSdescHistCountOrDens"]] == "LSdescHistDens"
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
      if (allCTs) {
        xLimits <- c(xBreaks[1], xBreaks[length(xBreaks)] * 1.4)
      } else {
        xLimits <- range(xBreaks)
      }
      plotObject <- jaspDescriptives:::.plotMarginal(data$x, variableName = "Observations", displayDensity = displayDensity,
                                                     binWidthType = "sturges", rugs = options[["LSdescHistBarRugs"]]) 
      yMax <- max(ggplot2::ggplot_build(plotObject)$data[[1]]$y) * 1.3
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(ggplot2::ggplot_build(plotObject)$data[[1]]$y)
      yLimits <- c(0, yMax)
      plotObject <- plotObject + ggplot2::scale_y_continuous(limits = yLimits, breaks = yBreaks) +
        ggplot2::scale_x_continuous(name = "Observations", breaks = xBreaks, limits = xLimits) 
    }
    plotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, plotObject, yMax = yMax, discrete = discrete)
    p$plotObject <- plotObject
  }
  jaspResults[["descHistogramOrBarplot"]] <- p
}

.lstDescCreateDotplot <- function(jaspResults, options, data, ready, discrete){
  jaspResults[["descDotplot"]] <- createJaspContainer(gettext("Dotplot"))
  height <- 400 + (length(data$x) / 50) * 25
  dp <- createJaspPlot(title = gettext("Dotplot"), width = 700, height = height)
  dp$position <- 3
  
  if (ready){
    dpPlotObjectList <- .lstDescCreateDotPlotObject(data, options)
    dpPlotObject <- dpPlotObjectList$p
    if ((options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMode" | options[["LSdescCT"]] == "LSdescMMM"))
      dpPlotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, dpPlotObject, yMax = dpPlotObjectList$yMax,
                                                lines = FALSE, discrete = discrete)
    dp$plotObject <- dpPlotObject
  }
  
  jaspResults[["descDotplot"]] <- dp
  
}

.drawMeanMedianOrModeLine <- function(jaspResults, options, data, plot, yMax, lines = TRUE, discrete){
  xBreaks <- pretty(data$x)
  n <- length(data$x)
  labelSize <- 3 + 300 / (100 + n * 2)
  if(options[["LSdescCT"]] == "LSdescMode"| options[["LSdescCT"]] == "LSdescMMM"){
    xPos <- median(xBreaks)
    modeLines <- lines
    noMode <- length(unique(data$x)) == length(data$x)
    if (noMode) {
      modeLabelData <- data.frame(x = xPos, y = yMax * .9, label = gettext("All values unique, \n no mode defined."))
      modeLines <- FALSE # also plot no line then
    } else {
      tableData <- table(data$x)
      modeCol <- tableData[tableData == max(tableData)]
      mode <- as.numeric(names(modeCol))
      modeHeight <- as.numeric(modeCol)
      plotData <- ggplot2::ggplot_build(plot)$data
      if (length(mode) == 1) {
        modeLabelText <- ifelse(discrete, gettextf("Mode = %i (Count = %i)", mode, modeHeight),
                                gettextf("Mode = %.2f (Count = %i)", mode, modeHeight))
        modeLabelData <- data.frame(x = xPos, y = yMax, label = modeLabelText)
      } else {
        firstMode <- ifelse(discrete, gettextf("Mode = {%i", mode[1]), gettextf("Mode = {%.2f", mode[1]))
        if (discrete) {
          otherModes <- gettextf(rep(", %i", length(mode) - 1), mode[2:length(mode)])
        } else {
          otherModes <- gettextf(rep(", %.2f", length(mode) - 1), mode[2:length(mode)])
        }
        otherModes <- paste(otherModes, collapse = "")
        modeHeightString <- gettextf( "} (Count = %i)", modeHeight)
        modeLabelText <- paste(firstMode, otherModes, modeHeightString, sep = "")
        modeLabelData <- data.frame(x = xPos, y = yMax, label = modeLabelText)
      }
    }
    if (modeLines){
      if (discrete) {
        modeLineYPos <- max(plotData[[1]]$ymax)
        for (i in 1:length(mode)){
          modeLineXPos <- plotData[[1]]$x[plotData[[1]]$xmin < mode[i] & plotData[[1]]$xmax > mode[i]][1]
          lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos * 1.05))
          plot <- plot + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = "blue", size = 3)
        }
      } else {
        modeLineYPos <- max(plotData[[2]]$ymax)
        for (i in 1:length(mode)){
          modeLineXPos <- plotData[[2]]$x[plotData[[2]]$xmin < mode[i] & plotData[[2]]$xmax > mode[i]][1]
          lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos * 1.05))
          plot <- plot + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = "blue", size = 3)
        }
      }
      plot <- plot + ggplot2::geom_hline(yintercept = modeLineYPos, size = 1, color = "blue") 
    }
    if (options[["LSdescCT"]] == "LSdescMode") {
      plot <- plot + ggplot2::geom_label(data = modeLabelData, 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = labelSize)
    } else if (options[["LSdescCT"]] == "LSdescMMM") {
      modeLabelData$x <- max(xBreaks) * 1.2
      modeLabelData$y <- ifelse(noMode, yMax * 0.75, yMax * .8)
      plot <- plot + ggplot2::geom_label(data = modeLabelData, 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = labelSize)
    }
  }
  if (options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMMM") {
    mean <- mean(data$x)
    if (lines)
      plot <- plot + ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y),
                                        data = data.frame(x = rep(mean, 2), y = c(0, yMax)), size = 1, color = "red")
    if (options[["LSdescCT"]] == "LSdescMean") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = mean, y = yMax, label = gettextf("Mean = %.2f", mean)), 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = labelSize)
    } else if (options[["LSdescCT"]] == "LSdescMMM") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = max(xBreaks) * 1.2, y = yMax, label = gettextf("Mean = %.2f", mean)), 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = labelSize)
    }
  }
  if (options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM") {
    median <- median(data$x)
    if (lines)
      plot <- plot + ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y),
                                        data = data.frame(x = rep(median, 2), y = c(0, yMax)), size = 1, color = "green")
    if (options[["LSdescCT"]] == "LSdescMedian") {
      plot <- plot +  ggplot2::geom_label(data = data.frame(x = median, y = yMax, label = gettextf("Median = %.2f", median)), 
                                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = labelSize)
    } else if (options[["LSdescCT"]] == "LSdescMMM") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = max(xBreaks) * 1.2, y = yMax * .9, label = gettextf("Median = %.2f", median)), 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = labelSize)
    }
  }
  return(plot)
}


.lstDescCreateDotPlotObject <- function(data, options){
  n <- length(data$x)
  if (length(unique(data$x)) == 1){ 
    dotsize <- .0333
  } else if (n > 50){
    dotsize <- 1 - (log(n)) / 30
  } else {
    dotsize <- 1
  }
  
  allCTs <- options[["LSdescCT"]] == "LSdescMMM"
  labelSize <- 3 + 300 / (100 + n * 2)
  
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  xBuffer <- ifelse(allCTs, 1.4, 1.1)
  xLimits <- c(min(xBreaks) * .9, max(xBreaks) * xBuffer)
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = x)) +
    ggplot2::geom_dotplot(binaxis = 'x', stackdir = 'up', dotsize = dotsize, fill = "grey") +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = xLimits) +
    ggplot2::coord_fixed() 
  
  if (options[["LSdescDotPlotRugs"]])
    p <- p + ggplot2::geom_rug(data = data, mapping = ggplot2::aes(x = x), sides = "b")
  
  pData <- ggplot2::ggplot_build(p)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
  yBreaks <- yLabels * dotWidth
  yMax <- ifelse(max(yBreaks) < (10 * dotWidth), (10 * dotWidth), max(yBreaks))
  yLimits <-  c(0, yMax + (2 * dotWidth))
  
  p <- p + ggplot2::scale_y_continuous(name = "Counts", limits = yLimits, breaks = yBreaks, labels = yLabels) + 
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  if (options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM") {
    sortedDf <- data.frame(x = sort(data$x))
    halfway <- median(as.numeric(rownames(sortedDf)))
    med <- median(data$x)
    if(n %% 2 != 0){   # if median is a single datapoint
      halfwayDot <- pData[[1]][halfway,]
      y0 <- ifelse(halfwayDot$countidx == 1, dotWidth/2, dotWidth/2 + (halfwayDot$countidx - 1) * dotWidth)
      x0 <- halfwayDot$x
      circleData <- data.frame(x0 = x0, 
                               y0 = y0,
                               r = dotWidth / 2)
      medianLineData <- data.frame(x = c(x0, med),
                                   y = c(y0, max(yLimits) * .9))
      chairData <- data.frame(x = c(x0 - dotWidth * .65, x0 - dotWidth * .65,
                                    x0 + dotWidth * .65, x0 + dotWidth * .65),
                              y = c(y0 + dotWidth/2, y0, y0, y0 - dotWidth/2))
      p <- p + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                    inherit.aes = FALSE, fill = "green") +
        ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = chairData, size = 1, color = "springgreen4")
      if (!allCTs)
        p <- p +  ggplot2::geom_path(data = medianLineData, mapping = ggplot2::aes(x = x, y = y), color = "green",
                                     size = 1)
    } else {  # if median is the average of two points
      halfwayDots <- list("lowerDot" = pData[[1]][halfway - .5,],
                          "upperDot" = pData[[1]][halfway + .5,])
      y0lower <- ifelse(halfwayDots$lowerDot$countidx == 1, dotWidth / 2, dotWidth / 2 + (halfwayDots$lowerDot$countidx - 1) * dotWidth)
      y0upper <- ifelse(halfwayDots$upperDot$countidx == 1, dotWidth / 2, dotWidth / 2 + (halfwayDots$upperDot$countidx - 1) * dotWidth)
      if (all(c(halfwayDots$lowerDot$count, halfwayDots$upperDot$count) == 1)) {
        start <- c(pi * 2, pi * 2)
        end <-c(pi * 3, pi)
      } else {
        start <- c(pi * 1.5, pi / 2)
        end <- c(pi * 2.5, pi * 1.5)
      }
      circleData <- data.frame(x0 = c(halfwayDots$lowerDot$x, halfwayDots$upperDot$x),
                               y0 = c(y0lower, y0upper),
                               r = rep(dotWidth/2, 2),
                               r0 = rep(0, 2),
                               start = start,
                               end = end)
      chairData <- data.frame(x = c(halfwayDots$lowerDot$x - dotWidth * .65, halfwayDots$lowerDot$x - dotWidth * .65,
                                    halfwayDots$lowerDot$x + dotWidth * .65, halfwayDots$lowerDot$x + dotWidth * .65),
                              y = c(y0lower + dotWidth, y0lower + dotWidth/2, y0lower + dotWidth/2, y0lower))
      medianLineData1 <- data.frame(x = c(halfwayDots$lowerDot$x, med),
                                    y = c(y0lower, max(yLimits) * .95))
      medianLineData2 <- data.frame(x = c(halfwayDots$upperDot$x, med),
                                    y = c(y0upper, max(yLimits) * .95))
      
      p <- p + ggforce::geom_arc_bar(data = circleData,
                                     mapping = ggplot2::aes(x0 = x0, y0 = y0, r0 = r0, r = r,  start = start, end = end),
                                     inherit.aes = FALSE, fill = "green") +
        ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = chairData, size = 1, color = "springgreen4")
      if(!allCTs){
        p <- p + ggplot2::geom_path(data = medianLineData1, mapping = ggplot2::aes(x = x, y = y), color = "green", size = 1) +
          ggplot2::geom_path(data = medianLineData2, mapping = ggplot2::aes(x = x, y = y), color = "green", size = 1)
      }
    }
    if(!allCTs)
      p <- p + ggplot2::geom_label(data = data.frame(x = med, y = max(yLimits) * .95, label = gettextf("Median = %.2f", median(data$x))), 
                                   mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = labelSize)
  }
  if (options[["LSdescCT"]] == "LSdescMean" || options[["LSdescCT"]] == "LSdescMMM"){
    mean <- mean(data$x)
    y0 <- dotWidth / 2
    circleData <- data.frame(x0 = mean, 
                             y0 = y0,
                             r = dotWidth / 2)
    meanLineData <- data.frame(x = c(mean, mean),
                               y = c(y0 + dotWidth / 2,  max(yLimits) * .95))
    p <- p + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                  inherit.aes = FALSE, fill = "red", alpha = .3, color = "red", n = 4)
    if (!allCTs)
      p <- p + ggplot2::geom_path(data = meanLineData, mapping = ggplot2::aes(x = x, y = y), color = "red", size = .8)
  }
  if (options[["LSdescCT"]] == "LSdescMode" || options[["LSdescCT"]] == "LSdescMMM"){
    if(length(unique(data$x)) != n){
      modeTable <- table(data$x)
      mode <- as.numeric(names(modeTable[modeTable == max(modeTable)]))
      modeLineYPos <- dotWidth * max(pData[[1]]$count)
      for (i in 1:length(mode)){
        modeLineXPos <- pData[[1]]$x[pData[[1]]$xmin < mode[i] & pData[[1]]$xmax > mode[i]][1]
        lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos + dotWidth / 2))
        p <- p + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = "blue", size = 3)
      }
      p <- p + ggplot2::geom_hline(yintercept = modeLineYPos, color = "blue", size = 1)
    }
  }
  return(list("p" = p, "yMax" = max(yLimits) * .95))
}
