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
   
  pdPlot <- createJaspPlot(title = gettext("Theoretical example distribution"), width = 500, height = 300)
  pdPlot$position <- 1
  
  
  xLimits <- c(mean - 5 * sd, mean + 5 * sd)
  yLimits <- c(-.1 , .5)
  hLineData <- data.frame(x = c(mean - 4 * sd, mean + 4 * sd), y = rep(0, 2))
  
  df <- data.frame(x = .scaledSkewedNormal(100000, xi = mean, omega = sd, alpha = skew))
  pdPlotObject <-  ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_density(mapping = ggplot2::aes(y = ..density..), n = 2^7, bw = sd/3, size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = hLineData, size = 1) +
    ggplot2::ylim(yLimits) +
    ggplot2::xlim(xLimits) +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
  
  if(options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMMM"){
    pdPlotObject <- pdPlotObject + ggplot2::geom_vline(xintercept = mean, size = 1, color = "red") +
      ggplot2::geom_label(data = data.frame(x = mean, y = max(yLimits)*0.95, label = gettext("Mean")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = 6)
    text <- gettext("Text for Mean : Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  
  if(options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM"){
    median <- median(df$x)
    pdPlotObject <- pdPlotObject +
      ggplot2::geom_vline(xintercept = median, size = 1, color = "green") +
      ggplot2::geom_label(data = data.frame(x = median, y = max(yLimits)*0.85, label = gettext("Median")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = 6)
    text <- gettext("Text for Median:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
    
    
    #ggplot2::geom_area(mapping = ggplot2::aes(x = ifelse(x>65 & x< 70 , x, 0)),
    # fill = 'purple') +      MAP DENSITY AT TOP LAYER??
  }
  
  if(options[["LSdescCT"]] == "LSdescMode"| options[["LSdescCT"]] == "LSdescMMM"){
    plotData <- ggplot2::ggplot_build(pdPlotObject)$data[[1]]
    mode <- plotData$x[plotData$y == max(plotData$y)]
    pdPlotObject <- pdPlotObject + ggplot2::geom_vline(xintercept = mode, size = 1, color = "blue") + 
      ggplot2::geom_label(data = data.frame(x = mode, y = max(yLimits)*0.75, label = gettext("Mode")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = 6)
    text <- gettext("Text for Mode:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
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
                                                                                   col = "black", size = .3) + 
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
                                                     binWidthType = "sturges") 
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
      plot <- plot + ggplot2::geom_vline(xintercept = mean, size = 1, color = "red")
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
      plot <- plot + ggplot2::geom_vline(xintercept = median, size = 1, color = "green")
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
  
  pData <- ggplot2::ggplot_build(p)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
  yBreaks <- yLabels * dotWidth
  yMax <- ifelse(max(yBreaks) < (10 * dotWidth), (10 * dotWidth), max(yBreaks))
  yLimits <-  c(0, yMax + (2 * dotWidth))
  
  p <- p + ggplot2::scale_y_continuous(name = "Count", limits = yLimits, breaks = yBreaks, labels = yLabels) + 
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  # ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
  #                axis.title.y = ggplot2::element_blank(),
  #                axis.text.y = ggplot2::element_blank())
  
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
      p <- p + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                    inherit.aes = FALSE, fill = "green")
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
      medianLineData1 <- data.frame(x = c(halfwayDots$lowerDot$x, med),
                                    y = c(y0lower, max(yLimits) * .95))
      medianLineData2 <- data.frame(x = c(halfwayDots$upperDot$x, med),
                                    y = c(y0upper, max(yLimits) * .95))
      
      p <- p + ggforce::geom_arc_bar(data = circleData,
                                     mapping = ggplot2::aes(x0 = x0, y0 = y0, r0 = r0, r = r,  start = start, end = end),
                                     inherit.aes = FALSE, fill = "green")
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
