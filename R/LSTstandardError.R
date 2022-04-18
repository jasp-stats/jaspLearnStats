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

LSTstandardError <- function(jaspResults, dataset, options) {
  set.seed(options[["cltSampleSeed"]])
  colors <- .getColors(options[["cltColorPalette"]])
  parentData <- .generateParentData(options)
  samples <- .cltTakeSamples(jaspResults, options = options, data = parentData)
  
  if (options[["parentShow"]])
    jaspResults[["seParentDistribution"]] <- .cltParentDistribution(jaspResults, options = options, colors, showMean = TRUE,
                                                                    showSD = TRUE, labelsInCorner = TRUE)
  
  if (options[["samplesShow"]]){
    maxSamples <- length(samples)
    fromTo <- .getFromToSampleShow(options[["cltSampleShowType"]], maxSamples, singleValue = options[["cltFirstOrLastSamples"]],
                                   start = options[["cltFromSample"]], stop = options[["cltToSample"]])
    from <- fromTo[1]
    to <- fromTo[2]
    jaspResults[["seSamples"]] <- .cltPlotSamples(jaspResults, options, samples, from, to, colors, showMean = TRUE, showSE = TRUE, 
                                                  showSD = TRUE, labelsInCorner = TRUE)
  }
  
  if (options[["samplingDistShow"]])
    jaspResults[["seSamplingDistribution"]] <- .cltSamplingDistribution(jaspResults, options, samples, colors, showSD = TRUE)
  
  return()
}