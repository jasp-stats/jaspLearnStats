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

LSTsampleVariability <- function(jaspResults, dataset, options) {
  set.seed(options[["cltSampleSeed"]])
  if(options[["svParentSizeType"]] == "svParentInfinite"){
    parentData <- .generateParentData(options)
    samples <- .cltTakeSamples(jaspResults, options = options, data = parentData)
  } else {
    parentData <- .generateParentData(options, finite = options[["svParentSize"]])
    samples <- .cltTakeSamples(jaspResults, options = options, data = parentData, replace = FALSE)
  }

  
  if (options[["parentShow"]])
    if(options[["svParentSizeType"]] == "svParentInfinite"){
      jaspResults[["cltParentDistribution"]] <- .cltParentDistribution(jaspResults, options = options)
    } else {
      jaspResults[["cltParentDistribution"]]
    }
  
  if (options[["samplesShow"]])
    jaspResults[["cltSamples"]] <- .cltPlotSamples(jaspResults, options = options, samples = samples)
  
  return()
}
