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

LSTbinomDist <- function(jaspResults, dataset, options) {
  
  options <- jaspDistributions:::.ldRecodeOptionsBinomial(options)
  
  #### Show binomial section ----
  jaspDistributions:::.ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("binomial distribution"), 
                      parSupportMoments = jaspDistributions:::.ldBinomialParsSupportMoments,
                      formulaPMF        = jaspDistributions:::.ldFormulaBinomialPMF, 
                      formulaCMF        = jaspDistributions:::.ldFormulaBinomialCDF)
  
  #### Generate and Display data section ----
  # simulate and read data
  jaspDistributions:::.simulateData(jaspResults, options)
  
  ready <- options[['variable']] != ""
  errors <- FALSE
  # if(ready && is.null(dataset)){
  #   dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])
  #   
  #   variable <- dataset[[.v(options[['variable']])]]
  #   variable <- variable[!is.na(variable)]
  #   errors <- .hasErrors(dataset, type = c("observations", "variance", "infinity", "limits"),
  #                        observations.amount = "<1",
  #                        limits.min = options$support$min, limits.max = options$support$max, 
  #                        exitAnalysisIfErrors = FALSE)
  #   errors <- .ldCheckInteger(variable, errors)
  # }
  
  # overview of the data
  jaspDistributions:::.ldDescriptives(jaspResults, variable, options, ready, errors, "discrete")
  
  #### Fit data and assess fit ----
  jaspDistributions:::.ldMLE(jaspResults, variable, options, ready, errors, jaspDistributions:::.ldFillBinomialEstimatesTable)
  
  return()
}