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

LSTdecisionTree <- function(jaspResults, dataset = NULL, options) {
  decisionDf <- data.frame(from = c("One", "One", "Two or more", "Continuous", "Continuous", "Categorical", "Categorical",
                                    "Continuous2", "Continuous2", "One2", "One2", "Two or more2", "Two or more2", "Two or more2",
                                    "One3", "One3", "Two or more3", "Two or more3", "Two or more3", "One4",
                                    "Two or more4", "Two or more4", "Continuous3", "Categorical2", "Categorical2",
                                    "Continuous4", "Categorical3", "Categorical3", "Categorical3", "Both", "Continuous5", 
                                    "Categorical4", "Continuous6", "Categorical5", "Both2", "Categorical6", "Categorical7",
                                    "Both3", "NULL", "Two", "Two", "More than two", "More than two", "NULL2", "NULL3",
                                    "NULL4", "NULL5", "NULL6", "NULL7", "NULL8", "NULL9", "NULL10", "NULL11", "NULL12",
                                    "NULL13", "NULL14", "NULL15", "Same", "Different", "Same2", "Different2", "NULL16",
                                    "Same3", "Different3", "Both4", "NULL17", "NULL18", "Different4", "NULL19", "Different5",
                                    "Different6", "NULL20", "NULL21", "NULL22", "Pearson correlation \n or regression",
                                    "Dependent t-test", "Independent t-test or \n Point-biserial correlation",
                                    "One-way repeated \n measures ANOVA", "One-way independent \n ANOVA", "Multiple regression",
                                    "Factorial repeated \n measures ANOVA", "Independent factorial \n ANOVA / multiple \n regression",
                                    "Factorial mixed ANOVA", "Multiple regression\n/ANCOVA"),
                           to = c("Continuous", "Categorical", "Continuous2", "One2", "Two or more2", "One3", "Two or more3",
                                  "One4", "Two or more4", "Continuous3", "Categorical2", "Continuous4", "Categorical3", "Both",
                                  "Continuous5", "Categorical4", "Continuous6", "Categorical5", "Both2", "Categorical6",
                                  "Categorical7", "Both3", "NULL", "Two", "More than two", "NULL2", "NULL3", "NULL4", "NULL5",
                                  "NULL6", "NULL7", "NULL8", "NULL9", "NULL10", "NULL11", "NULL12", "NULL13", "NULL14", "NULL15",
                                  "Same", "Different", "Same2", "Different2", "NULL16", "Same3", "Different3", "Both4", "NULL17",
                                  "NULL18", "Different4", "NULL19", "Different5", "Different6", "NULL20", "NULL21", "NULL22",
                                  "Pearson correlation \n or regression", "Dependent t-test",
                                  "Independent t-test or \n Point-biserial correlation", "One-way repeated \n measures ANOVA",
                                  "One-way independent \n ANOVA", "Multiple regression", "Factorial repeated \n measures ANOVA",
                                  "Independent factorial \n ANOVA / multiple \n regression", "Factorial mixed ANOVA",
                                  "Multiple regression\n/ANCOVA", "Logistic regression or \n biserial/point biserial \n correlation",
                                  "Pearson chi-square or \n likelihood ratio", "Logistic regression", "Loglinear analysis", 
                                  "Logistic regression2", "MANOVA", "Factorial MANOVA", "MANCOVA", 
                                  "Bootstrap correlation/\nregression, Spearman \n correlation, Kendall's tau",
                                  "Bootstrapped t-test or \n Wilcoxon matched-pairs \n test",
                                  "Bootstrapped t-test or \n Mann-Whitney Test", "Bootstrapped ANOVA or\nFriedman's ANOVA",
                                  "Robust ANOVA or \n Kruskal-Wallis test", "Bootstrapped multiple\nregression",
                                  "Robust factorial repeated\nmeasures ANOVA", "Robust independent \n factorial ANOVA/\n multiple regression",
                                  "Robust factorial mixed\nANOVA", "Robust ANCOVA/\nbootstrapped regression"))
  graph <- igraph::graph_from_data_frame(decisionDf, directed = TRUE)
  coords <- igraph::layout_as_tree(graph)
  names <- igraph::vertex_attr(graph, "name")
  
  # data frame for nodes and box size
  boxHeight <- 0.46
  boxWidth <-  .3
  nodesDf <- data.frame(label = names, x = coords[,2], y = coords[,1], 
                        xmin = coords[,2] - boxWidth, ymin = coords[,1] - boxHeight,
                        xmax = coords[,2] + boxWidth, ymax = coords[,1] + boxHeight)
  
  # remove NULL nodes from edgesDf and decision Df
  nodesDf <- subset(nodesDf, !grepl("NULL", nodesDf$label))
  decisionDf <- subset(decisionDf, !(grepl("NULL", decisionDf$from) | grepl("NULL", decisionDf$to)))
  
  # add edges that skip rows
  extraEdges <- data.frame(from = c("Continuous3", "Continuous4", "Categorical3", "Categorical3", "Categorical3",
                                    "Both", "Continuous5", "Categorical4", "Continuous6", "Categorical5", "Both2",
                                    "Categorical6", "Categorical7", "Both3"),
                           to = c("Pearson correlation \n or regression", "Multiple regression", "Same3", "Different3",
                                  "Both4", "Multiple regression\n/ANCOVA", "Logistic regression or \n biserial/point biserial \n correlation",
                                  "Different4", "Logistic regression", "Different5", "Different6", "MANOVA", "Factorial MANOVA", "MANCOVA"))
  decisionDf <- rbind(decisionDf, extraEdges)
  
  #remove NULL nodes from edgesDf and decision Df
  nodesDf <- subset(nodesDf, !grepl("NULL", nodesDf$label))
  decisionDf <- subset(decisionDf, !(grepl("NULL", decisionDf$from) | grepl("NULL", decisionDf$to)))

  
  # add edges that skip rows
  extraEdges <- data.frame(from = c("Continuous3", "Continuous4", "Categorical3", "Categorical3", "Categorical3",
                                    "Both", "Continuous5", "Categorical4", "Continuous6", "Categorical5", "Both2",
                                    "Categorical6", "Categorical7", "Both3"),
                           to = c("Pearson correlation \n or regression", "Multiple regression", "Same3", "Different3",
                                  "Both4", "Multiple regression\n/ANCOVA", "Logistic regression or \n biserial/point biserial \n correlation",
                                  "Different4", "Logistic regression", "Different5", "Different6", "MANOVA", "Factorial MANOVA", "MANCOVA"))
  decisionDf <- rbind(decisionDf, extraEdges)
  
  # data frame for edges
  edgesDf <- cbind(decisionDf, "id" = seq_len(nrow(decisionDf))) 
  edgesDf <- tidyr::pivot_longer(edgesDf, cols = c("from", "to"),
                                 names_to = "order",
                                 values_to = "label")
  xCoord <- c()
  yCoord <- c()
  for (i in seq_len(nrow(edgesDf))) {
    yCoord <- c(yCoord, nodesDf$y[nodesDf$label == edgesDf$label[i]])
    xDirection <- edgesDf$order[i]
    newXCoord <- ifelse(xDirection == "from", nodesDf$xmin[nodesDf$label == edgesDf$label[i]], nodesDf$xmax[nodesDf$label == edgesDf$label[i]])
    xCoord <- c(xCoord, newXCoord)
  }
  edgesDf <- cbind(edgesDf, x = xCoord, y = yCoord)
  
  # set active nodes
  ## create list with all paths to parametric tests
  allTestNames <- tail(nodesDf, 28)$label
  pathList <- igraph::all_shortest_paths(graph, "One", head(allTestNames, 15))$res
  pathList <- append(pathList, igraph::all_shortest_paths(graph, "Two or more", allTestNames[c(16, 17, 18)])$res)
  pathMatchList <- list()
  
  ##transform to characters, remove test outcome, numbers and NULLs to match
  for (i in seq_along(pathList)) {
    pathList[[i]] <- igraph::as_ids(pathList[[i]])
    pathList[[i]] <- pathList[[i]][!grepl("NULL", pathList[[i]])]
    pathMatchList[[i]] <- head(pathList[[i]], -1)
    pathMatchList[[i]] <- gsub("[[:digit:]]", "", pathMatchList[[i]])
  }
  
  ## get selected options to match
  selectedOptionsVector <- .getSelectedOptions(options)
  
  ## match selected options with paths
  pathID <- "No Match"   # default, if through QML bug an impossible combination is selected
  
  for (i in seq_along(pathMatchList)) {
    if (all(pathMatchList[[i]] == selectedOptionsVector))
      pathID <- i
  }
  
  # return error as workaround for QML bug
  if (pathID == "No Match") {
    plot <- createJaspPlot(title = gettext("Decision Tree"), width = 2000, height = 1200)
    plot$setError("Not all required questions answered.")
    jaspResults[["DecisionTree"]] <- plot
    return()
  }
  
  activeNodes <- pathList[[pathID]]
  
  # switch to non-parametric alternative if necessary
  if (options[["parametricTest"]] == "nonparametric") {
    parametricTest <- tail(activeNodes, 1)
    if (parametricTest == "Pearson correlation \n or regression")
      activeNodes <- c(activeNodes, "Bootstrap correlation/\nregression, Spearman \n correlation, Kendall's tau")
    if (parametricTest == "Dependent t-test")
      activeNodes <- c(activeNodes, "Bootstrapped t-test or \n Wilcoxon matched-pairs \n test")
    if (parametricTest == "Independent t-test or \n Point-biserial correlation")
      activeNodes <- c(activeNodes, "Bootstrapped t-test or \n Mann-Whitney Test")
    if (parametricTest == "One-way repeated \n measures ANOVA")
      activeNodes <- c(activeNodes, "Bootstrapped ANOVA or\nFriedman's ANOVA")
    if (parametricTest == "One-way independent \n ANOVA")
      activeNodes <- c(activeNodes, "Robust ANOVA or \n Kruskal-Wallis test")
    if (parametricTest == "Multiple regression")
      activeNodes <- c(activeNodes, "Bootstrapped multiple\nregression")
    if (parametricTest == "Factorial repeated \n measures ANOVA")
      activeNodes <- c(activeNodes, "Robust factorial repeated\nmeasures ANOVA")
    if (parametricTest == "Independent factorial \n ANOVA / multiple \n regression")
      activeNodes <- c(activeNodes, "Robust independent \n factorial ANOVA/\n multiple regression")
    if (parametricTest == "Factorial mixed ANOVA")
      activeNodes <- c(activeNodes, "Robust factorial mixed\nANOVA")
    if (parametricTest == "Multiple regression\n/ANCOVA")
      activeNodes <- c(activeNodes, "Robust ANCOVA/\nbootstrapped regression")
  }
  
  # declare active nodes
  nodesDf$active <- FALSE
  for (i in seq_len(nrow(nodesDf))) {
    if (nodesDf[i,]$label %in% activeNodes)
      nodesDf[i,]$active <- TRUE
  }
  
  if (options[["nOutcomeVariables"]] == "nOutcomeMany")
    nodesDf$active[nodesDf$label == "Two or more"] <- TRUE
  
  # remove numbers from labels
  for (i in seq_along(nodesDf$label)) {
    nodesDf$label[i] <- gsub("[[:digit:]]", "", nodesDf$label[i])
  }
  
  
  # create Df for questions as headings
  yPos <- min(nodesDf$y) - 2
  xPos <- 7:0
  questionsDf <- data.frame(x = xPos, y = yPos,
                            label = c("How many outcome \n variables?",
                                      "What type of outcome?",
                                      "How many predictor variables?",
                                      "What type of predictor?",
                                      "How many categories \n in predictor variable?",
                                      "Same or different entities \n in categories of predictor?",
                                      "Parametric test",
                                      "Non-parametric alternative"))
  
  # Divide nodes into "answers to questions" and "test outcomes" to format them differently
  answerNodesDf <- head(nodesDf, 36)
  testNodesDf <- tail(nodesDf, 28)
  testChoice <- tail(activeNodes, 1)
  labelDf <- data.frame(x = 1, y = 1, label = testChoice)
  
  # create test outcome plot
  plot1 <- createJaspPlot(title = gettext("Test choice"), width = 450, height = 150)
  plotObject <- ggplot2::ggplot() +
    ggplot2::geom_label(data = labelDf, mapping = ggplot2::aes(x = x, y = y, label = label), size = 10, fill = "#8dc63f") +
    ggplot2::theme_void()
  plot1$plotObject <- plotObject
  
  jaspResults[["DecisionTree"]] <- createJaspContainer("")
  jaspResults[["DecisionTree"]][["Heading"]] <- plot1
  jaspResults[["DecisionTree"]][["Text"]] <- createJaspHtml("Some explanation and maybe a link to the test?", "p")
  
  # create decision tree plot
  if (options[["displayFullTree"]]) {
    plot2 <- createJaspPlot(title = gettext("Decision Tree"), width = 2300, height = 1500)
    plotObject <- ggplot2::ggplot() +
      ggplot2::scale_x_reverse() +
      ggplot2::scale_y_reverse() +
      ggplot2::geom_rect(data = nodesDf, mapping = ggplot2::aes(xmin = xmin, ymin = ymin,
                                                                xmax = xmax, ymax = ymax, fill = active), color = "black") +
      ggplot2::geom_text(data = answerNodesDf, mapping = ggplot2::aes(x = x, y = y, label = label), size = 8) +
      ggplot2::geom_text(data = testNodesDf, mapping = ggplot2::aes(x = x, y = y, label = label), size = 5) +
      ggplot2::geom_text(data = questionsDf, mapping = ggplot2::aes(x = x, y = y, label = label), size = 7, fontface = "bold") +
      ggplot2::geom_path(data = edgesDf, mapping = ggplot2::aes(x = x, y = y, group = id),
                         arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
                         size = 1) +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#8dc63f", "FALSE" = "#15a1e2")) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")
    plot2$plotObject <- plotObject
    
    jaspResults[["DecisionTree"]][["Plot"]] <- plot2
  }
  return()
}

.getSelectedOptions <- function(options) { # extra if conditions to prevent qml bug from selecting impossible options
  selectedOptionsVector <- c()
  
  #Q1: "How many outcome variables?")
  if (options[["nOutcomeVariables"]] == "nOutcomeOne")
    selectedOptionsVector[1] <- "One"
  if (options[["nOutcomeVariables"]] == "nOutcomeMany")
    selectedOptionsVector[1] <- "Two or more"
  
  #Q2: "What type of outcome?"
  if (options[["typeOutcomeVariables"]] == "typeOutcomeCont")
    selectedOptionsVector[2] <- "Continuous"
  if (options[["typeOutcomeVariables"]] == "typeOutcomeCat" & options[["nOutcomeVariables"]] == "nOutcomeOne") 
    selectedOptionsVector[2] <- "Categorical"
  
  #Q3: "How many predictor variables?"
  if (options[["nPredictor"]] == "onePredictor")
    selectedOptionsVector[3] <- "One"
  if (options[["nPredictor"]] == "twoPlusPredictors")
    selectedOptionsVector[3] <- "Two or more"
  
  #Q4: "What type of predictor?"
  if (options[["typePredictor"]] == "typePredictorCont" && options[["nOutcomeVariables"]] == "nOutcomeOne")
    selectedOptionsVector[4] <- "Continuous"
  if (options[["typePredictor"]] == "typePredictorCat")
    selectedOptionsVector[4] <- "Categorical"
  if (options[["typePredictor"]] == "typePredictorBoth" && options[["nPredictor"]] == "twoPlusPredictors")
    selectedOptionsVector[4] <- "Both"
  
  ## optional questions:
  
  #Q5: "How many categories in predictor variable?"
  if (options[["typePredictor"]] == "typePredictorCat" && options[["nPredictor"]] == "onePredictor" &&
      options[["nOutcomeVariables"]] == "nOutcomeOne" && options[["typeOutcomeVariables"]] == "typeOutcomeCont") {
    if (options[["nCatPredictor"]] == "nCatPredictorTwo")
      selectedOptionsVector <- c(selectedOptionsVector, "Two")
    if (options[["nCatPredictor"]] == "nCatPredictorTwoPlus")
      selectedOptionsVector <- c(selectedOptionsVector, "More than two")
  }
  
  #Q6: "Same or different entities in categories of predictor?"
  if ((options[["typePredictor"]] == "typePredictorCat" && options[["nOutcomeVariables"]] == "nOutcomeOne") ||
      (options[["typeOutcomeVariables"]] == "typeOutcomeCat" && options[["typePredictor"]] == "typePredictorBoth")) {
    if (options[["repCatPredictor"]] == "repCatPredictor" && options[["typeOutcomeVariables"]] == "typeOutcomeCont")
      selectedOptionsVector <- c(selectedOptionsVector, "Same")
    if (options[["repCatPredictor"]] == "mixedRepCatPredictor" && options[["nPredictor"]] == "twoPlusPredictors" &&
        options[["typeOutcomeVariables"]] == "typeOutcomeCont")
      selectedOptionsVector <- c(selectedOptionsVector, "Both")
    if (options[["repCatPredictor"]] == "noRepCatPredictor")
      selectedOptionsVector <- c(selectedOptionsVector, "Different")
  }
  
  return(selectedOptionsVector)
}


