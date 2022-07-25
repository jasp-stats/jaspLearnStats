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
                                    "One-way repeated measures \n ANOVA", "One-way independent \n ANOVA", "Multiple regression",
                                    "Factorial repeated measures \n ANOVA", "Independent factorial \n ANOVA/multiple regression",
                                    "Factorial mixed ANOVA", "Multiple regression\n/ANCOVA"),
                           to = c("Continuous", "Categorical", "Continuous2", "One2", "Two or more2", "One3", "Two or more3",
                                  "One4", "Two or more4", "Continuous3", "Categorical2", "Continuous4", "Categorical3", "Both",
                                  "Continuous5", "Categorical4", "Continuous6", "Categorical5", "Both2", "Categorical6",
                                  "Categorical7", "Both3", "NULL", "Two", "More than two", "NULL2", "NULL3", "NULL4", "NULL5",
                                  "NULL6", "NULL7", "NULL8", "NULL9", "NULL10", "NULL11", "NULL12", "NULL13", "NULL14", "NULL15",
                                  "Same", "Different", "Same2", "Different2", "NULL16", "Same3", "Different3", "Both4", "NULL17",
                                  "NULL18", "Different4", "NULL19", "Different5", "Different6", "NULL20", "NULL21", "NULL22",
                                  "Pearson correlation \n or regression", "Dependent t-test",
                                  "Independent t-test or \n Point-biserial correlation", "One-way repeated measures \n ANOVA",
                                  "One-way independent \n ANOVA", "Multiple regression", "Factorial repeated measures \n ANOVA",
                                  "Independent factorial \n ANOVA/multiple regression", "Factorial mixed ANOVA",
                                  "Multiple regression\n/ANCOVA", "Logistic regression or \n biserial/point biserial \n correlation",
                                  "Pearson chi-square or \n likelihood ratio", "Logistic regression", "Loglinear analysis", 
                                  "Logistic regression2", "MANOVA", "Factorial MANOVA", "MANCOVA", 
                                  "Bootstrap correlation/\nregression, Spearman \n correlation, Kendall's tau",
                                  "Bootstrapped t-test or \n Wilcoxon matched-pairs test",
                                  "Bootstrapped t-test or Mann-\nWhitney Test", "Bootstrapped ANOVA or\nFriedman's ANOVA",
                                  "Robust ANOVA or Kruskal-\nWallis test", "Bootstrapped multiple\nregression",
                                  "Robust factorial repeated\nmeasures ANOVA", "Robust independent factorial\n ANOVA/multiple regression",
                                  "Robust factorial mixed\nANOVA", "Robust ANCOVA/\nbootstrapped regression"))
  graph <- igraph::graph_from_data_frame(decisionDf, directed = TRUE)
  coords <- igraph::layout_as_tree(graph)
  names <- igraph::vertex_attr(graph, "name")
  
  # data frame for nodes and box size
  boxHeight <- 0.4
  boxWidth <-  .3
  nodesDf <- data.frame(label = names, x = coords[,1], y = coords[,2], 
                        xmin = coords[,1] - boxWidth, ymin = coords[,2] - boxHeight,
                        xmax = coords[,1] + boxWidth, ymax = coords[,2] + boxHeight)
  
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
  
  for(i in seq_len(nrow(edgesDf))){
    xCoord <- c(xCoord, nodesDf$x[nodesDf$label == edgesDf$label[i]])
    yDirection <- edgesDf$order[i]
    newYCoord <- ifelse(yDirection == "from", nodesDf$ymin[nodesDf$label == edgesDf$label[i]], nodesDf$ymax[nodesDf$label == edgesDf$label[i]])
    yCoord <- c(yCoord, newYCoord)
  }
  edgesDf <- cbind(edgesDf, x = xCoord, y = yCoord)
  
  
  #remove numbers from labels
  # for(i in seq_along(nodesDf$label)){
  #   nodesDf$label[i] <- gsub("[[:digit:]]", "", nodesDf$label[i])
  # }
  
  plot <- createJaspPlot(title = gettext("Decision Tree"), width = 1700, height = 1200)
  
  plotObject <- ggplot2::ggplot() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse() +
    ggplot2::geom_rect(data = nodesDf, mapping = ggplot2::aes(xmin = xmin, ymin = ymin,
                                                              xmax = xmax, ymax = ymax)) +
    ggplot2::geom_text(data = nodesDf, mapping = ggplot2::aes(x = x, y = y, label = label), size = 4) +
    ggplot2::geom_path(data = edgesDf, mapping = ggplot2::aes(x = x, y = y, group = id),
                       arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
                       size = 1) +
    ggplot2::theme_void()
  
  plot$plotObject <- plotObject
  
  jaspResults[["DecisionTree"]] <- plot
  
  return()
}
