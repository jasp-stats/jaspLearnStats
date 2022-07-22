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
                                    "Both3", "NULL"),
                           to = c("Continuous", "Categorical", "Continuous2", "One2", "Two or more2", "One3", "Two or more3",
                                  "One4", "Two or more4", "Continuous3", "Categorical2", "Continuous4", "Categorical3", "Both",
                                  "Continuous5", "Categorical4", "Continuous6", "Categorical5", "Both2", "Categorical6",
                                  "Categorical7", "Both3", "NULL", "Two", "More than two", "NULL2", "NULL3", "NULL4", "NULL5",
                                  "NULL6", "NULL7", "NULL8", "NULL9", "NULL10", "NULL11", "NULL12", "NULL13", "NULL14", "NULL15"))
  graph <- igraph::graph_from_data_frame(decisionDf, directed = TRUE)
  coords <- igraph::layout_as_tree(graph)
  names <- igraph::vertex_attr(graph, "name")
  
  # data frame for nodes and box size
  boxHeight <- 0.15
  boxWidth <-  0.45
  nodesDf <- data.frame(label = names, x = coords[,1], y = coords[,2], 
                        xmin = coords[,1] - boxWidth, ymin = coords[,2] - boxHeight,
                        xmax = coords[,1] + boxWidth, ymax = coords[,2] + boxHeight)
  
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
  
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = nodesDf, mapping = ggplot2::aes(xmin = xmin, ymin = ymin,
                                                              xmax = xmax, ymax = ymax)) +
    ggplot2::geom_text(data = nodesDf, mapping = ggplot2::aes(x = x, y = y, label = label)) +
    ggplot2::geom_path(data = edgesDf, mapping = ggplot2::aes(x = x, y = y, group = id),
                       arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
                       size = 1) +
    ggplot2::theme_void()
}
