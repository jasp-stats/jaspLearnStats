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
#

LSTdecisionTree <- function(jaspResults, dataset = NULL, options) {

  decisionDf <- data.frame(from = c("One", "One"), to = c("Continuous", "Categorical"))
  graph <- igraph::graph_from_data_frame(decisionDf, directed = TRUE)
  coords <- igraph::layout_as_tree(graph)
  names <- igraph::vertex_attr(graph, "name")
  
  # data frame for nodes and box properties
  nodesDf <- data.frame(x = coords[,1], y = coords[,2], label = names)
  boxHeight <- 0.15
  boxWidth <-  0.35
  
  # data frame for edges
  tidyr::pivot_longer()
  
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = nodesDf, mapping = ggplot2::aes(xmin = x - boxWidth, ymin = y - boxHeight,
                                                               xmax = x + boxWidth, ymax = y + boxHeight)) +
    ggplot2::geom_text(data = nodesDf, mapping = ggplot2::aes(x = x, y = y, label = label)) +
    ggplot2::theme_void()
  
  
}