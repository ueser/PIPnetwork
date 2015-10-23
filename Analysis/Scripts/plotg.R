suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(network))
suppressPackageStartupMessages(library(sna))
suppressPackageStartupMessages(library(ergm))
library(network)
library(ggplot2)
require(sna)
library(ergm)

plotg <- function(net, value = NULL) {
  m <- as.matrix.network.adjacency(net)  # get sociomatrix
  # get coordinates from Fruchterman and Reingold's force-directed placement
  # algorithm.
  plotcord <- data.frame(gplot.layout.fruchtermanreingold(m, NULL))
  # or get it them from Kamada-Kawai's algorithm: 
#   plotcord <-data.frame(gplot.layout.kamadakawai(m, NULL))
  colnames(plotcord) = c("X1", "X2")
  edglist <- as.matrix.network.edgelist(net)
  edges <- data.frame(plotcord[edglist[, 1], ], plotcord[edglist[, 2], ])
  plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))
  plotcord$groups <- as.factor(get.vertex.attribute(net, "groups"))
  colnames(edges) <- c("X1", "Y1", "X2", "Y2")
  edges$midX <- (edges$X1 + edges$X2)/2
  edges$midY <- (edges$Y1 + edges$Y2)/2
  pnet <- ggplot() + 
  geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2), data = edges, size = .5, colour = "grey") + 
  geom_point(aes(X1, X2,size=20, colour = groups, alpha=.5), data = plotcord) + 
  geom_text(data=plotcord,aes(x=X1,y=X2,label=elements)) +
#   scale_colour_manual(groups=c("1"="red","2"="lightblue","3"="darkgreen","4"="orange")) + 
  scale_x_continuous(expand=c(0,1)) + 
  scale_y_continuous(expand=c(0,1)) +
  theme_bw()+  # use the ggplot black and white theme
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # remove x-axis text
    axis.text.y = element_blank(), # remove y-axis text
    axis.ticks = element_blank(),  # remove axis ticks
    axis.title.x = element_blank(), # remove x-axis labels
    axis.title.y = element_blank(), # remove y-axis labels
    panel.background = element_blank(), 
    panel.border =element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())
  return(print(pnet))
}