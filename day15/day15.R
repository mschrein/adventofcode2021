# load package
library(igraph)

# Part 1 ------------------------------------------------------------------

# read in data
risks <- read.table("day15/data_day15.txt", colClasses = "character")
risks <- t(apply(risks, 1, function(x) unlist(strsplit(x, ""))))
mode(risks) <- "numeric"

create_graph <- function(dat, start = c(1,1), end = c(nrow(dat), ncol(dat))){
  require(igraph)
  # create edges
  edges <- data.frame(node1 = c(which(row(dat) < end[1]), which(col(dat) < end[2])),
                      node2 = c(which(row(dat) > start[1]), which(col(dat) > start[2])))
  edges_rev <- edges[,c(2,1)]
  names(edges_rev) <- c("node1", "node2")
  edges <- rbind(edges, edges_rev)
  edges$weight <- dat[edges$node2] # assign risk values as weights for edges
  graph <- graph_from_data_frame(edges) # create graph
  return(graph)
}

graph <- create_graph(risks) # create graph

distances(graph, v = 1, to = vcount(graph), mode = "out") # get shortest path given weighting (i.e., lowest total risk)


# Part 2 ------------------------------------------------------------------
# add new tiles columnwise
new_tile <- risks
tilemap <- risks
for(i in 1:4){
  new_tile <- new_tile + 1
  new_tile[new_tile > 9] <- 1
  tilemap <- cbind(tilemap, new_tile)
}
# add new tiles rowwise
new_row <- tilemap
for(i in 1:4){
  new_row <- new_row + 1
  new_row[new_row > 9] <- 1
  tilemap <- rbind(tilemap, new_row)
}

graph2 <- create_graph(tilemap)

distances(graph2, v = 1, to = vcount(graph2), mode = "out")