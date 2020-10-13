library(igraph)
library(networkD3)
library(visNetwork)


# igraph ------------------------------------------------------------------
edge_df <- data.frame(SourceName=c('a','b','c','d','d'), TargetName=c('u1','u1','u1','u2','u3'))

network <- graph_from_data_frame(edge_df, directed = TRUE)

deg = degree(network, mode = "in")

plot(network)

plot(network, vertex.size=deg*10)

p <- simpleNetwork(edge_df, height="100px", width="100px")

p



# networkD3 ---------------------------------------------------------------
edge_df <- data.frame(SourceName=c('a','b','c','d','d'), TargetName=c('u1','u1','u1','u2','u3'))

gD <- igraph::simplify(igraph::graph.data.frame(edge_df, directed=TRUE))

nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)),
                       nName = igraph::V(gD)$name)


getNodeID <- function(x){
  which(x == igraph::V(gD)$name) - 1 
}

edgeList <- plyr::ddply(edge_df, .variables = c("SourceName", "TargetName"), 
                        function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                TargetID = getNodeID(x$TargetName)))


# Calculate degree for all nodes
nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all")*100)

# Calculate betweenness for all nodes
betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = TRUE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) 
# We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
# rm(betAll, betAll.norm)

#Calculate Dice similarities between all pairs of nodes
dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")

F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "SourceID", "TargetID"), 
                        function(x) data.frame(F1(x)))

# rm(dsAll, F1, getNodeID, gD)


# F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(edgeList), space = "rgb", interpolate = "linear")
# colCodes <- F2(length(unique(edgeList$diceSim)))
# edges_col <- sapply(edgeList$diceSim, function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])
# 
# rm(colCodes, F2)


D3_network_LM <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                                         Nodes = nodeList, # data frame that contains info about nodes
                                         Source = "SourceID", # ID of source node 
                                         Target = "TargetID", # ID of target node
                                         NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                         Nodesize = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for a node size
                                         Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
                                         height = 500, # Size of the plot (vertical)
                                         width = 1000,  # Size of the plot (horizontal)
                                         fontSize = 20, # Font size
                                         linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                         linkWidth = networkD3::JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                         opacity = 0.85, # opacity
                                         zoom = TRUE, # ability to zoom when click on the node
                                         opacityNoHover = 0.1) # opacity of labels when static


# visNetwork --------------------------------------------------------------
# Main Reference: https://datastorm-open.github.io/visNetwork/edges.html
# with Clustering: https://www.statworx.com/de/blog/interactive-network-visualization-with-r/
# More comprehensive: https://cran.rstudio.com/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html
edge_df <- data.frame(from=c('a','b','c','d','d'), to=c('u1','u1','u1','u2','u3'))

label_df <- edge_df %>% 
  pivot_longer(everything(), names_to = "group", values_to = "id") %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(group) %>% 
  mutate(label = paste(group, row_number(), sep = "_"))

graph_obj <- igraph::simplify(igraph::graph.data.frame(edge_df, directed=TRUE))

deg = igraph::degree(graph_obj, mode = "all")
deg_df = data.frame(id = names(deg), degree = as.numeric(deg))

node_df <- data.frame(id = igraph::V(graph_obj)$name) %>%
  left_join(deg_df, by = "id") %>%
  left_join(label_df, by = "id") %>% 
  mutate(
    value = if_else(group == "term", 1, degree),
    title = id
  ) %>%
  select(-degree)

visNetwork(node_df, edge_df)



# Usage -------------------------------------------------------------------
# ```{r, eval=FALSE, include=FALSE}
# # edge_df <- keymatch_data_unique %>% 
# #   dplyr::select(from = term, to = url)
# # 
# # label_df <- edge_df %>% 
# #   pivot_longer(everything(), names_to = "group", values_to = "id") %>% 
# #   distinct(id, .keep_all = TRUE) %>% 
# #   group_by(group) %>% 
# #   mutate(label = paste(group, row_number(), sep = "_"))
# # 
# # graph_obj <- igraph::simplify(igraph::graph.data.frame(edge_df, directed=TRUE))
# # 
# # deg = igraph::degree(graph_obj, mode = "all")
# # deg_df = data.frame(id = names(deg), degree = as.numeric(deg))
# # 
# # node_df <- data.frame(id = igraph::V(graph_obj)$name) %>%
# #   left_join(deg_df, by = "id") %>%
# #   left_join(label_df, by = "id") %>% 
# #   mutate(
# #     value = if_else(group == "from", 10, degree*10), 
# #     title = id 
# #   ) %>%
# #   select(-degree)
# # 
# # # visNetwork(node_df, edge_df)
# # 
# # keymatch_visNet_obj <- visNetwork(node_df, edge_df)
# # 
# # visSave(keymatch_visNet_obj, "keymatch_visNet.html")
# ```

