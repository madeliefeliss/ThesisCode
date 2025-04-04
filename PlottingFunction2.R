#' Hierarchical Tree Visualization of Pruning Variability (Final Version)
#' 
#' @param x REmrt object from metacart
#' @param iter Number of bootstrap iterations (default 10)
#' @param level_spacing Vertical spacing between levels (default 150)
#' @param node_spacing Horizontal spacing between nodes (default 100)
#' @export
plot_pruning_tree <- function(x, iter = 10, level_spacing = 150, node_spacing = 100) {
  
  # Load required package
  if (!requireNamespace("visNetwork", quietly = TRUE)) install.packages("visNetwork")
  library(visNetwork)
  environment(x$formula) <- environment()
  
  # Compute node stability
  compute_node_stability <- function(x, iter) {
    node_frequencies <- numeric(0)
    
    for(i in 1:iter) {
      new_model <- REmrt(x$formula, data = x$data, vi = x$data$`(vi)`, c.pruning = 0)
      fnodes <- computetable(new_model)
      node_frequencies <- c(node_frequencies, fnodes$leaf)
    }
    
    node_frequency_df <- as.data.frame(table(node_frequencies))
    nodes <- computetableIT(x)
    
    merged_nodes <- merge(nodes, node_frequency_df, 
                          by.x = "leaf", by.y = "node_frequencies", 
                          all.x = TRUE)
    merged_nodes$Freq[is.na(merged_nodes$Freq)] <- 0
    
    list(nodes = merged_nodes)
  }
  
  # Calculate hierarchical node depth
  calculate_node_depth <- function(leaf_ids, parent_ids) {
    depths <- rep(0, length(leaf_ids))
    
    changed <- TRUE
    while(changed) {
      changed <- FALSE
      for (i in seq_along(leaf_ids)) {
        if (parent_ids[i] == 0) {
          new_depth <- 0
        } else {
          parent_index <- which(leaf_ids == parent_ids[i])
          if (length(parent_index) == 0) next
          new_depth <- depths[parent_index] + 1
        }
        
        if (new_depth > depths[i]) {
          depths[i] <- new_depth
          changed <- TRUE
        }
      }
    }
    return(depths)
  }
  
  # Calculate tree layout with proper spacing
  calculate_tree_layout <- function(nodes, l_spacing, n_spacing) {
    nodes$depth <- calculate_node_depth(nodes$leaf, nodes$pleaf)
    
    depths <- unique(nodes$depth)
    x_pos <- numeric(nrow(nodes))
    y_pos <- numeric(nrow(nodes))
    
    for (d in depths) {
      idx <- which(nodes$depth == d)
      n_nodes <- length(idx)
      x_pos[idx] <- seq(from = -(n_nodes - 1)/2 * n_spacing, 
                        to = (n_nodes - 1)/2 * n_spacing, 
                        length.out = n_nodes)
      y_pos[idx] <- -d * l_spacing
    }
    
    data.frame(leaf = nodes$leaf, x_pos = x_pos, y_pos = y_pos)
  }
  
  # Main computation
  stability_data <- compute_node_stability(x, iter)
  nodes <- stability_data$nodes
  
  # Calculate positions with proper spacing
  node_positions <- calculate_tree_layout(nodes, level_spacing, node_spacing)
  nodes <- merge(nodes, node_positions, by = "leaf")
  
  # Prepare nodes data 
  nodes$id <- nodes$leaf
  nodes$label <- ifelse(is.na(nodes$split), 
                        paste("K =", nodes$leaf.no), 
                        nodes$split)  # Show split rules in nodes
  nodes$title <- paste0(
    "<b>", ifelse(is.na(nodes$split), "Leaf Node", "Split Node"), "</b><br>",
    "<b>Studies:</b> ", nodes$leaf.no, "<br>",
    "<b>Retention:</b> ", nodes$Freq, "/", iter, " (", round(nodes$Freq/iter*100,1), "%)"
  )
  nodes$value <- nodes$Freq
  nodes$group <- ifelse(is.na(nodes$split), "Leaf", "Split")
  nodes$color.background <- ifelse(
    nodes$Freq/iter >= 0.7, "#66C2A5",
    ifelse(
      nodes$Freq/iter >= 0.3, "#FDB462",
      "#FC8D62"
    )
  )
  nodes$color.border <- "#2B2B2B"
  nodes$font.color <- "black"
  nodes$font.size <- ifelse(is.na(nodes$split), 16, 14)  # Larger font
  nodes$shape <- ifelse(is.na(nodes$split), "ellipse", "box")
  nodes$level <- calculate_node_depth(nodes$leaf, nodes$pleaf)
  nodes$fixed <- TRUE
  nodes$size <- ifelse(is.na(nodes$split), 35, 30)  # Larger nodes
  
  # Prepare edges with black "Yes/No" labels
  edges <- data.frame(
    from = nodes$pleaf[nodes$pleaf != 0],
    to = nodes$leaf[nodes$pleaf != 0],
    label = ifelse(nodes$leaf[nodes$pleaf != 0] %% 2 == 0, "Yes", "No"),
    font.size = 14,  # Larger edge labels
    font.color = "black",  # Black edge labels
    color = "#7F7F7F",
    width = 2,
    arrows = "to",
    smooth = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Create visualization 
  visNetwork(nodes, edges) %>%
    visHierarchicalLayout(direction = "UD",
                          levelSeparation = level_spacing,
                          nodeSpacing = node_spacing) %>%
    visNodes(size = nodes$size) %>%
    visEdges(arrows = "to") %>%
    visLegend(position = "right",
              addNodes = list(
                list(label = "Retention Rate", shape = "text", 
                     font = list(size = 16, align = "center")),
                list(label = "High (≥70%)", shape = "ellipse", color = "#66C2A5"),
                list(label = "Medium (30-69%)", shape = "ellipse", color = "#FDB462"),
                list(label = "Low (<30%)", shape = "ellipse", color = "#FC8D62")
              ),
              useGroups = FALSE,
              ncol = 1) %>%  # Force single column layout
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
               nodesIdSelection = FALSE) %>%
    visInteraction(dragNodes = FALSE,
                   dragView = FALSE,
                   zoomView = TRUE) %>%
    visPhysics(hierarchicalRepulsion = list(nodeDistance = node_spacing * 2))
}
