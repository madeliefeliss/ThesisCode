#' Visualisation of a RE meta-tree
#'
#' Plot function for a \code{REmrt} object. The plot shows the result of \code{REmrt}.
#' The plot function uses the plot method from the package \pkg{ggplot2}
#'
#' For categorical variables we recommend to use short names for levels to 
#' avoid overlapping labels at split points.
#' @method plot REmrt
#' @param x A REmrt object.
#' @param iter Amount of runs for frequency
#' @import ggplot2
#' @import gridExtra
#' @export
plot.PV <- function(x, iter = 10){
  
  test_model <- x
  environment(test_model$formula) <- environment()
  n_iterations <- iter
  node_frequencies <- numeric(0)
  
  # Loop for multiple iterations
  for (i in 1:n_iterations) {
    
    # Train the new model using the same formula and parameters
    new_model <- REmrt(test_model$formula, data = test_model$data, vi = test_model$data$`(vi)`, c.pruning = 0)
    
    # Build the tree and track nodes
    fnodes <- computetable(new_model)
    
    # Extract all nodes (split and leaf nodes) from the tree
    all_nodes <- fnodes$leaf  # Assuming leaf column stores all the node identifiers
    
    # Update the frequency of the visited nodes
    node_frequencies <- c(node_frequencies, all_nodes)
  }
  
  # Convert the frequency vector into a data frame for easy viewing
  node_frequency_df <- as.data.frame(table(node_frequencies))
  
  nodes <- computetableIT(x)
  
  merged_nodes <- merge(nodes, node_frequency_df, 
                        by.x = "leaf", by.y = "node_frequencies", 
                        all.x = TRUE)
  
  # Replace NAs in 'Freq' column with 0 ONLY for missing nodes
  merged_nodes$Freq[is.na(merged_nodes$Freq)] <- 0
  
  nodes <- merged_nodes
  
  # Using the transparent theme of ggplot2:
  transparent_theme <- ggplot2::theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # legend.position="none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )
  
  transparent_theme2 <- ggplot2::theme(
    #axis.line = element_blank(),
    axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # legend.position="none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )

  config.leaf_width_scale <- 0.9
  
  # find the good size for the ovals representing nodes
  # half of the minimun distance between adjacent node centroids
  x.scale <- config.leaf_width_scale / 2 *
    min(sapply(split(nodes[-1, ]$x,f =nodes[-1, ]$y), function(x) min(diff(sort(x)))))
  y.scale <- x.scale*diff(range(nodes$y))/diff(range(nodes$x))
  
  # Build the plot
  vis <- ggplot()
  
  # Add lines first
  for(i in 1:nrow(nodes)){
    node <- nodes[i, ]
    # skip root
    if(node$pleaf == 0){
      next
    }
    parent = nodes[nodes$leaf == node$pleaf, ]
    data_line = data.frame(x = c(node$x, parent$x),
                           y = c(node$y, parent$y))
    vis <- vis + geom_line(data = data_line, aes(x, y), color = "black")
  }
  
  config.branch_text_left_dx = -0.2
  config.branch_text_right_dx = 0.2
  config.branch_text_left = "Yes"
  config.branch_text_right = "No"
  config.branch_text_size = 3
  
  config.leaf_oval_ratio = 1.3
  config.leaf_text_size = 5
  
  config.split_text_dy = -0.33
  config.split_text_size = 3
  config.split_label = T
  
  # TODO: customize the leaf text
  for (i in 1:nrow(nodes)) {
    node <- nodes[i, ]
    parent = nodes[nodes$leaf == node$pleaf,]
    text_color <- ifelse(node$Freq > iter / 2, "white", "black")
    
    # Add nodes
    vis <- oval_draw(vis, node$x, node$y, config.leaf_oval_ratio, x.scale, 
                     y.scale, frequency = node$Freq, iter) + 
      geom_text(
        data = data.frame(x = node$x, y = node$y),
        aes(x, y),
        label = paste("K =", node$leaf.no),
        size = config.leaf_text_size,
        color = text_color  # Dynamically set text color
      )
    
    # The height difference between two levels is used as a baseline
    # TODO: customizable
    h = 1
    
    # Add split conditions in case it's a splitting node
    if(!is.na(node$split)){
      dy <- h * config.split_text_dy
      data_text = data.frame(x = node$x, y = node$y + dy)
      show_text = ifelse(config.split_label, geom_label, geom_text)
      vis <- vis +
        show_text(
          data = data_text,
          aes(x, y),
          label = encodeHtml(node$split),
          size = config.split_text_size
        )
    }
    
    
    # Add `yes/no`` text in branch lines
    # Calculate offset of text, to avoid overlapping lines of branch.
    dx = h * ifelse(node$leaf %% 2 == 0,
                    config.branch_text_left_dx,
                    config.branch_text_right_dx)
    data_text = data.frame(x = (node$x + parent$x) / 2 + dx,
                           y = (node$y + parent$y) / 2)
    vis <- vis +
      geom_text(
        data = data_text,
        aes(x, y),
        label = ifelse(
          node$leaf %% 2 == 0,
          config.branch_text_left,
          config.branch_text_right
        ),
        size = config.branch_text_size
      )
  }
  
  color_palette <- c("#e0f3ff", "#c2e1ff", "#a3ceff", "#85bbff", "#6699ff",
                     "#4a7aff", "#2e5aff", "#143aff", "#0022cc", "#001199")  # Darker blues
  
  steps <- ceiling(iter / 10)  # Round up to nearest integer
  breaks <- seq(0, iter, steps)
  
  # Generate labels (e.g., "0-10", "10-20", ...)
  labels <- paste(head(breaks, -1), tail(breaks, -1), sep = "-")

  # Apply the color to the plot
  vis <- vis + scale_fill_manual(
    values = setNames(rev(color_palette), rev(labels)),  # Assign reversed colors to reversed labels
    name = "Node Frequency",
    guide = guide_legend(reverse = F)  # Reverse legend order
  )

  
  vis <- vis + transparent_theme

  print(nodes)
  print( vis)
  
}
