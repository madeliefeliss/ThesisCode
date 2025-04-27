#' Visualisation of a initial RE meta-tree with frequency coloring
#'
#' Plot function for a \code{REmrt} object. The plot shows the initial tree
#' result of \code{REmrt} with the nodes being colored based on how often they
#' appear in the amount of iterations given.
#' The plot function uses the plot method from the package \pkg{ggplot2}
#'
#' For categorical variables we recommend to use short names for levels to 
#' avoid overlapping labels at split points.
#' @method plot REmrt
#' @param x A REmrt object.
#' @param iter Amount of iterations for frequency check.
#' @import ggplot2
#' @import gridExtra
#' @export
plot.PV.TN <- function(x, iter = 10){
  
  test_model <- x
  environment(test_model$formula) <- environment()
  n_iterations <- iter
  node_frequencies <- numeric(0)
  
  # Looping for the multiple iterations:
  for (i in 1:n_iterations) {
    
    # Training a new model using the same formula, vi and data:
    new_model <- REmrt(test_model$formula, data = test_model$data, vi = test_model$data$`(vi)`, c.pruning = 0)
    
    # Building the tree and tracking nodes:
    fnodes <- computetable(new_model)
    
    # Extracting terminal nodes from the tree:
    inx.term <- !(fnodes$leaf %in% fnodes$pleaf)
    terminal_nodes <- fnodes$leaf[inx.term]
    
    # Updating the frequency of the nodes:
    node_frequencies <- c(node_frequencies, terminal_nodes)
  }
  
  # Converting the frequency vector into a data frame:
  node_frequency_df <- as.data.frame(table(node_frequencies))
  
  # Computing the full intitial tree table:
  nodes <- computetableIT(x)
  
  # Merging the frequency column with the initial tree table:
  merged_nodes <- merge(nodes, node_frequency_df, 
                        by.x = "leaf", by.y = "node_frequencies", 
                        all.x = TRUE)
  
  # Replace NAs in the frequency column with 0 for nodes without frequency:
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
  
  # Find the good size for the ovals representing nodes
  # half of the minimun distance between adjacent node centroids:
  x.scale <- config.leaf_width_scale / 2 *
    min(sapply(split(nodes[-1, ]$x,f =nodes[-1, ]$y), function(x) min(diff(sort(x)))))
  y.scale <- x.scale*diff(range(nodes$y))/diff(range(nodes$x))
  
  # Building the plot:
  vis <- ggplot()
  
  # Adding lines first:
  for(i in 1:nrow(nodes)){
    node <- nodes[i, ]
    # Skipping root:
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
  config.leaf_text_size = 3
  
  config.split_text_dy = -0.33
  config.split_text_size = 3
  config.split_label = T
  
  # Customizing the leaf text:
  for (i in 1:nrow(nodes)) {
    node <- nodes[i, ]
    parent = nodes[nodes$leaf == node$pleaf,]
    text_color <- ifelse(node$Freq > iter / 2, "white", "black")
    
    # Adding nodes:
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
    
    h = 1
    
    # Adding split conditions in case it's a splitting node:
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
    
    
    # Adding `yes/no`` text in branch lines:
    
    # Calculating offset of text, to avoid overlapping lines of branch:
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
  
  # Defining the fixed breaks in increments of 5
  # & rounding up to nearest integer:
  steps <- ceiling(iter / 5)  
  breaks <- seq(0, iter, by = steps)
  
  # Creating non-overlapping labels:
  if (steps == 1) {
    
    # If steps are 1, just use single numbers:
    labels <- as.character(breaks[1:(length(breaks)-1)])
    labels[length(breaks)-1] <- iter
    print(labels)
    
  } else{
    
    # Otherwise, create range labels:
    labels <- vector("character", length(breaks) - 1)
    
    for(i in 1:(length(breaks)-1)) {
      if(i == 1){
        labels[i] <- paste0(breaks[i], "-", breaks[i+1])
      } else{
        labels[i] <- paste0(breaks[i] + 1, "-", breaks[i+1])
        
      }
    }
    
  }
  
  # Generating 5 distinct shades of blue:
  color_palette <- colorRampPalette(c("#e0f3ff", "#001199"))(length(labels))
  
  
  # Applying the color to the plot:
  vis <- vis + scale_fill_manual(
    values = setNames(rev(color_palette), rev(labels)),  
    name = "Terminal Node \nFrequency",
    breaks = labels,
    guide = guide_legend(reverse = T)) + 
    transparent_theme +
    labs(
      title = "Meta-CART Terminal Node Pruning Variability Analysis",
      subtitle = paste("Frequency of terminal node appearances across", iter, "iterations")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    )
  
  print(nodes)
  print(vis)
  
}
