#' A function to update node
#'
#' @param nodes a data frame
#' @param newNode a new row
#' @param name a string
#' @return a data.frame updated the nodes
#' @keywords internal
updateNodes <- function(nodes, newNode, name = "leaf") {
  rows <- nodes[name] == newNode[1, name]
  nodes <- rbind(nodes[!rows, ], newNode)
  nodes
}

#' A function to compute table
#'
#' @param x a data frame
#' @return a data.frame with the nodes of the current tree
#' @keywords internal
# Function to build the tree and track node frequencies
computetable <- function(x) {
  
  y <- NULL
  term.node <- NULL
  yi <- NULL
  leaf.no <- NULL
  
  tree <- x$tree
  tree <- tree[!is.na(tree$pleaf), ]
  # first, grow the tree from the root node with order and
  # find out the order of terminal nodes
  count <- 1
  nodes <- data.frame(leaf=1, pleaf=0, x=0, y=0, w=1)
  
  for(pleaf in tree$pleaf) {
    pleaf_row <- nodes[nodes$leaf == pleaf, ]
    
    # Add left child.
    count <- count + 1
    # reduce the width in half, in order to calculate the
    # terminal X-axis values
    nodes <- updateNodes(
      nodes,
      data.frame(
        leaf = count,
        pleaf = pleaf,
        x = pleaf_row$x - pleaf_row$w / 2,
        y = pleaf_row$y - 1,
        w = pleaf_row$w / 2
      )
    )
    # Add right child.
    count <- count + 1
    nodes <- updateNodes(
      nodes,
      data.frame(
        leaf = count,
        pleaf = pleaf,
        x = pleaf_row$x + pleaf_row$w / 2,
        y = pleaf_row$y - 1,
        w = pleaf_row$w / 2
      )
    )
  }
  # Add split conditions
  nodes$split = NA
  nodes$split[tree$pleaf] = as.character(tree$split)
  
  # Second, find the new x.coordinates
  nodes$x.new <- rep(NA, nrow(nodes))
  # fix the x.coord for terminal nodes
  inx.term <- !(nodes$leaf %in% nodes$pleaf)
  nodes.term <- nodes[inx.term,]
  nodes$x.new[inx.term] <- rank(nodes$x[inx.term])
  nodes$leaf.no[inx.term] <- x$n[as.character(nodes$leaf[inx.term])]
  # fix the x.coord for parent nodes
  for (i in min(nodes$y):-1){
    inx.pleaf <- which(nodes$y == i)
    coords <- sapply(split(nodes$x.new[inx.pleaf], nodes$pleaf[inx.pleaf]), mean)
    leaf.no <- sapply(split(nodes$leaf.no[inx.pleaf], nodes$pleaf[inx.pleaf]), sum)
    inx.replace <- names(coords[!is.na(coords)])
    nodes$x.new[as.numeric(inx.replace)] <- coords[inx.replace]
    nodes$leaf.no[as.numeric(inx.replace)] <- leaf.no[inx.replace]
  }
  # replace the x.coord
  nodes$x <- nodes$x.new
  
  return(nodes)
}

#' A function to compute table for initial tree
#'
#' @param x a data frame
#' @return a data.frame with all the initial tree nodes
#' @keywords internal
# Function to build the tree and track node frequencies
computetableIT <- function(x){
  
  x <- x$initial.tree
  
  #Rounding change:
  rows<-grep("<", x$split)
  for (i in rows) {
    x$split[i]<-paste0(substr(x$split[i], 1, 
                              unlist(gregexpr('<', x$split[i]))+1), 
                       " " ,round(as.numeric(substr(x$split[i], 
                                                    unlist(gregexpr('<', x$split[i]))+2, 
                                                    nchar(x$split[i]))),2))
  }
  
  y <- NULL
  term.node <- NULL
  yi <- NULL
  leaf.no <- NULL
  
  tree <- data.frame(Qb=x$Qb, tau2=x$tau2, split=x$split, mod=x$mod, pleaf=x$pleaf)
  tree <- tree[!is.na(tree$pleaf), ]
  # first, grow the tree from the root node with order and
  # find out the order of terminal nodes
  count <- 1
  nodes <- data.frame(leaf=1, pleaf=0, x=0, y=0, w=1)
  
  for(pleaf in tree$pleaf) {
    pleaf_row <- nodes[nodes$leaf == pleaf, ]
    
    # Add left child.
    count <- count + 1
    # reduce the width in half, in order to calculate the
    # terminal X-axis values
    nodes <- updateNodes(
      nodes,
      data.frame(
        leaf = count,
        pleaf = pleaf,
        x = pleaf_row$x - pleaf_row$w / 2,
        y = pleaf_row$y - 1,
        w = pleaf_row$w / 2
      )
    )
    # Add right child.
    count <- count + 1
    nodes <- updateNodes(
      nodes,
      data.frame(
        leaf = count,
        pleaf = pleaf,
        x = pleaf_row$x + pleaf_row$w / 2,
        y = pleaf_row$y - 1,
        w = pleaf_row$w / 2
      )
    )
  }
  # Add split conditions
  nodes$split = NA
  nodes$split[tree$pleaf] = as.character(tree$split)
  
  # Second, find the new x.coordinates
  nodes$x.new <- rep(NA, nrow(nodes))
  # fix the x.coord for terminal nodes
  inx.term <- !(nodes$leaf %in% nodes$pleaf)
  nodes.term <- nodes[inx.term,]
  nodes$x.new[inx.term] <- rank(nodes$x[inx.term])
  names(x$initial)<-as.character(nodes$leaf[inx.term])
  nodes$leaf.no[inx.term] <- x$initial#[as.character(nodes$leaf[inx.term])]
  # fix the x.coord for parent nodes
  for (i in min(nodes$y):-1){
    inx.pleaf <- which(nodes$y == i)
    coords <- sapply(split(nodes$x.new[inx.pleaf], nodes$pleaf[inx.pleaf]), mean)
    leaf.no <- sapply(split(nodes$leaf.no[inx.pleaf], nodes$pleaf[inx.pleaf]), sum)
    inx.replace <- names(coords[!is.na(coords)])
    nodes$x.new[as.numeric(inx.replace)] <- coords[inx.replace]
    nodes$leaf.no[as.numeric(inx.replace)] <- leaf.no[inx.replace]
  }
  # replace the x.coord
  nodes$x <- nodes$x.new
  
  return(nodes)
  
}

#' A function to deal with symbols
#'
#' @param input a string
#' @return converted string
#' @keywords internal
encodeHtml <- function(input) {
  # reference: https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
  dict <- data.frame(
    c('&', '\u0026'),
    c('<=', '\u2264'),
    c('>=', '\u2265'),
    c('<', '\u003C'),
    c('>', '\u003E')
  )
  tmp <- input
  for (i in 1:ncol(dict)) {
    tmp <- gsub(dict[1, i], dict[2, i], tmp)
    Encoding(tmp) <- "UTF-8"
  }
  tmp
}

#' A function to draw an oval
#'
#' @param plotobj the obj to be plot
#' @param x x
#' @param y y
#' @param c c
#' @param x.scale x.scale
#' @param y.scale y.scale
#' @param frequency frequency of nodes
#' @param iter i
#' @return a ggplot object
#' @importFrom ggplot2 geom_polygon aes
#' @keywords internal
oval_draw <- function(plotobj, x, y, c, x.scale = 1, y.scale = 1, 
                      frequency = NULL, iter, ...){
  
  t <- seq(-1 * pi, 1 * pi, length = 100)
  df <- data.frame(
    x = x.scale * sin(t) + x,
    y = y.scale * cos(t) / c + y
  )
  
  if (!is.null(frequency)) {
    
    # Defining the fixed breaks in increments of 5
    # & rounding up to nearest integer:
    steps <- ceiling(iter / 5)  
    breaks <- seq(0, iter, by = steps)
    
    # Creating non-overlapping labels:
    if (steps == 1) {
      
      # If steps are 1, just use single numbers:
      labels <- as.character(breaks[1:(length(breaks)-1)])
      labels[length(breaks)-1] <- iter
      
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
    
    # Generating 5 distinct colors:
    color_palette <- c("#ffffcc", "#ffd27f", "#a6c8ff", "#4a90e2", "#003366")
    
    # Assigning the frequency values to bins:
    color_bins <- cut(frequency, breaks = breaks, include.lowest = TRUE, labels = labels, right = FALSE)
    fill_color <- factor(color_bins, levels = rev(labels))  
    
    } else {
    
      fill_color <- "lightgrey"
  
    }
    
  
  # Applying the color to the plot:
  plotobj <- plotobj +
    geom_polygon(data = df,
                 aes(x, y, fill = fill_color),
                 color = "black") 
  
  return(plotobj)  
  
}

#' Visualization of a initial RE meta-tree with frequency coloring
#'
#' Plot function for a \code{REmrt} object. The plot shows the initial tree
#' result of \code{REmrt} with the nodes being colored based on how often they
#' appear as nodes in the amount of iterations given.
#' The plot function uses the plot method from the package \pkg{ggplot2}
#'
#' For categorical variables we recommend to use short names for levels to 
#' avoid overlapping labels at split points.
#' @method plot REmrt
#' @param x A REmrt object.
#' @param iter Amount of iterations for frequency check.
#' @param c.pruning The pruning strictness value (used in the c * SD rule in the pruning process)
#' @import ggplot2
#' @import gridExtra
#' @export
plot.PV <- function(x, iter = 10, c.pruning = 0){
  
  test_model <- x
  environment(test_model$formula) <- environment()
  n_iterations <- iter
  node_frequencies <- numeric(0)
  
  # Looping for the multiple iterations:
  for (i in 1:n_iterations) {
    
    # Training a new model using the same formula, vi and data:
    new_model <- REmrt(test_model$formula, data = test_model$data, vi = test_model$data$`(vi)`, c.pruning = c.pruning)
    
    # Building the tree and tracking nodes:
    fnodes <- computetable(new_model)
    
    # Extracting all nodes (split and terminal nodes) from the tree:
    all_nodes <- fnodes$leaf  
    
    # Updating the frequency of the nodes:
    node_frequencies <- c(node_frequencies, all_nodes)
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
  
  # Generating 5 distinct colors:
  color_palette <- c("#ffffcc", "#ffd27f", "#a6c8ff", "#4a90e2", "#003366")
  
  # Applying the color to the plot:
  vis <- vis + scale_fill_manual(
    values = setNames(rev(color_palette), rev(labels)),  
    name = "Node Frequency",
    breaks = labels,
    guide = guide_legend(reverse = T)) + 
    transparent_theme +
    labs(
      title = "Meta-CART Node Pruning Variability Analysis",
      subtitle = paste("Frequency of node appearances across", iter, "iterations")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    )
  
  print(vis)
  
}


