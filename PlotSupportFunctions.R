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
    # Generate 10 highly distinct shades of blue
    color_palette <- c("#e0f3ff", "#c2e1ff", "#a3ceff", "#85bbff", "#6699ff",
                       "#4a7aff", "#2e5aff", "#143aff", "#0022cc", "#001199")  # Darker blues
    
    # Define fixed breaks in increments of 10
    steps <- ceiling(iter / 10)  # Round up to nearest integer
    breaks <- seq(0, iter, steps)
    
    # Ensure exactly 10 bins
    if (length(breaks) > 11) breaks <- breaks[1:11]
    
    # Generate labels (e.g., "0-10", "10-20", ...)
    labels <- paste(head(breaks, -1), tail(breaks, -1), sep = "-")
    
    # Assign frequency values to bins
    color_bins <- cut(frequency, breaks = breaks, include.lowest = TRUE, labels = labels)
    
    # Convert to a factor with the correct order (reversed)
    fill_color <- factor(color_bins, levels = rev(labels))  # Reverse levels
    
  } else {
    fill_color <- "lightgrey"
  }
  
  # Apply the color to the plot
  plotobj <- plotobj +
    geom_polygon(data = df,
                 aes(x, y, fill = fill_color),
                 color = "black") 
  
  return(plotobj)  

}


