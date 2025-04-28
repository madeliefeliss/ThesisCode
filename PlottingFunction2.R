#' Visualisation of a initial RE meta-tree with frequency coloring
#'
#' Plot function for a \code{REmrt} object. The plot shows the initial tree
#' result of \code{REmrt} with the nodes being colored based on how often they
#' appear as nodes in the amount of iterations given.
#' The plot function uses the plot method from the package \pkg{visNetwork}
#' @param x A REmrt object.
#' @param iter Amount of iterations for frequency check.
#' @param c.pruning The pruning strictness value (used in the c * SD rule in the pruning process)
#' @import visNetwork
#' @export
plot.PV.I <- function(x, iter = 10, c.pruning = 0) {
  
  # Loading visNetwork package with silent installation if missing:
  if (!requireNamespace("visNetwork", quietly = TRUE)) install.packages("visNetwork")
  
  library(visNetwork)
  
  environment(x$formula) <- environment()
  
  # Function to calculate node stability across iterations:
  compute_node_stability <- function(x, iter) {
    node_frequencies <- numeric(0)
    
    # Running the models for all iterations:
    for(i in 1:iter) {
      
      # Fitting a model with same parameters:
      new_model <- REmrt(x$formula, data = x$data, vi = x$data$`(vi)`, c.pruning = c.pruning)
      
      # Extracting node information from fitted model by computing the table:
      fnodes <- computetable(new_model)
      node_frequencies <- c(node_frequencies, fnodes$leaf)
      
    }
    
    # Converting into a dataframe:
    node_frequency_df <- as.data.frame(table(node_frequencies))
    
    # Getting the node structure table of the initial tree:
    nodes <- computetableIT(x)
    
    # Merging frequencies with the initial tree table:
    merged_nodes <- merge(nodes, node_frequency_df, 
                          by.x = "leaf", by.y = "node_frequencies", 
                          all.x = TRUE)
    
    # Giving a 0 frequency to nodes that did not appear during the iterations:
    merged_nodes$Freq[is.na(merged_nodes$Freq)] <- 0
    
    list(nodes = merged_nodes)
    
  }
  
  # Calculating the node depth:
  calculate_node_depth <- function(leaf_ids, parent_ids) {
    depths <- rep(0, length(leaf_ids))
    
    # Iteratively determine depths until no changes:
    changed <- TRUE
    while(changed) {
      changed <- FALSE
      for (i in seq_along(leaf_ids)) {
        # Root node handling:
        if (parent_ids[i] == 0) {
          new_depth <- 0
        } else {
          # Finding parent and calculating depth:
          parent_index <- which(leaf_ids == parent_ids[i])
          if (length(parent_index) == 0) next
          new_depth <- depths[parent_index] + 1
        }
        
        # Updating depth if changed:
        if (new_depth > depths[i]) {
          depths[i] <- new_depth
          changed <- TRUE
        }
      }
    }
    return(depths)
  }
  
  # Calculating the node positions for visualization:
  calculate_tree_layout <- function(nodes, l_spacing, n_spacing) {
    nodes$depth <- calculate_node_depth(nodes$leaf, nodes$pleaf)
    
    depths <- unique(nodes$depth)
    x_pos <- numeric(nrow(nodes))
    y_pos <- numeric(nrow(nodes))
    
    # Position nodes horizontally centered at each level:
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
  
  # Computing the stability:
  stability_data <- compute_node_stability(x, iter)
  nodes <- stability_data$nodes
  
  # Merging position data with node information:
  node_positions <- calculate_tree_layout(nodes, 150, 100)
  nodes <- merge(nodes, node_positions, by = "leaf")
  
  # Configure node properties for visualization:
  nodes$id <- nodes$leaf
  
  # Label nodes with split rules or leaf number:
  nodes$label <- ifelse(is.na(nodes$split), 
                        paste("K =", nodes$leaf.no), 
                        nodes$split)
  # Tooltip content:
  nodes$title <- paste0(
    "<b>Studies:</b> ", nodes$leaf.no, "<br>",
    "<b>Retention:</b> ", nodes$Freq, "/", iter, " (", round(nodes$Freq/iter*100,1), "%)"
  )
  
  # Coloring nodes based on the frequency:
  nodes$value <- nodes$Freq
  nodes$group <- ifelse(is.na(nodes$split), "Leaf", "Split")
  nodes$color.background <- ifelse(
    nodes$Freq/iter >= 0.7, "#001199", 
    ifelse(
      nodes$Freq/iter >= 0.3, "#6699ff",  
      "#e0f3ff" 
    )
  )
  nodes$color.border <- "#2B2B2B"
  
  # Coloring the text based on color background:
  nodes$font.color <- ifelse(
    nodes$Freq/iter >= 0.7, "white", 
    ifelse(
      nodes$Freq/iter >= 0.3, "white",  
      "black" 
    )
  )
  nodes$font.size <- ifelse(is.na(nodes$split), 16, 14)
  nodes$shape <- ifelse(is.na(nodes$split), "box", "ellipse")
  nodes$level <- calculate_node_depth(nodes$leaf, nodes$pleaf)
  nodes$fixed <- TRUE 
  nodes$size <- ifelse(is.na(nodes$split), 35, 30)
  
  # Creating edge properties:
  edges <- data.frame(
    from = nodes$pleaf[nodes$pleaf != 0],
    to = nodes$leaf[nodes$pleaf != 0],
    label = ifelse(nodes$leaf[nodes$pleaf != 0] %% 2 == 0, "Yes", "No"),
    font.size = 14,
    font.color = "black",
    color = "#7F7F7F",
    width = 2,
    arrows = "to",
    smooth = FALSE
  )
  
  # Creating the final visualization:
  visNetwork(nodes, edges) %>%
    
    # Adding the title:
    htmlwidgets::prependContent(
      htmltools::tags$div(
        style = "text-align: center; font-size: 12pt; font-weight: bold; margin: 0 auto;",
        "Meta-CART Node Pruning Variability Analysis"
      )
    ) %>%
    
    # Adding the subtitle:
    htmlwidgets::prependContent(
      htmltools::tags$div(
        style = "text-align: center; font-size: 10pt; margin: 10px auto 20px;",
        paste("Percentage of nodes appearing across", iter, "iterations")
      )
    ) %>%
    
    # Setting the layout:
    visHierarchicalLayout(direction = "UD",
                          levelSeparation = 150,
                          nodeSpacing = 100) %>%
    
    visNodes(size = nodes$size) %>%
    
    visEdges(arrows = "to") %>%
    
    # Creating the legend:
    htmlwidgets::prependContent(
      htmltools::tags$div(
        id = "customLegend",
        style = "
          position: absolute;
          top: 60px;
          right: 40px;
          background: white;
          padding: 8px 10px;
          border: 1px solid #ccc;
          border-radius: 6px;
          font-size: 10pt;
          box-shadow: 1px 1px 4px rgba(0,0,0,0.3);
          cursor: move;
          z-index: 1000;
          width: 160px;
          height: auto;
          transform-origin: top right;
          transition: transform 0.1s ease;
        ",
        htmltools::tags$b(style = "font-size: 11pt;", "Retention Rate"),
        htmltools::tags$div(style = "margin-top: 5px;",
                            htmltools::tags$span(style = "background: #001199; color: white; padding: 2px 4px; border-radius: 3px;", "High (≥70%)")
        ),
        htmltools::tags$div(style = "margin-top: 5px;",
                            htmltools::tags$span(style = "background: #6699ff; color: white; padding: 2px 4px; border-radius: 3px;", "Medium (30-69%)")
        ),
        htmltools::tags$div(style = "margin-top: 5px;",
                            htmltools::tags$span(style = "background: #e0f3ff; color: black; padding: 2px 4px; border-radius: 3px;", "Low (<30%)")
        )
      )
    ) %>%
    
    # Making the legend movable and resizable:
    htmlwidgets::onRender("
      function(el) {
        // Load interact.js for drag and wheel handling
        var script = document.createElement('script');
        script.src = 'https://cdn.jsdelivr.net/npm/interactjs@1.10.11/dist/interact.min.js';
        script.onload = function() {
          var legend = document.getElementById('customLegend');
          var scale = 1;
          
          // Make draggable
          interact(legend).draggable({
            onmove: function(event) {
              legend.style.top = (legend.offsetTop + event.dy) + 'px';
              legend.style.left = (legend.offsetLeft + event.dx) + 'px';
            }
          });
          
          // Handle wheel events for scaling
          legend.addEventListener('wheel', function(e) {
            e.preventDefault();
            e.stopPropagation();
            
            // Calculate new scale (min 0.5x, max 2x)
            const delta = e.deltaY > 0 ? 0.9 : 1.1;
            scale = Math.min(Math.max(0.5, scale * delta), 2);
            
            // Apply scaling transform
            legend.style.transform = `scale(${scale})`;
            
            // Adjust position to maintain top-right anchor
            const rect = legend.getBoundingClientRect();
            legend.style.right = `${window.innerWidth - rect.right}px`;
            legend.style.top = `${rect.top}px`;
          });
        };
        document.head.appendChild(script);
        
        // Prevent default wheel behavior on legend
        document.getElementById('customLegend').addEventListener('wheel', function(e) {
          e.stopPropagation();
        });
      }
    ") %>%
    
    # Adding interaction options:
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
               nodesIdSelection = FALSE) %>%
    
    visInteraction(dragNodes = FALSE,
                   dragView = TRUE,
                   zoomView = TRUE) %>%
    
    visPhysics(hierarchicalRepulsion = list(nodeDistance = 100 * 2)) 
  
  }

