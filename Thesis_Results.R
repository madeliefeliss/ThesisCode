# Loading all results:
{
  
path <- "Results/"
prefixes <- c("CON", "CAT", "UBCAT")
tree_structures <- c("T1", "T2")
d_values <- c("d0.3", "d0.5")
c_values <- c("c0.3", "c0.5", "c0.7")
p_values <- c("p0", "p0.5")
terminal_node_counts <- list()
moderator_counts <- list()

for (prefix in prefixes) {
  for (tree in tree_structures) {
    for (d in d_values) {
      for (c in c_values) {
        for (p in p_values) {
          # Constructing the filename for terminal_node_counts:
          filename <- paste0(prefix, "_", tree, "_", d, "_", c, "_", p, "_terminal_node_counts.rds")
          filepath <- file.path(path, filename)
          terminal_node_counts[[filename]] <- readRDS(filepath)
          
          # Constructing the filename for moderator_counts:
          filename2 <- paste0(prefix, "_", tree, "_", d, "_", c, "_", p, "_moderator_counts.rds")
          filepath2 <- file.path(path, filename2)
          
          # Reading the RDS file and storing it in the list:
          moderator_counts[[filename2]] <- readRDS(filepath2)
        }
      }
    }
  }
}


}

# Making the entropy data frame:
{
  
# Packages:
library(data.table)
library(dplyr)

# Function to compute the Shannon entropy:
compute_entropy <- function(counts) { 
  
  # Converting counts to probabilities:
  probs <- counts / sum(counts) 
  
  # Computing the entropy:
  -sum(probs * log(probs, base=2))  
}

# Initializing an empty data frame:
results_df <- data.table(
  moderators = character(),
  tree = integer(),
  delta = numeric(),
  correlation = numeric(),
  c_pruning = numeric(),
  entropy = numeric()
)

# Loop through the stored results list:
for (filename in names(terminal_node_counts)) {
  
  # Extracting parts of the filename:
  parts <- strsplit(filename, "_")[[1]]
  
  moderator <- parts[1]
  tree <- as.integer(substr(parts[2], 2, 2))  # Extract '1' or '2' 
  delta <- as.numeric(sub("d", "", parts[3]))  # Extract '0.3' or '0.5'
  correlation <- as.numeric(sub("c", "", parts[4]))  # Extract '0.3', '0.5', or '0.7'
  c_pruning <- as.numeric(sub("p", "", parts[5]))  # Extract '0' or '0.5'

  # Extracting counts from the list:
  counts <- terminal_node_counts[[filename]]
  
  # Computing the entropy:
  entropy_value <- compute_entropy(counts)
  
  # Adding results to the data frame:
  results_df <- rbind(results_df, data.table(
    moderators = moderator,
    tree = tree,
    delta = delta,
    correlation = correlation,
    c_pruning = c_pruning,
    entropy = entropy_value
  ))
}

}

# Plotting the entropy:
{

# Packages:  
library(ggplot2)
library(dplyr)

# Preparing the data frame:
results_df <- results_df %>%
  mutate(
    c_delta = factor(paste0("r = ", correlation, ", d = ", delta)),  
    moderators = factor(moderators, levels = c("CON", "CAT", "UBCAT"))  
  )


# Function to generate the bar plots:
plot_entropy <- function(tree_num) {
  
  ggplot(results_df %>% filter(tree == tree_num), aes(x = as.factor(c_pruning), y = entropy, fill = as.factor(c_pruning))) +
    
    geom_bar(stat = "identity", position = position_dodge()) +
    
    facet_grid(moderators ~ c_delta, scales = "fixed", space = "free") +  # Keep y-axis on the same scale
    
    labs(
      title = "",
      x = "c.pruning",  # Set x-axis label to "c.pruning"
      y = "Entropy"
    ) +
    
    theme_minimal() +
    
    theme(
      axis.text.x = element_text(size = 12),  
      axis.title.y = element_text(size = 14),
      strip.text.y = element_text(size = 12, face = "bold"),  
      strip.text.x = element_text(size = 10, face = "italic"),  
      panel.spacing = unit(1, "lines"),  
      legend.position = "none"  
    ) +
    
    scale_x_discrete(labels = c("0", "0.5")) +  
   
     scale_fill_manual(
      values = c("0" = "#1f78b4", "0.5" = "#f87a32"),
      guide = "none"  
    ) +
    ylim(0, 2.2)  
  
}

# Generating plots for Tree Model 1 and Tree Model 2:
plot1 <- plot_entropy(1)
plot2 <- plot_entropy(2)

plot1
plot2

}  

# Making the proportions data frame:
{
  
# Packages:
library(ggplot2)
library(dplyr)
library(data.table)

# Creating a new empty dataframe for terminal 
# node proportions:
node_df <- data.table()

# Loop through the stored results list:
for (filename in names(terminal_node_counts)) {
  
  # Extracting the filename parts:
  parts <- strsplit(filename, "_")[[1]]
  moderator <- parts[1]
  tree <- as.integer(substr(parts[2], 2, 2))  
  delta <- as.numeric(sub("d", "", parts[3]))  
  correlation <- as.numeric(sub("c", "", parts[4]))  
  c_pruning <- as.numeric(sub("p", "", parts[5]))  

  # Computing the terminal node proportions:
  counts <- terminal_node_counts[[filename]]
  total_nodes <- sum(counts)
  proportions <- counts / total_nodes
  
  # Creating a data frame with each terminal 
  # node size and the proportions:
  temp_df <- data.table(
    moderators = moderator,
    tree = tree,
    delta = delta,
    correlation = correlation,
    c_pruning = c_pruning,
    terminal_size = factor(seq_along(proportions)),  # Terminal node sizes
    proportion = proportions
  )
  
  # Adding the results:
  node_df <- rbind(node_df, temp_df)
}

}

# Plotting the proportions:
{
  
# Preparing the variables for plotting:
node_df <- node_df %>%
  mutate(
    c_delta = factor(paste0(~delta~"ρ = ", correlation, ", δ = ", delta)),  # Correlation + delta as facet variable
    moderators = factor(moderators, levels = c("CON", "CAT", "UBCAT"))  # Ensure correct order
  )

# Function to generate the stacked bar plots:
plot_terminal_nodes <- function(tree_num) {
  
  ggplot(node_df %>% filter(tree == tree_num), aes(x = as.factor(c_pruning), 
                                                   y = proportion.N, 
                                                   fill = proportion.terminal_node_counts)) +
    
    geom_bar(stat = "identity", position = "fill") +  
    
    facet_grid(moderators ~ c_delta, scales = "fixed", space = "free") +  
    
    labs(
      title = "",
      x = "Pruning Strictness",  
      y = "Proportion of Terminal Nodes"
    ) +
    
    theme_minimal() +
    
    theme(
      axis.text.x = element_text(size = 12),  
      axis.title.y = element_text(size = 14),
      strip.text.y = element_text(size = 12, face = "bold"),  
      strip.text.x = element_text(size = 10, face = "italic"),  
      panel.spacing = unit(1, "lines"),  
      legend.title = element_text(size = 12),  
      legend.text = element_text(size = 10)  
    ) +
    
    scale_x_discrete(labels = c("0", "0.5")) +  
    
    scale_fill_manual(
      values = c("#a6cee3", "#1f78b4", "#d66a75", "#f87a32", "#7a2d3c", "black"),
        name = "Terminal Node\nCount"
      )
}

# Generating plots for Tree Model 1 and Tree Model 2:
plot11 <- plot_terminal_nodes(1)
plot22 <- plot_terminal_nodes(2)

plot11
plot22
  
}

# Investigating the correlation:
{
  
library(metacart)
set.seed(3568970)
datCON <- read.csv("Datasets/datasetCON_T1_delta0.3_cor0.7.csv")
datCAT <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.7.csv")
datUBCAT <- read.csv("Datasets/datasetUBCAT_T1_delta0.3_cor0.7.csv")

# Convert categorical columns (m1, m2, m3, etc.) into numeric values:
datCAT$m1 <- as.numeric(factor(datCAT$m1))
datCAT$m2 <- as.numeric(factor(datCAT$m2))
datCAT$m3 <- as.numeric(factor(datCAT$m3))
datCAT$m4 <- as.numeric(factor(datCAT$m4))
datCAT$m5 <- as.numeric(factor(datCAT$m5))
datUBCAT$m1 <- as.numeric(factor(datUBCAT$m1))
datUBCAT$m2 <- as.numeric(factor(datUBCAT$m2))
datUBCAT$m3 <- as.numeric(factor(datUBCAT$m3))
datUBCAT$m4 <- as.numeric(factor(datUBCAT$m4))
datUBCAT$m5 <- as.numeric(factor(datUBCAT$m5))

# Selecting columns m1 to m10:
m_columnsCON <- datCON[, c("m1", "m2", "m3", "m4", "m5")]
m_columnsCAT <- datCAT[, c("m1", "m2", "m3", "m4", "m5")]
m_columnsUBCAT <- datUBCAT[, c("m1", "m2", "m3", "m4", "m5")]

# Computing the correlation matrix:
cor_matrixCON <- cor(m_columnsCON, use = "complete.obs")
cor_matrixCAT <- cor(m_columnsCAT, use = "complete.obs")
cor_matrixUBCAT <- cor(m_columnsUBCAT, use = "complete.obs")

# Extracting the upper triangle of the correlation matrix:
upper_triangleCON <- cor_matrixCON[upper.tri(cor_matrixCON)]
upper_triangleCAT <- cor_matrixCAT[upper.tri(cor_matrixCAT)]
upper_triangleUBCAT <- cor_matrixUBCAT[upper.tri(cor_matrixUBCAT)]

# Computing the mean of the correlations:
mean_correlationCON3 <- mean(upper_triangleCON)
mean_correlationCAT3 <- mean(upper_triangleCAT)
mean_correlationUBCAT3 <- mean(upper_triangleUBCAT)

mean_correlationCON3 
mean_correlationCAT3 
mean_correlationUBCAT3

mean_correlationCON1 ; mean_correlationCAT1 ; mean_correlationUBCAT1
mean_correlationCON2 ; mean_correlationCAT2 ; mean_correlationUBCAT2
mean_correlationCON3 ; mean_correlationCAT3 ; mean_correlationUBCAT3

}

# Analysis:
{

# Preparation: 
library(ggplot2)
library(dplyr)  
library(patchwork)

results_df$moderators <- as.factor(results_df$moderators)
results_df$correlation <- as.factor(results_df$correlation)
results_df$c_pruning <- as.factor(results_df$c_pruning)
results_df$delta <- as.factor(results_df$delta) 
results_df$tree <- as.factor(results_df$tree) 

dfT1 <- results_df[results_df$tree == 1, ]
dfT2 <- results_df[results_df$tree == 2, ]

dfT1

# Tree model 1:

# Model with only main effects:
aov_mainT1 <- aov(entropy ~ moderators + correlation + delta + c_pruning, data = dfT1)

# Model with main effects and two-way interactions:
aov_2wayT1 <- aov(entropy ~ moderators + correlation + delta + c_pruning + 
                  moderators:correlation + moderators:delta + moderators:c_pruning + 
                  correlation:delta + correlation:c_pruning + delta:c_pruning, 
                data = dfT1)

# Model with main effects, two-way interactions and
# three-way interactions:
aov_3wayT1 <- aov(entropy ~ moderators + correlation + delta + c_pruning + 
                  moderators:correlation + moderators:delta + moderators:c_pruning + 
                  correlation:delta + correlation:c_pruning + delta:c_pruning +
                  moderators:correlation:delta + moderators:correlation:c_pruning +
                  moderators:delta:c_pruning + correlation:delta:c_pruning,
                data = dfT1)

# Testing if adding two-way interactions improves fit:
anova(aov_mainT1, aov_2wayT1)  

# Testing if adding three-way interactions improves fit:
anova(aov_2wayT1, aov_3wayT1)  
anova(aov_mainT1, aov_3wayT1)  

# Checking assumptions:
 
# Normality of residuals:

# Q-Q plot:
qq_data1 <- data.frame(resid = residuals(aov_2wayT1))
ggplot(qq_data1, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Shapiro-Wilk test:
shapiro.test(residuals(aov_2wayT1))  

# Homoscedasticity: 

# Residuals vs. fitted plot:
fitted_vals1 <- fitted(aov_2wayT1)
resid_vals1 <- residuals(aov_2wayT1)
resid_data1 <- data.frame(fitted = fitted_vals1, resid = resid_vals1)

ggplot(resid_data1, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "",
       x = "Fitted Values",
       y = "Residuals")

summary(aov_2wayT1)

TukeyHSD(aov_2wayT1, "delta")

# Calculating the mean entropy for each combination of 
# c_pruning and moderators:
df_summaryT1 <- dfT1 %>%
  group_by(c_pruning, correlation) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

df_summaryT1_2 <- dfT1 %>%
  group_by(correlation, moderators) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

# C.pruning-Correlation interaction plot:
ggplot(df_summaryT1, aes(x = correlation, y = mean_entropy, color = c_pruning)) +
  geom_point(size = 4) +  # Larger points
  geom_line(aes(group = c_pruning), linewidth = 1) +  # Lines connecting the points
  labs(
    x = "Correlation",
    y = "Mean Entropy",
    title = "",
    color = "Pruning Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Correlation-Moderator interaction plot:
ggplot(df_summaryT1_2, aes(x = correlation, y = mean_entropy, color = moderators)) +
  geom_point(size = 4) +  # Larger points
  geom_line(aes(group = moderators), linewidth = 1) +  # Lines connecting the points
  labs(
    x = "Correlation",
    y = "Mean Entropy",
    title = "",
    color = "Moderators"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Tree model 2:

# Model with only main effects:
aov_mainT2 <- aov(entropy ~ moderators + correlation + delta + c_pruning, data = dfT2)

# Model with main effects and two-way interactions:
aov_2wayT2 <- aov(entropy ~ moderators + correlation + delta + c_pruning + 
                    moderators:correlation + moderators:delta + moderators:c_pruning + 
                    correlation:delta + correlation:c_pruning + delta:c_pruning, 
                  data = dfT2)

# Model with main effects, two-way interactions and
# three-way interactions:
aov_3wayT2 <- aov(entropy ~ moderators + correlation + delta + c_pruning + 
                    moderators:correlation + moderators:delta + moderators:c_pruning + 
                    correlation:delta + correlation:c_pruning + delta:c_pruning +
                    moderators:correlation:delta + moderators:correlation:c_pruning +
                    moderators:delta:c_pruning + correlation:delta:c_pruning,
                  data = dfT2)

# Testing if adding two-way interactions improves fit:
anova(aov_mainT2, aov_2wayT2)  

# Testing if adding three-way interactions improves fit:
anova(aov_2wayT2, aov_3wayT2)  
anova(aov_mainT2, aov_3wayT2) 

# Checking assumptions:

# Normality of residuals:

# Q-Q plot:
qq_data2 <- data.frame(resid = residuals(aov_3wayT2))
ggplot(qq_data2, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Shapiro-Wilk test:
shapiro.test(residuals(aov_3wayT2))  

# Homoscedasticity: 

# Residuals vs. fitted plot:
fitted_vals2 <- fitted(aov_3wayT2)
resid_vals2 <- residuals(aov_3wayT2)
resid_data2 <- data.frame(fitted = fitted_vals2, resid = resid_vals2)

ggplot(resid_data2, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "",
       x = "Fitted Values",
       y = "Residuals")

summary(aov_3wayT2)

# Interaction plots:

df_summary_T2_delta03_1 <- dfT2 %>%
  filter(delta == 0.3) %>%
  group_by(correlation, moderators) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

df_summary_T2_delta05_1 <- dfT2 %>%
  filter(delta == 0.5) %>%
  group_by(correlation, moderators) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

# Theme:
plot_theme <- theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

# Plot 1: Moderator and Correlation Interaction for delta = 0.3
p1 <- ggplot(df_summary_T2_delta03_1, aes(x = correlation, y = mean_entropy, color = moderators, group = moderators)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 4) + 
  labs(
    x = "Correlation",
    y = "Mean Entropy",
    title = expression("(a) "~delta~"= 0.3"),
    color = "Moderators"
  ) +
  ylim(0, 1.8) +
  plot_theme +
  theme(legend.position = "none")

# Plot 2: Moderator and Correlation Interaction for delta = 0.5
p2 <- ggplot(df_summary_T2_delta05_1, aes(x = correlation, y = mean_entropy, color = moderators, group = moderators)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 4) + 
  labs(
    x = "Correlation",
    y = NULL,
    title = expression("(b) "~delta~"= 0.5")
  ) +
  scale_color_discrete(guide = "none") +  # Removes second legend
  ylim(0, 1.8) +
  plot_theme

# Combining:
p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

df_summary_T2_delta03_2 <- dfT2 %>%
  filter(delta == 0.3) %>%
  group_by(c_pruning, correlation) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

df_summary_T2_delta05_2 <- dfT2 %>%
  filter(delta == 0.5) %>%
  group_by(c_pruning, correlation) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

# Plot 3: Correlation and C Pruning Interaction for delta = 0.3
p3 <- ggplot(df_summary_T2_delta03_2, aes(x = correlation, y = mean_entropy, color = c_pruning, group = c_pruning)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 4) +
  labs(
    x = "Correlation",
    y = "Mean Entropy",
    title = expression("(a) "~delta~"= 0.3"),
    color = "Pruning Strictness"
  ) +
  ylim(0, 1.8) +
  plot_theme

# Plot 4: Moderator and Correlation Interaction for delta = 0.5
p4 <- ggplot(df_summary_T2_delta05_2, aes(x = correlation, y = mean_entropy, color = c_pruning, group = c_pruning)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 4) +
  labs(
    x = "Correlation",
    y = "Mean Entropy",
    title = expression("(b) "~delta~"= 0.5")
  ) +
  scale_color_discrete(guide = "none") +
  ylim(0, 1.8) +
  plot_theme

# Combining:
p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

df_summary_T2_delta03_3 <- dfT2 %>%
  filter(delta == 0.3) %>%
  group_by(c_pruning, moderators) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

df_summary_T2_delta05_3 <- dfT2 %>%
  filter(delta == 0.5) %>%
  group_by(c_pruning, moderators) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

# Plot 5: Moderator and C Pruning Interaction for delta = 0.3
p5 <- ggplot(df_summary_T2_delta03_3, aes(x = c_pruning, y = mean_entropy, color = moderators, group = moderators)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 4) +
  labs(
    x = "Pruning Strictness",
    y = "Mean Entropy",
    title = expression("(a) "~delta~"= 0.3"),
    color = "Moderators"
  ) +
  ylim(0, 1.8) +
  plot_theme

# Plot 6: Correlation and C Pruning Interaction for delta = 0.5
p6 <- ggplot(df_summary_T2_delta05_3, aes(x = c_pruning, y = mean_entropy, color = moderators, group = moderators)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 4) +
  labs(
    x = "Pruning Strictness",
    y = "Mean Entropy",
    title = expression("(b) "~delta~"= 0.5")
  ) +
  scale_color_discrete(guide = "none") +
  ylim(0, 1.8) +
  plot_theme

# Combining:
p5 + p6 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

summary(aov_3wayT2)

TukeyHSD(aov_3wayT2, "delta")
TukeyHSD(aov_3wayT2, "moderators:delta:c_pruning")
TukeyHSD(aov_3wayT2, "moderators:correlation:delta")
TukeyHSD(aov_3wayT2, "correlation:delta:c_pruning")

}


