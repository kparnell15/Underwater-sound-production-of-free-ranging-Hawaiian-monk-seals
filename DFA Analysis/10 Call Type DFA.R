# 2025 03 11_All Vocal Type DFA

# Load necessary libraries
library(corrplot)      # For visualizing correlations
library(MASS)          # For performing Discriminant Function Analysis
library(ggplot2)       # For visualization
library(caret)         # For LOOCV and confusion matrix
library(dplyr)         # For data manipulation
library(reshape2)      # For reshaping data


# Load the dataset
data <- read.csv("DFA_18 call types.csv")


#--------------------------------------------------------------------------------------

# Checking predictors (acoustic parameters) for multicollinearity

# Select acoustic parameters for correlation analysis
acoustic_params <- data %>%
  select(DeltaTime, Dur90, Freq5, Freq25, CenterFreq, Freq75, Freq95, BW50, BW90, 
         PeakFreq, AggEntropy)

# Compute correlation matrix (using complete observations only)
cor_matrix <- cor(acoustic_params, use = "complete.obs")

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.7, diag = FALSE)

# Identify highly correlated parameters (threshold: 0.7)
high_corr_indices <- findCorrelation(cor_matrix, cutoff = 0.7)
high_corr_names <- colnames(cor_matrix)[high_corr_indices]

# Display results
cat("\nHighly correlated predictors:\n", paste(high_corr_names, collapse = ", "), "\n")

# Show correlation values for each highly correlated predictor
for (param in high_corr_names) {
  cat("\nCorrelations for", param, ":\n")
  print(cor_matrix[param, ])
}

# Drop selected highly correlated predictors
filtered_predictors <- acoustic_params %>%
  select(-Freq25, -Freq75, -Freq95, -Dur90, -CenterFreq)

# Confirm final predictor set
cat("\nFiltered predictors:\n", paste(colnames(filtered_predictors), collapse = ", "), "\n")


#--------------------------------------------------------------------------------------

# 10 Vocal Type DFA 

# Combine predictors with the response variable
all_data <- data.frame(filtered_predictors, VocalType = data$VocalType)

# Calculate the total number of calls
total_calls <- nrow(all_data)

# Check how many calls per call type have been measured
table(all_data$VocalType)

# Exclude vocal types with fewer than 5 samples
filtered_data <- all_data %>%
  filter(VocalType %in% names(table(all_data$VocalType)[table(all_data$VocalType) > 4]))

# Calculate the total number of calls **(after filtering)**
total_filtered_calls <- nrow(filtered_data)

# Create a matrix with call type, total number, and chance level
chance_matrix <- as.data.frame(table(filtered_data$VocalType))
colnames(chance_matrix) <- c("Call Type", "Total Analyzed")

# Add column with chance level percentage
chance_matrix$`Chance Level (%)` <- (chance_matrix$`Total Analyzed` / total_filtered_calls) * 100

# Print the matrix
print("Chance level matrix for each call type:")
print(chance_matrix)

# Check the updated counts
table(filtered_data$VocalType)

# Define the control for LOOCV
train_control <- trainControl(method = "LOOCV")

# Perform DFA
model <- train(VocalType ~ ., data = filtered_data, 
               method = "lda", trControl = train_control)

# Print the model's accuracy results
print(model)

# Generate predictions using LOOCV
predictions <- predict(model, filtered_data)

# Confusion Matrix
conf_matrix <- confusionMatrix(factor(predictions), factor(filtered_data$VocalType))
print(conf_matrix)


# Convert confusion matrix counts to percentages
conf_matrix_table <- as.data.frame.matrix(conf_matrix$table) # Extract the table
conf_matrix_percent <- sweep(conf_matrix_table, 2, colSums(conf_matrix_table), FUN = "/") * 100 # Convert to percentages

# Print the percentage confusion matrix
print("Confusion matrix (percentages):")
print(round(conf_matrix_percent, 2))



#-------------------------------------------------------------------------------------- 

# Perform Non-Cross-Validated DFA for LDA Plot
non_cv_model <- lda(VocalType ~ ., data = filtered_data)

# Summary of the model
print(non_cv_model)

# Predict the groupings to plot acoustic space
non_cv_predictions <- predict(non_cv_model)

# Create a data frame with the LD scores for plotting
lda_scores <- data.frame(
  LD1 = non_cv_predictions$x[, 1],
  LD2 = non_cv_predictions$x[, 2],
  VocalType = filtered_data$VocalType
)



#--------------------------------------------------------------------------------------

# Extract coefficients for linear discriminants
ld_coefficients <- as.data.frame(non_cv_model$scaling)

# Normalize coefficients to calculate percentage contributions
ld_contributions <- ld_coefficients
ld_contributions$LD1_percent <- abs(ld_contributions$LD1) / sum(abs(ld_contributions$LD1)) * 100
ld_contributions$LD2_percent <- abs(ld_contributions$LD2) / sum(abs(ld_contributions$LD2)) * 100

# Print the contributions sorted by LD1 and LD2
ld_contributions <- ld_contributions[order(-ld_contributions$LD1_percent), ]
print("Contributions to LD1 and LD2 (in percentages):")
print(ld_contributions)

# Perform a 1-way ANOVA for LD1
anova_ld1 <- aov(LD1 ~ VocalType, data = lda_scores)
summary(anova_ld1)

# Perform a 1-way ANOVA for LD2
anova_ld2 <- aov(LD2 ~ VocalType, data = lda_scores)
summary(anova_ld2)



# --------------------------------------------------------------------------------------
# LDA Plot with 10 Vocal Types

# Define custom colors for elemental and combination calls
custom_colors <- c("Croak" = colors()[30],  
                   "Growl" = colors()[614],
                   "Moan" = colors()[616],
                   "Whoop" = colors()[555],
                   "GrowlRumbleWhoop" = "black",  
                   "MoanBridgeGrowl" = "black",
                   "MoanForagingCall" = "black",
                   "MoanGrowl" = "black",
                   "MoanGrowlRumbleWhoop" = "black",
                   "MoanWhoop" = "black")

# Define custom shapes for differentiation
custom_shapes <- c("Croak" = 16,  
                   "Growl" = 17,
                   "Moan" = 18,
                   "Whoop" = 8,
                   "GrowlRumbleWhoop" = 3,  
                   "MoanBridgeGrowl" = 4,
                   "MoanForagingCall" = 5,
                   "MoanGrowl" = 6,
                   "MoanGrowlRumbleWhoop" = 7,
                   "MoanWhoop" = 0)

# Filter data for elemental calls (for ellipses only)
elemental_calls <- c("Croak", "Growl", "Moan", "Whoop")
lda_scores_elemental <- lda_scores %>% filter(VocalType %in% elemental_calls)

# Save the LDA plot as a high-resolution PNG
ggsave("LDA_Plot_10calls.png", width = 12, height = 10, dpi = 800, bg = "white")


# LDA PLOT
ggplot(lda_scores, aes(x = LD1, y = LD2, color = VocalType, shape = VocalType)) +
  geom_point(aes(size = VocalType), alpha = 0.6, position = position_jitter(width = 0.2, height = 0.2)) +  
  stat_ellipse(data = lda_scores_elemental, aes(fill = VocalType), 
               geom = "polygon", alpha = 0.2, color = NA) +  
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  scale_fill_manual(values = custom_colors) +  # Match ellipse fill to point colors
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  scale_size_manual(values = c("Croak" = 7, "Growl" = 6.5, "Moan" = 8, "Whoop" = 6,  # Slightly larger Moan
                               "GrowlRumbleWhoop" = 5, "MoanBridgeGrowl" = 5, 
                               "MoanForagingCall" = 5, "MoanGrowl" = 5, 
                               "MoanGrowlRumbleWhoop" = 5, "MoanWhoop" = 5)) +  
  labs(
    title = "Linear Discriminant Analysis (LDA) Plot",
    x = "Linear Discriminant 1 (LD1)",
    y = "Linear Discriminant 2 (LD2)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered, bold title
    axis.title = element_text(size = 16),  # Larger axis titles
    axis.text = element_text(size = 14),  # Increase axis text size
    panel.grid.major = element_line(color = "gray90"),  # Lighten gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines for clarity
    legend.position = "none"  # REMOVE LEGEND
  )



# --------------------------------------------------------------------------------------
# Creating the Legend

# Load necessary libraries
library(ggplot2)

# Define colors and shapes (same as LDA plot)
custom_colors <- c("Croak" = colors()[30],  
                   "Growl" = colors()[614],
                   "Moan" = colors()[616],
                   "Whoop" = colors()[555],
                   "GrowlRumbleWhoop" = "black",  
                   "MoanBridgeGrowl" = "black",
                   "MoanForagingCall" = "black",
                   "MoanGrowl" = "black",
                   "MoanGrowlRumbleWhoop" = "black",
                   "MoanWhoop" = "black")

custom_shapes <- c("Croak" = 16,  
                   "Growl" = 17,
                   "Moan" = 18,   # Moan shape
                   "Whoop" = 8,
                   "GrowlRumbleWhoop" = 3,  
                   "MoanBridgeGrowl" = 4,
                   "MoanForagingCall" = 5,
                   "MoanGrowl" = 6,
                   "MoanGrowlRumbleWhoop" = 7,
                   "MoanWhoop" = 0)  # Unfilled square

# Adjust Moan's size so it visually matches the others
size_adjustments <- rep(5, length(custom_shapes))  # Default size = 5
names(size_adjustments) <- names(custom_shapes)
size_adjustments["Moan"] <- 6  # Slightly increase Moan's size

# Create a data frame for plotting the legend manually
legend_data <- data.frame(
  VocalType = names(custom_colors),
  x = rep(1, length(custom_colors)),  # Positioning x-axis
  y = seq(length(custom_colors), 1, by = -1)  # Positioning y-axis
)

# Manually create a plot for the legend
legend_plot <- ggplot(legend_data, aes(x = x, y = y, color = VocalType, shape = VocalType)) +
  geom_point(aes(size = VocalType)) +  # Use size adjustment
  geom_text(aes(label = VocalType), hjust = -0.5, size = 5, color = "black") +  # Add text labels
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_size_manual(values = size_adjustments) +  # Apply size fix
  theme_void() +  # No axes or background
  theme(legend.position = "none")  # Remove default legend

# Save the manually created legend
ggsave("LDA_Legend_10calls.png", plot = legend_plot, width = 6, height = 4, dpi = 600, bg = "white")



#--------------------------------------------------------------------------------------
# Create Confusion Matrix as Counts
DFAtable <- table(filtered_data$VocalType, predictions)

# Calculate Percent Correct Classification for Each Row
VocTypeClass <- matrix(nrow = nrow(DFAtable), ncol = ncol(DFAtable))
for (i in 1:nrow(DFAtable)) {
  VocTypeClass[i, ] <- round(DFAtable[i, ] / sum(DFAtable[i, ]), digits = 2)
}

# Rename Dimensions for Readability
dimnames(VocTypeClass) <- list(Actual = rownames(DFAtable), "Predicted (%)" = colnames(DFAtable))

# View Percent Correct Classification Table
print("Percent Correct Classification Matrix:")
print(VocTypeClass)

#--------------------------------------------------------------------------------------
# Plotting the confusion matrix

# Convert the percent correct classification matrix to a data frame for ggplot2
VocTypeClass_df <- as.data.frame(as.table(VocTypeClass))
colnames(VocTypeClass_df) <- c("Actual", "Predicted", "Percent")

# Reverse the order of the "Actual" axis to ensure Croak is at the top
VocTypeClass_df$Actual <- factor(VocTypeClass_df$Actual, levels = rev(levels(VocTypeClass_df$Actual)))

# Save the plot as a high-resolution PNG
png(
  filename = "Confusion_Matrix.png",
  width = 10,
  height = 8,
  units = "in",
  res = 800
)

# Save the plot as a high-resolution PNG
png(
  filename = "Confusion_Matrix.png",
  width = 12,
  height = 10,
  units = "in",
  res = 800
)

# Plot the confusion matrix
ggplot(VocTypeClass_df, aes(x = Predicted, y = Actual, fill = Percent)) +
  geom_tile(color = "grey80", width = 1, height = 1) +  # Grey grid lines between tiles
  scale_fill_gradientn(
    colors = c("white", "#E8F1FA", "#335577"),  # White → Very pale blue → Soft navy
    values = c(0, 0.02, 1),  # Ensures 1-4% is barely distinguishable from white
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::percent
  ) +  
  geom_text(aes(label = round(Percent * 100, 0)),  # Convert to whole number without "%"
            color = "black", size = 5, fontface = "bold") +  # Keep text readable
  labs(title = "Confusion Matrix: Percent Correct Classification",
       x = "Predicted Vocal Type",
       y = "Actual Vocal Type") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Reduce axis text size
    axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),  
    plot.title = element_text(size = 14, hjust = 0.5),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  # Shrink legend text
    plot.margin = margin(5, 5, 5, 5)  # Reduce margins for tighter fit
  )

dev.off()

#--------------------------------------------------------------------------------------
# Creating tables for averages of acoustic parameters for each vocal type 

# Compute averages and standard deviations for each call type
summary_table <- data %>%
  group_by(VocalType) %>%
  summarise(
    across(
      where(is.numeric), 
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  )

# Combine mean and SD into "mean ± SD" format
formatted_table <- summary_table %>%
  rowwise() %>%
  mutate(across(
    matches("_mean$"), 
    ~ {
      param <- sub("_mean$", "", cur_column())
      sd_col <- paste0(param, "_sd")
      sprintf("%.2f ± %.2f", get(cur_column()), get(sd_col))
    },
    .names = "{.col}"
  )) %>%
  ungroup()

# Remove intermediate mean and sd columns
formatted_table <- formatted_table %>%
  select(VocalType, ends_with("_mean")) %>%
  rename_with(~ sub("_mean$", "", .x), ends_with("_mean"))

# Export the table as a CSV file
write.csv(formatted_table, "All_Call_Types_Averages.csv", row.names = FALSE)

# View the resulting table
print("Formatted table for all call types with 'mean ± SD':")
print(formatted_table)




# Filter data to include only the 10 vocal types included in the DFA
final_ten_data <- data %>%
  filter(VocalType %in% c("Croak", "Growl", "GrowlRumbleWhoop", "Moan", "MoanBridgeGrowl", "MoanForagingCall", "MoanGrowl", "MoanGrowlRumbleWhoop", "MoanWhoop", "Whoop"))

# Compute averages and standard deviations for each call type
final_summary_table <- final_ten_data %>%
  group_by(VocalType) %>%
  summarise(
    across(
      where(is.numeric), 
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  )

# Combine mean and SD into "mean ± SD" format
final_formatted_table <- final_summary_table %>%
  rowwise() %>%
  mutate(across(
    matches("_mean$"), 
    ~ {
      param <- sub("_mean$", "", cur_column())
      sd_col <- paste0(param, "_sd")
      sprintf("%.2f ± %.2f", get(cur_column()), get(sd_col))
    },
    .names = "{.col}"
  )) %>%
  ungroup()

# Remove intermediate mean and sd columns
final_formatted_table <- final_formatted_table %>%
  select(VocalType, ends_with("_mean")) %>%
  rename_with(~ sub("_mean$", "", .x), ends_with("_mean"))

# Export the table as a CSV file
write.csv(final_formatted_table, "Final_Ten_Vocal_Types_Averages.csv", row.names = FALSE)

# View the resulting table
print("Formatted table for the final ten vocal types with 'mean ± SD':")
print(final_formatted_table)