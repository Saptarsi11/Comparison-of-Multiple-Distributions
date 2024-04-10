# Introductory to Case Studies - Project 2
#Team members - Group 5
#Group members: Zahidul Islam Prince, Ikhtiar Ahmed, Saptarsi Bhattacharya, Hemalatha Sekar

#Q1

# Load the dataset
data <- read.csv("C:/Users/bhatt/Downloads/babies.csv")  

mean_wt <- mean(data$wt, na.rm = TRUE)
data$wt[is.na(data$wt)] <- mean_wt

# Select the relevant variables
selected_vars <- c("wt", "smoke")
selected_data <- data[selected_vars]

# Convert smoke variable to a factor
selected_data$smoke <- factor(selected_data$smoke,
                              labels = c("Never", "Smokes Now", "Until Current Pregnancy", "Once Did, Not Now", "Unknown"))

# Descriptive statistics for birth weight (continuous variable)
birth_weight_stats <- summary(selected_data$wt)

# Descriptive statistics for smoking history (categorical variable)
smoking_history_count <- table(selected_data$smoke)

# Print the results
cat("Descriptive statistics for birth weight (wt):\n")
print(birth_weight_stats)

cat("\nCount of smoking history (smoke):\n")
print(smoking_history_count)

# Load the ggplot2 package
library(ggplot2)

# Select the birth weight variable
wt <- data$wt

# Create a histogram of birth weight
ggplot(data, aes(x = wt)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(x = "Birth Weight (oz)", y = "Count", title = "Distribution of Birth Weight")



#Q2
#Boxplot for homogeneity of variance assessment
library(ggplot2)
data$smoke <- factor(data$smoke)

ggplot(data = data, aes(x = smoke, y = wt)) +
  geom_boxplot(fill = "darkcyan") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))


#QQPlot for normality assessment

library(gridExtra)

qq_plots <- list()

for (i in levels(data$smoke)){
  new_df <- data[data$smoke == i,]
  
  p <- ggplot(data = new_df, mapping = aes(sample = wt)) + 
    geom_qq(colour = "darkcyan") + 
    geom_qq_line(size = 1) +
    labs(x = "", y = i) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, colour = "black", size = 1.5))
  
  qq_plots[[i]] <- p
}
grid.arrange(grobs = qq_plots, nrow = 2, ncol = 3)



#Anova Test
anova.res <- aov(formula = wt ~ smoke, data = data)
summary(anova.res)


#Q3

#Without Adjustment pairwise t-test
pairwise.t.test(x = data$wt, g = data$smoke, p.adjust.method = "none", pool.sd = TRUE)

#Bonferroni Adjustment pairwise t-test
pairwise.t.test(x = data$wt, g = data$smoke, p.adjust.method = "bonferroni", pool.sd = TRUE)



library(TukeyC)
# Apply Tukey's HSD test

tukey_results <- TukeyHSD(anova.res)
tukey_results

# Calculate Tukey's confidence intervals
confidence_intervals <- confint(anova.res, level = 0.95)
confidence_intervals