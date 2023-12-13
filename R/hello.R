#' Automatic Normality Test with Boxplots
#'
#' @param data A data frame with a grouping variable and a numeric variable.
#' @param group_var The name of the grouping variable.
#' @param numeric_var The name of the numeric variable.
#' @param alpha The significance level for the normality test (default is 0.05).
#'
#' @return A data frame with group-wise normality test results.
#'
#' @examples
#' # Example usage
#' data <- data.frame(
#'   Group = rep(c("A", "B", "C"), each = 30),
#'   Value = rnorm(90)
#' )
#' shapiro_test_results <- automatic_test(data, "Group", "Value")
#' @export
automatic_test <- function(data, group_var, numeric_var, alpha = 0.05) {
  groups <- unique(data[[group_var]])
  normality_results <- data.frame(Group = character(), p_value = numeric(), stringsAsFactors = FALSE)

  par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid for boxplots

  for (group in groups) {
    subset_data <- data[data[[group_var]] == group, ]

    # Boxplot
    boxplot(subset_data[[numeric_var]] ~ subset_data[[group_var]], main = paste("Boxplot for Group", group),
            xlab = group_var, ylab = numeric_var)

    shapiro_test <- shapiro.test(subset_data[[numeric_var]])

    normality_results <- rbind(normality_results, data.frame(Group = group, p_value = shapiro_test$p.value))

    if (shapiro_test$p.value > alpha) {
      cat(sprintf("Group '%s' is approximately normally distributed (Shapiro-Wilk p-value = %.4f)\n", group, shapiro_test$p.value))
    } else {
      cat(sprintf("Group '%s' is not approximately normally distributed (Shapiro-Wilk p-value = %.4f)\n", group, shapiro_test$p.value))
    }
  }

  par(mfrow = c(1, 1))  # Reset the plotting grid

  return(normality_results)
}
