# Load necessary libraries
library(dplyr)
library(readr)
library(readxl)

# Inputs
N_cookies_view_page <- 40000  # Unique cookies visiting per day
N_clicks <- 3200              # Unique cookies clicking "Start free trial"
N_enrollments <- 660          # Enrollments per day
pageviews <- 5000

# Probabilities
p_click_through <- 0.08                        # Probability of click-through
p_enroll_given_click <- 0.20625                # Probability of enrollment given click
p_payment_given_enroll <- 0.53                 # Probability of payment given enrollment
p_payment_given_click <- 0.1093125             # Probability of payment given click

# Standard deviation calculations
gross_conversion <- round(sqrt((p_enroll_given_click * (1 - p_enroll_given_click)) / (pageviews * N_clicks / N_cookies_view_page)), 4)

retention <- round(sqrt((p_payment_given_enroll * (1 - p_payment_given_enroll)) / (pageviews * N_enrollments / N_cookies_view_page)), 4)

# Corrected: closed the parentheses properly for net_conversion
net_conversion <- round(sqrt((p_payment_given_click * (1 - p_payment_given_click)) / (pageviews * N_clicks / N_cookies_view_page)), 4)

# Output
gross_conversion
retention
net_conversion


# Gross Conversion
gross_conversion_baseline <- 0.20625
mde_gross <- 0.01  # Minimum Detectable Effect
alpha <- 0.05
beta <- 0.20
sample_size_gross <- 25835 * 2  # Multiply by 2 for experiment and control
pageviews_gross <- sample_size_gross / 0.08  # clicks/pageview = 3200/40000

# Retention
retention_baseline <- 0.53
mde_retention <- 0.01
sample_size_retention <- 39115 * 2
pageviews_retention <- sample_size_retention / 0.0165  # enrollments/pageview = 660/40000

# Net Conversion
net_conversion_baseline <- 0.109313
mde_net <- 0.0075
sample_size_net <- 27413 * 2
pageviews_net <- sample_size_net / 0.08

# Print calculated pageviews
pageviews_gross
pageviews_retention
pageviews_net

# Print calculated pageviews
pageviews_gross/N_cookies_view_page
pageviews_retention/ N_cookies_view_page
pageviews_net/N_cookies_view_page

# Adjusting for 60% Traffic
pageviews_net/ (0.6 * N_cookies_view_page)



# Sanity Checks

# Load the data from both sheets
control_data <- read_excel("~/TURING COLLEGE/Control vs Experiment.xlsx", sheet = "Control")
experiment_data <- read_excel("~/TURING COLLEGE/Control vs Experiment.xlsx", sheet = "Experiment")

# Calculate the sums for Control group
control_summary <- control_data %>%
  summarise(Total_Pageviews = sum(Pageviews, na.rm = TRUE),
            Total_Clicks = sum(Clicks, na.rm = TRUE))

# Calculate the sums for Experiment group
experiment_summary <- experiment_data %>%
  summarise(Total_Pageviews = sum(Pageviews, na.rm = TRUE),
            Total_Clicks = sum(Clicks, na.rm = TRUE))

# Print the results
print("Control Group Summary:")
print(control_summary)

print("Experimental Group Summary:")
print(experiment_summary)



# Supporting calculation for checking invariant metrics

# Number of cookies
p <- 0.5
total_cookies <- 345543 + 344660
SE_cookies <- sqrt(p * p / total_cookies)
cat("Number of cookies= ", total_cookies,"\n")
cat("Standard error SE =", SE_cookies, "\n")
ME_cookies <- SE_cookies * 1.96
cat("Margin of error ME =", ME_cookies, "\n")
LB_cookies <- p - ME_cookies
UB_cookies <- p + ME_cookies
cat("Confidential interval CI =", LB_cookies, "to", UB_cookies, "\n")
cat("p̂ =", 345543 / total_cookies, "∈", LB_cookies, "to", UB_cookies, "✓\n\n")

# Number of clicks on "Start free trial"
total_clicks <- 28378 + 28325
SE_clicks <- sqrt(0.5 * 0.5 / total_clicks)
cat("Number of clicks on 'Start free trial'=", total_clicks,"\n")
cat("Standard error SE =", SE_clicks, "\n")
ME_clicks <- SE_clicks * 1.96
cat("Margin of error ME =", ME_clicks, "\n")
LB_clicks <- p - ME_clicks
UB_clicks <- p + ME_clicks
cat("Confidential interval CI =", LB_clicks, "to", UB_clicks, "\n")
cat("p̂ =", 28378 / total_clicks, "∈", LB_clicks, "to", UB_clicks, "✓\n\n")

# Click-through probability on "Start free trial"
p_pool <- (28378 + 28325) / total_cookies
cat("Click-through probability on 'Start free trial'=", p_pool,"\n")
cat("Pooled probability p_pool =", p_pool, "\n")
SE_pool <- sqrt(p_pool * (1 - p_pool) * (1 / 345543 + 1 / 344660))
cat("Standard error SE =", SE_pool, "\n")
ME_pool <- SE_pool * 1.96
cat("Margin of error ME =", ME_pool, "\n")
d_hat <- (28325 / 344660) - (28378 / 345543)
cat("Difference d̂ =", d_hat, "\n")
LB_pool <- 0 - ME_pool
UB_pool <- 0 + ME_pool
cat("Confidential interval CI =", LB_pool, "to", UB_pool, "\n")
cat("d̂ ∈", LB_pool, "to", UB_pool, "✓\n")

#Effect Size Tests

# Select the first 23 rows from control_data and experiment_data
new_control_data <- control_data[1:23, ]
new_experiment_data <- experiment_data[1:23, ]

cat("Clicks =", sum(new_control_data$Clicks, na.rm = TRUE), 
    "   Enrollments =", sum(new_control_data$Enrollments, na.rm = TRUE), 
    "   Payments =", sum(new_control_data$Payments, na.rm = TRUE), "\n\n")

# Supporting calculation for checking evaluation metrics

cat("Gross conversion\n")
# Pooled probability for gross conversion
p_pool_gross <- (3785.0 + 3423.0) / (17293 + 17260)
cat("Pooled probability p_pool =", p_pool_gross, "\n")

# Standard error calculation for gross conversion
SE_pool_gross <- sqrt(p_pool_gross * (1 - p_pool_gross) * (1 / 17293 + 1 / 17260))
cat("Standard error SE =", SE_pool_gross, "\n")

# Margin of error for gross conversion
ME_pool_gross <- SE_pool_gross * 1.96
cat("Margin of error ME =", ME_pool_gross, "\n")

# Difference in gross conversion rates between experiment and control
d_gross <- 3423.0 / 17260 - 3785.0 / 17293
cat("Difference d =", d_gross, "\n")

# Confidence interval for the difference
LB_pool_gross <- d_gross - ME_pool_gross
UB_pool_gross <- d_gross + ME_pool_gross
cat("Confidence interval CI =", LB_pool_gross, ",", UB_pool_gross, "\n")
cat(c(-0.01, 0, 0.01), "∉", c(LB_pool_gross, UB_pool_gross), "\n")
cat("Statistical significance ✓  Practical significance ✓\n\n")


cat("Net conversion\n")
# Pooled probability for net conversion
p_pool_net <- (2033.0 + 1945.0) / (17293 + 17260)
cat("Pooled probability p_pool =", p_pool_net, "\n")

# Standard error calculation for net conversion
SE_pool_net <- sqrt(p_pool_net * (1 - p_pool_net) * (1 / 17293 + 1 / 17260))
cat("Standard error SE =", SE_pool_net, "\n")

# Margin of error for net conversion
ME_pool_net <- SE_pool_net * 1.96
cat("Margin of error ME =", ME_pool_net, "\n")

# Difference in net conversion rates between experiment and control
d_net <- 1945.0 / 17260 - 2033.0 / 17293
cat("Difference d =", d_net, "\n")

# Confidence interval for the difference
LB_pool_net <- d_net - ME_pool_net
UB_pool_net <- d_net + ME_pool_net
cat("Confidence interval CI =", LB_pool_net, ",", UB_pool_net, "\n")

# Practical and statistical significance for net conversion
cat("0 belongs to CI:", LB_pool_net <= 0 && 0 <= UB_pool_net, "\n")
cat("dmin = -0.0075 belongs to CI:", LB_pool_net <= -0.0075 && -0.0075 <= UB_pool_net, "\n")
cat("Statistical significance ✘  Practical significance ✘\n")


# Sign Tests
# Supporting calculation of successful events for the evaluation metrics

# Gross conversion success comparison between new_experiment_data and new_control_data
Gross_conversion_success <- (new_experiment_data$Enrollments / new_experiment_data$Clicks) > 
  (new_control_data$Enrollments / new_control_data$Clicks)

cat("Gross conversion: success =", sum(Gross_conversion_success, na.rm = TRUE), 
    "  total =", length(Gross_conversion_success), "\n")

# Net conversion success comparison between new_experiment_data and new_control_data
Net_conversion_success <- (new_experiment_data$Payments / new_experiment_data$Clicks) > 
  (new_control_data$Payments / new_control_data$Clicks)

cat("Net conversion: success =", sum(Net_conversion_success, na.rm = TRUE), 
    "  total =", length(Net_conversion_success), "\n")
