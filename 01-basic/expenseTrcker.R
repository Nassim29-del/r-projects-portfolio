
# ============================================================
# Project 01: Personal Expense Tracker
# Level: Basic
# Skills: Data frames, functions, aggregate(), sorting,
#         loops, conditional statements, and base R plots
#
# Description:
# This project analyzes personal expenses by calculating
# total spending, category spending, monthly spending,
# budget status, and the five most expensive days.
# It also creates charts to visualize spending patterns.
# ============================================================


# ---- 1. Create Sample Data ----------------------------------

# set.seed() ensures that random results are reproducible.
# This means we get the same sample data every time we run
# the project.

set.seed(42)

# Create a vector containing the expense categories.

categories <- c(
  "Food",
  "Transport",
  "Rent",
  "Entertainment",
  "Shopping",
  "Bills"
)

# Create a data frame containing 120 sample expenses.
# Each row represents one expense.

expenses <- data.frame(

  # Generate random dates between January and June 2026.
  # as.Date() converts text into date format.
  # seq() creates a sequence of dates.
  # sample() randomly selects 120 dates.
  date = sample(
    seq(as.Date("2026-01-01"),
        as.Date("2026-06-30"),
        by = "day"),
    120,
    replace = TRUE
  ),

  # Randomly assign one of the expense categories
  # to each expense. replace = TRUE allows repetition.
  category = sample(categories, 120, replace = TRUE),

  # Generate random expense amounts between $5 and $250.
  # runif() generates random numbers.
  # round() rounds amounts to two decimal places.
  amount = round(runif(120, min = 5, max = 250), 2)
)


# ---- 2. Sort Data and Create a New Column -------------------

# Sort the expenses by date in ascending order
# (earliest date to latest date).

expenses <- expenses[order(expenses$date), ]

# Create a new column called month.
# format() extracts the year and month from each date.
# %Y represents the year and %m represents the month.

expenses$month <- format(expenses$date, "%Y-%m")

# Display the first six rows of the data frame.

cat("First rows of the data:\n")
print(head(expenses))


# ---- 3. Create a Helper Function ---------------------------

# Create a reusable function to format numbers as money.
# paste0() joins the dollar sign with the formatted number.
# formatC() adds two decimal places and comma separators.

format_money <- function(x) {
  paste0(
    "$",
    formatC(
      x,
      format = "f",
      digits = 2,
      big.mark = ","
    )
  )
}


# ---- 4. Calculate Overall Expense Summary ------------------

# Calculate the total, average, largest expense,
# and total number of expense records.

cat("\n===== Overall Summary =====\n")

# sum() calculates the total spending.
cat("Total spent:        ",
    format_money(sum(expenses$amount)), "\n")

# mean() calculates the average expense.
cat("Average expense:    ",
    format_money(mean(expenses$amount)), "\n")

# max() identifies the largest individual expense.
cat("Largest expense:    ",
    format_money(max(expenses$amount)), "\n")

# nrow() counts the total number of expense records.
cat("Number of expenses: ",
    nrow(expenses), "\n")


# ---- 5. Analyze Spending by Category -----------------------

# aggregate() groups expenses by category and calculates
# the total spending for each category.
# FUN = sum adds all amounts within each category.

by_category <- aggregate(
  amount ~ category,
  data = expenses,
  FUN = sum
)

# Sort categories from highest spending to lowest spending.
# The minus sign (-) indicates descending order.

by_category <- by_category[
  order(-by_category$amount),
]

# Calculate the percentage of total spending
# represented by each expense category.

by_category$percent <- round(
  100 * by_category$amount / sum(by_category$amount),
  1
)

# Display the category spending summary.

cat("\n===== Spending by Category =====\n")
print(by_category, row.names = FALSE)


# ---- 6. Analyze Spending by Month ---------------------------

# Group expenses by month and calculate the total
# amount spent during each month.

by_month <- aggregate(
  amount ~ month,
  data = expenses,
  FUN = sum
)

# Display monthly spending results.

cat("\n===== Spending by Month =====\n")
print(by_month, row.names = FALSE)


# ---- 7. Monthly Budget Check -------------------------------

# Set the monthly spending budget to $2,500.

monthly_budget <- 2500

cat(
  "\n===== Budget Check (budget:",
  format_money(monthly_budget),
  ") =====\n"
)

# Loop through each month to compare actual spending
# against the monthly budget.

for (i in seq_len(nrow(by_month))) {

  # Retrieve the spending amount for the current month.
  spent <- by_month$amount[i]

  # Check whether spending exceeds the monthly budget.
  # if/else assigns a status based on the condition.

  status <- if (spent > monthly_budget) {
    "OVER budget"
  } else {
    "within budget"
  }

  # Display the month, total spending, and budget status.

  cat(
    by_month$month[i],
    ":",
    format_money(spent),
    "->",
    status,
    "\n"
  )
}


# ---- 8. Identify the Top 5 Most Expensive Days -------------

# Group expenses by date and calculate total daily spending.

by_day <- aggregate(
  amount ~ date,
  data = expenses,
  FUN = sum
)

# Sort daily spending from highest to lowest
# and select only the top five days using head().

top_days <- head(
  by_day[order(-by_day$amount), ],
  5
)

# Display the five days with the highest spending.

cat("\n===== Top 5 Spending Days =====\n")
print(top_days, row.names = FALSE)


# ---- 9. Create and Save Charts -----------------------------

# Create an output folder for the chart image.
# showWarnings = FALSE prevents warnings if the folder exists.

dir.create("output", showWarnings = FALSE)

# Open a PNG graphics device.
# All charts created after this command will be saved
# inside the specified PNG file.

png(
  "output/expenses_charts.png",
  width = 1000,
  height = 450
)

# Arrange two charts side by side.
# mfrow = c(1, 2) means one row and two columns.
# mar controls the margins around the charts.

par(
  mfrow = c(1, 2),
  mar = c(9, 5, 4, 2)
)


# ---- 9A. Bar Chart: Spending by Category -------------------

# Create a bar chart showing total spending by category.
# names.arg displays category names below each bar.
# col sets the bar color.
# las = 2 rotates category labels for better readability.

barplot(
  by_category$amount,
  names.arg = by_category$category,
  col = "steelblue",
  las = 2,
  main = "Total Spending by Category",
  ylab = "Amount ($)"
)


# ---- 9B. Line Chart: Monthly Spending ----------------------

# Create a line chart showing monthly spending trends.
# type = "b" displays both points and connecting lines.
# pch = 19 uses solid circular points.
# xaxt = "n" hides the default horizontal axis.
# ylim adjusts the vertical axis to include the budget line.

plot(
  seq_along(by_month$month),
  by_month$amount,
  type = "b",
  pch = 19,
  col = "darkorange",
  lwd = 2,
  xaxt = "n",
  ylim = c(
    0,
    max(by_month$amount, monthly_budget) * 1.1
  ),
  main = "Monthly Spending",
  ylab = "Amount ($)",
  xlab = "Month"
)

# Add month labels to the horizontal axis.
# axis(1) refers to the bottom horizontal axis.

axis(
  1,
  at = seq_along(by_month$month),
  labels = by_month$month
)

# Add a horizontal dashed red line representing
# the monthly budget.
# This makes it easier to identify months exceeding
# the budget.

abline(
  h = monthly_budget,
  col = "red",
  lty = 2
)


# ---- 10. Close Graphics Device and Finish ------------------

# dev.off() closes the PNG device and saves the charts.
# invisible() prevents the returned device information
# from being printed.

invisible(dev.off())

# Confirm that the chart image has been saved.

cat("\nCharts saved to output/expenses_charts.png\n")

