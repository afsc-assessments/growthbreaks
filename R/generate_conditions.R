
# Function to generate all combinations of conditions based on columns in df2
generate_conditions <- function(df1, row) {
  columns <- colnames(row)[colnames(row) %in% c('year','lat','long')]

  # Create a list of conditions for each column
  conditions <- map(columns, function(col) {
    list(
      less = df1 %>% filter(!!sym(col) < row[[col]]),
      greater = df1 %>% filter(!!sym(col) > row[[col]])
    )
  })

  # Generate all combinations of conditions
  combinations <- expand.grid(
    lapply(columns, function(col) c("less", "greater"))
  )

  # Apply the combinations to filter df1
  split_list <- map(1:nrow(combinations), function(i) {
    comb <- combinations[i, ]
    df1_filtered <- df1
    for (j in seq_along(columns)) {
      col <- columns[j]
      condition <- comb[[j]]
      df1_filtered <- df1_filtered %>%
        filter(!!sym(col) %in% conditions[[j]][[condition]][[col]])
    }
    df1_filtered
  })
  # Create names for the split list
  names(split_list) <- apply(combinations, 1, function(x) {
    paste0(columns, "_", x, "_", round(row[columns]), collapse = "_")
  })

  # names(split_list) <- apply(combinations, 1, paste, collapse = "_")
  return(split_list)
}
