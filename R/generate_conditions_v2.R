## alternative version that splits data at all combinations implied by df2, not by row
label_conditions <- function(df1, df2) {
  unique_x <- sort(unique(df2$x))
  unique_y <- sort(unique(df2$y))

  df1 %>%
    rowwise() %>%
    mutate(
      x_label = cut(x, breaks = c(-Inf, unique_x, Inf), labels = FALSE),
      y_label = cut(y, breaks = c(-Inf, unique_y, Inf), labels = FALSE),
      label = paste0(
        "x: ", case_when(
          x_label == 1 ~ paste0("< ", unique_x[1]), ## less than min
          x_label == length(unique_x) + 1 ~ paste0(">= ", unique_x[length(unique_x)]), ## more than max
          TRUE ~ paste0(unique_x[x_label - 1], " <= x < ", unique_x[x_label])
        ),
        " & y: ", case_when(
          y_label == 1 ~ paste0("< ", unique_y[1]),
          y_label == length(unique_y) + 1 ~ paste0(">= ", unique_y[length(unique_y)]),
          TRUE ~ paste0(unique_y[y_label - 1], " <= y < ", unique_y[y_label])
        )
      )
    )
}

# df1_labeled <- label_conditions(df1, df2)
# print(df1_labeled)
# subset(df1_labeled, x_label == 2 & y_label == 1)
#
# df2$des <- c('break detected for age 5', 'break detected for age 10')
#
# ggplot(df1_labeled, aes(x,y, pch =label)) +
#   # scale_color_grey() +
#   geom_point() +
#   # theme(legend.position = 'none') +
#   geom_hline(data = df2, aes(yintercept = y, color = des))+
#   geom_vline(data = df2, aes(xintercept = x, color = des))
