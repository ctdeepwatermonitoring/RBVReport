#sidebar: match new names to outdated names
df1 <- taxon_wqx_antijoin
df2 <- taxon

#empty list to store matching rows
matching_rows <- list()

#find matching rows
for (i in 1:nrow(df1)) {
  for (j in 1:nrow(df2)) {
    if (grepl(df1$Name[i], df2$Name[j])) {
      matching_rows[[length(matching_rows) + 1]] <- list(df1_row = df1[i,], df2_row = df2[j,])
    }
  }
}

#combine matching rows into a single dataframe
matching_df <- do.call(rbind, lapply(matching_rows, as.data.frame))
subset_matching_df <- filter(matching_df, (grepl("retired", df2_row.Name)))
subset_matching_df_trimmed <- subset_matching_df[c("df1_row.finalID", "df2_row.Name" )]