

labs_desc <- c("Support (Rate Outcome)", "Support (Choice Outcome)", "Perceived Effectiveness of Prior Benefits",
               "EV Charging Stations",  "Driver", "Home Owner", "Age", "Education", "French",
               "Employment Sector", "Financial Condition", "Left-Right", "Salience: Globalisation", "Salience: Environment and Climate",
               "Region: Geneva", "Region: Middle Land", "Region: North East", "Region: Zurich", "Region: East", "Region: Central", "Region: Ticino", "Urban Area", "Intermediate Area", "Rural Area")

library(stargazer)
stargazer(dat_desc,
          out.header = F,
          no.space = TRUE, 
          label = "tab:summary_stats",
          column.sep.width = "3pt",
          font.size = "footnotesize",
          covariate.labels = labs_desc,
          out = "Tables/summary_stats.tex"
)

library(Hmisc)
correlation_matrix <- cor(dat_desc %>% mutate_all(., as.numeric), use = "pairwise.complete.obs")
correlation_matrix <- round(correlation_matrix, 2)
correlation_matrix[upper.tri(correlation_matrix)] <- NA
diag(correlation_matrix) <- NA
colnames(correlation_matrix) <- rownames(correlation_matrix) <- labs_desc
correlation_matrix1 <- correlation_matrix[,1:12]
correlation_matrix2 <- correlation_matrix[,13:ncol(correlation_matrix)]

stargazer(correlation_matrix1, title="Correlation Matrix Part 1", 
          float.env = "sidewaystable", 
          type = "latex", 
          out.header = F,
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "1pt", # to reduce column width
          font.size = "footnotesize", # to make font size smaller
          label = "tab:correlation_pt1",
          out = "Tables/correlation_pt1.tex"
          # covariate.labels = labs_desc,
          # dep.var.labels = labs_desc
)
stargazer(correlation_matrix2, title="Correlation Matrix  Part 2", 
          float.env = "sidewaystable", 
          type = "latex", 
          out.header = F,
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "1pt", # to reduce column width
          font.size = "footnotesize", # to make font size smaller
          label = "tab:correlation_pt2",
          out = "Tables/correlation_pt2.tex"
)
