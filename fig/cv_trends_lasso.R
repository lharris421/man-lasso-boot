source("./fig/setup.R")
indexr::read_objects(rds_path, args_list = list(script_name = "cv_trends_lasso")) %>%
  mutate(folds = factor(folds)) %>%
  ggplot(aes(x = folds, y = sse, fill = folds)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(~scenario, nrow = 4, scales = "free") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Lasso: K-Fold CV SSEEs relative to LOOCV")

