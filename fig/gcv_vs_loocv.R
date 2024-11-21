source("./fig/setup.R")
indexr::read_objects(rds_path, args_list = list(script_name = "gcv_vs_loocv")) %>%
  mutate(differences = ridge_gcv - ridge_loocv) %>%
  ggplot(aes(x = differences)) +
  geom_histogram(bins = 10) +
  facet_wrap(~scenario, nrow = 4, scales = "free") +
  theme_bw() +
  geom_vline(xintercept = 0, color = "red") +
  ggtitle("Difference in SSEEs For Ridge: GCV - LOOCV")
