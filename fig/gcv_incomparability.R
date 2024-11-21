source("./fig/setup.R")
indexr::read_objects(rds_path, args_list = list(script_name = "gcv_incomparability")) %>%
  pivot_longer(diff_rl_gl:diff_rr_gl, names_to = "diff_type", values_to = "differences") %>%
  mutate(
    diff_type = factor(
      diff_type,
      levels = c("diff_rl_gl", "diff_rl_ll", "diff_rr_gl"),
      labels = c("Ridge GCV SSEEs - Lasso LOOCV SSEEs", "Ridge LOOCV SSEEs - Lasso LOOCV SSEEs", "Ridge GCV SSEEs - Ridge LOOCV SSEEs")
    )
  ) %>%
  ggplot(aes(x = differences)) +
  geom_histogram(bins = 10) +
  facet_wrap(~diff_type, nrow = 3) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "red") +
  ggtitle("Difference in SSEEs: p = 100, 32 = +/- 0.25")
