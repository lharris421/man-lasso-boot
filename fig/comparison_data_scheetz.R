source("./fig/setup.R")

alpha <- 0.2
for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- alpha
}

simulation_info <- list(seed = 1234, iterations = 1,
                        simulation_function = "gen_data_distribution", simulation_arguments = list(
                          distribution = "Scheetz2006"
                        ), script_name = "distributions", same_lambda = TRUE)

## Load data back in
methods <- methods[c("lasso_boot", "selective_inference", "lasso_proj_boot_shortcut")]

files <- expand.grid(
  "method" = names(methods),
  stringsAsFactors = FALSE
)

results <- list()
for (i in 1:nrow(files)) {

  results[[i]] <- indexr::read_objects(
    rds_path,
    c(methods[[files[i,"method"]]], simulation_info)
    # args
  ) %>%
    mutate(method = files %>% filter(1:n() == i) %>% pull(method))
}


cis <- bind_rows(results)

cis %>%
  mutate(is_not_zero = lower > 0 | upper < 0) %>%
  group_by(method) %>%
  summarise(sum(is_not_zero))


## Plotting
pdf("./fig/comparison_data_scheetz.pdf", width = 8, height = 4)
plot_ci_comparison(cis, nvars = 30, ref = "lasso_boot")
dev.off()
