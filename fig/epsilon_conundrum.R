source("./fig/setup.R")

for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- 0.2
}

simulation_info <- list(seed = 1234, iterations = 1000,
                        simulation_function = "gen_data_distribution", simulation_arguments = list(
                          p = 100, SNR = 1
                        ), script_name = "distributions")

## Load data back in
methods <- methods[c("lasso_boot", "traditional", "mcp_boot")]
ns <- c(100)
distributions <- c( "epsilon_conundrum")

files <- expand.grid("method" = names(methods), "n" = ns, "distribution" = distributions, stringsAsFactors = FALSE)

results <- list()
for (i in 1:nrow(files)) {

  simulation_info$simulation_arguments$n <- files[i,] %>% pull(n)
  simulation_info$simulation_arguments$distribution <- files[i,] %>% pull(distribution)

  results[[i]] <- indexr::read_objects(
    rds_path,
    c(methods[[files[i,"method"]]], simulation_info)
    # args
  ) %>%
    mutate(method = files[i,] %>% pull(method), distribution = files[i,] %>% pull(distribution), n = files[i,] %>% pull(n))
}

results <- bind_rows(results)

coverages <- results %>%
  data.frame() %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(method) %>%
  summarise(coverage = mean(covered))

methods <- unique(results$method)
plots <- list()
selected_iteration <- sample(1:simulation_info$iterations, 1)
for (i in 1:length(methods)) {

  true_vals <- results %>%
    filter(method == methods[i], iteration == 1) %>%
    pull(truth)
  names(true_vals) <- results %>%
    filter(method == methods[i], iteration == 1) %>%
    pull(variable)
  ordering <- names(sort(true_vals))[1:30]
  true_vals <- true_vals[1:30]

  cov <- coverages %>%
    filter(method == methods[i]) %>%
    pull(coverage)

  res <- results %>%
    filter(method == methods[i], iteration == selected_iteration) %>%
    filter(variable %in% names(true_vals))

  plots[[i]] <- ggplot(res) +
    geom_errorbar(aes(xmin = lower, xmax = upper, y = variable)) +
    geom_point(aes(x = estimate, y = variable)) +
    theme_bw() +
    labs(y = "Variable", x = "Estimate") +
    ggtitle(glue("{methods_pretty[methods[i]]} - Coverage: {round(cov * 100, 1)} %")) +
    ylab("Variable") +
    geom_point(data = data.frame(y = names(true_vals), x = true_vals), aes(x = x, y = y), color = "red", shape = 1, size = 2) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
}


pdf(glue("./fig/epsilon_conundrum.pdf"), height = 4, width = 4)
plots[[which(methods == "lasso_boot")]]
dev.off()

pdf(glue("./fig/epsilon_conundrum_traditional.pdf"), height = 4, width = 4)
plots[[which(methods == "traditional")]]
dev.off()

pdf(glue("./fig/epsilon_conundrum_mcp_boot.pdf"), height = 4, width = 4)
plots[[which(methods == "mcp_boot")]]
dev.off()
