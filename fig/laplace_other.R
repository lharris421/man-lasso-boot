source("./fig/setup.R")

alpha <- 0.2
for (i in 1:length(methods)) {
  methods[[i]]$method_arguments["alpha"] <- alpha
}

simulation_info <- list(seed = 1234, iterations = 1000,
                        simulation_function = "gen_data_distribution", simulation_arguments = list(
                          p = 100, SNR = 1, sigma = 10
                        ), script_name = "distributions")

## Load data back in
methods <- methods[c("lasso_boot", "selective_inference")]
ns <- c(50, 100, 400)
distributions <- c( "laplace")

files <- expand.grid("method" = names(methods), "n" = ns, "distribution" = distributions, stringsAsFactors = FALSE)

results <- list()
results_per_sim <- list()
for (i in 1:nrow(files)) {

  simulation_info$simulation_arguments$n <- files[i,] %>% pull(n)
  simulation_info$simulation_arguments$distribution <- files[i,] %>% pull(distribution)

  results[[i]] <- indexr::read_objects(
    rds_path,
    c(methods[[files[i,"method"]]], simulation_info)
    # args
  ) %>%
    mutate(method = files[i,] %>% pull(method), n = factor(files[i,] %>% pull(n)))

  results_per_sim[[i]] <- indexr::read_objects(
    rds_path,
    c(methods[[files[i,"method"]]], simulation_info)
    # args
  ) %>%
    group_by(iteration) %>%
    mutate(index = 1:n()) %>%
    ungroup() %>%
    filter(index == 1) %>%
    mutate(method = files[i,] %>% pull(method), n = factor(files[i,] %>% pull(n))) %>%
    select(method, n, time)

}

results <- bind_rows(results)
results_per_sim <- bind_rows(results_per_sim)

methods <- names(methods)
n_methods <- length(methods)


## Coverage
p1 <- results %>%
  filter(!is.na(estimate)) %>%
  mutate(covered = lower <= truth & upper >= truth) %>%
  group_by(method, iteration, n) %>%
  summarise(coverage = mean(covered, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = methods_pretty[method], y = coverage, fill = n)) +
  geom_boxplot() +
  geom_hline(yintercept = 1 - alpha) +
  scale_fill_manual(values = colors, name = "Sample Size", labels = c("50", "100", "400")) +
  ylab("Coverage") +
  xlab("Method") +
  theme_bw()

## Time
p2 <- results_per_sim %>%
  ggplot(aes(x = methods_pretty[method], y = time, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size", labels = c("50", "100", "400")) +
  ylab("Time") +
  xlab("Method") +
  theme_bw() +
  scale_y_continuous(labels = function(x) x, trans = "log10")

p3 <- results %>%
  mutate(width = upper - lower) %>%
  group_by(method, iteration, n) %>%
  summarise(width = median(width, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = methods_pretty[method], y = width, fill = n)) +
  geom_boxplot() +
  scale_fill_manual(values = colors, name = "Sample Size", labels = c("50", "100", "400")) +
  ylab(expression(`Median Width`)) +
  xlab("Method") +
  theme_bw() +
  scale_y_continuous(labels = function(x) x, trans = "log10")

## 28, 2 failed / 4, 10, 3, infinite median
results %>%
  mutate(width = upper - lower) %>%
  group_by(method, iteration, n) %>%
  summarise(width = median(width, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}")) %>%
  group_by(group) %>%
  summarise(nonfinite_median = mean(is.infinite(width)))

results %>%
  filter(method == "selective_inference") %>%
  mutate(width = upper - lower) %>%
  group_by(method, iteration, n) %>%
  summarise(width = any(is.infinite(width), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = glue("{method}-{n}")) %>%
  group_by(group) %>%
  summarise(nonfinite_median = sum(width))


pdf("./fig/laplace_other.pdf", height = 5, width = 7)
(p1 / p3 / p2) + plot_layout(guides = "collect", axes = "collect")
dev.off()
