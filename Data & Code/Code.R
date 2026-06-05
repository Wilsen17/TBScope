library(sf)
library(spdep)
library(dplyr)
library(tmap)
library(readxl)
library(CARBayes)
library(coda)
library(ggplot2)
library(nimble)

jabar <- st_read("C:/Users/lenovo/Downloads/Skripsi/[geosai.my.id]Jawa_Barat_Kab/Jawa_Barat_ADMIN_BPS.shp")
data_kasus <- read_excel("C:/Users/lenovo/Downloads/Skripsi/Kasus.xlsx")

jabar_joined <- jabar %>%
  left_join(data_kasus, by = c("Kabupaten" = "KabupatenKota")) %>%
  filter(Kabupaten != "Waduk Cirata")

names(jabar_joined)[names(jabar_joined) == "Jumlah Kasus Penyakit - Angka Penemuan TBC"] <- "KasusTB"

jabar_geom <- jabar_joined
jabar_data <- st_drop_geometry(jabar_joined)

nb <- poly2nb(jabar_geom)
lw <- nb2listw(nb, style = "W")
W  <- nb2mat(nb, style = "B", zero.policy = TRUE)

moran_result <- moran.test(jabar_data$KasusTB, lw, alternative = "greater")
print(moran_result)

# Model CAR
jabar_data$trials <- jabar_data$Populasi

burnin_values   <- c(1000, 2000, 4000, 5000, 6000, 8000, 10000)
n_sample_values <- c(50000, 100000, 110000, 120000)

results_carbayess <- data.frame(
  burnin           = integer(),
  n_sample_add     = integer(),
  total_n_sample   = integer(),
  DIC_CAR_Poisson  = numeric(), WAIC_CAR_Poisson  = numeric(), LMPL_CAR_Poisson  = numeric(),
  DIC_CAR_Binomial = numeric(), WAIC_CAR_Binomial = numeric(), LMPL_CAR_Binomial = numeric(),
  DIC_iCAR_Poisson = numeric(), WAIC_iCAR_Poisson = numeric(), LMPL_iCAR_Poisson = numeric(),
  DIC_iCAR_Binomial= numeric(), WAIC_iCAR_Binomial= numeric(), LMPL_iCAR_Binomial= numeric()
)

param_list_carbayess <- list()

for (b in burnin_values) {
  for (ns in n_sample_values) {
    
    total_ns <- b + ns
    cat("Running burn-in =", b, "| n.sample add =", ns, "| total =", total_ns, "\n")
    
    set.seed(42)
    model_carp <- S.CARbym(
      formula  = KasusTB ~ 1, family = "poisson",
      data = jabar_data, W = W, burnin = b, n.sample = total_ns, thin = 10
    )
    
    set.seed(42)
    model_carl <- S.CARbym(
      formula  = KasusTB ~ 1, family = "binomial",
      data = jabar_data, W = W, trials = jabar_data$trials,
      burnin = b, n.sample = total_ns, thin = 10
    )
    
    set.seed(42)
    model_icarp <- S.CARleroux(
      formula  = KasusTB ~ 1, family = "poisson",
      data = jabar_data, W = W, burnin = b, n.sample = total_ns, thin = 10, rho = 1
    )
    
    set.seed(42)
    model_icarl <- S.CARleroux(
      formula  = KasusTB ~ 1, family = "binomial",
      data = jabar_data, W = W, trials = jabar_data$trials,
      burnin = b, n.sample = total_ns, thin = 10, rho = 1
    )
    
    key <- paste0("burnin_", b, "_ns_", ns)
    param_list_carbayess[[key]] <- list(
      CAR_Pois_params   = model_carp$summary.results,
      CAR_Binom_params  = model_carl$summary.results,
      iCAR_Pois_params  = model_icarp$summary.results,
      iCAR_Binom_params = model_icarl$summary.results
    )
    
    results_carbayess <- rbind(results_carbayess, data.frame(
      burnin            = b,
      n_sample_add      = ns,
      total_n_sample    = total_ns,
      DIC_CAR_Poisson   = model_carp$modelfit[["DIC"]],
      WAIC_CAR_Poisson  = model_carp$modelfit[["WAIC"]],
      LMPL_CAR_Poisson  = model_carp$modelfit[["LMPL"]],
      DIC_CAR_Binomial  = model_carl$modelfit[["DIC"]],
      WAIC_CAR_Binomial = model_carl$modelfit[["WAIC"]],
      LMPL_CAR_Binomial = model_carl$modelfit[["LMPL"]],
      DIC_iCAR_Poisson  = model_icarp$modelfit[["DIC"]],
      WAIC_iCAR_Poisson = model_icarp$modelfit[["WAIC"]],
      LMPL_iCAR_Poisson = model_icarp$modelfit[["LMPL"]],
      DIC_iCAR_Binomial = model_icarl$modelfit[["DIC"]],
      WAIC_iCAR_Binomial= model_icarl$modelfit[["WAIC"]],
      LMPL_iCAR_Binomial= model_icarl$modelfit[["LMPL"]]
    ))
  }
}

print(results_carbayess)
param_list_carbayess

# SKew-normal
dskewnorm <- nimbleFunction(
  run = function(x = double(0), xi = double(0), omega = double(0),
                 lambda = double(0), log = integer(0, default = 0)) {
    returnType(double(0))
    z           <- (x - xi) / omega
    log_phi     <- dnorm(z, log = TRUE) - log(omega)
    log_Phi     <- pnorm(lambda * z, log.p = TRUE)
    log_density <- log(2) + log_phi + log_Phi
    if (log) return(log_density) else return(exp(log_density))
  }
)

rskewnorm <- nimbleFunction(
  run = function(n = integer(0), xi = double(0), omega = double(0),
                 lambda = double(0)) {
    returnType(double(0))
    delta <- lambda / sqrt(1 + lambda^2)
    u1    <- rnorm(1)
    u2    <- rnorm(1)
    z     <- delta * abs(u1) + sqrt(1 - delta^2) * u2
    return(xi + omega * z)
  }
)

registerDistributions(list(
  dskewnorm = list(
    BUGSdist = "dskewnorm(xi, omega, lambda)",
    Rdist    = "dskewnorm(xi, omega, lambda)",
    types    = c("value = double(0)", "xi = double(0)",
                 "omega = double(0)", "lambda = double(0)")
  )
))

adj_info   <- nb2WB(nb)
m          <- nrow(jabar_data)
L          <- length(adj_info$adj)
y_vec      <- jabar_data$KasusTB
log_pop    <- log(jabar_data$Populasi)
init_beta0 <- log(mean(y_vec / exp(log_pop)))

num_vec <- adj_info$num
M_vec   <- ifelse(num_vec > 0, 1 / num_vec, 0)
C_vec   <- rep(NA, L)
idx     <- 1

for (i in 1:m) {
  for (j in 1:num_vec[i]) {
    C_vec[idx] <- M_vec[i]
    idx <- idx + 1
  }
}

nimble_data_icar <- list(
  y = y_vec, log_pop = log_pop,
  adj = adj_info$adj, weights = rep(1, L), num = adj_info$num
)
nimble_consts_icar <- list(m = m, L = L)
nimble_inits_icar  <- list(
  beta0 = init_beta0, omega_phi = 0.3, lambda_phi = 2.0,
  tau_xi = 4.0, sigma_u = 0.2,
  xi = rep(0, m), phi = rep(0, m), u = rep(0, m)
)

nimble_data_car <- list(
  y = y_vec, log_pop = log_pop,
  adj = adj_info$adj, num = num_vec,
  C = C_vec, M = M_vec, zeros = rep(0, m)
)
nimble_consts_car <- list(m = m, L = L)
nimble_inits_car  <- list(
  beta0 = init_beta0, omega_phi = 0.5, lambda_phi = 2.0,
  tau_xi = 3.0, rho = 0.7, sigma_u = 0.2,
  xi = rep(0, m), phi = rep(0, m), u = rep(0, m)
)

run_one <- function(model_code, nimble_data, nimble_consts,
                    nimble_inits, base_monitors, slice_targets,
                    extra_monitors = NULL, b, ns, thin = 10,
                    seed = 42) {
  
  total_ns <- b + ns
  cat("  burnin =", b, "| n.sample =", ns, "| total =", total_ns, "\n")
  
  set.seed(seed)
  
  mdl  <- nimbleModel(code = model_code, data = nimble_data,
                      constants = nimble_consts, inits = nimble_inits)
  cmdl <- compileNimble(mdl)
  
  cfg <- configureMCMC(mdl, enableWAIC = TRUE, print = FALSE)
  cfg$addMonitors(c(base_monitors, extra_monitors))
  
  for (trg in slice_targets) {
    cfg$removeSamplers(trg)
    cfg$addSampler(target = trg, type = "slice")
  }
  
  cmcmc <- compileNimble(buildMCMC(cfg), project = mdl)
  
  set.seed(seed)
  samp <- runMCMC(cmcmc, niter = total_ns, nburnin = b,
                  thin = thin, nchains = 1, samplesAsCodaMCMC = TRUE)
  
  WAIC_out   <- cmcmc$getWAIC()
  samp_mat   <- as.matrix(samp)
  mu_mat     <- samp_mat[, grep("^mu\\[", colnames(samp_mat)), drop = FALSE]
  loglik_mat <- t(apply(mu_mat, 1, function(r) dpois(y_vec, r, log = TRUE)))
  
  D_bar <- mean(-2 * rowSums(loglik_mat))
  D_pm  <- -2 * sum(dpois(y_vec, colMeans(mu_mat), log = TRUE))
  DIC   <- D_bar + (D_bar - D_pm)
  LMPL  <- sum(log(1 / colMeans(1 / exp(loglik_mat))))
  
  diag_params <- base_monitors[base_monitors != "mu"]
  if (!is.null(extra_monitors))
    diag_params <- c(diag_params, extra_monitors[extra_monitors != "mu"])
  
  list(
    summary = summary(samp[, diag_params, drop = FALSE]),
    geweke  = geweke.diag(samp[, diag_params, drop = FALSE]),
    DIC     = DIC,
    WAIC    = WAIC_out$WAIC,
    LMPL    = LMPL,
    row     = data.frame(
      burnin         = b,
      n_sample_add   = ns,
      total_n_sample = total_ns,
      DIC            = round(DIC,           3),
      WAIC           = round(WAIC_out$WAIC, 3),
      LMPL           = round(LMPL,          3)
    )
  )
}

print_results <- function(label, results, param_list) {
  cat("\n", strrep("=", 60), "\n")
  cat("HASIL", label, "(DIC / WAIC / LMPL)\n")
  cat(strrep("=", 60), "\n")
  print(results)
  cat("\n=== Summary Posterior", label, "===\n")
  for (key in names(param_list)) {
    cat("\n--", key, "--\n")
    print(param_list[[key]]$summary)
  }
  cat("\n=== Geweke Z-scores", label, "===\n")
  for (key in names(param_list)) {
    cat("\n--", key, "--\n")
    print(param_list[[key]]$geweke)
  }
}

burnin_values   <- c(1000, 2000, 4000, 6000, 8000, 10000, 15000)
n_sample_values <- c(50000, 100000, 110000)
param_grid      <- expand.grid(b = burnin_values, ns = n_sample_values)

# ICAR Skew-normal
model_code_icar <- nimbleCode({
  for (i in 1:m) {
    y[i]       ~ dpois(mu[i])
    log(mu[i]) <- log_pop[i] + beta0 + phi[i] + u[i]
    phi[i] ~ dskewnorm(xi = xi[i], omega = omega_phi, lambda = lambda_phi)
    u[i]   ~ dnorm(0, sd = sigma_u)
  }
  xi[1:m] ~ dcar_normal(adj[1:L], weights[1:L], num[1:m], tau_xi, zero_mean = 1)
  beta0      ~ dnorm(0, sd = 1)
  omega_phi  ~ dgamma(2, 4)
  lambda_phi ~ T(dnorm(2, sd = 0.5), 0.5, )
  tau_xi     ~ dgamma(2, 0.5)
  sigma_u    ~ dgamma(2, 10)
})

results_icar    <- data.frame()
param_list_icar <- list()

for (row_i in seq_len(nrow(param_grid))) {
  b   <- param_grid$b[row_i]
  ns  <- param_grid$ns[row_i]
  key <- paste0("burnin_", b, "_ns_", ns)
  
  cat("\n=== ICAR |", key, "===\n")
  
  res <- run_one(
    model_code    = model_code_icar,
    nimble_data   = nimble_data_icar,
    nimble_consts = nimble_consts_icar,
    nimble_inits  = nimble_inits_icar,
    base_monitors = c("beta0", "omega_phi", "lambda_phi", "tau_xi", "sigma_u", "mu"),
    slice_targets = c("lambda_phi", "omega_phi", "sigma_u"),
    b = b, ns = ns, seed = 42
  )
  
  param_list_icar[[key]] <- res[c("summary", "geweke", "DIC", "WAIC", "LMPL")]
  results_icar           <- rbind(results_icar, res$row)
  
  cat("  DIC:", round(res$DIC, 3),
      "| WAIC:", round(res$WAIC, 3),
      "| LMPL:", round(res$LMPL, 3), "\n")
  cat("  Summary:\n"); print(res$summary)
  cat("  Geweke:\n");  print(res$geweke)
}

print_results("ICAR", results_icar, param_list_icar)

# CAR Skew-normal
model_code_car <- nimbleCode({
  for (i in 1:m) {
    y[i]       ~ dpois(mu[i])
    log(mu[i]) <- log_pop[i] + beta0 + phi[i] + u[i]
    phi[i] ~ dskewnorm(xi = xi[i], omega = omega_phi, lambda = lambda_phi)
    u[i]   ~ dnorm(0, sd = sigma_u)
  }
  xi[1:m] ~ dcar_proper(
    mu = zeros[1:m], C = C[1:L], adj = adj[1:L],
    num = num[1:m],  M = M[1:m], tau = tau_xi, gamma = rho
  )
  beta0      ~ dnorm(0, sd = 1)
  omega_phi  ~ dgamma(2, 4)
  lambda_phi ~ T(dnorm(2, sd = 0.5), 0.5, )
  tau_xi     ~ dgamma(2, 0.5)
  sigma_u    ~ dgamma(2, 10)
  rho        ~ dunif(0.05, 0.95)
})

results_car    <- data.frame()
param_list_car <- list()

for (row_i in seq_len(nrow(param_grid))) {
  b   <- param_grid$b[row_i]
  ns  <- param_grid$ns[row_i]
  key <- paste0("burnin_", b, "_ns_", ns)
  
  cat("\n=== CAR |", key, "===\n")
  
  res <- run_one(
    model_code     = model_code_car,
    nimble_data    = nimble_data_car,
    nimble_consts  = nimble_consts_car,
    nimble_inits   = nimble_inits_car,
    base_monitors  = c("beta0", "omega_phi", "lambda_phi", "tau_xi", "sigma_u", "mu"),
    slice_targets  = c("lambda_phi", "omega_phi", "sigma_u"),
    extra_monitors = "rho",
    b = b, ns = ns, seed = 42
  )
  
  param_list_car[[key]] <- res[c("summary", "geweke", "DIC", "WAIC", "LMPL")]
  results_car           <- rbind(results_car, res$row)
  
  cat("  DIC:", round(res$DIC, 3),
      "| WAIC:", round(res$WAIC, 3),
      "| LMPL:", round(res$LMPL, 3), "\n")
  cat("  Summary:\n"); print(res$summary)
  cat("  Geweke:\n");  print(res$geweke)
}

print_results("CAR", results_car, param_list_car)

# Histogram
hist(jabar_data$KasusTB,
     probability = FALSE,
     main = "Distribusi Kasus Tuberkulosis di Jawa Barat",
     xlab = "Jumlah Kasus Tuberkulosis",
     ylab = "Frekuensi",
     col = "grey", border = "white", breaks = 5)

hist(jabar_data$Populasi,
     probability = FALSE,
     main = "Distribusi Populasi Penduduk di Jawa Barat",
     xlab = "Jumlah Populasi Penduduk",
     ylab = "Frekuensi",
     col = "grey", border = "white", breaks = 5)

jabar_data <- jabar_data %>%
  mutate(rate_tbc = (KasusTB / Populasi) * 100000) %>%
  arrange(desc(rate_tbc))

ggplot(jabar_data, aes(x = reorder(Kabupaten, rate_tbc), y = rate_tbc)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(
    title = "Proporsi Kasus Tuberkulosis per 100.000 Penduduk",
    x = "Kabupaten/Kota",
    y = "Kasus per 100.000 Penduduk"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Risiko Relatif
b_best     <- 10000
ns_best    <- 50000
total_best <- b_best + ns_best

set.seed(42)
mdl_best <- nimbleModel(
  code      = model_code_icar,
  data      = nimble_data_icar,
  constants = nimble_consts_icar,
  inits     = nimble_inits_icar
)
cmdl_best <- compileNimble(mdl_best)

cfg_best <- configureMCMC(mdl_best, enableWAIC = TRUE, print = FALSE)
cfg_best$addMonitors(c("beta0", "omega_phi", "lambda_phi", "tau_xi", "sigma_u", "mu"))

for (trg in c("lambda_phi", "omega_phi", "sigma_u")) {
  cfg_best$removeSamplers(trg)
  cfg_best$addSampler(target = trg, type = "slice")
}

mcmc_best  <- buildMCMC(cfg_best)
cmcmc_best <- compileNimble(mcmc_best, project = mdl_best)

set.seed(42)
samp_best <- runMCMC(
  cmcmc_best,
  niter             = total_best,
  nburnin           = b_best,
  thin              = 10,
  nchains           = 1,
  samplesAsCodaMCMC = TRUE
)

post_mat_best <- as.matrix(samp_best)
mu_cols       <- grep("^mu\\[", colnames(post_mat_best))
mu_mean_best  <- colMeans(post_mat_best[, mu_cols])

diag_params_best <- c("beta0", "omega_phi", "lambda_phi", "tau_xi", "sigma_u")

cat("\n=== Summary Model Terbaik (ICAR + Skew-Normal) ===\n")
print(summary(samp_best[, diag_params_best, drop = FALSE]))

jabar_geom$RR_icar_skewnorm <- mu_mean_best / mean(jabar_data$KasusTB)

tm_shape(jabar_geom) +
  tm_polygons(
    fill        = "RR_icar_skewnorm",
    fill.scale  = tm_scale_intervals(style = "quantile", values = "brewer.greens"),
    fill.legend = tm_legend(
      title            = "Risiko Relatif (RR)",
      item.width       = 0.7,
      item.height      = 0.6,
      item_text.margin = 0.5
    )
  ) +
  tm_text("Kabupaten", size = 0.6) +
  tm_borders() +
  tm_layout(
    legend.position  = c("RIGHT", "TOP"),
    legend.text.size = 0.9
  )

rr_table <- data.frame(
  Kabupaten = jabar_data$Kabupaten,
  KasusTB   = jabar_data$KasusTB,
  Mu_fitted = round(mu_mean_best, 4),
  RR        = round(mu_mean_best / mean(jabar_data$KasusTB), 4)
) %>% arrange(desc(RR))

print(rr_table)

