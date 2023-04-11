get_cox_row <- function(df,
                        population = "adults", # (adults, pediatrics)
                        comorb_method = "lpca", #(lpca, score, ind)
                        censor_cutoff = "30", # (30, 60, 90)
                        outcome = "deceased_reg_elix") # (deceased_reg_elix, time_first_discharge_reg_elix))
  {

  survival_results_to_analyze = paste0('surv_results_', population, '_', comorb_method, '_', censor_cutoff)

  cox_output <- df[["first_hosp_results"]][["survival_results"]][[survival_results_to_analyze]][[outcome]][["fit_summary"]]
  coefficients(cox_output) %>%
    data.frame() %>%
    mutate(
      site = df$site
    ) %>%
    rownames_to_column("variable")

}

get_km_row <- function(df,
                       population = "adults", # (adults, pediatrics)
                       comorb_method = "lpca", #(lpca, score, ind)
                       censor_cutoff = "30", # (30, 60, 90)
                       outcome = "deceased_reg_elix") # (deceased_reg_elix, time_first_discharge_reg_elix))
{

  site <- df$site
  print(site)

  survival_results_to_analyze = paste0('surv_results_', population, '_', comorb_method, '_', censor_cutoff)

  km_output <- df[["first_hosp_results"]][["survival_results"]][[survival_results_to_analyze]][[outcome]][["surv_avg"]]

  if(!is.null(km_output)) {
    surv = km_output[["surv"]] %>%
    data.frame() %>%
    pivot_longer(., cols = names(km_output[['surv']]), names_to = "strata", values_to = "surv")

    std.err.sqrt = km_output[["std.err.sqrt"]] %>%
      data.frame() %>%
      pivot_longer(.,cols = names(km_output[['std.err.sqrt']]), names_to = "strata", values_to = "std.err.sqrt")

    std.err = km_output[["std.err"]] %>%
      data.frame() %>%
      pivot_longer(.,cols = names(km_output[['std.err']]), names_to = "strata", values_to = "std.err")

    time = km_output[["time"]] %>%
      data.frame() %>%
      pivot_longer(.,cols = names(km_output[['time']]), names_to = "strata", values_to = "time")

    km_output_df <- bind_cols(time,
                              surv %>% select(surv),
                              std.err.sqrt %>% select(std.err.sqrt),
                              std.err %>% select(std.err))

    km_output_df$site <- df$site

    return(km_output_df)
  }
}

get_severe_pmi <- function(df,
                        population = "adults", # (adults, pediatrics)
                        comorb_method = "lpca", #(lpca, score, ind)
                        censor_cutoff = "30" # (30, 60, 90)
                        )
  {

  severe_results_to_analyze = paste0('surv_results_', population, '_', comorb_method, '_', censor_cutoff)

  # pmi
  pmi <- df[["first_hosp_results"]][["survival_results"]][[severe_results_to_analyze]][["severe_reg_elix"]][["log.pmi"]] %>%
    pivot_longer(data = ., cols = contains("pmi"), names_to = "analysis", values_to = "PMI")

  # standard error
  pmi_se <- df[["first_hosp_results"]][["survival_results"]][[severe_results_to_analyze]][["severe_reg_elix"]][["se"]] %>%
    data.frame() %>%
    rename("se" = '.') %>%
    cbind(., "analysis" = pmi$analysis)

  pmi_results <- left_join(pmi, pmi_se, by = "analysis")

  site = df$site

  pmi_results$site <- site

  return(pmi_results)

}

get_severe_table <- function(df,
                             population = "adults", # (adults, pediatrics)
                             comorb_method = "lpca", #(lpca, score, ind)
                             censor_cutoff = "30" # (30, 60, 90)
                             )
  {

  severe_results_to_analyze = paste0('surv_results_', population, '_', comorb_method, '_', censor_cutoff)

  severe_table <- df[["first_hosp_results"]][["survival_results"]][[severe_results_to_analyze]][["severe_reg_elix"]][["severe_table_obfs"]]

  site = df$site

  severe_table <- cbind(site, severe_table)

  return(severe_table)


}

get_summary_stats <- function(df,
                        population = "adults", # (adults, pediatrics)
                        comorb_method = "lpca", #(lpca, score, ind)
                        censor_cutoff = "30", # (30, 60, 90)
                        outcome = "deceased_reg_elix",# (deceased_reg_elix, time_first_discharge_reg_elix))
                        cox_stat = "concordance") # stat from a site's fit_summary
{

  survival_results_to_analyze = paste0('surv_results_', population, '_', comorb_method, '_', censor_cutoff)

  cox_output <- df[["first_hosp_results"]][["survival_results"]][[survival_results_to_analyze]][[outcome]][["fit_summary"]][[cox_stat]] %>%
    data.frame() %>%
    t() %>%
    data.frame() %>%
    mutate(
      site = df$site,
      comorb_method = comorb_method,
      censor_cutoff = censor_cutoff,
      outcome = outcome
    ) %>%
    select(site, outcome, comorb_method, censor_cutoff, C, se.C.)

}

get_risk_table <- function(df,
                           population = "adults", # (adults, pediatrics)
                           comorb_method = "lpca", #(lpca, score, ind)
                           censor_cutoff = "30", # (30, 60, 90)
                           outcome = "deceased_reg_elix") {

  survival_results_to_analyze = paste0('surv_results_', population, '_', comorb_method, '_', censor_cutoff)

  risk_table <- df[["first_hosp_results"]][["survival_results"]][[survival_results_to_analyze]][[outcome]][["survtable"]]

  if(!is.null(risk_table)) {
    risk_table <- risk_table %>%
    mutate(
      site = df$site
    )

  return(risk_table)
  }
}


format_meta <- function(meta_results) {

  ma_combine <- list()

  for (i in names(meta_results)) {
    TE <- meta_results[[i]][["TE"]]
    #seTE <- meta_results[[i]][["seTE"]]
    studlab <- meta_results[[i]][["studlab"]]
    upper <- meta_results[[i]][["upper"]]
    lower <- meta_results[[i]][["lower"]]
    TE.random <- meta_results[[i]][["TE.random"]]
    lower.random <- meta_results[[i]][["lower.random"]]
    upper.random <- meta_results[[i]][["upper.random"]]
    pval.random <- meta_results[[i]][["pval.random"]]
    Weight.random <- meta_results[[i]][["w.random"]]
    lower.predict <- meta_results[[i]][["lower.predict"]]
    upper.predict <- meta_results[[i]][["upper.predict"]]
    I2 <- meta_results[[i]][["I2"]]
    lower.I2 <- meta_results[[i]][["lower.I2"]]
    upper.I2 <- meta_results[[i]][["upper.I2"]]
    H <- meta_results[[i]][["H"]]
    lower.H <- meta_results[[i]][["lower.H"]]
    upper.H <- meta_results[[i]][["upper.H"]]
    tau2 <- meta_results[[i]][["tau2"]]
    lower.tau2 <- meta_results[[i]][["lower.tau2"]]
    upper.tau2 <- meta_results[[i]][["upper.tau2"]]
    analysis <- paste(i)
    df <- cbind(
      analysis, studlab, TE, upper, lower, TE.random, lower.random, upper.random, Weight.random,
      pval.random, lower.predict, upper.predict, I2, lower.I2, upper.I2, H, lower.H, upper.H,
      tau2, lower.tau2, upper.tau2
    )

    ma_combine[[i]] <- df
  }

  results <- do.call(rbind.data.frame, ma_combine)

}

format_meta_severe <- function(meta_results, analysis_name) {


  TE <- meta_results[["TE"]]
  seTE <- meta_results[["seTE"]]
  studlab <- meta_results[["studlab"]]
  upper <- meta_results[["upper"]]
  lower <- meta_results[["lower"]]
  TE.random <- meta_results[["TE.random"]]
  lower.random <- meta_results[["lower.random"]]
  upper.random <- meta_results[["upper.random"]]
  pval.random <- meta_results[["pval.random"]]
  Weight.random <- meta_results[["w.random"]]
  lower.predict <- meta_results[["lower.predict"]]
  upper.predict <- meta_results[["upper.predict"]]
  I2 <- meta_results[["I2"]]
  lower.I2 <- meta_results[["lower.I2"]]
  upper.I2 <- meta_results[["upper.I2"]]
  H <- meta_results[["H"]]
  lower.H <- meta_results[["lower.H"]]
  upper.H <- meta_results[["upper.H"]]
  tau2 <- meta_results[["tau2"]]
  lower.tau2 <- meta_results[["lower.tau2"]]
  upper.tau2 <- meta_results[["upper.tau2"]]
  analysis <- paste(analysis_name)

  results <- cbind(
    analysis, studlab, TE, seTE, upper, lower, TE.random, lower.random, upper.random, Weight.random,
    pval.random, lower.predict, upper.predict, I2, lower.I2, upper.I2, H, lower.H, upper.H,
    tau2, lower.tau2, upper.tau2
  )

  results <- data.frame(results)

  return(results)

}


save_weights <- function(meta_results_df, population = "adults", comorb_method = "lpca", censor_cutoff = "30", cns_pns = "CNS", outcome = "deceased_reg_elix") {

  weights <- meta_results_df %>%
    filter(analysis == outcome) %>%
    select(analysis, studlab, Weight.random) %>%
    mutate(strata = cns_pns)

  analysis_name = paste0(population, '_', outcome, '_', comorb_method, '_', censor_cutoff, '_', cns_pns)

  assign(analysis_name, weights)

  # Save weights
  save(list = analysis_name, file = paste0('survival_model/', analysis_name, '.rda'))

}


prepare_data_forest_plots <- function(meta_results_df) {

  # We will convert our log(HR) to HR to facilitate interpretation and we will also perform appropriate rounding and rename variables here
  meta_results_df <- meta_results_df %>%
    group_by(analysis, studlab) %>%
    arrange(analysis, studlab) %>%
    ungroup() %>%
    mutate_at(vars(TE:upper.tau2), ~ as.character(.) %>% as.numeric()) %>%
    mutate_at(vars(TE:upper.predict, -pval.random, -Weight.random), ~ exp(.)) %>%
    mutate_at(vars(TE:upper.tau2, -pval.random), ~ round(., 2)) %>%
    mutate(CI.random = paste("(", lower.random, ",", upper.random, ")")) %>%
    rename(
      "Site" = studlab,
      "Hazard Ratio" = TE,
      "p-value" = pval.random,
      "CI.High" = upper,
      "CI.Low" = lower,
      "CI" = CI.random
    )


}
