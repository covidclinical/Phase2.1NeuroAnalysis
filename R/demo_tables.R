

create_demo_tableone <- function(sorted_sites, is_pediatric = FALSE) {

  # create empty list to save demographics results
  demo_table_list <- list()

  if(is_pediatric==FALSE) {
    population = "adults"
  } else {
    population = "pediatrics"
  }

  # for each healthcare system, import the demographics table
  for (i in sorted_sites) {
    #print(i)
    tmp <- get(i)
    demo_table <- tmp[[c(
      "first_hosp_results",
      "tableone_results",
      paste0("tableone_", population),
      "demo_table"
    )]]

    if(is.null(demo_table)) {
      #print(i)
      print('no pediatric patients')
    } else {
      demo_table_list[[i]] <- demo_table %>%
        select(site, everything())
    }
  }


  demo_table <- bind_rows(demo_table_list) %>%
    mutate(
      # site level differences in variable names require tolower()
      site = toupper(site),
      variable = tolower(variable),
      # rename race.other to avoid recoding conflicts with age_group.other
      variable = if_else(variable == "race.other", "race.Other", variable),
      variable = if_else(variable == "age_group.other", "age_group.Unknown", variable),
      Demo_var = sub("\\..*", "", variable),
      Demo_var_i = sub(".*\\.", "", variable) %>%
        gsub("_", " ", .) %>%
        str_to_title() %>%
        recode(
          `00to02` = "0-2",
          `03to05` = "3-5",
          `06to11` = "6-11",
          `12to17` = "12-17",
          `18to25` = "18-25",
          `26to49` = "26-49",
          `50to69` = "50-69",
          `70to79` = "70-79",
          `80plus` = "80+",
          race.white = "White",
          race.american_indian = "American Indian",
          `Hawaiian Pacific Islander` = "Hawaiian/Pacific Islander",
          `Hispanic Latino` = "Hispanic/Latino",
          `False` = "Not Readmitted",
          `True` = "Readmitted"
        )
    )

  return(demo_table)


}

create_clinical_tableone <- function(sorted_sites, is_pediatric = FALSE) {

  # create empty list to save clinical-demographics results
  clinical_table_list <- list()

  if(is_pediatric==FALSE) {
    population = "adults"
  } else {
    population = "pediatrics"
  }

  # for each healthcare system, import the clinical-demographics table of continuous variable
  for (i in sorted_sites) {
    #print(i)
    tmp <- get(i)
    clinical_table <- tmp[[c(
      "first_hosp_results",
      "tableone_results",
      paste0("tableone_", population),
      "other_obfus_table"
    )]]

    if(is.null(clinical_table)) {
      print(i)
      print('no pediatric patients')
    } else {
      clinical_table_list[[i]] <- clinical_table %>%
        select(site, everything())
    }
  }

  # create a dataframe with the continuous variables prefixed "Median"
  clinical_table_wide <- bind_rows(clinical_table_list) %>%
    filter(grepl("Median", name))


  # here we back-transform the median pre-admission scores and round them
  clinical_table_wide_pre_adm <- clinical_table_wide %>%
    filter(name == "Median pre admission cns [Min, Max]" | name == "Median pre admission pns [Min, Max]") %>%
    separate(None, c("None_first", "None_min", "None_max"), sep=" ") %>%
    separate(Central, c("CNS_first", "CNS_min", "CNS_max"), sep=" ") %>%
    separate(Peripheral, c("PNS_first", "PNS_min", "PNS_max"), sep=" ") %>%
    mutate(across(None_first:CNS_max, ~ gsub("\\[|\\]", "", .x))) %>%
    mutate(across(None_first:CNS_max, ~ gsub(",", "", .x))) %>%
    mutate(across(None_first:CNS_max, ~ as.numeric(.) %>%
                    transform_count())) %>%
    mutate(across(None_first:CNS_max, ~ round(.,1)))%>%
    mutate(None = paste0(None_first, " [", None_min, ",", None_max, "]"),
           Central = paste0(CNS_first, " [", CNS_min, ",", CNS_max, "]"),
           Peripheral = paste0(PNS_first, " [", PNS_min, ",", PNS_max, "]")) %>%
    select(site, name, None, Peripheral, Central)

  # remove the old pre-admission scores
  clinical_table_wide <- clinical_table_wide %>%
    filter(!name == "Median pre admission cns [Min, Max]",
           !name == "Median pre admission pns [Min, Max]",
           !name == "Mean pre admission cns (SD)",
           !name == "Mean pre admission pns (SD)")

  # add revised pre-admission scores
  clinical_table <- rbind(clinical_table_wide, clinical_table_wide_pre_adm)

  return(clinical_table)

}

clean_clinical_tables <- function(clinical_table) {


  # create an empty list to save pre-processed variables
  processed_list <- list()

  # create a list of clinical variables to further pre-process
  vars_to_process <- unique(clinical_table$name)

  # for each variable, we will first remove the [min, max] from the median in order to convert the variable to numeric
  # then we will calculate summary statistics (mean, median, min, max, sd)
  for (i in vars_to_process) {
    mod_table <- clinical_table %>%
      rename("var" = name) %>%
      filter(var == i) %>%
      mutate(
        site = toupper(site),
        None = sub("(\\(.*|\\[.*)", "", None),
        Peripheral = sub("(\\(.*|\\[.*)", "", Peripheral),
        Central = sub("(\\(.*|\\[.*)", "", Central)
      ) %>%
      mutate(across(None:Central, as.numeric)) %>%
      as_tibble() %>%
      select(-site, -var) %>%
      summarise_all(list(
        mean = mean,
        median = median,
        min = min,
        max = max,
        sd = sd
      ),
      na.rm = TRUE
      )

    processed_list[[i]] <- mod_table
  }


  # We will calculate the overall mean and standard deviation of the median clinical variables recorded by each healthcare system
  clinical_table_compiled <- bind_rows(processed_list, .id = "Table 1") %>%
    filter(grepl("Median", `Table 1`)) %>%
    mutate(across(ends_with("mean"), ~ round(., 1))) %>%
    mutate(across(ends_with("sd"), ~ round(., 1))) %>%
    # get average of the medians
    mutate(
      None = glue("{None_mean} ({None_sd})"),
      Peripheral = glue("{Peripheral_mean} ({Peripheral_sd})"),
      Central = glue("{Central_mean} ({Central_sd})"),
      N = NA
    ) %>%
    select(`Table 1`, N, None, Peripheral, Central) %>%
    mutate(`Table 1` = gsub("\\[|\\]", " ", `Table 1`),
           `Table 1` = gsub("Min, Max", "(sd)", `Table 1`))

}

create_tableone <- function(demo_table, clinical_table) {

  # sum up the total counts of each demographic variable across healthcare systems
  tableOne_inter <- demo_table %>%
    group_by(variable, Demo_var, Demo_var_i) %>%
    summarise(across(
      starts_with("n_var"),
      function(x) sum(x, na.rm = TRUE)
    ), .groups = "drop")

  # order variables for appearance in table
  ordered_vars <- c(
    "all", "sex", "age_group", "race", "covid_discharged",
    "readmitted", "severity", "survival"
  )

  # specify the order of Table 1
  row_order <- c(
    "All Patients", "Female", "Male", "Unknown Sex",
    "0-2", "3-5", "6-11", "12-17", "18-25",
    "26-49", "50-69", "70-79", "80+", "Unknown Age",
    "American Indian", "Asian", "Black",
    "Hawaiian/Pacific Islander",
    "Hispanic/Latino", "White", "Other",
    "Median Elixhauser score  (sd) ",
    "Median pre admission cns  (sd) ",
    "Median pre admission pns  (sd) ",
    "Non-Severe", "Severe",
    "Median time to severe  (sd) ",
    "Alive", "Deceased",
    "Median time to death  (sd) ",
    "Discharged", "Not Discharged",
    "Median time to first discharge  (sd) ",
    "Not Readmitted", "Readmitted",
    "Median time to first readmission  (sd) ",
    "Median number of readmissions  (sd) "
  )

  # filter and order table by specified variables
  tableOne_raw <- tableOne_inter %>%
    # use 'sex' to calculate total counts (less likely to be masked from straification of smaller numbers such as would be the case if we chose 'age_group')
    filter(Demo_var == "sex") %>%
    summarise(across(where(is.numeric), sum)) %>%
    data.frame(
      variable = "all",
      Demo_var = "all",
      Demo_var_i = "All Patients",
      .
    ) %>%
    # add this dataframe back to original table one
    bind_rows(tableOne_inter) %>%
    mutate(
      Demo_var_i = if_else(variable == "sex.other", 'Unknown Sex', Demo_var_i),
      Demo_var_i = if_else(variable == "race.Other", 'Other', Demo_var_i),
      N = rowSums(across(where(is.numeric))),
      Demo_var = factor(Demo_var, levels = ordered_vars)
    ) %>%
    group_by(Demo_var) %>%
    mutate(across(
      starts_with("n_var"),
      function(x) {
        paste0(x, " (", round(x / sum(x, na.rm = TRUE) * 100, 1), "%", ")")
      }
    )) %>%
    ungroup() %>%
    arrange(Demo_var) %>%
    select(Demo_var_i, N, starts_with("n_var")) %>%
    `colnames<-`(gsub(
      x = names(.),
      pattern = "n_var_",
      replacement = ""
    )) %>%
    rename("Table 1" = Demo_var_i) %>%
    bind_rows(clinical_table) %>%
    slice(match(row_order, `Table 1`))

  # Add total patient counts to table 1
    tableOne = tableOne_raw %>%
    mutate(
      N = case_when(
        grepl("Median time to first discharge", `Table 1`) ~ get_table_n(df = tableOne_raw, "Discharged"),
        grepl("Median time to last discharge", `Table 1`) ~ get_table_n(df = tableOne_raw, "Discharged"),
        grepl("Median time to severe", `Table 1`) ~ get_table_n(df = tableOne_raw, "Severe"),
        grepl("death", `Table 1`) ~ get_table_n(df = tableOne_raw, "Deceased"),
        grepl("readmission", `Table 1`) ~ get_table_n(df = tableOne_raw, "Readmitted"),
        grepl("Median Elixhauser score", `Table 1`) ~ get_table_n(df = tableOne_raw, "All Patients"),
        grepl("Median pre admission cns", `Table 1`) ~ get_table_n(df = tableOne_raw, "All Patients"),
        grepl("Median pre admission pns", `Table 1`) ~ get_table_n(df = tableOne_raw, "All Patients"),
        TRUE ~ N
      )
    ) %>%
    select(`Table 1`, N, None, Central, Peripheral)

    return(tableOne)

}
