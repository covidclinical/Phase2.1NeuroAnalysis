# create a function to back-transform the pre-admission scores which were computed as log(z + 1)
# where z is the total number of pre CNS or PNS codes
transform_count <- function(z) {

  exp(z) - 1

}

# define function to count total number of patients
get_table_n <- function(df, var) {
  df = df %>%
    filter(`Table 1` == var) %>%
    pull(N)

  if(is_empty(df)) {
    df = 0
  }
  return(df)
}

# calculate chi.square tests
calc_chisq <- function(var, tableOne_sums, none_n, pns_n, cns_n) {

  # define matrix for each variable
  mat <- tableOne_sums %>%
    filter(variable == paste(var)) %>%
    select(starts_with("n_var")) %>%
    mutate(none_dif = none_n - n_var_None,
           pns_dif = pns_n - n_var_Peripheral,
           cns_dif = cns_n - n_var_Central) %>%
    # due to blurring, sometimes the numbers can be < 0. We will convert to 0
    mutate(none_dif = if_else(none_dif < 0, 0, none_dif),
           pns_dif = if_else(pns_dif < 0, 0, pns_dif),
           cns_dif = if_else(cns_dif < 0, 0, cns_dif)) %>%
    as.integer() %>%
    matrix(nrow = 2, ncol = 3, byrow = TRUE)


  result <- chisq.test(unlist(mat))

}
