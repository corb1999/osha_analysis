
# add functions to execute analysis with ::::::::::::::::::

# simple aggregation calculation 
fn_calc_agg <- function(arg_gvar, arg_df = dfplt) {
  arg_df <- arg_df |> rename(gvar = !!arg_gvar) |> 
    group_by(gvar) |> 
    summarise(recs = n(), 
              n_establishments = n_distinct(establishment_id, 
                                            na.rm = TRUE), 
              employees_sum = sum(annual_average_employees, na.rm = TRUE), 
              hours_worked_sum = sum(total_hours_worked, na.rm = TRUE), 
              deaths_sum = sum(total_deaths, na.rm = TRUE), 
              injuries_sum = sum(total_injuries, na.rm = TRUE), 
              illnesses_sum = sum(total_illnesses, na.rm = TRUE)) |> 
    mutate(inj_ill_sum = injuries_sum + illnesses_sum, 
           incidence_rate = (inj_ill_sum * 200000) / hours_worked_sum)
  return(arg_df)
}
# tests ????????????????????????????
# fn_calc_agg(arg_gvar = 'year_filing_for')

# histogram simple plot
fn_plt_hist1 <- function(arg_df = dfplt, 
                         arg_pltnm = pltname, 
                         arg_metric = 'annual_average_employees', 
                         arg_metric_cap = 500, 
                         arg_metric_min = 0) {
  arg_df <- arg_df |> 
    rename(metric = !!arg_metric) |> 
    filter(!is.na(metric), 
           metric > arg_metric_min) |> 
    mutate(metric_cap = ifelse(metric > arg_metric_cap, 
                               arg_metric_cap, 
                               metric))
  p1 <- arg_df |> 
    ggplot(aes(x = metric_cap)) + 
    geom_histogram(color = 'white', alpha = 0.75, bins = 30) + 
    geom_vline(aes(xintercept = median(metric)), 
               color = 'blue') + 
    labs(x = arg_metric, y = 'N')
  p2 <- arg_df |> 
    ggplot(aes(x = metric_cap)) + 
    stat_ecdf(geom = 'step') + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    labs(x = arg_metric, y = '')
  p1 <- my_gg(p1)
  p2 <- my_gg(p2)
  p3 <- p1 + p2 + 
    plot_annotation(title = arg_pltnm)
  return(p3)
}
# tests ???????????????????????????????????????
# fn_plt_hist1()
# fn_plt_hist1(arg_metric = 'incidence_rate', arg_metric_cap = 30)

# table function for worst incident rates
fn_tbl_incidents <- function(arg_df = dfplt, 
                             arg_pltnm = pltname, 
                             arg_emp_min = 500, 
                             arg_hrs_min = 500000, 
                             arg_return_n = 25) {
  aa <- arg_df |> 
    filter(annual_average_employees >= arg_emp_min, 
           total_hours_worked >= arg_hrs_min) |> 
    mutate(estab_id = establishment_id)
  zz <- aa |> 
    select(establishment_id, establishment_name) |> 
    rename(gvar = establishment_id) |> 
    mutate(dedupe_key = paste0(gvar, ':::', establishment_name))
  zz <- stringr::str_split(unique(zz$dedupe_key), 
                           pattern = ':::', n = 2, simplify = TRUE) |> 
    data.frame() |> 
    rename(gvar = X1, establishment_name = X2) |> 
    mutate(gvar = as.integer(gvar))
  aa <- fn_calc_agg(arg_gvar = 'estab_id', 
                    arg_df = aa) |> 
    arrange(desc(incidence_rate))
  aa <- aa[c(1:arg_return_n), ]
  bb <- left_join(aa, zz, by = 'gvar')
  return(bb)
}
# tests ???????????????????????????????????????
# fn_tbl_incidents() |> View()
