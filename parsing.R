parse_names <- function(x, parameters) {
  the_levels <- rep(NA_character_, length(parameters))
  names(the_levels) <- parameters
  if ('(Intercept)' %in% x) return(as.list(the_levels))
  for (p in x) {
    the_levels[substr(x, 1, nchar(x)-2)] <- substr(x, nchar(x)-1, nchar(x))
  }  
  as.list(the_levels)
}

fill_baseline <- function(the_levels, level_combinations) {
  baseline <- list()
  for (x in colnames(level_combinations)) {
    values <- unique(level_combinations[[x]])
    baseline[[x]] <- values[! values %in% unique(the_levels[[x]])]
  }
  list(the_levels %>% replace_na(baseline),
       baseline)
}

parse_estimates <- function(the_fit, measurements) {
  estimates <- data.frame(estimate=lme4::fixef(the_fit)) %>%
    rownames_to_column('parameter')
  level_combinations <- measurements %>% select(-samp, -acc) %>% unique()
  res <- fill_baseline(
    bind_rows(
      lapply(
        X=strsplit(estimates$parameter, ':'),
        FUN = function(x) parse_names(x,
                                      colnames(level_combinations)))),
    level_combinations)
  list(parsed_estimates = res[[1]] %>% 
         bind_cols(estimates %>% select(estimate)),
       baselines = res[[2]])
}

calculate_estimates <- function(the_levels, parsed_estimates) {
  criteria <- the_levels[the_levels %in% parsed_estimates$baselines]
  if (ncol(criteria) == 0)
    return(sum(parsed_estimates$parsed_estimates$estimate))
  suppressMessages(right_join(parsed_estimates$parsed_estimates, criteria)) %>%
    pull(estimate) %>% sum()
}

calculate_all_estimates <- function(all_levels, parsed_estimates) {
  estimates <- as.numeric(by(all_levels, 1:nrow(all_levels),
                             function(x) calculate_estimates(x, parsed_estimates)))
  all_levels %>% mutate(estimate = estimates)
}

obtain_estimates <- function(the_fit, measurements) {
  calculate_all_estimates(
    measurements %>% select(-samp, -acc) %>% unique(),
    parse_estimates(the_fit, measurements))
}