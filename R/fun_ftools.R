# # [SETUP] -----------------------------------------------------------------
# # - Packages --------------------------------------------------------------
# pkg <- c(
#   # 'dplyr', 'tidyr', 'stringr', 'purrr' #Data wrangling
#   'dplyr', 'tidyr', 'stringr', 'vctrs' #Data wrangling
#   , 'psych' #Fast factor scores
#   , 'purrr' #Data wrangling
#   , 'stats' #Factor loadings function
# )
#
# # Activate / install packages
# lapply(pkg, function(x)
#   if(!require(x, character.only = T))
#   {install.packages(x); require(x)})
#
# # Package citation
# # lapply(pkg, function(x)
# #   {citation(package = x)})
#

# [FUNCTIONS] -------------------------------------------------------------
# - Factor loadings -------------------------------------------------------
fun_ftools_loadings <- function(efa_model){

  # Arguments validation
  stopifnot(
    "'efa_model' must be a factor analysis object." =
      any(
        class(efa_model) == 'factanal'
        , class(efa_model) == 'fa'
        , class(efa_model) == 'principal'
      )
  )

  # Get factor loadings
  loadings(efa_model)[,] %>%
    as_tibble(
      rownames = 'item'
    ) -> df_loadings

  # Data wrangling
  if(ncol(df_loadings) == 2){

    df_loadings %>%
      rename(
        factor1 = 2
      ) -> df_loadings

  }

  df_loadings %>%
    rename_with(
      .cols = -item
      , .fn = ~ .x %>%
        str_extract(
          '[[:digit:]]+'
        ) %>%
        paste0('factor', .)
    ) %>%
    relocate(
      item
      , str_sort(
        names(.)
        , numeric = T
      )
    ) -> df_loadings

  # Add loadings class
  df_loadings %>%
    new_data_frame(
      class = c(
        'df_loadings'
        , 'tbl'
      )
    ) -> df_loadings
  # structure(
  #   class = c(
  #     class(.)
  #     , 'list'
  #     , 'df_loadings'
  #   )
  # ) -> df_loadings

  # Output
  return(df_loadings)

}

# - Factor loadings match ----------------------------------------------------------
fun_ftools_factor_match <- function(efa_model){

  # Arguments validation with fun_ftools_loadings

  # Get factor loadings from efa_model
  fun_ftools_loadings(
    efa_model
  ) -> df_loadings

  rm(efa_model)

  # Match items to factors by max loading
  df_loadings %>%
    pivot_longer(
      cols = -item
      , names_to = 'factor'
      , values_to = 'loading'
    ) %>%
    relocate(factor) %>%
    group_by(item) %>%
    mutate(
      crossloadings =
        sum(loading) -
        max(loading)
    ) %>%
    filter(
      loading ==
        max(loading)
    ) %>%
    ungroup() %>%
    # Manually add factors that don't have any items
    bind_rows(
      tibble(
        factor =
          setdiff(
            names(df_loadings[-1])
            , unique(.$factor)
          )
      )
    ) %>%
    mutate(
      factor =
        factor(
          factor
          , levels = names(
            df_loadings[-1]
          )
        )
    ) %>%
    arrange(factor) %>%
    new_data_frame(
      class = c(
        'df_factor_match'
        , 'tbl'
      )
    ) -> df_factor_match
  # structure(
  #   class = c(
  #     class(.)
  #     , 'df_factor_match'
  #     , 'list'
  #   )
  # ) -> df_factor_match

  # Output
  return(df_factor_match)

}

# - Factor scores ---------------------------------------------------------
fun_ftools_factor_scores <- function(
    df_data
    , efa_model
    , lgc_pivot = F
){

  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame containing item scores." =
      all(
        is.data.frame(df_data)
        , any(
          loadings(efa_model)[,] %>%
            rownames() %in%
            names(df_data)
        )))

  # Data wrangling
  fun_ftools_factor_match(
    efa_model
  ) -> df_factor_match

  # Factor keys data frame
  df_data %>%
    select(any_of(
      df_factor_match$item
    )) -> df_data_factors

  df_factor_match %>%
    filter(
      item %in% names(
        df_data_factors
      )
    ) -> df_factor_match

  df_data %>%
    select(!any_of(
      df_factor_match$item
    )) -> df_data

  # Create factor keys list
  df_factor_match %>%
    split(.$factor) %>%
    map(~ pull(.x, item)) ->
    list_factors

  rm(df_factor_match)

  # Score items
  if(nrow(df_data_factors) == 1){

    scoreVeryFast(
      keys = list_factors
      , items = rbind(
        df_data_factors,
        df_data_factors
      )
      , totals = F
    ) %>%
      as_tibble() %>%
      slice_head(n = 1) ->
      df_factor_scores

  } else {

    scoreVeryFast(
      keys = list_factors
      , items = df_data_factors
      , totals = F
    ) %>%
      as_tibble() ->
      df_factor_scores

  }

  # Add id columns to data
  df_data %>%
    bind_cols(
      df_factor_scores
    ) %>%
    relocate(
      !starts_with('factor')
      , str_sort(
        names(.)
        , numeric = T
      )
    ) -> df_factor_scores

  # Add df_factor_scores class
  df_factor_scores %>%
    new_data_frame(
      class = c(
        'df_factor_scores'
        , 'tbl'
      )
    ) -> df_factor_scores

  # Aggregate results
  if(lgc_pivot){

    df_factor_scores %>%
      pivot_longer(
        cols = starts_with('factor')
        , names_to = 'factor'
        , values_to = 'factor_score'
      ) -> df_factor_scores

    # Remove df_factor_scores class
    # Add df_factor_scores_long class
    df_factor_scores %>%
      new_data_frame(
        class = c(
          'df_factor_scores_long'
          , 'tbl'
        )
      ) -> df_factor_scores

  }

  # Output
  return(df_factor_scores)

}

# # [TEST] ------------------------------------------------------------------
# # - Data ------------------------------------------------------------------
# library(readr)
#
# # read_rds(
# read_rds(
#   'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
# ) -> efa_model
#
# read_csv(
#   'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
# ) -> df_occupations
#
# # - Test factor loadings ------------------------------------------------------------------
# fun_ftools_loadings(efa_model)
#
# # - Test factor loadings match ------------------------------------------------------------------
# fun_ftools_factor_match(efa_model)
#
# # - Test factor scores 1 ----------------------------------------------------
# fun_ftools_factor_scores(
#   df_data =
#     df_occupations
#   , efa_model =
#     efa_model
#   , lgc_pivot = F
# )
#
# # - Test factor scores 2 ----------------------------------------------------
# fun_ftools_factor_scores(
#   df_data =
#     df_occupations %>%
#     slice(1)
#   , efa_model =
#     efa_model
#   , lgc_pivot = F
# )
