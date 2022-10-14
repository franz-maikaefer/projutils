#' @import dplyr, ggplot2
NULL

#' @export
glued_par <- function(param, env) {
  stringr::str_glue(param, .envir = env) %>% as.character()
}

#' @export
ofile <- function(filename, base_dir = BASE_DIR, .envir = parent.frame()) {
  glued_par(filename, env = .envir) %>% file.path(base_dir, .)
}

#' @export
write_rds <- function(x, file) readr::write_rds(x, ofile(file, .envir = parent.frame()))

#' @export
read_rds <- function(file) readr::read_rds(ofile(file, .envir = parent.frame()))

#' @export
lst_cache_dt_for <- function(...) {
  vars_vector <- sapply(rlang::enquos(...), rlang::as_name)
  cache_file_pattern <- "(.*)_(\\d{4}\\-\\d{2}\\-\\d{2}).rds"
  cache_files <- list.files(pattern = cache_file_pattern, path = BASE_DIR)
  matches <-
    stringr::str_match(cache_files, cache_file_pattern) %>%
    as_tibble(.name_repair = "minimal")
  colnames(matches) <- c("file", "var", "dt")
  matches <- mutate(matches, dt = lubridate::ymd(dt))
  not_found <- setdiff(vars_vector, unique(matches$var))
  if(length(not_found) != 0) {
    stop("Following vars not found in variables caches: ", not_found)
  }
  matches %>%
    filter(var %in% vars_vector) %>%
    group_by(var) %>%
    summarise(max_dt = max(dt)) %>%
    summarise(lst_dt = min(max_dt)) %>%
    pull()
}

#' @export
greatest_common_dt_range_for <- function(...) {
  lapply(list(...), function(df) {
    df_dt_range <- range(df$DATA)
    names(df_dt_range) <- c("min_dt", "max_dt")
    df_dt_range
  }) %>%
    bind_rows() %>%
    summarise(min = max(min_dt), max = min(max_dt))
}

#' @export
isnt_na <- function(x) {
  !is.na(x)
}

#' @export
sum_across <- function(g_df, valores) {
  g_df %>%
    summarise(across({{ valores }}, ~sum(.x, na.rm = TRUE)), .groups = "drop")
}

#' @export
soma <- function(df, por, valores) {
  df %>% group_by({{ por }}) %>% sum_across({{ valores }})
}

#' @export
sum_qtde_and_valor_by <- function(df, ...) {
  df %>% group_by(...) %>% sum_across(c(QTDE, VALOR))
}

convert_chars_to_encoding_without_invalid <- function(df, dest_encoding) {
  df %>%
    mutate(across(where(is.character),
                  ~ iconv(., to = dest_encoding, sub = "")))
}

#' @export
convert_chars_to_unicode_without_invalid <- function(df) {
  df %>%
    convert_chars_to_encoding_without_invalid(dest_encoding = "UTF-8")
}

#' @export
convert_chars_to_ascii_without_invalid <- function(df) {
  df %>%
    convert_chars_to_encoding_without_invalid(dest_encoding = "ASCII//TRANSLIT")
}

#' @export
add_quotient_between_cols <- function(to, col, over,
                                      name_pattern = "{col_nm}_over_{over_nm}") {
  col_nm <- rlang::as_name(rlang::enquo(col))
  over_nm <- rlang::as_name(rlang::enquo(over))
  quotient_col_nm <- stringr::str_glue(name_pattern)

  to %>%
    mutate(
      "{quotient_col_nm}" := {{ col }}/{{ over }}
    ) %>%
    relocate(all_of(quotient_col_nm), .after = {{col}})
}

#' @export
add_quotients_between <- function(.data, ..., over,
                                  name_pattern = "{col_nm}_over_{over_nm}") {
  loc <- tidyselect::eval_select(
    expr(c(...)),
    data = .data
  )
  loc <- vctrs::vec_as_location(loc, n = ncol(.data), names = names(.data))
  cols <- rlang::syms(names(.data)[loc])

  purrr::reduce(cols, function(df_i, col) {
    df_i %>% add_quotient_between_cols({{ col }}, {{ over }},
                                       name_pattern = name_pattern)
  },
  .init = .data
  )
}

#' @export
display_error_as_plot <- function(e) {
  p <- tibble::tibble(x = 0, y = 10, label = "Error") %>%
    ggplot(aes(x, y, label = label)) +
    geom_text(color = "red", size = 12) +
    theme_void()
  plotly::ggplotly(p)
}

#' @export
create_plot_for_each_nested_df_in <- function(data, .fns) {
  trelliscopejs::map_plot(
    data,
    function(df) {
      tryCatch(.fns(df), error = display_error_as_plot)
    }
  )
}

#' @export
error_proof_density <- function(x) {
  tryCatch(
    {
      dens <- density(x, na.rm = TRUE)
      dens$x[which.max(dens$y)]
    },
    error=function(cond) {
      NA
    },
    warning=function(cond) {
      NA
    }
  )
}

#' @export
em_reais <- scales::label_dollar(prefix = "R$ ",
                                 big.mark = ".", decimal.mark = ",")

#' @export
em_potencias_de_real <-
  scales::label_dollar(prefix = "R$ ",scale_cut = scales::cut_short_scale())

#' @export
fmt_br_dt <- function(dt) {
  format(dt, "%d/%m/%Y")
}

#' @export
corr_plot_for_vars_above <- function(df, threshold, reference, renamer = identity) {
  reference <- renamer(reference)

  cor_ratioVars <-
    df %>%
    rename_with(.fn = renamer) %>%
    stats::cor(., use="pairwise.complete.obs") #correlations of all ratio variables

  cor_sorted <- as.matrix(sort(cor_ratioVars[, reference], decreasing = TRUE))
  # select only high correlations
  cor_to_show <-
    names(which(apply(cor_sorted, 1, function(x) abs(x) > threshold)))
  cor_ratioVars_to_show <- cor_ratioVars[cor_to_show, cor_to_show]

  corrplot::corrplot.mixed(cor_ratioVars_to_show,
                 tl.col="black", tl.pos = "lt", tl.cex = 0.7, tl.srt = 45)

  grDevices::recordPlot()
}
