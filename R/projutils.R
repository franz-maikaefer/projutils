#' @import dplyr
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
