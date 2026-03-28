# =============================================================================
# deskR: Funciones auxiliares para datos de desk.carlosdeanta.net
# Econometria del Riesgo Financiero
# =============================================================================
#
# Autor: Carlos de Anta Puig
#        carlos@cwconsultores.com
#
# Los datos se descargan manualmente desde https://desk.carlosdeanta.net
# en formato XLSX y se guardan en la carpeta data/ del proyecto.
#
# Uso:
#   source("scripts/deskR.R")
#   tef <- desk_read("TEF_MC")
#   ret <- desk_returns("TEF_MC", type = "log")
#
# =============================================================================

library(readxl)
library(readr)
library(tibble)
library(dplyr)

# -----------------------------------------------------------------------------
# Configuracion
# -----------------------------------------------------------------------------

.desk_data_dir <- function() {
  getOption("desk.data_dir", default = "data")
}

# -----------------------------------------------------------------------------
# Lectura de datos OHLCV
# -----------------------------------------------------------------------------

#' Leer datos OHLCV descargados de desk.carlosdeanta.net
#' Busca un fichero XLSX o CSV en data/ cuyo nombre contenga el ticker.
#' @param ticker Nombre del fichero sin extension. Ej: "TEF_MC", "EURUSD"
#' @param from Fecha inicio (opcional, filtra despues de leer)
#' @param to Fecha fin (opcional)
#' @return tibble con date, open, high, low, close, volume
desk_read <- function(ticker, from = NULL, to = NULL) {
  dir <- .desk_data_dir()
  xlsx_path <- file.path(dir, paste0(ticker, ".xlsx"))
  csv_path  <- file.path(dir, paste0(ticker, ".csv"))

  if (file.exists(xlsx_path)) {
    df <- read_excel(xlsx_path) |> as_tibble()
  } else if (file.exists(csv_path)) {
    df <- read_csv(csv_path, show_col_types = FALSE)
  } else {
    stop(paste0(
      "No se encuentra '", ticker, ".xlsx' ni '", ticker, ".csv' en ", dir, "/\n",
      "Descarga los datos desde https://desk.carlosdeanta.net"
    ))
  }

  names(df) <- tolower(names(df))
  if ("fecha" %in% names(df)) df <- rename(df, date = fecha)
  if ("apertura" %in% names(df)) df <- rename(df, open = apertura)
  if ("maximo" %in% names(df)) df <- rename(df, high = maximo)
  if ("minimo" %in% names(df)) df <- rename(df, low = minimo)
  if ("cierre" %in% names(df)) df <- rename(df, close = cierre)
  if ("volumen" %in% names(df)) df <- rename(df, volume = volumen)

  df <- df |> mutate(date = as.Date(date)) |> arrange(date)
  if (!is.null(from)) df <- df |> filter(date >= as.Date(from))
  if (!is.null(to))   df <- df |> filter(date <= as.Date(to))
  df
}

# -----------------------------------------------------------------------------
# Rendimientos
# -----------------------------------------------------------------------------

#' Calcular rendimientos a partir de datos OHLCV
#' @param ticker Nombre del fichero de datos
#' @param from Fecha inicio
#' @param to Fecha fin
#' @param type "log" (default) o "simple"
#' @return tibble con date, return
desk_returns <- function(ticker, from = NULL, to = NULL, type = "log") {
  df <- desk_read(ticker, from, to)
  if (type == "log") {
    df <- df |> mutate(return = c(NA_real_, diff(log(close))))
  } else {
    df <- df |> mutate(return = c(NA_real_, diff(close) / head(close, -1)))
  }
  df |> filter(!is.na(return)) |> select(date, return)
}

# -----------------------------------------------------------------------------
# Precios de cierre de multiples activos
# -----------------------------------------------------------------------------

#' Leer precios de cierre de varios activos y unir por fecha
#' @param tickers Vector de nombres de ficheros
#' @param from Fecha inicio
#' @param to Fecha fin
#' @return tibble ancho: date, ticker1, ticker2, ...
desk_close <- function(tickers, from = NULL, to = NULL) {
  dfs <- lapply(tickers, function(tk) {
    desk_read(tk, from, to) |>
      select(date, close) |>
      rename(!!tk := close)
  })
  result <- dfs[[1]]
  for (i in seq_along(dfs)[-1]) {
    result <- full_join(result, dfs[[i]], by = "date")
  }
  result |> arrange(date)
}

# -----------------------------------------------------------------------------
# Matriz de rendimientos
# -----------------------------------------------------------------------------

#' Matriz de rendimientos logaritmicos de varios activos
#' @param tickers Vector de nombres de ficheros
#' @param from Fecha inicio
#' @param to Fecha fin
#' @return tibble ancho: date, ticker1, ticker2, ...
desk_returns_matrix <- function(tickers, from = NULL, to = NULL) {
  dfs <- lapply(tickers, function(tk) {
    desk_returns(tk, from, to, type = "log") |>
      rename(!!tk := return)
  })
  result <- dfs[[1]]
  for (i in seq_along(dfs)[-1]) {
    result <- inner_join(result, dfs[[i]], by = "date")
  }
  result |> arrange(date)
}

# -----------------------------------------------------------------------------
# Utilidades
# -----------------------------------------------------------------------------

#' Listar ficheros de datos disponibles en data/
desk_list <- function() {
  dir <- .desk_data_dir()
  files <- list.files(dir, pattern = "\\.(xlsx|csv)$", ignore.case = TRUE)
  tibble(
    file = files,
    ticker = tools::file_path_sans_ext(files),
    size_mb = round(file.size(file.path(dir, files)) / 1e6, 2)
  )
}

cat("[deskR] Funciones cargadas. Datos en:", .desk_data_dir(), "/\n")
cat("[deskR] Descarga datos desde https://desk.carlosdeanta.net\n")
cat("[deskR] Uso: desk_read('TEF_MC') o desk_returns('TEF_MC')\n")
