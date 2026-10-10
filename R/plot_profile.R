#' Add the bookkeeping columns used by the review modules
#'
#' @param data A mapped NMPK dataset.
#' @return `data` with `.rowid` (row number, matching the check results) and
#'   `.group` (see [obs_group()]).
#' @noRd
add_review_columns <- function(data) {
  data[[".rowid"]] <- seq_len(nrow(data))
  data[[".group"]] <- obs_group(data)
  data
}

#' Time after the most recent dose
#'
#' @param id Subject identifier.
#' @param time Numeric time.
#' @param is_dose Logical; `TRUE` on dose records.
#' @return Time since the subject's latest dose record at or before each
#'   record; `NA` before the first dose. Doses implied by `ADDL` are not
#'   expanded.
#' @noRd
time_after_dose <- function(id, time, is_dose) {
  out <- rep(NA_real_, length(time))
  for (idx in split(seq_along(time), id)) {
    idx <- idx[order(time[idx], !is_dose[idx])]
    dose_time <- ifelse(is_dose[idx], time[idx], -Inf)
    dose_time[is.na(dose_time)] <- -Inf
    last <- cummax(dose_time)
    out[idx] <- ifelse(is.finite(last), time[idx] - last, NA_real_)
  }
  out
}

#' Shared look of the review plots
#'
#' @return A list of ggplot components.
#' @noRd
theme_review <- function() {
  list(
    ggplot2::theme_minimal(base_size = 11),
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey40"),
      plot.caption = ggplot2::element_text(color = "grey40"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
  )
}

#' Colour-blind safe discrete colour scale
#'
#' @param values The values mapped to colour.
#' @return An Okabe-Ito colour scale, or `NULL` (the ggplot2 default) when
#'   there are more levels than colours.
#' @noRd
scale_color_review <- function(values) {
  palette <- c(
    "#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9",
    "#999999", "#000000"
  )
  if (length(unique(values)) > length(palette)) {
    return(NULL)
  }
  ggplot2::scale_color_manual(values = palette)
}

#' Concentration-time profile of one subject
#'
#' @param sub The subject's records, with `TIME`, `DV`, `EVID`, `.group`, and a
#'   logical `flagged` column.
#' @param subject Subject identifier, for the title.
#' @param log_y Whether to draw the concentration axis on a log scale.
#' @return A ggplot, or `NULL` when the subject has no observation to plot.
#' @importFrom rlang .data
#' @noRd
plot_subject_profile <- function(sub, subject, log_y = TRUE) {
  sub$TIME <- suppressWarnings(as.numeric(sub$TIME))
  sub$DV <- suppressWarnings(as.numeric(sub$DV))
  evid <- suppressWarnings(as.numeric(sub$EVID))
  obs <- sub[evid %in% 0 & !is.na(sub$DV), , drop = FALSE]
  doses <- sub[evid %in% c(1, 4), , drop = FALSE]
  if (nrow(obs) == 0) {
    return(NULL)
  }
  dropped <- sum(obs$DV <= 0)
  if (log_y) {
    obs <- obs[obs$DV > 0, , drop = FALSE]
  }
  grouped <- length(unique(obs$.group)) > 1

  p <- ggplot2::ggplot(obs, ggplot2::aes(x = .data$TIME, y = .data$DV))
  if (nrow(doses) > 0) {
    p <- p + ggplot2::geom_vline(
      data = doses, ggplot2::aes(xintercept = .data$TIME),
      linetype = "dashed", color = "grey60"
    )
  }
  p <- p + if (grouped) {
    list(
      ggplot2::geom_line(ggplot2::aes(color = .data$.group)),
      ggplot2::geom_point(ggplot2::aes(color = .data$.group), size = 2),
      scale_color_review(obs$.group)
    )
  } else {
    list(
      ggplot2::geom_line(color = "#0072B2"),
      ggplot2::geom_point(color = "#0072B2", size = 2)
    )
  }
  p <- p +
    ggplot2::geom_point(
      data = obs[obs$flagged, , drop = FALSE],
      shape = 21, size = 5, stroke = 1.2, color = "#dc2626"
    ) +
    ggplot2::labs(
      x = "TIME", y = "DV", color = NULL,
      title = paste("Subject", subject),
      subtitle = "Red circle: flagged observation. Dashed line: dose.",
      caption = if (log_y && dropped > 0) {
        paste0(
          dropped, " BLQ / non-positive point(s) omitted on the log scale."
        )
      }
    ) +
    theme_review()
  if (log_y) {
    p <- p + ggplot2::scale_y_log10()
  }
  p
}

#' Concentration-time overview of all subjects
#'
#' One line per subject, split into panels by analyte / compartment.
#'
#' @param data A mapped NMPK dataset.
#' @param time_axis `"first"` for time since first dose (`TIME`) or `"last"`
#'   for time after the most recent dose.
#' @param color_by Name of a column to colour by, `"dose"` for the subject's
#'   first dose amount, or `""` for no colouring.
#' @param log_y Whether to draw the concentration axis on a log scale.
#' @return A ggplot, or `NULL` when there is no observation to plot.
#' @importFrom rlang .data
#' @noRd
plot_population_profile <- function(data, time_axis = "first", color_by = "",
                                    log_y = TRUE) {
  evid <- suppressWarnings(as.numeric(data$EVID))
  time <- suppressWarnings(as.numeric(data$TIME))
  dv <- suppressWarnings(as.numeric(data$DV))
  id <- as.character(data$ID)
  is_dose <- evid %in% c(1, 4)

  d <- data.frame(
    id = id, time = time, dv = dv, group = obs_group(data),
    stringsAsFactors = FALSE
  )
  if (time_axis == "last") {
    d$time <- time_after_dose(id, time, is_dose)
  }
  d$color <- if (color_by == "dose") {
    amt <- suppressWarnings(as.numeric(data$AMT))
    first <- which(is_dose & !duplicated(ifelse(is_dose, id, NA)))
    dose <- as.character(amt[first][match(id, id[first])])
    ifelse(is.na(dose), "no dose", dose)
  } else if (nzchar(color_by)) {
    as.character(data[[color_by]])
  } else {
    NA_character_
  }
  d <- d[evid %in% 0 & !is.na(dv) & !is.na(d$time), , drop = FALSE]
  if (log_y) {
    d <- d[d$dv > 0, , drop = FALSE]
  }
  if (nrow(d) == 0) {
    return(NULL)
  }
  d <- d[order(d$id, d$time), ]

  colored <- !all(is.na(d$color))
  mapping <- if (colored) {
    ggplot2::aes(
      x = .data$time, y = .data$dv,
      group = interaction(.data$id, .data$group), color = .data$color
    )
  } else {
    ggplot2::aes(
      x = .data$time, y = .data$dv, group = interaction(.data$id, .data$group)
    )
  }
  p <- ggplot2::ggplot(d, mapping) +
    ggplot2::geom_line(alpha = 0.35) +
    ggplot2::geom_point(size = 0.8, alpha = 0.5) +
    ggplot2::labs(
      x = if (time_axis == "last") "Time after dose" else "TIME",
      y = "DV", color = NULL
    ) +
    theme_review()
  if (colored) {
    p <- p + scale_color_review(d$color) + ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(alpha = 1))
    )
  }
  if (length(unique(d$group)) > 1) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$group), scales = "free_y")
  }
  if (log_y) {
    p <- p + ggplot2::scale_y_log10()
  }
  p
}
