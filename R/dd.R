# Reshape the long, one-row-per-text-per-category output of
# distributional_density() into one row per text, with metric columns
# prefixed by category name when there is more than one category.
widen_scores <- function(scores) {
  categories <- unique(scores$category)
  metric_cols <- c("n_events", "prevalence", "burstiness", "burstiness_raw",
                    "position", "dispersion", "dispersion_continuous")

  if (length(categories) == 1L && identical(categories, "category")) {
    out <- scores[, c("id", "n_words", metric_cols)]
    rownames(out) <- NULL
    return(out)
  }

  base <- unique(scores[, c("id", "n_words")])
  pieces <- list(base)
  for (cat in categories) {
    sub <- scores[scores$category == cat, c("id", metric_cols)]
    names(sub)[-1] <- paste(cat, names(sub)[-1], sep = "_")
    pieces[[length(pieces) + 1L]] <- sub
  }
  out <- Reduce(function(x, y) merge(x, y, by = "id", sort = FALSE), pieces)
  rownames(out) <- NULL
  out
}

#' Score text with distributional density metrics
#'
#' Shorthand, data.frame-friendly wrapper around
#' [distributional_density()]. Where `distributional_density()`
#' returns a long table (one row per text-by-category combination),
#' `dd()` returns one row per text, with the four distributional
#' density parameters appended as columns -- either onto a data frame
#' you already have, or onto a new one built from a plain character
#' vector of texts.
#'
#' @param text Either a character vector of texts, or, if `data` is
#'   supplied, a single string naming the column of `data` that holds
#'   the text.
#' @param dictionary As in [distributional_density()]: a character
#'   vector for a single category, or a named list of character
#'   vectors to score several categories at once. Category names
#'   become column-name prefixes when more than one category is
#'   scored.
#' @param data Optional data frame to append scores to. If supplied,
#'   `text` (and `id`, if given) must name columns of `data`, and the
#'   return value is `data` with score columns appended. If omitted,
#'   `text` is used directly as the vector of texts and a new data
#'   frame is built from it.
#' @param id Optional text identifiers, used to align scores back onto
#'   `data`. If `data` is supplied, this can be a single string naming
#'   an id column in `data`. Defaults to row order, or to `names(text)`
#'   if `data` is not supplied and `text` is a named vector.
#' @param bound As in [dd_dispersion()]: `"finite"` (default) or
#'   `"continuous"`.
#' @param standardize As in [distributional_density()]: if `TRUE`,
#'   `burstiness` is a z-score against a simulated finite-size null
#'   rather than the raw Kim & Jo value, removing burstiness's
#'   residual dependence on occurrence count (and so on text length).
#'   Default `FALSE`.
#' @param n_sim Number of null placements to simulate per
#'   text-by-category combination when `standardize = TRUE`. Default
#'   1000.
#'
#' @return A data frame: `data` with score columns appended if `data`
#'   was supplied, otherwise a new data frame with `id`, `text`, and
#'   score columns.
#' @export
#'
#' @examples
#' texts <- c(
#'   "For me, the best part of the day is the morning coffee.",
#'   "The best part of the day, for me, is the morning coffee."
#' )
#' self_words <- c("i", "me", "my", "mine", "myself")
#'
#' # vector in, new data frame out
#' dd(text = texts, dictionary = self_words)
#'
#' # existing data frame in, same data frame out with scores appended
#' df <- data.frame(id = c("p1", "p2"), text = texts, stringsAsFactors = FALSE)
#' dd(text = "text", data = df, id = "id",
#'    dictionary = list(self = self_words, time = c("day", "morning")))
dd <- function(text, dictionary, data = NULL, id = NULL, bound = c("finite", "continuous"),
               standardize = FALSE, n_sim = 1000) {
  bound <- match.arg(bound)

  if (!is.null(data)) {
    if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
    if (!is.character(text) || length(text) != 1L || !(text %in% names(data))) {
      stop("When `data` is supplied, `text` must be a single string naming a column of `data`.",
           call. = FALSE)
    }
    text_vec <- data[[text]]
    if (!is.null(id) && is.character(id) && length(id) == 1L && id %in% names(data)) {
      id_vec <- data[[id]]
    } else if (!is.null(id)) {
      id_vec <- id
    } else {
      id_vec <- seq_len(nrow(data))
    }
    out <- data
  } else {
    if (!is.character(text)) {
      stop("`text` must be a character vector when `data` is not supplied.", call. = FALSE)
    }
    text_vec <- text
    id_vec <- if (!is.null(id)) {
      id
    } else if (!is.null(names(text_vec))) {
      names(text_vec)
    } else {
      seq_along(text_vec)
    }
    out <- data.frame(id = id_vec, text = unname(text_vec), stringsAsFactors = FALSE)
  }

  scores <- distributional_density(text_vec, dictionary, id = id_vec, bound = bound,
                                    standardize = standardize, n_sim = n_sim)
  wide <- widen_scores(scores)
  wide <- wide[match(id_vec, wide$id), , drop = FALSE]

  new_cols <- setdiff(names(wide), "id")
  out[new_cols] <- wide[new_cols]
  rownames(out) <- NULL
  out
}
