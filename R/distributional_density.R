#' Compute distributional density metrics for text
#'
#' Scores one or more texts against one or more word-list categories
#' and returns all four distributional density parameters --
#' prevalence, burstiness, position, and dispersion -- for each
#' text-by-category combination.
#'
#' @param text A character vector of texts. Can be a single string or
#'   many; each is scored independently.
#' @param dictionary Either a character vector of words/stems
#'   defining a single category (e.g. `c("i", "me", "my", "mine",
#'   "myself")`), or a named list of such vectors to score multiple
#'   categories at once (e.g. `list(self = c("i", "me", ...), negemo =
#'   c("sad", "angry", ...))`). Entries ending in `"*"` match by
#'   prefix (e.g. `"happi*"` matches "happy", "happiness", "happier");
#'   matching is otherwise exact and case-insensitive.
#' @param id Optional vector of text identifiers, recycled to
#'   `length(text)`. Defaults to `1:length(text)`, or to `names(text)`
#'   if `text` is named.
#' @param bound Which upper bound to normalize dispersion against; see
#'   [dd_dispersion()]. Defaults to `"finite"`.
#' @param standardize One of `"none"` (default), `"z"`, or `"center"`.
#'   If not `"none"`, the `burstiness` column holds the observed value
#'   compared against a simulated finite-size null instead of the raw
#'   Kim & Jo value; see [dd_burstiness()] for the difference between
#'   `"z"` (an inferential statistic, confounded with occurrence count
#'   for genuinely bursty categories) and `"center"` (an effect size,
#'   comparable across texts of different lengths and occurrence
#'   counts). `burstiness_raw` is never standardized.
#' @param n_sim Number of null placements to simulate per
#'   text-by-category combination when `standardize` is `"z"` or
#'   `"center"`. Default 1000.
#'
#' @return A data frame with one row per text-by-category combination
#'   and columns `id`, `category`, `n_words`, `n_events`, `prevalence`,
#'   `burstiness`, `burstiness_raw`, `position`, `dispersion`, and
#'   `dispersion_continuous`. `burstiness` is `NA` for categories with
#'   fewer than 3 occurrences in a text, and `dispersion`/
#'   `dispersion_continuous` are `NA` for fewer than 2.
#' @export
#'
#' @examples
#' texts <- c(
#'   "For me, the best part of the day is the morning coffee.",
#'   "The best part of the day, for me, is the morning coffee.",
#'   "The best part of the day is the morning coffee, for me."
#' )
#' self_words <- c("i", "me", "my", "mine", "myself")
#' distributional_density(texts, self_words)
#'
#' distributional_density(
#'   texts,
#'   list(self = self_words, time = c("day", "morning"))
#' )
distributional_density <- function(text, dictionary, id = NULL, bound = c("finite", "continuous"),
                                    standardize = c("none", "z", "center"), n_sim = 1000) {
  bound <- match.arg(bound)
  standardize <- match.arg(standardize)
  if (!is.character(text) || length(text) < 1) {
    stop("`text` must be a non-empty character vector.", call. = FALSE)
  }
  if (is.null(id)) {
    id <- if (!is.null(names(text))) names(text) else seq_along(text)
  }
  if (length(id) != length(text)) {
    stop("`id` must be the same length as `text`.", call. = FALSE)
  }
  dictionary <- as_dictionary_list(dictionary)

  rows <- list()
  for (i in seq_along(text)) {
    tokens <- dd_tokenize(text[[i]])
    n_words <- length(tokens)
    for (cat_name in names(dictionary)) {
      is_match <- match_category(tokens, dictionary[[cat_name]])
      positions <- which(is_match)
      k <- length(positions)

      rows[[length(rows) + 1L]] <- data.frame(
        id                    = id[[i]],
        category              = cat_name,
        n_words               = n_words,
        n_events              = k,
        prevalence            = if (n_words > 0) dd_prevalence(k, n_words) else NA_real_,
        burstiness            = dd_burstiness(positions, method = "kj", standardize = standardize,
                                               n_words = n_words, n_sim = n_sim),
        burstiness_raw        = dd_burstiness(positions, method = "raw"),
        position              = dd_position(positions, n_words),
        dispersion            = dd_dispersion(positions, n_words, bound = "finite"),
        dispersion_continuous = dd_dispersion(positions, n_words, bound = "continuous"),
        stringsAsFactors      = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (bound == "continuous") {
    out$dispersion <- out$dispersion_continuous
  }
  out
}
