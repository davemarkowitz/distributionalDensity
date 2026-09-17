# Population standard deviation (denominator n, not n - 1). Every
# distributional density parameter is defined on the observed
# occurrences themselves, not a sample estimate of a larger
# population, so the population form is used throughout.
pop_sd <- function(x) sqrt(mean((x - mean(x))^2))

#' Tokenize text into words
#'
#' Splits text on runs of characters that are not letters or
#' apostrophes, so contractions and possessives (`"don't"`, `"mine's"`)
#' stay intact as single tokens. Empty tokens produced by leading,
#' trailing, or repeated delimiters are dropped.
#'
#' @param text A single character string.
#'
#' @return A character vector of tokens, in order of appearance.
#' @export
#'
#' @examples
#' dd_tokenize("For me, the best part of the day is the morning coffee.")
dd_tokenize <- function(text) {
  if (length(text) != 1L || !is.character(text)) {
    stop("`text` must be a single character string.", call. = FALSE)
  }
  tokens <- strsplit(text, "[^A-Za-z']+")[[1]]
  tokens[tokens != ""]
}

# Which tokens belong to a dictionary category. Dictionary entries
# ending in "*" match by prefix (LIWC-style stemming, e.g. "happi*"
# matches "happy", "happiness", "happier"); all other entries match
# whole tokens exactly, case-insensitively.
match_category <- function(tokens, dictionary) {
  if (length(dictionary) == 0) {
    return(logical(length(tokens)))
  }
  tokens_lc <- tolower(tokens)
  dictionary_lc <- tolower(dictionary)
  is_wild <- grepl("\\*$", dictionary_lc)

  exact <- dictionary_lc[!is_wild]
  is_match <- tokens_lc %in% exact

  stems <- sub("\\*$", "", dictionary_lc[is_wild])
  stems <- stems[nzchar(stems)]
  if (length(stems) > 0) {
    pattern <- paste0("^(", paste(stems, collapse = "|"), ")")
    is_match <- is_match | grepl(pattern, tokens_lc)
  }
  is_match
}

# Normalize the DD `dictionary` argument (character vector or named
# list of character vectors) to a always-named list, so downstream
# code has one shape to iterate over.
as_dictionary_list <- function(dictionary) {
  if (is.character(dictionary)) {
    dictionary <- list(category = dictionary)
  }
  if (!is.list(dictionary) || !all(vapply(dictionary, is.character, logical(1)))) {
    stop("`dictionary` must be a character vector or a named list of character vectors.",
         call. = FALSE)
  }
  if (is.null(names(dictionary)) || any(!nzchar(names(dictionary)))) {
    names(dictionary) <- paste0("category", seq_along(dictionary))
  }
  dictionary
}
