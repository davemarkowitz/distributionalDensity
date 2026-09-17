texts <- c(
  "For me, the best part of the day is the morning coffee.",
  "The best part of the day, for me, is the morning coffee."
)
self_words <- c("i", "me", "my", "mine", "myself")

test_that("dd() with a bare text vector builds a new id/text data frame with scores appended", {
  res <- dd(text = texts, dictionary = self_words)
  expect_true(is.data.frame(res))
  expect_equal(res$id, c(1, 2))
  expect_equal(res$text, texts)
  expect_equal(res$prevalence, c(100 / 12, 100 / 12))
  expect_equal(res$position, c(0.125, 0.625))
})

test_that("dd() with data appends columns to the existing data frame without altering original columns", {
  df <- data.frame(id = c("p1", "p2"), text = texts, age = c(30, 45),
                    stringsAsFactors = FALSE)
  res <- dd(text = "text", data = df, id = "id", dictionary = self_words)

  expect_equal(res$id, df$id)
  expect_equal(res$text, df$text)
  expect_equal(res$age, df$age)
  expect_true(all(c("prevalence", "burstiness", "position", "dispersion") %in% names(res)))
  expect_equal(res$position, c(0.125, 0.625))
})

test_that("dd() prefixes columns by category when multiple categories are scored", {
  df <- data.frame(id = c("p1", "p2"), text = texts, stringsAsFactors = FALSE)
  res <- dd(text = "text", data = df, id = "id",
            dictionary = list(self = self_words, time = c("day", "morning")))

  expect_true(all(c("self_prevalence", "self_position", "self_dispersion",
                     "time_prevalence", "time_position", "time_dispersion") %in% names(res)))
  expect_false("prevalence" %in% names(res))
})

test_that("dd() preserves row order of `data` even if scores are computed out of order", {
  df <- data.frame(id = c("z", "a", "m"),
                    text = c("I like my coffee.", "You like your tea.", "We like our soda."),
                    stringsAsFactors = FALSE)
  res <- dd(text = "text", data = df, id = "id", dictionary = c("i", "you", "we"))
  expect_equal(res$id, c("z", "a", "m"))
})

test_that("dd() errors informatively when `text` does not name a column of `data`", {
  df <- data.frame(id = 1, text = "hello", stringsAsFactors = FALSE)
  expect_error(dd(text = "not_a_column", data = df, dictionary = "hello"))
})

test_that("dd() errors when text is not a character vector and no data is supplied", {
  expect_error(dd(text = 1:3, dictionary = "x"))
})
