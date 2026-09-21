# ---- tokenization and category matching --------------------------------

test_that("dd_tokenize splits on non-letters and keeps apostrophes", {
  expect_equal(
    dd_tokenize("For me, the best part of the day is the morning coffee."),
    c("For", "me", "the", "best", "part", "of", "the", "day", "is",
      "the", "morning", "coffee")
  )
  expect_equal(dd_tokenize("don't stop"), c("don't", "stop"))
})

test_that("dd_tokenize requires a single string", {
  expect_error(dd_tokenize(c("a", "b")))
  expect_error(dd_tokenize(1))
})

test_that("wildcard dictionary entries match by prefix", {
  res <- distributional_density(
    "I am happy, happiness makes me happier than anything.",
    list(happy = "happi*")
  )
  expect_equal(res$n_events, 2)  # happiness, happier (not "happy" itself)
})

test_that("category matching is case-insensitive", {
  res <- distributional_density("I said I would go.", c("i"))
  expect_equal(res$n_events, 2)
})

# ---- distributional_density shape and edge cases ------------------------

test_that("a category absent from the text returns zero prevalence and NA for the other metrics", {
  res <- distributional_density("The cat sat on the mat.", c("xyz_never_present"))
  expect_equal(res$n_events, 0)
  expect_equal(res$prevalence, 0)
  expect_true(is.na(res$burstiness))
  expect_true(is.na(res$position))
  expect_true(is.na(res$dispersion))
})

test_that("multiple texts and multiple categories produce one row per combination", {
  res <- distributional_density(
    c(a = "I like my coffee.", b = "You like your tea."),
    list(self = c("i", "my"), other = c("you", "your"))
  )
  expect_equal(nrow(res), 4)
  expect_setequal(res$id, c("a", "b"))
  expect_setequal(res$category, c("self", "other"))
})

test_that("id defaults to 1:length(text) when text is unnamed", {
  res <- distributional_density(c("I like coffee.", "You like tea."), c("i"))
  expect_equal(unique(res$id), c(1, 2))
})

test_that("explicit id argument is honored and must match text length", {
  res <- distributional_density(c("I like coffee.", "You like tea."), c("i"), id = c("p1", "p2"))
  expect_equal(res$id, c("p1", "p2"))
  expect_error(distributional_density(c("a", "b"), c("i"), id = "only_one"))
})

test_that("a single unnamed dictionary vector is labeled 'category'", {
  res <- distributional_density("I like my coffee.", c("i", "my"))
  expect_equal(res$category, "category")
})

test_that("standardize = 'z'/'center' replace burstiness but leave burstiness_raw untouched", {
  set.seed(3)
  text <- "I lift my mug and I breathe in my first sip of it, mine alone, made for me."
  dict <- c("i", "me", "my", "mine")
  res_raw <- distributional_density(text, dict)
  res_z <- distributional_density(text, dict, standardize = "z", n_sim = 300)
  res_center <- distributional_density(text, dict, standardize = "center", n_sim = 300)

  expect_equal(res_z$burstiness_raw, res_raw$burstiness_raw)
  expect_equal(res_center$burstiness_raw, res_raw$burstiness_raw)
  expect_false(isTRUE(all.equal(res_z$burstiness, res_raw$burstiness)))
  expect_false(isTRUE(all.equal(res_center$burstiness, res_raw$burstiness)))
})

test_that("bound = 'continuous' substitutes the continuous-bound dispersion", {
  res_finite <- distributional_density(
    "I lift my mug and I breathe in my first sip of it, mine alone, made for me.",
    c("i", "me", "my", "mine")
  )
  res_cont <- distributional_density(
    "I lift my mug and I breathe in my first sip of it, mine alone, made for me.",
    c("i", "me", "my", "mine"),
    bound = "continuous"
  )
  expect_equal(res_cont$dispersion, res_cont$dispersion_continuous)
  expect_false(isTRUE(all.equal(res_finite$dispersion, res_cont$dispersion)))
})
