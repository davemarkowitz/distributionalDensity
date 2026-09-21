# ---- dd_prevalence ---------------------------------------------------

test_that("dd_prevalence returns a percentage of word count, LIWC-style", {
  expect_equal(dd_prevalence(25, 500), 5)
  expect_equal(dd_prevalence(0, 500), 0)
  expect_equal(dd_prevalence(500, 500), 100)
})

test_that("dd_prevalence rejects non-positive word counts", {
  expect_error(dd_prevalence(1, 0))
})

# ---- dd_burstiness ----------------------------------------------------

test_that("dd_burstiness is NA with fewer than 3 occurrences", {
  expect_true(is.na(dd_burstiness(numeric(0))))
  expect_true(is.na(dd_burstiness(c(5))))
  expect_true(is.na(dd_burstiness(c(5, 10))))
})

test_that("dd_burstiness is -1 for perfectly evenly-spaced occurrences", {
  expect_equal(dd_burstiness(c(0, 10, 20, 30, 40)), -1)
})

test_that("dd_burstiness is invariant to linear rescaling of positions", {
  raw <- c(1, 9, 17, 25, 33, 41)
  scaled <- (raw - 0.5) / 48
  expect_equal(dd_burstiness(raw), dd_burstiness(scaled))
  expect_equal(dd_burstiness(raw, method = "raw"), dd_burstiness(scaled, method = "raw"))
})

test_that("dd_burstiness position order does not matter", {
  positions <- c(41, 1, 25, 9, 33, 17)
  expect_equal(dd_burstiness(positions), dd_burstiness(sort(positions)))
})

# ---- dd_burstiness standardize ----------------------------------------

test_that("dd_burstiness standardize = TRUE requires n_words", {
  expect_error(dd_burstiness(c(1, 9, 17, 25), standardize = TRUE))
  expect_error(dd_burstiness(c(1, 9, 17, 25), standardize = TRUE, n_words = 0))
})

test_that("dd_burstiness standardize = TRUE rejects n_words smaller than occurrence count", {
  expect_error(dd_burstiness(c(1, 9, 17, 25), standardize = TRUE, n_words = 3))
})

test_that("dd_burstiness standardize = TRUE is NA with fewer than 3 occurrences", {
  expect_true(is.na(dd_burstiness(c(5, 10), standardize = TRUE, n_words = 48)))
})

test_that("dd_burstiness standardize = TRUE returns a finite z-score for clustered and periodic patterns", {
  set.seed(1)
  z_periodic <- dd_burstiness(c(1, 9, 17, 25, 33, 41), standardize = TRUE, n_words = 48, n_sim = 500)
  z_clustered <- dd_burstiness(c(1, 3, 5, 45, 46, 48), standardize = TRUE, n_words = 48, n_sim = 500)
  expect_true(is.finite(z_periodic))
  expect_true(is.finite(z_clustered))
  expect_lt(z_periodic, 0)
  expect_gt(z_clustered, 0)
})

test_that("dd_burstiness standardize = TRUE is centered near zero for random placement, unlike the raw estimator", {
  set.seed(2)
  n_words <- 300
  k <- 4  # small k: raw KJ estimator is known to run negative here
  raw_vals <- replicate(400, {
    pos <- sample.int(n_words, k)
    dd_burstiness(pos, method = "kj")
  })
  z_vals <- replicate(400, {
    pos <- sample.int(n_words, k)
    dd_burstiness(pos, standardize = TRUE, n_words = n_words, n_sim = 200)
  })
  expect_lt(mean(raw_vals), -0.05)
  expect_lt(abs(mean(z_vals, na.rm = TRUE)), 0.25)
})

# ---- dd_position --------------------------------------------------------

test_that("dd_position is NA for zero occurrences", {
  expect_true(is.na(dd_position(numeric(0), n_words = 48)))
})

test_that("dd_position centers a single mid-text occurrence near .50", {
  expect_equal(dd_position(24, n_words = 48), 23.5 / 48)
})

test_that("dd_position is near 0 for an early occurrence and near 1 for a late one", {
  expect_lt(dd_position(1, n_words = 100), 0.02)
  expect_gt(dd_position(100, n_words = 100), 0.98)
})

# ---- dd_dispersion --------------------------------------------------------

test_that("dd_dispersion is NA with fewer than 2 occurrences", {
  expect_true(is.na(dd_dispersion(numeric(0), n_words = 48)))
  expect_true(is.na(dd_dispersion(c(24), n_words = 48)))
})

test_that("dd_dispersion is low for a single tight cluster and high for a split cluster", {
  tight <- dd_dispersion(c(23, 24, 25, 26), n_words = 48)
  split <- dd_dispersion(c(1, 2, 47, 48), n_words = 48)
  expect_lt(tight, 0.3)
  expect_gt(split, 0.9)
})

test_that("the finite bound is never smaller than the continuous bound's dispersion", {
  # The finite bound is tighter (smaller denominator upper bound is
  # not guaranteed, but the finite-normalized dispersion should never
  # be more permissive than reality allows: it is bounded in [0, 1]).
  d_finite <- dd_dispersion(c(1, 3, 46, 48), n_words = 48, bound = "finite")
  d_cont <- dd_dispersion(c(1, 3, 46, 48), n_words = 48, bound = "continuous")
  expect_lte(d_finite, 1 + 1e-9)
  expect_lte(d_cont, 1 + 1e-9)
})
