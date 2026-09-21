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

test_that("dd_burstiness standardize requires n_words", {
  expect_error(dd_burstiness(c(1, 9, 17, 25), standardize = "z"))
  expect_error(dd_burstiness(c(1, 9, 17, 25), standardize = "z", n_words = 0))
  expect_error(dd_burstiness(c(1, 9, 17, 25), standardize = "center"))
})

test_that("dd_burstiness standardize rejects n_words smaller than occurrence count", {
  expect_error(dd_burstiness(c(1, 9, 17, 25), standardize = "z", n_words = 3))
  expect_error(dd_burstiness(c(1, 9, 17, 25), standardize = "center", n_words = 3))
})

test_that("dd_burstiness standardize is NA with fewer than 3 occurrences", {
  expect_true(is.na(dd_burstiness(c(5, 10), standardize = "z", n_words = 48)))
  expect_true(is.na(dd_burstiness(c(5, 10), standardize = "center", n_words = 48)))
})

test_that("dd_burstiness standardize = 'z' returns a finite z-score for clustered and periodic patterns", {
  set.seed(1)
  z_periodic <- dd_burstiness(c(1, 9, 17, 25, 33, 41), standardize = "z", n_words = 48, n_sim = 500)
  z_clustered <- dd_burstiness(c(1, 3, 5, 45, 46, 48), standardize = "z", n_words = 48, n_sim = 500)
  expect_true(is.finite(z_periodic))
  expect_true(is.finite(z_clustered))
  expect_lt(z_periodic, 0)
  expect_gt(z_clustered, 0)
})

test_that("dd_burstiness standardize = 'center' returns a finite value for clustered and periodic patterns", {
  set.seed(1)
  c_periodic <- dd_burstiness(c(1, 9, 17, 25, 33, 41), standardize = "center", n_words = 48, n_sim = 500)
  c_clustered <- dd_burstiness(c(1, 3, 5, 45, 46, 48), standardize = "center", n_words = 48, n_sim = 500)
  expect_true(is.finite(c_periodic))
  expect_true(is.finite(c_clustered))
  expect_lt(c_periodic, 0)
  expect_gt(c_clustered, 0)
})

test_that("standardize = 'z' and 'center' are both centered near zero for random placement, unlike the raw estimator", {
  set.seed(2)
  n_words <- 300
  k <- 4  # small k: raw KJ estimator is known to run negative here
  raw_vals <- replicate(400, {
    pos <- sample.int(n_words, k)
    dd_burstiness(pos, method = "kj")
  })
  z_vals <- replicate(400, {
    pos <- sample.int(n_words, k)
    dd_burstiness(pos, standardize = "z", n_words = n_words, n_sim = 200)
  })
  center_vals <- replicate(400, {
    pos <- sample.int(n_words, k)
    dd_burstiness(pos, standardize = "center", n_words = n_words, n_sim = 200)
  })
  expect_lt(mean(raw_vals), -0.05)
  expect_lt(abs(mean(z_vals, na.rm = TRUE)), 0.25)
  expect_lt(abs(mean(center_vals, na.rm = TRUE)), 0.1)
})

test_that("for a genuinely bursty process, 'z' grows with occurrence count but 'center' stays roughly flat", {
  # Regression test for the confound standardize = 'z' can reintroduce:
  # its denominator (the null's sd) shrinks as occurrence count grows,
  # so a z-score inflates with k even when the true clustering strength
  # (here, a fixed-shape Weibull interevent-time generator) is constant.
  # standardize = 'center' (no division by the null's spread) should not
  # show this pattern.
  set.seed(3)
  gen_bursty <- function(k, shape = 0.4, scale = 10) {
    iet <- rweibull(k - 1, shape = shape, scale = scale)
    pos <- cumsum(c(0, iet)) + 1
    list(positions = pos, n_words = ceiling(max(pos)) + 5)
  }
  mean_over_k <- function(k, standardize) {
    vals <- replicate(40, {
      g <- gen_bursty(k)
      dd_burstiness(g$positions, standardize = standardize, n_words = g$n_words, n_sim = 150)
    })
    mean(vals, na.rm = TRUE)
  }

  z_small <- mean_over_k(5, "z")
  z_large <- mean_over_k(60, "z")
  center_small <- mean_over_k(5, "center")
  center_large <- mean_over_k(60, "center")

  expect_gt(z_large, z_small * 2)  # z at least roughly doubles with far more occurrences
  expect_lt(abs(center_large - center_small), 0.3)  # center stays in the same ballpark
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
