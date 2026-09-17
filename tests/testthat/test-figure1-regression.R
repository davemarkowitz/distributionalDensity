# Regression tests against the manuscript's Figure 1 example texts
# and their independently-verified expected values (see
# `burstiness.R` in the parent "Intraindividual Variation in
# Language" project, which derives and sanity-checks these numbers).
# If these break, the package and the published figure have diverged.

texts <- c(
  Periodic = paste(
    "I wake at dawn and brew the coffee. My mug comes outside to the",
    "porch. There, I watch the sky brighten over the yard. My thoughts",
    "settle as the light spreads. For me, this is the best hour of all.",
    "Mine alone, before the busy day begins again."
  ),
  Random = paste(
    "I love my coffee, and I savor it slowly while my porch stays quiet",
    "in the early light. The first hour is mine, spent watching steam",
    "curl from the mug while the house sleeps and the sky slowly",
    "brightens; the whole morning seems to belong just to me."
  ),
  Bursty = paste(
    "I brew my coffee; I always have, the same way every morning, while",
    "the house goes quiet and the kettle cools and the day takes over",
    "with errands, emails, and everything else. Still, that first warm",
    "sip always feels like it was made for me, my reward, mine."
  ),
  Concentrated = paste(
    "The kettle clicks on at dawn and the quiet house holds still while",
    "early light spreads. I lift my mug and I breathe in my first sip of",
    "it, mine alone, made for me, before the phone starts buzzing with",
    "the noise of everyone else in the world."
  )
)

i_words <- c("i", "me", "my", "mine", "myself")

expected <- data.frame(
  id         = c("Periodic", "Random", "Bursty", "Concentrated"),
  n_words    = 48L,
  n_events   = 6L,
  prevalence = 0.125,
  burstiness = c(-1.00, 0.09, 0.73, -0.59),
  position   = c(0.427, 0.309, 0.503, 0.500),
  dispersion = c(0.645, 0.798, 0.931, 0.254),
  stringsAsFactors = FALSE
)

test_that("all texts are 48 tokens with 6 I-word occurrences", {
  res <- distributional_density(texts, i_words)
  expect_equal(res$n_words, rep(48L, 4))
  expect_equal(res$n_events, rep(6L, 4))
})

test_that("burstiness matches the manuscript's Figure 1 values", {
  res <- distributional_density(texts, i_words)
  res <- res[match(expected$id, res$id), ]
  expect_equal(round(res$burstiness, 2), expected$burstiness)
})

test_that("position matches the manuscript's Figure 1 values", {
  res <- distributional_density(texts, i_words)
  res <- res[match(expected$id, res$id), ]
  expect_equal(round(res$position, 3), expected$position)
})

test_that("dispersion matches the manuscript's Figure 1 values", {
  res <- distributional_density(texts, i_words)
  res <- res[match(expected$id, res$id), ]
  expect_equal(round(res$dispersion, 3), expected$dispersion)
})

test_that("Bursty and Concentrated are matched on prevalence and position but separated on dispersion", {
  # This is the figure's central claim: the two conditions are
  # indistinguishable by prevalence or position, and only dispersion
  # tells them apart.
  res <- distributional_density(texts, i_words)
  bc <- res[res$id %in% c("Bursty", "Concentrated"), ]
  expect_lt(diff(range(bc$prevalence)), 1e-9)
  expect_lt(diff(range(bc$position)), 0.02)
  expect_gt(diff(range(bc$dispersion)), 0.50)
})

test_that("the Kim & Jo estimator reaches its +1 bound at maximum clustering", {
  # Five interevent times at the maximum coefficient of variation of
  # sqrt(4) = 2 must return exactly +1.
  expect_equal(
    dd_burstiness(c(0, 1e-9, 2e-9, 3e-9, 4e-9, 1)),
    1,
    tolerance = 1e-4
  )
})
