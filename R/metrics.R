#' Prevalence
#'
#' The percentage of a text occupied by a verbal category: how much of
#' the dimension is present, independent of where it falls. Expressed
#' as a percentage of total word count, matching the convention used
#' by LIWC and similar word-count tools.
#'
#' @param k Number of category occurrences (a non-negative integer).
#' @param n_words Total number of tokens in the text.
#'
#' @return A numeric percentage in \eqn{[0, 100]}.
#' @export
#'
#' @examples
#' dd_prevalence(k = 25, n_words = 500)
dd_prevalence <- function(k, n_words) {
  if (n_words <= 0) stop("`n_words` must be positive.", call. = FALSE)
  100 * k / n_words
}

#' Burstiness
#'
#' Measures how clustered a category's occurrences are in time (i.e.,
#' in token order), using the interevent times between successive
#' occurrences. \eqn{B} approaches -1 when occurrences are perfectly
#' periodic, equals 0 when they are randomly spaced (Poisson), and
#' approaches +1 when they are extremely clustered.
#'
#' The uncorrected statistic of Goh & Barabasi (2008),
#' \eqn{B = (\sigma - \mu) / (\sigma + \mu)} on interevent times, is
#' confounded with the number of occurrences: under random placement
#' it runs systematically negative when occurrences are few, which
#' means it partly restates prevalence rather than measuring a
#' property independent of it. `method = "kj"` (the default) applies
#' the finite-size correction of Kim & Jo (2016), which removes most
#' of that dependence and is the estimator distributional density
#' analyses should use. `method = "raw"` is provided for comparison
#' only.
#'
#' Burstiness requires at least 3 occurrences (2 interevent times) to
#' be defined; with fewer, `NA` is returned.
#'
#' Even with the Kim & Jo correction, burstiness for texts with few
#' occurrences remains noisier than for texts with many (the
#' correction removes most of the mean bias as a function of
#' occurrence count, but not the sampling variance): since occurrence
#' count scales with text length at a given category rate, this shows
#' up as an apparent relationship between burstiness and text length.
#' `standardize` addresses this by simulating the same finite-size
#' null the correction is already built on -- `n_sim` random
#' placements of the observed number of occurrences among `n_words`
#' token slots -- and comparing the observed value to it. Two ways of
#' doing that comparison are offered, and they are not interchangeable:
#'
#' - `standardize = "z"` divides by the null's standard deviation as
#'   well as subtracting its mean. This is an inferential statistic --
#'   it answers "how surprising is this value under random placement"
#'   -- and it reintroduces an occurrence-count dependence of its own:
#'   the null's standard deviation shrinks as occurrence count grows,
#'   so for a *genuinely* bursty category (not random placement), the
#'   z-score grows with occurrence count even though nothing about the
#'   degree of clustering has changed. It reflects the strength of
#'   evidence against randomness, not the size of the clustering
#'   effect, and should not be used to compare effect magnitudes
#'   across texts that differ in occurrence count.
#' - `standardize = "center"` subtracts only the null's mean, without
#'   dividing by its spread. This does not have that problem: for a
#'   genuinely bursty category it stays roughly constant as occurrence
#'   count varies, while still correcting the residual mean bias the
#'   Kim & Jo estimator leaves at very small occurrence counts. This is
#'   the version to use when comparing burstiness magnitudes across
#'   texts of different lengths or occurrence counts (e.g.
#'   intercorrelating burstiness with prevalence).
#'
#' `standardize = "none"` (the default) leaves the `method` formula
#' untouched.
#'
#' @param positions Numeric vector of occurrence positions within the
#'   text, in any consistent unit (e.g. 1-based token indices, or
#'   positions normalized to \eqn{[0, 1]}); order does not matter, the
#'   function sorts them. Burstiness is invariant to linear rescaling
#'   of `positions`, so raw token indices and normalized positions
#'   give identical results.
#' @param method Either `"kj"` (Kim & Jo, 2016, finite-size-corrected;
#'   default) or `"raw"` (Goh & Barabasi, 2008, uncorrected).
#' @param standardize One of `"none"` (default, leaves the `method`
#'   formula intact), `"z"`, or `"center"`; see Details. `"z"` and
#'   `"center"` require `n_words`.
#' @param n_words Total number of tokens in the text. Only required
#'   when `standardize` is `"z"` or `"center"`, to define the null's
#'   token slots.
#' @param n_sim Number of null placements to simulate when
#'   `standardize` is `"z"` or `"center"`. Default 1000.
#'
#' @return When `standardize = "none"` (default), a numeric value in
#'   \eqn{[-1, 1]}, or `NA` if fewer than 3 occurrences are supplied.
#'   When `standardize` is `"z"` or `"center"`, a numeric value
#'   (unbounded), or `NA` if fewer than 3 occurrences are supplied or
#'   (`"z"` only) the simulated null has zero variance (e.g.
#'   occurrences fill nearly every token slot).
#' @export
#'
#' @references
#' Goh, K.-I., & Barabasi, A.-L. (2008). Burstiness and memory in
#' complex systems. *Europhysics Letters*, *81*(4), 48002.
#' \doi{10.1209/0295-5075/81/48002}
#'
#' Kim, E.-K., & Jo, H.-H. (2016). Measuring burstiness for finite
#' event sequences. *Physical Review E*, *94*(3), 032311.
#' \doi{10.1103/PhysRevE.94.032311}
#'
#' @examples
#' dd_burstiness(c(1, 9, 17, 25, 33, 41))  # periodic, B near -1
#' dd_burstiness(c(1, 3, 5, 45, 46, 48))   # clustered, B near +1
#'
#' # mean-centered against a simulated null: an effect size comparable
#' # across texts of different length / occurrence count
#' dd_burstiness(c(1, 3, 5, 45, 46, 48), standardize = "center", n_words = 48)
dd_burstiness <- function(positions, method = c("kj", "raw"),
                           standardize = c("none", "z", "center"),
                           n_words = NULL, n_sim = 1000) {
  method <- match.arg(method)
  standardize <- match.arg(standardize)
  positions <- sort(positions)
  iet <- diff(positions)
  n <- length(iet)
  if (n < 2 || mean(iet) == 0) return(NA_real_)

  b <- if (method == "raw") {
    s <- pop_sd(iet)
    m <- mean(iet)
    (s - m) / (s + m)
  } else {
    # Kim & Jo (2016) finite-size estimator. n here is the number of
    # interevent times, not the number of events: substituting the
    # maximum coefficient of variation sqrt(n - 1) for r below
    # returns exactly +1, which is what makes the estimator reach
    # its bounds.
    r <- pop_sd(iet) / mean(iet)
    (sqrt(n + 1) * r - sqrt(n - 1)) /
      ((sqrt(n + 1) - 2) * r + sqrt(n - 1))
  }

  if (standardize == "none") return(b)

  if (is.null(n_words) || n_words <= 0) {
    stop("`n_words` must be a positive number when `standardize` is \"z\" or \"center\".",
         call. = FALSE)
  }
  k <- length(positions)
  if (k > n_words) {
    stop("`n_words` cannot be smaller than the number of occurrences.", call. = FALSE)
  }

  null_b <- vapply(seq_len(n_sim), function(i) {
    dd_burstiness(sample.int(n_words, k), method = method)
  }, numeric(1))
  null_b <- null_b[!is.na(null_b)]
  if (length(null_b) < 2) return(NA_real_)

  if (standardize == "center") return(b - mean(null_b))

  null_sd <- sample_sd(null_b)
  if (null_sd == 0) return(NA_real_)
  (b - mean(null_b)) / null_sd
}

# Occurrence positions normalized to the unit interval at the
# midpoint of their token slot: p = (i - 0.5) / N. This keeps a
# single occurrence in an N = 1 text at p = 0.5 rather than at a
# boundary, and spaces slots evenly across [0, 1].
normalize_positions <- function(positions, n_words) {
  (sort(positions) - 0.5) / n_words
}

#' Position
#'
#' The mean normalized placement of a category's occurrences within a
#' text, expressed as a proportion of text length. A position near 0
#' indicates attention to the category is concentrated early in the
#' disclosure, near .50 indicates the middle, and near 1 indicates it
#' arrives late.
#'
#' @inheritParams dd_dispersion
#'
#' @return A numeric value in \eqn{[0, 1]}, or `NA` if `positions` is
#'   empty.
#' @export
#'
#' @examples
#' dd_position(c(1, 9, 17, 25, 33, 41), n_words = 48)
dd_position <- function(positions, n_words) {
  if (length(positions) < 1) return(NA_real_)
  mean(normalize_positions(positions, n_words))
}

# Continuous upper bound on the SD of a distribution on [0, 1] with
# mean m: attained only when mass splits between exactly 0 and 1,
# which no finite token sequence can do, so dispersion normalized
# against it is biased downward.
dispersion_cap_continuous <- function(m) sqrt(m * (1 - m))

# Tighter upper bound on the SD attainable by k occurrences among
# n_words discrete token slots at mean m. Maximizing SD subject to a
# fixed mean on a bounded interval puts every point at a boundary
# except at most one, so it suffices to search over how many
# occurrences sit at each end and solve for the interior point.
#
# This is an upper bound on the attainable maximum, not the maximum
# itself: it optimizes over real-valued positions and so permits
# occurrences to coincide, which distinct token slots cannot do. The
# exact combinatorial maximum requires enumerating all placements,
# which is intractable at realistic corpus sizes; dispersion computed
# against this bound is therefore slightly conservative.
dispersion_cap_finite <- function(m, k, n_words) {
  a <- 0.5 / n_words
  b <- (n_words - 0.5) / n_words
  best <- 0
  for (j in 0:k) {
    for (n_b in 0:(k - j)) {
      n_int <- k - j - n_b
      if (n_int > 1) next
      if (n_int == 0) {
        if (abs((j * a + n_b * b) / k - m) > 1e-9) next
        pts <- c(rep(a, j), rep(b, n_b))
      } else {
        v <- k * m - j * a - n_b * b
        if (v < a - 1e-9 || v > b + 1e-9) next
        pts <- c(rep(a, j), rep(b, n_b), v)
      }
      s <- pop_sd(pts)
      if (s > best) best <- s
    }
  }
  best
}

#' Dispersion
#'
#' The spread of a category's occurrences around their mean position,
#' normalized by the maximum spread attainable given that mean.
#' Dispersion approaches 0 when occurrences concentrate at a single
#' point and approaches 1 when they split between the text's
#' beginning and end. Position and dispersion are complementary:
#' position identifies where attention is centered, and dispersion
#' identifies whether that center is one concentrated region or the
#' midpoint between two regions.
#'
#' Dispersion requires at least 2 occurrences to be defined; with
#' fewer, `NA` is returned.
#'
#' @param positions Numeric vector of 1-based token positions at
#'   which the category occurs (unsorted is fine).
#' @param n_words Total number of tokens in the text.
#' @param bound Which upper bound on spread to normalize against.
#'   `"finite"` (default) uses the tighter, discrete-token-aware
#'   bound; `"continuous"` uses \eqn{\sqrt{m(1-m)}}, the bound for a
#'   continuous distribution on \eqn{[0, 1]}, which is only attainable
#'   in the limit of infinite text length and so understates
#'   dispersion for short texts.
#'
#' @return A numeric value in \eqn{[0, 1]}, or `NA` if fewer than 2
#'   occurrences are supplied.
#' @export
#'
#' @examples
#' # two clusters bookending the text: high dispersion
#' dd_dispersion(c(1, 3, 46, 48), n_words = 48)
#' # one cluster at the midpoint: low dispersion
#' dd_dispersion(c(23, 24, 25, 26), n_words = 48)
dd_dispersion <- function(positions, n_words, bound = c("finite", "continuous")) {
  bound <- match.arg(bound)
  k <- length(positions)
  if (k < 2) return(NA_real_)

  p <- normalize_positions(positions, n_words)
  m <- mean(p)
  s <- pop_sd(p)
  cap <- if (bound == "finite") {
    dispersion_cap_finite(m, k, n_words)
  } else {
    dispersion_cap_continuous(m)
  }
  if (cap == 0) return(NA_real_)
  s / cap
}
