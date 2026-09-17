# distributionalDensity

`distributionalDensity` computes the four **distributional density**
parameters of a psychologically-consequential language dimension
within a text: prevalence, burstiness, position, and dispersion.

Traditional psychology-of-language and NLP research measures a
construct's **prevalence**: how much of it appears in a text, relative
to total word count. Prevalence collapses a text into a single count
and discards everything about *where* and *how* those words are
placed. Two texts with identical prevalence of self-references, for
example, can differ enormously in whether those references are spread
evenly through the text, clustered into a single burst, or split
between the opening and closing lines. Distributional
density recovers that information with three additional parameters:

| Parameter | Question it answers | Range |
|---|---|---|
| **Prevalence** | How much of the dimension is present? | `[0, 100]` |
| **Burstiness** | How clustered are its occurrences? | `[-1, 1]` |
| **Position** | How early or late does it occur, on average? | `[0, 1]` |
| **Dispersion** | How widely are occurrences spread around that mean? | `[0, 1]` |

The four parameters are conceptually independent: a text can have
extreme clustering (high burstiness) yet near-perfect positional
balance (position ≈ .50), and dispersion is what distinguishes "one
cluster at the midpoint" from "two clusters bookending the text" —
cases prevalence, burstiness, and position alone cannot tell apart.

## Installation

```r
# install.packages("devtools")
devtools::install_github("davemarkowitz/distributionalDensity")
```

## Use

`dd()` is the main entry point. Give it a character vector of texts
and a dictionary, and it returns one row per text with the four
parameters appended as columns:

```r
library(distributionalDensity)

texts <- c(
  periodic = "I wake at dawn and brew the coffee. My mug comes outside to the porch. There, I watch the sky brighten over the yard. My thoughts settle as the light spreads. For me, this is the best hour of all. Mine alone, before the busy day begins again.",
  bursty   = "I brew my coffee; I always have, the same way every morning, while the house goes quiet and the kettle cools and the day takes over with errands, emails, and everything else. Still, that first warm sip always feels like it was made for me, my reward, mine."
)

self_refs <- c("i", "me", "my", "mine", "myself")

dd(text = texts, dictionary = self_refs)[, c("id", "n_words", "n_events",
                                              "prevalence", "burstiness",
                                              "position", "dispersion")]
#> (also returns `text`, `burstiness_raw`, and `dispersion_continuous`, omitted here for width)
#>         id n_words n_events prevalence burstiness position dispersion
#> 1 periodic      48        6       12.5    -1.0000   0.4271     0.6448
#> 2   bursty      48        6       12.5     0.7278   0.5035     0.9306
```

Both texts are 48 words long with 6 self-references each, so prevalence
is identical (12.5%) — a prevalence-only analysis would treat them as
the same. But the self-references are spaced almost perfectly evenly
in `periodic` (burstiness ≈ -1) and clumped together late in `bursty`
(burstiness ≈ +0.73, dispersion higher too), which is exactly the
distinction prevalence alone can't make. (Burstiness needs at least 3
occurrences and dispersion at least 2 to be defined; texts with fewer
self-references than that correctly return `NA` for those columns
rather than a misleading number.)

If you already have a data frame of texts, pass it as `data` and the
scores are appended onto it directly rather than returned as a
separate table:

```r
df <- data.frame(id = c("p1", "p2"), text = unname(texts))
dd(text = "text", data = df, id = "id", dictionary = self_refs)
```

Score multiple categories at once with a named list of word lists;
column names are prefixed by category when there's more than one:

```r
dd(
  text = "text", data = df, id = "id",
  dictionary = list(
    self = c("i", "me", "my", "mine", "myself"),
    time = c("day", "morning")
  )
)
#> columns: self_prevalence, self_burstiness, ..., time_prevalence, time_burstiness, ...
```

Dictionary entries ending in `*` match by prefix, LIWC-style (e.g.
`"happi*"` matches "happy", "happiness", "happier").

`distributional_density()` is the underlying engine behind `dd()` and
returns the long, one-row-per-text-per-category form directly (handy
for plotting or joining, e.g. recreating a figure like the manuscript's
Figure 1):

```r
distributional_density(texts, self_refs)
```

### Building metrics from token positions directly

The individual metric functions operate on raw occurrence positions,
for users who want to work from their own tokenization or an existing
dictionary-tagging pipeline:

```r
dd_prevalence(k = 6, n_words = 48)
dd_burstiness(c(1, 9, 17, 25, 33, 41))   # periodic placement, B near -1
dd_position(c(1, 9, 17, 25, 33, 41), n_words = 48)
dd_dispersion(c(1, 9, 17, 25, 33, 41), n_words = 48)
```

## Method notes

- **Burstiness** uses the finite-size-corrected estimator of Kim & Jo
  (2016), built on the uncorrected statistic of Goh & Barabási (2008).
  The correction matters: the uncorrected statistic is confounded with
  the number of occurrences (and therefore with prevalence), which
  would manufacture a spurious burstiness–prevalence correlation in
  any corpus where text length or base rate varies. The uncorrected
  version is available via `dd_burstiness(..., method = "raw")` for
  comparison only.
- **Dispersion** is normalized by an upper bound on the spread
  attainable at a given mean position and number of occurrences. The
  default (`bound = "finite"`) uses a tighter, discrete-token-aware
  bound; `bound = "continuous"` uses the closed-form bound
  `sqrt(m * (1 - m))` (m = mean position), which is only attainable in
  the limit of infinite text length and so understates dispersion for
  short texts.
- All four parameters are computed on the observed occurrences
  themselves (population statistics), not sample estimates of a larger
  population.

## References

Goh, K.-I., & Barabási, A.-L. (2008). Burstiness and memory in complex
systems. *Europhysics Letters*, *81*(4), 48002.
<https://doi.org/10.1209/0295-5075/81/48002>

Kim, E.-K., & Jo, H.-H. (2016). Measuring burstiness for finite event
sequences. *Physical Review E*, *94*(3), 032311.
<https://doi.org/10.1103/PhysRevE.94.032311>

Fleeson, W. (2001). Toward a structure- and process-integrated view of
personality: Traits as density distributions of states. *Journal of
Personality and Social Psychology*, *80*(6), 1011–1027.
<https://doi.org/10.1037/0022-3514.80.6.1011>
