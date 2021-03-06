
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postforecasts

The goal of the `postforecasts` package is to unite a variety of
post-processing methods into a single easy-to-use interface.

This repository was created in context of the *Practical Statistical
Training* course offered by the Chair of Statistics at the University of
Göttingen during Winter Term 2021/22. If you want to jump straight to
the results of our project, please refer to the final course paper
`term_paper/Herp_Beck_Post-Processing.pdf` which explains the theory as
well as the empirical results of all implemented post-processing methods
in great detail.

## Installation

You can install the development version of postforecasts from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nikosbosse/post-processing-forecasts")
```

## Data

The `postforecasts` package is tailored towards post-processing Covid-19
forecasts which are provided by two separate data sets:

### UK Covid-19 Crowd Forecasting Challenge

As part of an ongoing research project by the
[EpiForecasts](https://epiforecasts.io/) group at the London School of
Hygiene & Tropical Medicine, the *UK Covid-19 Crowd Forecasting
Challenge* consisted of submitting weekly predictions of Covid-19 Cases
and Deaths in the United Kingdom in 2021. The challenge was not
restricted to experienced researchers in the field but rather intended
to collect quantile predictions for the upcoming four weeks by
non-expert individuals.

One of the main motivations was to gather evidence for or against the
hypothesis that humans are highly capable of precise *point forecasts*.
Yet, at the same time, they tend to be too confident in their beliefs
such that prediction *intervals* are chosen too narrow. In fact, this
tendency presents one motivation for post-processing: Extract valuable
information from point forecasts and adjust the corresponding prediction
intervals with a systematic correction procedure.

In case of individuals that are unfamiliar with statistical methodology,
specifying forecasts for very specific quantiles of the predictive
distribution might lead to inconsistencies. Therefore all participants
could determine an uncertainty parameter around their median prediction
via an interactive web application such that all quantile predictions
could be concluded in an automatic fashion. Note that this procedure
inherently leads to *symmetric* forecast intervals. The results of the
12-week challenge are [publicly
available](https://epiforecasts.io/uk-challenge/).

### European Covid-19 Forecast Hub

According to their [webpage](https://covid19forecasthub.eu/index.html)
the *European Covid-19 Forecast Hub* collects “short-term forecasts of
Covid-19 cases and deaths across Europe, created by a multitude of
infectious disease modelling teams”.

In contrast to the compact UK data, the European Forecast Hub data
contains almost two million observations for over 20 European countries.
Further, the forecasters are knowledgeable research groups that submit
their weekly predictions based on statistical models. Although the data
collection continues in regular frequency up to this day, our data set
is limited to a 32-week span from March 2021 until October 2021.

------------------------------------------------------------------------

The overall structure of the two data sets above is very similar. To get
a better understanding of the data the most important columns for our
analysis are briefly explained here:

-   `location`: The country for which the forecasts were submitted.
    Equals `GB` for the UK data. Our analysis for the European Forecast
    Hub data selects 18 different European countries.

-   `model`: The forecaster (group). Mostly (non-expert) individuals for
    the UK data and international research groups for the European
    Forecast Hub.

-   `target_type`: Either Covid-19 Cases or Covid-19 Deaths.

-   `horizon`: The time horizon how far in advance the predictions were
    submitted. Ranges from 1 week-ahead to 4 weeks-ahead.

-   `forecast_date`: The exact date when the forecasts were submitted.

-   `target_end_date`: The exact date for which the forecasts were
    submitted.

-   `quantile`: One of 23 different quantile values ranging from 0.01 to
    0.99.

-   `prediction`: The predicted value for one specific combination of
    the variables above.

-   `true_value`: The actual, observed number of Covid-19 Cases or
    Deaths. This value is repeated 23 times, once for each quantile
    value.

### Weighted Interval Score

In order to quantify if the post-processed prediction intervals improve
the original forecasts we chose the *Weighted Interval Score* (WIS) as
our evaluation metric. The WIS is a so-called *Proper Scoring Rule*: It
incentivizes the forecaster to state their true best belief and cannot
be manipulated in favour of own interests. It combines measures for
interval *sharpness* as well as *overprediction* and *underprediction*
and can thus be understood as a trade-off between interval *coverage*
and *precision*. A lower WIS score indicates a better overall
performance.

For a rigorous mathematical introduction of the WIS see Section 2.1.3 of
our course paper.

## Main Findings

The `postforecasts` package implements three versions for each of two
post-processing frameworks: *Conformalized Quantile Regression* (CQR)
and *Quantile Spread Averaging* (QSA) as well an as *Ensemble* method,
where each quantile prediction is a *convex combination* of the
individual methods, i.e. a linear combination where all weights are
contained in the unit interval and sum up to one.

This section briefly presents the main findings of comparing all
post-processing methods in terms of the Weighted Interval Score for the
UK Covid-19 Crowd Forecasting Challenge data set. Note that only five of
the six building block methods are included in the comparison since the
*multiplicative CQR* version could not show competitive results early on
and is thus omitted.

The data set which contains original and updated forecasts for all
post-processing methods for the UK data can be conveniently loaded with
the command

``` r
library(postforecasts)
uk_results <- readRDS("data_results/uk_complete.rds")
```

The required computations for obtaining these results can be reproduced
with the following commands but may take a lot of computation time:

``` r
uk_data <- readRDS("data_modified/uk_data_incidences.csv")

uk_results <- uk_data |>
  update_predictions(
    methods = c(
      "cqr", "cqr_asymmetric", "qsa_uniform", "qsa_flexible", "qsa_flexible_symmetric"
    ),
    cv_init_training = 0.5
  ) |>
  collect_predictions() |>
  add_ensemble()
```

The following figure provides a visual illustration of original and
adjusted prediction intervals of all post-processing methods including
the ensemble for one specific combination of the variables `model`,
`target_type`, `horizon` and `quantile`. The color legend displays the
ensemble weights for each method. In this case only the asymmetric CQR
and the flexible (not symmetric) QSA methods contribute to the ensemble.
As a simple weighted average with weights close to 0.5 the lower (upper)
bounds of the ensemble intervals are approximately halfway between the
lower (upper) bounds of the `cqr_asymmetric` and `qsa_flexible`
intervals:

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Except for the last observations on the horizontal axis the forecasts of
the two CQR versions are quite similar and significantly closer to the
original predictions than the QSA intervals. Within the QSA family
`qsa_flexible` and `qsa_flexible_symmetric` produce almost identical
corrections whereas `qsa_uniform` behaves quite differently from all
other methods and consistently causes the largest intervals.

The previous figure shows that different methods can have significantly
different effects, yet it does not provide any hints which method
improves the Weighted Interval Score most. Thus, the following table
collects the WIS for each method on the training and validation set,
aggregated over all models, target types, horizons and quantiles and
sorted by increasing validation score:

|         method         | validation score | training score | dispersion |
|:----------------------:|:----------------:|:--------------:|:----------:|
|        ensemble        |      57.69       |     18.22      |   21.73    |
|      qsa_uniform       |      60.00       |     20.88      |   26.84    |
|      qsa_flexible      |      60.47       |     19.48      |   25.31    |
| qsa_flexible_symmetric |      60.92       |     20.49      |   33.22    |
|          cqr           |      62.15       |     20.82      |   24.10    |
|     cqr_asymmetric     |      63.97       |     14.46      |   17.99    |
|        original        |      65.74       |     23.62      |   12.00    |

Based on the table above, our comparison of the `postforecasts`
post-processing methods yields a couple of interesting findings:

-   All six custom methods improve out-of-sample performance compared to
    the original predictions on the UK data set.

-   All three QSA versions lead to lower validation scores than any CQR
    variant. Thus, based on this first impression, the familiy of QSA
    post-processing methods clearly outperforms the CQR algorithm for
    the UK data.

-   The ensemble model is the clear winner: Combining information from
    multiple QSA and CQR methods works better on new data than any
    individual method on its own. This suggests that the five building
    block methods are not redundant in the sense that they have
    different strengths and weaknesses depending on the location in
    feature space.

-   The asymmetric CQR method suffers most from *overfitting* as it
    results in the lowest training but highest validation score for the
    small UK data set.

-   In general, additional design restrictions such as identical weights
    in case of `qsa_uniform` and/or the symmetry assumption in case of
    `cqr` and `qsa_flexible_symmetric` have some kind of
    *regularization* effect which leads to better generalization to the
    validation set. Indeed, the *least* flexible versions of both method
    frameworks indicate the best validation performance and yet,
    unsurprisingly, the worst training score.

-   All methods improve the original forecasts by *expanding* the
    prediction intervals which is indicated by the larger *dispersion*
    values. `qsa_flexible_symmetric` produces by far the widest
    intervals on average, yet we can not observe a correlation of better
    validation scores and either narrower or wider prediction intervals.

## User Guide

This section provides an overview of the most important `postforecasts`
functions as as a compact guide how to use our package effectively.

The `postforecasts` functions can be grouped into three categories:

1.  Exploratory

    The `plot_quantiles()`, `plot_intervals()` and
    `plot_intervals_grid()` functions visualize the development of true
    Covid-19 Cases and Deaths over time as well as corresponding
    original and adjusted quantile predictions.

2.  Model Fitting

    The `update_predictions()` function is the workhorse of the entire
    `postforecasts` package. It specifies both the raw data and the
    post-processing method(s) that should be applied to the data set.
    The function returns a list of *k* + 1 equally shaped data frames
    for *k* selected post-processing methods where the first element is
    given by the original, possibly filtered, data frame.

    All list elements can be analyzed separately or collectively by
    stacking them into one large data frame with the
    `collect_predictions()` function. The combined data frame is
    designed to work well with analysis functions of the
    [scoringutils](https://epiforecasts.io/scoringutils/) package. If
    multiple post-processing methods are applied, an ensemble model of
    all selected methods can be added via the `add_ensemble()` function,
    which lets the user access both the weighted ensemble predictions
    and a data frame with the corresponding weights.

3.  Evaluation

    As noted above the Weighted Interval Score is our primary metric to
    evaluate the *quality* of prediction intervals. The `score()`
    function of the `scoringutils` package computes this quantity for
    each observation in the data set which can then be aggregated by the
    related `summarise_scores()` function.

    Depending on the *granularity* of the aggregation the output might
    contain many interval scores of vastly different magnitudes. To
    simplify interpretation the `eval_methods()` function computes
    *relative* or *percentage* changes in the Weighted Interval Score
    for each selected method compared to the original quantile
    predictions. Further, these relative changes can be visualized by
    the `plot_eval()` function.

To illustrate the application of the functions above to a practical
example we use the Covid-19 data for Germany in 2021 that is provided by
the European Forecast Hub.

``` r
hub_1 <- readr::read_csv(here::here("data_modified", "hub_data_1_incidences.csv"))
hub_2 <- readr::read_csv(here::here("data_modified", "hub_data_2_incidences.csv"))
hub_3 <- readr::read_csv(here::here("data_modified", "hub_data_3_incidences.csv"))

hub <- dplyr::bind_rows(hub_1, hub_2, hub_3)

hub_germany <- hub |>
  dplyr::filter(location == "DE") |>
  dplyr::distinct(
    model, target_end_date, target_type, quantile, horizon,
    .keep_all = TRUE
  )
```

We start with a visual overview of the original 5%, 20% 80% and 95%
quantile predictions during the summer months of 2021 in Germany that
were submitted by the `EuroCOVIDhub-ensemble` forecasting model:

``` r
plot_quantiles(
  hub_germany,
  model = "EuroCOVIDhub-ensemble", quantiles = c(0.05, 0.2, 0.8, 0.95)
)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

The original predictions look quite noisy overall with the clear trend
that uncertainty and, hence, the interval width increases with growing
forecast horizons. We want to analyze if one particular post-processing
method, *Conformalized Quantile Regression*, improves the predictive
performance for this model on a validation set by computing the Weighted
Interval Scores for Covid-19 Cases and Covid-19 Deaths separately:

``` r
df_updated <- update_predictions(
  hub_germany,
  methods = "cqr", models = "EuroCOVIDhub-ensemble", cv_init_training = 0.5
)
df_combined <- collect_predictions(df_updated)
```

``` r
df_combined |>
  extract_validation_set() |>
  scoringutils::score() |>
  scoringutils::summarise_scores(by = c("method", "target_type"))
```

|  method  | target type | interval score | dispersion |
|:--------:|:-----------:|:--------------:|:----------:|
|   cqr    |    Cases    |     13.40      |    5.07    |
| original |    Cases    |     13.78      |    3.81    |
|   cqr    |   Deaths    |      0.06      |    0.01    |
| original |   Deaths    |      0.05      |    0.03    |

We can observe that CQR improved the WIS for Covid-19 Cases, whereas the
predictive performance for Covid-19 Deaths dropped slightly.

The `update_predictions()` and `collect_predictions()` combination
immediately generalize to multiple post-processing methods. The only
syntax change is a vector input of strings for the `methods` argument
instead of a single string. Hence, if not desired, the user does not
have to learn separate interfaces for each method nor be familar with
the precise implementation. This design allows for maximum syntactic
consistency by masking the internal functionality.

Further, the `update_predictions()` function automatically takes care of
*quantile crossing* by reordering the output predictions in increasing
quantile order. The `cv_init_training` parameter specifies the fraction
of observations that is used for the pure training set before the Time
Series Cross Validation process starts.

As seen in the previous table CQR increases the *dispersion* of the
predictions for Cases significantly. We can visualize one example of
these wider intervals with the `plot_intervals()` function:

``` r
plot_intervals(df_combined, target_type = "Cases", horizon = 2, quantile = 0.05)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

Indeed, the 2 weeks-ahead 90% prediction intervals for Covid Cases in
Germany are widened by CQR. The solid black line represents the true
Case incidences whereas the grey dashed line indicates the end of the
training set within the Cross Validation as specified by the
`cv_init_training` parameter.

Recall that prediction uncertainty increases with larger forecast
horizons. Similarly, CQR *corrections* also increase in size for
forecasts that are submitted further in advance, which can be seen in
the next plot along the horizontal dimension. Interestingly, CQR expands
the intervals only for Cases whereas the forecasts for Deaths are
narrowed!

``` r
plot_intervals_grid(df_combined, facet_by = "horizon", quantiles = 0.05)
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

Besides the target type (Cases or Deaths), it is also useful to compare
CQR effects across forecast horizons or quantiles. Quite intuitively,
CQR generally has a stronger *relative* benefit for large time horizons
and extreme quantiles, where the original forecaster faced a greater
uncertainty. The figure below illustrates how, in special cases like
this one, the effect on the validation set can show rather mixed trends
due to disadvantageous adjustments for the two and three weeks-ahead 98%
prediction intervals:

``` r
df_eval <- eval_methods(df_combined, summarise_by = c("quantile", "horizon"))
plot_eval(df_eval)
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />
