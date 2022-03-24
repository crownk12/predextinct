# `predextinct`</a>

### Predicting species-level extinctions from population-level data for asessments of climate-related extinction probabilities.

## What is `predextinct`?

`predextinct` is an R package that is designed to estimate probabilities of species-level extinctions using synthetic datasets of population-level characteristics. This package is largely a wrapper for `rangeshiftR`. 

`predextinct` is also able to perform ...


## What is the origin of `predextinct`?

Predicting extinctions bear a particular interest not only in biology, but for society in general. Dr. Román-Palacios published in 2020 a paper explaining the drivers of climate-related biological extinctions. In this paper, they focused on exploring patterns of climate-related extinctions using populations, and not species. However, a natural follow up to this paper would be to see if we can predict whether a species will experience extinction or not based on data collected across populations. We may consider that conducting species-based predictive studies will further contribute to biodiversity since they may survive, even if a population in a certain area goes extinct. Namely, in the case of species-level analysis rather than only population-level extinction prediction, it is possible to continuously sustain the biodiversity of society as a whole in the future. However, using species-level only is more realistically difficult than using population-level because we have to put more effort and time into it. Therefore, we will try to conduct research that predicts species-level extinctions from population-level data for climate-related extinction risk assessment; using population-level data to examine risks of extinction has not been done before.

`predextinct` simplifies ...


## Installing `predextinct`

`predextinct` is currently only available through `GitHub`. It can be easily installed using the following code. (It might be moved to CRAN in the future).

```
library(devtools) 
devtools::install_github("crownk12/predextinct")
```

`predextinct` has to be built from source and requires the package `RangeShiftR`.

## Dedication

## Additional resources

More details about the functions implemented in `predextinct` can be found in the different vignettes associated with the package.

## Contributing

## Contact

## References
