
# testCompareR

<!-- badges: start -->
<!-- badges: end -->

Test metrics like sensitivity, specificity, the predictive values and the likelihood ratios are common ways to measure the performance of a diagnostic test. The goal of testCompareR is to make comparing the test metrics from two diagnostic tests with dichotomous outcomes easy. Really easy.

We want clinical researchers to be able to quickly access the statistical methods with the best performance, without having to trawl through the literature or learn a new complicated package. `testCompareR` does all of that so you don't have to. 

## Installation

You can install the development version of testCompareR from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Kajlinko/testCompareR")
```

## Examples

`testCompareR` has two principal functions: `compareR()` and `interpretR`.

This example with simulated data demonstrates how they work. The data is provided as a data frame or matrix with three columns in the order test1, test2 and gold. It is sensible to code positive and negative results systematically, but real research can sometimes be messy. The `compareR()` function will try to take care of that for you.

``` r
library(testCompareR)

# simulate some data
test1 <- rep(c("positive", "pos", "p", "yes", "y", "+", "1"), 10)
test2 <- rep(c("negative", "neg", "no", "n", "-", "0", "2"), 10)
gold <- rep(c("positive", "pos", "p", "yes", "y", "+", "1"), 10)

df <- data.frame(test1, test2, gold)

# run the tests
results <- compareR(df)

# interpret the results (optional)
interpretR <- results
```

With two function calls you can calculate the test metrics, including the confidence intervals with the best coverage, a compare the test metrics using the hypothesis tests with the best asymptotic performance. `interpretR()` even provides a plain English summary of what your results mean in the console. 


### Additional functions

If you only have one test and the gold standard you can summarise the descriptive statistics quickly with `summariseR()`. Here the data should be presented as a data frame or matrix with two columns.

``` r
library(testCompareR)

# simulate some data
test1 <- rep(c("positive", "pos", "p", "yes", "y", "+", "1"), 10)
gold <- rep(c("positive", "pos", "p", "yes", "y", "+", "1"), 10)

df <- data.frame(test1, gold)

# run the tests
summariseR(df)
```

One final function has been defined to help clinical researchers who want to perform pooled meta-analysis of data that is already available. `dataframeR()` constructs a data frame that can be provided to `compareR()` from figures commonly quoted in the literature. `dataframeR()` has eight parameters: `s11`, `s10`, `s01`, `s00`, `r11`, `r10`, `r01`, `r00`.

Understanding the parameter names:
s & r represent positive and negative results for the gold standard test, respectively.
The first digit represents a positive (1) or negative (0) result for Test 1.
The second digit represents a positive (1) or negative (0) result for Test 2.

``` r
dataframeR(70, 5, 11, 40, 11, 2, 3, 120)
```

This data frame can be combined with other data frames to produce a master data frame ready for analysis.

If you have any ideas about additional functionalities that you think should be added, please get in touch or, better still, make a pull request and see what you can do with the code.

## Contributors

This project was helped greatly by Marc Henrion [GitHub](https://github.com/gitMarcH/).

Additionally, the statistical methods underlying this package and the source code upon which it is based are provided by José Antonio Roldán Nofuentes. If you use the package, please consider referencing his paper when describing your statistical methods. The paper is available [here](https://doi.org/10.1186/s12874-020-00988-y).

## License

This work is licensed under the General Public License v2.0 (1999). See LICENSE.md for more details.

## References

Yu, Guo & Xu (2014) JSCS. 2014; 84:5,1022-1038
<doi:10.1080/00949655.2012.738211>

Martín Andrés & Álvarez Hernández (2014) Stat Comput. 2014; 24,65–75
<doi:10.1007/s11222-012-9353-5>

Roldán-Nofuentes & Sidaty-Regad (2019) JSCS. 2019; 89:14,2621-2644
<doi:10.1080/00949655.2019.1628234>

Roldán-Nofuentes, Luna del Castillo & Montero-Alonso (2012) Comput Stat Data Anal. 2012; 6,1161–1173.
<doi:10.1016/j.csda.2011.06.003>

Kosinski (2012) Stat Med. 2012; 32,964-977
<doi:10.1002/sim.5587>

Roldán-Nofuentes, Luna del Castillo (2007) Stat Med. 2007; 26:4179–201.
<doi:10.1002/sim.2850>

Roldán-Nofuentes (2020) BMC Med Res Methodol. 2020; 20,143
<doi:10.1186/s12874-020-00988-y>
