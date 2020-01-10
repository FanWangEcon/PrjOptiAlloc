[![HitCount](http://hits.dwyl.io/fanwangecon/PrjOptiAlloc.svg)](https://github.com/FanWangEcon/PrjOptiAlloc)  [![Star](https://img.shields.io/github/stars/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/stargazers) [![Fork](https://img.shields.io/github/forks/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/network/members) [![Star](https://img.shields.io/github/watchers/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/watchers)

This is a research project [website](https://fanwangecon.github.io/PrjOptiAlloc/) for solving optimal allocation problems given estimation results. Various theorems from [Fan](https://fanwangecon.github.io/)'s Optimal Allocation paper are implemented here as R code in one package that supports the paper. 

See [here](https://github.com/FanWangEcon) for all of [Fan](https://fanwangecon.github.io/)'s public repositories. Please contact [FanWangEcon](https://fanwangecon.github.io/) for issues or problems.

[![](https://img.shields.io/github/last-commit/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/commits/master) [![](https://img.shields.io/github/commit-activity/m/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/graphs/commit-activity) [![](https://img.shields.io/github/issues/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/issues) [![](https://img.shields.io/github/issues-pr/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/pulls)


# 1. Introduction

## 1.1 From Causal Estimates to Optimal Allocations 

The we we approach an estimation problem here is in three steps. 

*First*, we get estimates from estimation function from some existing research project. Finding causal estimates whether in reduced form or structural context is the main objective of most empirical papers. Generally, most research ends with finding what inputs/factors/policies matter, and how much they matter. 

*Second*, using our optimal allocation theroems to solve for the optimal allocations that are implied by the estimation results. This is rarely done in empirical works. Most empirical papers do not then discuss how to allocate the input/factor/policy across individuals--children, households, schools, etc--given finite resource and the estimation results. Our theorems provide analytical or pseudo-analytical solutions to the optimal allocation problem given a large class of estimation results. 

*Third*, given our allocation theorems, we can analyze how inequality shifts given different level of resource available and different types of planner preference. In particular, we calculate inequality gini coefficients for allocation as well as expected outcomes given allocations that are the solutions to the optimal allocation problems of different planners.

## 1.2 Installation

One key aspect of this project is that the theorems we derive are easily implemenable for large classes of estimation results. The theorems are computationally implemented here using R (the algorithms are language agnostic) in a package called [PrjOptiAlloc](https://github.com/FanWangEcon/PrjOptiAlloc). Functions in this package are described in the various [reference pages](https://fanwangecon.github.io/PrjOptiAlloc/reference/) and [vignette articles](https://fanwangecon.github.io/PrjOptiAlloc/articles/) on this website. 

The tools here depend on [base R](), [tidyverse]() and [tidymodels](). After installing those, two packages should be installed, [fan](http://fanwangecon.github.io/)'s [REconTools](http://fanwangecon.github.io/REconTools/), which provides a number of support tools required, as well as this package itself [PrjOptiAllocs](http://fanwangecon.github.io/PrjOptiAlloc/). For installation help, see [devtools](http://r-pkgs.had.co.nz/intro.html).

```
# To Install the Programs in the R folder of the REconTools Repository
devtools::install_github("fanwangecon/PrjOptiAlloc")
# There are various dependencies that need to be installed, including
devtools::install_github("fanwangecon/REconTools")
```

# 2 Optimal Allocation Continuous Linear Problems

Linear Optimal Allocations for the *N_i* and *H_i* problem. Optimal constrained resource allocation given continuous linear estimation results. The continuous linear estimation results could be based on causal reduced form estimation, or production function estimation results. 

## 2.1 Allocation Function

Taking estimation results as function parameters, we solve the optimal allocation problem using the function below.

1. [**function reference**](https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_solin_relow.html): solves optimal allocations given linear estimates
2. [**function testing vignette**](https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_solin_relow.html): illustrate function math steps

## 2.2 Allocation Start to End

Here, to illustrate all steps, we start from loading in data, estimate, allocate and analyze. This is the full vignette that shows all steps to. The steps here become three parts. First, get estimates from estimation function from some existing research project. Second, use our [optimal allocation function](https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_solin_relow.html)/algoritm given estimation results. Third, analyze allocation results's implications for equality of allocations.

These functions show in practice how the various tools can be used in sequence to solve and anlyze the optimal allocation problem. 

1. [**line by line**](https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_solin_relow_allrw.html): data to estimation to optimal allocation to planner preference to gini
2. [**call functions**](https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_solin_relow_allfn.html): from data to gini call functions calling various functions in sequence.

## 2.3 Saved Allocation Examples

To make the allocation results transparent, we store example allocation results from the [Height and Protein](https://fanwangecon.github.io/PrjOptiAlloc/reference/ffy_opt_dtgch_cbem4.html) data, which has slightly more than 1000 observations. 

1. [**data reference**](https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_dtgch_cbem4_rrlop.html): reference file describing example allocation data.
2. [**rda data file**](https://github.com/FanWangEcon/PrjOptiAlloc/blob/master/data/df_opt_dtgch_cbem4_rrlop.rda): download the rda datafile and open in R to review example allocation results

# 3 Support Files

## 3.1 Generate Regression Results Examples

First, we generate some regression results. These provide the optimal allocation problems with parameter inputs. 

1. **Height and Protein**: linear and log linear production function 
    - Data generation function: [reference](https://fanwangecon.github.io/PrjOptiAlloc/reference/ffy_opt_dtgch_cbem4.html) \| [save data vignette](https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_dtgch_cbem4.html)
    - Saved data file: [reference](https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_dtgch_cbem4.html) \| [data file](https://github.com/FanWangEcon/PrjOptiAlloc/blob/master/data/df_opt_dtgch_cbem4.rda)

## 3.2 Allocation Distributional Analysis

1. **Planner Preference and Gini**: variety of planner elasticity and resulting allocation gini
    - Data generation function: [reference](https://fanwangecon.github.io/PrjOptiAlloc/reference/ffp_opt_anlyz_rhgin.html) \| [save data vignette](https://fanwangecon.github.io/PrjOptiAlloc/articles/ffv_opt_anlyz_rhgin.html)
    - Saved example (height linear) allocations: [reference](https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_dtgch_cbem4_rrlop_allrh.html) \| [data file](https://github.com/FanWangEcon/PrjOptiAlloc/blob/master/data/df_opt_dtgch_cbem4_rrlop_allrh.rda)
    - Saved example (height linear) gini: [reference](https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_dtgch_cbem4_rrlop_argin.html) \| [data file](https://github.com/FanWangEcon/PrjOptiAlloc/blob/master/data/df_opt_dtgch_cbem4_rrlop_argin.rda)
  

----
Please contact [![](https://img.shields.io/github/followers/fanwangecon?label=FanWangEcon&style=social)](https://github.com/FanWangEcon) [![](https://img.shields.io/twitter/follow/fanwangecon?label=%20&style=social)](https://twitter.com/fanwangecon) for issues or problems.

![RepoSize](https://img.shields.io/github/repo-size/fanwangecon/PrjOptiAlloc)
![CodeSize](https://img.shields.io/github/languages/code-size/fanwangecon/PrjOptiAlloc)
![Language](https://img.shields.io/github/languages/top/fanwangecon/PrjOptiAlloc)
![Release](https://img.shields.io/github/downloads/fanwangecon/PrjOptiAlloc/total)
![License](https://img.shields.io/github/license/fanwangecon/PrjOptiAlloc)
