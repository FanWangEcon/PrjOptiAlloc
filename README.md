[![Star](https://img.shields.io/github/stars/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/stargazers) [![Fork](https://img.shields.io/github/forks/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/network/members) [![Star](https://img.shields.io/github/watchers/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/watchers)

This is a research project [website](https://fanwangecon.github.io/PrjOptiAlloc/) for solving individual-specific optimal allocation problems. This projects supports [Nygaard, Sorensen, and Wang (2021)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3839890).

[![](https://img.shields.io/github/last-commit/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/commits/master) [![](https://img.shields.io/github/commit-activity/m/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/graphs/commit-activity) [![](https://img.shields.io/github/issues/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/issues) [![](https://img.shields.io/github/issues-pr/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/pulls)

## Support Projects

- "Optimal Allocations to Heterogeneous Agents with An Application to Stimulus Checks": This project supports [Nygaard, Sorensen, and Wang (2021)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3839890), which focuses on stimulus check and tax rebate optimal allocation problems. [PrjOptiSNW](https://github.com/FanWangEcon/PrjOptiSNW) solves the dynamic programming problems for the paper, and this project handles solving optimal stimulus allocation problems.
- "The Optimal Allocation of Resources among Heterogeneous Individuals": This project supports [Nygaard, Sorensen, and Wang (2020)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3547706), which focuses on COVID-19 stimulus checks related problems. [PrjOptiSNW](https://github.com/FanWangEcon/PrjOptiSNW) solves the dynamic programming problems for the paper, and this project handles solving optimal stimulus allocation problems.
- "The Optimal Allocation of Resources Among Heterogeneous Individuals": This project supports [Wang (2020)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3547706), which focuses on generic optimal allocation problems.

## Usage

The [Functions](https://fanwangecon.github.io/PrjOptiAlloc/reference/index.html) tab provides a list of functions that implements discrete and bounded-continuous optimal allocation problems. It also includes estimation functions, data functions, and other support functions.

The [Tutorials](https://fanwangecon.github.io/PrjOptiAlloc/articles/index.html) tab provides groups of vignettes that solve the various types of optimal allocation problems step-by-step from raw data to optimal allocation queues without invoking external functions. These step by step results are converted into ingredients of different [functions](https://fanwangecon.github.io/PrjOptiAlloc/reference/index.html). Some Vignettes solve for optimal allocation programs by calling these functions directly rather than solving step-by-step.

## Data

To demonstrate intuitions graphically, the [Tutorials](https://fanwangecon.github.io/PrjOptiAlloc/articles/index.html) tab provides numerical examples when the number of candidate recipients of allocations is two. Most files under the [Tutorials](https://fanwangecon.github.io/PrjOptiAlloc/articles/index.html) tab use empirical data where the number of candidate recipients is several hundred or more. For these problems, computational brute-force comparison methods quickly become intractable due to factorially increasing choice-sets and exponentially increasing state-spaces. We provide closed-form solutions to several classes of optimal allocation problems under CES preference aggregation. The computational burden of the solutions increases only linearly with the number of candidate recipients of allocations.

Datasets used include:

1. [R Testing Dataset Birth Weight](https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_birthwt.html)
2. [R Testing Dataset mtcars](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html)
3. [California Test Score Data](https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_caschool.html) from [Stock and Watson (2003)](https://www.amazon.com/dp/B00XIGZW9W/ref=dp-kindle-redirect?_encoding=UTF8&btkr=1)
4. [NSW Job Training Data](https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_caschool.html) from [Lalonde (1986)](https://www.jstor.org/stable/1806062?)
5. [Guatemala and Philippines Nutrition and Height Data](https://fanwangecon.github.io/PrjOptiAlloc/reference/df_opt_dtgch_aorig.html) from [Puentes el. al (2006)](https://www-sciencedirect-com.ezproxy.lib.uh.edu/science/article/pii/S1570677X16300107)
6. See [Nygaard, Sorensen, and Wang (2021)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3839890) and [Nygaard, Sorensen, and Wang (2020)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3547706) for relevant data structures in those settings.

## Installation

The tools here depend on [base R](https://stat.ethz.ch/R-manual/R-devel/library/base/html/base-package.html), [tidyverse](https://www.tidyverse.org/), and [tidymodels](https://www.tidymodels.org/). After installing those, two packages should be installed, [fan](http://fanwangecon.github.io/)'s [REconTools](http://fanwangecon.github.io/REconTools/), which provides a number of support tools required, as well as this package itself [PrjOptiAllocs](http://fanwangecon.github.io/PrjOptiAlloc/). For installation help, see [devtools](http://r-pkgs.had.co.nz/intro.html).

Installing the package will download all datasets, functions as well as vignettes associated with the project.

```
# To Install the Programs in the R folder of the REconTools Repository
devtools::install_github("fanwangecon/PrjOptiAlloc")
# There are various dependencies that need to be installed, including
devtools::install_github("fanwangecon/REconTools")
```

See [here](https://github.com/FanWangEcon) for all of [Fan](https://fanwangecon.github.io/)'s public repositories. Please contact [FanWangEcon](https://fanwangecon.github.io/) for issues or problems.

----
Please contact [![](https://img.shields.io/github/followers/fanwangecon?label=FanWangEcon&style=social)](https://github.com/FanWangEcon) [![](https://img.shields.io/twitter/follow/fanwangecon?label=%20&style=social)](https://twitter.com/fanwangecon) for issues or problems.

![RepoSize](https://img.shields.io/github/repo-size/fanwangecon/PrjOptiAlloc)
![CodeSize](https://img.shields.io/github/languages/code-size/fanwangecon/PrjOptiAlloc)
![Language](https://img.shields.io/github/languages/top/fanwangecon/PrjOptiAlloc)
![Release](https://img.shields.io/github/downloads/fanwangecon/PrjOptiAlloc/total)
![License](https://img.shields.io/github/license/fanwangecon/PrjOptiAlloc)
