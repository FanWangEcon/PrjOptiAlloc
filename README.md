[![HitCount](http://hits.dwyl.io/fanwangecon/PrjOptiAlloc.svg)](https://github.com/FanWangEcon/PrjOptiAlloc)  [![Star](https://img.shields.io/github/stars/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/stargazers) [![Fork](https://img.shields.io/github/forks/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/network/members) [![Star](https://img.shields.io/github/watchers/fanwangecon/PrjOptiAlloc?style=social)](https://github.com/FanWangEcon/PrjOptiAlloc/watchers)

This is a research project [website](https://fanwangecon.github.io/PrjOptiAlloc/) for solving individual specific optimal allocation problems given estimation results. Various theorems from [Fan](https://fanwangecon.github.io/)'s [Optimal Allocation paper](http://fanwangecon.github.io/assets/FanWang_OptimalTargetingQueue.pdf) are implemented here as R code in one package that supports the paper.

See [here](https://github.com/FanWangEcon) for all of [Fan](https://fanwangecon.github.io/)'s public repositories. Please contact [FanWangEcon](https://fanwangecon.github.io/) for issues or problems.

[![](https://img.shields.io/github/last-commit/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/commits/master) [![](https://img.shields.io/github/commit-activity/m/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/graphs/commit-activity) [![](https://img.shields.io/github/issues/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/issues) [![](https://img.shields.io/github/issues-pr/fanwangecon/PrjOptiAlloc)](https://github.com/FanWangEcon/PrjOptiAlloc/pulls)

# Installation

One key aspect of this project is that the theorems from the [Optimal Allocation paper](http://fanwangecon.github.io/assets/FanWang_OptimalTargetingQueue.pdf) are easily implemenable for large classes of estimation results. The theorems are computationally implemented here using R (the algorithms are language agnostic) in a package called [PrjOptiAlloc](https://github.com/FanWangEcon/PrjOptiAlloc). Functions in this package are described in the various [reference pages](https://fanwangecon.github.io/PrjOptiAlloc/reference/) and [vignette articles](https://fanwangecon.github.io/PrjOptiAlloc/articles/) on this website.

The tools here depend on [base R](), [tidyverse]() and [tidymodels](). After installing those, two packages should be installed, [fan](http://fanwangecon.github.io/)'s [REconTools](http://fanwangecon.github.io/REconTools/), which provides a number of support tools required, as well as this package itself [PrjOptiAllocs](http://fanwangecon.github.io/PrjOptiAlloc/). For installation help, see [devtools](http://r-pkgs.had.co.nz/intro.html).

```
# To Install the Programs in the R folder of the REconTools Repository
devtools::install_github("fanwangecon/PrjOptiAlloc")
# There are various dependencies that need to be installed, including
devtools::install_github("fanwangecon/REconTools")
```

# 1. Introduction

We approach an estimation/allocation problem here is in three steps.

## 1.1 Causal Estimation

We estimate some causal relationship between individual specific allocable input $V_i$ and some outcome of interest. For example, we can estimate the relationship between nutritional supplements and height growth, the effect of a training program on employment chances, the relationship between teacher-student-ratio and test scores, or the effect of offering savings accounts to households in village economies. Finding causal estimates in reduced form or structural context is the main objective of most empirical papers. Generally, most research ends with finding what inputs/factors/policies matter, and how much they matter.

## 1.2 Optimal Allocation given Estimates

What are the allocative implications of the causal estimates and observables? One way to think about this is that we might want to randomize in order to estimate causal effects, but given our estimates, how should we allocate among heterogeneous individuals the finite resources that we found to be beneficial. A development Aid agency, an NGO or a government branch has limited budgets to purchase nutrition, to offer training spots, to hire additional teachers, and to offer savings account access to village households. How do we allocate resources, given resource constraints, heterogeneities across individuals, heterogeneous effects of treatment, and planner preferences and biases?

Using the optimal allocation theorems developed in [Fan](https://fanwangecon.github.io/)'s [Optimal Allocation paper](http://fanwangecon.github.io/assets/FanWang_OptimalTargetingQueue.pdf), it turns out, we can analytical solve for the individual allocation problem analytically. Various pages on the site provides tutorials and examples for different theorems.

# 2. Optimal Allocation

## 2.1 Optimal Targeting Queue

The key analytical concept that make the solutions possible is the Optimal Targeting Queue. It turns out, in both discrete and continuous cases, we can analytical solve for the optimal ranking in which individuals should receive allocations. This ranking, crucially, is invariant to resources. In the discrete cases, the ranking is the allocation. For example, for the binary allocation problem, the theorems will tell us, given the planner's preference for efficiency and equality, exactly who should be the first to receive a training opportunity, the 2nd, the 3rd, etc.

## 2.2 Optimal Targeting Queue and Knots

One might think the targeting queue concept is not relevant for the continuous problems, because there, we need to figure out how much to provide to each individual, not just who should receive first or second. But it turns out that for the continuous problem, the solution is to first analytically find who should be the first individual to receive allocations. Then, there is an analytical function that maps total resource available to how much this first recipient should optimally receive as total resource availability increase. This function, it turns out, is a linear spline, and each successive knot of the spline is where the next individual in the optimal targeting queue begins to receive allocations.

So we first find the optimal targeting queue of who should begin to receive first, second, and third. Then we find the optimal targeting knots associated with the targeting queue, which tells us how much resources is needed before an individual begins to receive. These two analytical sequences lead to analytical solutions to each individual's optimal allocation solution.

## 2.3 Brute-Force vs Analytical Solutions

While the brute-force computational problem is infeasible to solve, the analytical solutions are instantaneous to implement even as the number of candidate recipient of allocations increase to large $N$.

How to optimally allocate across individuals is a computationally difficult question to answer because if we want to allocate to $N$ individuals given $W$ units of resource, the choice set grows factorially with each additional individual, and the state space of the problem grows exponentially with each additional individual. How much the planner can allocate to each individual will be a function of the attributes of all other candidate recipients of allocations.

Trying to solve this problem using brute-force comparison of all possible allocations is computational infeasible even with very small $N$. For example, if we have 700 individuals and 200 training spots, the number of possible combinations is $\frac{700!}{(700=200)!200!}$. This number can not be calculated by normal programs, it is significantly large than $10^{308}$, which is the maximum number allowed by double precision.

# 3. Accessible Welfare Analysis

## 3.1 Allocation and Misallocation

With analytical results on what is optimal, we can also compute an interesting welfare measure, which the paper calls REV (resource equivalent variation).

Suppose there are 10 bottles of protein, and 5 kids. If we randomly allocate, 5 kids will get the proteins. There are $\frac{10!}{5!5!}=252$ combinations of allocations. The random allocation is one of these 252 allocations. The optimal allocation is also one of these 252 allocations. The chance that the random allocation is optimal is 1 out of 252.

But how far away is the random allocation away from the optimal? How much resource is wasted by not allocating more optimally?

## 3.2 Resource Equivalent Variation

To address this, we can use expenditure minimization. We can evaluate the utility of the planner given planner preferences at the observed allocations. Then we can solve for how much less resources is needed to achieve the same level of planner utility if we were allocating optimally? This is an analytical object because it is a function of the analytical optimal targeting queue.

For the example here, we had 5 bottles, perhaps the planner have the same utility (given the equality of efficiency objective it has) if only 2 bottles were offered to the top 2 ranked individuals on the targeting queue. REV in this case would be $\frac{5-2}{5}=0.6$. 60 percent of the resource, we can say, is misallocated.

The exciting thing here is that welfare calculation traditionally has been mainly a tool/concept that macro-economists use. The dominant concept is CEV (consumption equivalent variation). REV is applying in some sense a similar idea, but now to wide swaths of reduced form empirical estimation results. As a simple post estimation command, empirical researchers can evaluate the welfare implications of the observables they see given they estimates they find.

## 3.3 From the Utilitarian to the Rawlsian

For the more equality focused planner, the top two ranked could be the two kids who were be expected to be the shortest without the nutrition. For the more efficiency minded planner, the top two ranked could be the two kids for whom additional nutrient have the greatest marginal effects. As we shift along the  preference spectrum from the Utilitarian to the Rawlsian, the rankings of each of the 10 individuals on the optimal targeting queue shift.


# 3 Optimal Allocation Continuous Linear Problems

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
