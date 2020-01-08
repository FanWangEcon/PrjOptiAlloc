# Project

## R files

Different R files should be created that correspond to different core theorems. In addition to that, there should be a number of utility functions. Corresponding to these, there are vignette files.

### Core R Functions

1. Given the input array of optimal linear allocation problem, solve for optimal allocation of all individuals
    - inputs: 1, total resource (scalar); 2, planner reference (scalar); 3, matrix of inputs; 4, col/var name for A vec; 5, col/var name for alpha vec; 6, col/var name for beta vec, optional, assume equal weight if null, default is null.
    - returns: a vector of allocation
2. The same input and output structure as above, but for the log linear case.
3. A function that converts the CES problem to A vec and alpha vec.
    - input: 1, matrix of price, each row a different individual, each column a different product, marginal productivity, etc for each product. This does not need to be produced right away, this can wait for more time.

#### Linear Function Solution Algorithm

1. rank receiving order
2. solve first recipient problem
3. solve all others relative to first
4. predict expected optimal allocation given allocation: A + alpha*N

**Tests**

1. Given different planer preference, using up the same finite total resource
2. Graphically show how allocations change.
3. Gini Inequality measure changes.

There are two functions

1. Inside *ffy_opt_solin.R*: function *ffy_opt_solin_flinr* solves the linear allocation problem at a particular rho level.
2. But also to process the results, note there are similarity to how we want to process log lin and linear results  

#### Log-Linear Function Solution Algorithm

#### Results Processing

There are N children and teach are Z different planner preference parameters, going from perfect inequality to equality. Given actual allocation, there are observed inputs, and predicted expected inputs. There are two vectors for each planner preference: optimal allocation, and expected outcome given optimal allocation. This structure is the same for both linear and log linear models.

So how should this be stored. x`
