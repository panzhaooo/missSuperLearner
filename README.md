# missSuperLearner


The `missSuperLearner` `R` package implements a version of the *super learning
algorithm* that handles missing data while identifying the best algorithm in a
library, or the best combination of  algorithms in the library, where the said
library consists of several super learners.


The package is developed by P. Zhao (Inria), N. Gatulle (La Pitié Salpétrière,
APHP), J.  Josse (Inria) and A.  Chambaz (Université Paris Cité).


## Introduction

The   main   function   of    the   package   are   `missSuperLearner()`   and
`CV.missSuperLearner()`. 

```r
> library("missSuperLearner")
> example(missSuperLearner)
```

## Citation

To cite the package, see 

```r
> citation("missSuperLearner")
> toBibtex(citation("missSuperLearner"))
```

## Installation 

Both  a   stable  version  and   a  development  version  are   available  via
[GitHub](https://github.com/panzhaooo/missSuperLearner)   and    can   be
installed in R as:

```r 
devtools::install_github("panzhaooo/missSuperLearner", ref = "main")
```

or 

```r 
devtools::install_github("panzhaooo/missSuperLearner", ref = "develop")
```

