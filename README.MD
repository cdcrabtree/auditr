[![Build Status](https://travis-ci.org/cdcrabtree/auditr.svg?branch=master)](https://travis-ci.org/cdcrabtree/auditr) [![Build status](https://ci.appveyor.com/api/projects/status/github/cdcrabtree/auditr?svg=true)](https://ci.appveyor.com/api/projects/status/github/cdcrabtree/)

# auditr: Classify Surnames by Race Across U.S. Counties Demographics

This is a companion package to Crabtree and Chykina (2018). It contains one function `names_probabilites`. This function takes a vector of last names, generates a matrix of name and county pairs using packaged data, takes this matrix and returns the probability that a name denotes one of four racial (or ethnic) groups (i.e. Asian, Black, Hispanic, and White) for all counties, and then plots these values. This allows individuals to visually identify the extent to which the racial information provided by surnames varies across geographic contexts and to identify potentially problematic surnames.

For the reasons why you might want to do this, see Crabtree and Chykina (2018) and Gaddis (2017).

## Package Installation
The latest development version (0.1.0) is on GitHub can be installed using devtools.

```
if(!require("ghit")){
  install.packages("ghit")
}
ghit::install_github("cdcrabtree/auditr")
```

## Support or Contact
Please use the issue tracker for problems, questions, or feature requests. If you would rather email with questions or comments, you can contact [Charles Crabtree](mailto:ccrabtr@umich.edu) and he will address the issue.

If you would like to contribute to the package, that is great! We welcome pull requests and new developers.

## Tests
To test the software, users and potential contributors can use the example code provided in the documentation for each function.

## References
- Crabtree, Charles and Volha Chykina. 2018. "Last Name Selection in Audit Studies." _Sociological Science_.
- Gaddis, S. Michael. 2017. "How Black Are Lakisha and Jamal? Racial Perceptions from Names Used in Correspondence Audit Studies." _Sociological Science_, DOI 10.15195/v4.a19.
