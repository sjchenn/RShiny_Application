# Predicting the probability of admission to UCLA via regression methods

<!-- badges: start -->
  [![R-CMD-check](https://github.com/Sta523-Fa21/final_proj_skr523/workflows/R-CMD-check/badge.svg)](https://github.com/Sta523-Fa21/final_proj_skr523/actions)
  <!-- badges: end -->
  
This project intends to provide a Shiny APP, which is based on a dataset from kaggle and 3 regression methods, to enable users to predict their chance of being admitted to UCLA. 

## Description

Many students applying for a graduate program are facing the problem of choosing the appropriate school to apply for, either because they are not aware of university
rankings or would have been misinformed by seniors and fellow applicants. This can results in students missing out on admissions and leads to a complete wastage of resources. To help them solve this problem, we fit the kaggle dataset, which contains 500 people's performance on some important features, on 3 common regression methods to form a robust and precise evaluation of their profile, and provide them with a reasonable chance of admission. 

In this project, we used OLS, Ridge Regression and LASSO to fit the data and select one with the lowest test MSE for prediction output. Then we used a Shiny APP for user to input their own profile and generate their own result with predicted chance of admission and beautiful radar plots visualizing their performance.

## Getting Started

### Dependencies

* Required libraries: patchwork, ggplot2, GGally, caret, glmnet, dplyr
* R (>= 2.10)

### Installing

* How to install the package: 
```
library(skrFinal)
```

### Executing program

* After the package is loaded, you can run the Rmd file containing the Shiny APP to use it
* Open the Shiny APP
* Input your own scores and ratings, click the button if you have research experience
* Hit the "Let's See!" button to see your result

## Authors

* Haoming Yang (haoming.yang@duke.edu) [@HaomingYang](https://github.com/imkeithyang)
* Sijie Chen (sijie.chen@duke.edu) [@SijieChen](https://github.com/sjchenn)
* Shuo Wang (shuo.wang717@duke.edu) [@ShuoWang](https://github.com/star7878)
* Tianhao Li (tianhao.li@duke.edu) [@TianhaoLi](https://github.com/Tianhao-Li)
* Xiaoyu Wang (elena.wang@duke.edu) [@XiaoyuWang](https://github.com/ElenaW0528)

## Version History

* 0.0.0.9000
    * Initial Release

## License

This project is licensed under the [MIT + file LICENSE] License - see the LICENSE.md file for details

## Acknowledgments

* [README-Template](https://gist.github.com/DomPizzie/7a5ff55ffa9081f2de27c315f5018afc#file-readme-template-md)
* [Graduat Admission 2](https://www.kaggle.com/mohansacharya/graduate-admissions)


