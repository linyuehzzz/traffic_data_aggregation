## Traffic Speed Estimation Based on Multi-Source Data Feeds


In this project, we aggregate different sources of traffic data in order to provide a more accurate and reliable estimation of traffic performance across the road network systems. Our method is built based on [Ordinary CoKriging (OCK)](https://doi.org/10.1016/0098-3004(91)90028-C), a data imputation model that can take advantage of multiple datasets. We also introduce a [network distance metric](https://doi.org/10.1080/13658816.2011.609488) in the variogram modelling for OCK, which, compared to Euclidean distance, is more suitable to describe the spatial distance between road links.


### Study area
<img src="https://github.com/linyuehzzz/traffic_data_aggregation/blob/master/img/Fig1.png" width="350">


### Semivariogram and cross-semivariogram
<img src="https://github.com/linyuehzzz/traffic_data_aggregation/blob/master/img/Fig2.png" width="300">


### OCK interpolation results
<img src="https://github.com/linyuehzzz/traffic_data_aggregation/blob/master/img/Fig3.png" width="400">
