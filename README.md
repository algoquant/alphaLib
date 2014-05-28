alphaLib
========
alphaLib is a library of R functions for defining trading strategies and for performing backtesting (both in-sample and out-of-sample).  
alphaLib can be thought of as a simpler and lighter version of the [quantstrat][1] package from the [TradeAnalytics][2] project.  
alphaLib functions can be used in conjunction with other packages like [TTR][3], [PerformanceAnalytics][4], and [quantmod][5].  

List of files:
* **alphaModel.R** contains the core alphaModel library functions,
* **chartLib.R** contains functions for time series data plotting,
* **chartLib.new.R** contains new versions of functions for time series data plotting,
* **dataLib.R** contains functions for reading and writing time series data from/to *OneTick*,
* **funcUtil.R** contains various utility functions,
* **utilLib.R** contains generic utility functions for handling strings, xts objects, etc.,
* **optimLib.R** contains functions for portfolio optimization, objective functions for optimization of model parameters,
* **riskLib.R** contains functions for calculating portfolio risk/return statistics (variance, correlation, Hurst exponent), and for running risk/return statistics,
* **retiredLib.R** contains deprecated functions.


[1]: https://r-forge.r-project.org/scm/viewvc.php/pkg/quantstrat/?root=blotter
[2]: https://r-forge.r-project.org/R/?group_id=316
[3]: http://cran.r-project.org/web/packages/TTR/index.html
[4]: http://cran.r-project.org/web/packages/PerformanceAnalytics/index.html
[5]: http://cran.r-project.org/web/packages/quantmod/index.html

