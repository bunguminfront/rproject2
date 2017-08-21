install.packages("devtools")
library("devtools")

install_github('ramnathv/slidify')
install_github('ramnathv/slidifyLibraries')

library(slidify)

author("mydeck2")

---
title: "my presentation"
subtitle:
author: LB
job:
framework:io2012 # {html5slides}  
highlighter:highlight.js # {highlight.jst}  
hitheme:tomorrow #   
widgets:[] # {mathjax, quiz, bootstrap}  
mode:selfcontained # {standalone}  
knit:slidify::knit2slides
- --

setwd("C:\\Users\\bungum\\Documents\\mydeck")
slidify('index.Rmd')

package_df <- as.data.frame(installed.packages("/Library/Frameworks/R.framework/Versions/2.15/Resources/library"))
package_list <- as.character(package_df$Package)
system.time(install.packages(package_list))