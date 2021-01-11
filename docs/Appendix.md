
# Appendix{-}

We are still going to work with the Hillenbrand et al. data, again focusing on variation in f0. We are again going to add a variable indicating 'adultness', and one indicating gender. However, this time we are going to work with all four groups at the same time: `b` (boys), `g` (girls), `m` (men), and `w` (women).


```r
options (contrasts = c("contr.sum","cont.sum"))
url1 = "https://raw.githubusercontent.com/santiagobarreda"
url2 = "/stats-class/master/data/h95_vowel_data.csv"
h95 = read.csv (url(paste0 (url1, url2)))[,c('f0','speaker','group')]
## set up colors for plotting
source (url(paste0 (url1, "/stats-class/master/data/colors.R")))
## source functions
devtools::source_url (paste0 (url1, "/stats-class/master/data/functions.R"))

## make variable that indicates if the talker is an adult
h95$adult = ""
h95$adult[h95$group %in% c('w','m')] = "adult"
h95$adult[h95$group %in% c('g','b')] = "child"
## make variable indicating speaker gender
h95$gender = "female"
h95$gender[h95$group %in% c('b','m')] = "male"
## make speaker number a factor
h95$speaker = factor (h95$speaker)  
```







```r
f0 = h95$f0
par (mfrow = c(1,2), mar = c(4,4,1,1))
plot (f0)
plot (f0, xlim = c(-100, 1800), ylim = c(50,360))
```

<img src="Appendix_files/figure-html/unnamed-chunk-2-1.png" width="672" />




```r
f0 = h95$f0[1:20]
par (mfrow = c(2,2), mar = c(4,4,1,1))
plot (f0, ylim=c(80,190),xlim=c(0,21))
plot (f0, pch = 1:20, col = 1:20, ylim=c(80,190),xlim=c(0,21))
plot (f0, pch = 1:20, col = 1:20, cex = 4, ylim=c(80,190),xlim=c(0,21))
plot (f0, pch = 1:20, col = 1:20, cex = 4, lwd = 2, ylim=c(80,190),xlim=c(0,21))
```

<img src="Appendix_files/figure-html/unnamed-chunk-3-1.png" width="672" />





```r
f0 = h95$f0[1:20]
par (mfrow = c(1,3), mar = c(4,4,1,1))
plot (f0, col = 4,cex=2,pch=16)
plot (f0, col = 4,cex=2,pch=16, type = 'b')
plot (f0, col = 4,cex=2,pch=16, type = 'l')
```

<img src="Appendix_files/figure-html/unnamed-chunk-4-1.png" width="672" />




```r
f0 = h95$f0[1:20]
par (mfrow = c(3,2), mar = c(4,4,1,1))
plot (f0, col = 4,cex=2,pch=16, type = 'l')
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 3)
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 2, lty = 1)
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 2, lty = 2)
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 2, lty = 3)
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 2, lty = 4)
```

<img src="Appendix_files/figure-html/unnamed-chunk-5-1.png" width="672" />







```r
f0 = h95$f0[1:20]
par (mfrow = c(1,4), mar = c(2,2,1,1))
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 3)
par (mar = c(4,4,1,1))
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 3)
par (mar = c(6,6,1,1))
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 3)
par (mar = c(8,8,1,1))
plot (f0, col = 4,cex=2,pch=16, type = 'l', lwd = 3)
```

<img src="Appendix_files/figure-html/unnamed-chunk-6-1.png" width="672" />





```r
f0 = h95$f0[1:20]
par (mfrow = c(1,3), mar = c(4.2,4.2,1,1))
plot (f0, col = 4,cex=2,pch=16, xlab = "Observations", ylab = "f0 (Hz)",
      main = "Main Title")
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "")
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
```

<img src="Appendix_files/figure-html/unnamed-chunk-7-1.png" width="672" />





```r
f0 = h95$f0[1:20]
par (mfrow = c(1,4), mar = c(4.2,4.2,1,1))
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
axis (side = 1, at = 1:20)
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
axis (side = 1, at = 1:20, labels = (1:20)+20)
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
axis (side = 1, at = 1:20, labels = rep('text',20))
```

<img src="Appendix_files/figure-html/unnamed-chunk-8-1.png" width="672" />





```r
f0 = h95$f0[1:20]
par (mfrow = c(1,4), mar = c(4.2,4.2,1,1), oma = c(4,0,0,0))
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
axis (side = 1, at = 1:20)
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
axis (side = 1, at = 1:20, labels = (1:20)+20)
plot (f0, col = 4,cex=2,pch=16, xlab = "", ylab = "",xaxt='n',yaxt='n')
axis (side = 1, at = 1:20, labels = rep('text',20))

mtext (side = 1, outer = TRUE, "Label For all Three", cex = 1.5, line = 1)
```

<img src="Appendix_files/figure-html/unnamed-chunk-9-1.png" width="672" />




















