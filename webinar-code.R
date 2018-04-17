## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
screenshots <- FALSE

## ---- eval=screenshots, message=FALSE, results='hide', echo=FALSE--------
## library(webshot)
## webshot("https://www.r-project.org/", "./img/rweb.png", cliprect = "viewport") %>%
##  resize("75%") %>%
##  shrink()
## webshot("https://cran.r-project.org/", "./img/rcran.png", cliprect = "viewport") %>%
##  resize("75%") %>%
##  shrink()
## webshot("https://cran.r-project.org/web/packages/", "./img/rpackages.png", cliprect = "viewport") %>%
##  resize("75%") %>%
##  shrink()
## webshot("https://cran.r-project.org/web/views/Econometrics.html", "./img/rtveco.png", cliprect = "viewport") %>%
##  resize("75%") %>%
##  shrink()
## webshot("https://cran.r-project.org/web/views/", "./img/rtv.png", cliprect = "viewport") %>%
##  resize("75%") %>%
##  shrink()
## webshot("http://r-es.org/", "./img/rhisp.png", cliprect = "viewport") %>%
##  resize("75%") %>%
##  shrink()
## 

## ----npack, echo=slide, eval=slide---------------------------------------
  nrow(available.packages(filter="CRAN"))

## ---- eval=FALSE---------------------------------------------------------
## install.packages(c("qcc", "SixSigma", "MPCI", "usethis", "readxl"))
## usethis::use_course("https://goo.gl/B3RWHM")

## ---- message = FALSE, fig.align='center', fig.height=4, results='hide'----
library(SixSigma) # Just for the example data set
library(qcc)      # Load package
{{qcc(data = ss.data.density, type = "xbar.one") }}

## ---- message = FALSE, fig.align='center', fig.height=4, results='hide'----
groups <- qcc.groups(ss.data.thickness2$thickness, 
    ss.data.thickness2$ushift)
myqcc <- qcc(data = groups, type = "xbar", plot = FALSE)
{{process.capability(object = myqcc, spec.limits = c(0.718, 0.782))}}

## ---- warning=FALSE, fig.align='center', out.width='50%'-----------------
ss.study.ca(ss.data.ca$Volume, LSL = 740, USL = 760, Target = 750, 
            alpha = 0.5, f.su = "Winery Project")

## ----fig.height=10, fig.width=10-----------------------------------------
imbalance <- readxl::read_excel("data_22514-6-B-2.xlsx")[1:40, 2:3]
head(imbalance)
library(MPCI)

## ------------------------------------------------------------------------
{{mpci(index = "taam", x = imbalance, LSL = c(-140, -140), 
           USL = c(140, 140), Target = c(0, 0), graph = FALSE)}}

## ---- results='hide', fig.keep='none'------------------------------------
ss.rr(time1, prototype, operator, data = ss.data.rr, 
	sub = "Six Sigma Paper Helicopter Project", alphaLim = 0.05,
	errorTerm = "interaction", lsl = 0.7, usl = 1.8)

## ------------------------------------------------------------------------
head(ss.data.rr)

## ---- echo=FALSE, fig.keep='none'----------------------------------------
ss.rr(time1, prototype, operator, data = ss.data.rr, 
	sub = "Six Sigma Paper Helicopter Project", alphaLim = 0.05,
	errorTerm = "interaction", lsl = 0.7, usl = 1.8)

## ---- echo=FALSE, results='hide', fig.align='center'---------------------
ss.rr(time1, prototype, operator, data = ss.data.rr, 
	sub = "Six Sigma Paper Helicopter Project", alphaLim = 0.05,
	errorTerm = "interaction", lsl = 0.7, usl = 1.8)

## ------------------------------------------------------------------------
median_chart <- function(x, n){
  A4 <- c(1.880, 1.187, 0.796, 0.691, 0.548, 
          0.508, 0.433, 0.412, 0.362)
  groups <- rep(1:(length(x)/n), each = n)
  Median <- tapply(x, groups, median)
  ranges <- sapply(tapply(x, groups, range), diff)
  CL <- mean(Median)
  LCL <- CL - A4[n-1]*mean(ranges)
  UCL <- CL + A4[n-1]*mean(ranges)
  lims <- c(min(LCL, min(Median)) - 0.01*min(LCL, min(Median)),
            max(UCL, max(Median)) + 0.01*max(UCL, max(Median)))
  {{plot(Median, pch = 16, type = "b", ylim = lims, 
       col = rgb(0,110/255,190/255), las = 1, xlab = "Subgroup")}}
  grid()
  abline(h = c(CL, LCL, UCL), col = c(rgb(0,200/255,0), 2, 2))
  ## other output
}


## ---- fig.align='center', fig.width=9, out.width="70%"-------------------
median_chart(ss.data.thickness2$thickness, 6)

