## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width=6, 
  fig.height=3,
  comment = "#>"
)
hook_output <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(output = function(x, options) {
   lines <- options$output.lines
   if (is.null(lines)) {
     return(hook_output(x, options))  # pass to default hook
   }
   x <- unlist(strsplit(x, "\n"))
   more <- "..."
   if (length(lines)==1) {        # first n lines
     if (length(x) > lines) {
       # truncate the output, but add ....
       x <- c(head(x, lines), more)
     }
   } else {
     x <- c(more, x[lines], more)
   }
   # paste these lines together
   x <- paste(c(x, ""), collapse = "\n")
   hook_output(x, options)
 })


## ----setup, include=FALSE-----------------------------------------------------
library(cols4all)

## -----------------------------------------------------------------------------
knitr::include_graphics("dash-3.png")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("cols4all", dependencies = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("mtennekes/cols4all", dependencies = TRUE)

## ----eval=TRUE----------------------------------------------------------------
library(cols4all)

## ----eval=FALSE---------------------------------------------------------------
#  c4a_gui()

## -----------------------------------------------------------------------------
c4a_types()

c4a_series()

## -----------------------------------------------------------------------------
c4a_overview()

## -----------------------------------------------------------------------------
# Diverging palettes from the 'hcl' series
c4a_palettes(type = "div", series = "hcl")

## -----------------------------------------------------------------------------
# select purple green palette from the hcl series:
c4a("hcl.purple_green", 11)

# get the associated color for missing values
c4a_na("hcl.purple_green")

## ----fig.height = 2-----------------------------------------------------------
c4a_plot("hcl.purple_green", 11, include.na = TRUE)

## -----------------------------------------------------------------------------
library(ggplot2)
data("diamonds")
diam_exp = diamonds[diamonds$price >= 15000, ]

# discrete categorical scale
ggplot(diam_exp, aes(x = carat, y = price, color = color)) +
	geom_point(size = 2) +
	scale_color_discrete_c4a_cat("carto.safe") +
	theme_light()

# continuous diverging scale
ggplot(diam_exp, aes(x = carat, y = depth, color = price)) +
	geom_point(size = 2) +
	scale_color_continuous_c4a_div("wes.zissou1", mid = mean(diam_exp$price)) +
	theme_light()

