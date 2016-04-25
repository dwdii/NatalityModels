#
# This R script is included in R markdown to initialize the  environment with
# libraries and commonly used objects, functions.
#

library(ggplot2)
library(gridExtra)
library(knitr)
library(car)
library(knitcitations)
library(RefManageR)

cleanbib()

cite_options(style="markdown")

myTheme <- theme(axis.ticks=element_blank(),  
                 panel.border = element_rect(color="gray", fill=NA), 
                 panel.background=element_rect(fill="#FBFBFB"), 
                 panel.grid.major.y=element_line(color="white", size=0.5), 
                 panel.grid.major.x=element_line(color="white", size=0.5),
                 plot.title=element_text(size="8"))

rotateXaxisLabels <- theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Setup for KNITR Table Captions
tn = local({
  i = 0
  function(x) {
    i <<- i + 1
    paste('\n\n:', ' ', x, sep = '')
    # The : before Table tells pandoc to wrap your caption in <caption></caption>
  }
})
knit_hooks$set(tab.cap = function(before, options, envir) {
  if(!before)
    tn(options$tab.cap)
})
default_output_hook = knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
  if (is.null(options$tab.cap) == F)  
    x
  else
    default_output_hook(x,options)
})

# Disable scientific notation
options(scipen=999)

