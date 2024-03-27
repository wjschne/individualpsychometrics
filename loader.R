# Load packages
library(conflicted)
conflicts_prefer(dplyr::select, 
                 dplyr::filter, 
                 scales::alpha, 
                 dplyr::lag,
                 tibble::add_case)
library(extrafont)
loadfonts("win", quiet = TRUE)
library(knitr)
library(sn)
library(fMultivar)
library(IDPmisc)
library(psych)
library(tidyverse)
library(scales)

library(gganimate)
library(ggforce)
library(sjmisc)
library(WJSmisc)
# library(tippy)
library(tikzDevice)
library(patchwork)
library(qualvar)
library(modeest)
library(tinter)
library(ggfx)
library(ggtext)
library(lemon)
library(signs)

library(psycheval)
library(bezier)
library(DescTools)
library(ggh4x)
library(ggthemes)
library(rsvg)
library(ggarrow)
library(arrowheadr)
library(rgl)
knitr::knit_hooks$set(webgl = hook_webgl)
arrow_head_deltoid <- purrr::partial(arrowheadr::arrow_head_deltoid, d = 2.3)
# Set options
options(knitr.kable.digits = 2, knitr.kable.na = '')
knitr::opts_template$set(marginfigure = list(fig.column = "margin", fig.cap.location = "top", out.width = "100%", fig.align = "left"),
                         bodyfigure = list(fig.column = "body", fig.cap.location = "margin"))

# Default fonts and colors
bfont = "Equity Text A Tab"
bsize = 16
myfills <- c("royalblue4", "firebrick4", "#51315E")
txt_color <- "gray20"

btxt_size = ggtext_size(bsize)

# Default geoms and themes
ggplot2::update_geom_defaults(
  "text",
  list(family = bfont, 
       size = btxt_size,
       color = txt_color))
ggplot2::update_geom_defaults(
  "label",
  list(
    family = bfont,
    size = btxt_size,
    label.padding = unit(0, "lines"),
    label.size = 0,
    color = txt_color))

ggplot2::update_geom_defaults(
  "richtext",
  list(family = bfont, 
       size = btxt_size,
       color = txt_color))

ggplot2::update_geom_defaults("density", list(fill = myfills[1]))

theme_set(theme_minimal(base_size = bsize, base_family = bfont))


# font family
span_style <- function(x, style = "font-family:serif") {
  paste0('<span style=\"',
         style,
         '\">',
         x,
         "</span>")
}


# Set table column width
# https://github.com/rstudio/bookdown/issues/122#issuecomment-221101375
html_table_width <- function(kable_output, width, tag = "</caption>"){
  width_html <- paste0(
    paste0('<col width="',
           width,
           '">'),
    collapse = "\n")
  sub(tag,
      paste0(tag,
             "\n",
             width_html),
      kable_output)
}

# Make a matrix with braces
bmatrix <- function(M, brace = "bmatrix", includenames=TRUE) {
  if (includenames) {
    M <- cbind(rownames(M),M)
    M <- rbind(colnames(M), M)
  }
  M <-  paste(apply(M,
                    MARGIN = 1,
                    FUN = paste0,
                    collapse = " & "),
              collapse = "\\\\\n")


  if (!is.null(brace)) {
    M <- paste0("\\begin{",brace,"}\n", M, "\n\\end{", brace , "}")
    }
  M
}

# Hooks -------

# # Enclose collapsible r chunk in  button
# knitr::opts_hooks$set(button_r = function(options) {
#   if (isTRUE(options$button_r)) {
#     options$button_before_r <- TRUE
#     options$button_after <- TRUE
#     options$echo = TRUE
#     options$eval = FALSE
#   }
# 
#   options
# })
# 
# # Enclose collapsible latex chunk in  button
# knitr::opts_hooks$set(button_latex = function(options) {
#   if (isTRUE(options$button_latex)) {
#     options$button_before_latex <- TRUE
#     options$button_after <- TRUE
#     options$echo = TRUE
#     options$eval = FALSE
#   }
# 
#   options
# })

# before button for collapsible r chunk
knit_hooks$set(
  button_before = function(before, options, envir) {
    if (before) {
      if (is.null(options$figlabel)) {
        l <- options$label %>% 
          str_remove("^coder\\-") %>% 
          str_remove("^codelatex\\-") %>% 
          str_remove("^codeojs\\-") 
        if (str_detect(l, "^fig\\-") | str_detect(l, "^tbl\\-")) {
          options$figlabel <- l
        }
        
      } 
      
      codetype <- options$codelabel
      if (!is.null(options$figlabel)) {
        codetype <- paste0(codetype, " for @", options$figlabel)
      } 
      return(
        paste0(
          # '<div class="wrap-collapsible" style="margin-top: 1em">',
          # "\n",
          # '<input id="collapsible-',
          # options$label,
          # '" class="toggle" type="checkbox">',
          # "\n",
          # '<label for="collapsible-',
          # options$label,
          # '" class="lbl-toggle">', codetype,'</label>',
          # '<div class="collapsible-content">',
          # "\n",
          # '<div class="content-inner">'
          ':::{.callout-note collapse="true" appearance="minimal"}\n## ',codetype
        )
      )
    }
  }
)


# After button for collapsible chunks
knit_hooks$set(button_after = function(before, options, envir) {
  # if (!before) return('</div></div></div>')
  if (!before) return('\n:::\n')
})


# Solution chunk
# knitr::opts_hooks$set(solution = function(options) {
#   options$echo <- TRUE
#   options$solutionsetter <- TRUE
#   return(options)
# })

knitr::knit_hooks$set(solutionsetter = function(before,options, envir) {
  
  if (before) {
    
    "\n\n<details><summary>Suggested Solution</summary>\n\n"
  } else {
    
    "\n\n</details>\n\n"
  }
})


# Make all chunks with demo-prefix echo = TRUE

knitr::opts_hooks$set(label = function(options) {
  if (startsWith(options$label, "demo-")) {
    options$echo <- TRUE
  }
  if (startsWith(options$label, "solution-")) {
    options$echo <- TRUE
    options$solutionsetter <- TRUE
  }
  if (str_starts(options$label, "code")) {
    options$button_before <- TRUE
    options$button_after <- TRUE
    options$echo = TRUE
    options$eval = FALSE
    codelanguages <- c(r = "R Code", 
                       latex = "$\\small\\rm\\LaTeX$ Code",
                       ojs = "Observable Code")
    mycode <- str_match(options$label, "^code(.*?)\\-")
    if (length(mycode) == 2) {
      options$codelabel = codelanguages[mycode[2]]
    }
  }
  return(options)
})
