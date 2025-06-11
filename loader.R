# Load packages
library(conflicted)

library(xfun)
xfun::pkg_attach2(
  c(
    "extrafont",
    "knitr",
    "fMultivar",
    "IDPmisc",
    "mvtnorm",
    "Matrix",
    "psych",
    "tidyverse",
    "scales",
    "gganimate",
    "ggforce",
    "sjmisc",
    "WJSmisc",
    "tikzDevice",
    "patchwork",
    "qualvar",
    "modeest",
    "tinter",
    "ggfx",
    "ggtext",
    "lemon",
    "signs",
    "psycheval",
    "bezier",
    "DescTools",
    "ggh4x",
    "ggthemes",
    "rsvg",
    "ggarrow",
    "arrowheadr",
    "rgl",
    "ggdiagram",
    "ggbeeswarm",
    "tmvtnorm",
    "rmarkdown",
    "downlit",
    "xml2")
  )


conflicts_prefer(dplyr::select, 
                 dplyr::filter, 
                 scales::alpha, 
                 dplyr::lag,
                 tibble::add_case,
                 ggdiagram::`+`,
                 purrr::discard,
                 readr::col_factor,
                 scales::alpha,
                 scales::rescale,
                 purrr::is_empty,
                 tidyr::replace_na,
                 tidyr::expand,
                 tidyr::pack,
                 tidyr::unpack,
                 tibble::add_case,
                 psycheval::multivariate_ci,
                 sjmisc::`%nin%`,
                 psych::AUC,
                 psych::ICC,
                 psych::SD,
                 ggh4x::geom_pointpath,
                 ggh4x::GeomPointPath,
                 ggdiagram::signs_centered,
                 ggdiagram::distance
)

loadfonts("win", quiet = TRUE)



knitr::knit_hooks$set(webgl = hook_webgl)
# Set options
options(knitr.kable.digits = 2, knitr.kable.na = '')
knitr::opts_template$set(
  marginfigure = list(fig.column = "margin", 
                      fig.cap.location = "top", 
                      out.width = "100%", 
                      fig.align = "left"),
  bodyfigure = list(fig.column = "body", 
                    fig.cap.location = "margin"))

# Default fonts and colors
bfont = "Equity Text A Tab"
bsize = 16
# myfills <- class_color(c("royalblue4", "firebrick4", "#51315E"))@color
# myfills <- viridis::viridis(3, begin = .4, end = .65)
# myfills <- pal_brewer(type = "div", 3)(4)
# myfills <-  hsv(h = degree(c(150,210, 250))@turn, 
#     s = .7, 
#     v = c(.45,.45,.45))[c(2,1,3)]

myfills <- class_color(c("#27408B", "#22734B", "#51315E"))@color
# myfills %>% show_col()

txt_color <- "gray20"
btxt_size = ggtext_size(bsize)
my_arrowhead <- arrow_head_deltoid(2.3)

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
span_style <- function(x, style = "font-family:serif;"
                       ) {
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

# Function to make dice
makedice <- function(i, id, add_blank = TRUE) {
  x = switch(
    i,
    `1` = 0,
    `2` = c(-1, 1),
    `3` = c(-1, 1, 0),
    `4` = c(-1, 1, -1, 1),
    `5` = c(-1, 1, -1, 1, 0),
    `6` = c(-1, 1, -1, 1, -1, 1)
  )
  y = switch(
    i,
    `1` = 0,
    `2` = c(1,-1),
    `3` = c(1,-1, 0),
    `4` = c(1,-1,-1, 1),
    `5` = c(1,-1,-1, 1, 0),
    `6` = c(1,-1,-1, 1, 0, 0))
  
  d <- tibble(id = id * 1,
              i = i,
              x = x,
              y = y)
  
  if (add_blank) {
    d <- d %>%
      add_case(id = id + 0.5,
               i = 0,
               x = NA,
               y = NA)
  }
  d
}

center_neg <- function(x) {
  signs <- sign(x)
  paste0(ifelse(signs < 0,"$",""), x, ifelse(signs < 0,"\\phantom{-}$",""))
}

all_tick_labels <- function(side = 1, at, labels = at) {
  axis(side, labels = rep("",length(at)), at = at)
  for (i in 1:length(at)) {
    axis(side, 
         at = at[i], 
         labels = labels[i],
         tick = F)
  }
}

whitespace <- function(
    size = 10, 
    text = ".", 
    color = "white") {
  paste0("<span style='color:",
         color,
         "; font-size:",
         size, 
         "pt;'>",
         text,
         "</span>")
}

middle_axes <- function(limits = c(0, 5)) {
  breaks <- seq(limits[1], limits[2])
  breaks <- breaks[breaks != 0]
  ggplot() +
    theme_classic(
      base_family = bfont,
      base_size = 18,
      base_line_size = .5
    ) +
    theme(
      axis.text = element_text(color = "gray40"),
      axis.line = element_blank(),
      axis.ticks = element_line(color = "gray"),
      axis.title.x = element_text(
        angle = 0,
        vjust = .5,
        face = "italic",
        color = "gray40"
      ),
      axis.title.y = element_text(
        angle = 0,
        vjust = .5,
        face = "italic",
        color = "gray40"
      )
    ) +
    scale_x_continuous(name = "y", 
                       breaks = breaks, 
                       labels = WJSmisc::signs_centered) +
    scale_y_continuous(name = "x", 
                       breaks = breaks, 
                       labels = signs) +
    ggh4x::coord_axes_inside(
      xlim = limits,
      ylim = limits,
      labels_inside = T,
      ratio = 1
    ) +
    ob_segment(x = c(0, limits[1] - abs(max(limits) - min(limits)) / 20), 
               xend = c(0, limits[2] + abs(max(limits) - min(limits)) / 20), 
               y = c(limits[1] - abs(max(limits) - min(limits)) / 20, 0), 
               yend = c(limits[2] + abs(max(limits) - min(limits)) / 20, 0), 
               linewidth = .75, 
               arrow_head = my_arrowhead, 
               arrow_fins = my_arrowhead, 
               color = "gray")
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
  if (startsWith(options$label, "ex-")) {
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
                       latex = "$\\rm\\LaTeX$ Code",
                       ojs = "Observable Code")
    mycode <- str_match(options$label, "^code(.*?)\\-")
    if (length(mycode) == 2) {
      options$codelabel = codelanguages[mycode[2]]
    }
  }
  return(options)
})

# knitr hooks for quartolive extension
knitr::opts_hooks$set(include = function(options) {
  if (options$engine == "webr" || options$engine == "pyodide") {
    options$include <- TRUE
  }
  options
})

# Passthrough engine for webr
knitr::knit_engines$set(webr = function(options) {
  knitr:::one_string(c(
    "```{webr}",
    options$yaml.code,
    options$code,
    "```"
  ))
})

# Passthrough engine for pyodide
knitr::knit_engines$set(pyodide = function(options) {
  knitr:::one_string(c(
    "```{pyodide}",
    options$yaml.code,
    options$code,
    "```"
  ))
})

