---
  title: "Example"
subtitle: "SUBTITLE"
author: "AUTHORS"
date: "`r format(Sys.Date(), '%d %b %Y')`"
output:
  html_document: 
  df_print: paged
number_sections: yes
toc: yes
toc_float: true
toc_depth: 3
code_folding: show
editor_options:
  chunk_output_type: inline
---
  
  # $\LaTeX$ Math
  
  This is just markdown that can include latex math.

$$
  \begin{align}
\dot{x} & = \sigma(y-x) \\
\dot{y} & = \rho x - y - xz \\
\dot{z} & = -\beta z + xy
\end{align}
$$
  
  # System Info
  
  ```{r}
# session info
sessionInfo()
```

# Data

```{r}
# data
iris
```

# Plot

```{r, fig.width=6, fig.height=4}
library(ggplot2)
ggplot(iris) + 
  aes(Sepal.Length, Sepal.Width, color = Species) +
  geom_point() + 
  theme_bw()
```