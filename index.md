---
title: GFUNC | OuMark.ME
author: Mark Ou
---

[OuMark.ME / Projects](https://www.oumark.me/#projects)

<div style='text-align:center; margin-top:5vh'>
  <h1>GFUNC</h1>
  <p>Useful R functions for bioinformatic works</p>
</div>

Functions are not packed as an R package. For usage, just download and include the script or load it directly by the link.

## Manhatten plot

This function is designed to make a Manhattan plot for GWAS.

```R
source("https://raw.githubusercontent.com/oumarkme/GFUNC/main/manhattan.R")
manhattan(gwrst, path = NULL)
```

**Input**

- gwrst: A data frame that includes 3 columns: CHR, BP, P.
- path: Path and prefix for saving plot.

**Output**

- A Manhattan plot saved as `PATH_asso.jpeg`
