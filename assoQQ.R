library(tidyverse)
library(compiler)

assoQQ = function(gwrst, path = NULL){
    p = ggplot(data = gwrst, aes(sample = P)) +
        stat_qq() +
        geom_abline(slope = 1, color = "brown3") +
        theme_bw()
    ggsave(paste0(path, "_assoQQ.jpeg"), p, width = 5, height = 5)
}
assoQQ = cmpfun(assoQQ)
