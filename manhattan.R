library(tidyverse)
library(compiler)

manhattan = function(gwrst, path = NULL){
    if(is.null(path)) path = paste0(getwd(), "/plot")
    
    don <- gwrst %>%
        group_by(CHR) %>%
        summarise(chr_len = max(BP)) %>%
        mutate(tot=cumsum(chr_len)-chr_len) %>%
        select(-chr_len) %>%
        left_join(gwrst, ., by=c("CHR"="CHR")) %>%
        arrange(CHR, BP) %>%
        mutate(BPcum=BP+tot)
    axisdf <- don %>%
        group_by(CHR) %>% 
        summarize(center=(max(BPcum) + min(BPcum) ) / 2)
    p = ggplot(don, aes(x=BPcum, y=-log10(P))) +
        geom_point(aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
        scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
        scale_x_continuous(label = axisdf$CHR, breaks= axisdf$center) +
        scale_y_continuous(expand = c(0, 0) ) +
        theme_bw() +
        theme( 
            legend.position="none",
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    ggsave(paste0(path, "_asso.jpeg"), p, width = 12, height = 5)
}
manhattan = cmpfun(manhattan)
