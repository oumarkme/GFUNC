library(tidyverse)
library(compiler)

manhattanPlot = function(asso, pointAlpha = 0.8, pointSize = 0.8, pointCol = c('grey', 'skyblue'), saveDir = NULL){
    asso = asso %>% mutate(nlogFDR = -log10(p.adjust(.$P, method = 'fdr')))
    don = asso %>%
        group_by(CHR) %>%
        summarise(chr_len=max(BP)) %>%
        mutate(tot=cumsum(chr_len) - chr_len) %>%
        select(-chr_len) %>%
        left_join(asso, ., by=c("CHR"="CHR")) %>%
        arrange(CHR, BP) %>%
        mutate(BPcum = BP + tot)
    axisdf = don %>%group_by(CHR) %>% summarise(center = (max(BPcum) + min(BPcum))/2)
    
    returnPlot = ggplot( don, aes(x = BPcum, y = nlogFDR) ) +
        geom_point( aes(color=as.factor(CHR)), alpha = pointAlpha, size = pointSize ) +
        scale_color_manual( values = rep(pointCol, length(unique(don$CHR))) ) +
        scale_x_continuous( label=axisdf$CHR, breaks=axisdf$center ) +
        scale_y_continuous( expand = c(0, 0), limits = c(0, round(max(asso$nlogFDR)*1.25)) ) +
        theme_bw() +
        theme(
            legend.position='none',
            panel.border = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    
    if(is.null(saveDir)){
        return(returnPlot)
    }else{
        ggsave(saveDir, returnPlot, width = 20, height = 4)
        cat(paste('Figure saved:', saveDir))
    }
}

manhattanPlot = cmpfun(manhattanPlot)
