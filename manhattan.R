library(tidyverse)
library(compiler)

manhattanPlot = function(asso, saveDir = NULL, pAdjust = 'none', cutRatio = 0, pointAlpha = 0.8, pointSize = 0.8, figWidth = 15, figHeight = 4, pointCol = c('dimgrey', 'grey')){   
    
    asso = asso %>% mutate(nlogFDR = -log10(p.adjust(.$P, method = pAdjust))) %>% filter(.$nlogFDR >= quantile(.$nlogFDR, cutRatio))
    
    if(length(unique(asso$CHR)) > 1){
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
            scale_x_continuous( label=axisdf$CHR, breaks=axisdf$center, name = 'Chromosome' ) +
            scale_y_continuous( expand = c(0, 0), limits = c(min(asso$nlogFDR), round(max(asso$nlogFDR)*1.1)), name='-log10(p-value)') +
            theme_bw() +
            theme(
                legend.position='none',
                panel.border = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
            )
    }else{
        returnPlot = ggplot( asso, aes(x = BP/1e6, y = nlogFDR) ) +
            geom_point( color = pointCol[1], alpha = pointAlpha, size = pointSize) +
            scale_y_continuous( expand = c(0, 0), limits = c(min(asso$nlogFDR), round(max(asso$nlogFDR)*1.1)), name='-log10(p-value)') +
            scale_x_continuous( name = 'Position (Mb)') +
            theme_bw() +
                theme(
                    legend.position='none',
                    panel.border = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank()
                )
    }
    
    
    if(is_null(saveDir)){
        return(returnPlot)
    }else{
        ggsave(saveDir, returnPlot, width = figWidth, height = figHeight, dpi = 300, limitsize = FALSE)
        cat(paste('Figure saved:', saveDir))
    }
}

manhattanPlot = cmpfun(manhattanPlot)
