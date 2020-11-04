library(dplyr)
library(ggplot2)
library(scales)


data_files <- list.files(data_folder, full.names = TRUE)

data_files <-
    data_files[grepl(pattern = "Presidential Election 2020_Next President", data_files)]

get_next_pres_odds <- function(a_file){
    load(a_file)
    
    data.frame(Date = MarketBook$time,
               totalMatched = MarketBook$Catalogue$raw$totalMatched,
               Biden_back = MarketBook$MarketBook[[1]]$runners$`6816445`$ex$back %>% 
                   summarise(max(backPrice)) %>% as.numeric(),
               Biden_lay = MarketBook$MarketBook[[1]]$runners$`6816445`$ex$lay %>% 
                   summarise(min(layPrice)) %>% as.numeric(),
               Trump_back = MarketBook$MarketBook[[1]]$runners$`10874213`$ex$back %>% 
                   summarise(max(backPrice)) %>% as.numeric(),
               Trump_lay = MarketBook$MarketBook[[1]]$runners$`10874213`$ex$lay %>% 
                   summarise(min(layPrice)) %>% as.numeric())
}


next_pres_odds <- 
    do.call("rbind",
            lapply(data_files,get_next_pres_odds))


# png("US_pres_elec_2020_betfair_tot_matched-1.png", width = 800, height = 600)
next_pres_odds %>% 
    ggplot(aes(x = Date, y = totalMatched)) + 
    geom_point(colour = "blue") + 
    scale_y_continuous(limits = c(0,NA),
                       labels = unit_format(unit = "M", scale = 1e-6)) +
    ylab("Total Matched (£)") +
    ggtitle("Next US President Market - Total Matched (Betfair)")
# dev.off()


# png("US_pres_elec_2020_betfair_tot_matched-3.png", width = 800, height = 600)
next_pres_odds %>% 
    filter(Date > as.POSIXct('2020/11/02')) %>%
    ggplot(aes(x = Date, y = totalMatched)) + 
    geom_point(colour = "blue") + 
    scale_y_continuous(
                       labels = unit_format(unit = "M", scale = 1e-6)) +
    ylab("Total Matched (£)") +
    xlab("time (GMT)") +
    ggtitle("Next US President Market - Total Matched (Betfair)")
# dev.off()
