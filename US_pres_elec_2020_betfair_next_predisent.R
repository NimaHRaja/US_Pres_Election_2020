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

next_pres_odds <- 
    rbind(
        next_pres_odds %>% 
            mutate(prob = 0.5/Biden_back + 0.5/Biden_lay,
                   ymin = 1/Biden_lay,
                   ymax = 1/Biden_back,
                   runner = "Biden") %>%
            select(Date, prob, ymin, ymax, runner),
        next_pres_odds %>% 
            mutate(prob = 0.5/Trump_back + 0.5/Trump_lay,
                   ymin = 1/Trump_lay,
                   ymax = 1/Trump_back,
                   runner = "Trump") %>%
            select(Date, prob, ymin, ymax, runner))




next_pres_odds <- 
    next_pres_odds %>% 
    mutate(prob = 0.5/Biden_back + 0.5/Biden_lay) 

next_pres_odds$runner <- factor(next_pres_odds$runner, levels = c("Trump", "Biden"))


png("US_pres_elec_2020_betfair_next_predisent-1.png", width = 1000, height = 600)
next_pres_odds %>% 
    filter(Date > as.POSIXct('2020/11/04')) %>%
    ggplot(aes(x = Date, y = prob, colour = runner)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
    ylab("Probability") +
    xlab("time (GMT)") +
    ylim(c(0,1))+
    ggtitle("Next US President Market (Betfair)")
dev.off()
