library(dplyr)
library(ggplot2)
library(scales)


data_files <- list.files(data_folder, full.names = TRUE)

data_files <-
    data_files[grepl(pattern = "USA - Presidential Election State Betting_Florida Winner", data_files)]

get_winner_flor_odds <- function(a_file){
    load(a_file)
    
    output <- 
        data.frame(Date = MarketBook$time,
                   totalMatched = MarketBook$Catalogue$raw$totalMatched,
                   Biden_back = NA,
                   Biden_lay = NA,
                   Trump_back = NA,
                   Trump_lay = NA)
    
    
    if(!is.null(MarketBook$MarketBook[[1]]$runners$`1171580`$ex$back)){
        
        output <- output %>%
            mutate(
                Biden_back = MarketBook$MarketBook[[1]]$runners$`1171581`$ex$back %>% 
                    summarise(max(backPrice)) %>% as.numeric(),
                Biden_lay = MarketBook$MarketBook[[1]]$runners$`1171581`$ex$lay %>% 
                    summarise(min(layPrice)) %>% as.numeric(),
                Trump_back = MarketBook$MarketBook[[1]]$runners$`1171580`$ex$back %>% 
                    summarise(max(backPrice)) %>% as.numeric(),
                Trump_lay = MarketBook$MarketBook[[1]]$runners$`1171580`$ex$lay %>% 
                    summarise(min(layPrice)) %>% as.numeric())
    }
    
    output
}


winner_flor_odds <- 
    do.call("rbind",
            lapply(data_files,get_winner_flor_odds))


winner_flor_odds <- 
    rbind(
        winner_flor_odds %>% 
            mutate(prob = 0.5/Biden_back + 0.5/Biden_lay,
                   ymin = 1/Biden_lay,
                   ymax = 1/Biden_back,
                   runner = "Biden") %>%
            select(Date, prob, ymin, ymax, runner),
        winner_flor_odds %>% 
            mutate(prob = 0.5/Trump_back + 0.5/Trump_lay,
                   ymin = 1/Trump_lay,
                   ymax = 1/Trump_back,
                   runner = "Trump") %>%
            select(Date, prob, ymin, ymax, runner))

winner_flor_odds$runner <- factor(winner_flor_odds$runner, levels = c("Trump", "Biden"))


png("US_pres_elec_2020_betfair_florida_winner-1.png", width = 1000, height = 600)
winner_flor_odds %>% 
    filter(Date > as.POSIXct('2020/11/04')) %>%
    ggplot(aes(x = Date, y = prob, colour = runner)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
    ylab("Probability") +
    xlab("time (GMT)") +
    ylim(c(0,1))+
    ggtitle("US President Market - Florida Winner (Betfair)")
dev.off()
