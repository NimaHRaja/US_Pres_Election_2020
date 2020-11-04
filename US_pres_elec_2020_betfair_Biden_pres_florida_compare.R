library(dplyr)
library(ggplot2)
library(scales)


data_files <- list.files(data_folder, full.names = TRUE)

data_files <-
    data_files[grepl(pattern = "Presidential Election 2020_Next President", data_files) |
                   grepl(pattern = "USA - Presidential Election State Betting_Pennsylvania Winner", data_files) #|
                #   grepl(pattern = "USA - Presidential Election State Betting_Florida Winner", data_files)
               ]

get_biden_pres_flor_penn_odds <- function(a_file){
    load(a_file)
    
    selection_Id <- MarketBook$MarketBook[[1]]$runner_names %>%
        filter(runnerName %in% c("Joe Biden", "Democrats")) %>%
        select(selectionId) %>% as.character()
    
    back <- MarketBook$MarketBook[[1]]$runners[[selection_Id]]$ex$back
    
    lay <- MarketBook$MarketBook[[1]]$runners[[selection_Id]]$ex$lay
    
    back <- if(is.null(back)){NA}else{back %>% 
            summarise(max(backPrice)) %>% as.numeric()}
    
    lay <- if(is.null(lay)){NA}else{lay %>% 
            summarise(min(layPrice)) %>% as.numeric()}
    
    data.frame(Date = MarketBook$time,
               Market = MarketBook$Catalogue$market$marketName,
               back = back,
               lay = lay)
}


biden_pres_flor_penn_odds <- 
    do.call("rbind",
            lapply(data_files,get_biden_pres_flor_penn_odds))

biden_pres_flor_penn_odds <- 
    biden_pres_flor_penn_odds %>% 
    mutate(prob = 0.5/back + 0.5/lay)

# png("US_pres_elec_2020_betfair_Biden_pres_pennsylvania_compare-1.png", width = 1000, height = 600)
biden_pres_flor_penn_odds %>% 
    filter(Date > as.POSIXct('2020/11/04')) %>% 
    ggplot(aes(x = Date, y = prob, colour = Market)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = 1/lay, ymax = 1/back)) +
    ylab("Probability") +
    xlab("time (GMT)") +
    ggtitle("Comparing probabilities of Joe Biden's winning the election and winnig Pennsylvania (Betfair)")
# dev.off()
