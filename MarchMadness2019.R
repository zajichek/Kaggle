#Created: 2019-03-15
#Author: Alex Zajichek
#Description: March madness Kaggle competition 2019

#Load packages
require(tidyverse)

#Path to MM data sets
path <- rstudioapi::selectDirectory("Find directory containing data files.")

#Combine into a list
dat <-
    list(
        Mens = str_remove(str_subset(list.files(path), "^W"), "^W"),
        Womens = str_subset(list.files(path), "^W")
    ) %>%
    
    #Read in data sets
    map(
        function(.x) {
            
            #Set names to retain
            names(.x) <- str_remove(.x, "^W")
            names(.x) <- str_remove(names(.x), "[.]csv$")
            
            #Read in each file
            .x %>%
                map(
                    ~
                        read_csv(
                            str_c(
                                path,
                                .x
                            )
                        )
                )
            
        }
    ) %>%
    
    #Transpose the list elements
    transpose() %>%
    
    #Bind datasets together
    map(
        bind_rows,
        .id = "Tournament"
    )

#Set up data set to answer "What is the probability the high (better) seed beats the low (worse) seed"?
dat$NCAATourneyCompactResults %>%
    
    #Winning team seeds
    inner_join(
        dat$NCAATourneySeeds %>%
            rename(
                WTeamID = TeamID,
                WSeed = Seed
            ),
        by =
            c(
                "Tournament",
                "Season",
                "WTeamID"
            )
    ) %>%
    
    #Losing team seed
    inner_join(
        dat$NCAATourneySeeds %>%
            rename(
                LTeamID = TeamID,
                LSeed = Seed
            ),
        by =
            c(
                "Tournament",
                "Season",
                "LTeamID"
            )
    ) %>%
    
    #Extract the region
    mutate_at(
        vars(
            matches("Seed$")
        ),
        funs(
            as.numeric(
                str_extract(
                    .,
                    pattern = "[0-9]{1,2}"
                )
            )
        )
    ) %>%
    
    #Change to higher (H) vs. lower seed (L)
    mutate(
        Tournament,
        Season,
        DayNum,
        
        #Rearrange columns
        TeamH = 
            case_when(
                WSeed <= LSeed ~ WTeamID,
                TRUE ~ LTeamID
            ),
        TeamL = 
            case_when(
                WSeed <= LSeed ~ LTeamID,
                TRUE ~ WTeamID
            ),
        
        ScoreH = 
            case_when(
                WSeed <= LSeed ~ WScore,
                TRUE ~ LScore
            ),
        ScoreL = 
            case_when(
                WSeed <= LSeed ~ LScore,
                TRUE ~ WScore
            ),
        
        SeedH = 
            case_when(
                WSeed <= LSeed ~ WSeed,
                TRUE ~ LSeed
            ),
        SeedL = 
            case_when(
                WSeed <= LSeed ~ LSeed,
                TRUE ~ WSeed
            )
    )
