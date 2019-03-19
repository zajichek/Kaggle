#Created: 2019-03-15
#Author: Alex Zajichek
#Description: March madness Kaggle competition 2019

#Load packages
require(tidyverse); require(randomForest)

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
                                "/",
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

#Extract the seed number
dat$NCAATourneySeeds <-
    dat$NCAATourneySeeds %>%
    mutate(Seed =
               as.numeric(
                   str_extract(Seed, pattern = "[0-9]{1,2}")
               )
    )

#####Make a lookup table of regular season statistics for each team
#Get a frame of unique teams per season
teams_reg_season <- 
    dat$RegularSeasonDetailedResults %>%
    select(
        Tournament,
        Season,
        DayNum,
        WTeamID,
        LTeamID
    ) %>%
    
    #Gather to key value pairs
    gather(
        key = "key",
        value = "TeamID",
        -Tournament,
        -Season,
        -DayNum
    ) %>%
    select(-key, -DayNum) %>%
    distinct()

#Get all stats and summarise over the season
teams_reg_season <- 
    teams_reg_season %>%
    inner_join(
        y = 
            dat$RegularSeasonDetailedResults %>%
            select(
                -DayNum,
                -WLoc,
                -NumOT
            ) %>%
            
            #Remove leading letter for winning team
            rename_at(
                vars(
                    matches("^W")
                ),
                str_remove,
                pattern = "^W"
            ) %>%
            
            #Replace leading letter of losing team with 'O'
            rename_at(
                vars(
                    matches("^L")
                ),
                str_replace,
                pattern = "^L",
                replacement = "O"
            ),
        
        by = 
            c(
                "Tournament",
                "Season",
                "TeamID"
            )
    ) %>%
    
    #Bind with all losing games
    bind_rows(
        teams_reg_season %>%
            inner_join(
                y = 
                    dat$RegularSeasonDetailedResults %>%
                    select(
                        -DayNum,
                        -WLoc,
                        -NumOT
                    ) %>%
                    
                    #Remove leading letter
                    rename_at(
                        vars(
                            matches("^L")
                        ),
                        str_remove,
                        pattern = "^L"
                    ) %>%
                    
                    #Replace leading letter of losing team with 'O'
                    rename_at(
                        vars(
                            matches("^W")
                        ),
                        str_replace,
                        pattern = "^W",
                        replacement = "O"
                    ),
                
                by = 
                    c(
                        "Tournament",
                        "Season",
                        "TeamID"
                    )
            ) 
    ) %>%
    select(-OTeamID) %>%
    
    #Compute new statistics
    #Possestions = field goals attempted - offensive rebounds + turnovers + (0.4 x free throws attempted) 
    mutate(
        Poss = FGA - OR + TO + .4*FTA,
        OPoss = OFGA - OOR + OTO + .4*OFTA,
        PPP = Score/Poss,
        OPPP = OScore/Poss,
        PointDifferential = Score - OScore,
        Win = Score > OScore,
        FTP = FTM/FTA,
        OFTP = OFTM/OFTA,
        FGP = FGM/FGA,
        OFGP = OFGM/OFGA
    ) %>%
    
    #Gather stats into key-value pairs
    gather(
        key = "Attribute",
        value = "Value",
        -Tournament,
        -Season,
        -TeamID
    ) %>%
    
    #Group and compute summaries
    group_by(
        Tournament,
        Season,
        TeamID,
        Attribute
    ) %>%
    summarise(
        Average = mean(Value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    
    #Make an indicator for team vs. opponent attribute
    mutate(
        Whose =
            case_when(
                str_detect(Attribute, "^O") & nchar(Attribute) > 2 ~ "Opponent",
                TRUE ~ "Team"
            ),
        Attribute = 
            case_when(
                Whose == "Opponent" ~ str_remove(Attribute, "^O"),
                TRUE ~ Attribute
            )
    ) %>%
    
    #Spread averages to columns for team vs. opponent
    spread(
        key = Whose,
        value = Average
    ) %>%
    
    #Compute the difference in each measure
    transmute(
        Tournament,
        Season,
        TeamID,
        Attribute,
        Value =
            case_when(
                is.na(Opponent) ~ Team,
                TRUE ~ Team - Opponent
            )
    ) %>% 
    
    #Now spread attributes back to columns
    spread(
        key = Attribute,
        value = Value
    )

#Set up data set to answer "What is the probability the high (better) seed beats the low (worse) seed"?
set.seed(123)
dat$NCAATourneyCompactResults %>%
    
    #Bind with all possible match-ups for 2019
    bind_rows(
        dat$NCAATourneySeeds %>%
            
            #Extract 2019 teams
            filter(Season == 2019) %>%
            
            #Split and apply by mens/womens
            split(.$Tournament) %>%
            
            map_df(
                ~
                    .x %>%
                    pull(TeamID) %>%
                    combn(2) %>%
                    t() %>%
                    as_tibble() %>%
                    transmute(
                        Season = 2019,
                        WTeamID = V1,
                        LTeamID = V2
                    ),
                .id = "Tournament"
            )
    ) %>%
    
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
    
    #Change to higher (H) vs. lower seed (L)
    transmute(
        Tournament,
        Season,
        DayNum,
        
        #Rearrange columns
        Team_H = 
            case_when(
                WSeed <= LSeed ~ WTeamID,
                TRUE ~ LTeamID
            ),
        Team_L = 
            case_when(
                WSeed <= LSeed ~ LTeamID,
                TRUE ~ WTeamID
            ),
        
        Score_H = 
            case_when(
                WSeed <= LSeed ~ WScore,
                TRUE ~ LScore
            ),
        Score_L = 
            case_when(
                WSeed <= LSeed ~ LScore,
                TRUE ~ WScore
            ),
        
        Seed_H = 
            case_when(
                WSeed <= LSeed ~ WSeed,
                TRUE ~ LSeed
            ),
        Seed_L = 
            case_when(
                WSeed <= LSeed ~ LSeed,
                TRUE ~ WSeed
            )
    ) %>%
    group_by(Tournament) %>%
    
    #Define outcomes
    mutate(
        Differential = scale(Score_H - Score_L),
        HWins = Score_H > Score_L
    ) %>%
    select(
        -Score_H,
        -Score_L
    ) %>%
    ungroup() %>%
    
    #Get regular season stats
    inner_join(
        y = teams_reg_season,
        by =
            c(
                "Tournament",
                "Season",
                "Team_H" = "TeamID"
            )
    ) %>%
    inner_join(
        y = teams_reg_season,
        by =
            c(
                "Tournament",
                "Season",
                "Team_L" = "TeamID"
            ),
        suffix = c("_H", "_L")
    ) %>%
    
    #Add a index to keep track of games
    add_column(
        Index = 1:nrow(.)
    ) %>%
    select(
        Index,
        everything()
    ) %>%
    
    #Gather all statistics to key-value pairs
    gather(
        key = "Attribute",
        value = "Value",
        -Index,
        -Tournament,
        -Season,
        -DayNum,
        -Team_H,
        -Team_L,
        -Differential,
        -HWins
    ) %>%
    
    #Extract tail from attribute
    separate(
        Attribute,
        into = c("Attribute", "Whose")
    ) %>%
    
    #Spread to columns
    spread(
        key = Whose,
        value = Value
    ) %>%
    
    #Compute difference in attributes
    mutate(
        Value = H - L
    ) %>%
    select(
        -H, -L
    ) %>%
    
    #Send attributes back to columns
    spread(
        key = Attribute,
        value = Value
    ) %>%
    
    #Remove some variables
    select(
        -Index,
        -DayNum,
        -FGA,
        -FGM,
        -FTA,
        -FTM,
        -PointDifferential,
        -TO,
        -Score
    ) %>%
    
    ####Build models for mens and womens separately
    split(.$Tournament) %>%
    imap(
        function(.d, .tournament) {
            
            #1) Separate out 2019 data
            dat_train <- 
                .d %>%
                filter(Season != 2019) %>%
                select(
                    -Tournament,
                    -Team_H,
                    -Team_L
                )
            
            #2) 10-fold cross validation
            caret::createFolds(1:nrow(dat_train)) %>%
                
                #Repeat for each fold
                map_df(
                    function(.fold_i) {
                        
                        #Make train/test set
                        train_i <- dat_train[-.fold_i,]
                        test_i <- dat_train[.fold_i,]
                        
                        #Run a model for each combination of mtry/nodesize
                        list(
                            mtry = c(1, sqrt(ncol(train_i) - 2), (ncol(train_i) - 2)/3, ncol(train_i) - 2),
                            nodesize = c(1, 3)
                        ) %>%
                            
                            #Get each combination
                            cross() %>%
                            
                            #Run model
                            map_df(
                                function(.params_j) {
                                    
                                    ##(a) Run models
                                    #Continous outcome
                                    mod1 <- 
                                        randomForest(
                                            Differential ~ .,
                                            data = train_i %>% select(-HWins),
                                            mtry = .params_j$mtry,
                                            nodesize = .params_j$nodesize,
                                            ntree = 500
                                        )
                                    
                                    #Binary outcome
                                    mod2 <-
                                        randomForest(
                                            factor(HWins) ~ .,
                                            data = train_i %>% select(-Differential),
                                            mtry = .params_j$mtry,
                                            nodesize = .params_j$nodesize,
                                            ntree = 500
                                        )
                                    
                                    #(b) Get predictions
                                    pred1 <- predict(mod1, newdata = test_i) %>% unname()
                                    pred1 <- 1/(1 + exp(-pred1))
                                    pred2 <- predict(mod2, newdata = test_i, type = "prob")[,2]
                                    
                                    #Compute log-loss
                                    actual <- test_i %>% pull(HWins)
                                    
                                    
                                    #Make a tibble
                                    tibble(
                                        mtry = .params_j$mtry,
                                        nodesize = .params_j$nodesize,
                                        Continuous = -1*mean(actual*log(pred1) + (1-actual)*log(1-pred1)),
                                        Binary = -1*mean(actual*log(pred2) + (1-actual)*log(1-pred2))
                                    )
                                    
                                }
                            )
                    },
                    .id = "Fold"
                ) %>%
                
                #Average over the folds
                group_by(
                    mtry,
                    nodesize
                ) %>%
                summarise(
                    Continuous = mean(Continuous, na.rm = TRUE),
                    Binary = mean(Binary, na.rm = TRUE)
                ) %>% 
                ungroup() %>%
                
                #Get best set of hyperparameters for each model
                gather(
                    key = "Response",
                    value = "LogLoss",
                    -mtry,
                    -nodesize
                ) %>%
                group_by(
                    Response
                ) %>%
                
                #Find best set of hyperparameters by logloss
                top_n(-1, LogLoss) %>%
                
                #3) Run a final model for each response type
                split(.$Response) %>%
                
                imap(
                    function(.rf_params, .response) {
                        
                        #Get prediction data
                        pred_dat <-
                            .d %>%
                            filter(Season == 2019)
                        
                        #Check which model
                        if(.response == "Continuous") {
                            
                            pred <- 
                                predict(
                                    randomForest(
                                        Differential ~ .,
                                        data = dat_train %>% select(-HWins),
                                        ntree = 10000,
                                        nodesize = .rf_params$nodesize[1],
                                        mtry = .rf_params$mtry[1]
                                    ),
                                    newdata = pred_dat
                                ) %>% unname()
                            pred <- 1/(1 + exp(-pred))
                                
                            
                        } else {
                            
                            pred <- 
                                predict(
                                    randomForest(
                                        factor(HWins) ~ .,
                                        data = dat_train %>% select(-Differential),
                                        ntree = 10000,
                                        nodesize = .rf_params$nodesize[1],
                                        mtry = .rf_params$mtry[1]
                                    ),
                                    newdata = pred_dat,
                                    type = "prob"
                                )[,2] %>% unname()
                        }
                        
                        pred_dat %>%
                            select(
                                Season,
                                Team_H,
                                Team_L
                            ) %>%
                        bind_cols(
                            Pred = pred
                        )
                    }
                ) %>%
                bind_rows(
                    .id = "Response"
                ) %>%
                
                #Make data correct for Kaggle submission
                transmute(
                    Season,
                    T1 =
                        case_when(
                            Team_H < Team_L ~ Team_H,
                            TRUE ~ Team_L
                        ),
                    T2 = 
                        case_when(
                            Team_H < Team_L ~ Team_L,
                            TRUE ~ Team_H
                        ),
                    Pred = 
                        case_when(
                            Team_H < Team_L ~ Pred,
                            TRUE ~ 1 - Pred
                        ),
                    Response
                ) %>%
                
                #Arrange by teams
                arrange(
                    T1,
                    T2
                ) %>%
                
                #Concatenate columns
                unite(
                    col = ID,
                    Season,
                    T1,
                    T2
                ) %>%
                
                #Split by the model response
                split(.$Response) %>%
                
                #Write a csv for each model
                imap(
                    function(.frame, .outcome) {
                        .frame %>%
                            select(
                                -Response
                            )  %>%
                            write_csv(
                                path = str_c(path, "/MarchMadness2019_", .outcome, "_", .tournament, "_", Sys.Date(), ".csv")
                            )
                    }
                )
        }
    )

