install.packages(c('tidyverse', 'truncnorm', 'shiny', 'shinythemes', 'Rfast'))

library(tidyverse)
library(truncnorm) #To set up bounded distribution draws.
library(shiny)
library(shinythemes)
library(Rfast)


#To set up sliders for each of the party's 4 issue positions.
sliders <- function(x,y) {
  sliderInput(inputId = x,
              label= y,
              value= NULL, min = 0, max=100)
} 


ui <- fluidPage(
  # Input() functions
  titlePanel("Zetaland Political Institution Simulator"),
  
  sidebarLayout(
    sidebarPanel("Inputs and Institutions",
                 sliderInput(inputId = "partynum",
                             label = "Number of Parties",
                             value = 2, min = 2, max = 8),
                 actionButton("button", "Run Simulation"),
                 conditionalPanel (
                   condition = "input.button > 0",
                   sliderInput(inputId = "lower",
                               label = "Relative Power of Lower House",
                               value = 50, min = 50, max = 100),
                   radioButtons(inputId = "loweref",
                                label = "Lower House Electoral Formula",
                                choices = c("SMDP" = "smdp", "Alternative Vote" = "av", "2 Round Voting" = "r2", "Prop. Rep." = "pr")),
                   radioButtons(inputId = "upperef",
                                label = "Upper House Electoral Formula",
                                choices = c("SMDP" = "smdp", "Alternative Vote" = "av", "2 Round Voting" = "r2", "Prop. Rep." = "pr")),
                   radioButtons(inputId = "presparl",
                              label = "Presidential or Parliamentary?",
                              choices = c("Presidential" = "presi", "Parliamentary" = "parl")),
                   radioButtons(inputId = "presef",
                                label = "Presidential Electoral Formula",
                                choices = c("Plurality" = "smdp", "Alternative Vote" = "av", "2 Round Voting" = "r2"))
                 )),
    mainPanel("Party Positions and Outputs",
        fluidRow(
          column(3,  textInput(inputId = "pty1name", label = "Party 1 Name"),
              sliders("zetaidpty1", "Party 1 Zeta Assimilation"),
              sliders("gammaindpty1", "Party 1 Gammaland Independence"),
              sliders("urbanidpty1", "Party 1 Urban Bias"),
              sliders("classidpty1", "Party 1 Rich to Poor Redistribution")),
          
          column(3,  textInput(inputId = "pty2name", label = "Party 2 Name"),
              sliders("zetaidpty2", "Party 2 Zeta Assimilation"),
              sliders("gammaindpty2", "Party 2 Gammaland Independence"),
              sliders("urbanidpty2", "Party 2 Urban Bias"),
              sliders("classidpty2", "Party 2 Rich to Poor Redistribution")),
          
          column(3, conditionalPanel(
            condition = "input.partynum > 2",
            textInput(inputId = "pty3name", label = "Party 3 Name"),
            sliders("zetaidpty3", "Party 3 Zeta Assimilation"),
            sliders("gammaindpty3", "Party 3 Gammaland Independence"),
            sliders("urbanidpty3", "Party 3 Urban Bias"),
            sliders("classidpty3", "Party 3 Rich to Poor Redistribution"),
          )),
          
          column(3, conditionalPanel(
            condition = "input.partynum > 3",
            textInput(inputId = "pty4name", label = "Party 4 Name"),
            sliders("zetaidpty4", "Party 4 Zeta Assimilation"),
            sliders("gammaindpty4", "Party 4 Gammaland Independence"),
            sliders("urbanidpty4", "Party 4 Urban Bias"),
            sliders("classidpty4", "Party 4 Rich to Poor Redistribution"),
          ))
        ),

        fluidRow(
          column(3, conditionalPanel(
            condition = "input.partynum > 4",
            textInput(inputId = "pty5name", label = "Party 5 Name"),
            sliders("zetaidpty5", "Party 5 Zeta Assimilation"),
            sliders("gammaindpty5", "Party 5 Gammaland Independence"),
            sliders("urbanidpty5", "Party 5 Urban Bias"),
            sliders("classidpty5", "Party 5 Rich to Poor Redistribution"),
          )),
          
          column(3, conditionalPanel(
            condition = "input.partynum > 5",
            textInput(inputId = "pty6name", label = "Party 6 Name"),
            sliders("zetaidpty6", "Party 6 Zeta Assimilation"),
            sliders("gammaindpty6", "Party 6 Gammaland Independence"),
            sliders("urbanidpty6", "Party 6 Urban Bias"),
            sliders("classidpty6", "Party 6 Rich to Poor Redistribution"),
          )),
          
          column(3, conditionalPanel(
            condition = "input.partynum > 6",
            textInput(inputId = "pty7name", label = "Party 7 Name"),
            sliders("zetaidpty7", "Party 7 Zeta Assimilation"),
            sliders("gammaindpty7", "Party 7 Gammaland Independence"),
            sliders("urbanidpty7", "Party 7 Urban Bias"),
            sliders("classidpty7", "Party 7 Rich to Poor Redistribution"),
          )),
          
          column(3, conditionalPanel(
            condition = "input.partynum > 7",
            textInput(inputId = "pty8name", label = "Party 8 Name"),
            sliders("zetaidpty8", "Party 8 Zeta Assimilation"),
            sliders("gammaindpty8", "Party 8 Gammaland Independence"),
            sliders("urbanidpty8", "Party 8 Urban Bias"),
            sliders("classidpty8", "Party 8 Rich to Poor Redistribution"),
          ))
        ),
              # Output() functions
        fluidRow(
          column(8, plotOutput("seatgraph")),
          column(4, textOutput("smdppres")),
          column(4, textOutput("avpres")),
          column(4, textOutput("r2pres")),
          column(4, textOutput("zetaidmean")),
          column(4, tableOutput('policies'))
        )
        )
    
    
  ),

  

)

server <- function(input, output) {

  
  #save objects to display to output$
  #build objects with render 
  #Access input values with input$
  
  party1 <- eventReactive(input$button, {
    
    #######Population Draw###########
    N <- 10100
    zetaland <- data.frame(id   = numeric(N),
                           urban = numeric(N),
                           gamma = numeric(N))
    
    zetaland$id <- seq.int(nrow(zetaland))
    
    zetaland <- zetaland %>%
      mutate(dist= 1+ floor((id-1)/101)) %>%
      mutate(urban=ifelse(id < 2526, 1, 0)) %>%
      mutate(gamma=ifelse(id > 9090, 1, 0)) %>%
      mutate(zetapct=ifelse(urban==1, 25, ifelse(gamma==0, 54, 2))) %>%
      mutate(alphapct=ifelse(urban==1, 35, ifelse(gamma==0, 35, 1))) %>%
      mutate(betapct=ifelse(urban==1, 39, ifelse(gamma==0, 10, 1))) %>%
      mutate(gammapct=ifelse(urban==1, 1, ifelse(gamma==0, 1, 96))) %>%
      mutate(iddraw = runif(N, 0, 100)) %>%
      mutate(zeta = ifelse(iddraw < zetapct, 1, 0)) %>%
      mutate(alpha = ifelse(iddraw >zetapct & iddraw<(zetapct + alphapct), 1, 0)) %>%
      mutate(beta = ifelse(iddraw >(zetapct + alphapct) & iddraw<(zetapct + alphapct + betapct), 1, 0)) %>%
      mutate(gamman = ifelse(iddraw >(zetapct + alphapct + betapct), 1, 0)) %>%
      mutate(zetaid = ifelse(zeta==1, rtruncnorm(N, a=0, b=100, mean = 70, sd=35), ifelse(gamman==1, rtruncnorm(N, a=0, b=100, mean=10, sd=15),
                                                                                          rtruncnorm(N, a=0, b=100, mean=20, sd=15)))) %>%
      mutate(gammaind = ifelse(gamman==1, rtruncnorm(N, a=0, b=100, mean=100, sd = 15), rtruncnorm(N, a=0, b=100, mean=10, sd = 10))) %>%
      mutate(urbanid = ifelse(urban==1, rtruncnorm(N, a=0, b=100, mean=80, sd=20), rtruncnorm(N, a=0, b=100, mean=20, sd=20))) %>%
      mutate(classid = ifelse(urban==1, rtruncnorm(N, a=0, b=100, mean=50, sd=60), ifelse(beta==1, rtruncnorm(N, a=0, b=100, mean=20, sd=10), 
                                                                                          rtruncnorm(N, a=0, b=100, mean=70, sd=40)))) %>%
      mutate(zetawt = ifelse(urban==1, rtruncnorm(N, a=0, b=100, mean=30, sd=25), ifelse(gamman==1, rtruncnorm(N, a=0, b=100, mean=10, sd=10),
                                                                                         rtruncnorm(N, a=0, b=100, mean=50, sd=25)))) %>%
      mutate(gammawt = ifelse(gamman==1, rtruncnorm(N, a=0, b=100, mean=50, sd = 25), rtruncnorm(N, a=0, b=100, mean=10, sd = 5))) %>%
      mutate(urbanwt = ifelse(urban==1, rtruncnorm(N, a=0, b=100, mean=15, sd=12.5), rtruncnorm(N, a=0, b=100, mean=20, sd=20))) %>%
      mutate(classwt = ifelse(urban==1, rtruncnorm(N, a=0, b=100, mean=45, sd=12.5), rtruncnorm(N, a=0, b=100, mean=20, sd=20))) %>%
      mutate(totwt = zetawt + gammawt + classwt + urbanwt) %>%
      mutate(zetawt = 100*(zetawt/totwt)) %>%
      mutate(gammawt = 100*(gammawt/totwt)) %>%
      mutate(classwt = 100*(classwt/totwt)) %>%
      mutate(urbanwt = 100*(urbanwt/totwt))
    
    #Party Positions
    
    zetaidpty <- c(input$zetaidpty1, input$zetaidpty2, input$zetaidpty3, input$zetaidpty4, 
                   input$zetaidpty5, input$zetaidpty6, input$zetaidpty7, input$zetaidpty8)
    gammaindpty <- c(input$gammaindpty1, input$gammaindpty2, input$gammaindpty3, input$gammaindpty4, 
                     input$gammaindpty5, input$gammaindpty6, input$gammaindpty7, input$gammaindpty8)
    urbanidpty <- c(input$urbanidpty1, input$urbanidpty2, input$urbanidpty3, input$urbanidpty4, 
                    input$urbanidpty5, input$urbanidpty6, input$urbanidpty7, input$urbanidpty8)
    classidpty <- c(input$classidpty1, input$classidpty2, input$classidpty3, input$classidpty4, 
                    input$classidpty5, input$classidpty6, input$classidpty7, input$classidpty8)
    ptyname <- c(input$pty1name, input$pty2name, input$pty3name, input$pty4name,
                 input$pty5name, input$pty6name, input$pty7name, input$pty8name)
    partyall <- data.frame(ptyname, zetaidpty, gammaindpty, urbanidpty, classidpty)
    parties <- head(partyall, input$partynum)
    
    partynum <- input$partynum
    
    
    #Creating voter affinity scores -- how close does each voter feel to each party
    
    for (i in 1:partynum) {
      
      zetaland[[paste("party",i,"aff", sep = "")]] <- ((abs(zetaidpty[i]-zetaland$zetaid))*zetaland$zetawt + 
                                                         (abs(gammaindpty[i]-zetaland$gammaind))*zetaland$gammawt 
                                                       + (abs(urbanidpty[i]-zetaland$urbanid))*zetaland$urbanwt + 
                                                         (abs(classidpty[i]-zetaland$classid))*zetaland$classwt)/100
    }
    
    
    
    zetaland <- data.frame(zetaland, t(apply(zetaland[, c((which(colnames(zetaland)=="party1aff")):
                                                            (which(colnames(zetaland)==paste("party",partynum,"aff", sep = ""))))], 1, rank)))
    
    
    #Tabulating seats won in SMDP
    
    smdp <- data.frame(dist = numeric(100))
    smdp$dist <- seq.int(nrow(smdp))
    smdp$smdpdistwin <- NA
    
    zetasmdp <- zetaland %>%
      select(dist, party1aff.1:paste("party",partynum,"aff.1", sep=""))
    
    for (i in 1:partynum) {
      zetasmdp[[paste("rank",i, sep= "")]] <- zetasmdp[[paste("party",i,"aff.1", sep= "")]] 
      zetasmdp[[paste("party",i,"aff.1", sep= "")]] <- NULL
    }
    
    for (j in 1:100) {
      
      smallsmdp <- zetasmdp %>%
        filter(dist==j)
      
      
      smallsmdp$dist <- NULL
      
      p_rank <- prop.table(colSums(smallsmdp == 1))
      toprank <- which.max(p_rank)
      
      smdp$smdpdistwin[j] <- toprank
      
    }
    
    parties$smdpseat <- NA
    
    for (i in 1:partynum) {
      parties$smdpseat[i] <- length(which(smdp$smdpdistwin == paste(i)))
    }
    
    
    #Tabulating seats won under alternative vote
    #Thanks to the code provided by Brennan Beal here: https://stackoverflow.com/questions/61434966/r-function-to-simulate-ranked-votes
    #Party with lower number of votes in each seat is eliminated.  Those votes then go to the voters' second choice party.
    
    av <- data.frame(dist = numeric(100))
    av$dist <- seq.int(nrow(av))
    av$avwinner <- NA
    
    zetaav <- zetaland %>%
      select(dist, party1aff.1:paste("party",partynum,"aff.1", sep=""))
    
    for (i in 1:partynum) {
      zetaav[[paste("rank",i, sep= "")]] <- zetaav[[paste("party",i,"aff.1", sep= "")]] 
      zetaav[[paste("party",i,"aff.1", sep= "")]] <- NULL
    }
    
    for (j in 1:100) {
      smallav <- zetaav %>%
        filter(dist==j)
      
      smallav$dist <- NULL
      
      majoritycheck <- prop.table(colSums(smallav == 1))
      
      while(any(majoritycheck < 0.5)) {
        
        p_rank <- prop.table(colSums(smallav == 1))
        if(any(p_rank > 0.5)) {
          av$avwinner[j] <- names(p_rank[p_rank > 0.5])
          break
        } else {
          elim_candidate_index <- which.min(p_rank)
          smallav <- smallav[, -elim_candidate_index]
          
          smallav <- t(apply(smallav, 1, rank))
          
          majoritycheck <- prop.table(colSums(smallav == 1))
        }
        
      }
      
    }
    
    
    av$avdistwin <- substr(av$avwinner, 5,5)
    av$avdistwin <- as.numeric(av$avdistwin)
    av$avwinner <- NULL
    
    parties$avseat <- NA
    
    for (i in 1:partynum) {
      parties$avseat[i] <- length(which(av$avdistwin == paste(i)))
    }
    
    
    #Tabulating seats won under 2 round majoritarian
    
    r2 <- data.frame(dist = numeric(100))
    r2$dist <- seq.int(nrow(r2))
    r2$r2distwin <- NA
    
    zetar2 <- zetaland %>%
      select(dist, party1aff.1:paste("party",partynum,"aff.1", sep=""))
    
    for (i in 1:partynum) {
      zetar2[[paste("rank",i, sep= "")]] <- zetar2[[paste("party",i,"aff.1", sep= "")]] 
      zetar2[[paste("party",i,"aff.1", sep= "")]] <- NULL
    }
    
    for (j in 1:100) {
      
      smallr2 <- zetar2 %>%
        filter(dist==j)
      
      smallr22 <- data.frame(matrix("", ncol = 0, nrow=101))
      
      smallr2$dist <- NULL
      
      p_rank <- prop.table(colSums(smallr2 == 1))
      toprank <- which.max(p_rank)
      
      secondrank <- nth(p_rank, 2, descending=T, index.return = T )
      
      if (toprank==secondrank) {    #if there is a tie between the top two parties, you can accidentally get the same party selected twice, so this corrects that.
        secondrank <- nth(p_rank, 1, descending=T, index.return = T )
      }
      
      smallr22$top1 <- smallr2[toprank]
      smallr22$top2 <- smallr2[secondrank]
      
      smallr22 <- t(apply(smallr22, 1, rank))
      
      prank <- prop.table(colSums(smallr22 == 1))
      
      if (prank[1] > prank[2]) {
        r2$r2distwin[j] <- toprank
      } else {
        r2$r2distwin[j] <- secondrank
      }
    }
    
    parties$r2seat <- NA
    
    for (i in 1:partynum) {
      parties$r2seat[i] <- length(which(r2$r2distwin == paste(i)))
    }
    
    #Tabulating seats won under PR
    
    zetaland$votechoice <- NA
    for (i in 1:partynum) {
      zetaland$votechoice <- ifelse(zetaland[[paste("party",i,"aff.1", sep = "")]]==1, paste(i), zetaland$votechoice)
    }
    
    for (i in 1:partynum) {
      parties$votes[i] <- length(which(zetaland$votechoice == paste(i)))
    }
    
    quotients <- data.frame(quot1 = numeric(partynum))
    
    for (i in 1:100) {
      for (j in 1:partynum) {
        quotients[[paste("quot",i,sep = "")]][j] <- parties$votes[j]/i
      }
    }
    
    quot <- as.vector(as.matrix(quotients))
    quot <- sort(quot, decreasing = T)
    
    for(i in 1:partynum) {
      parties$prseat[i] <- length(which(as.numeric(quotients[i,])>quot[101]))
    }
    
    
    #########PRESIDENTIAL ELECTION###########
    
    
    ####SMDP President#####
    
    parties$smdppres <- ifelse(parties$votes==max(parties$votes), 1, 0)
    
    ####AV President######
    
    parties$avpres <- NA
    
    zetaav <- zetaland %>%
      select(party1aff.1:paste("party",partynum,"aff.1", sep=""))
    
    for (i in 1:partynum) {
      zetaav[[paste("rank",i, sep= "")]] <- zetaav[[paste("party",i,"aff.1", sep= "")]] 
      zetaav[[paste("party",i,"aff.1", sep= "")]] <- NULL
    }
    
    majoritycheck <- prop.table(colSums(zetaav == 1))
    
    while(any(majoritycheck < 0.5)) {
      
      p_rank <- prop.table(colSums(zetaav == 1))
      if(any(p_rank > 0.5)) {
        majwin <- names(p_rank[p_rank > 0.5])
        majwin <- substr(majwin, 5, 5)
        majwin <- as.numeric(majwin)
        parties$avpres[majwin] <- 1
        break
      } else {
        elim_candidate_index <- which.min(p_rank)
        zetaav <- zetaav[, -elim_candidate_index]
        
        zetaav <- t(apply(zetaav, 1, rank))
        
        majoritycheck <- prop.table(colSums(zetaav == 1))
      }
    }
    
    parties$avpres <- ifelse(is.na(parties$avpres), 0, 1)
    
    ####2-Round President######
    
    zetar2 <- zetaland %>%
      select(party1aff.1:paste("party",partynum,"aff.1", sep=""))
    
    for (i in 1:partynum) {
      zetar2[[paste("rank",i, sep= "")]] <- zetar2[[paste("party",i,"aff.1", sep= "")]] 
      zetar2[[paste("party",i,"aff.1", sep= "")]] <- NULL
    }
    
    zetar2finals <- data.frame(matrix("", ncol = 0, nrow=N))
    
    p_rank <- prop.table(colSums(zetar2 == 1))
    toprank <- which.max(p_rank)
    
    secondrank <- nth(p_rank, 2, descending=T, index.return = T )
    
    if (toprank==secondrank) {    #if there is a tie between the top two parties, you can accidentally get the same party selected twice, so this corrects that.
      secondrank <- nth(p_rank, 1, descending=T, index.return = T )
    }
    
    zetar2finals$top1 <- zetar2[toprank]
    zetar2finals$top2 <- zetar2[secondrank]
    
    zetar2finals <- t(apply(smallr22, 1, rank))
    
    prank <- prop.table(colSums(zetar2finals == 1))
    
    parties$r2pres <- NA
    
    if (prank[1] > prank[2]) {
      parties$r2pres[toprank] <- 1
    } else {
      parties$r2pres[secondrank] <- 1
    }
    
    parties$r2pres <- ifelse(is.na(parties$r2pres), 0, 1)
    
    party1 <- parties
    
    
  })
  
  output$seatgraph <- renderPlot({
    party2 <- party1() %>%
      select(ptyname, votes, smdpseat, avseat, r2seat, prseat) %>%
      rename(smdp = smdpseat, av = avseat, r2=r2seat, pr = prseat) %>%
      reshape(varying = c("votes", "smdp", "av", "r2", "pr"),
              v.names = "seatvotes",
              timevar = "inst",
              times = c("votes", "smdp", "av", "r2", "pr"),
              direction = "long")
    
    
    party3 <- party1() %>%
      select(ptyname, votes, smdpseat, avseat, r2seat, prseat) %>%
      mutate(votespct = votes/sum(votes)) %>%
      mutate(smdppct = smdpseat/sum(smdpseat)) %>%
      mutate(avpct = avseat/sum(avseat)) %>%
      mutate(r2pct = r2seat/sum(r2seat)) %>%
      mutate(prpct = prseat/sum(prseat)) %>%
      select(ptyname, votespct, smdppct, avpct, r2pct, prpct) %>%
      rename(votes=votespct, smdp = smdppct, av = avpct, r2=r2pct, pr = prpct) %>%
      reshape(varying = c("votes", "smdp", "av", "r2", "pr"),
              v.names = "percent",
              timevar = "inst",
              times = c("votes", "smdp", "av", "r2", "pr"),
              direction = "long")
    
    party4 <- party2 %>%
      full_join(party3, by = c("ptyname", "inst"))
    ggplot(party4, aes(fill=ptyname, y=percent, x=inst)) +
      geom_bar(position="fill", stat="identity") +
      geom_text(aes(label = seatvotes), position = position_stack(vjust = 0.5))  +
      labs(x = "Votes or Electoral Formula", y = "Total Votes or Seats Won")
  })
  
  output$smdppres <- renderText({
    party2 <- party1()
    smdppres <- party2$ptyname[which(party2$smdppres==1)]
    paste("Presidential Plurality Winner:", smdppres)
  })
  
  output$avpres <- renderText({
    party2 <- party1()
    avpres <- party2$ptyname[which(party2$avpres==1)]
    paste("Presidential Alternative Vote Winner:", avpres)
  })
  
  output$r2pres <- renderText({
    party2 <- party1()
    r2pres <- party2$ptyname[which(party2$r2pres==1)]
    paste("Presidential Two-Round Vote Winner:", r2pres)
  })
  
  output$policies <- renderTable({
    
    parties <- party1()
    partynum <- input$partynum
    zetaidpty <- c(input$zetaidpty1, input$zetaidpty2, input$zetaidpty3, input$zetaidpty4, 
                   input$zetaidpty5, input$zetaidpty6, input$zetaidpty7, input$zetaidpty8)
    gammaindpty <- c(input$gammaindpty1, input$gammaindpty2, input$gammaindpty3, input$gammaindpty4, 
                     input$gammaindpty5, input$gammaindpty6, input$gammaindpty7, input$gammaindpty8)
    urbanidpty <- c(input$urbanidpty1, input$urbanidpty2, input$urbanidpty3, input$urbanidpty4, 
                    input$urbanidpty5, input$urbanidpty6, input$urbanidpty7, input$urbanidpty8)
    classidpty <- c(input$classidpty1, input$classidpty2, input$classidpty3, input$classidpty4, 
                    input$classidpty5, input$classidpty6, input$classidpty7, input$classidpty8)
    
    lower <- input$lower
    
    presi <- ifelse(input$presparl=="presi", 1, 0)
    
    lowerefsmdp <- ifelse(input$loweref=="smdp", 1, 0)
    lowerefav <- ifelse(input$loweref=="av", 1, 0)
    lowerefr2 <- ifelse(input$loweref=="r2", 1, 0)
    lowerefpr <- ifelse(input$loweref=="pr", 1, 0)
    
    upperefsmdp <- ifelse(input$upperef=="smdp", 1, 0)
    upperefav <- ifelse(input$upperef=="av", 1, 0) 
    upperefr2 <- ifelse(input$upperef=="r2", 1, 0)
    upperefpr <- ifelse(input$upperef=="pr", 1, 0)
    
    pressmdp <- ifelse(input$presef=="smdp", 1, 0)
    presav <- ifelse(input$presef=="av", 1, 0)
    presr2 <- ifelse(input$presef=="r2", 1, 0)

    
    parties$avaff <- NA
    
    smdplargest <- NA
    avlargest <- NA
    r2largest <- NA
    prlargest <- NA
    
    
    for (i in 1:partynum) {
      parties$smdpgovt <- ifelse(parties$smdpseat==max(parties$smdpseat), 1, 0)
      parties$smdpgovtseats <- max(parties$smdpseat)
      smdplargest <- ifelse(parties$smdpseat[i]==max(parties$smdpseat), paste(i), smdplargest)
      
      parties$avgovt <- ifelse(parties$avseat==max(parties$avseat), 1, 0)
      parties$avgovtseats <- max(parties$avseat)
      avlargest <- ifelse(parties$avseat[i]==max(parties$avseat), paste(i), avlargest)
      
      parties$r2govt <- ifelse(parties$r2seat==max(parties$r2seat), 1, 0)
      parties$r2govtseats <- max(parties$r2seat)
      r2largest <- ifelse(parties$r2seat[i]==max(parties$r2seat), paste(i), r2largest)
      
      parties$prgovt <- ifelse(parties$prseat==max(parties$prseat), 1, 0)
      parties$prgovtseats <- max(parties$prseat)
      prlargest <- ifelse(parties$prseat[i]==max(parties$prseat), paste(i), prlargest)
    }
    
    smdplargest <- as.numeric(smdplargest)
    avlargest <- as.numeric(avlargest)
    r2largest <- as.numeric(r2largest)
    prlargest <- as.numeric(prlargest)
    
    
    for (i in 1:partynum) {
      parties$smdplrgaff[i] <- (abs(zetaidpty[smdplargest]-zetaidpty[i]) + abs(gammaindpty[smdplargest]-gammaindpty[i]) +
                                  abs(urbanidpty[smdplargest]-urbanidpty[i]) + abs(classidpty[smdplargest]-classidpty[i]))
      
      parties$avlrgaff[i] <- (abs(zetaidpty[avlargest]-zetaidpty[i]) + abs(gammaindpty[avlargest]-gammaindpty[i]) +
                                abs(urbanidpty[avlargest]-urbanidpty[i]) + abs(classidpty[avlargest]-classidpty[i]))
      
      parties$r2lrgaff[i] <- (abs(zetaidpty[r2largest]-zetaidpty[i]) + abs(gammaindpty[r2largest]-gammaindpty[i]) +
                                abs(urbanidpty[r2largest]-urbanidpty[i]) + abs(classidpty[r2largest]-classidpty[i]))
      
      parties$prlrgaff[i] <- (abs(zetaidpty[prlargest]-zetaidpty[i]) + abs(gammaindpty[prlargest]-gammaindpty[i]) +
                                abs(urbanidpty[prlargest]-urbanidpty[i]) + abs(classidpty[prlargest]-classidpty[i]))
      
    }
    
    parties$smdplrgaff[smdplargest] <- 1000
    parties$avlrgaff[avlargest] <- 1000
    parties$r2lrgaff[r2largest] <- 1000
    parties$prlrgaff[prlargest] <- 1000
    
    while (parties$smdpgovtseats<51) {
      parties$smdpaffmin <- min(parties$smdplrgaff, na.rm=T)
      parties$smdpgovt <- ifelse(parties$smdplrgaff==parties$smdpaffmin | parties$smdplrgaff==1000, 1, parties$smdpgovt)
      parties$smdpgovtseats <- sum(parties$smdpseat[parties$smdpgovt==1])
      parties$smdplrgaff <- ifelse(parties$smdplrgaff==parties$smdpaffmin, 1000, parties$smdplrgaff)
    }
    
    while (parties$avgovtseats<51) {
      parties$avaffmin <- min(parties$avlrgaff, na.rm=T)
      parties$avgovt <- ifelse(parties$avlrgaff==parties$avaffmin | parties$avlrgaff==1000, 1, parties$avgovt)
      parties$avgovtseats <- sum(parties$avseat[parties$avgovt==1])
      parties$avlrgaff <- ifelse(parties$avlrgaff==parties$avaffmin, 1000, parties$avlrgaff)
    }
    
    while (parties$r2govtseats<51) {
      parties$r2affmin <- min(parties$r2lrgaff, na.rm=T)
      parties$r2govt <- ifelse(parties$r2lrgaff==parties$r2affmin | parties$r2lrgaff==1000, 1, parties$r2govt)
      parties$r2govtseats <- sum(parties$r2seat[parties$r2govt==1])
      parties$r2lrgaff <- ifelse(parties$r2lrgaff==parties$r2affmin, 1000, parties$r2lrgaff)
    }
    
    while (parties$prgovtseats<51) {
      parties$praffmin <- min(parties$prlrgaff, na.rm=T)
      parties$prgovt <- ifelse(parties$prlrgaff==parties$praffmin | parties$prlrgaff==1000, 1, parties$prgovt)
      parties$prgovtseats <- sum(parties$prseat[parties$prgovt==1])
      parties$prlrgaff <- ifelse(parties$prlrgaff==parties$praffmin, 1000, parties$prlrgaff)
    }
    
    
    
    ##Average Policy Outputs for each electoral formula -- government party members average policy position weighted by seat share#####
    
    parties$zetaptysmdpoutput <- sum(parties$zetaidpty*parties$smdpseat*parties$smdpgovt)/parties$smdpgovtseats
    parties$gammaindptysmdpoutput <- sum(parties$gammaindpty*parties$smdpseat*parties$smdpgovt)/parties$smdpgovtseats
    parties$urbanptysmdpoutput <- sum(parties$urbanidpty*parties$smdpseat*parties$smdpgovt)/parties$smdpgovtseats
    parties$classptysmdpoutput <- sum(parties$classidpty*parties$smdpseat*parties$smdpgovt)/parties$smdpgovtseats
    
    parties$zetaptyavoutput <- sum(parties$zetaidpty*parties$avseat*parties$avgovt)/parties$avgovtseats
    parties$gammaindptyavoutput <- sum(parties$gammaindpty*parties$avseat*parties$avgovt)/parties$avgovtseats
    parties$urbanptyavoutput <- sum(parties$urbanidpty*parties$avseat*parties$avgovt)/parties$avgovtseats
    parties$classptyavoutput <- sum(parties$classidpty*parties$avseat*parties$avgovt)/parties$avgovtseats
    
    parties$zetaptyr2output <- sum(parties$zetaidpty*parties$r2seat*parties$r2govt)/parties$r2govtseats
    parties$gammaindptyr2output <- sum(parties$gammaindpty*parties$r2seat*parties$r2govt)/parties$r2govtseats
    parties$urbanptyr2output <- sum(parties$urbanidpty*parties$r2seat*parties$r2govt)/parties$r2govtseats
    parties$classptyr2output <- sum(parties$classidpty*parties$r2seat*parties$r2govt)/parties$r2govtseats
    
    parties$zetaptyproutput <- sum(parties$zetaidpty*parties$prseat*parties$prgovt)/parties$prgovtseats
    parties$gammaindptyproutput <- sum(parties$gammaindpty*parties$prseat*parties$prgovt)/parties$prgovtseats
    parties$urbanptyproutput <- sum(parties$urbanidpty*parties$prseat*parties$prgovt)/parties$prgovtseats
    parties$classptyproutput <- sum(parties$classidpty*parties$prseat*parties$prgovt)/parties$prgovtseats
    
    ##Average policy outputs for each potential presidential formula
    
    parties$zetapressmdpoutput <- sum(parties$zetaidpty*parties$smdppres)
    parties$gammaindpressmdpoutput <- sum(parties$gammaindpty*parties$smdppres)
    parties$urbanpressmdpoutput <- sum(parties$urbanidpty*parties$smdppres)
    parties$classpressmdpoutput <- sum(parties$classidpty*parties$smdppres)
    
    parties$zetapresavoutput <- sum(parties$zetaidpty*parties$avpres)
    parties$gammaindpresavoutput <- sum(parties$gammaindpty*parties$avpres)
    parties$urbanpresavoutput <- sum(parties$urbanidpty*parties$avpres)
    parties$classpresavoutput <- sum(parties$classidpty*parties$avpres)
    
    parties$zetapresr2output <- sum(parties$zetaidpty*parties$r2pres)
    parties$gammaindpresr2output <- sum(parties$gammaindpty*parties$r2pres)
    parties$urbanpresr2output <- sum(parties$urbanidpty*parties$r2pres)
    parties$classpresr2output <- sum(parties$classidpty*parties$r2pres)
    
    
    ########THE FINAL POLICY OUTPUTS####################
    
    
    zetaid <- (presi*(parties$zetapressmdpoutput[1]*pressmdp + parties$zetapresavoutput[1]*presav + parties$zetapresr2output[1]*presr2) +
                 (lower*(parties$zetaptysmdpoutput[1]*lowerefsmdp + parties$zetaptyavoutput[1]*lowerefav + parties$zetaptyr2output[1]*lowerefr2 + parties$zetaptyproutput[1]*lowerefpr)/100 +
                    (100-lower)*(parties$zetaptysmdpoutput[1]*upperefsmdp + parties$zetaptyavoutput[1]*upperefav + parties$zetaptyr2output[1]*upperefr2 + parties$zetaptyproutput[1]*upperefpr)/100  )
    )/(1+presi)
    
    
    gammaind <- (presi*(parties$gammaindpressmdpoutput[1]*pressmdp + parties$gammaindpresavoutput[1]*presav + parties$gammaindpresr2output[1]*presr2) +
                   (lower*(parties$gammaindptysmdpoutput[1]*lowerefsmdp + parties$gammaindptyavoutput[1]*lowerefav + parties$gammaindptyr2output[1]*lowerefr2 + parties$gammaindptyproutput[1]*lowerefpr)/100 +
                      (100-lower)*(parties$gammaindptysmdpoutput[1]*upperefsmdp + parties$gammaindptyavoutput[1]*upperefav + parties$gammaindptyr2output[1]*upperefr2 + parties$gammaindptyproutput[1]*upperefpr)/100  )
    )/(1+presi)
    
    urban <- (presi*(parties$urbanpressmdpoutput[1]*pressmdp + parties$urbanpresavoutput[1]*presav + parties$urbanpresr2output[1]*presr2) +
                (lower*(parties$urbanptysmdpoutput[1]*lowerefsmdp + parties$urbanptyavoutput[1]*lowerefav + parties$urbanptyr2output[1]*lowerefr2 + parties$urbanptyproutput[1]*lowerefpr)/100 +
                   (100-lower)*(parties$urbanptysmdpoutput[1]*upperefsmdp + parties$urbanptyavoutput[1]*upperefav + parties$urbanptyr2output[1]*upperefr2 + parties$urbanptyproutput[1]*upperefpr)/100  )
    )/(1+presi)
    
    
    class <- (presi*(parties$classpressmdpoutput[1]*pressmdp + parties$classpresavoutput[1]*presav + parties$classpresr2output[1]*presr2) +
                (lower*(parties$classptysmdpoutput[1]*lowerefsmdp + parties$classptyavoutput[1]*lowerefav + parties$classptyr2output[1]*lowerefr2 + parties$classptyproutput[1]*lowerefpr)/100 +
                   (100-lower)*(parties$classptysmdpoutput[1]*upperefsmdp + parties$classptyavoutput[1]*upperefav + parties$classptyr2output[1]*upperefr2 + parties$classptyproutput[1]*upperefpr)/100  )
    )/(1+presi)
    
    
    policies <- data.frame(zetaid, gammaind, urban, class)
    
    
  })

  }
  
  




shinyApp(ui = ui, server = server)
