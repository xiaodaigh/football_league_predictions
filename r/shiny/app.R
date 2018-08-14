library(shiny)
library(fst)
library(data.table)
library(magrittr)
library(DT)
elo_sim = fst::read_fst("elo_sim.fst",as.data.table = T)
elo_sim
ntries = 10000

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("EPL prediction"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # shiny::selectizeinput
            shiny::selectizeInput("select_league","League",choices = c("epl","brazil1","csl","finland1","ireland1","mls","norway1","sweden1","sweden2","colombia1","denmark2","switzerland1","ireland2","czech1"),selected="epl")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Winner",DT::DTOutput("dt_winner")),
                tabPanel("Winner wo City",DT::DTOutput("dt_winner_wo_city")),
                tabPanel("Winner wo Big 6",DT::DTOutput("dt_winner_wo_big6")),
                tabPanel("Top 4",DT::DTOutput("dt_top4")),
                tabPanel("Top 6",DT::DTOutput("dt_top6")),
                tabPanel("Relegated",DT::DTOutput("dt_relegated")),
                tabPanel("Not Relegated",DT::DTOutput("dt_not_relegated")),
                tabPanel("Top 10",DT::DTOutput("dt_top10")),
                tabPanel("Bottom 10",DT::DTOutput("dt_bottom10")),
                tabPanel("Bottom",DT::DTOutput("dt_bottom")),
                tabPanel("Not in Top 4",DT::DTOutput("dt_not_in_top4")),
                tabPanel("Top Newcomer",DT::DTOutput("dt_top_newcomer")),
                tabPanel("# Promoted to be relegated",DT::DTOutput("dt_n_promoted_relegated")),
                tabPanel("Points",DT::DTOutput("dt_points")),
                tabPanel("Arsenal",DT::DTOutput("dt_arsenal")),
                tabPanel("Tottenham",DT::DTOutput("dt_tottenham"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dt_points <- renderDT({
        neds_pts = data.table(
            team=c("Arsenal", "Chelsea","Man City","Liverpool","Man United","Tottenham")
            ,pts = c(66.5, 70.5, 84.5, 74.5, 75.5, 73.5)
            )
        
        elo_sim1 = merge(elo_sim, neds_pts, by = "team")
        
        elo_sim1[,.(pts = max(pts), over_elo_odds = round(ntries/sum(V1 > pts),2)
                    , under_elo_odds = round(ntries/sum(V1 < pts),2)), team]
        
    })
    
    output$dt_bottom <- renderDT({
        elo_sim[rank == 20, .(elo_odds = round(ntries/.N,2)),team][order(elo_odds)]
    })
    
    output$dt_n_promoted_relegated <- renderDT({
        if(input$select_league == "epl") {
            elo_sim1 = elo_sim[team %in% c("Wolves","Fulham","Cardiff"),]
            ntrials = elo_sim1[,dplyr::n_distinct(rseed)]
            elo_sim2 = elo_sim1[rank %in% 18:20]
            res1to3 = elo_sim2[,.N,rseed][,.N,.(nteams = N)][order(nteams)]
            res0 = data.table(nteams=0, N = ntrials - res1to3[,sum(N)])
            res0to3 = rbindlist(list(res0,res1to3))
            res0to31= res0to3[order(nteams),.(elo_odds = round(ntrials/N,2))]
            rownames(res0to31) <- c("0","1","2","3")
            return(res0to31)
        } else {
            return()
        }
    })
    
    output$dt_winner <- renderDT({
        if(input$select_league == "epl") {
            return(elo_sim[rank==1,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)])
        } else {
            resabc = readRDS(input$select_league %>% paste0(".rds"))
            return(resabc[[3]])
        }
    })
    
    output$dt_top_newcomer <- renderDT({
        if(input$select_league == "epl") {
            elo_sim1 = elo_sim[team %in% c("Wolves","Fulham","Cardiff"),]
            ntrials = elo_sim1[,dplyr::n_distinct(rseed)]
            return(elo_sim1[order(rank), head(.SD,1), rseed][,.(elo_odds = ntrials/.N),team][order(elo_odds)])
        } else {
            return()
        }
    })
    
    output$dt_winner_wo_city <- renderDT({
        if(input$select_league == "epl") {
            elo_sim1 = elo_sim[team != "Man City", ] 
            elo_sim1[,best_rank_wo_city := min(rank),.(rseed)]
            res = elo_sim1[rank==best_rank_wo_city,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
            return(res)
        } else {
            return()
        }
    })
    
    output$dt_winner_wo_big6 <- renderDT({
        if(input$select_league == "epl") {
            elo_sim1 = elo_sim[!team %in% c("Man City","Man United","Chelsea","Tottenham","Liverpool","Arsenal"),]
            elo_sim1[,best_rank_wo_city := min(rank),.(rseed)]
            res = elo_sim1[rank==best_rank_wo_city,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
            return(res)
        } else {
            return()
        }
    })
    
    output$dt_top4 <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[rank<=4,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
    })
    
    output$dt_top6 <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[rank<=6,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
    })
    
    output$dt_top10 <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[rank<=10,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
    })
    
    output$dt_bottom10 <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[rank>=11,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
    })
    
    output$dt_relegated <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[rank>=18,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
    })
    
    output$dt_not_relegated <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[rank<=17,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
    })
    
    output$dt_not_in_top4 <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[rank>=5,.(`Predicted Odds`=round(ntries/.N,2)),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
    })
    
    output$dt_arsenal <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[team=="Arsenal",.(`Predicted Odds`=round(ntries/.N,2)),.(rank5 = pmin(rank,5))][order(rank5)]
    })
    
    output$dt_tottenham <- renderDT({
        input$select_league
        # elo sim: winner ---------------------------------------------------------
        elo_sim[team=="Tottenham",.(`Predicted Odds`=round(ntries/.N,2)),.(rank5 = pmin(rank,5))][order(rank5)]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

