### plotting functions
TAFStatsPlot <- function(TAFStatistics, category, percentage) {
    TAFStatistics$TotStocks <- TAFStatistics$stocks + TAFStatistics$taF_Stocks
    TAFStatistics$stocks_perc <- TAFStatistics$stocks/TAFStatistics$TotStocks * 100
    TAFStatistics$taF_stocks_perc <- TAFStatistics$taF_Stocks/TAFStatistics$TotStocks * 100

    if (isTRUE(percentage)) {
        fig1 <- plot_ly(
            data = TAFStatistics %>% filter(categories == category),
            x = ~year,
            y = ~stocks_perc,
            type = "bar",
            name = "Assessments not in TAF"
        )
        fig1 <- fig1 %>% add_trace(
            y = ~taF_stocks_perc,
            name = "Assessments in TAF"
        )
        fig1 <- fig1 %>% layout(yaxis = list(title = "% of stocks"), barmode = "stack")
    } else {
        fig1 <- plot_ly(
            data = TAFStatistics %>% filter(categories == category),
            x = ~year,
            y = ~stocks,
            type = "bar",
            name = "Assessments not in TAF"
        )
        fig1 <- fig1 %>% add_trace(
            y = ~taF_Stocks,
            name = "Assessments in TAF"
        )
        fig1 <- fig1 %>% layout(yaxis = list(title = "N. of stocks"), barmode = "stack")
    }
    fig1
}