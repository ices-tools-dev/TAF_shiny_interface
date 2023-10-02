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
        fig1 <- fig1 %>% layout(yaxis = list(title = "% of stocks"), 
        barmode = "stack",
        font = list(size = 18))
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
        fig1 <- fig1 %>% layout(yaxis = list(title = "N. of stocks"), 
        barmode = "stack",
        font = list(size = 18))
    }
    fig1
}



EGStatsPlot <- function(EGStatistic) {
    
    # Pre-plotting operations
    df2=data.frame(expertGroup=unique(EGStatistic$expertGroup),colors=colorRampPalette(wes_palette("Darjeeling1"))(length(unique(EGStatistic$expertGroup))))
    EGStatistic <- dplyr::inner_join(EGStatistic,df2)
    EGStatistic$percent <- round(EGStatistic$percent, digits = 0)
   
    # Plotting
    g <- highlight_key(EGStatistic, ~expertGroup)
    
    p <- plot_ly(g) %>%
        group_by(expertGroup) %>%
        add_trace(x = ~year, 
                    y = ~percent, 
                    color = ~expertGroup, 
                    mode = 'lines+markers',
                    line = list(shape = 'spline', 
                                smoothing = .9, 
                                width = 4,
                                color = ~colors), 
                    marker = list(size = 8,
                                color = ~colors)) %>%
        layout(xaxis = list(title = "Year"),
                yaxis = list(title = "% of EG stocks in TAF"),
                font = list(size = 18)) %>%
        highlight(on = "plotly_hover", 
                    selected = attrs_selected(showlegend = FALSE))

    
    p
}