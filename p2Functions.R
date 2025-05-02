
#For newer stocks/shorter time frames. 

LovelyCandle <- function(tikr, tx, sbtx, bks, range1, range2) {
  p_price <- stocks %>% filter(Ticker == tikr) %>%  
    ggplot(aes(x = Date, open = Open, high = High, low = Low, close = Close)) +
    geom_candlestick() +
    geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) +
    theme_bw() +
    labs(
      y = 'Price - USD',
      x = 'Date',
      title = tx, 
      subtitle = sbtx
    ) +
    scale_y_continuous(labels = label_currency()) + 
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = bks,
                 limits = as.Date(c(range1, range2))
    )
  
  p_volume <- stocks %>% 
    filter(Ticker == tikr) %>% 
    ggplot(aes(x = Date, y = Volume)) +
    geom_col(fill = "grey60") +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "Volume") +
    scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) + 
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = bks,
                 limits = as.Date(c(range1, range2)))
  
  p_price / p_volume + plot_layout(heights = c(3, 1))
  
  
  
}



#For older stocks/larger time frames
LovelyStep <- function(tikr, tx, sbtx, bks, range2) {
  
  min_stock_date <- stocks %>%
    filter(Ticker == tikr, !is.na(Close)) %>%
    summarize(min_date = min(Date)) %>%
    pull(min_date)
  
  
  p_price <- stocks %>% 
    filter(Ticker == tikr) %>% 
    ggplot(aes(x = Date, y = Close)) +
    geom_step(color = "black") +  # step line fixed color, no legend
    geom_ma(aes(linetype = "50-day MA", color = "50-day MA"), ma_fun = SMA, n = 50, linewidth = 1.25) +
    geom_ma(aes(linetype = "200-day MA", color = "200-day MA"), ma_fun = SMA, n = 200, linewidth = 1.25) +
    theme_light() +
    labs(
      y = 'Price (USD)',
      x = 'Date',
      title = tx,
      subtitle = sbtx,
      linetype = "Moving Average",   # Custom legend titles
      color = "Moving Average"
    ) +
    scale_color_manual(values = c(
      "50-day MA" = "blue",
      "200-day MA" = "red"
    )) +
    scale_linetype_manual(values = c(
      "50-day MA" = 5,
      "200-day MA" = 2,
      "1-year MA" = 3,
      "2-year MA" = 4
    )) +
    scale_y_continuous(labels = label_currency()) +
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = bks,
                 limits = as.Date(c(min_stock_date, range2)))
  
  
  
  p_volume <- stocks %>% 
    filter(Ticker == tikr) %>% 
    ggplot(aes(x = Date, y = Volume)) +
    geom_col(fill = "grey60") +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "Volume",
         colour = "SMA") +
    scale_y_continuous(labels = label_number(scale = 1e-7, suffix = "M")) 
  
  p_price / p_volume + plot_layout(heights = c(3, 1))
  
}



#For reproducibility 
set.seed(123)



# For historical events
LovelyNNARModel <- function(tikr, tx, sbtx, bks, range1, range2, euler) {
  
  # 1. Prepare stock data
  tikr_data <- stocks %>%
    filter(Ticker == tikr) %>%
    arrange(Date)
  tikr_data <- tikr_data %>% drop_na(Close)
  
  
  # 2. Event window 
  event_data <- tikr_data %>%
    filter(Date >= as.Date(range1) & Date <= as.Date(range2))
  
  
  # 3. Prepare series
  stock_series <- ts(tikr_data$Close, frequency = 252)  # about 252 trading days/year
  
  # 4. Define training sets
  train_expected_series <- ts(
    tikr_data %>% filter(Date < as.Date(range1)) %>% pull(Close),
    frequency = 252
  )
  
  train_actual_series <- ts(
    tikr_data %>% filter(Date >= as.Date(range1) & Date <= as.Date(range2)) %>% pull(Close),
    frequency = 252
  )
  
  # 5. Fit NNAR models
  set.seed(123)
  model_expected <- nnetar(train_expected_series)
  model_actual <- nnetar(train_actual_series)
  
  # 6. Predict for event window
  event_data$Expected_Close <- forecast(model_expected, h = nrow(event_data))$mean
  event_data$Actual_Predicted_Close <- forecast(model_actual, h = nrow(event_data))$mean
  
  #7. Damage Ribbon
  event_data <- event_data %>%
    mutate(
      ymin = pmin(Expected_Close, Actual_Predicted_Close),
      ymax = pmax(Expected_Close, Actual_Predicted_Close)
    )
  # 7. Plot predictions vs actual
  p_price <- event_data %>%
    ggplot(aes(x = Date)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.3) +
    geom_step(aes(y = Close, color = "Close"), color = "black") +
    geom_line(aes(y = Expected_Close, color = "Expected"), color = "blue", size = 1.1 ) +
    geom_line(aes(y = Actual_Predicted_Close, color = "Predicted"), color = "red", size = 1.1 ) +
    theme_bw() +
    labs(
      y = 'Price',
      x = 'Date',
      title = tx,
      subtitle = sbtx,
      color = "Legend"
    ) +
    scale_color_manual(values = c(
      "Actual Price" = "black",
      "Expected (Pre-Event Model)" = "blue",
      "Event-Influenced Model" = "red"
    )) +
    scale_linetype_manual(values = c(
      "Actual Price" = "solid",
      "Expected (Pre-Event Model)" = "dashed",
      "Event-Influenced Model" = "solid"
    )) +
    scale_y_continuous(labels = label_currency()) +
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = bks,
                 limits = as.Date(c(range1, range2)))
  
  # Volume subplot
  p_volume <- event_data %>%
    ggplot(aes(x = Date, y = Volume)) +
    geom_col(fill = "grey60") +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "Volume") +
    scale_y_continuous(labels = label_number(scale = euler, suffix = "M"))
  
  # 8. Combine plots
  p_price / p_volume + plot_layout(heights = c(3, 1))
}




LovelyEvent <- function(tikr, event, tx, sbtx, bks, euler) {
  if (!(event %in% names(Timeframes))) stop("Invalid event name")
  
  range <- as.Date(Timeframes[[event]])
  
  LovelyNNARModel(
    tikr = tikr,
    tx = tx,
    sbtx = sbtx,
    bks = bks,
    range1 = range[1],
    range2 = range[2],
    euler = euler
  )
}



LovelyMultiNNARModel <- function(event, bks, euler) {
  tikrs <- c("AMZN", "NVDA", "COST", "WMT", "JNJ", "UNH", "ZSPC", "INLF")
  range <- as.Date(Timeframes[[event]])
  # Checks to see if event is in Timeframes list, stops and produces error statement. 
  if (!(event %in% names(Timeframes))) stop("Invalid event name")
  
  # Build a list of patchwork plots
  plots <- map(tikrs, function(tikr) {
    
    # Check if ticker has enough historical data
    tikr_data <- stocks %>% 
      filter(Ticker == tikr) %>%
      arrange(Date)
    
    # Removes NA rows from training data (historical) 
    valid_training_data <- tikr_data %>%
      filter(Date < as.Date(range[1]), !is.na(Close))
    # checks to see if I have enough pre-event data for models to be useful
    if (nrow(valid_training_data) < 2) {
      message(paste("Skipping", tikr, "- not enough valid pre-event Close prices"))
      return(NULL)
    }
    # checks to see if I have enough datapointst all for ticker/event
    if (nrow(tikr_data) < 5) {
      message(paste0("Skipping ", tikr, ": Not enough data points for event ", event))
      return(NULL)
    }
    
    
    LovelyNNARModel(
      tikr = tikr,
      tx = tikr,
      sbtx = NULL,
      bks = bks,
      range1 = range[1],
      range2 = range[2],
      euler = euler
    )
  }) %>% compact()  # Removes NULLs
  
  # all plots combined
  wrap_plots(plots, ncol = 2) & 
    theme(legend.position = "bottom")
}

Brandish <- function(tikr) {
  stocks %>% 
    filter(Ticker == tikr) %>%
    filter(Date == max(Date)) %>% 
    select(Date, Ticker, Close, Volume, Volatility, Sector, Market_Cap)
}
