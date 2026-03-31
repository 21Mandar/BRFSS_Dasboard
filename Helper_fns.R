# ================= MERGE FUNCTIONS =====================

merge_ResponseID <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, c(
    "RESP025"="RESP137", "RESP026"="RESP172", "RESP029"="RESP141",
    "RESP230"="RESP020", "RESP231"="RESP020", "RESP232"="RESP020",
    "RESP196"="RESP199", "RESP197"="RESP199", "RESP198"="RESP199"
  ))
  x
}

merge_Response <- function(ResponseID, Response) {
  Response <- as.character(Response)
  Response[str_detect(ResponseID, "RESP137")] <- "Employed"
  Response[str_detect(ResponseID, "RESP172")] <- "Self-employed"
  Response[str_detect(ResponseID, "RESP141")] <- "Homemaker"
  Response[str_detect(ResponseID, "RESP020")] <- "$50,000+"
  Response[str_detect(ResponseID, "RESP199")] <- "Alaskan/American Native, Asian, Pac. Islander,Other"
  Response[str_detect(ResponseID, "RESP008")] <- "Multiracial"
  Response[str_detect(ResponseID, "RESP005")] <- "White"
  Response[str_detect(ResponseID, "RESP006")] <- "Black"
  Response
}

merge_BreakoutID <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, c(
    "INCOME01"="INCOME1","INCOME02"="INCOME2","INCOME03"="INCOME3",
    "INCOME04"="INCOME4","INCOME05"="INCOME5","INCOME06"="INCOME5","INCOME07"="INCOME5",
    "RACE01"="RACE1","RACE02"="RACE2","RACE08"="RACE3","RACE04"="RACE4","RACE05"="RACE4",
    "RACE06"="RACE4","RACE03"="RACE4","RACE07"="RACE5"
  ))
  x
}

merge_Break_Out <- function(BreakoutID, Break_Out) {
  Break_Out <- as.character(Break_Out)
  Break_Out[str_detect(BreakoutID, "INCOME5")] <- "$50,000+"
  Break_Out[str_detect(BreakoutID, "RACE1")] <- "White"
  Break_Out[str_detect(BreakoutID, "RACE2")] <- "Black"
  Break_Out[str_detect(BreakoutID, "RACE3")] <- "Hispanic"
  Break_Out[str_detect(BreakoutID, "RACE4")] <- "A/A Native, Asian,Other"
  Break_Out[str_detect(BreakoutID, "RACE5")] <- "Multiracial"
  Break_Out
}

# ================= SAFE AGGREGATION HELPERS =====================

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

aggregate_question <- function(df, q) {
  df %>%
    mutate(Data_value = safe_numeric(Data_value),
           Sample_Size = safe_numeric(Sample_Size)) %>%
    filter(str_detect(Question, regex(q, ignore_case = TRUE)),
           BreakOutCategoryID == "CAT1",
           !is.na(Data_value), !is.na(Sample_Size)) %>%
    rename(persons = Sample_Size) %>%
    mutate(Sample_Size = persons * 100 / Data_value) %>%
    group_by(Response) %>%
    summarise(
      agg_ss = sum(Sample_Size),
      agg_persons = sum(persons),
      agg_percent = ifelse(agg_ss > 0, agg_persons*100/agg_ss, NA),
      agg_percent_sdev = sqrt(pmax(agg_percent*(100-agg_percent)/agg_ss,0)),
      low_CI = agg_percent - 2*agg_percent_sdev,
      high_CI = agg_percent + 2*agg_percent_sdev,
      .groups="drop"
    )
}

aggregate_by_category <- function(df, q, cat_id) {
  df %>%
    mutate(Data_value = safe_numeric(Data_value),
           Sample_Size = safe_numeric(Sample_Size)) %>%
    filter(str_detect(Question, regex(q, ignore_case = TRUE)),
           BreakOutCategoryID == cat_id,
           !is.na(Data_value), !is.na(Sample_Size)) %>%
    rename(persons = Sample_Size) %>%
    mutate(Sample_Size = persons * 100 / Data_value) %>%
    group_by(Break_Out, Response) %>%
    summarise(
      agg_ss = sum(Sample_Size),
      agg_persons = sum(persons),
      agg_percent = ifelse(agg_ss > 0, agg_persons*100/agg_ss, NA),
      agg_percent_sdev = sqrt(pmax(agg_percent*(100-agg_percent)/agg_ss,0)),
      low_CI = agg_percent - 2*agg_percent_sdev,
      high_CI = agg_percent + 2*agg_percent_sdev,
      .groups="drop"
    )
}

aggregate_by_year <- function(df, q) {
  df %>%
    mutate(Data_value = safe_numeric(Data_value),
           Sample_Size = safe_numeric(Sample_Size)) %>%
    filter(str_detect(Question, regex(q, ignore_case = TRUE)),
           BreakOutCategoryID == "CAT1",
           !is.na(Data_value), !is.na(Sample_Size)) %>%
    rename(persons = Sample_Size) %>%
    mutate(Sample_Size = persons * 100 / Data_value) %>%
    group_by(Year, Response) %>%
    summarise(
      agg_ss = sum(Sample_Size),
      agg_persons = sum(persons),
      agg_percent = ifelse(agg_ss > 0, agg_persons*100/agg_ss, NA),
      .groups="drop"
    ) %>% arrange(Year)
}

aggregate_by_location <- function(df, q) {
  df %>%
    mutate(Data_value = safe_numeric(Data_value),
           Sample_Size = safe_numeric(Sample_Size)) %>%
    filter(str_detect(Question, regex(q, ignore_case = TRUE)),
           BreakOutCategoryID == "CAT1",
           !is.na(Data_value), !is.na(Sample_Size)) %>%
    rename(persons = Sample_Size) %>%
    mutate(Sample_Size = persons * 100 / Data_value) %>%
    group_by(Locationabbr, Response) %>%
    summarise(
      agg_ss = sum(Sample_Size),
      agg_persons = sum(persons),
      agg_percent = ifelse(agg_ss > 0, agg_persons*100/agg_ss, NA),
      .groups="drop"
    )
}

# =============== SAFEST PLOT WRAPPER ====================

safe_plot <- function(tbl, fun, title="") {
  if (is.null(tbl) || nrow(tbl)==0) {
    return(plotly_empty() %>% layout(title=paste("No data for:",title)))
  }
  if (!"Break_Out" %in% names(tbl)) tbl$Break_Out <- "Overall"
  if (!"low_CI" %in% names(tbl)) tbl$low_CI <- NA
  if (!"high_CI" %in% names(tbl)) tbl$high_CI <- NA
  fun(tbl)
}

plot_category_panel <- function(tbl, title="") {
  arr_high <- ifelse(is.na(tbl$high_CI), 0, tbl$high_CI - tbl$agg_percent)
  arr_low  <- ifelse(is.na(tbl$low_CI), 0, tbl$agg_percent - tbl$low_CI)
  
  plot_ly(tbl, x=~Response, y=~agg_percent, color=~Break_Out, type="bar",
          error_y=list(type="data", array=arr_high, arrayminus=arr_low)) %>%
    layout(title=title, yaxis=list(title="Percentage (%)"))
}

plot_temporal_panel <- function(tbl, title="") {
  plot_ly(tbl, x=~Year, y=~agg_percent, color=~Response, type="scatter", mode="lines+markers") %>%
    layout(title=title, yaxis=list(title="Percentage (%)"))
}