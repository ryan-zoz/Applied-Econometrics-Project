# Descriptive quality-price-usage analysis. Run from the project root.
library(dplyr)
library(ggplot2)
d <- read.csv("llm_csv/quality_enriched/usage_price_quality_daily_20260919_20261016.csv",
              fileEncoding = "UTF-8-BOM", na.strings = "", stringsAsFactors = FALSE)
out <- "llm_csv/quality_eda_results"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stopifnot(!anyDuplicated(d[c("model_permaslug", "date_utc")]))
m <- d %>% group_by(model_permaslug) %>% summarise(
  quality = first(quality_index), aa_id = first(quality_aa_id),
  date_status = first(quality_date_status), days = n(),
  visible_tokens = sum(total_tokens), avg_tokens = mean(total_tokens),
  avg_input = if (any(!is.na(input_usd_per_million))) mean(input_usd_per_million, na.rm = TRUE) else NA_real_,
  avg_output = if (any(!is.na(output_usd_per_million))) mean(output_usd_per_million, na.rm = TRUE) else NA_real_,
  .groups = "drop")
q <- filter(m, !is.na(quality))
summary <- data.frame(models = nrow(m), scored_models = nrow(q),
  distinct_aa_entries = n_distinct(q$aa_id), scored_model_days = sum(q$days),
  share_of_visible_tokens_scored = sum(q$visible_tokens) / sum(m$visible_tokens),
  quality_min = min(q$quality), quality_median = median(q$quality), quality_max = max(q$quality))
correlations <- bind_rows(lapply(c("avg_input", "avg_output", "avg_tokens"), function(variable) {
  a <- q[is.finite(q[[variable]]) & q[[variable]] > 0, ]
  strict <- filter(a, date_status == "same_date")
  data.frame(variable = variable, models = nrow(a),
    pearson_quality_log_variable = cor(a$quality, log(a[[variable]])),
    spearman = cor(a$quality, a[[variable]], method = "spearman"),
    exact_date_models = nrow(strict),
    exact_date_spearman = cor(strict$quality, strict[[variable]], method = "spearman"))
}))
write.csv(m, file.path(out, "model_summary.csv"), row.names = FALSE, na = "")
write.csv(summary, file.path(out, "coverage.csv"), row.names = FALSE)
write.csv(correlations, file.path(out, "correlations.csv"), row.names = FALSE)
cat("COVERAGE\n"); print(summary, row.names = FALSE)
cat("MODEL-LEVEL CORRELATIONS\n"); print(correlations, row.names = FALSE)
cat("ZERO-PRICE SCORED PRODUCTS\n"); print(sum(q$avg_input == 0 & q$avg_output == 0, na.rm = TRUE))
cat("TOP QUALITY\n"); print(head(arrange(q, desc(quality)), 5))

plot_data <- bind_rows(
  transmute(q, model = model_permaslug, quality, panel = "Input quote\nUSD / million tokens", value = avg_input,
            zero_price = avg_input == 0 & avg_output == 0),
  transmute(q, model = model_permaslug, quality, panel = "Output quote\nUSD / million tokens", value = avg_output,
            zero_price = avg_input == 0 & avg_output == 0),
  transmute(q, model = model_permaslug, quality, panel = "Daily usage\nBillion tokens / observed day", value = avg_tokens / 1e9,
            zero_price = avg_input == 0 & avg_output == 0)) %>% filter(is.finite(value), value > 0)
plot_data$panel <- factor(plot_data$panel, levels = c("Input quote\nUSD / million tokens", "Output quote\nUSD / million tokens", "Daily usage\nBillion tokens / observed day"))
p <- ggplot(plot_data, aes(quality, value, color = zero_price)) +
  geom_point(size = 2.3, alpha = 0.8) + scale_y_log10() +
  scale_color_manual(values = c("FALSE" = "#376da0", "TRUE" = "#d07b36"),
                     labels = c("FALSE" = "Positive-price product", "TRUE" = "Zero-price product"),
                     name = NULL) +
  facet_wrap(~panel, scales = "free_y", nrow = 1) +
  labs(title = "Model quality, catalog quotes and observed usage",
    subtitle = "One point per scored OpenRouter product. Price panels exclude zero-price products.",
    x = "AA Intelligence Index v4.3 (reference configuration)",
    y = "Mean value (log scale)",
    caption = "2026-09-19 to 2026-10-16. Top-50 selected sample; descriptive between-product relationships.\nAA reference configurations may differ from actual API usage. Source: OpenRouter, TokenCanopy, Artificial Analysis.") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", panel.grid.minor = element_blank(),
    plot.title.position = "plot", plot.caption = element_text(hjust = 0))
ggsave(file.path(out, "quality_price_usage.png"), p, width = 15, height = 6, dpi = 150, bg = "white")
capture.output(sessionInfo(), file = file.path(out, "R_sessionInfo.txt"))
