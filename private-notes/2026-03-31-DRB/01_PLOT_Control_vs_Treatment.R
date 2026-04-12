#| echo: true
#| code-fold: true
#| code-summary: "👀 Updated Simulation & Plotting Code (투명 박스 적용)"
#| warning: false
#| message: false

library(data.table)
library(ggplot2)
library(grid)

# ==============================================================================
# 1. Data Generation
# ==============================================================================
set.seed(2026)
n <- 200L

dt <- rbindlist(list(
  data.table(Group = "A: Control (Stable Baseline)", Value = rnorm(n, mean = 100, sd = 1)),
  data.table(Group = "B: Treatment (Shifted + Noisy)", Value = rnorm(n, mean = 105, sd = 8))
))

group_levels <- c("A: Control (Stable Baseline)", "B: Treatment (Shifted + Noisy)")
dt[, Group := factor(Group, levels = group_levels)]
dt[, x := fifelse(Group == group_levels[1], 1, 2)]

# ==============================================================================
# 2. Statistics
# ==============================================================================
stats <- dt[, .(
  n = .N,
  Mean = mean(Value),
  SD_Own = sd(Value)
), by = .(Group, x)]

mean_A <- stats[Group == group_levels[1], Mean]
mean_B <- stats[Group == group_levels[2], Mean]

sd_ref <- stats[Group == group_levels[1], SD_Own]
sd_pooled <- with(stats, sqrt(sum((n - 1) * SD_Own^2) / sum(n - 1)))

mean_diff <- mean_B - mean_A
glass_val <- mean_diff / sd_ref
cohen_val <- mean_diff / sd_pooled

flag_glass <- as.integer(glass_val >= 1)
flag_cohen <- as.integer(cohen_val >= 1)

# ==============================================================================
# 3. Ruler data 
# ==============================================================================
ruler_dt <- data.table(
  type   = c("1 Ref SD", "1 Pooled SD"),
  x      = c(2.70, 3.00), 
  center = mean_A,
  ymin   = c(mean_A - sd_ref,    mean_A - sd_pooled),
  ymax   = c(mean_A + sd_ref,    mean_A + sd_pooled),
  col    = c("#0072B2", "#D55E00")
)

y_min <- min(dt$Value) - 3
y_max <- max(dt$Value) + 3

label_txt <- paste0(
  "Mean Diff = ", round(mean_diff, 2), "\n",
  "Glass's \u0394 = ", round(glass_val, 2), "   \u2192 Alarm = ", flag_glass, "\n",
  "Cohen's d = ", round(cohen_val, 2), "   \u2192 Alarm = ", flag_cohen
)

# ==============================================================================
# 4. Plot
# ==============================================================================
ggplot(dt, aes(x = x, y = Value)) +
  
  geom_point(
    aes(fill = Group),
    position = position_jitter(width = 0.35, height = 0, seed = 2026),
    shape = 21, size = 2.9, stroke = 0.28,
    color = "grey15", alpha = 0.82
  ) +
  
  geom_segment(
    data = stats,
    aes(x = x - 0.20, xend = x + 0.20, y = Mean, yend = Mean),
    inherit.aes = FALSE,
    linewidth = 1.3, color = "black"
  ) +
  
  geom_point(
    data = stats,
    aes(x = x, y = Mean),
    inherit.aes = FALSE,
    shape = 23, size = 4.5,
    fill = "white", color = "black", stroke = 1.1
  ) +
  
  annotate(
    "segment",
    x = 2.25, xend = 2.25,
    y = mean_A, yend = mean_B,
    arrow = arrow(length = unit(0.18, "cm"), ends = "both"),
    linewidth = 1.0, color = "black"
  ) +
  annotate(
    "text",
    x = 2.30, y = (mean_A + mean_B) / 2,
    label = paste0("Mean diff\n", round(mean_diff, 2)),
    hjust = 0, size = 4.2, fontface = "bold"
  ) +
  
  geom_linerange(
    data = ruler_dt,
    aes(x = x, ymin = ymin, ymax = ymax, color = type),
    inherit.aes = FALSE,
    linewidth = 2.3, lineend = "round"
  ) +
  geom_point(
    data = ruler_dt,
    aes(x = x, y = center, color = type),
    inherit.aes = FALSE,
    size = 3.3
  ) +
  
  # === 포인트 3: 글자 위아래 조절 ===
  # + 2.5 라는 숫자를 조절해보세요.
  annotate(
    "text",
    x = 2.7, y = mean_A + sd_ref + 3, 
    label = paste0("1 Ref SD\n(", round(sd_ref, 2), ")"),
    color = "#0072B2", fontface = "bold", size = 4.1
  ) +
  annotate(
    "text",
    x = 3.00, y = mean_A + sd_pooled + 3,
    label = paste0("1 Pooled SD\n(", round(sd_pooled, 2), ")"),
    color = "#D55E00", fontface = "bold", size = 4.1
  ) +
  
  # === 포인트 2: 투명 배경 & 글자 크기 ===
  # fill = NA (배경 투명), label.size = 0 (테두리 삭제)
  annotate(
    "label",
    x = 1.5, y = y_max,
    label = label_txt,
    size = 3.8, fontface = "bold",
    fill = alpha("white", 0.5),  # 흰색 배경에 50% 투명도 적용
    color = "black",
    label.size = 0, 
    vjust = 1
  ) +
  
  scale_fill_manual(values = c(
    "A: Control (Stable Baseline)" = "#BFD7EA",
    "B: Treatment (Shifted + Noisy)" = "#F6C5AF"
  )) +
  scale_color_manual(values = c(
    "1 Ref SD"    = "#0072B2",
    "1 Pooled SD" = "#D55E00"
  )) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("A: Control\n(Stable Baseline)", "B: Treatment\n(Shifted + Noisy)")
  ) +
  
  # === 포인트 1: 오른쪽 텅 빈 공간(여백) 조절 ===
  # xlim의 끝값(3.95)을 조절해보세요.
  coord_cartesian(
    xlim = c(0.65, 3.00), 
    ylim = c(y_min, y_max),
    clip = "off"
  ) +
  labs(
    title = "Same Mean Difference, Different Rulers",
    subtitle = "While large when measured by the Control SD,\nusing the Pooled SD inflates the 'ruler' due to the Treatment's variance,\ndiluting the signal.",
    x = NULL,
    y = "Metrology Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 12),
    # plot.margin의 두 번째 숫자(90)를 조절해보세요.
    plot.margin = margin(15, 90, 15, 15)
  )