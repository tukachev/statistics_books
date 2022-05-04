library(ggbeeswarm)
library(ggtext)

books_data2 <- books_data %>% 
  mutate(col = ifelse(avg_rating <= 4.18, "#dfc27d", "gray90"),
         col = ifelse(avg_rating == 4.18, "#80cdc1", col))

books_data2$col[books_data2$bookTitle == book_title] <- "brown"



subtitle <- glue("Средняя оценка {PR}% книг <span style='color:#dfc27d;'>ниже</span> 
                 или <span style='color:#80cdc1;'>такая же</span> как 
                 у <span style='color:brown;'>книги \"Искусство статистики\"</span>")

set.seed(666)
ggplot(books_data2, aes(x = "", y = avg_rating)) +
  geom_hline(yintercept = 4.18, color = "gray80") +
  geom_beeswarm(size = 2.3, cex = 2, 
                colour = books_data2$col,
                priority = "random") +
  scale_y_continuous(breaks = c(seq(2.5,4,0.5), 4.18, 4.5, 5)) +
  coord_flip() + 
  labs(
    y = "Средняя оценка книги",
    title = glue(
      "Распределение среднего рейтинг-балла*\nдля {books_count} книг со 
      словом Statistics в названии"),
    subtitle = subtitle,
    caption = c(
      glue("* на основе не менее {readers} оценок каждой книги читателями"),
      "\nИсточник данных: Goodreads\nВизуализация: Юрий Тукачев, 2022"
    )
  ) +
  theme_light(base_size = 20, base_family = font) +
  theme(
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_text(size = 18),
    # axis.title.y = element_blank(),
    text = element_text(
      family = font,
      color = "#53565A",
      size = 18
    ),
    # axis.text.y = element_text(vjust = -0.25),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # warning: Vectorized input to `element_text()` is not officially supported
    plot.caption = element_text(
      color = c("gray", "gray50"),
      size = c(12, 13),
      hjust = c(0, 1)
    ),
    plot.margin = margin(25, 25, 10, 25),
    plot.subtitle = element_markdown(hjust = 0,
                                     # size = rel(0.95), 
                                     family = font),
    # plot.subtitle = element_text(
      # hjust = 0,
      # size = rel(0.85),
      # family = font
    # ),
    plot.title = element_text(
      size = rel(1.3),
      family = font,
      face = "bold",
      color = "gray20"
    )
  )


ggsave(
  here("images", "books_ratings2.png"),
  device = agg_png,
  width = 6.5,
  height = 6,
  dpi = 120,
  scale = 1.25
)
