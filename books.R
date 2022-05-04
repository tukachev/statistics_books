# The Art of Statistics: How to Learn from Data
# by David Spiegelhalter 
# 
# Русское издание книги: Дэвид Шпигельхалтер
# Искусство статистики: как находить ответы в данных
#  
# В магазине Бук24:
# https://book24.ru/~ayNfc
# На сайте издательства:
# https://www.mann-ivanov-ferber.ru/books/iskusstvo-statistiki/

library(tidyverse)
library(purrrgress)
library(rvest)
library(glue)
library(showtext)
library(here)
library(ragg)

# font_add_google("Montserrat Alternates", family = "Montserrat")
# font_add_google("Roboto Condensed", family = "Roboto")
font_add_google("Istok Web", family = "Istok Web")
font <- "Istok Web"

showtext_auto()

book_title <- "The Art of Statistics: How to Learn from Data"
n_total <- 200 # берем только 200 страниц поисковой выдачи сайта
# URL-адреса
urls <- glue(
  "https://www.goodreads.com/search?page={1:n_total}&q=statistics&search%5Bfield%5D=title"
)
readers <- 10 #не менее 10 читателей книги

# функция для парсинга страниц поисковой выдачи сайта
get_books_rating <- function(urls) {
  Sys.sleep(sample(10:36, 1)) #случайная задержка в секундах перед новым запросом
  html <- read_html(urls) %>% minimal_html()
  ratings <- html %>%
    html_elements(".uitext") %>%
    html_text2()
  ratings <-
    ratings[!(ratings %in% c("Rate this book", "Clear rating"))]
  bookTitle <- html %>%
    html_elements("td:nth-child(2) > a") %>%
    html_text2()
  links <- html %>% html_elements("td:nth-child(2) > a") %>%
    html_attr("href") %>%
    sub("\\?.*", "", .) %>%
    paste0("https://www.goodreads.com", .)
  rank <- html %>% html_elements("td:nth-child(2) > a") %>%
    html_attr("href") %>%
    sub(".*rank=*", "", .)
  authorName <- html %>%
    html_elements("span:nth-child(4) > div:nth-child(1) > a") %>%
    html_text2()
  temp <- tibble(authorName, bookTitle, links, ratings, rank)
  return(temp)
}

books_data <- pro_map_df(urls, get_books_rating)

# сохраняем исходник
saveRDS(books_data, here("data", "books_data_source.Rds"))

# books_data <- readRDS(here("data", "books_data_source.Rds"))

# очистка и преобразование
books_data <- books_data %>%
  separate(ratings,
           c("avg_rating", "ratings", "published", "edition"),
           "—") %>%
  mutate_all(str_trim, "both") %>%
  mutate_at(c("avg_rating", "ratings", "published", "edition"),
            parse_number) %>%
  mutate(edition = coalesce(edition, published)) %>%
  mutate(published = ifelse(published < 1000, NA, published)) %>%
  distinct() %>%
  filter(ratings >= readers)

# сохраняем очищенный датасет для графика
saveRDS(books_data, here("data", "books_data.Rds"))
# books_data <- readRDS(here("data", "books_data.Rds"))

# write csv
write_csv2(books_data, here("data", "book_data.csv"))

# считаем процентильный ранг книги Искусство статистики
art_of_stat_rating <-
  books_data$avg_rating[books_data$bookTitle == book_title]
below_count <- sum(books_data$avg_rating < art_of_stat_rating)
# below_count
at_count <- sum(books_data$avg_rating == art_of_stat_rating)
# at_count
PR <-
  round((below_count + 0.5 * at_count) / nrow(books_data) * 100, 0)
PR #процентильный ранг

quantile(books_data$avg_rating, seq(0, 1, 0.01))

# Распределение рейтинг-балла книг по статистике
books_count <- nrow(books_data)

img <- png::readPNG(here("cover.png"))
book_cover <- grid::rasterGrob(img, interpolate = T)

ggplot(books_data) +
  geom_histogram(aes(avg_rating),
                 binwidth = .1,
                 color = "#8c510a",
                 fill = "#f6e8c3") +
  scale_y_continuous(limits = c(0, 85), 
                     breaks = seq(0,85, 10),
                     labels = seq(0,85, 10), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2.5,5,0.5)) +
  geom_vline(xintercept = art_of_stat_rating,
             colour = "gray80") +
  annotate(
    "text",
    x = 4.75,
    y = 50,
    family = font,
    size = 4.5,
    color = "gray50",
    lineheight = .9,
    label = glue(
      "Средний рейтинг книги\n\"Искусство статистики\"\n{art_of_stat_rating} ({PR}-й процентиль)"
    )
  ) +
  geom_curve(
    aes(
      x = 4.40,
      y = 50,
      xend = art_of_stat_rating,
      yend = 45
    ),
    arrow = arrow(length = unit(.02, "npc")),
    size = .15,
    color = "gray50",
    curvature = .20
  ) +
  annotation_custom(
    book_cover,
    xmin = 4.5,
    xmax = 5.05,
    ymin = 55,
    ymax = 85
  ) +
  labs(
    x = "Средняя оценка книги",
    y = "Количество книг",
    title = glue(
      "Распределение среднего рейтинг-балла*\nдля {books_count} книг со словом Statistics в названии"),
    subtitle = glue("Средняя оценка {PR}% книг ниже или такая же как у книги \"Искусство статистики\""
    ),
    caption = c(
      glue("* на основе не менее {readers} оценок каждой книги читателями"),
      "\nИсточник данных: Goodreads\nВизуализация: Юрий Тукачев, 2022"
    )
  ) +
  theme_light(base_size = 20, base_family = font) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_text(size = 18),
    # axis.title.y = element_blank(),
    text = element_text(
      family = font,
      color = "#53565A",
      size = 18
    ),
    # axis.text.y = element_text(vjust = -0.25),
    panel.background = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # warning: Vectorized input to `element_text()` is not officially supported
    plot.caption = element_text(
      color = c("gray", "gray50"),
      size = c(12, 13),
      hjust = c(0, 1)
    ),
    plot.margin = margin(25, 25, 10, 25),
    plot.subtitle = element_text(
      hjust = 0,
      # size = rel(0.95),
      family = font
    ),
    plot.title = element_text(
      size = rel(1.3),
      family = font,
      face = "bold",
      color = "gray20"
    )
  )

ggsave(
  here("images", "books_ratings.png"),
  device = agg_png,
  width = 6.5,
  height = 6,
  dpi = 120,
  scale = 1.25
)

# beeswarm plot
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
