# The Art of Statistics: How to Learn from Data
# by David Spiegelhalter 
# 
# Русское издание книги:
# Искусство статистики
# Как находить ответы в данных
# Дэвид Шпигельхалтер
# https://www.mann-ivanov-ferber.ru/books/iskusstvo-statistiki/

library(tidyverse)
library(purrrgress)
library(rvest)
library(glue)
library(showtext)
library(here)
library(ragg)

font_add_google("Montserrat Alternates", family = "Montserrat")
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
  Sys.sleep(sample(10:17, 1)) #случайная задержка в секундах перед новым запросом
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
  unique() %>%
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
  scale_y_continuous(limits = c(0, 85), expand = c(0, 0)) +
  annotate(
    "text",
    x = 4.75,
    y = 51,
    family = "Montserrat",
    size = 4,
    color = "gray50",
    lineheight = .8,
    label = glue(
      "Средний рейтинг книги\n\"Искусство статистики\"\n{art_of_stat_rating} ({PR}-й процентиль)"
    )
  ) +
  geom_curve(
    aes(
      x = 4.4,
      y = 50,
      xend = art_of_stat_rating,
      yend = 42
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
    title = "Статистический анализ книг по статистике",
    subtitle = glue(
      "Распределение среднего рейтинг-балла* для {books_count} книг со словом Statistics в названии"
    ),
    caption = c(
      glue("* на основе не менее {readers} оценок читателей книги"),
      "\nИсточник данных: Goodreads\nВизуализация: Юрий Тукачев, 2021"
    )
  ) +
  theme_light(base_size = 18, base_family = "Montserrat") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(
      family = "Montserrat",
      color = "#53565A",
      size = 18
    ),
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
      size = rel(0.8),
      family = "Montserrat"
    ),
    plot.title = element_text(
      size = rel(1.3),
      family = "Montserrat",
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
