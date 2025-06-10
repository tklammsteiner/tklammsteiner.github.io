library(tidyverse)
library(readxl)
library(reactable)
library(leaflet)
library(htmltools)

authors <- read_excel("bugbook_data.xlsx", sheet = "authors")

authors %>% 
  distinct(Author) %>% 
  nrow()

authors %>% 
  distinct(Country) %>% 
  nrow()

authors %>% 
  distinct(City) %>% 
  nrow()

authors_per_chapter <- authors %>%
  group_by(Chapter) %>%
  summarise(UniqueAuthors = n_distinct(Author))

# Authors per chapter
ggplot(authors_per_chapter, aes(x = Chapter, y = UniqueAuthors)) +
  geom_bar(stat = "identity", fill = "#FF6B35") +
  scale_x_continuous(breaks = seq(1, 13, 1)) +
  theme_minimal() +
  labs(title = "Number of Unique Authors per Chapter", x = "Chapter", y = "Unique Authors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Top contributing authors
chapters_per_author <- authors %>%
  group_by(Author) %>%
  summarise(ChaptersContributed = n_distinct(Chapter))

top_authors <- chapters_per_author %>%
  arrange(desc(ChaptersContributed)) %>%
  head(20)

ggplot(top_authors, aes(x = reorder(Author, ChaptersContributed), y = ChaptersContributed)) +
  geom_bar(stat = "identity", fill = "#2C5F5D") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Authors by Number of Chapters Contributed", x = "Author", y = "Chapters Contributed")

authors_by_country <- authors %>%
  distinct(Author, Country) %>%
  count(Country)

# Authors by country
ggplot(authors_by_country, aes(x = "", y = n, fill = Country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Authors by Country")



author_summary <- authors %>%
  group_by(Author, Institution, City, Country, Lat, Long) %>%
  summarise(
    Chapters = paste(sort(unique(Chapter)), collapse = ", "),
    .groups = "drop"
  )

library(dplyr)
library(igraph)
library(visNetwork)

edges <- authors %>% 
  filter(Chapter != 5) %>% 
  dplyr::group_by(Chapter) %>%
  summarise(Authors = list(unique(Author)), .groups = "drop") %>%
  pull(Authors) %>%
  lapply(function(x) if(length(x) >= 2) t(combn(x, 2)) else NULL) %>%
  Filter(Negate(is.null), .) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  setNames(c("from", "to"))

# Create graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Create nodes/edges for visNetwork
nodes <- data.frame(
  id = V(g)$name, 
  label = V(g)$name,  # Show labels by default
  font.size = 20,      # Larger text
  color = "#2C5F5D"    # Node color
)

edges_vis <- as_data_frame(g, what = "edges")

# Create interactive plot with stabilization
visNetwork(nodes, edges_vis) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1),
    nodesIdSelection = TRUE
  ) %>%
  visPhysics(  # Stabilization settings
    stabilization = list(
      enabled = TRUE,
      iterations = 1000  # More iterations = more stable
    ),
    solver = "barnesHut",  # Better for larger networks
    barnesHut = list(
      gravitationalConstant = -2000,  # Adjust for node spacing
      springLength = 150
    )
  ) %>%
  visLayout(randomSeed = 123)  # Consistent initial layout



