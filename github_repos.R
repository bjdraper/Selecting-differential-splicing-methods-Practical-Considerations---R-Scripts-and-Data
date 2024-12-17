# Install and load required packages
if (!requireNamespace("git2r", quietly = TRUE)) {
  install.packages("git2r")
}
library(git2r)
library(dplyr)
library(purrr)
library(ggridges)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(lubridate)

# List of GitHub repositories
repositories <- c(
  "https://github.com/TobiTekath/DTUrtle",
  "https://github.com/areyesq89/DEXSeq",
  "https://github.com/lengning/EBSeq",
  "https://github.com/Xinglab/rmats-turbo",
  "https://github.com/yarden/MISO",
  "https://github.com/davidaknowles/leafcutter",
  "https://github.com/comprna/SUPPA",
  "https://github.com/Xinglab/DARTS",
  "https://github.com/ConesaLab/NOISeq",
  "https://github.com/kvittingseerup/IsoformSwitchAnalyzeR",
  "https://github.com/Wanvdphelys/SeqGSEA",
  "https://github.com/hartleys/JunctionSeq",
  "https://github.com/Xinglab/GLiMMPS",
  "https://github.com/gosianow/DRIMSeq",
  "https://github.com/ratschlab/rDiff",
  "https://github.com/KoenigLabNM/SplicingCompass",
  "https://github.com/thelovelab/DESeq2"
)

# Cloning repositories with error handling
for (repo in repositories) {
  # Extract repository name
  repo_name <- basename(repo)
  
  # Attempt to clone repository with error handling
  tryCatch({
    git2r::clone(repo, paste0(getwd(), "/", repo_name))
    cat("Repository", repo_name, "cloned successfully.\n")
  }, error = function(e) {
    cat("Error cloning repository", repo_name, ":", conditionMessage(e), "\n")
  })
}






# Function to extract commit history from a repository
extract_commit_history <- function(repo_path) {
  repo <- git2r::repository(repo_path)
  commits <- git2r::commits(repo)
  commits
}

# List of cloned repository directories
repository_dirs <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)

# Initialize an empty list to store commit histories
commit_histories <- list()

# Loop through repository directories and extract commit histories
for (repo_path in repository_dirs) {
  repo_name <- basename(repo_path)
  print(paste("Extracting commit history from:", repo_name))
  commit_history <- extract_commit_history(repo_path)
  commit_histories[[repo_name]] <- commit_history
}




# Initialize an empty list to store commit dates
commit_dates_list <- list()

# Loop through commit histories and extract commit dates
for (repo_name in names(commit_histories)) {
  commit_history <- commit_histories[[repo_name]]
  
  # Extract commit dates from the commit history
  commit_dates <- sapply(commit_history, function(commit) {
    as.POSIXct(commit$author$when, tz = "GMT")
  })
  
  commit_dates_list[[repo_name]] <- commit_dates
}



# Convert commit_dates_list into a long dataframe
commit_dates_df <- lapply(commit_dates_list, function(commit_dates) {
  data.frame(date_of_commit = as.POSIXct(commit_dates, origin = "1970-01-01", tz = "GMT"))
})

commit_dates_df <- do.call(rbind, commit_dates_df)
commit_dates_df$repository_name <- rep(names(commit_dates_list), sapply(commit_dates_list, length))

# Print the resulting dataframe
print(commit_dates_df)

# Filter out rows corresponding to the "DESeq2" repository
commit_dates_df <- commit_dates_df[commit_dates_df$repository_name != "DESeq2", ]


# Create a dataframe with Category and Tool_Name information
category_tool_df <- data.frame(
  Category = c("Event", "Exon", "Transcript", "Transcript", "Event", "Event", "Transcript", "Exon", "Event", "Exon", "Transcript", "Event", "Exon", "Transcript", "Transcript"),
  Tool_Name = c("rmats-turbo", "DEXSeq", "EBSeq", "NOISeq", "leafcutter", "SUPPA", "IsoformSwitchAnalyzeR", "JunctionSeq", "DARTS", "GLiMMPS", "SeqGSEA", "SplicingCompass", "rDiff", "DTUrtle", "DRIMSeq")
)

# Perform a full join between commit_dates_df and category_tool_df
commit_dates_df <- full_join(commit_dates_df, category_tool_df, by = c("repository_name" ="Tool_Name"))


# Extract year from date_of_commit column
commit_dates_df$year <- year(commit_dates_df$date_of_commit)

# Count number of commits per year for each group of repository and category
commits_per_repo_category_year <- commit_dates_df %>%
  group_by(repository_name, Category, year) %>%
  summarise(total_commits = n())



# Define the custom color palette
uos_colours <- c("#440099", "#9ADBE8", "#131E29",
                 "#005A8F", "#00BBCC", "#64CBE8",
                 "#00CE7C", "#3BD4AE", "#A1DED2",
                 "#663DB3", "#981F92", "#DAA8E2",
                 "#E7004C", "#FF6371", "#FF9664")

names(uos_colours) <- c("Deep violet", "Powder blue", "Midnight black",
                        "Teal", "Aqua", "Sky blue",
                        "Mint green", "Spearmint", "Pastel green",
                        "Mauve", "Purple", "Lavender",
                        "Coral", "Flamingo", "Peach")

selected <- uos_colours[c("Aqua","Spearmint","Coral")]


#Arrange by total number of commits

commits_per_repo_category_year <- commits_per_repo_category_year %>%
  ungroup() %>%
  group_by(repository_name) %>%
  mutate(total_by_repos = sum(total_commits))

commits_per_repo_category_year$repository_name <- reorder(commits_per_repo_category_year$repository_name,
                                                          desc(commits_per_repo_category_year$total_by_repos))
commits_per_repo_category_year$repository_name <- factor(commits_per_repo_category_year$repository_name, 
                                                         levels=rev(levels(commits_per_repo_category_year$repository_name)))




# Determine the common x-axis limits
x_limits <- c(2010, 2025)

outline.color <- "grey"

# Create a heatmap with outlined tiles
heatmap1 <- ggplot(commits_per_repo_category_year %>% filter(Category == "Event"),
                   aes(x = year, y = repository_name, fill = total_commits)) +
  geom_tile(color = ifelse(commits_per_repo_category_year %>% filter(Category == "Event") %>% pull(total_commits) > 0, outline.color, NA)) +  # Outline tiles where total_commits > 0
  scale_fill_gradient(low = "white", high = uos_colours["Coral"]) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "",
    fill = "No. Commits",
    title = "Event Based Methods"
  ) +
  xlim(x_limits)

heatmap2 <- ggplot(commits_per_repo_category_year %>% filter(Category == "Transcript"),
                   aes(x = year, y = repository_name, fill = total_commits)) +
  geom_tile(color = ifelse(commits_per_repo_category_year %>% filter(Category == "Transcript") %>% pull(total_commits) > 0, outline.color, NA)) +  # Outline tiles where total_commits > 0
  scale_fill_gradient(low = "white", high = uos_colours["Spearmint"]) +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    fill = "No. Commits",
    title = "Transcript Based Methods"
  ) +
  xlim(x_limits)

heatmap3 <- ggplot(commits_per_repo_category_year %>% filter(Category == "Exon"),
                   aes(x = year, y = repository_name, fill = total_commits)) +
  geom_tile(color = ifelse(commits_per_repo_category_year %>% filter(Category == "Exon") %>% pull(total_commits) > 0, outline.color, NA)) +  # Outline tiles where total_commits > 0
  scale_fill_gradient(low = "white", high = uos_colours["Aqua"]) +
  theme_minimal() +
  labs(
    x = "",
    y = "Repository Name",
    fill = "No. Commits",
    title = "Exon Based Methods"
  ) +
  xlim(x_limits)


# Arrange plots together with white background
ggarrange(heatmap2, heatmap3, heatmap1, nrow = 3, common.legend = FALSE, legend = "right", align = "v")


# Save the arranged plot as a TIFF file with 300 dpi
ggsave("Fig4.tiff", dpi = 300, width = 5.5, height = 6, bg = "white")












