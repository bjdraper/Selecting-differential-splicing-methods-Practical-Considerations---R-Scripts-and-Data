library(dplyr)
library(ggplot2)
library(data.table)
library(ggridges)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)


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

#read in data
data <- read.csv(file = "citations_2023.csv") %>% arrange(Publication_Year)

#How many citations does DESeq2 have total?
data %>% filter(Tool_Name == "DESeq2") %>% pull(Total_Citations)
#Filter DESeq2 out:
data <- data %>% filter(Tool_Name != "DESeq2")
#What is the range of total citations in these specific tools
range(data$Total_Citations)

#Convert the data to long format
data <- data %>% select(-Title)
data.long <- melt(setDT(data), id.vars = c("Tool_Name","Publication_Year", "Total_Citations", "Average_per_Year", "Category"), variable.name = "Year")
data.long$Year <- gsub("X","",data.long$Year)



#Plot bar chart of total citations

# Plot using ggplot
ggplot(data=data, aes(x=reorder(Tool_Name,Total_Citations), y=Total_Citations, fill = Category)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=Total_Citations), vjust=-0.5, size=3, color="black") +  # Add labels for total count
  ggtitle("") +
  xlab("Tool Name") +
  ylab("Total Citations*") +
  theme_minimal() +  # Set theme to minimal
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("#E7004C","#00BBCC","#3BD4AE"), name = "Category") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, vjust=1)) +
  labs(caption = "*WoS up to 2024")  # Adding caption

ggsave("Fig2.tiff", dpi = 300, width = 5, height = 5, bg = "white")


#Plot bar chart of citations per year

#Filter out those with zero
data.long <- data.long %>% dplyr::filter(value>0)


data.long$Year <- as.numeric(data.long$Year)
data.long$Tool_Name <- reorder(data.long$Tool_Name, desc(data.long$value))
data.long$Tool_Name <- factor(data.long$Tool_Name, levels=rev(levels(data.long$Tool_Name)))



tile.height <- 0.9
outline.color <- "grey"

# Create separate plots for each category
plot_Transcript <- ggplot(subset(data.long, Category == "Transcript"),
                          aes(x = Year, y = Tool_Name, fill = value)) +
  geom_tile(height=tile.height,color = outline.color) +
  scale_fill_gradient(low = "white", high = uos_colours["Spearmint"], 
                      limits = c(0, max(data.long$value)),
                      breaks = c(0, max(data.long$value) * 0.25, max(data.long$value) * 0.5, 
                                 max(data.long$value) * 0.75, max(data.long$value))) +  # Specify custom breaks
  theme_minimal() +
  labs(title = "Transcript Based Methods", x = NULL, y = NULL) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = unique(data.long$Year), labels = unique(data.long$Year)) +
  labs(fill = "Citation count")

plot_Exon <- ggplot(subset(data.long, Category == "Exon"),
                    aes(x = Year, y = Tool_Name, fill = value)) +
  geom_tile(height=tile.height, color = outline.color) +
  scale_fill_gradient(low = "white", high = uos_colours["Aqua"], 
                      limits = c(0, max(data.long$value)),
                      breaks = c(0, max(data.long$value) * 0.25, max(data.long$value) * 0.5, 
                                 max(data.long$value) * 0.75, max(data.long$value))) +  # Specify custom breaks
  theme_minimal() +
  labs(title = "Exon Based Methods", x = NULL, y = "DS Methodology") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = unique(data.long$Year), labels = unique(data.long$Year)) +
  labs(fill = "Citation count")

plot_Event <- ggplot(subset(data.long, Category == "Event"),
                     aes(x = Year, y = Tool_Name, fill = value)) +
  geom_tile(height=tile.height, color = outline.color) +
  scale_fill_gradient(low = "white", high = uos_colours["Coral"], 
                      limits = c(0, max(data.long$value)),
                      breaks = c(0, max(data.long$value) * 0.25, max(data.long$value) * 0.5, 
                                 max(data.long$value) * 0.75, max(data.long$value))) +  # Specify custom breaks
  theme_minimal() +
  labs(title = "Event Based Methods", x = "Year", y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = unique(data.long$Year), labels = unique(data.long$Year), name = "Year") +
  labs(fill = "Citation count")


# Arrange plots together with white background
ggarrange(plot_Transcript, plot_Exon, plot_Event, nrow = 3, common.legend = FALSE, legend = "right", align = "v")

ggsave("Fig3.tiff", dpi = 300, width = 5.5, height = 6, bg = "white")







