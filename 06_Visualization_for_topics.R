

library(tidyverse)
library(readr)
library(openxlsx)
# 
# doc_topics_1_old <- read_csv("topics/doc_topics_가상화-융합화.csv")
# doc_topics_2_old <- read_csv("topics/doc_topics_자동화-노동 형태 다변화.csv")
# doc_topics_3_old <- read_csv("topics/doc_topics_초개인화-맞춤화.csv")
# doc_topics_4_old <- read_csv("topics/doc_topics_플랫폼의 전방위적 확산.csv")


doc_topics_1 <- read_csv("topics/doc_topics_가상화-융합화.csv")
doc_topics_2 <- read_csv("topics/doc_topics_자동화-노동 형태 다변화.csv")
doc_topics_3 <- read_csv("topics/doc_topics_초개인화-맞춤화.csv")
doc_topics_4 <- read_csv("topics/doc_topics_플랫폼의 전방위적 확산.csv")


bertopic_results_1 <- read_csv("topics/bertopic_results_가상화-융합화.csv")
bertopic_results_2 <- read_csv("topics/bertopic_results_자동화-노동 형태 다변화.csv")
bertopic_results_3 <- read_csv("topics/bertopic_results_초개인화-맞춤화.csv")
bertopic_results_4 <- read_csv("topics/bertopic_results_플랫폼의 전방위적 확산.csv")

bertopic_results_1 %>% glimpse
doc_topics_1 %>% glimpse
doc_topics_1 %>% count(Topic)
doc_topics_2 %>% count(Topic)
doc_topics_3 %>% count(Topic)
doc_topics_4 %>% count(Topic)


# doc_topics_1 %>% 
#   filter(Topic == 1) %>% 
#   select(Document) %>% 
#   sample_frac(.1) %>% 
#   write.xlsx("1_topic_1_docs.xlsx", overwrite = TRUE)

# Create a new workbook
wb <- createWorkbook()

# Loop through topics 1 to 9 and add each as a new sheet
for (topic_num in 0:8) {
  topic_data <- doc_topics_1 %>% 
    filter(Topic == topic_num) %>% 
    select(Document) %>% 
    sample_frac(.3)
  
  # Add a new sheet with the topic number as the sheet name
  addWorksheet(wb, paste0("Topic_", topic_num))
  
  # Write the topic data to the corresponding sheet
  writeData(wb, sheet = paste0("Topic_", topic_num), topic_data)
}

# Save the workbook as a single Excel file
saveWorkbook(wb, "1_topics_sampled_docs_new.xlsx", overwrite = TRUE)


# Create a new workbook
wb <- createWorkbook()

for (topic_num in 0:3) {
  topic_data <- doc_topics_2 %>% 
    filter(Topic == topic_num) %>% 
    select(Document) %>% 
    sample_frac(.3)
  
  # Add a new sheet with the topic number as the sheet name
  addWorksheet(wb, paste0("Topic_", topic_num))
  
  # Write the topic data to the corresponding sheet
  writeData(wb, sheet = paste0("Topic_", topic_num), topic_data)
}

# Save the workbook as a single Excel file
saveWorkbook(wb, "2_topics_sampled_docs_new.xlsx", overwrite = TRUE)

# Create a new workbook
wb <- createWorkbook()

for (topic_num in 0:2) {
  topic_data <- doc_topics_3 %>% 
    filter(Topic == topic_num) %>% 
    select(Document) %>% 
    sample_frac(.3)
  
  # Add a new sheet with the topic number as the sheet name
  addWorksheet(wb, paste0("Topic_", topic_num))
  
  # Write the topic data to the corresponding sheet
  writeData(wb, sheet = paste0("Topic_", topic_num), topic_data)
}

# Save the workbook as a single Excel file
saveWorkbook(wb, "3_topics_sampled_docs_new.xlsx", overwrite = TRUE)


# Create a new workbook
wb <- createWorkbook()

for (topic_num in 0:1) {
  topic_data <- doc_topics_4 %>% 
    filter(Topic == topic_num) %>% 
    select(Document) %>% 
    sample_frac(.3)
  
  # Add a new sheet with the topic number as the sheet name
  addWorksheet(wb, paste0("Topic_", topic_num))
  
  # Write the topic data to the corresponding sheet
  writeData(wb, sheet = paste0("Topic_", topic_num), topic_data)
}

# Save the workbook as a single Excel file
saveWorkbook(wb, "4_topics_sampled_docs_new.xlsx", overwrite = TRUE)



#################
# Topic Naming
# 
# 1. 각 토픽별로 Abstract 를 랜덤으로 10%씩 뽑아서 하나의 엑셀 파일로 만들고 토픽은 Sheet 별로 저장해서 하나의 파일로 만듦 
# 
# 2. 파일을 ChatGPT 에 업로드하고 아래 프롬프트 실행
# "이 엑셀 파일의 sheet 는 BERTopic 으로 나눠진 결과들입니다. 각 sheet 의 기술 abstract 를 참고하여 각 토픽 (0~8까지)의 대주제와 소주제를 만드시오. (대주제는 20자 내외, 소주제는 3개). 양식은
# 
# 토픽 0. 대주제
# - 소주제 1
# - 소주제 2
# - 소주제 3"
# 
# 3. 대주제만 뽑아서 테이블로 만듦 (아래 프롬프트 실행)
#################


# 아래 내용은 Trend 별 토픽에 대한 내용이다. 
# <Trend>와 토픽들의 대주제 (소주제 제외)만 추출해서 아래와 같이 R에서 표로 만들어줘.
# 
# Trend | Topic_no | Topic_label
# ---------------------------------
# 가상화-융합화 | 0 |  의료 영상 및 진단 장치 혁신


# Trend_and_Topic_Overview <- read_csv("Trend_and_Topic_Overview.csv")
library(readxl)
Trend_and_Topic_Overview <- read_excel("Trend_and_Topic_Overview.xlsx")
Trend_and_Topic_Overview %>% print(n=32)

#### 시각화
Trend_and_Topic_Overview %>% 
  count(Trend)

Trend_and_Topic_Overview %>% 
  filter(Trend == "가상화-융합화") %>% 
  rename(Topic = Topic_no) %>% 
  left_join(bertopic_results_1) ->
  bertopic_results_1


Trend_and_Topic_Overview %>% 
  filter(Trend == "자동화 - 노동 형태 다변화") %>% 
  rename(Topic = Topic_no) %>% 
  left_join(bertopic_results_2) ->
  bertopic_results_2

Trend_and_Topic_Overview %>% 
  filter(Trend == "초개인화-맞춤화") %>% 
  rename(Topic = Topic_no) %>% 
  left_join(bertopic_results_3) ->
  bertopic_results_3


Trend_and_Topic_Overview %>% 
  filter(Trend == "플랫폼의 전방위적 확산") %>% 
  rename(Topic = Topic_no) %>% 
  left_join(bertopic_results_4) ->
  bertopic_results_4

rbind(bertopic_results_1, 
      bertopic_results_2, 
      bertopic_results_3, 
      bertopic_results_4) ->
  trend_topic_label

trend_topic_label %>% 
  rename(type = Trend) ->
  trend_topic_label


glimpse(doc_topics_1)
glimpse(doc_topics_2)
glimpse(doc_topics_3)
glimpse(doc_topics_4)

rm(CPC)
load("C:/R/Rproj/[4]research/KISDI_Megatrend_2024/data/Bibliographic.RData.rdata")
load("C:/R/Rproj/[4]research/KISDI_Megatrend_2024/data/Priority.RData.rdata")

Priority %>%
  select(출원번호, 국가코드, 국가명) ->
  prior_n

Bibliographic %>% glimpse
Bibliographic %>%
  select(출원번호, 출원일자) ->
  Bibliographic_n

Abstract_trend <- read_csv("Abstract_trend.csv")


Abstract_trend %>%
  select(출원번호, type, 초록) %>%
  rename(Document = 초록) -> Abs_n


doc_topics_1 %>% glimpse
prior_n %>% glimpse
Bibliographic_n %>% glimpse

# 가상화-융합화
doc_topics_1 %>% 
  mutate(출원번호 = as.character(출원번호)) %>% 
  filter(Topic != -1) %>% 
  left_join(trend_topic_label %>% 
              filter(type == "가상화-융합화")) %>% 
  left_join(prior_n) -> doc_top_app_yy_1

doc_top_app_yy_1 %>% glimpse
doc_top_app_yy_1 %>% count(Year) %>% print(n=40)
doc_top_app_yy_1 %>% count(국가명)

doc_top_app_yy_1 %>% count(Topic, Topic_label)


# 자동화 - 노동 형태 다변화
doc_topics_2 %>% 
  mutate(출원번호 = as.character(출원번호)) %>% 
  filter(Topic != -1) %>% 
  left_join(trend_topic_label %>% 
              filter(type == "자동화 - 노동 형태 다변화")) %>% 
  left_join(prior_n) -> doc_top_app_yy_2

doc_top_app_yy_2 %>% glimpse
doc_top_app_yy_2 %>% count(Year) %>% print(n=40)
doc_top_app_yy_2 %>% count(Topic, Topic_label)

# 초개인화-맞춤화
doc_topics_3 %>% 
  mutate(출원번호 = as.character(출원번호)) %>% 
  filter(Topic != -1) %>% 
  left_join(trend_topic_label %>% 
              filter(type == "초개인화-맞춤화")) %>% 
  left_join(prior_n) -> doc_top_app_yy_3

doc_top_app_yy_3 %>% glimpse
doc_top_app_yy_3 %>% count(Year) %>% print(n=40)
doc_top_app_yy_3 %>% count(Topic, Topic_label)

# 플랫폼의 전방위적 확산
doc_topics_4 %>% 
  mutate(출원번호 = as.character(출원번호)) %>% 
  filter(Topic != -1) %>% 
  left_join(trend_topic_label %>% 
              filter(type == "플랫폼의 전방위적 확산")) %>% 
  left_join(prior_n) -> doc_top_app_yy_4

doc_top_app_yy_4 %>% glimpse
doc_top_app_yy_4 %>% count(Year) %>% print(n=40)
doc_top_app_yy_4 %>% count(Topic, Topic_label)


doc_top_app_yy <- rbind(doc_top_app_yy_1, 
      doc_top_app_yy_2, 
      doc_top_app_yy_3, 
      doc_top_app_yy_4)

doc_top_app_yy %>% glimpse

# Prepare data
doc_topic_distribution <- doc_top_app_yy %>%
  filter(!is.na(Topic) & !is.na(Topic_label)) %>%
  group_by(type, Topic_label) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

# Plot with facets by type and independent scales, and add count labels
ggplot(doc_topic_distribution, 
       aes(x = reorder(Topic_label, Count), 
           y = Count, fill = as.factor(Topic_label))) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend with show.legend = FALSE
  geom_text(aes(label = Count), 
            position = position_stack(vjust = 0.5),  # Center the text inside the bars
            color = "white", size = 3.5) +  # Adjust text color and size as needed
  coord_flip() +
  labs(
    title = "Topic Distribution by Megatrends",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  facet_wrap(~type, scales = "free")  # Set scales to "free" for independent axes




#################################
# Topic Trend by Year
#################################

library(scales)  # For formatting the axis labels

# Prepare the data: Filter out rows with missing 'Topic' and 'Year' values
doc_data_filtered <- doc_top_app_yy_1 %>%
  filter(!is.na(Topic), !is.na(Year)) %>%
  group_by(Topic_label, Year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  mutate(Year = as.integer(Year))  # Ensure year is treated as numeric for better plotting


# Define a more distinct custom color palette
colors <- c("#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", 
            "#911EB4", "#42D4F4", "#F032E6", "#BFEF45", "#FABEBE")


# Plot 1: Enhanced Topic count over years with Topic_label in legend
ggplot(doc_data_filtered, aes(x = Year, y = Count, 
                              color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "", y = "",
    title = "Trend by Topic",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(min(doc_data_filtered$Year), max(doc_data_filtered$Year), by = 2)) +
  geom_text(data = doc_data_filtered %>% filter(Count == max(Count)), 
            aes(label = Count), vjust = -1.5, size = 4, color = "black")  # Adjust position of da


# Plot 1: Enhanced Topic count over years with Topic_label in legend
ggplot(doc_data_filtered %>% filter(Year >= 2013), 
       aes(x = Year, y = Count, 
           color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "", y = "",
    title = "Trend by Topic since 2013",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(2013, max(doc_data_filtered$Year), by = 2))


# Prepare the data: Filter out rows with missing 'Topic' and 'Year' values, then calculate rank
doc_data_ranked <- doc_top_app_yy_1 %>%
  filter(!is.na(Topic), !is.na(Year), !is.na(Topic_label)) %>%
  group_by(Topic_label, Year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  arrange(Year, desc(Count)) %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Count, ties.method = "min")) %>%  # Rank by Count, with highest Count as rank 1
  ungroup() %>%
  mutate(Year = as.integer(Year))  # Ensure year is treated as numeric for better plotting

# Define a more distinct custom color palette
colors <- c("#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", 
            "#911EB4", "#42D4F4", "#F032E6", "#BFEF45", "#FABEBE")

# Plot 1: Rank changes over years with Topic_label in legend
ggplot(doc_data_ranked, aes(x = Year, y = Rank, color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_y_reverse(breaks = 1:max(doc_data_ranked$Rank)) +  # Reverse y-axis for rank (1 is highest)
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "",
    y = "",
    title = "Topic Rank Trends Over Time",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(min(doc_data_ranked$Year), max(doc_data_ranked$Year), by = 2))


###############
# 2nd Trend


#####
# Prepare the data: Filter out rows with missing 'Topic' and 'Year' values
doc_data_filtered <- doc_top_app_yy_2 %>%
  filter(!is.na(Topic), !is.na(Year)) %>%
  group_by(Topic_label, Year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  mutate(Year = as.integer(Year))  # Ensure year is treated as numeric for better plotting


# Plot 1: Enhanced Topic count over years with Topic_label in legend
ggplot(doc_data_filtered, aes(x = Year, y = Count, 
                              color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "", y = "",
    title = "Trend by Topic",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(min(doc_data_filtered$Year), max(doc_data_filtered$Year), by = 2)) +
  geom_text(data = doc_data_filtered %>% filter(Count == max(Count)), 
            aes(label = Count), vjust = -1.5, size = 4, color = "black")  # Adjust position of da


# Plot 1: Enhanced Topic count over years with Topic_label in legend
ggplot(doc_data_filtered %>% filter(Year >= 2013), 
       aes(x = Year, y = Count, 
           color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "", y = "",
    title = "Trend by Topic since 2013",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(2013, max(doc_data_filtered$Year), by = 2))


# Prepare the data: Filter out rows with missing 'Topic' and 'Year' values, then calculate rank
doc_data_ranked <- doc_top_app_yy_2 %>%
  filter(!is.na(Topic), !is.na(Year), !is.na(Topic_label)) %>%
  group_by(Topic_label, Year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  arrange(Year, desc(Count)) %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Count, ties.method = "min")) %>%  # Rank by Count, with highest Count as rank 1
  ungroup() %>%
  mutate(Year = as.integer(Year))  # Ensure year is treated as numeric for better plotting

# Define a more distinct custom color palette
colors <- c("#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", 
            "#911EB4", "#42D4F4", "#F032E6", "#BFEF45", "#FABEBE")

# Plot 1: Rank changes over years with Topic_label in legend
ggplot(doc_data_ranked, aes(x = Year, y = Rank, color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_y_reverse(breaks = 1:max(doc_data_ranked$Rank)) +  # Reverse y-axis for rank (1 is highest)
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "",
    y = "",
    title = "Topic Rank Trends Over Time",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(min(doc_data_ranked$Year), max(doc_data_ranked$Year), by = 2))




###############
# 3rd Trend


#####
# Prepare the data: Filter out rows with missing 'Topic' and 'Year' values
doc_data_filtered <- doc_top_app_yy_3 %>%
  filter(!is.na(Topic), !is.na(Year)) %>%
  group_by(Topic_label, Year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  mutate(Year = as.integer(Year))  # Ensure year is treated as numeric for better plotting


# Plot 1: Enhanced Topic count over years with Topic_label in legend
ggplot(doc_data_filtered, aes(x = Year, y = Count, 
                              color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "", y = "",
    title = "Trend by Topic",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(min(doc_data_filtered$Year), max(doc_data_filtered$Year), by = 2)) +
  geom_text(data = doc_data_filtered %>% filter(Count == max(Count)), 
            aes(label = Count), vjust = -1.5, size = 4, color = "black")  # Adjust position of da


# Plot 1: Enhanced Topic count over years with Topic_label in legend
ggplot(doc_data_filtered %>% filter(Year >= 2013), 
       aes(x = Year, y = Count, 
           color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "", y = "",
    title = "Trend by Topic since 2013",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(2013, max(doc_data_filtered$Year), by = 2))


# Prepare the data: Filter out rows with missing 'Topic' and 'Year' values, then calculate rank
doc_data_ranked <- doc_top_app_yy_3 %>%
  filter(!is.na(Topic), !is.na(Year), !is.na(Topic_label)) %>%
  group_by(Topic_label, Year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  arrange(Year, desc(Count)) %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Count, ties.method = "min")) %>%  # Rank by Count, with highest Count as rank 1
  ungroup() %>%
  mutate(Year = as.integer(Year))  # Ensure year is treated as numeric for better plotting

# Plot 1: Rank changes over years with Topic_label in legend
ggplot(doc_data_ranked, aes(x = Year, y = Rank, color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_y_reverse(breaks = 1:max(doc_data_ranked$Rank)) +  # Reverse y-axis for rank (1 is highest)
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "",
    y = "",
    title = "Topic Rank Trends Over Time",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(min(doc_data_ranked$Year), max(doc_data_ranked$Year), by = 2))





###############
# 4th Trend


#####
# Prepare the data: Filter out rows with missing 'Topic' and 'Year' values
doc_data_filtered <- doc_top_app_yy_4 %>%
  filter(!is.na(Topic), !is.na(Year)) %>%
  group_by(Topic_label, Year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  mutate(Year = as.integer(Year))  # Ensure year is treated as numeric for better plotting


# Plot 1: Enhanced Topic count over years with Topic_label in legend
ggplot(doc_data_filtered, aes(x = Year, y = Count, 
                              color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "", y = "",
    title = "Trend by Topic",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(min(doc_data_filtered$Year), max(doc_data_filtered$Year), by = 2)) +
  geom_text(data = doc_data_filtered %>% filter(Count == max(Count)), 
            aes(label = Count), vjust = -1.5, size = 4, color = "black")  # Adjust position of da


# Plot 1: Enhanced Topic count over years with Topic_label in legend
ggplot(doc_data_filtered %>% filter(Year >= 2013), 
       aes(x = Year, y = Count, 
           color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "", y = "",
    title = "Trend by Topic since 2013",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(2013, max(doc_data_filtered$Year), by = 2))


# Prepare the data: Filter out rows with missing 'Topic' and 'Year' values, then calculate rank
doc_data_ranked <- doc_top_app_yy_4 %>%
  filter(!is.na(Topic), !is.na(Year), !is.na(Topic_label)) %>%
  group_by(Topic_label, Year) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  arrange(Year, desc(Count)) %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Count, ties.method = "min")) %>%  # Rank by Count, with highest Count as rank 1
  ungroup() %>%
  mutate(Year = as.integer(Year))  # Ensure year is treated as numeric for better plotting

# Plot 1: Rank changes over years with Topic_label in legend
ggplot(doc_data_ranked, aes(x = Year, y = Rank, color = Topic_label, group = Topic_label)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2) +  # Add points for clarity at each year
  scale_y_reverse(breaks = 1:max(doc_data_ranked$Rank)) +  # Reverse y-axis for rank (1 is highest)
  scale_color_manual(values = colors) +  # Use custom color palette
  labs(
    x = "",
    y = "",
    title = "Topic Rank Trends Over Time",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  guides(color = guide_legend(ncol = 2)) +  # Arrange legend in 2 columns
  scale_x_continuous(breaks = seq(min(doc_data_ranked$Year), max(doc_data_ranked$Year), by = 2))




###############################################
# topic by country

library(dplyr)
library(ggplot2)
library(tidytext)  # For reorder_within and scale_x_reordered

# Prepare data: Group by Topic_label and 국가명 (Country) and count occurrences
doc_topic_country_distribution <- doc_top_app_yy_1 %>%
  filter(!is.na(Topic_label), !is.na(국가명)) %>%
  group_by(Topic_label, 국가명) %>%
  summarize(Count = n(), .groups = 'drop')

# Plot: Stacked bar chart by country, faceted by topic, sorted in descending order
ggplot(doc_topic_country_distribution, 
       aes(x = reorder_within(국가명, Count, Topic_label), y = Count, fill = 국가명)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend for country colors
  labs(
    x = "",
    y = ""
  ) +
  coord_flip() +  # Flip the axes for readability
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  facet_wrap(~Topic_label, scales = "free") +  # Set both x and y scales to be free for each facet
  scale_x_reordered()  # Use scale_x_reordered to correctly display reordered x-axis within facets


# Prepare data: Group by Topic_label and 국가명 (Country) and count occurrences
doc_topic_country_distribution <- doc_top_app_yy_2 %>%
  filter(!is.na(Topic_label), !is.na(국가명)) %>%
  group_by(Topic_label, 국가명) %>%
  summarize(Count = n(), .groups = 'drop')

# Plot: Stacked bar chart by country, faceted by topic, sorted in descending order
ggplot(doc_topic_country_distribution, 
       aes(x = reorder_within(국가명, Count, Topic_label), y = Count, fill = 국가명)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend for country colors
  labs(
    x = "",
    y = ""
  ) +
  coord_flip() +  # Flip the axes for readability
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  facet_wrap(~Topic_label, scales = "free") +  # Set both x and y scales to be free for each facet
  scale_x_reordered()  # Use scale_x_re



# Prepare data: Group by Topic_label and 국가명 (Country) and count occurrences
doc_topic_country_distribution <- doc_top_app_yy_3 %>%
  filter(!is.na(Topic_label), !is.na(국가명)) %>%
  group_by(Topic_label, 국가명) %>%
  summarize(Count = n(), .groups = 'drop')

# Plot: Stacked bar chart by country, faceted by topic, sorted in descending order
ggplot(doc_topic_country_distribution, 
       aes(x = reorder_within(국가명, Count, Topic_label), y = Count, fill = 국가명)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend for country colors
  labs(
    x = "",
    y = ""
  ) +
  coord_flip() +  # Flip the axes for readability
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  facet_wrap(~Topic_label, scales = "free") +  # Set both x and y scales to be free for each facet
  scale_x_reordered()  # Use scale_x_re


# Prepare data: Group by Topic_label and 국가명 (Country) and count occurrences
doc_topic_country_distribution <- doc_top_app_yy_4 %>%
  filter(!is.na(Topic_label), !is.na(국가명)) %>%
  group_by(Topic_label, 국가명) %>%
  summarize(Count = n(), .groups = 'drop')

# Plot: Stacked bar chart by country, faceted by topic, sorted in descending order
ggplot(doc_topic_country_distribution, 
       aes(x = reorder_within(국가명, Count, Topic_label), y = Count, fill = 국가명)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend for country colors
  labs(
    x = "",
    y = ""
  ) +
  coord_flip() +  # Flip the axes for readability
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  facet_wrap(~Topic_label, scales = "free") +  # Set both x and y scales to be free for each facet
  scale_x_reordered()  # Use scale_x_re
















