#### Fig 2i ####
library(tidyverse)
library(Rtsne)
library(e1071)
library(ggplot2)

data <- read.table("clipboard",header=T,sep="\t")#github-data.xlsx(sheet Fig 2i)
data_scaled <- scale(data[, -1])
tsne_results <- Rtsne(data_scaled, dims = 2, perplexity = 7) 
tsne_df <- data.frame(tsne_results$Y)
colnames(tsne_df) <- c("tSNE_1", "tSNE_2")
tsne_df$Sample <- data$Sample
tsne_df$Group <- ifelse(grepl("CTRL", tsne_df$Sample), "CTRL", "PD")
tsne_df$Group <- factor(tsne_df$Group)
svm_model <- svm(Group ~ tSNE_1 + tSNE_2, data = tsne_df, kernel = "linear", cost = 1, gamma = 0.1)
x_range <- seq(min(tsne_df$tSNE_1) - 1, max(tsne_df$tSNE_1) + 1, length.out = 200)
y_range <- seq(min(tsne_df$tSNE_2) - 1, max(tsne_df$tSNE_2) + 1, length.out = 200)
grid <- expand.grid(tSNE_1 = x_range, tSNE_2 = y_range)
grid$Group <- predict(svm_model, newdata = grid)

ggplot(tsne_df, aes(x = tSNE_1, y = tSNE_2, color = Group)) +
  geom_point(size = 3) +  
  geom_tile(data = grid, aes(x = tSNE_1, y = tSNE_2, fill = Group), alpha = 0.3) +  
  labs(title = "SVM Decision Boundary", x = "tSNE_1", y = "tSNE_2") +
  theme_minimal() + 
  scale_fill_manual(values = c("CTRL" = "#156385", "PD" = "#BC2321")) + 
  scale_color_manual(values = c("CTRL" = "#156385", "PD" = "#BC2321"))  
  
  
  
#### Fig 3h ####
library(tidyverse)
library(Rtsne)
library(e1071)
library(ggplot2) 

data <- read.table("clipboard",header=T,sep="\t")#github-data.xlsx(sheet Fig 3h)
data_scaled <- scale(data[, -1])
tsne_results <- Rtsne(data_scaled, dims = 2, perplexity = 15) 
tsne_df <- data.frame(tsne_results$Y)
colnames(tsne_df) <- c("tSNE_1", "tSNE_2")
tsne_df$Sample <- data$Sample
tsne_df$Group <- ifelse(grepl("CTRL", tsne_df$Sample), "CTRL", 
                        ifelse(grepl("PDVH", tsne_df$Sample), "PDVH", "PD"))
tsne_df$Group <- factor(tsne_df$Group)
svm_model <- svm(Group ~ tSNE_1 + tSNE_2, data = tsne_df, kernel = "linear", cost = 1, gamma = 0.1)
x_range <- seq(min(tsne_df$tSNE_1) - 1, max(tsne_df$tSNE_1) + 1, length.out = 200)
y_range <- seq(min(tsne_df$tSNE_2) - 1, max(tsne_df$tSNE_2) + 1, length.out = 200)
grid <- expand.grid(tSNE_1 = x_range, tSNE_2 = y_range)
grid$Group <- predict(svm_model, newdata = grid)

ggplot(tsne_df, aes(x = tSNE_1, y = tSNE_2, color = Group)) +
  geom_point(size = 3) +  
  geom_tile(data = grid, aes(x = tSNE_1, y = tSNE_2, fill = Group), alpha = 0.3) +  
  labs(title = "SVM Decision Boundary", x = "tSNE_1", y = "tSNE_2") +
  theme_minimal() + 
  scale_fill_manual(values = c("CTRL" = "#156385", "PD" = "#BC2321", "PDVH" ="#4F9B72")) + 
  scale_color_manual(values = c("CTRL" = "#156385", "PD" = "#BC2321", "PDVH" ="#4F9B72"))  



#### Fig 4c ####
library(ggplot2)
library(ggalluvial)

Control <- data.frame(
  source = c(
    "Motor-related Behavior", "Motor-related Behavior", "Motor-related Behavior",
    "Non-motor Behavior", "Non-motor Behavior", "Non-motor Behavior",
    "NatureBehavior", "NatureBehavior", "NatureBehavior"
  ),
  target = c(
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior",
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior",
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior"
  ),
  value = c(0.305252101,0.12394958,0.570798319,0.393746688,0.067302597,0.538950715,0.686342903,0.313657097,0)
)
behavior_order <- c("NatureBehavior","Motor-related Behavior", "Non-motor Behavior")
Control$source <- factor(Control$source, levels = behavior_order)
Control$target <- factor(Control$target, levels = behavior_order)
color_palette <- c(
    "NatureBehavior" = "#5A8FC8",  
    "Motor-related Behavior" = "#cd5c5c",    
    "Non-motor Behavior" = "#4C9F57"           
)

PD <- data.frame(
  source = c(
    "Motor-related Behavior", "Motor-related Behavior", "Motor-related Behavior",
    "Non-motor Behavior", "Non-motor Behavior", "Non-motor Behavior",
    "NatureBehavior", "NatureBehavior", "NatureBehavior"
  ),
  target = c(
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior",
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior",
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior"
  ),
  value = c(0.292932424,0.138871668,0.568195908,0.289425982,0.074924471,0.635649547,0.625086866,0.374913134,0)
)

behavior_order <- c("NatureBehavior","Motor-related Behavior", "Non-motor Behavior")
PD$source <- factor(PD$source, levels = behavior_order)
PD$target <- factor(PD$target, levels = behavior_order)

PDVH <- data.frame(
  source = c(
    "Motor-related Behavior", "Motor-related Behavior", "Motor-related Behavior",
    "Non-motor Behavior", "Non-motor Behavior", "Non-motor Behavior",
    "NatureBehavior", "NatureBehavior", "NatureBehavior"
  ),
  target = c(
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior",
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior",
    "Motor-related Behavior", "Non-motor Behavior", "NatureBehavior"
  ),
  value = c(0.189384422,0.208542714,0.602072864,0.266583748,0.134328358,0.599087894,0.576785714,0.423214286,0)
)

behavior_order <- c("NatureBehavior","Motor-related Behavior", "Non-motor Behavior")
PDVH$source <- factor(PDVH$source, levels = behavior_order)
PDVH$target <- factor(PDVH$target, levels = behavior_order)

ggplot(Control, aes(axis1 = source, axis2 = target, y = value)) +#PD、PDVH
  geom_alluvium(aes(fill = source), width = 1/12) +   
  geom_stratum(aes(fill = target), width = 1/12, color = "white") + 
  geom_stratum(aes(fill = source), width = 1/12, color = "white") + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), color = "black", size = 5, angle = 90) + 
  scale_fill_manual(values = color_palette) +  
  labs(
    title = "Control shift",
    x = NULL, 
    y = NULL,
    subtitle = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    axis.line = element_blank(),     
    panel.background = element_blank(),   
    plot.background = element_blank(),    
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.margin = margin(10, 10, 10, 10)
  )



#### Fig 4h ####
library(readxl)
library(tidyr)
library(nnet)
library(dplyr)
library(ggplot2)

final_merged_data <-read.table("clipboard",header=T,row.names=1,sep="\t")#github-data.xlsx(sheet Fig 4h)
final_merged_data$Group <- as.factor(final_merged_data$Group)
set.seed(123)
train_indices <- sample(1:nrow(final_merged_data), size = 0.8 * nrow(final_merged_data))
train_data <- final_merged_data[train_indices, ]
test_data <- final_merged_data[-train_indices, ]
logistic_model_multinom <- multinom(Group ~ M1_proportion + M1_frenqunecy + M34_proportion + M34_frequency + Time_bin, 
                                    data = train_data)
summary(logistic_model_multinom)

#用test_data来验证模型
pred <- predict(logistic_model_multinom, newdata = test_data)
true <- test_data$Y
rmse <- sqrt(mean((pred-true)^2))
mae <- mean(abs(pred-true))
> table(pred, true)
         true
pred      Control PD PDVH
  Control      53 29   25
  PD           50 58   35
  PDVH         28 41   65


library(caret)

conf_mat <- confusionMatrix(pred, test_data$Group)
print(conf_mat)


library(ggplot2)
library(dplyr)
library(reshape2)

cm <- table(Predicted = pred, Actual = test_data$Group)
cm_df <- as.data.frame(cm)

ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()


















PD <- final_merged_data %>% filter(Group == "PD")
# 定义回归系数
intercept <- 0.239849
beta1 <- 0.06615865
beta2 <-  -0.4372398
beta3 <- 0.06722804
beta4 <- -0.3087105
beta5 <- -0.003291024
# 计算预测值
PD$predicted_Y <- intercept +
  beta1 * PD$M1_proportion +
  beta2 * PD$M1_frenqunecy +
  beta3 * PD$M34_proportion +
  beta4 * PD$M34_frequency +
  beta5 * PD$Time_bin 

PDVH <- final_merged_data %>% filter(Group == "PDVH")
# 定义回归系数
intercept <- -1.025564
beta1 <- 0.05869361
beta2 <-  0.0118727
beta3 <- 0.04862555
beta4 <- 0.2343462
beta5 <- 0.007451315
# 计算预测值
PDVH$predicted_Y <- intercept +
  beta1 * PDVH$M1_proportion +
  beta2 * PDVH$M1_frenqunecy +
  beta3 * PDVH$M34_proportion +
  beta4 * PDVH$M34_frequency +
  beta5 * PDVH$Time_bin

data <- rbind(PD,PDVH)
summary_data <- data %>%
  group_by(Group, Time_bin) %>%
  summarise(
    mean_y = mean(predicted_Y),
    sem_y = sd(predicted_Y) / sqrt(n()),
    .groups = "drop"
  )

summary_data_wide <- summary_data %>%
  pivot_wider(
    names_from = Group, 
    values_from = c(mean_y, sem_y),  
    names_glue = "{.value}_{Group}" 
  )

ggplot(summary_data_wide, aes(x = Time_bin)) +
  geom_col(aes(y = mean_y_PD, fill = "PD"), color = "#BC2321", size = 0.6, width = 1) + 
  geom_col(aes(y = mean_y_PDVH, fill = "PDVH"), color = "#4F9B72", size = 0.6, width = 1) + 
  geom_errorbar(aes(ymin = mean_y_PD - sem_y_PD, ymax = mean_y_PD + sem_y_PD), 
                width = 0.3, size = 0.8) +  
  geom_errorbar(aes(ymin = mean_y_PDVH - sem_y_PDVH, ymax = mean_y_PDVH + sem_y_PDVH), 
                width = 0.3, size = 0.8) +  
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30,35,40), limits = c(0.5, 40.5)) +
  scale_fill_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) + 
  labs(
    title = "Predicted Y (Control as baseline)",
    x = "Time Bin",
    y = "Predicted Y",
    fill = "Group"
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12), 
    legend.key.size = unit(1, "cm"), 
    legend.box.margin = margin(10, 10, 10, 10),  
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), 
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 14), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(15, 15, 15, 15)
  )



#### Fig 5e ####
library(tidyverse)
library(Rtsne)
library(e1071)
library(ggplot2) 

data <- read.table("clipboard",header=T,sep="\t")#github-data.xlsx(sheet Fig 5e)
data$Group <- ifelse(grepl("PDVH", data$Sample), "PDVH", 
                     ifelse(grepl("PDPM", data$Sample), "PDPM", "PDSA"))
data$Group <- factor(data$Group)
svm_model <- svm(Group ~ X1 + X34, data = data, kernel = "linear", cost = 1, gamma = 0.1)
x_range <- seq(min(data$X1) - 0.02, max(data$X1) + 0.02, length.out = 200)
y_range <- seq(min(data$X34) - 0.02, max(data$X34) + 0.02, length.out = 200)
grid <- expand.grid(X1 = x_range, X34 = y_range)
grid$Group <- predict(svm_model, newdata = grid)

ggplot(data, aes(x = X1, y = X34, color = Group)) +
  geom_point(size = 3) + 
  geom_tile(data = grid, aes(x = X1, y = X34, fill = Group), alpha = 0.3) +
  labs(title = "SVM Decision Boundary (No Dimensionality Reduction)", x = "X1", y = "X34") +
  theme_minimal() + 
  scale_fill_manual(values = c("PDVH" = "#4F9B72", "PDPM" = "#FBAE17", "PDSA" = "#BC2321")) + 
  scale_color_manual(values = c("PDVH" = "#4F9B72", "PDPM" = "#FBAE17", "PDSA" = "#BC2321"))
  


#### Fig 5f ####
library(readxl)
library(tidyr)
library(nnet)
library(dplyr)
library(ggplot2)

final_merged_data_test <- read.table("clipboard",header=T,sep="\t",row.names=1)##github-data.xlsx(sheet Fig 5f)
pdvh <- final_merged_data_test %>% filter(Group == "pdvh")
# 定义回归系数
intercept <- -1.025564
beta1 <- 0.05869361
beta2 <-  0.0118727
beta3 <- 0.04862555
beta4 <- 0.2343462
beta5 <- 0.007451315
# 计算预测值
pdvh$predicted_Y <- intercept +
  beta1 * pdvh$M1_proportion +
  beta2 * pdvh$M1_frenqunecy +
  beta3 * pdvh$M34_proportion +
  beta4 * pdvh$M34_frequency +
  beta5 * pdvh$Time_bin 
pim <- final_merged_data_test %>% filter(Group == "pim")
# 计算预测值
pim$predicted_Y <- intercept +
  beta1 * pim$M1_proportion +
  beta2 * pim$M1_frenqunecy +
  beta3 * pim$M34_proportion +
  beta4 * pim$M34_frequency +
  beta5 * pim$Time_bin 

data<-rbind(pdvh,pim)
summary_data <- data %>%
  group_by(Group, Time_bin) %>%
  summarise(
    mean_y = mean(predicted_Y),
    sem_y = sd(predicted_Y) / sqrt(n()),
    .groups = "drop"
  )
summary_data_wide <- summary_data %>%
  pivot_wider(
    names_from = Group, 
    values_from = c(mean_y, sem_y),  
    names_glue = "{.value}_{Group}" 
  )

ggplot(summary_data_wide, aes(x = Time_bin)) +
  geom_col(aes(y = mean_y_pdvh, fill = "pdvh"), color = "#4F9B72", size = 0.6, width = 1) + 
  geom_col(aes(y = mean_y_pim, fill = "pim"), color = "#f1aa25", size = 0.6, width = 1) + 
  geom_errorbar(aes(ymin = mean_y_pdvh - sem_y_pdvh, ymax = mean_y_pdvh + sem_y_pdvh), 
                width = 0.3, size = 0.8) +  
  geom_errorbar(aes(ymin = mean_y_pim - sem_y_pim, ymax = mean_y_pim + sem_y_pim), 
                width = 0.3, size = 0.8) +  
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30,35,40), limits = c(0.5, 40.5)) +
  scale_fill_manual(values = c("pdvh" = "#4F9B72", "pim" = "#f1aa25")) + 
  labs(
    title = "Predicted Y",
    x = "Time Bin",
    y = "Predicted Y",
    fill = "Group"
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12), 
    legend.key.size = unit(1, "cm"), 
    legend.box.margin = margin(10, 10, 10, 10),  
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), 
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 14), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(15, 15, 15, 15)
  )
  


###review_roc###
library(dplyr)
df <- read.table("clipboard", header = T, sep = "\t")#github-data sheet figure 4h

# 分组求和  summary_result保存至github sheet review ROC
summary_result <- df %>%
  group_by(Mouse_Group) %>%
  summarise(
    M1_proportion_sum = sum(M1_proportion, na.rm = TRUE),
    M1_frenqunecy_sum = sum(M1_frenqunecy, na.rm = TRUE),
    M34_proportion_sum = sum(M34_proportion, na.rm = TRUE),
    M34_frequency_sum = sum(M34_frequency, na.rm = TRUE)
  )

library(pROC)
summary_result$label <- ifelse(grepl("^PDVH", summary_result$Mouse_Group), 1, 0)
model_all <- glm(label ~ M1_proportion_sum + M1_frenqunecy_sum + M34_proportion_sum + M34_frequency_sum,
                 data = summary_result, family = binomial)
roc_all <- roc(summary_result$label, predict(model_all, type = "response"))
model_M1 <- glm(label ~ M1_proportion_sum + M1_frenqunecy_sum, data = summary_result, family = binomial)
roc_M1 <- roc(summary_result$label, predict(model_M1, type = "response"))
model_M34 <- glm(label ~ M34_proportion_sum + M34_frequency_sum, data = summary_result, family = binomial)
roc_M34 <- roc(summary_result$label, predict(model_M34, type = "response"))
plot(roc_all, col = "red", print.auc = TRUE, main = "ROC for PDVH vs PD")
plot(roc_M1, col = "blue", print.auc = TRUE, add = TRUE)
plot(roc_M34, col = "green", print.auc = TRUE, add = TRUE)
legend("bottomright", legend = c("All variables", "M1 only", "M34 only"),
       col = c("red", "blue", "green"), lwd = 2)
auc(roc_all)
auc(roc_M1)
auc(roc_M34)
roc.test(roc_all, roc_M1, method = "delong")
roc.test(roc_all, roc_M34, method = "delong")


p<- ggroc(list(all=roc_all,M1=roc_M1,M34=roc_M34),legacy.axes=T, linewidth=0.9) 
p_aes<-p + scale_colour_manual(labels=c(paste0("M1+M34 ( AUC = ",round(roc_all$auc[[1]],2)," )"),paste0("M1 ( AUC = ",round(roc_M1$auc[[1]],2)," )"),paste0("M34 ( AUC = ",round(roc_M34$auc[[1]],2)," )")), values = c("#c26c36","#1e337f","#117f61")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    geom_abline(slope = 1,intercept = 0,lty=3, size=0.6) +
    theme_classic() +
    theme(
        plot.margin = margin(t = 40, r = 20, b = 20, l = 20),     # 上、右、下、左方空白
        axis.line = element_line(colour = "grey10", linewidth = 0.23), # 修饰坐标轴
        axis.ticks.y = element_line(linewidth = 0.3, colour = "grey10"), 
        axis.ticks.x = element_line(linewidth = 0.3, colour = "grey10") , 
        axis.ticks.length = unit(0.2, "cm"),
        axis.text.x = element_text(size = 16, colour = "grey10", angle = 0, vjust = 0.5, hjust = 0.5), 
        axis.text.y = element_text(size = 16, colour = "grey10", angle = 0, vjust = 0.5, hjust = 0.5),
        panel.grid.major.y = element_line(colour = "grey80"),     # 背景网格线
        legend.box.background = element_rect(colour = "grey50",linewidth = 2),  # 修饰legend
        legend.key.width = unit(35, "pt"),
        legend.text = element_text(size = 16),
        legend.title = element_blank(), 
        legend.position = c(0.7, 0.1), 
        legend.margin = margin(1,1,1,1), 
        plot.title = element_text(size = 20, vjust = 10),		# 图片标题
        axis.title.y = element_text(color = "grey10",size = 18), # 设置 y 轴标签颜色
        axis.title.x = element_text(color = "grey10",size = 18), # 设置 y 轴标签颜色
    ) + 
    labs( # 文本
        title = "ROC curves", 
        x = "1 - Specificity",
        y = "Sensitivity"
    )
pdf("G:/Bricks/张哥/revise/ROC_M1_M34.pdf")
p_aes
dev.off()




summary_result$label <- ifelse(grepl("^PDVH", summary_result$Mouse_Group), 1, 0)
model_all <- glm(label ~ M1_frenqunecy_sum + M34_frequency_sum,
                 data = summary_result, family = binomial)
roc_all <- roc(summary_result$label, predict(model_all, type = "response"))
model_M1 <- glm(label ~ M1_frenqunecy_sum, data = summary_result, family = binomial)
roc_M1 <- roc(summary_result$label, predict(model_M1, type = "response"))
model_M34 <- glm(label ~ M34_frequency_sum, data = summary_result, family = binomial)
roc_M34 <- roc(summary_result$label, predict(model_M34, type = "response"))
plot(roc_all, col = "red", print.auc = TRUE, main = "ROC for PDVH vs PD")
plot(roc_M1, col = "blue", print.auc = TRUE, add = TRUE)
plot(roc_M34, col = "green", print.auc = TRUE, add = TRUE)
legend("bottomright", legend = c("All variables", "M1 only", "M34 only"),
       col = c("red", "blue", "green"), lwd = 2)
auc(roc_all)
auc(roc_M1)
auc(roc_M34)
roc.test(roc_all, roc_M1, method = "delong")
roc.test(roc_all, roc_M34, method = "delong")






library(pROC)
summary_result$label <- ifelse(grepl("^PDVH", summary_result$Mouse_Group), 1, 0)
model_all <- glm(label ~ staring.to.climbing + staring.to.ht + ht.to.staring + rotate.to.staring,
                 data = summary_result, family = binomial)
roc_all <- roc(summary_result$label, predict(model_all, type = "response"))
model_all <- glm(label ~ staring.to.climbing + staring.to.ht + ht.to.staring + rotate.to.staring + M1_proportion_sum + M1_frenqunecy_sum + M34_proportion_sum + M34_frequency_sum,
                 data = summary_result, family = binomial)
roc_all <- roc(summary_result$label, predict(model_all, type = "response"))

Warning messages:
1: glm.fit:算法没有聚合 
2: glm.fit:拟合機率算出来是数值零或一 

library(detectseparation)
model_all <- glm(label ~ staring.to.climbing + staring.to.ht + ht.to.staring + rotate.to.staring + M1_proportion_sum + M1_frenqunecy_sum + M34_proportion_sum + M34_frequency_sum,
                 data = summary_result, family = binomial, method = "detect_separation")#检查是否存在完美分离（Perfect Separation）
library(car)
vif(model_all)#查看变量共线性（VIF）
library(arm)
#使用 bayesglm() 或 glmnet()（带正则化）
model_bayes <- bayesglm(label ~ staring.to.climbing + staring.to.ht + ht.to.staring + rotate.to.staring + M1_proportion_sum + M1_frenqunecy_sum + M34_proportion_sum + M34_frequency_sum, data = summary_result, family = binomial)
library(glmnet)
X <- model.matrix(label ~ staring.to.climbing + staring.to.ht + ht.to.staring + rotate.to.staring + M1_proportion_sum + M1_frenqunecy_sum + M34_proportion_sum + M34_frequency_sum, data = summary_result)[,-1]
y <- summary_result$label
model_glmnet <- glmnet(X, y, family = "binomial")





#transition
library(pROC)
summary_result <- read.table("clipboard", header = T, sep = "\t")
summary_result$label <- ifelse(grepl("^PDVH", summary_result$Mouse_Group), 1, 0)
model_all <- glm(label ~ staring_to_ht +  ht_to_staring + M1_proportion_sum + M1_frenqunecy_sum + M34_proportion_sum + M34_frequency_sum,
                 data = summary_result, family = binomial)
roc_all <- roc(summary_result$label, predict(model_all, type = "response"))
model_M1 <- glm(label ~ M1_proportion_sum + M1_frenqunecy_sum, data = summary_result, family = binomial)
roc_M1 <- roc(summary_result$label, predict(model_M1, type = "response"))
model_M34 <- glm(label ~ M34_proportion_sum + M34_frequency_sum, data = summary_result, family = binomial)
roc_M34 <- roc(summary_result$label, predict(model_M34, type = "response"))
model_transition <- glm(label ~ staring_to_ht +  ht_to_staring, data = summary_result, family = binomial)
roc_transition <- roc(summary_result$label, predict(model_transition, type = "response"))
model_M1_M34 <- glm(label ~ M1_proportion_sum + M1_frenqunecy_sum + M34_proportion_sum + M34_frequency_sum,
                 data = summary_result, family = binomial)
roc_M1_M34 <- roc(summary_result$label, predict(model_M1_M34, type = "response"))
plot(roc_transition, col = "red", print.auc = TRUE, main = "ROC for PDVH vs PD")
plot(roc_M1, col = "blue", print.auc = TRUE, add = TRUE)
plot(roc_M34, col = "green", print.auc = TRUE, add = TRUE)
legend("bottomright", legend = c("All variables", "M1 only", "M34 only"),
       col = c("red", "blue", "green"), lwd = 2)
auc(roc_transition)
auc(roc_M1)
auc(roc_M34)
roc.test(roc_transition, roc_M1, method = "delong")
roc.test(roc_transition, roc_M34, method = "delong")

p<- ggroc(list(Transition=roc_transition,M1=roc_M1,M34=roc_M34),legacy.axes=T, linewidth=0.9) 
p_aes<-p + scale_colour_manual(labels=c(paste0("Transition ( AUC = ",round(roc_transition$auc[[1]],2)," )"),paste0("M1 ( AUC = ",round(roc_M1$auc[[1]],2)," )"),paste0("M34 ( AUC = ",round(roc_M34$auc[[1]],2)," )")), values = c("#c26c36","#1e337f","#117f61")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    geom_abline(slope = 1,intercept = 0,lty=3, size=0.6) +
    theme_classic() +
    theme(
        plot.margin = margin(t = 40, r = 20, b = 20, l = 20),     # 上、右、下、左方空白
        axis.line = element_line(colour = "grey10", linewidth = 0.23), # 修饰坐标轴
        axis.ticks.y = element_line(linewidth = 0.3, colour = "grey10"), 
        axis.ticks.x = element_line(linewidth = 0.3, colour = "grey10") , 
        axis.ticks.length = unit(0.2, "cm"),
        axis.text.x = element_text(size = 16, colour = "grey10", angle = 0, vjust = 0.5, hjust = 0.5), 
        axis.text.y = element_text(size = 16, colour = "grey10", angle = 0, vjust = 0.5, hjust = 0.5),
        panel.grid.major.y = element_line(colour = "grey80"),     # 背景网格线
        legend.box.background = element_rect(colour = "grey50",linewidth = 2),  # 修饰legend
        legend.key.width = unit(35, "pt"),
        legend.text = element_text(size = 16),
        legend.title = element_blank(), 
        legend.position = c(0.7, 0.1), 
        legend.margin = margin(1,1,1,1), 
        plot.title = element_text(size = 20, vjust = 10),		# 图片标题
        axis.title.y = element_text(color = "grey10",size = 18), # 设置 y 轴标签颜色
        axis.title.x = element_text(color = "grey10",size = 18), # 设置 y 轴标签颜色
    ) + 
    labs( # 文本
        title = "ROC curves", 
        x = "1 - Specificity",
        y = "Sensitivity"
    )
pdf("G:/Bricks/张哥/revise/ROC_Transition_M1_M34.pdf")
p_aes
dev.off()
p<- ggroc(list(all=roc_all,Transition=roc_transition,M1M34=roc_M1_M34),legacy.axes=T, linewidth=0.9) 
p_aes<-p + scale_colour_manual(labels=c(paste0("Transition+M1+M4 ( AUC = ",round(roc_all$auc[[1]],2)," )"),paste0("Transition ( AUC = ",round(roc_transition$auc[[1]],2)," )"),paste0("M1+M34 ( AUC = ",round(roc_M1_M34$auc[[1]],2)," )")), values = c("#c26c36","#1e337f","#117f61")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    geom_abline(slope = 1,intercept = 0,lty=3, size=0.6) +
    theme_classic() +
    theme(
        plot.margin = margin(t = 40, r = 20, b = 20, l = 20),     # 上、右、下、左方空白
        axis.line = element_line(colour = "grey10", linewidth = 0.23), # 修饰坐标轴
        axis.ticks.y = element_line(linewidth = 0.3, colour = "grey10"), 
        axis.ticks.x = element_line(linewidth = 0.3, colour = "grey10") , 
        axis.ticks.length = unit(0.2, "cm"),
        axis.text.x = element_text(size = 16, colour = "grey10", angle = 0, vjust = 0.5, hjust = 0.5), 
        axis.text.y = element_text(size = 16, colour = "grey10", angle = 0, vjust = 0.5, hjust = 0.5),
        panel.grid.major.y = element_line(colour = "grey80"),     # 背景网格线
        legend.box.background = element_rect(colour = "grey50",linewidth = 2),  # 修饰legend
        legend.key.width = unit(35, "pt"),
        legend.text = element_text(size = 16),
        legend.title = element_blank(), 
        legend.position = c(0.7, 0.1), 
        legend.margin = margin(1,1,1,1), 
        plot.title = element_text(size = 20, vjust = 10),		# 图片标题
        axis.title.y = element_text(color = "grey10",size = 18), # 设置 y 轴标签颜色
        axis.title.x = element_text(color = "grey10",size = 18), # 设置 y 轴标签颜色
    ) + 
    labs( # 文本
        title = "ROC curves", 
        x = "1 - Specificity",
        y = "Sensitivity"
    )
pdf("G:/Bricks/张哥/revise/ROC_Transition_M1_M34.pdf")
p_aes
dev.off()

> auc(roc_transition)
Area under the curve: 0.916
> auc(roc_M1)
Area under the curve: 0.9023
> auc(roc_M34)
Area under the curve: 0.9297
> auc(roc_all)
Area under the curve: 0.9648
> auc(roc_M1_M34)
Area under the curve: 0.957



all<-coords(roc_all, "best", ret = c("threshold", "specificity", "sensitivity","tp","fp","fn","tn","youden"))
> all
          threshold specificity sensitivity tp fp fn tn youden
threshold 0.4085928      0.9375      0.9375 15  1  1 15  1.875
transition<-coords(roc_transition, "best", ret = c("threshold", "specificity", "sensitivity","tp","fp","fn","tn","youden"))
> transition
  threshold specificity sensitivity tp fp fn tn youden
1 0.3605602      0.8125      0.8750 14  3  2 13 1.6875
2 0.7367863      1.0000      0.6875 11  0  5 16 1.6875
M1<-coords(roc_M1, "best", ret = c("threshold", "specificity", "sensitivity","tp","fp","fn","tn","youden"))
> M1
          threshold specificity sensitivity tp fp fn tn youden
threshold 0.4902423       0.875      0.8125 13  2  3 14 1.6875
M34<-coords(roc_M34, "best", ret = c("threshold", "specificity", "sensitivity","tp","fp","fn","tn","youden"))
> M34
  threshold specificity sensitivity tp fp fn tn youden
1 0.4265889      0.9375      0.8750 14  1  2 15 1.8125
2 0.6473872      1.0000      0.8125 13  0  3 16 1.8125
M1_M34<-coords(roc_M1_M34, "best", ret = c("threshold", "specificity", "sensitivity","tp","fp","fn","tn","youden"))
> M1_M34
  threshold specificity sensitivity tp fp fn tn youden
1 0.5109222      0.9375      0.8750 14  1  2 15 1.8125
2 0.7446594      1.0000      0.8125 13  0  3 16 1.8125


model	Max Youden Index	Sensitivity	Specificity
all	1.875	0.9375	0.9375
M34	1.8125	0.8750	0.9375 / 1.0000
M1_M34	1.8125	0.8750 / 0.8125	0.9375 / 1.0000
M1	1.6875	0.8125	0.8750
transition	1.6875	0.8750 / 0.6875	0.8125 / 1.0000  



library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# 数据
df <- data.frame(
    Group = c("Transition+M1+M34", "Transition(cut1)", "Transition(cut2)", "M1", 
              "M34(cut1)", "M34(cut2)", "M1+M34(cut1)", "M1+M34(cut2)"),
    Sensitivity = c(0.9375, 0.875, 0.6875, 0.8125, 0.875, 0.8125, 0.875, 0.8125),
    Specificity = c(0.9375, 0.8125, 1, 0.875, 0.9375, 1, 0.9375, 1),
    Youden = c(0.875, 0.6875, 0.6875, 0.6875, 0.8125, 0.8125, 0.8125, 0.8125)
)

# 转为长格式
df_long <- df %>%
    pivot_longer(cols = c(Sensitivity, Specificity),
                 names_to = "Metric", values_to = "Value")

# Sensitivity & Specificity并排柱状图
p1 <- ggplot(df_long, aes(x = Group, y = Value, fill = Metric)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = round(Value, 3)), 
              position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("Sensitivity" = "skyblue", "Specificity" = "orange")) +
    labs(title = "Sensitivity and Specificity", y = "Value", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Youden指数图
p2 <- ggplot(df, aes(x = Group, y = Youden, fill = Youden)) +
    geom_col(width = 0.7) +
    scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
    geom_text(aes(label = round(Youden, 3)), vjust = -0.3, size = 3) +
    labs(title = "Youden Index", y = "Value", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 合并图像
p1 / p2






























#### 5s回归数据 ####
library(readxl)
library(tidyr)
library(nnet)
library(dplyr)
library(ggplot2)


#M1占比
M1_proportion <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/5s_M1_proportion.xlsx")
long_M1_proportion <- M1_proportion %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M1_proportion")

#M34占比
M34_proportion <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/5s_M34_proportion.xlsx")
long_M34_proportion <- M34_proportion %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M34_proportion")

#M1频率
M1_frequency <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/5s_M1_frequency.xlsx")
long_M1_frequency <- M1_frequency %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M1_frequency")

#M34频率
M34_frequency <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/5s_M34_frequency.xlsx")
long_M34_frequency <- M34_frequency %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M34_frequency")

merged_data_1 <- left_join(long_M1_proportion, long_M1_frequency, by = c("Time_bin", "Mouse_Group"))
merged_data_2 <- left_join(merged_data_1, long_M34_proportion, by = c("Time_bin", "Mouse_Group"))
final_merged_data <- left_join(merged_data_2, long_M34_frequency, by = c("Time_bin", "Mouse_Group"))
colnames(final_merged_data)<-c("Time_bin","Mouse_Group","M1_proportion","M1_frenqunecy","M34_proportion","M34_frequency")
final_merged_data <- final_merged_data %>%
  arrange(Mouse_Group)
final_merged_data$Group <- rep(c("Control", "PD", "PDVH"), each = 3840, length.out = nrow(final_merged_data))
final_merged_data$Sample_ID <- c(1:11520)
write.csv(final_merged_data,"G:/Bricks/张哥/revise/5s_10s_回归/5s_final_merged_data.csv")


#不要test 全都进去训练
#final_merged_data <-read.table("clipboard",header=T,row.names=1,sep="\t")#github-data.xlsx(sheet Fig 4h)
final_merged_data$Group <- as.factor(final_merged_data$Group)
set.seed(123)
train_indices <- sample(1:nrow(final_merged_data), size = 1 * nrow(final_merged_data))
train_data <- final_merged_data[train_indices, ]
test_data <- final_merged_data[-train_indices, ]
logistic_model_multinom <- multinom(Group ~ M1_proportion + M1_frenqunecy + M34_proportion + M34_frequency + Time_bin, 
                                    data = train_data)
summary(logistic_model_multinom)
Call:
multinom(formula = Group ~ M1_proportion + M1_frenqunecy + M34_proportion + 
    M34_frequency + Time_bin, data = train_data)

Coefficients:
     (Intercept) M1_proportion M1_frenqunecy M34_proportion M34_frequency     Time_bin
PD   -0.01814941      1.691452 -4.637189e-01       1.571503    -0.3035978 8.522843e-05
PDVH -0.34448695      2.159313  3.391241e-05       1.999740     0.2316537 6.879218e-04

Std. Errors:
     (Intercept) M1_proportion M1_frenqunecy M34_proportion M34_frequency     Time_bin
PD    0.04907218     0.2943369    0.08736125      0.2581534    0.06917517 0.0003332562
PDVH  0.05068990     0.2798476    0.07944697      0.2440138    0.06280974 0.0003368410

Residual Deviance: 24846.26 
AIC: 24870.26 


#用test_data来验证模型（不用）
pred <- predict(logistic_model_multinom, newdata = test_data)
true <- test_data$Y
rmse <- sqrt(mean((pred-true)^2))
mae <- mean(abs(pred-true))
> table(pred, true)
         true
pred      Control PD PDVH
  Control      53 29   25
  PD           50 58   35
  PDVH         28 41   65


library(caret)

conf_mat <- confusionMatrix(pred, test_data$Group)
print(conf_mat)


library(ggplot2)
library(dplyr)
library(reshape2)

cm <- table(Predicted = pred, Actual = test_data$Group)
cm_df <- as.data.frame(cm)

ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()









PD <- final_merged_data %>% filter(Group == "PD")
# 定义回归系数
intercept <- -0.01814941
beta1 <- 1.691452
beta2 <-  -4.637189e-01
beta3 <-  1.571503
beta4 <-  -0.3035978
beta5 <- -8.522843e-05
# 计算预测值
PD$predicted_Y <- intercept +
  beta1 * PD$M1_proportion +
  beta2 * PD$M1_frenqunecy +
  beta3 * PD$M34_proportion +
  beta4 * PD$M34_frequency +
  beta5 * PD$Time_bin 

PDVH <- final_merged_data %>% filter(Group == "PDVH")
# 定义回归系数
intercept <- -0.34448695
beta1 <- 2.159313
beta2 <-  3.391241e-05
beta3 <- 1.999740 
beta4 <- 0.2316537
beta5 <- 6.879218e-04
# 计算预测值
PDVH$predicted_Y <- intercept +
  beta1 * PDVH$M1_proportion +
  beta2 * PDVH$M1_frenqunecy +
  beta3 * PDVH$M34_proportion +
  beta4 * PDVH$M34_frequency +
  beta5 * PDVH$Time_bin

data <- rbind(PD,PDVH)
summary_data <- data %>%
  group_by(Group, Time_bin) %>%
  summarise(
    mean_y = mean(predicted_Y),
    sem_y = sd(predicted_Y) / sqrt(n()),
    .groups = "drop"
  )

summary_data_wide <- summary_data %>%
  pivot_wider(
    names_from = Group, 
    values_from = c(mean_y, sem_y),  
    names_glue = "{.value}_{Group}" 
  )

ggplot(summary_data_wide, aes(x = Time_bin)) +
  geom_col(aes(y = mean_y_PD, fill = "PD"), color = "#BC2321", size = 0.6, width = 1) + 
  geom_col(aes(y = mean_y_PDVH, fill = "PDVH"), color = "#4F9B72", size = 0.6, width = 1) + 
  geom_errorbar(aes(ymin = mean_y_PD - sem_y_PD, ymax = mean_y_PD + sem_y_PD), 
                width = 0.3, size = 0.8) +  
  geom_errorbar(aes(ymin = mean_y_PDVH - sem_y_PDVH, ymax = mean_y_PDVH + sem_y_PDVH), 
                width = 0.3, size = 0.8) +  
  scale_x_continuous(breaks = c(1,30,60,90,120,150,180,210,240), limits = c(0.5, 240.5)) +
  scale_fill_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) + 
  labs(
    title = "Predicted Y (Control as baseline)",
    x = "Time Bin",
    y = "Predicted Y",
    fill = "Group"
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12), 
    legend.key.size = unit(1, "cm"), 
    legend.box.margin = margin(10, 10, 10, 10),  
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), 
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 14), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(15, 15, 15, 15)
  )

#折线图
ggplot(summary_data_wide, aes(x = Time_bin)) +
  geom_line(aes(y = mean_y_PD, color = "PD"), size = 1.2) +
  geom_point(aes(y = mean_y_PD, color = "PD"), size = 2.5) +
  geom_line(aes(y = mean_y_PDVH, color = "PDVH"), size = 1.2) +
  geom_point(aes(y = mean_y_PDVH, color = "PDVH"), size = 2.5) +
  geom_errorbar(aes(ymin = mean_y_PD - sem_y_PD, ymax = mean_y_PD + sem_y_PD, color = "PD"),
                width = 2, size = 0.8) +
  geom_errorbar(aes(ymin = mean_y_PDVH - sem_y_PDVH, ymax = mean_y_PDVH + sem_y_PDVH, color = "PDVH"),
                width = 2, size = 0.8) +
  scale_x_continuous(breaks = c(1,30,60,90,120,150,180,210,240), limits = c(0.5, 240.5)) +
  scale_color_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) +
  labs(
    title = "Predicted Y (Control as baseline)",
    x = "Time Bin",
    y = "Predicted Y",
    color = "Group"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.box.margin = margin(10, 10, 10, 10),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(15, 15, 15, 15)
  )















#### 10s回归数据 ####
library(readxl)
library(tidyr)
library(nnet)
library(dplyr)
library(ggplot2)


#M1占比
M1_proportion <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/10s_M1_proportion.xlsx")
long_M1_proportion <- M1_proportion %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M1_proportion")

#M34占比
M34_proportion <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/10s_M34_proportion_new.xlsx")
long_M34_proportion <- M34_proportion %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M34_proportion")

#M1频率
M1_frequency <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/10s_M1_frequency.xlsx")
long_M1_frequency <- M1_frequency %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M1_frequency")

#M34频率
M34_frequency <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/10s_M34_frequency.xlsx")
long_M34_frequency <- M34_frequency %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M34_frequency")

merged_data_1 <- left_join(long_M1_proportion, long_M1_frequency, by = c("Time_bin", "Mouse_Group"))
merged_data_2 <- left_join(merged_data_1, long_M34_proportion, by = c("Time_bin", "Mouse_Group"))
final_merged_data <- left_join(merged_data_2, long_M34_frequency, by = c("Time_bin", "Mouse_Group"))
colnames(final_merged_data)<-c("Time_bin","Mouse_Group","M1_proportion","M1_frenqunecy","M34_proportion","M34_frequency")
final_merged_data <- final_merged_data %>%
  arrange(Mouse_Group)
final_merged_data$Group <- rep(c("Control", "PD", "PDVH"), each = 1920, length.out = nrow(final_merged_data))
final_merged_data$Sample_ID <- c(1:5760)
write.csv(final_merged_data,"G:/Bricks/张哥/revise/5s_10s_回归/10s_final_merged_data_new.csv")


#不要test 全都进去训练
#final_merged_data <-read.table("clipboard",header=T,row.names=1,sep="\t")#github-data.xlsx(sheet Fig 4h)
final_merged_data$Group <- as.factor(final_merged_data$Group)
set.seed(123)
train_indices <- sample(1:nrow(final_merged_data), size = 1 * nrow(final_merged_data))
train_data <- final_merged_data[train_indices, ]
test_data <- final_merged_data[-train_indices, ]
logistic_model_multinom <- multinom(Group ~ M1_proportion + M1_frenqunecy + M34_proportion + M34_frequency + Time_bin, 
                                    data = train_data)
summary(logistic_model_multinom)
Call:
multinom(formula = Group ~ M1_proportion + M1_frenqunecy + M34_proportion + 
    M34_frequency + Time_bin, data = train_data)

Coefficients:
     (Intercept) M1_proportion M1_frenqunecy M34_proportion M34_frequency      Time_bin
PD    0.03245096      2.952599  -0.462176945    0.008867679   -0.03773899 -3.600356e-05
PDVH -0.46053034      3.244718   0.006871053   -0.214643393    0.58522576  1.824512e-03

Std. Errors:
     (Intercept) M1_proportion M1_frenqunecy M34_proportion M34_frequency     Time_bin
PD    0.07417886     0.5665331    0.08982043     0.05418421    0.05557730 0.0009454058
PDVH  0.07754348     0.5462700    0.08244903     0.05603063    0.05116195 0.0009613576

Residual Deviance: 12301.81 
AIC: 12325.81 


#用test_data来验证模型（不用）
pred <- predict(logistic_model_multinom, newdata = test_data)
true <- test_data$Y
rmse <- sqrt(mean((pred-true)^2))
mae <- mean(abs(pred-true))
> table(pred, true)
         true
pred      Control PD PDVH
  Control      53 29   25
  PD           50 58   35
  PDVH         28 41   65


library(caret)

conf_mat <- confusionMatrix(pred, test_data$Group)
print(conf_mat)


library(ggplot2)
library(dplyr)
library(reshape2)

cm <- table(Predicted = pred, Actual = test_data$Group)
cm_df <- as.data.frame(cm)

ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()





Call:
multinom(formula = Group ~ M1_proportion + M1_frenqunecy + M34_proportion + 
    M34_frequency + Time_bin, data = train_data)

Coefficients:
     (Intercept) M1_proportion M1_frenqunecy M34_proportion M34_frequency      Time_bin
PD    0.03245096      2.952599  -0.462176945    0.008867679   -0.03773899 -3.600356e-05
PDVH -0.46053034      3.244718   0.006871053   -0.214643393    0.58522576  1.824512e-03

Std. Errors:
     (Intercept) M1_proportion M1_frenqunecy M34_proportion M34_frequency     Time_bin
PD    0.07417886     0.5665331    0.08982043     0.05418421    0.05557730 0.0009454058
PDVH  0.07754348     0.5462700    0.08244903     0.05603063    0.05116195 0.0009613576

Residual Deviance: 12301.81 
AIC: 12325.81 



PD <- final_merged_data %>% filter(Group == "PD")
# 定义回归系数
intercept <- 0.03245096
beta1 <- 2.952599
beta2 <-  -0.462176945
beta3 <-  0.008867679
beta4 <-  -0.03773899
beta5 <- -3.600356e-05
# 计算预测值
PD$predicted_Y <- intercept +
  beta1 * PD$M1_proportion +
  beta2 * PD$M1_frenqunecy +
  beta3 * PD$M34_proportion +
  beta4 * PD$M34_frequency +
  beta5 * PD$Time_bin 

PDVH <- final_merged_data %>% filter(Group == "PDVH")
# 定义回归系数
intercept <- -0.46053034
beta1 <- 3.244718
beta2 <-  0.006871053
beta3 <- -0.214643393
beta4 <- 0.58522576
beta5 <- 1.824512e-03
# 计算预测值
PDVH$predicted_Y <- intercept +
  beta1 * PDVH$M1_proportion +
  beta2 * PDVH$M1_frenqunecy +
  beta3 * PDVH$M34_proportion +
  beta4 * PDVH$M34_frequency +
  beta5 * PDVH$Time_bin

data <- rbind(PD,PDVH)
summary_data <- data %>%
  group_by(Group, Time_bin) %>%
  summarise(
    mean_y = mean(predicted_Y),
    sem_y = sd(predicted_Y) / sqrt(n()),
    .groups = "drop"
  )

summary_data_wide <- summary_data %>%
  pivot_wider(
    names_from = Group, 
    values_from = c(mean_y, sem_y),  
    names_glue = "{.value}_{Group}" 
  )

ggplot(summary_data_wide, aes(x = Time_bin)) +
  geom_col(aes(y = mean_y_PD, fill = "PD"), color = "#BC2321", size = 0.6, width = 1) + 
  geom_col(aes(y = mean_y_PDVH, fill = "PDVH"), color = "#4F9B72", size = 0.6, width = 1) + 
  geom_errorbar(aes(ymin = mean_y_PD - sem_y_PD, ymax = mean_y_PD + sem_y_PD), 
                width = 0.3, size = 0.8) +  
  geom_errorbar(aes(ymin = mean_y_PDVH - sem_y_PDVH, ymax = mean_y_PDVH + sem_y_PDVH), 
                width = 0.3, size = 0.8) +  
  scale_x_continuous(breaks = c(1,15,30,45,60,75,90,105,120), limits = c(0.5, 120.5)) +
  scale_fill_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) + 
  labs(
    title = "Predicted Y (Control as baseline)",
    x = "Time Bin",
    y = "Predicted Y",
    fill = "Group"
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12), 
    legend.key.size = unit(1, "cm"), 
    legend.box.margin = margin(10, 10, 10, 10),  
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), 
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 14), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(15, 15, 15, 15)
  )

#折线图
ggplot(summary_data_wide, aes(x = Time_bin)) +
  geom_line(aes(y = mean_y_PD, color = "PD"), size = 1.2) +
  geom_point(aes(y = mean_y_PD, color = "PD"), size = 2.5) +
  geom_line(aes(y = mean_y_PDVH, color = "PDVH"), size = 1.2) +
  geom_point(aes(y = mean_y_PDVH, color = "PDVH"), size = 2.5) +
  geom_errorbar(aes(ymin = mean_y_PD - sem_y_PD, ymax = mean_y_PD + sem_y_PD, color = "PD"),
                width = 2, size = 0.8) +
  geom_errorbar(aes(ymin = mean_y_PDVH - sem_y_PDVH, ymax = mean_y_PDVH + sem_y_PDVH, color = "PDVH"),
                width = 2, size = 0.8) +
  scale_x_continuous(breaks = c(1,15,30,45,60,75,90,105,120), limits = c(0.5, 120.5)) +
  scale_color_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) +
  labs(
    title = "Predicted Y (Control as baseline)",
    x = "Time Bin",
    y = "Predicted Y",
    color = "Group"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.box.margin = margin(10, 10, 10, 10),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(15, 15, 15, 15)
  )



#10s 二分类逻辑回归#
final_merged_data <-read.table("clipboard",header=T,row.names=1,sep="\t")
final_merged_data$Group <- as.factor(final_merged_data$Group)
set.seed(123)

library(nnet)
library(dplyr)

# 筛选Ctrl和PDVH两类
binary_data <- final_merged_data %>%
    filter(Group %in% c("Control", "PDVH"))

# 把Group重新设置成因子，且只有两个水平
binary_data$Group <- factor(binary_data$Group, levels = c("Control", "PDVH"))

train_indices <- sample(1:nrow(binary_data), size = 1 * nrow(binary_data))
train_data <- binary_data[train_indices, ]
test_data <- binary_data[-train_indices, ]

logistic_model_bin_glm <- glm(Group ~ M1_proportion + M1_frenqunecy + M34_proportion + M34_frequency + Time_bin, 
                              data = train_data, family = binomial)
summary(logistic_model_bin_glm)
> summary(logistic_model_bin_glm)

Call:
glm(formula = Group ~ M1_proportion + M1_frenqunecy + M34_proportion + 
    M34_frequency + Time_bin, family = binomial, data = train_data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0277  -1.0705  -0.3909   1.1660   1.8917  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)    -0.4609681  0.0775575  -5.944 2.79e-09 ***
M1_proportion   3.1065172  0.5382920   5.771 7.88e-09 ***
M1_frenqunecy   0.0400004  0.0824972   0.485   0.6278    
M34_proportion -0.3997185  0.0636129  -6.284 3.31e-10 ***
M34_frequency   0.7172356  0.0587230  12.214  < 2e-16 ***
Time_bin        0.0018520  0.0009747   1.900   0.0574 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 5323.4  on 3839  degrees of freedom
Residual deviance: 5056.1  on 3834  degrees of freedom
AIC: 5068.1

Number of Fisher Scoring iterations: 4



# 线性预测值（log-odds）
log_odds <- predict(logistic_model_bin_glm, newdata = train_data, type = "link")

# 预测概率
probabilities <- predict(logistic_model_bin_glm, newdata = train_data, type = "response")

library(dplyr)
library(tidyr)
library(ggplot2)

desired_order <- c(paste0("PDVH", 1:16), paste0("Control", 1:16))

# 给 prob_df 加上 factor 类型的 Mouse_Group，顺序是你指定的
prob_df <- train_data %>%
  mutate(Probability = probabilities,
         Mouse_Group = factor(Mouse_Group, levels = rev(desired_order))) %>%
  select(Mouse_Group, Time_bin, Probability)

prob_wide <- prob_df %>%
  pivot_wider(names_from = Time_bin, values_from = Probability)

head(prob_wide)

prob_long <- prob_df

# 画热图
#指定几个明显区间的颜色
ggplot(prob_long, aes(x = factor(Time_bin), y = Mouse_Group, fill = Probability)) +
    geom_tile() +
    scale_fill_gradientn(
        colors = c("darkblue", "white", "firebrick"),
        values = rescale(c(0, 0.5, 1)),
        limits = c(0, 1)
    ) +
    labs(x = "Time Bin", y = "Mouse Group", fill = "Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
	
	
	
	
	
	
	
#5s 二分类逻辑回归#
final_merged_data <-read.table("clipboard",header=T,row.names=1,sep="\t")
final_merged_data$Group <- as.factor(final_merged_data$Group)
set.seed(123)

library(nnet)
library(dplyr)

# 筛选Ctrl和PDVH两类
binary_data <- final_merged_data %>%
    filter(Group %in% c("Control", "PDVH"))

# 把Group重新设置成因子，且只有两个水平
binary_data$Group <- factor(binary_data$Group, levels = c("Control", "PDVH"))

train_indices <- sample(1:nrow(binary_data), size = 1 * nrow(binary_data))
train_data <- binary_data[train_indices, ]
test_data <- binary_data[-train_indices, ]

logistic_model_bin_glm <- glm(Group ~ M1_proportion + M1_frenqunecy + M34_proportion + M34_frequency, 
                              data = train_data, family = binomial)
summary(logistic_model_bin_glm)

# 线性预测值（log-odds）
log_odds <- predict(logistic_model_bin_glm, newdata = train_data, type = "link")

# 预测概率
probabilities <- predict(logistic_model_bin_glm, newdata = train_data, type = "response")

library(dplyr)
library(tidyr)
library(ggplot2)

desired_order <- c(paste0("PDVH", 1:16), paste0("Control", 1:16))

# 给 prob_df 加上 factor 类型的 Mouse_Group，顺序是你指定的
prob_df <- train_data %>%
  mutate(Probability = probabilities,
         Mouse_Group = factor(Mouse_Group, levels = rev(desired_order))) %>%
  select(Mouse_Group, Time_bin, Probability)

prob_wide <- prob_df %>%
  pivot_wider(names_from = Time_bin, values_from = Probability)

head(prob_wide)

prob_long <- prob_df

# 画热图
#指定几个明显区间的颜色
ggplot(prob_long, aes(x = factor(Time_bin), y = Mouse_Group, fill = Probability)) +
    geom_tile() +
    scale_fill_gradientn(
        colors = c("darkblue", "white", "firebrick"),
        values = rescale(c(0, 0.5, 1)),
        limits = c(0, 1)
    ) +
    labs(x = "Time Bin", y = "Mouse Group", fill = "Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
	



#5s 二分类逻辑回归  分两个函数写#
final_merged_data <-read.table("clipboard",header=T,row.names=1,sep="\t")
final_merged_data$Group <- as.factor(final_merged_data$Group)
set.seed(123)

library(nnet)
library(dplyr)

# 筛选Ctrl和PDVH两类
binary_data <- final_merged_data %>%
    filter(Group %in% c("Control", "PDVH"))

# 把Group重新设置成因子，且只有两个水平
binary_data$Group <- factor(binary_data$Group, levels = c("Control", "PDVH"))

train_indices <- sample(1:nrow(binary_data), size = 1 * nrow(binary_data))
train_data <- binary_data[train_indices, ]
test_data <- binary_data[-train_indices, ]

logistic_model_bin_glm_M1 <- glm(Group ~ M1_proportion + M1_frenqunecy, 
                              data = train_data, family = binomial)
logistic_model_bin_glm_M34 <- glm(Group ~ M34_proportion + M34_frequency, 
                              data = train_data, family = binomial)



# 预测概率
probabilities_M1 <- predict(logistic_model_bin_glm_M1, newdata = train_data, type = "response")
probabilities_M34 <- predict(logistic_model_bin_glm_M34, newdata = train_data, type = "response")


train_data$prob_M1 <- probabilities_M1
train_data$prob_M34 <- probabilities_M34
#train_data$Selected_Model <- ifelse(train_data$prob_M1 > train_data$prob_M34, "M1", "M34")


# 取每一行中较大的概率值作为最终概率
train_data$Probability <- pmax(train_data$prob_M1, train_data$prob_M34)


library(dplyr)
library(tidyr)
library(ggplot2)

desired_order <- c(paste0("PDVH", 1:16), paste0("Control", 1:16))

prob_df <- train_data %>%
  mutate(Mouse_Group = factor(Mouse_Group, levels = rev(desired_order))) %>%
  select(Mouse_Group, Time_bin, Probability)

prob_wide <- prob_df %>%
  pivot_wider(names_from = Time_bin, values_from = Probability)

head(prob_wide)

prob_long <- prob_df

# 画热图
#指定几个明显区间的颜色
ggplot(prob_long, aes(x = factor(Time_bin), y = Mouse_Group, fill = Probability)) +
    geom_tile() +
    scale_fill_gradientn(
        colors = c("darkblue", "white", "firebrick"),
        values = rescale(c(0, 0.5, 1)),
        limits = c(0, 1)
    ) +
    labs(x = "Time Bin", y = "Mouse Group", fill = "Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


#热图 区间定量 就两个颜色
library(dplyr)
library(ggplot2)
prob_long <- prob_long %>%
  mutate(Prob_Class = ifelse(Probability > 0.5, "High", "Low"))

# 绘图
ggplot(prob_long, aes(x = factor(Time_bin), y = Mouse_Group, fill = Prob_Class)) +
  geom_tile(color = "gray70", size = 0.4) +  # 添加灰色边框
  scale_fill_manual(
    values = c("High" = "firebrick", "Low" = "gray80"),
    name = "Probability"
  ) +
  labs(x = "Time Bin", y = "Mouse Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

	
#### 10s回归数据 ####
library(readxl)
library(tidyr)
library(nnet)
library(dplyr)
library(ggplot2)


#M1占比
M1_proportion <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/10s_M1_proportion.xlsx")
long_M1_proportion <- M1_proportion %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M1_proportion")

#M34占比
M34_proportion <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/10s_M34_proportion.xlsx")
long_M34_proportion <- M34_proportion %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M34_proportion")

#M1频率
M1_frequency <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/10s_M1_frequency.xlsx")
long_M1_frequency <- M1_frequency %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M1_frequency")

#M34频率
M34_frequency <- read_excel("G:/Bricks/张哥/revise/5s_10s_回归/10s_M34_frequency.xlsx")
long_M34_frequency <- M34_frequency %>%
  pivot_longer(cols = starts_with(c("Control","PD","PDVH")),
               names_to = "Mouse_Group", 
               values_to = "M34_frequency")

merged_data_1 <- left_join(long_M1_proportion, long_M1_frequency, by = c("Time_bin", "Mouse_Group"))
merged_data_2 <- left_join(merged_data_1, long_M34_proportion, by = c("Time_bin", "Mouse_Group"))
final_merged_data <- left_join(merged_data_2, long_M34_frequency, by = c("Time_bin", "Mouse_Group"))
colnames(final_merged_data)<-c("Time_bin","Mouse_Group","M1_proportion","M1_frenqunecy","M34_proportion","M34_frequency")
final_merged_data <- final_merged_data %>%
  arrange(Mouse_Group)
final_merged_data$Group <- rep(c("Control", "PD", "PDVH"), each = 1920, length.out = nrow(final_merged_data))
final_merged_data$Sample_ID <- c(1:5760)
write.csv(final_merged_data,"G:/Bricks/张哥/revise/5s_10s_回归/10s_final_merged_data.csv")


#0.7train 0.3train
#final_merged_data <-read.table("clipboard",header=T,row.names=1,sep="\t")
final_merged_data$Group <- as.factor(final_merged_data$Group)
library(nnet)

set.seed(123)
train_indices <- sample(1:nrow(final_merged_data), size = 0.7 * nrow(final_merged_data))
train_data <- final_merged_data[train_indices, ]
test_data <- final_merged_data[-train_indices, ]
  
  # 拟合多项逻辑回归模型
  logistic_model_multinom <- multinom(
    Group ~ M1_proportion + M1_frenqunecy + M34_proportion + M34_frequency + Time_bin, 
    data = train_data,
    trace = FALSE
  )
  
> dim(test_data)
[1] 1729    8
> dim(train_data)
[1] 4031    8
> dim(final_merged_data)
[1] 5760    8

summary(logistic_model_multinom)
Call:
multinom(formula = Group ~ M1_proportion + M1_frenqunecy + M34_proportion + 
    M34_frequency + Time_bin, data = train_data, trace = FALSE)

Coefficients:
     (Intercept) M1_proportion M1_frenqunecy M34_proportion M34_frequency
PD   -0.01499995      3.145400   -0.45317206     0.01948966   -0.01743155
PDVH -0.55896042      3.547999   -0.03029987    -0.19128969    0.59887486
         Time_bin
PD   9.795081e-05
PDVH 2.842813e-03

Std. Errors:
     (Intercept) M1_proportion M1_frenqunecy M34_proportion M34_frequency
PD    0.08791502     0.6882969    0.10675511     0.06519108    0.06706068
PDVH  0.09257184     0.6641636    0.09847574     0.06743364    0.06217749
        Time_bin
PD   0.001126409
PDVH 0.001147641

Residual Deviance: 8609.823 
AIC: 8633.823 




PD <- test_data %>% filter(Group == "PD")
# 定义回归系数
intercept <- -0.01499995
beta1 <- 3.145400
beta2 <-  -0.45317206
beta3 <-  0.01948966
beta4 <-  -0.01743155
beta5 <- 9.795081e-05
# 计算预测值
PD$predicted_Y <- intercept +
  beta1 * PD$M1_proportion +
  beta2 * PD$M1_frenqunecy +
  beta3 * PD$M34_proportion +
  beta4 * PD$M34_frequency +
  beta5 * PD$Time_bin 

#PD$predicted_prob <- 1 / (1 + exp(-PD$predicted_Y))

PDVH <- test_data %>% filter(Group == "PDVH")
# 定义回归系数
intercept <- -0.55896042
beta1 <- 3.547999
beta2 <-  -0.03029987
beta3 <- -0.19128969
beta4 <- 0.59887486
beta5 <- 2.842813e-03
# 计算预测值
PDVH$predicted_Y <- intercept +
  beta1 * PDVH$M1_proportion +
  beta2 * PDVH$M1_frenqunecy +
  beta3 * PDVH$M34_proportion +
  beta4 * PDVH$M34_frequency +
  beta5 * PDVH$Time_bin

data <- rbind(PD,PDVH)
summary_data <- data %>%
  group_by(Group, Time_bin) %>%
  summarise(
    mean_y = mean(predicted_Y),
    sem_y = sd(predicted_Y) / sqrt(n()),
    .groups = "drop"
  )

summary_data_wide <- summary_data %>%
  pivot_wider(
    names_from = Group, 
    values_from = c(mean_y, sem_y),  
    names_glue = "{.value}_{Group}" 
  )

ggplot(summary_data_wide, aes(x = Time_bin)) +
  geom_col(aes(y = mean_y_PD, fill = "PD"), color = "#BC2321", size = 0.6, width = 1) + 
  geom_col(aes(y = mean_y_PDVH, fill = "PDVH"), color = "#4F9B72", size = 0.6, width = 1) + 
  geom_errorbar(aes(ymin = mean_y_PD - sem_y_PD, ymax = mean_y_PD + sem_y_PD), 
                width = 0.3, size = 0.8) +  
  geom_errorbar(aes(ymin = mean_y_PDVH - sem_y_PDVH, ymax = mean_y_PDVH + sem_y_PDVH), 
                width = 0.3, size = 0.8) +  
  scale_x_continuous(breaks = c(1,15,30,45,60,75,90,105,120), limits = c(0.5, 120.5)) +
  scale_fill_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) + 
  labs(
    title = "Predicted Y (Control as baseline)",
    x = "Time Bin",
    y = "Predicted Y",
    fill = "Group"
  ) +
  theme_minimal(base_size = 16) +  
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12), 
    legend.key.size = unit(1, "cm"), 
    legend.box.margin = margin(10, 10, 10, 10),  
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), 
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 14), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(15, 15, 15, 15)
  )

#折线图
ggplot(summary_data_wide, aes(x = Time_bin)) +
  geom_line(aes(y = mean_y_PD, color = "PD"), size = 1.2) +
  geom_point(aes(y = mean_y_PD, color = "PD"), size = 2.5) +
  geom_line(aes(y = mean_y_PDVH, color = "PDVH"), size = 1.2) +
  geom_point(aes(y = mean_y_PDVH, color = "PDVH"), size = 2.5) +
  geom_errorbar(aes(ymin = mean_y_PD - sem_y_PD, ymax = mean_y_PD + sem_y_PD, color = "PD"),
                width = 2, size = 0.8) +
  geom_errorbar(aes(ymin = mean_y_PDVH - sem_y_PDVH, ymax = mean_y_PDVH + sem_y_PDVH, color = "PDVH"),
                width = 2, size = 0.8) +
  scale_x_continuous(breaks = c(1,15,30,45,60,75,90,105,120), limits = c(0.5, 120.5)) +
  scale_color_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) +
  labs(
    title = "Predicted Y (Control as baseline)",
    x = "Time Bin",
    y = "Predicted Y",
    color = "Group"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.box.margin = margin(10, 10, 10, 10),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(15, 15, 15, 15)
  )
  
 #折线图误差色带
 ggplot(summary_data_wide, aes(x = Time_bin)) +
    # PD组误差带
    geom_ribbon(aes(ymin = mean_y_PD - sem_y_PD, ymax = mean_y_PD + sem_y_PD, fill = "PD"), alpha = 0.2) +
    # PDVH组误差带
    geom_ribbon(aes(ymin = mean_y_PDVH - sem_y_PDVH, ymax = mean_y_PDVH + sem_y_PDVH, fill = "PDVH"), alpha = 0.2) +
    
    # 画线和点
    geom_line(aes(y = mean_y_PD, color = "PD"), size = 1.2) +
    geom_point(aes(y = mean_y_PD, color = "PD"), size = 2.5) +
    geom_line(aes(y = mean_y_PDVH, color = "PDVH"), size = 1.2) +
    geom_point(aes(y = mean_y_PDVH, color = "PDVH"), size = 2.5) +
    
    # 坐标轴和图例
    scale_x_continuous(breaks = c(1,15,30,45,60,75,90,105,120), limits = c(0.5, 120.5)) +
    scale_color_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) +
    scale_fill_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72"), guide = "none") +  # 误差带颜色和线条颜色对应，且不额外显示图例
    
    labs(
        title = "Predicted Y (Control as baseline)",
        x = "Time Bin",
        y = "Predicted Y",
        color = "Group"
    ) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        legend.box.margin = margin(10, 10, 10, 10),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
        plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
        panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(15, 15, 15, 15)
    )




	
#曲线图误差色带
ggplot(summary_data_wide, aes(x = Time_bin)) +
    geom_smooth(aes(y = mean_y_PD, color = "PD", fill = "PD"), method = "loess", span = 0.2, size = 1.2, alpha = 0.2, se = TRUE) +
    geom_smooth(aes(y = mean_y_PDVH, color = "PDVH", fill = "PDVH"), method = "loess", span = 0.2, size = 1.2, alpha = 0.2, se = TRUE) +
    scale_x_continuous(breaks = c(1,15,30,45,60,75,90,105,120), limits = c(0.5, 120.5)) +
	#scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) +
    scale_fill_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72"), guide = "none") +
    
    labs(
        title = "Predicted Y with Confidence Bands",
        x = "Time Bin",
        y = "Predicted Y",
        color = "Group"
    ) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        legend.box.margin = margin(10, 10, 10, 10),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
        panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
        panel.grid.minor = element_blank()
    )
	



library(dplyr)
library(ggplot2)

# 你的数据准备（省略）...

# 计算每个Time_bin的t检验p值
p_values <- data %>%
    group_by(Time_bin) %>%
    summarise(
        p_val = tryCatch(
            t.test(predicted_Y ~ Group)$p.value,
            error = function(e) NA_real_
        ),
        .groups = "drop"
    )

# 找p<0.05的显著时间点，合并连续时间点为区间
sig_points <- p_values %>%
    filter(p_val < 0.05) %>%
    arrange(Time_bin) %>%
    mutate(
        diff = Time_bin - lag(Time_bin, default = first(Time_bin) - 1),
        group = cumsum(diff != 1)
    )

sig_intervals <- sig_points %>%
    group_by(group) %>%
    summarise(
        start = min(Time_bin) - 0.5,
        end = max(Time_bin) + 0.5,
        .groups = "drop"
    )

# 画图，添加显著时间窗背景
ggplot(summary_data_wide, aes(x = Time_bin)) +
    geom_smooth(aes(y = mean_y_PD, color = "PD", fill = "PD"),
                method = "loess", span = 0.2, size = 1.2, alpha = 0.2, se = TRUE) +
    geom_smooth(aes(y = mean_y_PDVH, color = "PDVH", fill = "PDVH"),
                method = "loess", span = 0.2, size = 1.2, alpha = 0.2, se = TRUE) +
    
    geom_rect(data = sig_intervals, inherit.aes = FALSE,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              fill = "yellow", alpha = 0.15) +
    
    scale_x_continuous(breaks = c(1,15,30,45,60,75,90,105,120), limits = c(0.5, 120.5)) +
    scale_color_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72")) +
    scale_fill_manual(values = c("PD" = "#BC2321", "PDVH" = "#4F9B72"), guide = "none") +
    
    labs(
        title = "Smoothed Predicted Y with Significant Time Windows Highlighted",
        x = "Time Bin",
        y = "Predicted Y",
        color = "Group"
    ) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        legend.box.margin = margin(10, 10, 10, 10),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
        panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),
        panel.grid.minor = element_blank()
    )	
	
	



#10s 二分类逻辑回归  分两个函数写#
final_merged_data <-read.table("clipboard",header=T,row.names=1,sep="\t")
final_merged_data$Group <- as.factor(final_merged_data$Group)
set.seed(123)

library(nnet)
library(dplyr)

# 筛选Ctrl和PDVH两类
binary_data <- final_merged_data %>%
    filter(Group %in% c("Control", "PDVH"))

# 把Group重新设置成因子，且只有两个水平
binary_data$Group <- factor(binary_data$Group, levels = c("Control", "PDVH"))

train_indices <- sample(1:nrow(binary_data), size = 1 * nrow(binary_data))
train_data <- binary_data[train_indices, ]
test_data <- binary_data[-train_indices, ]

logistic_model_bin_glm_M1 <- glm(Group ~ M1_proportion + M1_frenqunecy, 
                              data = train_data, family = binomial)
logistic_model_bin_glm_M34 <- glm(Group ~ M34_proportion + M34_frequency, 
                              data = train_data, family = binomial)



# 预测概率
probabilities_M1 <- predict(logistic_model_bin_glm_M1, newdata = train_data, type = "response")
probabilities_M34 <- predict(logistic_model_bin_glm_M34, newdata = train_data, type = "response")


train_data$prob_M1 <- probabilities_M1
train_data$prob_M34 <- probabilities_M34
#train_data$Selected_Model <- ifelse(train_data$prob_M1 > train_data$prob_M34, "M1", "M34")


# 取每一行中较大的概率值作为最终概率
train_data$Probability <- pmax(train_data$prob_M1, train_data$prob_M34)


library(dplyr)
library(tidyr)
library(ggplot2)

desired_order <- c(paste0("PDVH", 1:16), paste0("Control", 1:16))

prob_df <- train_data %>%
  mutate(Mouse_Group = factor(Mouse_Group, levels = rev(desired_order))) %>%
  select(Mouse_Group, Time_bin, Probability)

prob_wide <- prob_df %>%
  pivot_wider(names_from = Time_bin, values_from = Probability)

head(prob_wide)

prob_long <- prob_df

# 画热图
#指定几个明显区间的颜色
ggplot(prob_long, aes(x = factor(Time_bin), y = Mouse_Group, fill = Probability)) +
    geom_tile() +
    scale_fill_gradientn(
        colors = c("darkblue", "white", "firebrick"),
        values = rescale(c(0, 0.5, 1)),
        limits = c(0, 1)
    ) +
    labs(x = "Time Bin", y = "Mouse Group", fill = "Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
	

#热图 区间定量 就两个颜色
library(dplyr)
library(ggplot2)
prob_long <- prob_long %>%
  mutate(Prob_Class = ifelse(Probability > 0.8, "High", "Low"))

# 绘图
ggplot(prob_long, aes(x = factor(Time_bin), y = Mouse_Group, fill = Prob_Class)) +
  #geom_tile(color = "gray70", size = 0.4) +  # 添加灰色边框
  geom_tile() +
  scale_fill_manual(
    values = c("High" = "firebrick", "Low" = "gray80"),
    name = "Probability"
  ) +
  labs(x = "Time Bin", y = "Mouse Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


ggplot() +
  # 先画 Low（不带边框）
  geom_tile(
    data = subset(prob_long, Prob_Class == "Low"),
    aes(x = factor(Time_bin), y = Mouse_Group, fill = Prob_Class)
  ) +
  # 再画 High（带灰色边框）
  geom_tile(
    data = subset(prob_long, Prob_Class == "High"),
    aes(x = factor(Time_bin), y = Mouse_Group, fill = Prob_Class),
    color = "gray40", size = 0.4
  ) +
  scale_fill_manual(
    values = c("High" = "firebrick", "Low" = "gray80"),
    name = "Probability"
  ) +
  labs(x = "Time Bin", y = "Mouse Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )