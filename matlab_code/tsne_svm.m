%% ============================================
% PCA + Linear SVM 可视化示例
% 严格红蓝分区 + 黑色决策线 + 原来的红蓝颜色
% ============================================

clear; clc; close all;

%% 1. 读取 Excel 数据
X = readmatrix('tsne.xlsx', 'Range', 'B1:U4');  % 5行 × 18列
X = X';  % 转置 -> 18样本 × 4维

% 标签：前8列模型=1，后10列对照=0
y = [ones(1,10), zeros(1,10)];
y = categorical(y');

%% 2. PCA 降维到 2D
[~, Y_pca, ~, ~, explained] = pca(X);
Y_pca = Y_pca(:,1:2);  % 前两个主成分
disp(['前两主成分解释方差比例: ', num2str(sum(explained(1:2))),'%']);

%% 3. 训练线性 SVM
linSVM = fitcsvm(Y_pca, y, 'KernelFunction','linear', 'Standardize',true);

%% 4. 创建网格
pad_ratio = 0.1;
x_range = max(Y_pca(:,1)) - min(Y_pca(:,1));
y_range = max(Y_pca(:,2)) - min(Y_pca(:,2));

x_min = min(Y_pca(:,1)) - pad_ratio*x_range;
x_max = max(Y_pca(:,1)) + pad_ratio*x_range;
y_min = min(Y_pca(:,2)) - pad_ratio*y_range;
y_max = max(Y_pca(:,2)) + pad_ratio*y_range;

nx = 500; ny = 500;
[xGrid, yGrid] = meshgrid(linspace(x_min, x_max, nx), linspace(y_min, y_max, ny));

%% 5. SVM预测类别 (严格0/1)
[labelGrid, ~] = predict(linSVM, [xGrid(:), yGrid(:)]);
labelGrid = reshape(double(labelGrid)-1, size(xGrid));  % 0/1

%% 6. 绘图
figure('Color','w'); hold on;

% 背景颜色保持原来的渐变风格
imagesc([x_min x_max], [y_min y_max], labelGrid);
set(gca,'YDir','normal');

% 原来的红蓝颜色
colormap([0.6 0.8 1; 1 0.7 0.7]);  % 蓝色类0 / 红色类1

class0_idx = y == categorical(0);
class1_idx = y == categorical(1);

scatter(Y_pca(class0_idx,1), Y_pca(class0_idx,2), 60, [0 0.5 1], 'filled', 'MarkerEdgeColor','k'); 
scatter(Y_pca(class1_idx,1), Y_pca(class1_idx,2), 60, [1 0 0], 'filled', 'MarkerEdgeColor','k'); 


% 决策边界黑色实线
contour(xGrid, yGrid, labelGrid, [0.5 0.5], 'LineStyle','-', 'LineWidth',2.5, 'LineColor','k');

% 美化
axis([x_min x_max y_min y_max]);
axis equal tight;
xlabel('PCA 1'); ylabel('PCA 2');
title('PCA with Linear SVM: Red-Blue Regions + Decision Line');
set(gca,'Layer','top');
hold off;
