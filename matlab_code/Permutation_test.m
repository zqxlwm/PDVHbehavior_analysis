
%% ============== 参数设置 ==============
M34 = 34;                   % 目标行为
n_perm = 1000;              % 置换次数
alpha = 0.05;               % 显著性阈值
filename = 'EthgramPDVH.xlsx';  % 原始行为矩阵 27000 x 16
n_states = 40;              % 行为总数

%% ============== Step 1: 导入 & 压缩行为序列 ==============
raw_data = xlsread(filename);   % 27000 x 16
[n_frames, n_mice] = size(raw_data);

compressed_data = cell(1, n_mice);
for m = 1:n_mice
    seq = raw_data(:, m);
    compressed_data{m} = seq([true; diff(seq(:)) ~= 0]); % 去除连续重复
end
disp('压缩完成，连续帧已去除');

%% ============== Step 2: 计算真实转移概率 A -> M34 ==============
real_probs = zeros(n_states, n_mice); % 行=行为, 列=小鼠
for m = 1:n_mice
    seq = compressed_data{m};
    for A = 1:n_states
        idx = find(seq(1:end-1) == A);
        if ~isempty(idx)
            real_probs(A, m) = sum(seq(idx+1) == M34) / length(idx);
        else
            real_probs(A, m) = 0;
        end
    end
end
disp('真实转移概率计算完成');

perm_probs = zeros(n_states, n_mice, n_perm);
for m = 1:n_mice
    seq = compressed_data{m};
    L = length(seq);
    for p = 1:n_perm
        perm_seq = seq(randperm(L)); % 打乱顺序，破坏时序依赖
        for A = 1:n_states
            idx = find(perm_seq(1:end-1) == A);
            if ~isempty(idx)
                perm_probs(A, m, p) = sum(perm_seq(idx+1) == M34) / length(idx);
            else
                perm_probs(A, m, p) = 0;
            end
        end
    end
end
disp('置换检验完成');

% 计算每个行为的 p 值
p_values = zeros(n_states,1);
for A = 1:n_states
    all_perm = [];  % 存放该行为的所有置换值 (16只鼠 × 1000置换)
    for m = 1:n_mice
        all_perm = [all_perm; squeeze(perm_probs(A,m,:))];
    end
    real_mean = mean(real_probs(A,:));
    % 单尾检验：真实值比置换均值高的比例
    p_values(A) = mean(all_perm >= real_mean);
    if p_values(A) == 0
        p_values(A) = 1/length(all_perm);  % 避免为0
    end
end

% ==== FDR (Benjamini–Hochberg) 校正 ====
[~, sort_idx] = sort(p_values);
m_tests = length(p_values);
fdr_values = zeros(size(p_values));
for k = 1:m_tests
    fdr_values(sort_idx(k)) = p_values(sort_idx(k)) * m_tests / k;
end
fdr_values(fdr_values>1) = 1;
disp('FDR 校正完成');

%% ============== Step 5: 计算均值比值 ==============
mean_real = mean(real_probs, 2);                  % 40×1
mean_perm = squeeze(mean(perm_probs, [2,3]));    % 40×1
ratio = mean_real ./ mean_perm;                   % 比值

%% ============== Step 6: 火山图可视化 ==============
figure;
scatter(ratio, -log10(fdr_values), 60, 'k', 'filled'); 
hold on;

xlabel('Mean Real / Mean Permutation');
ylabel('-log_{10}(FDR-adjusted p-value)');
title('Volcano Plot: Transition to M34');

% 阈值线
fdr_thresh = 0.05;
yline(-log10(fdr_thresh), '--r', 'FDR = 0.05');

% 根据显著性高亮
sig_up = ratio > 1 & fdr_values < fdr_thresh;
sig_down = ratio < 1 & fdr_values < fdr_thresh;

scatter(ratio(sig_up), -log10(fdr_values(sig_up)), 80, 'r', 'filled');
scatter(ratio(sig_down), -log10(fdr_values(sig_down)), 80, 'b', 'filled');

% Top5高亮
[~, sort_idx2] = sort(fdr_values, 'ascend');
topN = 5;
top_behaviors = sort_idx2(1:topN);
for i = 1:topN
    text(ratio(top_behaviors(i))+0.05, ...
        -log10(fdr_values(top_behaviors(i)))+0.1, ...
        sprintf('B%d', top_behaviors(i)), ...
        'FontSize', 10, 'FontWeight', 'bold', ...
        'Color', 'r');
end

legend({'Non-significant','Up (p<FDR 0.05)','Down (p<FDR 0.05)'}, ...
       'Location','best');
hold off;

%% ============== Step 6: 火山图 ==============
figure;
scatter(ratio, -log10(p_values), 60, 'b', 'filled'); 
hold on;

xlabel('Mean Real / Mean Permutation');
ylabel('-log_{10}(p-value)');
title('Volcano Plot: Transition to M34');

% 阈值线
yline(-log10(0.05), '--r', 'p = 0.05');

% Top5 高亮
[~, sort_idx] = sort(p_values, 'ascend');
topN = 5;
top_behaviors = sort_idx(1:topN);

scatter(ratio(top_behaviors), -log10(p_values(top_behaviors)), ...
    100, 'r', 'filled', 'MarkerEdgeColor', 'k');

% 标签避免重叠
for i = 1:topN
    text(ratio(top_behaviors(i))+0.1, ...
        -log10(p_values(top_behaviors(i)))+0.1, ...
        sprintf('B%d', top_behaviors(i)), ...
        'FontSize', 10, 'FontWeight', 'bold', 'Color', 'r');
end

hold off;