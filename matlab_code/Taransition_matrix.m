
% 两组鼠转移概率分析 + 带方向的 p 值 + 新顺序排列
% 示例文件：merged_PD.xlsx, merged_PDVH.xlsx
% 每列 = 一只鼠，每行 = 时间帧行为编号(1..40)

%% 1) 定义行为分组及名称
group_map = {
    [11,35];                        % 1. Climbing
    [10,17,37];                     % 2. Jumping
    [24,39];                      % 3. Turningleft
    [23,27];                     % 4. Turningright
    [1,2,22,33,34];            % 5. Hunching
    [5,6,12,13];                      % 6. Rising
    [3,4,9,26,29,36];               % 7. Stepping
    [14,15];                        % 8. Walking
    [7,8,18,19];                         % 9. Grooming
    [20,21,25,28,30,38];               % 10. Sniffing
    [16,31,32,40];                  % 11. Undifinable
};

class_names = {'Climbing', 'Jumping', 'Turningleft', 'Turningright', ...
               'Hunching', 'Rising', 'Stepping', 'Walking', ...
               'Grooming', 'Sniffing', 'Undifinable'};
n_behaviors = numel(group_map);

%% 2) 构建查表 map_vec (1..40 -> 1..14)
map_vec = zeros(1,40);
for k = 1:n_behaviors
    codes = group_map{k};
    map_vec(codes) = k;
end

%% 3) 读取数据
data1 = xlsread('merged_PD.xlsx');   % 行=时间帧, 列=鼠
data2 = xlsread('merged_PDVH.xlsx');

n_mice1 = size(data1,2);
n_mice2 = size(data2,2);

mats1 = zeros(n_behaviors, n_behaviors, n_mice1);
mats2 = zeros(n_behaviors, n_behaviors, n_mice2);

%% 4) 计算每只鼠的转移矩阵
for m = 1:n_mice1
    seq = data1(:,m);
    mapped = map_sequence(seq, map_vec);
    mats1(:,:,m) = calc_transition_mapped(mapped, n_behaviors);
end

for m = 1:n_mice2
    seq = data2(:,m);
    mapped = map_sequence(seq, map_vec);
    mats2(:,:,m) = calc_transition_mapped(mapped, n_behaviors);
end

%% 5) 平均矩阵
mean_mat1 = mean(mats1, 3);
mean_mat2 = mean(mats2, 3);


%% 6) 保存平均转移矩阵到 Excel
% 文件名
outputFile1 = 'PD_mean_transition.xlsx';
outputFile2 = 'PDVH_mean_transition.xlsx';

% 写入 Excel
writematrix(mean_mat1, outputFile1, 'Sheet', 1, 'Range', 'A1');
writematrix(mean_mat2, outputFile2, 'Sheet', 1, 'Range', 'A1');

disp(['PD 平均转移矩阵已保存到: ', outputFile1]);
disp(['PDVH 平均转移矩阵已保存到: ', outputFile2]);

% PD
freq1 = zeros(1, n_behaviors);
for m = 1:n_mice1
    seq = data1(:,m);
    mapped = map_sequence_vectorized(seq, map_vec);
    for b = 1:n_behaviors
        freq1(b) = freq1(b) + sum(mapped == b);
    end
end
% 转换为比例
freq1 = freq1 / sum(freq1);

% PDVH
freq2 = zeros(1, n_behaviors);
for m = 1:n_mice2
    seq = data2(:,m);
    mapped = map_sequence_vectorized(seq, map_vec);
    for b = 1:n_behaviors
        freq2(b) = freq2(b) + sum(mapped == b);
    end
end
freq2 = freq2 / sum(freq2);

% 显示结果
disp('PD 每个行为总频率:'); disp(array2table(freq1, 'VariableNames', class_names));
disp('PDVH 每个行为总频率:'); disp(array2table(freq2, 'VariableNames', class_names));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ---- helper functions ----
function mapped = map_sequence(seq, map_vec)
    mapped = nan(size(seq));
    valid = ~isnan(seq) & seq>=1 & seq<=numel(map_vec);
    if any(valid)
        idxs = round(seq(valid));
        mapped(valid) = map_vec(idxs);
        mapped(mapped==0) = NaN;
    end
end

function mat = calc_transition_mapped(mapped_seq, n)
    mat = zeros(n,n);
    L = numel(mapped_seq);
    for k = 1:(L-1)
        a = mapped_seq(k);
        b = mapped_seq(k+1);
        if ~isnan(a) && ~isnan(b)
            mat(a,b) = mat(a,b) + 1;
        end
    end
    % 行归一化
    for r = 1:n
        rs = sum(mat(r,:));
        if rs > 0
            mat(r,:) = mat(r,:) / rs;
        end
    end
end

function mapped = map_sequence_vectorized(seq, map_vec)
    seq = seq(~isnan(seq) & seq>=1 & seq<=numel(map_vec));
    mapped = map_vec(round(seq));
    mapped(mapped==0) = [];
end
