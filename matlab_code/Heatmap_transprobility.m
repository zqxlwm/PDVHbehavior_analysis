%% ================= 4x4 转移矩阵热图（11类→4类） =================
clear; clc;

%% 1️⃣ 读取 Excel 数据
filename = 'merged2.xlsx';
raw = readcell(filename);  % raw 是 cell 数组

nMouse = size(raw,2);
behaviors_cell = cell(1,nMouse);

for m = 1:nMouse
    colData = raw(:,m);
    colData = colData(~cellfun(@isempty,colData)); % 去掉空单元格
    nums = [];
    for i = 1:length(colData)
        val = colData{i};
        if isnumeric(val)
            nums(end+1,1) = val;
        elseif ischar(val)
            n = str2double(val);
            if ~isnan(n)
                nums(end+1,1) = n;
            end
        end
    end
    behaviors_cell{m} = nums;
end

groups = [ones(1,16), 2*ones(1,16)]; % 前16对照，后16模型

%% 2️⃣ 原始动作 1-40 → 11 类
action_to_behavior = zeros(40,1);
action_to_behavior([11,35]) = 1;     % Climbing
action_to_behavior([10,17,37]) = 2;  % Jumping
action_to_behavior([24,39]) = 3;     % Turningleft
action_to_behavior([23,27]) = 4;     % Turningright
action_to_behavior([1,2,22,33,34]) = 5;  % Hunching
action_to_behavior([5,6,12,13]) = 6;      % Rising
action_to_behavior([3,4,9,26,29,36]) = 7; % Stepping
action_to_behavior([14,15]) = 8;          % Walking
action_to_behavior([7,8,18,19]) = 9;      % Grooming
action_to_behavior([20,21,25,28,30,38]) = 10; % Sniffing
action_to_behavior([16,31,32,40]) = NaN;     % Undifinable → 忽略

%% 3️⃣ 11 类 → 4 类
behavior11_to_class4 = zeros(11,1);

% 1️⃣ Locomotion（位移/运动）
behavior11_to_class4([2,3,4,7,8]) = 1;  % Jumping, Turningleft, Turningright, Stepping, Walking

% 2️⃣ Exploration（竖立/爬/嗅探）
behavior11_to_class4([1,6,10]) = 2;     % Climbing, Rising, Sniffing

% 3️⃣ Maintenance（自我整理）
behavior11_to_class4(9) = 3;            % Grooming

% 4️⃣ Defensiveness（防御/蹲伏）
behavior11_to_class4(5) = 4;            % Hunching

% 11（Undifinable）保持忽略
behavior11_to_class4(11) = NaN;

nStates = 4;
P_all = NaN(nStates,nStates,nMouse);

%% 4️⃣ 计算每只鼠 4x4 转移矩阵
for m = 1:nMouse
    seq = behaviors_cell{m};
    seq(seq<1 | seq>40 | isnan(seq) | seq~=floor(seq)) = NaN;
    
    % 映射 11类 → 4类
    class_seq = NaN(length(seq),1);
    for t = 1:length(seq)
        act = seq(t);
        if ~isnan(act)
            beh11 = action_to_behavior(act);
            if ~isnan(beh11)
                class_seq(t) = behavior11_to_class4(beh11);
            end
        end
    end
    class_seq = class_seq(~isnan(class_seq)); % 去掉未定义动作
    
    % 转移矩阵
    C = zeros(nStates,nStates);
    for t = 1:length(class_seq)-1
        C(class_seq(t), class_seq(t+1)) = C(class_seq(t), class_seq(t+1)) + 1;
    end
    rowSums = sum(C,2);
    P = zeros(nStates,nStates);
    for i = 1:nStates
        if rowSums(i) > 0
            P(i,:) = C(i,:) / rowSums(i);
        end
    end
    P_all(:,:,m) = P;
end

%% 5️⃣ 组间平均差矩阵
mean_control = mean(P_all(:,:,groups==1),3);
mean_model   = mean(P_all(:,:,groups==2),3);
diff_obs = mean_model - mean_control;

%% 6️⃣ permutation test
nPerm = 10000;
diff_perm = zeros(nStates,nStates,nPerm);
allIdx = 1:nMouse;
nControl = sum(groups==1);
rng(42);

for p = 1:nPerm
    permIdx = allIdx(randperm(nMouse));
    perm_control = permIdx(1:nControl);
    perm_model   = permIdx(nControl+1:end);
    mean_c = mean(P_all(:,:,perm_control),3);
    mean_m = mean(P_all(:,:,perm_model),3);
    diff_perm(:,:,p) = mean_m - mean_c;
end

pvals = zeros(nStates,nStates);
for i = 1:nStates
    for j = 1:nStates
        null_dist = squeeze(diff_perm(i,j,:));
        obs = diff_obs(i,j);
        pvals(i,j) = (sum(abs(null_dist) >= abs(obs)) + 1) / (nPerm + 1);
    end
end

rej = mafdr(pvals(:), 'BHFDR', true) < 0.05;
rej = reshape(rej,nStates,nStates);

%% 7️⃣ 绘制 4x4 热图 (红白蓝, 0 居中)
fig = figure('Color','w','Position',[100 100 500 450]);
imagesc(diff_obs);

maxAbs = max(abs(diff_obs(:)));
if maxAbs == 0, maxAbs = 0.01; end
caxis([-maxAbs, maxAbs]);

% 自定义红白蓝配色
n = 256;
redRGB   = [171, 35, 44]/255;
whiteRGB = [1 1 1];
blueRGB  = [35, 51, 127]/255;
n1 = round(n/2);
top = [linspace(whiteRGB(1), redRGB(1), n1)', linspace(whiteRGB(2), redRGB(2), n1)', linspace(whiteRGB(3), redRGB(3), n1)'];
bottom = [linspace(blueRGB(1), whiteRGB(1), n-n1)', linspace(blueRGB(2), whiteRGB(2), n-n1)', linspace(blueRGB(3), whiteRGB(3), n-n1)'];
colormap([bottom; top]);
colorbar;

% 坐标轴
classNames = {'Locomotion','Exploration','Maintenance','Defensiveness'};
set(gca,'XTick',1:nStates,'XTickLabel',classNames,'YTick',1:nStates,'YTickLabel',classNames);
title('Transition probability difference (model - control)');

% 每格显示数值 + 显著性
for i = 1:nStates
    for j = 1:nStates
        val = diff_obs(i,j);
        text(j,i,sprintf('%.2f',val),'HorizontalAlignment','center','Color','k','FontSize',12);
        if rej(i,j)
            p = pvals(i,j);
            if p < 0.001, sig = '***';
            elseif p < 0.01, sig = '**';
            elseif p < 0.05, sig = '*';
            else, sig = '';
            end
            if ~isempty(sig)
                text(j,i+0.25,sig,'Color','k','FontSize',14,'HorizontalAlignment','center');
            end
        end
    end
end
