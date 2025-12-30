%% ==========================
%%一阶马尔可夫模型
%% ==========================
clc; clear;

filename = ''; % 输入文件
outputFile = ''; % 输出文件

%% -------- Step 0: 读取数据 --------
[numData, txtData, rawData] = xlsread(filename);
[numRows, numMice] = size(rawData);
filteredData = cell(1,numMice);

%% -------- Step 1: 行为映射 --------
groupMap = containers.Map('KeyType','double','ValueType','char');
for k = [11,35]; groupMap(k) = 'Climbing'; end
for k = [10,17,37]; groupMap(k) = 'Jumping'; end
for k = [24,39]; groupMap(k) = 'Turningleft'; end
for k = [23,27]; groupMap(k) = 'Turningright'; end
for k = [1,2,22,33,34]; groupMap(k) = 'Hunching'; end
for k = [5,6,12,13]; groupMap(k) = 'Rising'; end
for k = [3,4,9,26,29,36]; groupMap(k) = 'Stepping'; end
for k = [14,15]; groupMap(k) = 'Walking'; end
for k = [7,8,18,19]; groupMap(k) = 'Grooming'; end
for k = [20,21,25,28,30,38]; groupMap(k) = 'Sniffing'; end
for k = [16,31,32,40]; groupMap(k) = 'Other'; end

%% -------- Step 2: 转换为整数序列 --------
for m = 1:numMice
    colData = rawData(:,m);
    colData = colData(~cellfun(@(x) isempty(x) || (isnumeric(x) && isnan(x)), colData));
    colDataNum = cellfun(@(x) double(x), colData);
    newSeq = strings(length(colDataNum),1);
    for t = 1:length(colDataNum)
        if isKey(groupMap, colDataNum(t))
            newSeq(t) = groupMap(colDataNum(t));
        else
            newSeq(t) = "Other";
        end
    end
    filteredData{m} = newSeq;
end

allBehaviors = unique(vertcat(filteredData{:}));
numSymbols = length(allBehaviors);
map2int = containers.Map(allBehaviors, 1:numSymbols);

seqs = cell(1,numMice);
for m = 1:numMice
    seqs{m} = arrayfun(@(x) map2int(x), filteredData{m})';
end

%% -------- Step 3: 构建一阶转移矩阵 --------
numStates = numSymbols; % 每个行为看作可观测状态
trans1 = zeros(numStates, numStates); % 一阶转移矩阵

% 统计转移 s_{t-1} -> s_t
for m = 1:numMice
    seq = seqs{m};
    for t = 2:length(seq)
        i1 = seq(t-1); i2 = seq(t);
        trans1(i1, i2) = trans1(i1, i2) + 1;
    end
end

% 归一化为概率
trans1 = trans1 ./ max(sum(trans1,2),1);

%% -------- Step 4: 模拟序列 --------
numFramesSim = 1000;
simSeqs = cell(1,numMice);

for m = 1:numMice
    % 随机初始化第一个状态
    seqSim = zeros(1, numFramesSim);
    seqSim(1) = randi(numStates);

    % 后续时间点
    for t = 2:numFramesSim
        probs = trans1(seqSim(t-1), :);
        if sum(probs)==0
            seqSim(t) = randi(numStates);
        else
            seqSim(t) = find(mnrnd(1, probs/sum(probs)));
        end
    end
    simSeqs{m} = seqSim;
end

%% -------- Step 5: 导出Excel --------
invMap = containers.Map(1:numSymbols, allBehaviors);
exportCell = cell(numFramesSim+1, numMice);
for m = 1:numMice
    exportCell{1,m} = sprintf('Mouse_%d', m);
    seqStr = string(values(invMap, num2cell(simSeqs{m})))';
    exportCell(2:end,m) = cellstr(seqStr);
end
writecell(exportCell, outputFile);
disp(['✅ 模拟完成，已导出到 ', outputFile]);
