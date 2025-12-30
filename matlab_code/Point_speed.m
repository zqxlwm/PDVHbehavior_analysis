% 从Excel文件中读取数据
data = readtable('C:\Users\admin\Desktop\PDVH data\Movementdata.xlsx');

% 假设数据在data表格中，第一列是速度值，第二列是状态
speed_values = data{:, 1};  % 提取速度列为数组
states = data{:, 2};        % 提取状态列为数组
n = length(speed_values);   % 数据长度

% 创建一个折线图
figure;
plot(speed_values, 'b-', 'LineWidth', 1.5);  % 绘制蓝色速度折线图
hold on;

% 设置背景颜色
for i = 1:n
    if states(i) == 1

        % 为状态为 34 的点设置背景颜色为红色
        patch([i-0.5 i+0.5 i+0.5 i-0.5], [min(speed_values)-1 min(speed_values)-1 max(speed_values)+1 max(speed_values)+1], ...
              [0,0.537254902,0.4], 'FaceAlpha', 0.6, 'EdgeColor', 'none'); % 红色半透明背景
    end
end

% 重新绘制速度折线图，使其在背景之上
plot(speed_values, 'b', 'LineWidth', 1.5);

% 设置图表标题和标签
title('Speed Values with Background Highlight for States 1 (Orange) and 34 (Red)');
xlabel('Index');
ylabel('Speed Value');

hold off;

