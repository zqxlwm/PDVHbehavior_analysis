%%%% Fig.3e %%%% This code defines a venn class,designed to create Venn diagrams with multiple sets.
% The class includes several methods to customize the appearance of the diagram, handle data, adjust labels, and manage different visualization parameters.
VN=venn();
VN=VN.draw();

    methods
        function obj = venn(varargin)
            if isa(varargin{1},'matlab.graphics.axis.Axes')
                obj.ax=varargin{1};varargin(1)=[];
            else
                obj.ax=gca;
            end
            hold on
            obj.classNum=length(varargin);
            obj.dataList=varargin;

          
            obj.linePnts=load('LD.mat');
            obj.linePnts=obj.linePnts.lineData;

           
            obj.labelPos{2}=[-.38,.3;.38,.3];
            obj.labelPos{3}=[-.38,.3;-.38,-.4;.38,-.4];
            obj.labelPos{4}=[-.38,.2;.38,.2;-.15,.3;.15,.3];
            obj.labelPos{5}=[cos(linspace(2*pi/5,2*pi,5)+2*pi/5-pi/7).*.47;
                             sin(linspace(2*pi/5,2*pi,5)+2*pi/5-pi/7).*.47]';
            obj.labelPos{6}=[cos(linspace(2*pi/6,2*pi,6)+2*pi/3-pi/6).*.49;
                             sin(linspace(2*pi/6,2*pi,6)+2*pi/3-pi/6).*.49]';
            obj.labelPos{6}=obj.labelPos{6}+[0,+.09;-.01,-.04;0,+.015;0,-.1;0,0;0,-.015];
            obj.labelPos{7}=[cos(linspace(2*pi/7,2*pi,7)+2*pi/5-pi/7).*.47;
                             sin(linspace(2*pi/7,2*pi,7)+2*pi/5-pi/7).*.47]';
            help venn
        end

        function obj=draw(obj)
            warning off
            
            obj.ax.XLim=[-.5,.5];
            obj.ax.YLim=[-.5,.5];
            obj.ax.XTick=[];
            obj.ax.YTick=[];
            obj.ax.XColor='none';
            obj.ax.YColor='none';
            obj.ax.PlotBoxAspectRatio=[1,1,1];
            
            tcolorList=lines(7);
            for i=1:obj.classNum
                tPData=obj.linePnts(obj.classNum).pnts{i};
                obj.pshapeHdl{i}=polyshape(tPData(:,1),tPData(:,2));
                obj.fillHdl(i)=fill(tPData(:,1),tPData(:,2),tcolorList(i,:),...
                    'FaceAlpha',.2,'LineWidth',1.5,'EdgeColor',tcolorList(i,:));
            end
            
            baseData=[];
            for i=1:obj.classNum
                baseData=[baseData;obj.dataList{i}(:)];
            end
            baseShpae=polyshape([-.5,-.5,.5,.5],[.5,-.5,-.5,.5]);
            pBool=abs(dec2bin((1:(2^obj.classNum-1))'))-48;
           
            for i=1:obj.classNum
                tPos=obj.labelPos{obj.classNum};
                obj.labelHdl(i)=text(tPos(i,1),tPos(i,2),obj.labelSet{i},...
                    'HorizontalAlignment','center','FontName','Arial','FontSize',16);
            end
           
            for i=1:size(pBool,1)
                tShpae=baseShpae;
                tData=baseData;
                for j=1:size(pBool,2)
                    switch pBool(i,j)
                        case 1
                            tShpae=intersect(tShpae,obj.pshapeHdl{j});
                            tData=intersect(tData,obj.dataList{j});
                        case 0
                            tShpae=subtract(tShpae,obj.pshapeHdl{j});
                            tData=setdiff(tData,obj.dataList{j});
                    end                 
                end
                [cx,cy]=centroid(tShpae);
                obj.textHdl(i)=text(cx,cy,num2str(length(tData)),...
                    'HorizontalAlignment','center','FontName','Arial');
            end  
        end
        
        function obj=labels(obj,varargin)
            tlabel{length(varargin)}=' ';            
            for i=1:length(varargin)
                tlabel{i}=varargin{i};
            end
            obj.labelSet=tlabel;
        end
       
        function setPatch(obj,varargin)
            for i=1:obj.classNum
                set(obj.fillHdl(i),varargin{:})
            end
        end
       
        function setPatchN(obj,N,varargin)
            for i=1:obj.classNum
                set(obj.fillHdl(N),varargin{:})
            end
        end
        
        function setFont(obj,varargin)
            for i=1:length(obj.textHdl)
                set(obj.textHdl(i),varargin{:})
            end
        end
        
        function setLabel(obj,varargin)
            for i=1:length(obj.labelHdl)
                set(obj.labelHdl(i),varargin{:})
            end
        end
        end
        
        
%%% Fig 3k %%%

data_table = readtable()

% The data is in the data table, where the first column represents the speed values and the second column represents the status.
speed_values = data{:, 1};  
states = data{:, 2};        
n = length(speed_values);  


figure;
plot(speed_values, 'b-', 'LineWidth', 1.5);  
hold on;


for i = 1:n
    if states(i) == 1

       
        patch([i-0.5 i+0.5 i+0.5 i-0.5], [min(speed_values)-1 min(speed_values)-1 max(speed_values)+1 max(speed_values)+1], ...
              [0,0.537254902,0.4], 'FaceAlpha', 0.6, 'EdgeColor', 'none'); 
    end
end


plot(speed_values, 'b', 'LineWidth', 1.5);


title('Speed Values with Background Highlight for States 1 (Orange) and 34 (Red)');
xlabel('Index');
ylabel('Speed Value');

hold off;

%%% Fig 3m %%%
 
data_table = readtable('C:\Users\admin\Desktop\PDVH data\eye\PMF_PD.xlsx');


control_group_data = table2array(data_table(:, 1:6));  
treatment_group_data = table2array(data_table(:, 7:12)); 

frame_interval = 30;  


reduced_control_data = reshape(control_group_data, [], frame_interval, size(control_group_data, 2));
reduced_control_data = mean(reduced_control_data, 2);
reduced_control_data = squeeze(reduced_control_data);  

reduced_treatment_data = reshape(treatment_group_data, [], frame_interval, size(treatment_group_data, 2));
reduced_treatment_data = mean(reduced_treatment_data, 2);
reduced_treatment_data = squeeze(reduced_treatment_data);  

disp(['Reduced Control Group Data Size: ', num2str(size(reduced_control_data))]);
disp(['Reduced Treatment Group Data Size: ', num2str(size(reduced_treatment_data))]);

[f_control, sem_control, xi_control] = calculate_pmf_with_sem(reduced_control_data(:));

[f_treatment, sem_treatment, xi_treatment] = calculate_pmf_with_sem(reduced_treatment_data(:));
figure;
hold on;

fill([xi_control, fliplr(xi_control)], [f_control + sem_control, fliplr(f_control - sem_control)], 'b', ...
     'FaceAlpha', 0.2, 'EdgeColor', 'none', 'DisplayName', 'Control ± SEM');
plot(xi_control, f_control, 'LineWidth', 2, 'DisplayName', 'Control Group', 'Color', [0.160784314, 0.219607843, 0.564705882]);

fill([xi_treatment, fliplr(xi_treatment)], [f_treatment + sem_treatment, fliplr(f_treatment - sem_treatment)], 'r', ...
     'FaceAlpha', 0.2, 'EdgeColor', 'none', 'DisplayName', 'Treatment ± SEM');
plot(xi_treatment, f_treatment, 'LineWidth', 2, 'DisplayName', 'Treatment Group', 'Color', [0.749019608, 0.11372549, 0.176470588]);


axis on;  
set(gca, 'Box', 'on'); 

grid on;  
set(gca, 'GridLineStyle', '--'); 

xlim([0, 1]);  
ylim([0, max(max(f_control + sem_control), max(f_treatment + sem_treatment))]);  

xlabel('Speed');
ylabel('Probability Mass');
title('PMF with ±SEM for Control and Treatment Groups', 'FontSize', 14, 'FontWeight', 'bold');
legend('show', 'Location', 'Best');
hold off;


[ecdf_control, x_control] = ecdf(reduced_control_data(:));


[ecdf_treatment, x_treatment] = ecdf(reduced_treatment_data(:));


[h, p] = kstest2(reduced_control_data(:), reduced_treatment_data(:));
disp(['Kolmogorov-Smirnov检验结果: h = ', num2str(h), ', p值 = ', num2str(p)]);


function [f, sem, xi] = calculate_pmf_with_sem(values)

    num_bins = 50; 
    [counts, edges] = histcounts(values, num_bins, 'Normalization', 'probability');
   
    xi = (edges(1:end-1) + edges(2:end)) / 2;

    f = counts;
    sem = sqrt(f .* (1 - f) / length(values)); 
   
    area = trapz(xi, f);  
    if area ~= 0
        f = f / area;  
    end
end
