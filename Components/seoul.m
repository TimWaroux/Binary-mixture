% read data

A = readmatrix('ELISAoverzichtjuni2019_v2.xlsx');
X = A(:,4);
X = X(X>0);
X = log10(X);

hold on

histogram(X,'Binwidth',0.05,'Normalization','pdf','FaceAlpha',0.2,'EdgeAlpha',0.5)

% the model

AIC = zeros(1,4);
BIC = zeros(1,4);
negloglik = zeros(1,4);
GMModels = cell(1,4);
options = statset('MaxIter',500);
for k = 1:4
    GMModels{k} = fitgmdist(X,k,'Options',options);
    AIC(k) = GMModels{k}.AIC;
    BIC(k) = GMModels{k}.BIC;
    negloglik(k) = GMModels{k}.NegativeLogLikelihood;
end

[minAIC,numComponentsAIC] = min(AIC);
[minBIC,numComponentsBIC] = min(BIC);

AICModel = GMModels{numComponentsAIC};
BICModel = GMModels{numComponentsBIC};

fplot(@(x)reshape(pdf(AICModel,x(:)),size(x)),[-3 1],'LineWidth',1)
fplot(@(x)reshape(pdf(BICModel,x(:)),size(x)),[-3 1],'LineWidth',1)
% in case numComponentsBIC == 2:
% fplot(@(x)reshape(pdf(GMModels{2},x(:)),size(x)),[-3 1],'LineWidth',1)

hold off

disp(numComponentsAIC)
disp(numComponentsBIC)
