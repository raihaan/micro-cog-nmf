addpath /data/chamal/projects/raihaan/projects/inprogress/hc-nmf-micro/analysis/Pls/plscmd/

mri_raw = csvread('../Hweights_pls_k10.csv',1,1);
behavs_raw = csvread('../int-slope_pls.csv',1,1);
size(mri_raw)
size(behavs_raw)

behavs = {'intercept_words', 'time_since_ph5_words',...
'intercept_animals', 'time_since_ph5_animals', 'intercept_mem', 'time_since_ph5_mem',...
'intercept_ah4', 'time_since_ph5_ah4', 'intercept_ahverb', 'time_since_ph5_ahverb',...
'intercept_ahnum', 'time_since_ph5_ahnum', 'intercept_mh', 'time_since_ph5_mh'}


components={'Comp1_CT', 'Comp1_SA', 'Comp1_MD', 'Comp1_FA', 'Comp1_RD',...
'Comp2_CT', 'Comp2_SA', 'Comp2_MD', 'Comp2_FA', 'Comp2_RD',...
'Comp3_CT', 'Comp3_SA', 'Comp3_MD', 'Comp3_FA', 'Comp3_RD',...
'Comp4_CT', 'Comp4_SA', 'Comp4_MD', 'Comp4_FA', 'Comp4_RD',...
'Comp5_CT', 'Comp5_SA', 'Comp5_MD', 'Comp5_FA', 'Comp5_RD',...
'Comp6_CT', 'Comp6_SA', 'Comp6_MD', 'Comp6_FA', 'Comp6_RD',...
'Comp7_CT', 'Comp7_SA', 'Comp7_MD', 'Comp7_FA', 'Comp7_RD',...
'Comp8_CT', 'Comp8_SA', 'Comp8_MD', 'Comp8_FA', 'Comp8_RD',...
'Comp9_CT', 'Comp9_SA', 'Comp9_MD', 'Comp9_FA', 'Comp9_RD',...
'Comp10_CT', 'Comp10_SA', 'Comp10_MD', 'Comp10_FA', 'Comp10_RD'}

%zscore manually
mu = mean(mri_raw);
sigma = std(mri_raw);

z = bsxfun(@minus,mri_raw,mu);
datamat{1} = bsxfun(@rdivide,z,sigma);

mu = mean(behavs_raw);
sigma = std(behavs_raw);

z = bsxfun(@minus,behavs_raw,mu);
option.stacked_behavdata = bsxfun(@rdivide,z,sigma);

option.method = 3;  % 1 = mean-centering (i.e. group/condition comaprison)
                    % 3 = behaviour (comparing 2 sets of variables)
option.num_perm = 10000;
option.num_boot = 10000;

pls_prisma_result = pls_analysis(datamat,398,1,option);

% check pvalues
pls_prisma_result.perm_result.sprob

% percent covariance
pls_pct_cov = (pls_prisma_result.s .^2) / sum(pls_prisma_result.s .^2);
pls_pct_cov
save('pls_res_k10.mat');



