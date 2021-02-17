%this scrip loads in pls results and grabs the necessary data for plotting
%this includes the correlations for each behaviour in each of the 3 significant LVs (first 3)
%significance determined in the original pls run, if its more ore less, change
%also includes the confidence interval data as the lower and upper bounds
%also includes the bsr of each brain variable, pct_cov and pval of each lv, and text for behav and micro vars for plotting
%doing all this because i like plotting in python more. save data, load into python, plot


load('pls_res_k10.mat');

behavs_text = {'WordRecall_int', 'WordRecall_slope',...
'AnimalRecall_int', 'AnimalRecall_slope', 'Memory_int', 'Memory_slope',...
'AH4_int', 'AH4_slope', 'AH4verbal_int', 'AH4verbal_slope',...
'AH4numeric_int', 'AH4numeric_slope', 'MillHill_int', 'MillHill_slope'}

usc = pls_prisma_result.usc;
vsc = pls_prisma_result.vsc;
%LV 1
upper = transpose(pls_prisma_result.boot_result.ulcorr(:,1) - pls_prisma_result.lvcorrs(:,1));
lower = transpose(pls_prisma_result.lvcorrs(:,1) - pls_prisma_result.boot_result.llcorr(:,1));

lvcorrs = transpose(pls_prisma_result.lvcorrs(:,1));

bsr = transpose(pls_prisma_result.boot_result.compare_u(:,1));

pct_cov = transpose(pls_pct_cov);
pvals = transpose(pls_prisma_result.perm_result.sprob);
save('lv_one.mat', 'behavs_text', 'upper', 'lower', 'lvcorrs', 'bsr', 'components', 'pct_cov', 'pvals', 'usc', 'vsc');

%LV 2
upper = transpose(pls_prisma_result.boot_result.ulcorr(:,2) - pls_prisma_result.lvcorrs(:,2));
lower = transpose(pls_prisma_result.lvcorrs(:,2) - pls_prisma_result.boot_result.llcorr(:,2));

lvcorrs = transpose(pls_prisma_result.lvcorrs(:,2));

bsr = transpose(pls_prisma_result.boot_result.compare_u(:,2));

pct_cov = transpose(pls_pct_cov);
pvals = transpose(pls_prisma_result.perm_result.sprob);
save('lv_two.mat', 'behavs_text', 'upper', 'lower', 'lvcorrs', 'bsr', 'components', 'pct_cov', 'pvals', 'usc', 'vsc');

