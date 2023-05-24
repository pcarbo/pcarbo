% The MATLAB code was downloaded from
% https://github.com/tal-amir/sparse-approximation-gsm
%
% These were the steps taken to compile the MEX file on the midway2 
% cluster:
%
%   module load gcc/10.2.0
%   module load matlab/2022b
%   matlab -nosplash -nodesktop
%   cd ~/trimmed_lasso/gsm
%   makeit
%
rng(1);
addpath ~/trimmed_lasso
addpath ~/trimmed_lasso/gsm
load train
k = [1 3 10 20 100];
n = length(k);
m = size(X,2);
B = zeros(m,n);
t = 0;
for i = 1:n
  [b out] = sparse_approx_gsm_v1_22(X,y,k(i));
  B(:,i) = b;
  t = t + out.tElapsed;
end
t % Total elapsed time.
save('trimmed_lasso_coefs.mat','B');
