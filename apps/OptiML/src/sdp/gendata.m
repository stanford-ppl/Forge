randn('seed', 42); rand('seed', 42);
n = 100; m = 15; k = 10; sigma = 1; mu = 0.9; lmax = 2; a = mu / lmax;

disp('generating problem');

l = [rand(k,1)+1; -rand(n-k,1)-1];
C = diag(l);
Xopt = diag(max(l, 0));

V = randn(n, m);
V = V / norm(V, 2);

disp('writing c.dat');
cf = fopen('c.dat', 'w');
for i = 1:n
  fprintf(cf, '%d %d %f\n', i-1, i-1, l(i) / 2);
end
fclose(cf);

% disp('writing c.dat');
% cf = fopen('c.dat', 'w');
% for i = 1:n
%   fprintf(cf, '%f ', C(i, :));
%   fprintf(cf, '\n');
% end
% fclose(cf);

disp('writing v0.dat');
cv0 = fopen('v0.dat', 'w');
for i = 1:n
  fprintf(cv0, '%f ', V(i, :));
  fprintf(cv0, '\n');
end
fclose(cv0);

disp('writing soln.dat');
csoln = fopen('soln.dat', 'w');
for i = 1:n
  fprintf(csoln, '%f ', Xopt(i, :));
  fprintf(csoln, '\n');
end
fclose(csoln);
