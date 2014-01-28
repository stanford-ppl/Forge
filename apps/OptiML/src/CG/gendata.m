n = 20;

A = randn(n,n); A = A'*A / n;
b = randn(n,1);

soln = A \ b;

dlmwrite('a.dat', A, ' ')
dlmwrite('b.dat', b, ' ')
dlmwrite('soln.dat', soln, ' ')
