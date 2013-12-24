import optiml.compiler._
import optiml.library._
import optiml.shared._

object PDIPSolverCompiler extends OptiMLApplicationCompiler with PDIPSolver
object PDIPSolverInterpreter extends OptiMLApplicationInterpreter with PDIPSolver

trait PDIPSolver extends OptiMLApplication { 
  def print_usage = {
    println("Usage: PDIPSolver <c> <G> <h> <A> <b> <x0> <s0> <y0> <z0>")
    exit(-1)
  }

  def main() = {

    val MAXITERS = 100
    val ABSTOL = 1e-8
    val RELTOL = 1e-8
    val FEASTOL = 1e-8
    val MINSLACK = 1e-8
    val STEP = 0.99
    val EXPON = 3.0

    // if (args.length != 9) {
    //   print_usage
    // }

    val c = readVector(args(0)).t
    val G = readMatrix(args(1))
    val h = readVector(args(2)).t
    val A = readMatrix(args(3))
    val b = readVector(args(4)).t

    val n = c.length
    val m = h.length
    val p = b.length

    val x0 = readVector(args(5)).t
    val s0 = readVector(args(6)).t
    val y0 = readVector(args(7)).t
    val z0 = readVector(args(8)).t

    // if((G.numCols != c.length)||(A.numCols != c.length)||(G.numRows != h.length)||(A.numRows != b.length)) {
    //   println("error: matrix size mismatch")
    //   println("c := " + c.length)
    //   println("G := " + G.numCols + " x " + G.numRows)
    //   println("h := " + h.length)
    //   println("A := " + A.numCols + " x " + A.numRows)
    //   println("b := " + b.length)
    //   exit(-1)
    // }

    // if((x0.length != n)||(s0.length != m)) {
    //   println("error: primal start size mismatch")
    //   exit(-1)
    // }

    // if((y0.length != p)||(z0.length != m)) {
    //   println("error: dual start size mismatch")
    //   exit(-1)
    // }

    val rc = 1.0+z0*:*s0
    val ri = h-G*x0-s0
    val re = b-A*x0
    val rd = c+G.t*z0+A.t*y0
    val ro = -(c*:*x0+h*:*z0+b*:*y0+1.0)

    val nrmh = max(1.0,norm(h))
    val nrmb = max(1.0,norm(b))
    val nrmc = max(1.0,norm(c))

    val tau0 = 1.0
    val lambda0 = 1.0
    val theta0 = 1.0
    val u0 = DenseVector(tau0).t << z0
    val v0 = y0 << x0 << DenseVector(theta0).t
    val w0 = DenseVector(lambda0).t << s0

    val dimu = m+1
    val dimv = p+n+1
    val dimw = m+1    

    //println("\n%16s%14s".format("duality gap", "residuals"))
    //println("%9s%8s%10s%6s".format("abs.", "rel.", "primal", "dual"))

    implicit def diffPDIP(t1: Rep[Tup4[DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]],
                          t2: Rep[Tup4[DenseVector[Double],DenseVector[Double],DenseVector[Double],DenseVector[Double]]]) = {
      val (u, v, w, viters) = unpack(t2)
      val iters = viters(0)

      val tau = u(0)
      val z = u.slice(1, m+1)
      val y = v.slice(0, p)
      val x = v.slice(p, p+n)
      val theta = v(p+n)
      val lambda = w(0)
      val s = w.slice(1, m+1)

      val pcost = (c*:*x) / tau  
      val dcost = -(h*:*z + b*:*y)/ tau  
      val absgap = (u*:*w) / (tau*tau)
      val relgap = if (dcost > 0.0) {
        absgap / dcost
      } 
      else if (pcost < 0.0) {
        absgap / (-pcost)
      }
      else {
        INF
      }
      val hresi = G*x+s
      val resi = h - hresi/tau
      val hrese = A*x
      val rese = b-hrese/tau
      val hresd = G.t*z + A.t*y
      val resd = c+hresd/tau
      val pres = max(norm(resi)/nrmh, norm(rese)/nrmb)
      val dres = norm(resd)/nrmc
      val hpres = max(norm(hresi), norm(hrese))
      val hdres = norm(hresd)
      
      //println("%02.0f:% 7.0e% 8.0e% 8.0e% 8.0e ".format(iters - 1.0, absgap, relgap, pres, dres))

      if ((pres <= FEASTOL)&&(dres <= FEASTOL)&&((absgap <= ABSTOL)||(relgap <= RELTOL))) {
        // optimal
        0.0
      }
      else if ((h*:*z+b*:*y < 0.0)&&(hdres/abs(h*:*z+b*:*y)<= FEASTOL)) {
        // primal infeasible
        0.0
      }
      else if ((c*:*x < 0.0)&&(hpres/abs(c*:*x) <= FEASTOL)) {
        // dual infeasible
        0.0
      }
      else {
        // continue
        1.0
      }
    }

    val result = untilconverged(pack(u0,v0,w0,DenseVector(0.0)), maxIter = MAXITERS) { cur =>
      val (u, v, w, viters) = unpack(cur)
      val iters = viters(0)
      
      val tau = u(0)
      val z = u.slice(1, m+1)
      val y = v.slice(0, p)
      val x = v.slice(p, p+n)
      val theta = v(p+n)
      val lambda = w(0)
      val s = w.slice(1, m+1)

      val (solx1, soly1, solz1) =  kkt_sol(s / z, G, A, c, -b, -h) 
      val (solx2, soly2, solz2) =  kkt_sol(s / z, G, A, -rd, re, ri)
      val Gx = solx1.toMat <<| solx2.toMat
      val Gy = soly1.toMat <<| soly2.toMat
      val Gz = solz1.toMat <<| solz2.toMat
      val Gzyx = Gz << Gy << Gx

      // compute T = H22 - H21*(H11\H12)
      val T0 = DenseMatrix(DenseVector(-lambda/tau, ro), DenseVector(-ro, 0.0))
      val Ta = (-h.toMat.t <<| -b.toMat.t <<| -c.toMat.t) << (ri.toMat.t <<| re.toMat.t <<| rd.toMat.t)
      val T = T0 + Ta * Gzyx

      val r1 = -s*z
      val r2 = -tau*lambda
      val r3 = -(h*:*z + b*:*y + c*:*x + ro*theta + lambda)
      val r4 = -(-h*tau + G*x + ri*theta + s)
      val r5 = -(-b*tau + A*x + re*theta)
      val r6 = -(-c*tau - G.t*z - A.t*y + rd*theta)
      val r7 = rc - (-ro*tau - ri*:*z - re*:*y - rd*:*x)
      val (dx1,dy1,dz1) = kkt_sol(s/z,G,A,-r6,r5,r4-(r1/z));
      val sol = (T \ (DenseVector(r3-r2/tau, r7) + Ta * (dz1 << dy1 << dx1)))
      val dz = dz1 - Gz*sol
      val dy = dy1 - Gy*sol
      val dx = dx1 - Gx*sol
      val dtau = sol(0)
      val dtheta = sol(1)
      val ds = (r1-s*dz)/z
      val dlambda = (r2-lambda*dtau)/tau
      val du = DenseVector(dtau).t << dz
      val dv = dy << dx << DenseVector(dtheta).t
      val dw = DenseVector(dlambda).t << ds

      val step = 1.0 / ((-du/u) << (-dw/w)).max
      val mu = (u*:*w)/(m+1)
      val muaff = ((u + step*du)*:*(w + step*dw))/(m+1)
      val sigma = pow(muaff/mu, EXPON)

      val rr1 = -ds*dz + sigma*mu
      val rr2 = sigma*mu - dtau*dlambda
      val rr3 = 0.0
      val rr4 = DenseVector.zeros(r4.length).t
      val rr5 = DenseVector.zeros(r5.length).t
      val rr6 = DenseVector.zeros(r6.length).t
      val rr7 = 0.0
      val (ddx1,ddy1,ddz1) = kkt_sol(s/z,G,A,-rr6,rr5,rr4-(rr1/z))
      val tmpl = Ta * (ddz1 << ddy1 << ddx1)
      val ssol = (T \ (DenseVector(rr3-rr2/tau, rr7) + tmpl))
      val dzc = ddz1 - Gz*ssol
      val dyc = ddy1 - Gy*ssol
      val dxc = ddx1 - Gx*ssol
      val dtauc = ssol(0)
      val dthetac = ssol(1)
      val dsc = (rr1-s*dzc)/z
      val dlambdac = (rr2-lambda*dtauc)/tau
      val duc = DenseVector(dtauc).t << dzc
      val dvc = dyc << dxc << DenseVector(dthetac).t
      val dwc = DenseVector(dlambdac).t << dsc

      val ddu = du+duc
      val ddv = dv+dvc
      val ddw = dw+dwc
      val sstep = min(STEP/((-du/u) << (-dw/w)).max, 1.0)
      val u_next = u + sstep*ddu
      val v_next = v + sstep*ddv
      val w_next = w + sstep*ddw

      pack(u_next, v_next, w_next, DenseVector(iters + 1.0))
    }

    val (u, v, w, viters) = unpack(result)

    val tau = u(0)
    val z = u.slice(1, m+1)
    val y = v.slice(0, p)
    val x = v.slice(p, p+n)
    val theta = v(p+n)
    val lambda = w(0)
    val s = w.slice(1, m+1)

    val pcost = (c*:*x) / tau  
    val dcost = -(h*:*z + b*:*y)/ tau  
    val absgap = (u*:*w) / (tau*tau)
    val relgap = if (dcost > 0.0) {
      absgap / dcost
    } 
    else if (pcost < 0.0) {
      absgap / (-pcost)
    }
    else {
      INF
    }
    val hresi = G*x+s
    val resi = h - hresi/tau
    val hrese = A*x
    val rese = b-hrese/tau
    val hresd = G.t*z + A.t*y
    val resd = c+hresd/tau
    val pres = max(norm(resi)/nrmh, norm(rese)/nrmb)
    val dres = norm(resd)/nrmc
    val hpres = max(norm(hresi), norm(hrese))
    val hdres = norm(hresd)

    if ((pres <= FEASTOL)&&(dres <= FEASTOL)&&((absgap <= ABSTOL)||(relgap <= RELTOL))) {
      // optimal
      println("optimal")
      println("\nx")
      (x / tau).pprint
      println("\ns")
      (s / tau).pprint
      println("\nz")
      (z / tau).pprint
      println("\ny")
      (y / tau).pprint
    }
    else if ((h*:*z+b*:*y < 0.0)&&(hdres/abs(h*:*z+b*:*y)<= FEASTOL)) {
      // primal infeasible
      println("primal infeasible")
      val t = abs(h*:*z+b*:*y)
      println("\nt = " + t.toString + "\n")
      println("\nz")
      (z / t).pprint
      println("\ny")
      (y / t).pprint
    }
    else if ((c*:*x < 0.0)&&(hpres/abs(c*:*x) <= FEASTOL)) {
      // dual infeasible
      println("dual infeasible")
      val t = abs(c*:*x)
      println("\nt = " + t.toString + "\n")
      println("\nx")
      (x / t).pprint
      println("\ns")
      (s / t).pprint
    }
    else {
      // did not converge
      println("did not converge\n")
    }
  }

  def kkt_sol(d: Rep[DenseVector[Double]], G: Rep[DenseMatrix[Double]], A: Rep[DenseMatrix[Double]], rx: Rep[DenseVector[Double]], ry: Rep[DenseVector[Double]], rz: Rep[DenseVector[Double]]) = {
    val r = rz << ry << rx

    val GA = G << A
    val DGA = (DenseMatrix.diag(GA.numRows, (-d) << DenseVector.zeros(A.numRows)) <<| GA) << (GA.t <<| DenseMatrix.zeros(A.numCols, A.numCols))

    val xyz = (DGA \ r)

    (xyz.slice(G.numRows + A.numRows, G.numRows + A.numRows + A.numCols), xyz.slice(G.numRows, G.numRows + A.numRows), xyz.slice(0, G.numRows))
  }

  def norm(x: Rep[DenseVector[Double]]) = {
    //sqrt(sum((0::x.length) { i => x(i) * x(i) }))
    sqrt(x *:* x)
  }

  def normalize(x: Rep[DenseVector[Double]]): (Rep[Double], Rep[DenseVector[Double]]) = {
    val n = norm(x)
    (n, x / n)
  }

  def inv2x2(x: Rep[DenseMatrix[Double]]): Rep[DenseMatrix[Double]] = {
    val a = x(0,0)
    val b = x(0,1)
    val c = x(1,0)
    val d = x(1,1)

    val det = a*d - b*c

    DenseMatrix((d/det, -b/det), (-c/det, a/det))
  }

  def lsqr(A: Rep[DenseMatrix[Double]], b: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    val (beta0, u0) = normalize(b)
    val (alpha0, v0) = normalize(A.t * u0)
    val w0 = v0
    val x0 = DenseVector.zeros(A.numCols).t
    val phihat0 = beta0
    val rhohat0 = alpha0

    implicit def diffLSQR(t1: Rep[Tup7[DenseVector[Double],Double,DenseVector[Double],DenseVector[Double],DenseVector[Double],Double,Double]],
                          t2: Rep[Tup7[DenseVector[Double],Double,DenseVector[Double],DenseVector[Double],DenseVector[Double],Double,Double]]) = {
      val (u,alpha,v,w,x,phihat,rhohat) = unpack(t2)
      //println(norm(A*x-b))
      norm(A*x-b)
    }

    val result = untilconverged(pack(u0,alpha0,v0,w0,x0,phihat0,rhohat0), maxIter = 100) { cur =>
      val (u,alpha,v,w,x,phihat,rhohat) = unpack(cur)

      val (beta_, u_) = normalize(A * v - alpha * u)
      val (alpha_, v_) = normalize(A.t * u_ - beta_ * v)

      val rho = sqrt(rhohat * rhohat + beta_ * beta_)
      val c = rhohat / rho
      val s = beta_ / rho
      val theta_ = s * alpha_
      val rhohat_ = -c * alpha_
      val phi = c * phihat
      val phihat_ = s * phihat

      val x_ = x + (phi / rho) * w
      val w_ = v_ - (theta_ / rho) * w

      pack(u_,alpha_,v_,w_,x_,phihat_,rhohat_)
    }

    val (u_opt,alpha_opt,v_opt,w_opt,x_opt,phihat_opt,rhohat_opt) = unpack(result)

    x_opt
  }
}
