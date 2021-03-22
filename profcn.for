c
      module param
      integer, parameter :: knd = selected_real_kind(8)
      end module param
c      
       program profcn
c      version 1.09
c      December 2020
c
c  developed by arnie lee van buren and jeffrey boisvert
c  www.mathieuandspheroidalwavefunctions.com
c
c  purpose:     To calculate the first and second kind prolate
c               radial functions r1 and r2 and their first
c               derivatives r1d and r2d for a range of orders m,
c               a range of degrees l beginning at m, and for a
c               specific size parameter c and shape parameter x.
c               To calculate the first kind prolate angular
c               functions and their first derivatives with
c               respect to eta for a range of values for m, l,
c               and eta for a specified value of c.
c
c  Profcn can be run in either double precision or quadruple precision
c  arithmetic. The choice is set in the module param located above
c  right before profcn. For some compilers, the module param must be
c  located before profcn. For others it can be located right after
c  profcn. In param, the kind parameter knd is set by the statement:
c      integer, parameter :: knd = selected_real_kind(8)
c  Set the value of knd in the parenthesis to either 8 for 64 bit
c  arithmetic (double precision) or to 16 for 128 bit arithmetic
c  (quadruple precision). Using quadruple precision will provide
c  higher accuracy over larger parameter ranges but will increase the
c  run time by a significant factor.
c
c  An alternative to using module param is to create two versions of
c  profcn, one for double precision and one for quadruple precision. To
c  do this, replace knd throughout profcn with the appropriate value of
c  kind for the desired precision. Also remove all occurrences of the
c  statement 'use param'.
c
c  Some computers may have more than 8 bytes for double precision
c  data and more than 16 bytes for quadruple precision data. In this
c  case just use the appropriate integers for the kind parameters in
c  module param. Also change the values of kindd and kindq set in
c  statement 5 below the comments section to the number of bytes for
c  double precision data and quadruple precision data, respectively.
c  They are presently set to 8 and 16, respectively. Larger values of
c  kindd and kindq will lead to more accurate results, especially for
c  large values of c and small values of x.
c
c  Some computer also use values for kind that do not correspond to the
c  number of bytes used in real data, In this case just use the values
c  for kind that correspond to double and quadruple precison arithmetic.
c  This includes setting kindd and kindq to the proper values for double
c  and quadruple precision arithmetic, respectively. 
c
c     References
c
c  A. L. Van Buren and J. E. Boisvert, "Accurate calculation of
c  prolate spheroidal radial functions of the first kind and their
c  first derivatives," Quart. Appl. Math. vol 60, pp. 589-599 (2002)
c  [available as a pdf file on the website].
c
c  A. L. Van Buren and J. E. Boisvert, "Improved calculation of prolate
c  spheroidal radial functions of the second kind and their first
c  derivatives," Quart. Appl. Math. vol 62, pp. 493-507 (2004)
c  [available as a pdf file on the website].
c
c     Input options
c
c  Input parameters are read from unit 1 in the file profcn.dat
c  assumed to be in the directory of profcn.for. Profcn.dat
c  contains the following lines of data:
c
c       line 1:
c          mmin   : minimum value for m. (integer)
c          minc   : increment for m. (integer)
c          mnum   : number of values of m. (integer)
c          lnum   : number of values of l [l=m, l=m+1,
c                   ..., l=m+lnum-1]. (integer)
c
c       line 2:
c          ioprad : (integer)
c                 : =0 if radial functions are not computed
c                 : =1 if radial functions of only the first kind
c                      and their first derivatives are computed
c                 : =2 if radial functions of both kinds and
c                      their first derivatives are computed
c
c          iopang : (integer)
c                 : =0 if angular functions are not computed
c                 : =1 if angular functions of the first kind
c                      are computed
c                 : =2 if angular functions of the first kind and
c                      their first derivatives are computed
c
c          iopnorm: (integer)
c                 : =0 if not scaled. The angular functions have
c                      the same norm as the corresponding associated
c                      Legendre function [i.e., we use the Meixner-
c                      Schafke normalization scheme.]
c                 : =1 if angular functions of the first kind
c                      (and their first derivatives if computed)
c                      are scaled by the square root of the
c                      normalization of the corresponding
c                      associated Legendre function. The resulting
c                      scaled angular functions have unity norm.
c
c       line 3:
c          c      : value of the size parameter (= kd/2, where k =
c                   wavenumber and d = interfocal length) (real(knd))
c          x1     : value of the radial coordinate x minus one (real(knd))
c                   (a nominal value of 10.0e0_knd can be entered for x1
c                   if ioprad = 0)
c
c       line 4:
c          ioparg : (integer)
c                 : =0 if both arg1 and darg are angles in degrees
c                 : =1 if arg1 and darg are dimensionless values of eta
c
c          arg1   : first value for the angle coordinate (in degrees
c                   or dimensionless if eta) for which angular
c                   functions are to be computed. (real(knd))
c
c          darg   : increment used to calculate additional desired
c                   arguments for angular functions. (real(knd))
c
c          narg   : number of desired angle arguments. (integer)
c                   (line 4 is not read when iopang = 0)
c
c     Output files
c
c  Output data is written to five files: fort.20, fort.30, fort.40,
c  fort.50 and fort.60. If desired one or more of these files, say
c  fort.50, can be suppressed by searching oblfcn for write(50 and
c  adding a c in column 1 of the statements that are found. The
c  corresponding format statements can also be commented out. Also
c  the open statement found below for the file can be commented out.
c  The open statements use default specs. One may want to extend the
c  blocksize or perhaps change the access to append for one or more of
c  the files.
c
c   fort.20
c
c     This file contains values for all radial functions that have
c     been calculated.
c     The first line in the file contains the values for x, c, and
c     m, formatted as follows (see statements 115 and 120 in subroutine
c     main):
c
c                x      : e23.14 in real*8; e40.30 in real*16
c                c      : e23.14 in real*8; e40.30 in real*16
c                m      : i5
c
c     Each subsequent line in fort.20 contains radial functions
c     for given values of l. The first line contains values for l = m,
c     the next for l=m+1 and continuing to l=m+lnum-1. The radial
c     functions are preceeded by the value of l and followed by the
c     accuracy, equal to the estimated number of accurate decimal digits
c     in the radial functions as measured either using the wronskian
c     or estimated based on subtraction errors and degree of convergence
c     when the Wronskian is used to obtain r2 and r2d.
c
c       The output and corresponding format for each line is as follows
c       (see statements 690, 700, and 710 in subroutine main). [Only 15
c       decimal digits are written to fort.20 using these formats. This
c       is equivalent to the number of decimal digits in real*8
c       arithmetic. If more digits are desired in real*16 arithmetic,
c       the format statements can be modified. The exponents are formatted
c       as I6, allowing for exponents from -99999 to +99999. This can be
c       changed to accommodate a larger exponent range by increasing the
c       format to I7 or larger.]
c
c         for ioprad = 1 or 2:
c
c               l      : value for l (i6)
c               r1c    : characteristic of the prolate radial function
c                        of first kind (f17.14)
c               ir1e   : exponent of the prolate radial function of
c                        first kind (i6)
c               r1dc   : characteristic of the first derivative of the
c                        prolate radial function of first kind (f17.14)
c               ir1de  : exponent of the first derivative of the
c                        prolate radial function of first kind (i6)
c
c        for ioprad = 2, each line also includes;
c
c               r2c    : characteristic of the prolate radial function
c                        of second kind (f17.14)
c               ir2e   : exponent of the prolate radial function of
c                        second kind (i6). If the exponent for any
c                        function is greater than 9999, the format
c                        can be increased to i6 or higher. Note that
c                        the procedures used in this program allow for
c                        exponents much larger than those allowed
c                        in real(knd) arithmetic on the users computer
c                        since the floating point function values
c                        provided are given as a characteristic and an
c                        integer exponent. Use of ratios in calculating
c                        the functions eliminates overflow during the
c                        calculations.
c               r2dc   : characteristic of the first derivative of the
c                        prolate radial function of second kind (f17.14)
c               ir2de  : exponent of the first derivative of the
c                        prolate radial function of second kind (i6).
c                        [See comment above for ir2e.]
c               naccr  : accuracy: equal to the number of decimal digits
c                        of agreement between the theoretical wronskian
c                        and the calculated wronskian or estimated when
c                        the Wronskian is used to obtain r2 and r2d
c                        (i2). This is a measure of the accuracy of the
c                        radial function of the second kind and its
c                        first derivative. The algorithms for calculting
c                        the radial functions of the first kind and
c                        their first derivatives are both robust with
c                        accurate results.
c
c                        To indicate that the accuracy estimate naccr
c                        has been obtained using the Wronskian, the
c                        letter w follows naccr in the output. This
c                        distinguishes it from (1) the case where the
c                        theoretical Wronskian is used to obtain the
c                        value of the denominator (series) appearing
c                        in the variable eta expressions for the radial
c                        function of the second kind and its
c                        first derivative. Here naccr is instead
c                        calculated using the degree of convergence and
c                        associated subtraction errors of the two
c                        numerators. The letter e follows naccr in the
c                        output to designate this case. This version of
c                        profcn may also use the Wronskian to compute the
c                        leading coefficients when computing r2 and r2d
c                        using the taditional Legendre function
c                        expansion. This can sometimes provides accurate
c                        values for x less than or equal to 1.01 when
c                        all of the other methods fail and the
c                        coefficients are inaccurate due to large
c                        subtraction errors incurred in their
c                        computation. This case is also indicated in the
c                        output by the use of the letter e following the
c                        accuracy value naccr.
c
c   fort.30
c
c     This file contains values for all angular functions
c     that have been calculated. Its first line contains the values
c     for c and m, formatted as follows (see statements 60 and 70 in
c     subroutine main).
c
c                c      : e23.14 (knd = 8) or e40.30 (knd = 16)
c                m      : i5
c
c     The second line in fort.30 contains the value for the first l (=m)
c     formatted as follows (see statement 140 in subroutine main):
c
c                l      : i6
c
c     This is followed by a series of narg lines. Each line contains
c     a desired value of angle (ioparg = 0) or angular coordinate eta
c     (ioparg =1) followed by the corresponding angular functions and
c     accuracy. Specific output and format for each line is as follows.
c     [only 15 decimal digits are written to fort.30 using these
c     formats. If more digits are desired in real*16 arithmetic, the
c     formats can be modified.]
c
c        for iopang = 1:
c
c               arg    : for ioparg = 0, angle in degrees (f17.14; see
c                        statement 750 in subroutine main)
c            or barg   ; for ioparg = 1, angular coordinate eta
c                        (f17.14; see statement 750 in subroutine main)
c               s1c    : characteristic of the prolate angular function
c                        of first kind (f17.14; see statement 750)
c               is1e   : exponent of the prolate angular function of
c                        first kind (i5; see statement 750)
c
c        for iopang = 2, each line also includes:
c               s1dc   : characteristic of the first derivative of the
c                        prolate angular function of first kind (f17.14;
c                        see statement 760 in subroutine main)
c               is1de  : exponent of the first derivative of the
c                        prolate angular function of first kind (i5;
c                        see statement 760 in subroutine main)
c
c        for iopang = 1 or 2:
c               naccs  : accuracy: estimate of the number of decimal
c                        digits of accuracy in both the angular function
c                        (and its first derivative when iopang = 2). It
c                        is a conservative estimate based on the
c                        calculated subtraction error in the series
c                        calculation of the angular function. When the
c                        accuracy estimate is equal to 0, the
c                        corresponding angular functions are set equal
c                        to zero. (i2; see statements 750 and 760 in
c                        subroutine main).
c
c   fort.40 and fort.50
c
c     These files are diagnostic files that contain information
c     about specific techniques used and numbers of terms required
c     for the radial function and angular function calculations,
c     respectively. They are annotated and should be self
c     explanatory. Calculated Values are given in full quadruple
c     precision. If fort.40 is not desired, then search for (40,
c     in the program and comment out the lines of code found. Similary,
c     if fort.50 is not desired search for (50, and comment out the
c     lines of code found. The associated format statements and open
c     statements can also be commented out.
c
c   fort.60
c
c     This file is very important, especially when using this program
c     for values of c larger than those given in the table above.
c     Whenever the estimated accuracy falls below a designated integer
c     value during the running of this program, the associated values
c     of x, c, m, and l are written to fort.60. The integer is currently
c     set equal to 6 in the write statement for this file found just
c     before the line numbered 720 below in subrouting main. One can
c     of course change to any other desired value. Just search profcn
c     for write(60 and make the change. If the eigenvalue routine in
c     subroutine conver fails to converge to an eigenvalue between the
c     two neigboring eigenvalues after 50 tries using progressively
c     refined starting values, the m, l, and c values where this occurs
c     will be written to fort.60. Note that this has never been observed
c     over the many years that this routine has been used for prolate
c     eigenvalues.
c
c     Accuracy estimates
c
c  Profcn is written in fortran 90. Use of double precision arithmetic
c  can provide accurate results up to values of c ranging from moderate
c  to very large, depending on the value for x and m. Use of quadruple
c  precision arithmetic provides good results over much wider parameter
c  ranges. However, profcn runs significantly faster using double
c  precision arithmetic. For both double and quadruple precision,
c  the radial functions of the first kind r1 and their first derivatives
c  r1d are nearly fully accurate unless extremely close to a root. The
c  radial functions of the second kind r2 and their first derivatives
c  r2d are usually accurate to ten or more digits in double precision
c  and 25 or more digits in quadruple precision. But r2 and r2d can be
c  much less accurate than this, especially when c is very large, x
c  is between about 1.001 and 1.1, and the values for m and l are
c  intermediate.
c
c  Profcn was tested extensively for both double and quadruple precision
c  arithmetic. The compiler provided a precision of 15 decimal digits in
c  double precision (real*8) and 31 decimal digits in quadruple
c  precision (real*16).
c 
c  A set of values for x was chosen that includes those regions where
c  profcn has difficulty providing highly accurate values for r2 and
c  r2d. Testing for real*8 arithmetic included all orders m up to 1000.
c  Testing using real*16 arithmetic included values of m from 0 to 500
c  in steps of 10 and 550 to 1000 in steps of 50. For each value of m,
c  l went from m up to a sufficiently large value where r1 and r1d have
c  magnitudes smaller than about 10 to the -300 power.
c
c  The testing results are summarized in the table below. For each of
c  the listed x values, the table gives the largest tested value of c
c  for which all of the r2 and r2d values were found to have an
c  estimated accuracy of at least 5 decimal digits, except in the case
c  where one is near a root of either r2 or r2d, where the variable eta
c  method is used to obtain r2 and r2d and where the Wronskian is used
c  to obtain a sufficiently accurate value for the denominator term in
c  this method. Examination of the test results showed that the function
c  value near a root is smaller in magnitude than those values for
c  neighboring degrees l by an order of magnitude no less than the
c  difference between its estimated accuracy and the estimated accuracy
c  of the neighboring function values. Since the neighboring values have
c  an estimated accuracy of 5 or more digits, the effective accuracy of
c  the function value near the root is at least 5 digits. This is
c  because its contribution to the solution of problems involving these
c  functions is reduced by an amount corresponding to its reduction in
c  magnitude and accuracy.
c
c  The estimated accuracy is usually found by comparing the theoretical
c  value for the Wronskian to the value computed using the radial
c  function values. Sometimes, the Wronskian is used to determine values
c  for r2 and r2d and is unavailable to estimate the accuracy. Here a
c  conservative estimate of accuracy is obtained using subtraction
c  errors and the degree of convergence of the series involved in their
c  calculation. The choice of 5 digits is based on the likelihood that
c  5 digits of accuracy is sufficient for most applications of these
c  functions. A rare 4 digit result should also be acceptable.
c
c  Nearly all of the values obtained for r2 and r2d are much more
c  accurate than 5 digits. Lower accuracy tends to occur at higher
c  values of c and then only for a limited range of intermediate values
c  of m and l. For a desired value of x, a conservative estimate of the
c  limit on c is given by the smaller of the limits for the two values
c  of x that it lies between.
c
c  It is possible that a rare 4-digit result other than at a root will
c  occur even when c is less than the appropriate table value. It is not
c  expected that this will be a problem in use of these function values
c  to solve problems involving them. 
c
c                        Approximate Upper limit for c
c
c          x         real*8 arithmetic    real*16 arithmetic
c
c     1.000005            5000                 
c     1.00001             5000                 
c     1.00005             3500                  5000
c     1.0001              2500                  5000
c     1.0005              1200                  3100
c     1.001               1000                  2900
c     1.002                800                  2100
c     1.003                650                  1700
c     1.004                600                  1600
c     1.005                550                  1450
c     1.006                500                  1350
c     1.007                480                  1300
c     1.008                450                  1200
c     1.009                450                  1150
c     1.01                 400                  1100
c     1.02                 350                   900*
c     1.03                 300                   800*
c     1.04                 290                   800*
c     1.05                 290                   900*
c     1.06                 290                  1000*
c     1.07                 290                  2000*
c     1.08                 300                  5000
c     1.09                 300
c     1.10                4000**
c     1.101               5000
c     1.102               4000** 
c     1.103               2000**  
c     1.104               5000
c     1.105               5000
c     1.11                5000
c
c  *  For real*16 arithmetic and for x from about 1.02 to 1.07, much
c     larger values of c than those give in the table above can provide
c     at least 5 digits of accuracy for values of m up to a maximum
c     value less than 1000 but possibly large enough for many
c     applications. I summarize this by listing the largest value of m
c     for specific values of x and c that provide 5 or more digits of
c     accuracy up to the specified value of m. Following the value of x
c     is a series of values of c, each followed in parenthesis by the
c     maximum value of m for that value of c.
c
c     x = 1.02: 1000(90); 1500(130); 2000(170); 3000(230); 4000(290);
c               5000(350)
c     x = 1.03: 1000(120); 1500(180); 2000(240); 3000(330); 4000(420);
c               5000(490)
c     x = 1.04: 1000(180); 1500(250); 2000(310); 3000(440); 4000(550);
c               5000(650)
c     x = 1.05: 1000(190); 1500(310); 2000(420); 3000(550); 4000(700);
c               5000(850)
c     x = 1.06: 1100(300); 1500(390); 2000(500); 3000(750); 4000(900);
c               5000(1000)
c     x = 1.07: 3000(950); 4000(1000); 5000(1000)            
c  
c  ** There is a single 4 digit result that is not near a root for c
c     = 5000 when x = 1.10 and when x = 1.102 and for c = 3000 and 5000
c     when x = 1.103.  
c
c  Additional testing showed that profcn should provide highly accurate
c  results for values of m above 1000 and for l well above those used
c  for the table. Profcn should also provide accurate results for c up
c  to at least 5000 when x is greater than 1.11 for real*8 arithmetic
c  and when x is greater than or equal to 1.08 for real*16 arithmetic.
c  It should provide accurate results for c up to at least 5000 for
c  x from 1.00005 down to values at least as small as 1.000000000001
c  when using either real*8 or real*16 arithmetic.
c
c  Use of the Wronskian to estimate accuracy can sometimes overestimate
c  the accuracy of either r2 or r2d. This happens when one of the two
c  terms in the Wronskian, either r1*r2d or r2*r1d, is smaller in
c  magnitude that the other term. Here its effect on the Wronskian
c  is reduced according to the degree to which it is smaller. Thus the
c  accuracy of either r2 or r2d, whichever is in the smaller term, can
c  be less than the Wronksian test indicates. If the reason for the
c  smaller term is that one of the radial functions is near a root, then
c  its contribution to a calculation using these functions will be
c  reduced accordingly. It is also possible to underestimate the
c  accuracy if the two terms r1*r2d and r2*r1d are nearly equal. Profcn
c  checks for this rare occurrence and adjusts the Wronskian accuracy
c  estimate when it occurs.
c
c  An integer called minacc is used in profcn to designate the
c  minimum number of accurate digits desired for r2 and r2d. The value
c  for minacc controls which methods are used to calculate r2 and r2d
c  and in which order. Minacc is set equal to 10 for real*8 arithmetic.
c  It is recommended that this not be changed. Minacc is set equal to
c  15 for real*16 arithmetic. If more accuracy is desired, minacc can
c  be increased. If greater speed is desired in difficult regions where
c  real*8 arithmetic is insufficient, minacc can be reduced to a value
c  as low as 8 digits. The value of minacc is set in a statement below
c  following these introductory comments.
c
c  The calculated angular functions are highly accurate except for lower
c  values of l (less than about 2c/pi) when c is large. They tend to be
c  less accurate the larger the value of c, the smaller the value of
c  l - m and the closer eta is to unity (i.e., the closer theta is to 0
c  degrees). However, the loss of accuracy (in decimal digits) is due to
c  subtraction error and is accompanied by a proportional decrease in
c  the magnitude of the angular function relative to its corresponding
c  associated Legendre function. This decrease in magnitude almost
c  always results in a corresponding reduction in the magnitude of their
c  contribution to the solution of physical problems involving prolate
c  spheroidal functions. Thus the lower accuracy in some of the angular
c  functions almost always has insignificant impact on calculated
c  solutions.
c
c  Profcn is designed around the number of decimal digits ndec and
c  the maximum exponent nex available in the desired arithmetic on the
c  user's computer. Output values are provided in the form of a
c  characteristic (available up to ndec digits and presently formatted
c  at 15 decimal digits) and an integer exponent (unlimited but
c  presently formatted as i6 giving a range from -99999 to +99999).
c  Note that this exponent range extends well outside the usual real*8
c  and real*16 floating point exponent range.
c
c     d Coefficients
c
c  The user may desire values for the d coefficients that appear in
c  the expression for the angular functions as well as in many of the
c  expressions used to calculate the radial functions. Ratios of
c  successive d coefficients are stored in the vector enr where enr(k)
c  = d(subscript 2k+ix) divided by d(subscript 2k-2+ix). The vector enr
c  is calculated in the subroutine dnorm in statement 20 and passed to
c  subroutine main. The number lim2 of d coefficients calculated for a
c  given l is chosen to be sufficient to compute radial and angular
c  functions for that l. The size of lim2 necessary to compute r1, r1d
c  and s1, s1d and the normaliation factors ranges for low l from less
c  than 100 for low c and somewhat less than int(c) for large c. Lim2
c  increases with increasing l, eventually becoming less than l in size.
c  The size of lim2 needed to compute r2 and r2d can be comparable to
c  this unless they are computed using Neumann function expansions. Then
c  lim2 can be much larger, especially for x very close to unity. Note
c  that the vector enr returned by the subroutine conver contains scaled
c  ratios. The scaling factors are removed in subroutine dnorm to obtain
c  the desired d coefficient ratios.
c
c  The d coefficients themselves can be obtained starting with the value
c  for d with the subscript l - m. If iopnorm is set = 0, Oblfcn uses
c  the Meixner-Schafke scheme for normalizing the angular functions.
c  Here they have the same norm as the corresponding associated Legendre
c  functions. Computation of this normalization is very accurate since
c  the series involved has only positive terms. The subroutine dnorm
c  computes d(subscript l-m) for this normalization and returns it as
c  a characteristic dmlms and an exponent idmlmse to subroutine main.
c  Use of an exponent avoids possible overflow of d(subscript l-m) for
c  extremely large c and m. When the user sets iopnorm = 1 so that the
c  angular functions have unit norm, the corresponding characteristic
c  and exponent for d(subscript l-m) are calculated in subroutine s1
c  and returned to subroutine main as dmlms1 and idmlms1e. Values for
c  the characteristics and exponents of d(subscript l-m) for the  Morse-
c  Feshbach and Flammer normalizations are computed in dnorm and r1bes,
c  respectively, and returned to main as dmlmf, idmlmfe and dmlf,
c  idmlfe. Calculation of the Morse and Feshbach normalization suffers
c  subtraction errors for lower values of l-m and large c that increase
c  as c increases. The value for dmlmf will have reduced accuracy in
c  this case.
c
        use param
c
        real(knd) arg1,c,darg,x1
c
c  Here is where the user sets kindd, the value for kind that
c  corresponds to double precision data on the users computer. Usually,
c  kindd equals the number of bytes used in double precision real data.
c  Similarly, this is where kindq, the value of kind for quadruple
c  precision data, is set. These values are set below to 8 and 16,
c  respectively. They should be changed to the kind values for double
c  precision and quadruple precision if those values are different than
c  these.
c
5       kindd=8
        kindq=16
c
c  set the minimum desired accuray minacc to 10 for real*8
c  arithmetic and to 15 for real*16 arithmetic. These can be
c  changed if desired. See comments below about changing minacc
c
        if(knd.eq.kindd) minacc=8
        if(knd.eq.kindq) minacc=15
c
c     ndec: the maximum number of decimal digits available in real(knd)
c           arithmetic.
c     nex:  the maximum exponent available in real(knd) arithmetic.
c
        ndec=precision(c)
        nex=range(c)-1
c
c  open input and output files
        open(1, file='profcn.dat')
        open(20, file='fort.20')
        open(30, file='fort.30')
        open(40, file='fort.40')
        open(50, file='fort.50')
        open(60, file='fort.60')
c
c  read input data
        read(1,*) mmin,minc,mnum,lnum
        read(1,*) ioprad,iopang,iopnorm
        read(1,*) c,x1
        if(iopang.ne.0) read(1,*) ioparg,arg1,darg,narg
c
c  set array dimensions
        maxm=mmin+minc*(mnum-1)
        maxint=lnum+3*ndec+int(c)+5
        maxj=maxint+maxm
        maxp=maxint
        maxn=maxp+maxm
        maxpdr=4*ndec+5
        neta=993
        ngau=200
        if(ioprad.ne.2) go to 10
        lnump=max(lnum+maxm,1000)
        if(x1.ge.0.00065e0_knd) maxn=2*(lnump*(-18.5e0_knd-
     1                             20.e0_knd*log10(x1))+
     2                             5*ndec+4*maxm+c+05000)+maxm+5
        if(x1.gt.0.08e0_knd) maxn=2*(lnump*(0.5e0_knd-
     1                            3.0e0_knd*log10(x1))+
     2                            5*ndec+4*maxm+c+01000)+maxm+5
        if(x1.gt.1.0e0_knd) maxn=2*(lnump*0.5e0_knd+5*ndec+
     1                           4*maxm+c+00500)+maxm+5
        maxp=max(maxn,maxp)
        if(x1.lt.1.0e-3_knd) ngau=200-50*int(log10(x1)-1.0e-30_knd)
        if(x1.lt.1.0e-10_knd) ngau=250-50*int(log10(x1)-1.0e-30_knd)
        if(x1.lt.1.0e-11_knd) ngau=1200
        if(x1.lt.1.0e-12_knd) ngau=2400
        if(x1.le.0.5e0_knd) maxpdr=maxpdr+int(2.e0_knd*c+
     1                             100.0e0_knd*x1)+400
10      maxq=maxint+maxm+maxm
        maxdr=maxpdr/2+1
        maxp=max(maxp,maxpdr)
        maxd=maxp/2+1
        maxlp=lnum+maxm+5
        maxmp=maxm+5
        maxt=1
        jnenmax=10
        if(iopang.ne.0) maxt=narg
c
         call main (mmin,minc,mnum,lnum,c,ioprad,iopang,iopnorm,
     1              minacc,x1,ngau,ioparg,arg1,darg,narg,neta,maxd,
     2              maxdr,maxint,maxj,maxlp,maxm,maxmp,maxn,maxp,
     3              maxpdr,maxq,maxt,jnenmax,kindd,kindq,ndec,nex)
c
        end
c
c
        subroutine main (mmin,minc,mnum,lnum,c,ioprad,iopang,iopnorm,
     1                   minacc,x1,ngau,ioparg,arg1,darg,narg,neta,maxd,
     2                   maxdr,maxint,maxj,maxlp,maxm,maxmp,maxn,maxp,
     3                   maxpdr,maxq,maxt,jnenmax,kindd,kindq,ndec,
     4                   nex)
c
c  purpose:     To coordinate the calculation of both the prolate
c               spheroidal radial and angular functions and their
c               first derivatives using various algorithms.
c
c  parameters:
c
c     input:    mmin   : minimum desired value of m
c               minc   : increment in m used to compute other values
c               mnum   : number of values of m that are desired
c               lnum   : desired number of values of l = m, m + 1, ...,
c                        m + lnum - 1
c               c      : size parameter
c               ioprad : equal to 0 if no radial functions are desired;
c                        equal to 1 if only radial functions of the
c                          first kind and their first derivatives are
c                          desired;
c                        equal to 2 if radial functions of both kinds
c                          and their first derivatives are desired
c               iopang : equal to 0 if no angular functions are desired;
c                        equal to 1 if only angular functions of the
c                          first kind are desired;
c                        equal to 2 if angular functions of the first
c                          kind and their first derivatives are desired
c               iopnorm: equal to 0 when the angular functions have
c                        the same norm as the corresponding associated
c                        Legendre functions;
c                        equal to 1 when the angular functions are
c                        scaled by the square root of the normalization
c                        of the corresponding Legendre function, giving
c                        them unity norm
c               minacc : desired minimum accuracy for the radial
c                        functions
c               x1     : radial coordinate x minus 1
c               ngau   : order of the Gaussian quadrature to be used in
c                        computing integrals in subroutine pint for use
c                        in subroutine r2int where the integal method
c                        is used to calculate r2 and r2d
c               ioparg : =0 if both arg1 and darg are angles in degrees
c                        =1 if arg1 and darg are dimensionless values
c                           of eta
c               arg1   : first value for the angle coordinate (in
c                        degrees or dimensionless if equal to eta)
c                        for which angular functions are to be computed
c               darg   : increment used to calculate additional desired
c                        arguments for angular functions.
c               narg   : number of desired angle arguments.
c               neta   : number of values available for eta in the
c                        variable eta method for calculating r2 and r2d
c                        (subroutine r2eta); set equal to 993 above                         
c               maxd   : dimension of enr array containing ratios of
c                        the expansion d coefficients
c               maxdr  : dimension of drhor array containing special d
c                        coefficient ratios used in subroutine r2leg
c                        when computing the sum of Legendre functions of
c                        the first kind that appear in the Legendre
c                        function expansion for r2 and r2d   
c               maxint : maximum number of terms available for computing
c                        r2 and r2d in the subroutine r2int; dimension
c                        of the arrays of integrals computed in
c                        subroutine pint 
c               maxj   : equal to the dimension of the array of ratios
c                        of spherical Bessel functions of the first kind
c                        and the array of ratios of the first derivative
c                        of this Bessel function to the corresponding
c                        Bessel function
c               maxlp  : maximum value desired for l
c               maxm   : maximum value desired for m
c               maxmp  : maxm + 5; dimension of the integer array norme
c                        used in scaling of the Neumann functions in
c                        the integrands in subroutine pint
c               maxn   : dimension of the arrays of Neumann function
c                        ratios used in computing r2 and r2d           
c               maxp   : dimension of arrays of Legendre functions of
c                        the first kind used in computing angular
c                        functions, in computing integrands in
c                        subroutine pint and in computing r2 and r2d in
c                        subroutine r2eta
c               maxpdr : dimension of the arrays of ratios of both
c                        Legendre functions of the first kind and their
c                        first derivatives used in the sum of these
c                        functions that contribute to r2 and r2d in
c                        subroutine r2leg 
c               maxq   : dimension of arrays of ratios of Legendre
c                        functions of the second kind and ratios of
c                        their first derivatives used in their sum in
c                        subroutine r2leg                        
c               maxt   : equal to narg if angular functions are
c                        computed where it is the maximum value of the
c                        first index in the arrays of Legendre functions
c                        used in subroutine s1leg;
c                        otherwise equal to 1 to specify the
c                        first index for the Legendre functions used
c                        in the variable eta method for computing r2
c                        and r2d in subroutine r2eta  
c               jneumax: number of arrays of ratios of Legendre and
c                        Neumann functions stored as eta is varied in
c                        subroutine r2eta; set equal to 10 so that the
c                        previous 10 sets of ratios are available
c                        to use without recalculating them when one of
c                        thes previous values for eta is used again for
c                        a later value of l                         
c               kindd  : kind value for double precision real data
c               kindq  : kind value for quadruple precision real data   
c               ndec   : number of decimal digits for real(knd)
c               nex    : maximum exponent for real(knd)
c
        use param
c
c  real(knd) scalars
        real(knd) aj1,aj2,ang,apcoef,apcoefn,api,arg1,c,c2,c4,coefn,
     1            coefme,coefmo,darg,dec,dfnorm,dmfnorm,dmsnorm,dmlf,
     2            dmlmf,dmlms,dmlms1,dneg,d01,eigval,eigvalp,eig1,
     3            eig2,eig3,eig4,eig5,etaval,factor,pcoefe,pcoefet,
     4            pcoefn,pcoefo,pdcoefe,pdcoefet,pdcoefo,pi,qdml,qml,
     5            rm,rm2,r1c,r1dc,r2c,r2dc,r2ec,r2dec,r2ic,r2dic,r2lc,
     6            r2dlc,r2nc,r2dnc,sgn,termpq,x,xb,xbninp,x1,wm,wronc,
     7            wront,wronca,wroncb
c
c  integer and real(knd) arrays with dimension lnum
        dimension iqdl(lnum),iql(lnum),ifajo(lnum)
        real(knd) qdl(lnum),ql(lnum),fajo(lnum)
c
c  real(knd) arrays with dimension maxd
        real(knd) enr(maxd),bliste(maxd),gliste(maxd),
     1            blisto(maxd),glisto(maxd)
c
c  real(knd) arrays with dimension maxdr
        real(knd) drhor(maxdr)
c
c  real(knd) arrays with dimension maxint
        real(knd) pint1(maxint),pint2(maxint),pint3(maxint),
     1            pint4(maxint),rpint1(maxint),rpint2(maxint)
c
c  real(knd) array with dimension maxj
        real(knd) sbesf(maxj),sbesdf(maxj)
c
c  integer and real(knd) arrays with dimension maxlp
        dimension ibese(maxlp),ineue(maxlp),ineuee(maxlp),
     1            ipnormint(maxlp),ineuesv(jnenmax,maxlp)
        real(knd) pnormint(maxlp),sbesdr(maxlp),sbesn(maxlp),
     1            sneun(maxlp),sneune(maxlp),sneudr(maxlp),
     2            sneudre(maxlp),sneunsv(jnenmax,maxlp),
     3            sneudrsv(jnenmax,maxlp)
c
c  integers and real(knd) arrays with dimension maxmp
        real(knd) enrneg(maxmp)
        dimension norme(maxmp)
c
c  real(knd) arrays with dimension maxn
        real(knd) sneuf(maxn),sneudf(maxn),sneufe(maxn),sneudfe(maxn),
     1            sneufsv(jnenmax,maxn),sneudfsv(jnenmax,maxn)
c
c  real(knd) arrays with dimension given by maxp
        real(knd) alpha(maxp),beta(maxp),coefa(maxp),coefb(maxp),
     1            coefc(maxp),coefd(maxp),coefe(maxp),gamma(maxp),
     2            pdr(maxt,maxp),pdrat(maxt,maxp),pdratt(maxp),
     3            pr(maxt,maxp),prat(maxt,maxp),pratb(maxp),pratt(maxp),
     4            prat1(maxp),pratbsv(jnenmax,maxp),
     5            prattsv(jnenmax,maxp),pdratsv(jnenmax,maxp)
c
c  real(knd) arrays with dimension maxpdr
        real(knd) prx(maxpdr),pdrx(maxpdr)
c
c  real(knd) arrays with dimension maxq
        real(knd) qr(maxq),qdr(maxq)
c
c  real(knd) and integer arrays with dimension maxt
        real(knd) arg(maxt),barg(maxt),etainp(maxt),pdnorm(maxt),
     1            pdnorma(maxt),pnorm(maxt),pnorma(maxt),pdtempe(maxt),
     2            pdtempo(maxt),ptempe(maxt),ptempo(maxt),s1c(maxt),
     3            s1dc(maxt),xin(maxt),xlninp(maxt)
        dimension ipdnorm(maxt),ipdnorma(maxt),ipnorm(maxt),
     1            ipnorma(maxt),ipdtempe(maxt),ipdtempo(maxt),
     2            iptempe(maxt),iptempo(maxt),is1e(maxt),is1de(maxt),
     3            naccs(maxt)
c
c  real(knd) arrays with dimension neta
        real(knd) eta(neta),wmeta2(neta),xbn(neta),xln(neta)
c
c  real(knd) arrays with dimension ngau
        real(knd) wr(ngau),xr(ngau)
c
c  miscellaneous integer arrays
        dimension nees(100),naccsav(100),neeb(jnenmax),limpsv(jnenmax),
     1            limnsv(jnenmax),jelimsv(jnenmax)
c
        dec=10.0e0_knd**(-ndec-1)
        if(ioprad.ne.0) x=x1+1.0e0_knd
        jtest=ndec-minacc-2
        pi=acos(-1.0e0_knd)
        api=pi/180.0e0_knd
        c2=c*c
        c4=c2*c2
        nbp=int(2.0e0_knd*c/3.14e0_knd)
        if(knd.eq.kindd) legtest=8
        if(knd.eq.kindq) legtest=min(15,minacc)
c
c  begin loops
          if(iopang.eq.0) go to 20
            do jarg=1,narg
            arg(jarg)=arg1+(jarg-1)*darg
            if(ioparg.eq.0) barg(jarg)=cos(arg(jarg)*api)
            if(ioparg.eq.1) barg(jarg)=arg(jarg)
            end do
20        continue
          igau=0
          if(knd.eq.kindd.and.ioprad.ne.0) write(40,25) x,c
25        format(1x,'x = ',e23.14,/,1x,'c = ',e23.14)
          if(knd.eq.kindq.and.ioprad.ne.0) write(40,25) x,c
30        format(1x,'x = ',e39.31,/,1x,'c = ',e39.31)
          nc=int(log10(c))
          if(nc.lt.0) nc=0
          if(ioprad.eq.2) wront=1.0e0_knd/(c*x1*(x1+2.0e0_knd))
          ibflag1=0
            do 900 mi=1,mnum
            m=mmin+minc*(mi-1)
            m2=m+m
            if(knd.eq.kindd.and.iopang.ne.0) write(50,35) c,m
35          format(1x,'c = ',e23.14,'; m = ',i5)
            if(knd.eq.kindq.and.iopang.ne.0) write(50,40) c,m
40          format(1x,'c = ',e40.30,'; m = ',i5)
            if(ioprad.ne.0) write(40,50) m
50          format(1x,'m = ',i5)
            if(knd.eq.kindd.and.iopang.ne.0) write(30,60) c,m
60          format(1x,e23.14,i5)
            if(knd.eq.kindq.and.iopang.ne.0) write(30,70) c,m
70          format(1x,e40.30,i5)
            rm=m
            rm2=m+m
            iopleg=0
            iopneu=0
            iopeta=0
            iopint=1
            jintm=0
            iopd=3
            limcsav=0
            jjjflag=0
            if(ioprad.ne.2) go to 80
            if(x1.le.0.4e0_knd.and.c.le.10.0e0_knd) iopleg=1
            if(x1.gt.0.4e0_knd.and.c.le.10.0e0_knd) iopneu=1
            if(x1.le.0.4e0_knd.and.c.le.15.0e0_knd.and.minacc.le.16)
     1           iopleg=1
            if(x1.gt.0.4e0_knd.and.c.le.20.0e0_knd.and.minacc.le.16)
     1           iopneu=1
            if(iopleg.eq.1.or.iopneu.eq.1) iopint=0
            ioppsum=1
            iopqnsum=1
              if(knd.eq.kindd) then
              neest=897
              if(x1.gt.0.01e0_knd) neest=769
              if(x1.gt.0.03e0_knd) neest=705
              if(x1.gt.0.04e0_knd) neest=641
              if(x1.gt.0.05e0_knd) neest=577
              if(x1.gt.0.06e0_knd) neest=513
              if(x1.gt.0.07e0_knd) neest=449
              if(x1.gt.0.08e0_knd) neest=385
              if(x1.gt.0.09e0_knd) neest=1
              end if
              if(knd.eq.kindq) then
              neest=905
              if(x1.gt.0.01e0_knd) neest=1
              end if
            nee=neest
            jnen=0
            incnee=64
            if(knd.eq.kindd.and.x1.lt.0.2e0_knd) incnee=32
            msearch=0
80          continue
            if(iopang.eq.0) go to 90
            limps1=lnum+3*ndec+int(c)
            if((limps1+3).gt.maxp) limps1=maxp-3
            iopd=0
            if(iopang.eq.2) iopd=1
            call pleg(m,limps1,maxp,limcsav,iopd,ndec,nex,barg,narg,
     1                maxt,pr,pdr,pdnorm,ipdnorm,pnorm,ipnorm,alpha,
     2                beta,gamma,coefa,coefb,coefc,coefd,coefe)
            limcsav=limps1
            iopd=3
90          if(ioprad.eq.0.or.mi.ne.1) go to 100
            limj=lnum+3*ndec+int(c)+maxm
            xb=sqrt(x1*(x1+2.0e0_knd))
            call sphbes(c,xb,limj,maxj,maxlp,sbesf,sbesdf,sbesn,ibese,
     1                  sbesdr)
100         eig1=0.0e0_knd
            eig2=0.0e0_knd
            eig3=0.0e0_knd
            eig4=0.0e0_knd
            eig5=0.0e0_knd
            iflag=0
            ibflag2=0
            legflag=0
            jflagleg=0
            legstart=m
            nflag=0
            lowacc=ndec
            lowtest=minacc
            nacctest=minacc
            naccintp=minacc+1
            naccleg=0
            naccneu=0
            nacclegp=0
            naccneup=0
            nacceta=0
            naccr=minacc+1
            ietacount=0
            incnflag=0
            iplflag=0
            factor=1.0e0_knd
            ijnet=0
            naccrp=0
            iflagnee=0
            istartr2=1
            jeta=0
            iflagq=0
            iflagp=0
            jbes=3*ndec+int(c)
110         continue
            if(knd.eq.kindd.and.ioprad.ne.0) write(20,115) x,c,m
115         format(1x,e23.14,e23.14,i5)
            if(knd.eq.kindq.and.ioprad.ne.0) write(20,120) x,c,m
120         format(1x,e40.30,e40.30,i5)
              do 850 li=1,lnum
              l=m+(li-1)
              if(iopang.ne.0) write(30,140) l
140           format(1x,i6)
              if(iopang.ne.0) write(50,150) l
150           format(1x,'l = ',i6)
              ix=l-m-2*((l-m)/2)
              iopnee=0
                if(iflagnee.eq.1) then
                incnee=8
                  if(knd.eq.kindd) then
                  if(x1.ge.0.05e0_knd) incnee=16
                  if(x1.ge.0.2e0_knd) incnee=32
                  end if
                  if(knd.eq.kindq) then
                  if(x1.ge.0.05e0_knd) incnee=16
                  if(x1.ge.0.1e0_knd) incnee=32
                  end if
                iflagnee=2
                end if
              naccetas=minacc
              if(li.eq.1) naccrsav=minacc
              if(li.gt.1) naccrsav=naccr
              naccr=-1
              nacce=0
              limdrad=3*ndec+int(c)
              if(ioprad.ne.0.and.li.ne.1) limdrad=jbes+jbes+20+
     1                                            int(sqrt(c))
              if(iopint.ne.0.and.li.ne.1.and.jintm.gt.jbes)
     1            limdrad=jintm+jintm+20+int(sqrt(c))
              limdang=3*ndec+int(c)
              if(iopang.ne.0.and.li.ne.1) limdang=jang+jang+20+
     1                                            int(sqrt(c))
              if(iopang.eq.0) limd=limdrad
              if(ioprad.eq.0) limd=limdang
              if(iopang.ne.0.and.ioprad.ne.0) limd=max(limdang,limdrad)
              if(li.eq.1) limmf=limdang
              if(li.gt.1) limmf=jmf+jmf+20+int(sqrt(c))
              limd=max(limd,limmf)
              if(ioprad.ne.2) go to 155
              if(iopleg.eq.1) limdleg=l-m+3*ndec+int(c)
              if(iopleg.eq.2) limdleg=jleg+jleg+20+int(sqrt(c))
              if(iopleg.ne.0) limd=max(limd,limdleg)
              limdneu=limd
              lplus=max(l,1000)
              if(x1.ge.0.00065e0_knd) limdneu=2*((lplus)*(-18.5e0_knd-
     1                                  20.0e0_knd*log10(x1))+
     2                                  5*ndec+4*m+c+01000)
              if(x1.gt.0.08e0_knd) limdneu=2*((lplus)*(0.5e0_knd-
     1                                  3.0e0_knd*log10(x1))+
     2                                  5*ndec+4*m+c+01000)
              if(x1.gt.1.0e0_knd) limdneu=2*((lplus)*0.5e0_knd+5*ndec+
     1                                    4*m+c+00500)
              if(iopneu.eq.2.and.naccneu.gt.0)
     1               limdneu=jneu+jneu+20+int(sqrt(c))
              if(iopneu.ne.0) limd=max(limd,limdneu)
              limdeta=limd
              if(x1.ge.0.00065e0_knd) limdeta=2*((lplus)*(-18.5e0_knd-
     1                                     20.0e0_knd*log10(x1))+5*ndec+
     2                                     4*m+c+05000)
              if(x1.gt.0.08e0_knd) limdeta=2*((lplus)*(0.5e0_knd-
     1                                     3.0e0_knd*log10(x1))+5*ndec+
     2                                     4*m+c+01000)
              if(x1.gt.1.0e0_knd) limdeta=2*((lplus)*0.5e0_knd+5*ndec+
     1                                    4*m+c+00500)
              if(iopeta.eq.3.and.naccrsav.gt.minacc)
     1                        limdeta=jeta+jeta+500+c/10
              if(iopeta.eq.3.and.naccrsav.le.minacc)
     1                        limdeta=jeta+jeta+500+c
              if(iopeta.ne.0) limd=max(limd,limdeta)
155           continue
              if(limd.gt.maxp) limd=maxp
              call geteig (l,m,c,eig2,eig3,eig4,eig5,eigval)
              eig1=eig2
              eig2=eig3
              eig3=eig4
              eig4=eig5
c
c  use Bouwkamp procedure to obtain accurate eigenvalues
              if(l.eq.m) ienre=(3*ndec+int(c))/2
              if(l.eq.m) jlowe=1
              if(l.eq.m) limdle=2
              if(l.eq.m+1) ienro=(3*ndec+int(c))/2
              if(l.eq.m+1) jlowo=1
              if(l.eq.m+1) limdlo=3
c
c  compute the coeficients in the Bouwkamp method
              if(ix.eq.1) go to 160
c
c  beta coefficients (bliste) for l-m even
              if(limdle.gt.limd) go to 163
              j=jlowe
                do 158 i=limdle,limd,2
                i2=i+i
                bliste(j)=c4*real(i,knd)*real((i-1),knd)*
     1                    real((m2+i),knd)*real((m2+i-1),knd)/
     2                    (real((m2+i2-1),knd)*real((m2+i2-1),knd)*
     3                    real((m2+i2-3),knd)*real((m2+i2+1),knd))
                j=j+1
158             continue
c
c  gamma coeficients (gliste) for l-m even
              j=jlowe
                do 159 i=limdle-1,limd+1,2
                i2=i+i
                gliste(j)=real((m+i-1),knd)*real((m+i),knd)+0.5e0_knd*
     1                    c2*((1.0e0_knd-real((m2*m2-1),knd)/
     2                    (real((m2+i2-3),knd)*real((m2+i2+1),knd))))
                j=j+1
159             continue
              go to 163
160           continue
c
c  beta coefficients (blisto) for l-m odd
              if(limdlo.gt.limd) go to 163
              j=jlowo
                do 161 i=limdlo,limd,2
                i2=i+i
                blisto(j)=c4*real(i,knd)*real((i-1),knd)*
     1                    real((m2+i),knd)*real((m2+i-1),knd)/
     2                    (real((m2+i2-1),knd)*real((m2+i2-1),knd)*
     3                    real((m2+i2-3),knd)*real((m2+i2+1),knd))
                j=j+1
161             continue
c
c  gamma coeficient (glisto) for l-m odd
              j=jlowo
                do 162 i=limdlo-1,limd+1,2
                i2=i+i
                glisto(j)=real((m+i-1),knd)*real((m+i),knd)+0.5e0_knd*
     1                    c2*(1.0e0_knd-real((m2*m2-1),knd)/
     2                    (real((m2+i2-3),knd)*real((m2+i2+1),knd)))
              j=j+1
162           continue
163           continue
              if(ix.eq.0) call conver(l,m,c,limd,bliste,gliste,eig1,
     1                        eig3,eig4,ndec,maxd,eigval,eig5,enr,ienre)
              if(ix.eq.1) call conver(l,m,c,limd,blisto,glisto,eig1,
     1                        eig3,eig4,ndec,maxd,eigval,eig5,enr,ienro)
              if(knd.eq.kindd.and.ioprad.ne.0) write(40,165) l,eigval
165           format(1x,'l =',i6,5x,'eigenvalue =',e23.14)
              if(knd.eq.kindq.and.ioprad.ne.0) write(40,170) l,eigval
170           format(1x,'l =',i6,5x,'eigenvalue =',e40.30)
              if(ix.eq.1) go to 175
              limdle=limd+2
              if(2*(limd/2).ne.limd) limdle=limd+1
              jlowe=limd/2+1
              go to 176
175           limdlo=limd+1
              if(2*(limd/2).ne.limd) limdlo=limd+2
              jlowo=(limd-1)/2+1
176           call dnorm (l,m,c,ndec,nex,limd,maxd,enr,sgn,d01,id01,
     1                    dmfnorm,idmfe,dmlmf,idmlmfe,dmsnorm,idmse,
     2                    dmlms,idmlmse,jmf,jsub)
              if(li.ne.1.and.eigval.le.eigvalp) go to 900
              if(l.eq.m.and.jsub.gt.jtest.and.iopneu.ne.0) iopneu=0
              if(l.eq.m.and.jsub.gt.jtest.and.iopleg.ne.0) iopleg=0
              eigvalp=eigval
c
              if(ioprad.eq.0) go to 720
c  determine prolate radial functions of the first kind
              write(40,178)
178           format(4x,'r1 and r1d calculation')            
              if(li.eq.1) limr1=3*ndec+int(c)
              if(li.ne.1) limr1=jbes+jbes+20+int(sqrt(c))
              call r1bes(l,m,c,x1,limr1,ndec,maxd,enr,maxj,maxlp,
     1                   nex,iflag,sbesf,sbesdf,sbesn,ibese,sbesdr,
     2                   d01,id01,r1c,ir1e,r1dc,ir1de,dfnorm,jbes,
     3                   factor)
              iterm=int(log10(abs(dfnorm)))
              dfnorm=dfnorm*(10.0e0_knd**(-iterm))
              idfe=iterm
              dmlf=1.0e0_knd/dfnorm
              idmlfe=-idfe
              if(ioprad.eq.2) ir2est=int(log10(wront))-ir1de+1
              if(knd.eq.kindd) write(40,180) r1c,ir1e,r1dc,ir1de
              if(knd.eq.kindq) write(40,185) r1c,ir1e,r1dc,ir1de
180           format(10x,'r1 = ', f17.14,i6,5x,'r1d = ',f17.14,i6)
185           format(10x,'r1 = ', f34.31,i6,5x,'r1d = ',f34.31,i6)
              if(ioprad.ne.2) go to 680
c
c  determine prolate radial functions of the second kind
c
              write(40,187)
187           format(4x,'r2 and r2d calculation')
c  calculation using integration technique
              if(iopint.eq.0) go to 230
              if(iopint.eq.2) go to 190
              limint=lnum+3*ndec+int(c)
              if(igau.eq.0) call gauss(ndec,ngau,xr,wr)
              igau=1
              ngqs=10
              if(c.gt.2000.0e0_knd) ngqs=ngqs*(c/2000.0e0_knd)*
     1                                    (c/2000.0e0_knd)
              call pint(c,m,lnum,x1,limint,maxint,maxlp,maxmp,ndec,
     1                  wr,xr,ngau,ngqs,rpint1,rpint2,pint1,
     2                  pint2,pint3,pint4,norme,pnormint,ipnormint,
     3                  coefme,coefmo)
190           continue
              if(iopint.eq.1) limint=3*ndec+int(c)
              if(iopint.eq.2) limint=jintm+jintm+20+int(sqrt(c))
              call r2int(l,m,c,x,limint,ndec,nex,maxd,enr,d01,id01,
     1                   maxint,maxmp,maxlp,rpint1,rpint2,pint1,pint2,
     2                   pint3,pint4,norme,pnormint,ipnormint,coefme,
     3                   coefmo,r2ic,ir2ie,r2dic,ir2die,jint,coefn,
     4                   icoefn)
              iopint=2
              if(jint.gt.jintm) jintm=jint
              wronca=r1c*r2dic*10.0e0_knd**(ir1e+ir2die)
              wroncb=r2ic*r1dc*10.0e0_knd**(ir2ie+ir1de)
              wronc=wronca-wroncb
              naccint=-int(log10(abs((wronc-wront)/wront)+dec))
              if(naccint.lt.0) naccint=0
              nsubw=-int(log10(abs(wronc/wronca)+dec))
              if(nsubw.lt.0) nsubw=0
              if(naccint.gt.1) naccint=naccint+nsubw
                if(nsubw.gt.0) then
                write(40,200) nsubw
                end if
200           format(15x,'sub. error in forming wronskian = ',i3,
     1               ' digits.')
                if(naccint.ge.naccr) then
                naccr=naccint
                r2c=r2ic
                ir2e=ir2ie
                r2dc=r2dic
                ir2de=ir2die
                nacce=0
                end if
              if(naccint.ge.minacc.and.iopneu.ne.0) iopneu=4
              if(naccint.ge.minacc.and.(iopeta.eq.1.or.iopeta.eq.2.or.
     1             iopeta.eq.4)) iopeta=4
              istartr2=1
                if(naccint.ge.minacc.and.naccintp.ge.minacc) then
                istartr2=0
                iopneu=0
                iopeta=0
                end if
              if(naccint.ge.minacc.and.ndec-jsub.le.naccint.and.
     1                iopleg.ne.0) iopleg=0
              if(naccint.lt.minacc.and.x1.le.0.1e0_knd.and.
     1           iopleg.eq.0.and.l.ge.legstart.and.
     2           jsub.le.ndec-naccint.and.jsub.le.ndec-naccrp) iopleg=1
              if(naccint.eq.0.and.naccintp.eq.0) iopint=0
              if(knd.eq.kindd) write(40,210) naccint,r2ic,ir2ie,r2dic,
     1                                       ir2die
              if(knd.eq.kindq) write(40,220) naccint,r2ic,ir2ie,r2dic,
     1                                       ir2die
210           format(15x,'accuracy in decimal digits = ',i2,/,10x,
     1               'r2 = ',f17.14,i6,5x,'r2d = ',f17.14,i6)
220           format(15x,'accuracy in decimal digits = ',i2,/,10x,
     1               'r2 = ',f34.31,i6,5x,'r2d = ',f34.31,i6)
230           continue
c
c  calculation using Legendre expansion and joining factor
              if(iopleg.eq.0) go to 360
              if(jflagleg.eq.1) go to 310
              jflagleg=1
              limdr=c+2*ndec+50.0e0_knd*x1+200
              if(limdr.gt.maxdr-2) limdr=maxdr-2
              if(ioppsum.eq.0) go to 250
              xin(1)=x
              limpleg=limdr+limdr
              iopd=3
              call pleg(m,limpleg,maxp,limcsav,iopd,ndec,nex,xin,1,maxt,
     1                  prat,pdrat,pdnorma,ipdnorma,pnorma,ipnorma,
     2                  alpha,beta,gamma,coefa,coefb,coefc,coefd,coefe)
              limcsav=max(limcsav,limpleg)
                do jj=1,limpleg
                prx(jj)=prat(1,jj)
                pdrx(jj)=pdrat(1,jj)
                end do
250           limq=lnum+3*ndec+int(c)
              call qleg(m,lnum,limq,maxq,x1,ndec,qdr,qdml,iqdml,qdl,
     1                  iqdl,qr,qml,iqml,ql,iql,termpq,itermpq)
              fajo(1)=c/(rm2-1.0e0_knd)
              ifajo(1)=0
              if(m.eq.0) go to 280
                do im=1,m
                fajo(1)=fajo(1)*(im+im)/c
                if(abs(fajo(1)).lt.1.0e+10_knd) go to 260
                fajo(1)=fajo(1)*(1.0e-10_knd)
                ifajo(1)=ifajo(1)+10
260             continue
                if(abs(fajo(1)).gt.1.0e-10_knd) go to 270
                fajo(1)=fajo(1)*(1.0e+10_knd)
                ifajo(1)=ifajo(1)-10
270             continue
                end do
280           continue
              fajo(2)=-c*fajo(1)/(rm2-3.0e0_knd)
              ifajo(2)=ifajo(1)
                do jl=3,lnum-1,2
                fajo(jl)=fajo(jl-2)*real((jl+m+m-1),knd)/(jl-2)
                ifajo(jl)=ifajo(jl-2)
                if(abs(fajo(jl)).lt.1.0e10_knd) go to 290
                fajo(jl)=fajo(jl)*1.0e-10_knd
                ifajo(jl)=ifajo(jl)+10
290             fajo(jl+1)=fajo(jl-1)*real((jl+m+m-1),knd)/(jl)
                ifajo(jl+1)=ifajo(jl-1)
                if(abs(fajo(jl+1)).lt.1.0e10_knd) go to 300
                fajo(jl+1)=fajo(jl+1)*1.0e-10_knd
                ifajo(jl+1)=ifajo(jl+1)+10
300             end do
              if(2*(lnum/2).eq.lnum.or.lnum.eq.2) go to 310
              fajo(lnum)=fajo(lnum-2)*real((lnum+m+m-1),knd)/(lnum-2)
              ifajo(lnum)=ifajo(lnum-2)
310           continue
c
              limleg=l-m+3*ndec+int(c)
              limdr=c+ndec+50.0e0_knd*x1+200
              if(iopleg.eq.2) limleg=jleg+jleg+20+int(sqrt(c))
              if(iopleg.eq.2) limdr=jlegp+10+int(0.5e0_knd*sqrt(c))
              if(limdr.gt.maxdr) limdr=maxdr
              nsdneg=0
              dneg=1.0e0_knd
              idneg=0
              call dalt(l,m,c,limdr,maxdr,maxmp,ndec,nex,ioppsum,eigval,
     1                  enrneg,drhor,dneg,idneg,nsdneg,nsdrho)
              call r2leg(l,m,c,x1,lnum,limleg,limdr,ndec,nex,
     1                   maxd,maxmp,maxpdr,maxdr,maxq,enr,enrneg,drhor,
     2                   nsdrho,d01,id01,dneg,idneg,nsdneg,dfnorm,idfe,
     3                   dmfnorm,idmfe,prx,pdrx,qdr,qdml,iqdml,qdl,iqdl,
     4                   qr,qml,iqml,ql,iql,fajo,ifajo,jsub,termpq,
     5                   itermpq,ioppsum,iopqnsum,r1c,ir1e,r1dc,
     6                   ir1de,wront,minacc,r2lc,ir2le,r2dlc,ir2dle,
     7                   jleg,jlegp,naccleg,nsubw,jflagl,iflagq,iflagp)
                if(nsubw.gt.0) then
                write(40,200) nsubw
                end if
              if(nacclegp-naccleg.gt.8) naccleg=nacclegp
              if(naccleg.le.naccr) go to 320
              naccr=naccleg
              r2c=r2lc
              ir2e=ir2le
              r2dc=r2dlc
              ir2de=ir2dle
              nacce=jflagl
320           continue
              if(naccleg.ge.naccrsav) then
              iopleg=2
              else
              if(iopleg.eq.1.and.l.ne.m) iopleg=0
              legstart=l+naccrsav-naccleg
              end if
              if(naccleg.ge.minacc.and.nacclegp.ge.minacc) then
              iopleg=2
              iopneu=0
              iopeta=0
              end if
              if(iopint.ne.0.and.naccleg.gt.max(naccint,naccintp)
     1           .and.nacclegp.gt.max(naccint,naccintp)
     2           .and.jflagl.eq.0) iopint=0
              nacclegp=naccleg
              if(knd.eq.kindd) write(40,210) naccleg,r2lc,ir2le,r2dlc,
     1                                       ir2dle
              if(knd.eq.kindq) write(40,220) naccleg,r2lc,ir2le,r2dlc,
     1                                       ir2dle
360           continue
c
c  calculation using conventional Neumann expansion (eta=1)
              if(iopneu.eq.0) go to 420
              if(iopneu.eq.2) go to 380
              if(ibflag1.eq.1) go to 370
              ibflag1=1
              lnump=max(lnum+maxm,1000)
              limn1=2*(lnump*(-18.5e0_knd-20.0e0_knd*log10(x1))+
     1             5*ndec+4*m+c+01000)+maxm
              if(x1.gt.0.08e0_knd) limn1=2*(lnump*(0.5e0_knd-3.0e0_knd*
     1                                  log10(x1))+5*ndec+4*m+c+01000)+
     2                                   maxm
              if(x1.gt.1.0e0_knd) limn1=2*(lnump*0.5e0_knd+5*ndec+4*m+c+
     1                                 00500)+maxm
              if(limn1.gt.maxn) limn1=maxn
              call sphneu(c,x,limn1,maxn,maxlp,sneuf,sneun,ineue,sneudf,
     1                    sneudr)
370           if(ibflag2.eq.1) go to 380
              ibflag2=1
              lp=max(lnum+m,1000)
              limp1=2*(lp*(-18.5e0_knd-20.0e0_knd*log10(x1))+
     1             5*ndec+4*m+c+01000)
              if(x1.gt.0.08e0_knd) limp1=2*(lp*(0.5e0_knd-3.0e0_knd*
     1                                  log10(x1))+5*ndec+4*m+c+01000)
              if(x1.gt.1.0e0_knd) limp1=2*(lp*0.5e0_knd+5*ndec+4*m+c+
     1                                 00500)
              if(limp1.gt.maxp) limp1=maxp
              prat1(1)=1.0e0_knd
              prat1(2)=rm2+1.0e0_knd
                do jp=3,limp1
                aj1=jp-1
                aj2=jp-2
                prat1(jp)=(rm2+aj1)*(rm2+aj2)/(aj1*aj2)
                end do
              pcoefn=x1*(x1+2.0e0_knd)/(x*x)
              apcoefn=(rm/2.0e0_knd)*log10(pcoefn)
              ipcoefn=int(apcoefn)
              pcoefn=10.0e0_knd**(apcoefn-ipcoefn)
380           continue
              lplus=max(l,1000)
              limneu=2*((lplus)*(-18.5e0_knd-20.0e0_knd*log10(x1))+
     1               5*ndec+4*m+c+01000)
              if(x1.gt.0.08e0_knd) limneu=2*((lplus)*(0.5e0_knd-
     1                      3.0e0_knd*log10(x1))+5*ndec+4*m+c+01000)
              if(x1.gt.1.0e0_knd) limneu=2*((lplus)*0.5e0_knd+5*ndec+
     1                                   4*m+c+00500)
              if(iopneu.eq.2.and.naccneu.gt.0) limneu=jneu+jneu+20+
     1                               int(sqrt(c))+int(1.0e0_knd/x1)
              if(limneu.gt.limp1-2) limneu=limp1-2
              call r2neu(l,m,c,x1,limneu,ndec,nex,maxd,maxlp,maxn,maxp,
     1                   minacc,enr,sneuf,sneun,ineue,sneudf,sneudr,
     2                   prat1,pcoefn,ipcoefn,dmfnorm,idmfe,r1dc,ir1de,
     3                   r2nc,ir2ne,r2dnc,ir2dne,jneu)
              wronca=r1c*r2dnc*10.0e0_knd**(ir1e+ir2dne)
              wroncb=r2nc*r1dc*10.0e0_knd**(ir2ne+ir1de)
              wronc=wronca-wroncb
              naccneu=-int(log10(abs((wronc-wront)/wront)+dec))
              if(naccneu.lt.0) naccneu=0
              nsubw=-int(log10(abs(wronc/wronca)+dec))
              if(nsubw.lt.0) nsubw=0
              if(naccneu.gt.1) naccneu=naccneu+nsubw
                if(nsubw.gt.0) then
                write(40,200) nsubw
                end if
              if(naccneup-naccneu.gt.8) naccneu=naccneup
              naccneup=naccneu
              if(naccneu.le.naccr) go to 390
              naccr=naccneu
              iopneu=2
              r2c=r2nc
              ir2e=ir2ne
              r2dc=r2dnc
              ir2de=ir2dne
              nacce=0
390           continue
              if(naccneu.gt.minacc) then
              nflag=1
              iopneu=2
              iopeta=0
              iopint=0
              end if
              if(naccneu.eq.minacc) then
              if(iopeta.ne.0) iopeta=4
              end if
              if(iopeta.eq.0.and.naccr.lt.minacc.and.nflag.eq.0) then
              iopeta=1
              end if
              if(knd.eq.kindd) write(40,210) naccneu,r2nc,ir2ne,r2dnc,
     1                                       ir2dne
              if(knd.eq.kindq) write(40,220) naccneu,r2nc,ir2ne,r2dnc,
     1                                       ir2dne
420           continue
c
c  calculation using the variable eta expansion
              if(iopeta.eq.0.or.iopeta.eq.4) go to 670
                do 430 inn=1,100
                nees(inn)=0
                naccsav(inn)=0
430             continue
              inen=0
              neemark=nee
              naccetamax=0
              neemax=nee
              naccnmax=0
              nacctemp=0
              kounte=0
              netatry=1
              naccdp=0
              if(iopeta.gt.1) go to 440
              kounter=0
                if(ijnet.eq.0) then
                  do jnet=1,neta
                  ang=(neta+1-jnet)*pi*0.5e0_knd/(neta+1)
                  eta(jnet)=cos(ang)
                  wmeta2(jnet)=2.0e0_knd*(1.0e0_knd+eta(jnet))*
     1                         (sin(0.5e0_knd*ang)**2)
                  xbn(jnet)=sqrt(x1*(x1+2.0e0_knd)+eta(jnet)*
     1                      eta(jnet))
                  xln(jnet)=eta(jnet)*(x1+1.0e0_knd)/xbn(jnet)
                  end do
                ijnet=1
                end if
              iopeta=2
440           if(iopeta.eq.3) go to 540
              etaval=eta(nee)
450           xbninp=xbn(nee)
              netainp=1
              etainp(1)=eta(nee)
              xlninp(1)=xln(nee)
              lplus=max(l,1000)
              limn=2*((lplus)*(-18.5e0_knd-20.0e0_knd*log10(x1))+
     1             5*ndec+10*incnee+4*m+c+05000)+m
              if(x1.gt.0.08e0_knd) limn=2*((lplus)*(0.5e0_knd-
     1           3.0e0_knd*log10(x1))+10*ndec+10*incnee+4*m+c+01000)+m
              if(x1.gt.1.0e0_knd) limn=2*((lplus)*0.5e0_knd+5*ndec+
     1                                 10*incnee+4*m+c+00500)+m
              if(limn.gt.maxn-2) limn=maxn-2
              limp=limn-m
              if(jnen.eq.0) go to 510
              jnenlim=jnen
              if(jnen.gt.jnenmax) jnenlim=jnenmax
              limplim=limp
              limnlim=limn
                do 500 jn=1,jnenlim
                if(nee.ne.neeb(jn)) go to 500
                if(limplim.gt.limpsv(jn)) limplim=limpsv(jn)
                if(limnlim.gt.limnsv(jn)) limnlim=limnsv(jn)
                  do 460 je=1,limplim
                  if(je.le.limpd) pratb(je)=pratbsv(jn,je)
                  pratt(je)=prattsv(jn,je)
                  pdratt(je)=pdratsv(jn,je)
460               continue
                  do 470 je=1,limnlim
                  sneufe(je)=sneufsv(jn,je)
                  sneudfe(je)=sneudfsv(jn,je)
470               continue
                  jelim=maxlp
                  if(maxlp.gt.limn+1) jelim=limn+1
                  if(jelim.gt.jelimsv(jn)) jelim=jelimsv(jn)
                  do 480 je=1,jelim
                  sneune(je)=sneunsv(jn,je)
                  sneudre(je)=sneudrsv(jn,je)
                  ineuee(je)=ineuesv(jn,je)
480               continue
                write(40,490) etaval
490             format(8x,'r2eta: reused expansion functions for eta ='
     1                 ,f13.9,'.')
                go to 530
500             continue
510           continue
              jnen=jnen+1
              jnencur=jnen-(jnenmax*int((jnen-1)/jnenmax))
              neeb(jnencur)=nee
              call sphneu(c,xbninp,limn,maxn,maxlp,sneufe,sneune,
     1                    ineuee,sneudfe,sneudre)
                do je=1,limn
                sneufsv(jnencur,je)=sneufe(je)
                sneudfsv(jnencur,je)=sneudfe(je)
                limnsv(jnencur)=limn
                end do
              jelim=maxlp
              if(maxlp.gt.limn+1) jelim=limn+1
                do 520 je=1,jelim
                sneunsv(jnencur,je)=sneune(je)
                sneudrsv(jnencur,je)=sneudre(je)
                ineuesv(jnencur,je)=ineuee(je)
520             continue
              jelimsv(jnencur)=jelim
              iopd=3
              call pleg(m,limp,maxp,limcsav,iopd,ndec,nex,xlninp,
     1                  netainp,maxt,prat,pdrat,pdnorma,ipdnorma,pnorma,
     2                  ipnorma,alpha,beta,gamma,coefa,coefb,coefc,
     3                  coefd,coefe)
              limcsav=max(limcsav,limp)
                do je=1,limp
                pratt(je)=prat(1,je)
                pdratt(je)=pdrat(1,je)
                prattsv(jnencur,je)=pratt(je)
                pdratsv(jnencur,je)=pdratt(je)
                limpsv(jnencur)=limp
                end do
              limpd=2*(lnum+int(c)+ndec)
              if(limpd.gt.limp) limpd=limp
              iopd=2
              call pleg(m,limpd,maxp,limcsav,iopd,ndec,nex,etainp,
     1                  netainp,maxt,prat,pdrat,pdnorma,ipdnorma,pnorma,
     2                  ipnorma,alpha,beta,gamma,coefa,coefb,coefc,
     3                  coefd,coefe)
              iopd=3
                do je=1,limpd
                pratb(je)=prat(1,je)
                pratbsv(jnencur,je)=pratb(je)
                end do
              pratb(limpd+1)=0.0e0_knd
              pratb(limpd+2)=0.0e0_knd
              pratbsv(jnencur,limpd+1)=0.0e0_knd
              pratbsv(jnencur,limpd+2)=0.0e0_knd
530           continue
              pcoefe=((x1*(x1+2.0e0_knd))/(x1*(x1+2.0e0_knd)+
     1                eta(nee)**2))
              apcoef=(rm/2.0e0_knd)*log10(pcoefe)
              ipcoefe=int(apcoef)
              pcoefe=10.0e0_knd**(apcoef-ipcoefe)
              pcoefo=pcoefe*pratt(2)/pratb(2)
              ipcoefo=ipcoefe
              pdcoefe=pcoefe
              if(m.ne.0) pdcoefe=-pcoefe*rm*xln(nee)*xbn(nee)*xbn(nee)/
     1                           (x1*(x1+2.0e0_knd)*wmeta2(nee))
              ipdcoefe=ipcoefe
              pdcoefo=pdcoefe*pdratt(2)/pratb(2)
              ipdcoefo=ipdcoefe
              if(li.lt.3) go to 540
                do jl=3,li+ix,2
                pcoefe=pcoefe*pratt(jl)/pratb(jl)
                iterm=log10(abs(pcoefe))
                pcoefe=pcoefe*10.0e0_knd**(-iterm)
                ipcoefe=ipcoefe+iterm
                pdcoefe=pdcoefe*pdratt(jl)/pratb(jl)
                iterm=log10(abs(pdcoefe))
                pdcoefe=pdcoefe*10.0e0_knd**(-iterm)
                ipdcoefe=ipdcoefe+iterm
                end do
              continue
              if(li.lt.4) go to 540
                do jl=4,li+1-ix,2
                pcoefo=pcoefo*pratt(jl)/pratb(jl)
                iterm=log10(abs(pcoefo))
                pcoefo=pcoefo*10.0e0_knd**(-iterm)
                ipcoefo=ipcoefo+iterm
                pdcoefo=pdcoefo*pdratt(jl)/pratb(jl)
                iterm=log10(abs(pdcoefo))
                pdcoefo=pdcoefo*10.0e0_knd**(-iterm)
                ipdcoefo=ipdcoefo+iterm
                end do
540           continue
              if(ix.eq.0) go to 550
              pcoefet=pcoefo
              ipcoefet=ipcoefo
              pcoefo=pcoefo*pratt(li+2)/pratb(li+2)
              iterm=int(log10(abs(pcoefo)))
              pcoefo=pcoefo*10.0e0_knd**(-iterm)
              ipcoefo=ipcoefo+iterm
              pdcoefet=pdcoefo
              ipdcoefet=ipdcoefo
              pdcoefo=pdcoefo*pdratt(li+2)/pratb(li+2)
              iterm=int(log10(abs(pdcoefo)))
              pdcoefo=pdcoefo*10.0e0_knd**(-iterm)
              ipdcoefo=ipdcoefo+iterm
              go to 560
550           pcoefet=pcoefe
              ipcoefet=ipcoefe
              pcoefe=pcoefe*pratt(li+2)/pratb(li+2)
              iterm=int(log10(abs(pcoefe)))
              pcoefe=pcoefe*10.0e0_knd**(-iterm)
              ipcoefe=ipcoefe+iterm
              pdcoefet=pdcoefe
              ipdcoefet=ipdcoefe
              pdcoefe=pdcoefe*pdratt(li+2)/pratb(li+2)
              iterm=int(log10(abs(pdcoefe)))
              pdcoefe=pdcoefe*10.0e0_knd**(-iterm)
              ipdcoefe=ipdcoefe+iterm
560           continue
              lplus=max(l,1000)
              limeta=2*((lplus)*(-18.5e0_knd-20.0e0_knd*log10(x1))+
     1               5*ndec+4*m+c+05000)
              if(x1.gt.0.08e0_knd) limeta=2*((lplus)*(0.50e0_knd-
     1              3.0e0_knd*log10(x1))+5*ndec+4*m+c+01000)
              if(x1.gt.1.0e0_knd) limeta=2*((lplus)*0.5e0_knd+5*ndec+
     1                                   4*m+c+00500)
              if(iopeta.eq.3.and.naccrsav.gt.minacc)
     1                        limeta=jeta+jeta+500+c
              if(iopeta.eq.3.and.naccrsav.le.minacc)
     1                        limeta=jeta+jeta+500+c
              if(iopeta.eq.2) limeta=max(limeta,jeta+jeta+500+int(c))
              if(limeta.gt.limp-2) limeta=limp-2
              if(limeta.gt.limd) limeta=limd
              wm=wmeta2(nee)
              call r2eta(l,m,c,x1,etaval,nee,incnee,limeta,ndec,nex,
     1                   maxd,maxlp,maxn,maxp,minacc,wm,enr,sneufe,
     2                   sneune,ineuee,sneudfe,sneudre,pdratt,
     3                   pratb,pratt,pcoefet,ipcoefet,pdcoefet,
     4                   ipdcoefet,r1c,ir1e,r1dc,ir1de,naccetamax,naccr,
     5                   r2ec,ir2ee,r2dec,ir2dee,nacceta,nacciop,jeta,
     6                   iopnee,neemark,naccd,naccn,naccnmax)
              netatry=netatry+1
              naccetas=nacceta
              if(naccetas.eq.0.and.naccmax.eq.0.and.iopnee.eq.0
     1            .and.naccnmax.lt.3) neemax=nee
                if(naccetas.eq.naccmax.and.naccetas.gt.0) then
                kounte=kounte+1
                if(kounte.eq.1) nee1=nee
                if(kounte.eq.2) nee2=nee
                  if(kounte.gt.2) then
                  neemax=nee1
                  nee1=nee2
                  nee2=nee
                  end if
                end if
                if(naccetas.gt.naccmax) then
                naccmax=naccetas
                kounte=0
                neemax=nee
                end if
                if(naccetas.lt.naccmax) then
                kounte=0
                end if
              if(nacciop.eq.0) write(40,590)
590           format(15x,'r2eta accuracy is calculated using the',
     1               ' wronskian.')
              if(nacciop.eq.1) write(40,600)
600           format(15x,'r2eta accuracy = estimated numerator',
     1               ' accuracy minus sub. error in forming Wronskian.')
              iopeta=3
              if(naccetas.gt.nacctemp) nacctemp=naccetas
                if(naccetas.gt.naccr.or.(naccetas.eq.naccr.and.
     1               nacciop.eq.0)) then
                naccr=naccetas
                r2c=r2ec
                ir2e=ir2ee
                r2dc=r2dec
                ir2de=ir2dee
                nacce=nacciop
                end if
              if(iopint.ne.0.and.naccetas.gt.minacc.and.naccint.lt.
     1           6.and.naccintp.lt.6) iopint=0
              if(knd.eq.kindd) write(40,620) naccetas,etaval,nee,r2ec,
     1                                   ir2ee,r2dec,ir2dee
              if(knd.eq.kindq) write(40,630) naccetas,etaval,nee,r2ec,
     1                                    ir2ee,r2dec,ir2dee
620           format(15x,'accuracy in decimal digits = ',i2,5x,
     1              'eta = ',f12.9,'; nee = ',i4/,10x,
     2               'r2 = ',f17.14,i5,5x,'r2d = ',f17.14,i5)
630           format(15x,'accuracy in decimal digits = ',i2,5x,
     1              'eta = ',f12.9,'; nee = ',i4/,10x,
     2               'r2 = ',f34.31,i5,5x,'r2d = ',f34.31,i5)
              if(naccetas.gt.naccetamax.or.(naccetas.eq.0.and.naccetamax
     1           .eq.0.and.iopnee.eq.0)) neemax=nee
              if(naccetas.gt.naccetamax) naccetamax=naccetas
              if(naccetas.ge.minacc) ietacount=ietacount+1
              if(ietacount.ge.5) incnflag=1
                if(naccetas.ge.minacc.and.iplflag.eq.0) then
                nee=nee-incnee
                iopeta=2
                if(nee.lt.1) nee=1
                iplflag=1
                end if
                if(naccetas.ge.minacc) go to 660
              iopeta=2
              if(iplflag.eq.1.and.incnflag.eq.1.and.netatry.eq.2)
     1               iopnee=0
              ietacount=0
              if((naccd.ge.naccdp.or.naccd.ge.minacc.or.naccd.ge.naccrp
     1            .or.naccd.ge.naccr).and.(naccd+naccdp.gt.0)) iopnee=0 
              naccdp=naccd
              if(iopnee.eq.0) go to 650
              if(iopnee.eq.2) nee=neemax-incnee
              if(iopnee.eq.1) nee=max(neemark,neemax)-incnee
              if(nee.lt.1) nee=1
              if(iopnee.eq.2.and.naccetas.lt.lowtest-1) go to 640
              incnee=8
                if(knd.eq.kindd) then
                if(x1.ge.0.01e0_knd) incnee=16
                end if
                if(knd.eq.kindq) then
                if(x1.ge.0.05e0_knd) incnee=16
                if(x1.ge.0.1e0_knd) incnee=32
                end if
640           if(nacctemp.ge.lowtest-1) msearch=1
              if(msearch.eq.0) iopeta=0
              if(msearch.eq.0) lowtest=lowtest-1
              go to 665
650           if(nee.eq.neta) go to 660
              nee=nee+incnee
              if(nee.gt.neta) nee=neta
              if(msearch.ne.0) kounter=0
              go to 440
660           continue
              if(naccetas.lt.minacc.and.nee.eq.neta)
     1               nee=nee-incnee
              if(naccetas.lt.minacc) iopeta=2
              if(nee.ne.neta) msearch=1
              if(naccetas.ge.minacc) kounter=kounter+1
              if(kounter.ge.(2*incnee).and.msearch.ne.0)
     1               incnee=2*incnee
              if(incnee.gt.64) incnee=64
                if(knd.eq.kindd) then
                if(x1.le.0.2e0_knd.and.incnee.gt.32) incnee=32
                if(x1.le.0.15e0_knd.and.incnee.gt.16) incnee=16
                end if
                if(knd.eq.kindq) then
                if(x1.le.0.1e0_knd.and.incnee.gt.32) incnee=32
                end if
              if(iopint.ne.0.and.naccetas.lt.lowacc) iopeta=0
              if(iopeta.eq.0) nacctest=naccetas
              if(naccetas.lt.minacc) iplflag=0
              naccetap=naccetamax
665           if(naccetas.lt.minacc.and.iflagnee.eq.0) iflagnee=1
              if(nee.lt.neest) nee=neest
670           if(naccr.gt.0) go to 680
              naccr=0
              r2c=0.0e0_knd
              ir2e=0
              r2dc=0.0e0_knd
              ir2de=0
680           continue
              if(ioprad.eq.2.and.nacce.ne.1) write(20,690)l,r1c,ir1e,
     1               r1dc,ir1de,r2c,ir2e,r2dc,ir2de,naccr
690           format(1x,i6,2x,4(f17.14,1x,i6,2x),i2,'w')
              if(ioprad.eq.2.and.nacce.eq.1) write(20,700)l,r1c,ir1e,
     1               r1dc,ir1de,r2c,ir2e,r2dc,ir2de,naccr
700           format(1x,i6,2x,4(f17.14,1x,i6,2x),i2,'e')
              if(ioprad.eq.1) write(20,710) l,r1c,ir1e,r1dc,ir1de
710           format(1x,i6,2x,2(f17.14,1x,i6,2x))
              if(ioprad.ne.2) go to 720
              if(lowacc.gt.naccr) lowacc=naccr
              if(iopint.ne.0) naccintp=naccint
                if(istartr2.eq.1) then
                if(ndec-jsub.gt.naccr.and.x1.le.0.4e0_knd.and.
     1             iopleg.eq.0.and.l.ge.legstart) iopleg=1
                if(ndec-jsub.gt.naccr.and.x1.ge.0.00065e0_knd.and.
     1             iopneu.eq.0.and.iopleg.ne.2) iopneu=1
                if(iopeta.eq.0.and.x1.ge.0.00065e0_knd.and.iopneu.eq.0
     1             .and.iopleg.ne.2) iopeta=1
                end if
              if(iopeta.eq.4) iopeta=1
              if((naccr.lt.legtest.or.naccrp.lt.legtest).and.
     1            x1.le.0.01e0_knd.and.l.gt.legstart.and.iopleg.eq.0)
     2            iopleg=1
              if(iopneu.eq.0.and.naccr.lt.minacc.and.naccrp.lt.minacc
     1           .and.ndec-jsub.gt.naccr.and.ndec-jsub.gt.naccrp.and.
     2           x1.ge.0.00065e0_knd) iopneu=1
              naccrp=naccr
                if(ioprad.eq.2.and.naccr.lt.6) then
                write(60,*) ' est. acc. = ',naccr, ' digits for x = ',
     1                    x,' c = ', c,' m = ',m,' l = ',l
                end if   
720           if(iopang.eq.0) go to 850
c
c  determine first kind prolate angular function
              if(l.eq.m) lims1=3*ndec+int(c)
              if(l.ne.m) lims1=jang+jang+20+int(sqrt(c))
              if(lims1.gt.maxp) lims1=maxp
              call s1leg(l,m,c,iopang,iopnorm,barg,narg,lims1,ndec,maxt,
     1                   maxd,maxp,enr,pr,pdr,pdnorm,ipdnorm,pnorm,
     2                   ipnorm,pdtempe,ipdtempe,pdtempo,ipdtempo,
     3                   ptempe,iptempe,ptempo,iptempo,dmlms,idmlmse,
     4                   s1c,is1e,s1dc,is1de,dmlms1,idmlms1e,naccs,jang)
                do 810 jarg=1,narg
                if(knd.eq.kindd.and.ioparg.eq.0) write(50,730)
     1              arg(jarg),naccs(jarg)
                if(knd.eq.kindq.and.ioparg.eq.0) write(50,735)
     1              arg(jarg),naccs(jarg)
                if(knd.eq.kindd.and.ioparg.eq.1) write(50,740)
     1              barg(jarg),naccs(jarg)
                if(knd.eq.kindq.and.ioparg.eq.1) write(50,745)
     1              barg(jarg),naccs(jarg)
730             format(1x,'theta = ',e21.14,'  accuracy = ',i5,
     1                 ' digits.')
735             format(1x,'theta = ',e38.31,'  accuracy = ',i5,
     1                 ' digits.')
740             format(1x,'eta = ',e21.14,'  accuracy = ',i5,
     1                 ' digits.')
745             format(1x,'eta = ',e38.31,'  accuracy = ',i5,
     1                 ' digits.')
                if(ioparg.eq.0.and.iopang.eq.1) write(30,750) arg(jarg),
     1                s1c(jarg),is1e(jarg),naccs(jarg)
                if(ioparg.eq.0.and.iopang.eq.2) write(30,760) arg(jarg),
     1           s1c(jarg),is1e(jarg),s1dc(jarg),is1de(jarg),naccs(jarg)
                if(ioparg.eq.1.and.iopang.eq.1) write(30,750)
     1                barg(jarg),s1c(jarg),is1e(jarg),naccs(jarg)
                if(ioparg.eq.1.and.iopang.eq.2) write(30,760)
     1           barg(jarg),s1c(jarg),is1e(jarg),s1dc(jarg),is1de(jarg),
     2           naccs(jarg)
                if(knd.eq.kindd.and.iopang.eq.1) write(50,770)
     1                s1c(jarg),is1e(jarg)
                if(knd.eq.kindd.and.iopang.eq.2) write(50,780)
     1                s1c(jarg),is1e(jarg),s1dc(jarg),is1de(jarg)
                if(knd.eq.kindq.and.iopang.eq.1) write(50,790)
     1                s1c(jarg),is1e(jarg)
                if(knd.eq.kindq.and.iopang.eq.2) write(50,800)
     1                s1c(jarg),is1e(jarg),s1dc(jarg),is1de(jarg)
750             format(1x,f19.14,2x,f17.14,2x,i5,2x,', ',i2)
760             format(1x,f19.14,2x,f17.14,2x,i5,2x,f17.14,2x,i5,2x,i2)
770             format(12x,'s1 = ',f17.14,2x,i5)
780             format(12x,'s1 = ',f17.14,2x,i5,5x,'s1d = ',f17.14,
     1                 2x,i5)
790             format(12x,'s1 = ',f34.31,2x,i5)
800             format(12x,'s1 = ',f34.31,2x,i5,/,10x,'s1d = ',f34.31,
     1                 2x,i5)
810             continue
850           continue
900         continue
          return
          end
c
c
        subroutine s1leg (l,m,c,iopang,iopnorm,barg,narg,lims1,ndec,
     1                    maxt,maxd,maxp,enr,pr,pdr,pdnorm,ipdnorm,
     2                    pnorm,ipnorm,pdtempe,ipdtempe,pdtempo,
     3                    ipdtempo,ptempe,iptempe,ptempo,iptempo,
     4                    dmlms,idmlmse,s1c,is1e,s1dc,is1de,dmlms1,
     5                    idmlms1e,naccs,jang)
c
c  purpose:     To calculate the prolate angular functions of the first
c               kind and their first derivatives with respect to eta.
c
c
c  parameters:
c
c     input :   l       : l
c               m       : m
c               c       : c
c               iopang  : index = 1 when angular functions of the
c                         first kind are calculated; = 2 when the
c                         first derivatives with respect to eta are
c                         also calculated
c               iopnorm : = 1 when the angular functions (and
c                         first derivatives) are scaled by the
c                         square root of the normalization of the
c                         corresponding Legendre function, giving them
c                         unity norm; iopnorm = 0 otherwise
c               barg    : array of eta values for which angular
c                         functions are desired
c               narg    : number of eta values
c               lims1   : approximately twice the maximum number
c                         of terms available to be taken in the
c                         sums
c               ndec    : number of decimal digits for real(knd)
c               maxt    : dimension of barg, pdnorm, ipdnorm, pnorm,
c                         ipnorm, pdtempe, ipdtempe, pdtempo, ipdtempo,
c                         ptempe, iptempe, ptempo, iptempo, s1c, is1e,
c                         s1dc, is1de, and naccs arrays. first dimension
c                         of the doubly dimensioned arrays pr and pdr
c               maxd    : dimension of enr array
c               maxp    : second dimension of pr and pdr arrays
c               enr     : array of d coefficient ratios
c               pr      : array of ratios of successive first kind
c                         associated Legendre functions of the same
c                         parity
c               pdr     : array of ratios of successive derivatives of
c                         first kind associated Legendre functions of
c                         the same parity
c               pdnorm  : array of characteristics of the first
c                         derivatives of associated Legendre functions
c                         of the first kind of order m and degree m
c               ipdnorm : array of exponents corresponding to pdnorm
c               pnorm   : array of characteristics of the associated
c                         Legendre functions of the first kind of order
c                         m and degree m
c               ipnorm  : array of exponents corresponding to pnorm
c               pdtempe : storage array of characteristics of the ratio
c                         of the first derivative of the associated
c                         Legendre function of order m and degree l - 2
c                         or l - 1, depending on whether l - m is even
c                         or odd, to the first derivative of the
c                         function of order m and degree m
c               ipdtempe: array of exponents corresponding to pdtempe
c               pdtempo : storage array of characteristics of the ratio
c                         of the first derivative of the associated
c                         Legendre function of order m and degree l - 2
c                         or l - 1, depending on whether l - m is odd
c                         or even, to the first derivtive of the
c                         function of order m and degree m
c               ipdtempo: array of exponents corresponding to pdtempo
c               ptempe  : storage array of characteristics of the ratio
c                         of the associated Legendre function of order
c                         m and degree l - 2 or l - 1, depending on
c                         whether l - m is even or odd, to the function
c                         of order m and degree m
c               iptempe : array of exponents corresponding to ptempe
c               ptempo  : storage array of characteristics of the ratio
c                         of the associated Legendre function of order
c                         m and degree l - 2 or l - 1, depending on
c                         whether l - m is odd or even, to the
c                         function of order m and degree m
c               iptempo : array of exponents corresponding to ptempo
c               dmlms   : characteristic of d coefficient with subscript
c                         l - m using the Meixner and Schafke
c                         normalization
c               idmlmse : exponent associated with dmlms
c
c
c     output:   s1c    : array of characteristics of prolate
c                        angular functions of the first kind
c               is1e   : array of exponents of prolate angular
c                        functions of the first kind
c               s1dc   : array of characteristics of derivative with
c                        respect to eta of prolate angular functions
c                        of the first kind
c               is1de  : array of exponents of derivatives with respect
c                        to eta of prolate angular functions of first
c                        kind
c               dmlms1 : characteristic of the d coefficient with
c                        subscript l - m for unit normalization of
c                        the angular functions
c               idmlms1e: exponent associated with dmlms1
c               naccs  : array of integer estimates of the number of
c                        accurate decimal digits in the values obtained
c                        for s1 and s1d
c               jang   : maximum value of the index j in the forward
c                        sum for r1 and r1d, i.e., the highest enr(j)
c                        used
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) adec,aj,c,dcon,dec,dnew,dmlms,dmlms1,dnewd,dold,
     1            doldd,factor,fterm,rm2,rm2m1,rm2m3,rm2p1,s1,s1d
        real(knd) barg(maxt),enr(maxd),pdr(maxt,maxp),pdnorm(maxt),
     1            pnorm(maxt),pr(maxt,maxp),pdtemp(maxt),ptemp(maxt),
     2            pdtempe(maxt),ptempe(maxt),pdtempo(maxt),ptempo(maxt),
     3            s1c(maxt),s1dc(maxt)
c
c  integer arrays
        integer ipdnorm(maxt),ipnorm(maxt),ipdtemp(maxt),iptemp(maxt),
     1          ipdtempe(maxt),iptempe(maxt),ipdtempo(maxt),
     2          iptempo(maxt),is1de(maxt),is1e(maxt),naccs(maxt)
c
        dec=10.0e0_knd**(-ndec-1)
        dcon=dec
        adec=1000.0e0_knd*dec
        rm2=m+m
        rm2m1=m+m-1
        rm2p1=m+m+1
        rm2m3=m+m-3
        if(l.gt.(m+1)) go to 30
          do 20 k=1,narg
          if(pnorm(k).eq.0.0e0_knd) go to 20
          if(l.eq.(m+1)) go to 10
          ptempe(k)=pr(k,1)
          iptempe(k)=0
          pdtempe(k)=pdr(k,1)
          ipdtempe(k)=0
          ptemp(k)=ptempe(k)
          pdtemp(k)=pdtempe(k)
          iptemp(k)=0
          ipdtemp(k)=0
          go to 20
10        ptempo(k)=pr(k,2)
          iptempo(k)=0
          pdtempo(k)=pdr(k,2)
          ipdtempo(k)=0
          ptemp(k)=ptempo(k)
          pdtemp(k)=pdtempo(k)
          iptemp(k)=0
          ipdtemp(k)=0
20        continue
30      continue
        lm2=(l-m)/2
        ix=l-m-2*lm2
        ixx=ix-1
        ixx2=ixx+2
        if(l.lt.(m+2)) go to 110
          do 100 k=1,narg
          if(pnorm(k).eq.0.0e0_knd) go to 100
          if(ix.ne.0) go to 60
          ptempe(k)=ptempe(k)*pr(k,l-m+1)
          if(abs(ptempe(k)).lt.1.0e+10_knd) go to 40
          ptempe(k)=ptempe(k)*(1.0e-10_knd)
          iptempe(k)=iptempe(k)+10
40        ptemp(k)=ptempe(k)
          iptemp(k)=iptempe(k)
          if(abs(barg(k)).lt.adec) go to 100
          pdtempe(k)=pdtempe(k)*pdr(k,l-m+1)
          if(abs(pdtempe(k)).lt.1.0e+10_knd) go to 50
          pdtempe(k)=pdtempe(k)*(1.0e-10_knd)
          ipdtempe(k)=ipdtempe(k)+10
50        pdtemp(k)=pdtempe(k)
          ipdtemp(k)=ipdtempe(k)
          go to 100
60        if(abs(barg(k)).lt.adec) go to 80
          ptempo(k)=ptempo(k)*pr(k,l-m+1)
          if(abs(ptempo(k)).lt.1.0e+10_knd) go to 70
          ptempo(k)=ptempo(k)*(1.0e-10_knd)
          iptempo(k)=iptempo(k)+10
70        ptemp(k)=ptempo(k)
          iptemp(k)=iptempo(k)
80        pdtempo(k)=pdtempo(k)*pdr(k,l-m+1)
          if(abs(pdtempo(k)).lt.1.0e+10_knd) go to 90
          pdtempo(k)=pdtempo(k)*(1.0e-10_knd)
          ipdtempo(k)=ipdtempo(k)+10
90        pdtemp(k)=pdtempo(k)
          ipdtemp(k)=ipdtempo(k)
100        continue
110     continue
        lim=lims1/2-ix
        jlow=lm2+1
        jang=0
c
c  compute the associated Legendre function normalization factor
        factor=1.0e0_knd
        ifactor=0
        if(iopnorm.eq.0) go to 210
        if(m.eq.0) go to 170
          do 160 j=1,m
          aj=j
          factor=factor*(aj+aj)*(aj+aj-1.0e0_knd)
          if(factor.lt.1.0e100_knd) go to 160
          factor=factor*1.0e-100_knd
          ifactor=ifactor+100
160       continue
170     if(l.eq.m) go to 190
          do 180 j=1,l-m
          aj=j
          factor=factor*(rm2+aj)/(aj)
          if(factor.lt.1.0e100_knd) go to 180
          factor=factor*1.0e-100_knd
          ifactor=ifactor+100
180       continue
190     factor=factor*2.0e0_knd/(l+l+1.0e0_knd)
        factor=sqrt(factor)
        ifactor=ifactor/2
        iterm=int(log10(factor))
        factor=factor*(10.0e0_knd**(-iterm))
        ifactor=ifactor+iterm
        dmlms1=dmlms/factor
        idmlms1e=idmlmse-ifactor
        write(50,200)
200     format(5x,'s1 is normalized to have unit norm.')
210     continue
        if(iopnorm.eq.0) write(50,215)
215     format(5x,'s1 has the same normalization as the',
     1         ' corresponding Legendre function.')
c
c  compute the angular function s1
          do 380 k=1,narg
          if(pnorm(k).eq.0.0e0_knd) go to 220
          if((ix.eq.1).and.(abs(barg(k)).lt.adec)) go to 220
          if(((abs(abs(barg(k))-1.0e0_knd)).lt.adec).
     1         and.(m.ne.0)) go to 220
          go to 230
220       s1c(k)=0.0e0_knd
          is1e(k)=0
          naccs(k)=ndec
          go to 300
230       dold=1.0e0_knd
          s1=dold
          fterm=1.0e0_knd
            do 240 j=jlow,lim
            dnew=dold*enr(j)*pr(k,j+j+ixx2)
            s1=s1+dnew
            if(abs(dnew).gt.fterm) fterm=abs(dnew)
            if(abs(dnew/s1).lt.dcon) go to 250
            dold=dnew
240         continue
250       if(j.gt.jang) jang=j
          write(50,260) barg(k),j
260       format(8x,'s1 calculation for eta = ',f13.8,' converged in ',
     1           i6,' terms.')
          if(lm2.lt.1) go to 280
          dold=1.0e0_knd
          j=lm2
            do 270 jj=1,lm2
            dnew=dold/(pr(k,j+j+ixx2)*enr(j))
            s1=s1+dnew
            if(abs(dnew).gt.fterm) fterm=abs(dnew)
            if(abs(dnew/s1).lt.dcon) go to 280
            dold=dnew
            j=j-1
270         continue
280       s1c(k)=s1*dmlms*ptemp(k)*pnorm(k)/factor
          if(s1c(k).ne.0.0e0_knd) iterm=int(log10(abs(s1c(k))))
          if(s1c(k).eq.0.0e0_knd) iterm=0
          s1c(k)=s1c(k)*(10.0e0_knd**(-iterm))
          is1e(k)=iptemp(k)+ipnorm(k)+iterm+idmlmse-ifactor
          if(abs(s1c(k)).ge.1.0e0_knd) go to 290
          s1c(k)=s1c(k)*10.0e0_knd
          is1e(k)=is1e(k)-1
          if(s1c(k).eq.0.0e0_knd) naccs(k)=0
290       if(s1c(k).ne.0.0e0_knd) naccs(k)=ndec-2-
     1                            log10(abs((fterm)/(s1)))
          if(naccs(k).gt.0) go to 300
          naccs(k)=0
          s1c(k)=0.0e0_knd
          is1e(k)=0
          s1dc(k)=0.0e0_knd
          is1de(k)=0
          go to 380
c
c       compute the first derivative of the anguar function when
c       iopang equals 2
300       if(iopang.ne.2) go to 380
          if(pnorm(k).eq.0.0e0_knd) go to 310
          if((ix.eq.0).and.(abs(barg(k)).lt.adec)) go to 310
          if(((abs(abs(barg(k))-1.0e0_knd)).lt.adec).and.(m.ne.0)
     1        .and.(m.ne.2)) go to 310
          go to 320
310       s1dc(k)=0.0e0_knd
          is1de(k)=0
          go to 370
320       doldd=1.0e0_knd
          s1d=doldd
          if(l.eq.0) s1d=0.0e0_knd
            do 330 j=jlow,lim
            dnewd=doldd*enr(j)*pdr(k,j+j+ixx2)
            s1d=s1d+dnewd
            if(abs(dnewd/s1d).lt.dcon) go to 340
            doldd=dnewd
330         continue
340       if(lm2.lt.1) go to 360
          doldd=1.0e0_knd
          j=lm2
          ja=lm2
          if(m.eq.0.and.ix.eq.0) ja=lm2-1
          if(ja.eq.0) go to 360
            do 350 jj=1,ja
            dnewd=doldd/(pdr(k,j+j+ixx2)*enr(j))
            s1d=s1d+dnewd
            if(abs(dnewd/s1d).lt.dcon) go to 360
            doldd=dnewd
            j=j-1
350         continue
360       s1dc(k)=s1d*dmlms*pdtemp(k)*pdnorm(k)/factor
          if(s1dc(k).ne.0.0e0_knd) iterm=int(log10(abs(s1dc(k))))
          if(s1dc(k).eq.0.0e0_knd) iterm=0
          s1dc(k)=s1dc(k)*10.0e0_knd**(-iterm)
          is1de(k)=ipdtemp(k)+ipdnorm(k)+iterm+idmlmse-ifactor
          if(abs(s1dc(k)).ge.1.0e0_knd) go to 370
          s1dc(k)=s1dc(k)*10.0e0_knd
          is1de(k)=is1de(k)-1
370       continue
380       continue
        return
        end
c
c
        subroutine r1bes (l,m,c,x1,limr1,ndec,maxd,enr,maxj,maxlp,
     1                    nex,iflag,sbesf,sbesdf,sbesn,ibese,sbesdr,
     2                    d01,id01,r1c,ir1e,r1dc,ir1de,dfnorm,jbes,
     3                    factor)
c
c  purpose:     To calculate the prolate radial function of the
c               first kind and its first derivative with respect
c               to x, using an expansion of spherical Bessel
c               functions of the first kind with argument
c               c*sqrt(x*x-1).
c
c  parameters:
c
c     input:    l      : l
c               m      : m
c               c      : c
c               x1     : x-1
c               limr1  : approximately twice the maximum number of
c                        terms available to be taken in the series
c               ndec   : number of decimal digits for real(knd)
c               maxd   : dimension of enr array
c               enr    : d coefficient ratios
c               maxj   : dimension of sbesf and sbesdf arrays
c               maxlp  : maximum  l value desired; dimension
c                        of the sbesdr, sbesn, and ibese arrays
c               nex    : maximum exponent available in real(knd)
c                        arithmetic
c               iflag  : integer = 1 if forward series not needed;
c                        =0 if the forward series is computed
c               sbesf  : array of ratios of successive first kind
c                        spherical Bessel functions of the same parity
c               sbesdf : array of ratios of successive derivatives of
c                        first kind spherical Bessel functions of the
c                        same parity
c               sbesn  : array of characteristics for Bessel functions
c               ibese  : array of exponents corresponding to sbesn
c               sbesdr : value of ratio of first derivative of
c                        spherical Bessel function to the corresponding
c                        Bessel function
c               d01    : characteristic of ratio of first d coefficient
c                        to the d coefficient for n = l-m
c               id01   : exponent associated with d01
c
c     output  : r1c    : characteristic of prolate radial function
c                        of the first kind
c               ir1e   : exponent of prolate radial function of the
c                        first kind
c               r1dc   : characteristic of derivative with respect
c                        to x of prolate radial function of the first
c                        kind
c               ir1de  : exponent of derivative with respect to x of
c                        prolate radial function of the first kind
c               dfnorm : Flammer normalization factor of the
c                        d coefficients. equal to the reciprocal of
c                        the value of the d coefficient d(n = l - m)
c                        using this normalization for the angular
c                        functions
c               jbes   : maximum value of the index j in the forward
c                        sum for r1 and r1d, i.e., the highest enr(j)
c                        used
c               factor : coefficient used in alternative expression for
c                        contribution to r1d from the first order
c                        Bessel function and its first derivative.
c                        Used to avoid subtraction error that occurs
c                        for m = 0 when x1 is very small, especially
c                        for values of l. Value of the factor for a
c                        given l is used to obtain the value for the
c                        next l.
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) c,con,dec,dfnorm,dnew,dnewd,dold,doldd,d01,factor,
     1            r1bot,r1c,r1d,r1dc,r1dstore,r1temp,r1top,r1dtop,
     2            r1topd,r1dtopd,rj1,rj2,term,termd,teste,testeo,x,x1
        real(knd) enr(maxd),sbesdf(maxj),sbesdr(maxlp),sbesf(maxj),
     1            sbesn(maxlp)
c
c  integer array
        dimension ibese(maxlp)
c
c  convergence ratio dec is set according to the requested accuracy
        dec=10.0e0_knd**(-ndec-1)
        lm2=(l-m)/2
c
c  ix=0 for l-m even, ix=1 for l-m odd
        ix=l-m-2*lm2
        lim=limr1/2-ix
        x=x1+1.0e0_knd
        con=x/sqrt(x1*(x1+2.0e0_knd))
        nfac=nex/3
        teste=10.0e0_knd**nfac
        testeo=1.0e0_knd/teste
        ir1tope=0
        mml=m+m-1+ix
        iflagd=0
        if(x1.lt.0.1e0_knd.and.ix.eq.1.and.m.eq.0) iflagd=1
        if(iflagd.eq.1.and.l.ne.1) factor=factor*real(l,knd)/(l-1)
c
c
c  compute radial function of the first kind r1 and its first
c  derivative r1d
c
c  forward summation of numerator series for both r1 and r1d
        dold=1.0e0_knd
        doldd=1.0e0_knd
        r1top=dold
        r1dtop=doldd
        if(iflag.eq.1) write(40,10) lm2
10      format(8x,'r1bes: numerator forward series not used; backward ',
     1         'series has 'i5,' terms.')
        jtop=lm2
        if(iflag.eq.1) go to 50
          do 20 j=lm2+1,lim
          jj=j+j+ix
          rj1=jj-ix
          rj2=jj+mml
          dnew=dold*enr(j)*sbesf(jj+m)*rj2/rj1
          dnewd=doldd*enr(j)*sbesdf(jj+m)*rj2/rj1
          r1top=r1top+dnew
          r1dtop=r1dtop+dnewd
          if((abs(dnew/r1top)+abs(dnewd/r1dtop)).lt.dec) go to 30
          dold=dnew
          doldd=dnewd
20        continue
30      continue
        jtop=min(lim,j)
        write(40,40) jtop,lim
40      format(8x,'r1bes: numerator series converged in ',i6,' terms; ',
     1         i6,' available.' )
        if(iflagd.eq.0.or.l.ne.1) go to 45
        r1topd=r1top-1.0e0_knd
        r1dtopd=r1dtop-1.0e0_knd
45      continue
50        continue
c
c  backward summation of numerator series for r1 and r1d
        if (lm2.lt.1) go to 80
        dold=1.0e0_knd
        doldd=1.0e0_knd
          do 70 j=lm2,1,-1
          jj=j+j+ix
          rj1=jj-ix
          rj2=jj+mml
          dnew=dold*rj1/(sbesf(jj+m)*rj2*enr(j))
          dnewd=doldd*rj1/(sbesdf(jj+m)*rj2*enr(j))
          r1top=r1top+dnew
          r1dtop=r1dtop+dnewd
          if((abs(dnew/r1top)+abs(dnewd/r1dtop)).lt.dec) go to 75
          if(abs(r1top).lt.teste) go to 60
          r1top=r1top*testeo
          dnew=dnew*testeo
          ir1tope=ir1tope+nfac
          r1dtop=r1dtop*testeo
          dnewd=dnewd*testeo
60        dold=dnew
          doldd=dnewd
70        continue
          if(jj.ne.3) iflagd=0
          if(iflagd.eq.0) go to 80
          r1topd=r1top-dnew
          r1dtopd=r1dtop-dnewd
          go to 80
75        continue
          iflagd=0
80        continue
c
c  forward summation of denominator series for r1 and r1d
c  the denominator series is the Flammer normalization constant dfnorm
        dold=1.0e0_knd
        r1bot=dold
          do 90 j=lm2+1,lim
          jj=j+j+ix
          dnew=-dold*enr(j)*real((jj+mml),knd)/real(jj-ix,knd)
          r1bot=r1bot+dnew
          if(abs(dnew/r1bot).lt.dec) go to 100
          dold=dnew
90        continue
100     continue
        jbot=min(j,lim)
        jbes=max(jtop,jbot)
        write(40,110) jbot,lim
110     format(15x,'denominator series converged in ',i6,' terms; ',
     1         i6,' available.')
c
c  backward summation of denominator series for both r1 and r1d
        if(lm2.lt.1) go to 130
        dold=1.0e0_knd
          do 120 j=lm2,1,-1
          jj=j+j+ix
          dnew=-dold*(jj-ix)/(real((jj+mml),knd)
     1         *enr(j))
          r1bot=r1bot+dnew
          if(abs(dnew/r1bot).lt.dec) go to 130
          dold=dnew
120       continue
130     dfnorm=r1bot
c
c  compute r1 and r1d
        r1temp=r1top*sbesn(l+1)/r1bot
        if(ix.eq.1) r1temp=r1temp*con
        iterm=int(log10(abs(r1temp)))
        ir1e=ir1tope+ibese(l+1)+iterm
        r1c=r1temp*(10.0e0_knd**(-iterm))
        if(abs(r1c).ge.1.0e0_knd) go to 140
        r1c=r1c*10.0e0_knd
        ir1e=ir1e-1
140     if(iflagd.eq.1) r1temp=r1temp*r1topd/r1top
        if(iflagd.eq.1) r1dtop=r1dtopd
        r1d=r1dtop*sbesn(l+1)*c*con*sbesdr(l+1)/r1bot
        r1dstore=r1d*con
        ndsub=0
        if(ix.eq.1) r1d=r1d*con-r1temp/(x1*x*(x1+2.0e0_knd))
        if(ix.eq.1) ndsub=-int(log10(abs(r1d/r1dstore)))
        if(ndsub.lt.0) ndsub=0
        if(iflagd.eq.0) go to 150
        term=x1*(x1+2.0e0_knd)*sbesdr(2)*sbesn(2)
        termd=term-sbesn(3)*(10.0e0_knd**(ibese(3)-ibese(2)))
        ndsub1=-log10(abs(termd/term))
        if(ndsub1.lt.0) ndsub1=0
        termd=termd*(c*d01/(x1*(x1+2.0e0_knd)*r1bot*factor))*
     1         (10.0e0_knd**(id01+ibese(2)-ibese(l+1)-ir1tope))
        r1d=r1d+termd
        ndsub2=-log10(abs(r1d/termd))
        if(ndsub2.lt.0) ndsub2=0
        ndsub=ndsub+ndsub1+ndsub2
150     continue        
        if(ix.eq.1.and.ndsub.gt.0) write(40,160) ndsub
160     format(24x,'subtraction error in forming r1d =',i3,' digits.')
        iterm=log10(abs(r1d))
        ir1de=ir1tope+ibese(l+1)+iterm
        r1dc=r1d*(10.0e0_knd**(-iterm))
        if(abs(r1dc).ge.1.0e0_knd) go to 170
        r1dc=r1dc*10.0e0_knd
        ir1de=ir1de-1
170     continue
        mfac=ir1e-ibese(l+1)
        if(mfac.gt.(ndec+5)) iflag=1
        if(mfac.le.(ndec+5)) iflag=0
        return
        end
c
c
        subroutine r2int (l,m,c,x,limint,ndec,nex,maxd,enr,d01,id01,
     1                    maxint,maxmp,maxlp,rpint1,rpint2,
     2                    pint1,pint2,pint3,pint4,norme,pnorm,ipnorm,
     3                    coefme,coefmo,r2c,ir2e,r2dc,ir2de,jint,
     4                    coefn,icoefn)
c
c
c  purpose:     To calculate values of the radial function of the
c               second kind and its first derivative using an integral
c               representation of the radial functions in terms of the
c               angular function of the first kind together with a
c               Neumann function kernal. The angular function is
c               expanded in a series of associated Legendre functions.
c               Gaussian quadrature is used (in subroutine pint) to
c               evaluate the resulting integrals involving associated
c               Legendre functions times the Neumann function kernel.
c               This subroutine performs the summation of the
c               integrals times d coefficients to obtain r2 and r2d.
c
c  parameters:
c
c     input:    l      : l
c               m      : m
c               c      : c
c               x      : x
c               limint : approximately twice the maximum number of
c                        terms available to be taken in the series
c               ndec   : number of decimal digits for real(knd)
c               nex    : maximum exponent for real(knd)
c               maxd   : dimension of enr array
c               enr    : d coefficient ratios
c               d01    : characteristic of the first d coefficient,
c                        either d0 or d1, depending on whether l-m
c                        is even or odd
c               id01   : exponent (base 10) of the first d coefficient
c               maxint : dimension of pint and rpint arrays
c               maxmp  : dimension of norme array
c               maxlp  : maximum  l value desired; dimension
c                        of the pnorm and ipnorm arrays
c               rpint1 : arrays of ratios of successive integrals of
c                        either the first or the third kind, depending
c                        on whether l-m is even or odd
c               rpint2 : array of ratios of successive integrals of
c                        either the second or the fourth kind,
c                        depending on whether l-m is even or odd
c               pint1  : array of scaled values for the integrals of
c                        the first kind
c               pint2  : array of scaled values for the integrals of
c                        the second kind
c               pint3  : array of scaled values for the integrals of
c                        the third kind
c               pint4  : array of scaled values for the integrals of
c                        the fourth kind
c               norme  : array of exponents used to scale the Neumann
c                        function of order m involved in the integrals
c               pnorm  : array of characteristics of the scaling factors
c                        used for the associated Legendre functions in
c                        the integrals to avoid overflow
c               ipnorm : array of exponents (base 10) corresponding to
c                        pnorm
c               coefme : coefficient used to multiply the resulting
c                        sum to obtain r2 when l-m is even
c               coefmo : coefficient used to multiply the resulting
c                        sum to obtain r2 when l-m is odd
c
c     output:   r2c    : characteristic of prolate radial function
c                        of the second kind
c               ir2e   : exponent of prolate radial function of the
c                        second kind
c               r2dc   : characteristic of derivative with respect
c                        to x of prolate radial function of the second
c                        kind
c               ir2de  : exponent of derivative with respect to x of
c                        prolate radial function of the second kind
c               jint   : maximum value of the index j in the forward
c                        sum for r2 and r2d, i.e., the highest enr(j)
c                        used
c               coefn  : characteristic of coefficient that is only
c                        calculated once (for l = m) and is then
c                        used for all values of l
c               icoefn : exponent for coefn
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) c,coefa,coefl,coefme,coefmo,coefn,dec,dcon,dnew,
     1            dnewd,dold,doldd,d01,ri,rm,rm2,r2c,r2dc,r2dpos,
     2            r2dtemp,r2pos,r2temp,x
        real(knd) enr(maxd),pnorm(maxlp),pint1(maxint),pint2(maxint),
     1            pint3(maxint),pint4(maxint),rpint1(maxint),
     2            rpint2(maxint)
c
c  integer arrays
        dimension norme(maxmp),ipnorm(maxlp)
c
        rm=m
        rm2=rm+rm
        lm2=(l-m)/2
        ix=l-m-2*lm2
        ixx=ix-1
        ixx2=ixx+2
        lim=limint/2-ix
c
c  compute the leading coefficient
        if(l.gt.m) go to 20
        icoefn=norme(m+1)
        coefn=0.5e0_knd
        if(m.eq.0) go to 20
          do 10 i=1,m
          ri=i
          coefn=coefn/(ri+ri)
          iterm=int(log10(abs(coefn)))
          coefn=coefn*10.0e0_knd**(-iterm)
          icoefn=icoefn+iterm
10        continue
20      continue
        if(ix.eq.0) coefa=(rm2+1.0e0_knd)*coefn
        if(ix.eq.1) coefa=(rm2+3.0e0_knd)*coefn
        if((ix.eq.0).and.(2*(lm2/2).ne.lm2)) coefa=-coefa
        if((ix.eq.1).and.(2*((l-m-1)/4).ne.(l-m-1)/2)) coefa=-coefa
        coefl=coefa/d01
        icoefl=-id01+icoefn
        dec=10.0e0_knd**(-ndec-1)
        dcon=dec
        jlow=lm2+1
c
c  compute the integrals involving the angular functions by summing
c  d coefficients times corresponding integrals of Legendre
c  functions
c
c  forward summation of series for r2 and r2d
        dold=1.0e0_knd
        doldd=1.0e0_knd
        r2dtemp=doldd
        r2temp=dold
        r2pos=dold
        r2dpos=doldd
          do 30 j=jlow,lim
          dnew=dold*enr(j)*rpint1(j+j+ixx2)
          dnewd=doldd*enr(j)*rpint2(j+j+ixx2)
          r2temp=r2temp+dnew
          r2dtemp=r2dtemp+dnewd
          if(dnew.gt.0.0e0_knd) r2pos=r2pos+dnew
          if(dnewd.gt.0.0e0_knd) r2dpos=r2dpos+dnewd
          if((abs(dnew/r2temp)+abs(dnewd/r2dtemp)).lt.dcon) go to 40
          dold=dnew
          doldd=dnewd
30        continue
c
c  backward summation of series for r2 and r2d
40      jint=j
        if(jint.gt.lim) jint=lim
        if(lm2.lt.1) go to 60
        dold=1.0e0_knd
        doldd=1.0e0_knd
        j=lm2
          do 50 jj=1,lm2
          dnew=dold/(rpint1(j+j+ixx2)*enr(j))
          dnewd=doldd/(rpint2(j+j+ixx2)*enr(j))
          r2temp=r2temp+dnew
          r2dtemp=r2dtemp+dnewd
          if(dnew.gt.00.0e0_knd) r2pos=r2pos+dnew
          if(dnewd.gt.00.0e0_knd) r2dpos=r2dpos+dnewd
          if((abs(dnew/r2temp)+abs(dnewd/r2dtemp)).lt.dcon) go to 60
          dold=dnew
          doldd=dnewd
          j=j-1
50        continue
60      continue
        isub=int(log10(abs(r2pos/r2temp)+dec))
        isubd=int(log10(abs(r2dpos/r2dtemp)+dec))
        if(isubd.lt.0) isubd=0
        r2temp=r2temp*coefl*pnorm(l-m+1)
        if(ix.eq.0) r2temp=r2temp*pint1(l-m+1)
        if(ix.eq.1) r2temp=r2temp*pint3(l-m+1)*x
        iterm=int(log10(abs(r2temp)))
        ir2e=iterm+ipnorm(l-m+1)+icoefl
        r2c=r2temp*10.0e0_knd**(-iterm)
        if(abs(r2c).ge.1.0e0_knd) go to 70
        r2c=r2c*10.0e0_knd
        ir2e=ir2e-1
70        r2dtemp=-r2dtemp*coefl*pnorm(l-m+1)*c*x
        if(ix.eq.0) r2dtemp=r2dtemp*pint2(l-m+1)+r2temp*coefme
        if(ix.eq.1) r2dtemp=r2dtemp*pint4(l-m+1)*x+r2temp*coefmo
        if(ix.eq.0) jsub=int(log10(abs(r2temp*coefme/r2dtemp)+dec))
        if(ix.eq.1) jsub=int(log10(abs(r2temp*coefmo/r2dtemp)+dec))
        if(jsub.lt.0) jsub=0
        isub=max(isub,isubd+jsub)
        write(40,80) jint,lim,isub,isubd+jsub
80      format(8x,'r2int: converged in ',i6,' terms; 'i6,
     1         ' available; ',i3,' and ',i3,' digits of sub. error.')
        jterm=int(log10(abs(r2dtemp)))
        ir2de=jterm+ipnorm(l-m+1)+icoefl
        r2dc=r2dtemp*10.0e0_knd**(-jterm)
        if(abs(r2dc).ge.1.0e0_knd) go to 90
        r2dc=r2dc*10.0e0_knd
        ir2de=ir2de-1
90      continue
        return
        end
c
c
        subroutine r2leg (l,m,c,x1,lnum,limleg,limdr,ndec,nex,
     1                    maxd,maxmp,maxpdr,maxdr,maxq,enr,enrneg,drhor,
     2                    nsdrho,d01,id01,dneg,idneg,nsdneg,dfnorm,idfe,
     3                    dmfnorm,idmfe,prx,pdrx,qdr,qdml,iqdml,qdl,
     4                    iqdl,qr,qml,iqml,ql,iql,fajo,ifajo,jsub,
     5                    termpq,itermpq,ioppsum,iopqnsum,r1c,ir1e,
     6                    r1dc,ir1de,wront,minacc,r2c,ir2e,r2dc,ir2de,
     7                    jleg,jlegp,naccleg,nsubw,jflagl,iflagq,iflagp)
c
c  purpose:     To evaluate the prolate radial function of the
c               second kind and its first derivative with respect
c               to x using the traditional expansion in associated
c               Legendre functions.
c
c  parameters:
c
c     input :   l       : l
c               m       : m
c               c       : c
c               x1      : x-1
c               lnum    : number of l values desired
c               limleg  : approximately twice the maximum number
c                         of terms available to be taken in qsum,
c                         (sum involving q's time d coefficients)
c               limdr   : maximum number of terms available to be
c                         taken in psum (sum involving p's time
c                         d rho coefficients)
c               ndec    : number of decimal digits for real(knd)
c               nex     : largest exponent available for real(knd)
c               maxd    : dimension of enr array
c               maxmp   : dimension of enrneg array
c               maxpdr  : dimension of prx and pdrx arrays
c               maxdr   : dimension of drhor array
c               maxq    : dimension of qr and qdr arrays
c               enr     : array of d coefficient ratios
c               enrneg  : array of d coefficient ratios with
c                         negative subscripts
c               drhor   : array of d rho coefficient ratios
c               nsdrho  : maximum subtraction error in calculating
c                         drhor array
c               d01     : characteristic of the ratio of the first d
c                         coefficient with nonnegative subscript,
c                         either d0 or d1 depending on whether l-m is
c                         even or odd, to the d coefficient with
c                         subscript l - m
c               id01    : exponent (base 10) corresponding to d01
c               dneg    : characteristic of the ratio of the d
c                         coefficient with subscript -2m+ix to the
c                         d coefficient with subscript ix, where
c                         ix = 0 or 1 depending on whether l - m
c                         is even or odd
c               idneg   : exponent corresponding to dneg
c               nsdneg  : subtraction error in calculating dneg, also
c                         maximum subtraction error in the enrneg array
c               dfnorm  : characteristic of the Flammer normalization
c                         factor of the d coefficients. equal to the
c                         reciprocal of the value of the d coefficient
c                         d(n = l - m) using this normalization for
c                         the angular functions
c               idfe    : exponent associated with dfnorm
c               dmfnorm : characteristic of the Morse-Feshbach
c                         normalization factor of the d coefficients.
c                         equal to the reciprocal of the value of the
c                         d coefficient d(n = l - m) using this
c                         normalization for the angular functions
c               idmfe   : exponent associated with dmfnorm
c               prx     : ratios of successive Legendre functions of
c                         the first kind of the same parity
c               pdrx    : ratios of successive first derivatives of
c                         Legendre functions of the first kind of the
c                         same parity
c               qdr     : ratios of first derivatives of successive
c                         Legendre functions of the second kind
c               qdml    : characteristic of the first derivative of
c                         the associated Legendre function of the second
c                         kind with order m and degree m-1
c               iqdml   : exponent corresponding to qdml
c               qdl     : characteristic of the first derivative of
c                         the associated Legendre function of the second
c                         kind with order m and degree m
c               iqdl    : exponent corresponding to qdl
c               qr      : array of ratios of successive associated
c                         Legendre functions of the second kind
c               qml     : characteristic of the associated Legendre
c                         function of the second kind with order m
c                         and degree m-1
c               iqml    : exponent corresponding to qml
c               ql      : characteristic of the associated Legendre
c                         function of the second kind with order m and
c                         degree m
c               iql     : exponent corresponding to ql
c               fajo    : characteristic of the joining factor of the
c                         second kind
c               ifajo   : exponent corresponding to fajo
c               jsub    : subtraction error in forming fajo coming from
c                         dmfnorm
c               termpq  : characteristic of the relative size of the
c                         maximum terms in the positive degree q series
c                         and the p series used to calculate r2 and r2d
c               itermpq : exponent corresponding to termpq
c               ioppsum : integer flag = 0 if psum need not be computed
c                         since its contribution to r2 and r2d is
c                         negligible; = 1 if psum is computed
c               iopqnsum: integer flag = 0 if qnsum need not be computed
c                         since its contribution to r2 and r2d is
c                         negligible; = 1 if qnsum is computed
c               r1c     : charcteristic of corresponding radial function
c                         of the first kind
c               ir1e    : exponent of corresponding radial function of
c                         the first kind
c               r1dc    : charcteristic of corresponding first
c                         derivative of the radial function of the first
c                         kind
c               ir1de   : exponent of corresponding first derivative of
c                         the radial function of the first kind
c               wront   : theoretical Wronskian
c               minacc  : minimum desired accuracy in decimal digits
c
c     output:   r2c     : characteristic of prolate
c                         radial function of the second kind
c               ir2e    : exponent of prolate radial function of the
c                         second kind
c               r2dc    : characteristic of derivative with
c                         respect to x of prolate radial function
c                         of the second kind
c               ir2de   : exponent of derivative with respect to x of
c                         prolate radial function of second kind
c               jleg    : maximum number of terms taken in qsum
c               jlegp   : maximum number of terms taken in psum
c               naccleg : estimated accuracy of r2 and r2d
c               nsubw   : subtraction error in decimal digits that
c                         occurs in forming Wronskian for calculated
c                         radial functions and their first derivatives
c               jflagl  : equal to unity if Wronskian is used to
c                         calculate more accurate values for leading
c                         coefficient and thus more accurate values
c                         for r2 and r2d; equal to zero otherwise
c               iflagq  : set equal to zero at l = m. Remains equal
c                         to zero if set function values and accuracy
c                         to zero because terms in backward qsum
c                         become too large to allow accurate results.
c                         Set equal to unity when this first does not
c                         occur.
c               iflagp  : set equal to zero at l = m. Remains equal
c                         to zero if set function values and accuracy
c                         to zero because terms in psum become too large
c                         to allow accurate results. Set equal to unity
c                         when this first does not occur.
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) c,dconp,dconq,dconqn,dec,dec1,dfnorm,dmfnorm,dneg,
     1            dnegjf,dnew,dnewd,dold,doldd,d01,fac,psum,psump,pdsum,
     2            pdsump,qdml,qndsum,qndsump,qdsum,qdsump,qml,qnsum,
     3            qnsump,qsum,qsump,r1c,r1dc,r2c,r2dc,rm,spsum,spsump,
     4            spdsum,spdsump,ten,termpq,test,testd,teste,testeo,
     5            testm,testdm,tm,wronca,wroncb,wronc,wront,x1
        real(knd) drhor(maxdr),enr(maxd),enrneg(maxmp),fajo(lnum),
     1            prx(maxpdr),pdrx(maxpdr),qdl(lnum),qdr(maxq),ql(lnum),
     2            qr(maxq)
c
c  integer arrays
        dimension ifajo(lnum),iqdl(lnum),iql(lnum)
c
        jflagl=0
        ten=10.0e0_knd
        nfac=nex/3
        if(2*(nfac/2).ne.nfac) nfac=nfac-1
        teste=ten**(nfac)
        testeo=1.0e0_knd/teste
        iscale=0
        dec=ten**(-ndec-1)
        dec1=ten**(-ndec-7)
        lm2=(l-m)/2
        ix=l-m-2*lm2
        imxp=m+m+ix
        ixx=1-ix
        lim1=limleg/2-ix
        lim2=limdr-1
        if(ioppsum.eq.0) lim2=0
        rm=m
        tm=rm+rm
        dconq=dec
        dconqn=dec
        dconp=dec
        dnegjf=dneg*d01
        if(m.eq.0) dnegjf=d01
        idnegjf=idneg+id01
        if(m.eq.0) idnegjf=id01
        fajo(l-m+1)=fajo(l-m+1)*dmfnorm*dnegjf/dfnorm
        iterm=int(log10(abs(fajo(l-m+1))))
        fajo(l-m+1)=fajo(l-m+1)*ten**(-iterm)
        ifajo(l-m+1)=ifajo(l-m+1)+idnegjf+idmfe-idfe+iterm
        iterm=-int(log10(abs(c*x1*(x1+2.0e0_knd)*r1dc)))
        itermq=int(log10(abs(fajo(l-m+1)*termpq/ql(l-m+1))))
        itestq=iterm+itermq-ir1de+itermpq+ifajo(l-m+1)-iql(l-m+1)+ndec+3
        itermp=int(log10(abs(fajo(l-m+1)/(dnegjf*termpq))))
        itestp=iterm+itermp-ir1de-idnegjf-itermpq+ifajo(l-m+1)+ndec+3
c
c  begin calculation of series for r2
c
c  calculate d*q sum over positive n using pyramid summation
c
c  backward summation
        qsum=1.0e0_knd
        qdsum=1.0e0_knd
        qsump=1.0e0_knd
        qdsump=1.0e0_knd
        if(lm2.lt.1) go to 20
        dold=1.0e0_knd
        doldd=1.0e0_knd
        j=lm2
          do 10 jj=1,lm2
          dnew=dold/(qr(j+j+imxp)*qr(j+j+imxp-1)*enr(j))
          qsum=qsum+dnew
          if(dnew.gt.0.0e0_knd) qsump=qsump+dnew
          dnewd=doldd/(qdr(j+j+imxp)*qdr(j+j+imxp-1)*enr(j))
          qdsum=qdsum+dnewd
          if(dnewd.gt.0.0e0_knd) qdsump=qdsump+dnewd
            if(int(log10(abs(qsum)))+iscale.gt.itestq.and.iflagq.eq.0)
     1          then
            r2c=0.0e0_knd
            r2dc=0.0e0_knd
            ir2e=0
            ir2de=0
            nsub=ndec
            nsubd=ndec
            jleg=j
            jlegp=0
            naccleg=0
            go to 180
            end if
            if(abs(qsum).gt.teste) then
            dnew=dnew*testeo
            dnewd=dnewd*testeo
            qsum=qsum*testeo
            qdsum=qdsum*testeo
            qsump=qsump*testeo
            qdsump=qdsump*testeo
            iscale=iscale+nfac
            end if
          if((abs(dnew/qsum)+abs(dnewd/qdsum)).lt.dconq) go to 20
          dold=dnew
          doldd=dnewd
          j=j-1
10        continue
20      continue
c
c  forward summation
         iflagq=1
          if(iscale.ne.0) then
          jleg=lm2
          go to 45
          end if
        jlow=lm2+1
        dold=1.0e0_knd
        doldd=1.0e0_knd
          do 30 j=jlow,lim1
          dnew=dold*enr(j)*qr(j+j+imxp)*qr(j+j+imxp-1)
          qsum=qsum+dnew
          if(dnew.gt.0.0e0_knd) qsump=qsump+dnew
          dnewd=doldd*enr(j)*qdr(j+j+imxp)*qdr(j+j+imxp-1)
          qdsum=qdsum+dnewd
          if(dnewd.gt.0.0e0_knd) qdsump=qdsump+dnewd
          if((abs(dnew/qsum)+abs(dnewd/qdsum)).lt.dconq) go to 40
          dold=dnew
          doldd=dnewd
30        continue
40      continue
        jleg=j
        if(jleg.gt.lim1) jleg=lim1
45      nsqsum=0
        if(qsum*qsump.ne.0.0e0_knd) nsqsum=int(log10(abs(qsump/qsum)))
        if(nsqsum.gt.ndec) nsqsum=ndec
        if(nsqsum.lt.0) nsqsum=0
        nsqdsum=0
        if(qdsum*qdsump.ne.0.0e0_knd) nsqdsum=
     1                            int(log10(abs(qdsump/qdsum)))
        if(nsqdsum.gt.ndec) nsqdsum=ndec
        if(nsqdsum.lt.0) nsqdsum=0
        qsum=qsum*ql(l-m+1)/(fajo(l-m+1)*termpq)
        iterm=int(log10(abs(qsum)))
        qsum=qsum*(10.0e0_knd**(-iterm))
        iqsum=iql(l-m+1)-ifajo(l-m+1)-itermpq+iscale+iterm
        qdsum=qdsum*qdl(l-m+1)/(fajo(l-m+1)*termpq)
        iterm=int(log10(abs(qdsum)))
        qdsum=qdsum*(10.0e0_knd**(-iterm))
        iqdsum=iqdl(l-m+1)-ifajo(l-m+1)-itermpq+iscale+iterm
        qdsum=qdsum*(10.0e0_knd**(iqdsum-iqsum))
c
c  calculate d*q sum over negative n
        if(m.eq.0) iopqnsum=0
        qnsum=0.0e0_knd
        qndsum=0.0e0_knd
        qnsump=0.0e0_knd
        qndsump=0.0e0_knd
        iqnsum=0
        iqndsum=0
        nmterm=0
        nsqnsum=0
        nsqndsum=0
        if(iopqnsum.eq.0.or.m.eq.0) go to 90
        nmterm=m
        qnsum=enrneg(m)
        qndsum=enrneg(m)
        if(ix.eq.1) go to 50
        qnsum=qnsum*qr(m+m-1)
        qndsum=qndsum*qdr(m+m-1)
50      if(qnsum.gt.0.0e0_knd) qnsump=qnsum
        if(qndsum.gt.0.0e0_knd) qndsump=qndsum
        if(m.eq.1) go to 80
        dold=qnsum
        doldd=qndsum
          do 60 j=2,m
          dnew=dold*enrneg(m-j+1)*qr(imxp-j-j+1)*qr(imxp-j-j+2)
          qnsum=qnsum+dnew
          if(dnew.gt.0.0e0_knd) qnsump=qnsump+dnew
          dnewd=doldd*enrneg(m-j+1)*qdr(imxp-j-j+1)*qdr(imxp-j-j+2)
          qndsum=qndsum+dnewd
          if(dnewd.gt.0.0e0_knd) qndsump=qndsump+dnewd
          dold=dnew
60        doldd=dnewd
70      nsqnsum=0
        if(qnsum*qnsump.ne.0.0e0_knd) nsqnsum=
     1                    int(log10(abs(qnsump/qnsum)))
        if(nsqnsum.gt.ndec) nsqnsum=ndec
        if(nsqnsum.lt.0) nsqnsum=0
        nsqnsum=max(nsqnsum,nsdneg)
        nsqndsum=0
        if(qndsum*qndsump.ne.0.0e0_knd) nsqndsum=
     1                    int(log10(abs(qndsump/qndsum)))
        if(nsqndsum.gt.ndec) nsqndsum=ndec
        if(nsqndsum.lt.0) nsqndsum=0
        nsqndsum=max(nsqndsum,nsdneg)
80      qnsum=qnsum*qml*d01/(fajo(l-m+1)*termpq)
        iterm=int(log10(abs(qnsum)))
        qnsum=qnsum*(10.0e0_knd**(-iterm))
        iqnsum=iqml+id01-ifajo(l-m+1)-itermpq+iterm
        qnsum=qnsum*(10.0e0_knd**(iqnsum-iqsum))
        qndsum=qndsum*qdml*d01/(fajo(l-m+1)*termpq)
        iterm=int(log10(abs(qndsum)))
        qndsum=qndsum*(10.0e0_knd**(-iterm))
        iqndsum=iqdml+id01-ifajo(l-m+1)-itermpq+iterm
        qndsum=qndsum*(10.0e0_knd**(iqndsum-iqsum))
90      continue
c
c       calculate d(rho|n)*p summation
        psum=0.0e0_knd
        pdsum=0.0e0_knd
        ipsum=0
        ipdsum=0
        jlegp=0
        nspsum=0
        nspdsum=0
        if(ioppsum.eq.0) go to 160
        psum=prx(ixx+1)*drhor(1)
        pdsum=pdrx(ixx+1)*drhor(1)
        dold=psum
        doldd=pdsum
        if(m.ne.0.or.ix.ne.1) go to 100
        pdsum=0.0e0_knd
        doldd=drhor(1)
100     continue
        spsum=psum
        spdsum=pdsum
        psump=0.0e0_knd
        if(psum.gt.0.0e0_knd) psump=psum
        pdsump=0.0e0_knd
        if(pdsum.gt.0.0e0_knd) pdsump=pdsum
        spsump=psump
        spdsump=pdsump
        testm=1.0e0_knd
        testdm=1.0e0_knd
        jlegpf=1
        jlegpd=1
          do 130 j=2,lim2
          dnew=dold*drhor(j)*prx(j+j-ix)
          psum=psum+dnew
          if(dnew.gt.0.0e0_knd) psump=psump+dnew
          dnewd=doldd*drhor(j)*pdrx(j+j-ix)
          pdsum=pdsum+dnewd
            if(int(log10(abs(psum))).gt.itestp.and.iflagp.eq.0) then
            r2c=0.0e0_knd
            r2dc=0.0e0_knd
            ir2e=0
            ir2de=0
            nsub=ndec
            nsubd=ndec
            naccleg=0
            jlegp=0
            go to 180
            end if
          if(dnewd.gt.0.0e0_knd) pdsump=pdsump+dnewd
          test=abs(dnew/psum)
          testd=abs(dnewd/pdsum)
          if(test.gt.testm.or.test.eq.00.0e0_knd) go to 110
          testm=test
          spsum=psum
          spsump=psump
          jlegpf=j
110       if(testd.gt.testdm.or.testd.eq.00.0e0_knd) go to 120
          testdm=testd
          spdsum=pdsum
          spdsump=pdsump
          jlegpd=j
120       if(test+testd.lt.dconp) go to 150
          dold=dnew
          doldd=dnewd
130       continue
150     jlegp=max(jlegpf,jlegpd)
        iflagp=1
        psum=spsum
        pdsum=spdsum
        psump=spsump
        pdsump=spdsump
        ntestm=-int(log10(testm))
        ntestdm=-int(log10(testdm))
        if(ntestm.gt.ndec) ntestm=ndec
        if(ntestdm.gt.ndec) ntestdm=ndec
        nspsum=0
        if(psum*psump.ne.0.0e0_knd) nspsum=int(log10(abs(psump/psum)))
        if(nspsum.gt.ndec) nspsum=ndec
        if(nspsum.lt.0) nspsum=0
        nspsum=max(ndec-ntestm,nspsum,nsdrho)
        nspdsum=0
        if(pdsum*pdsump.ne.0.0e0_knd) nspdsum=
     1                    int(log10(abs(pdsump/pdsum)))
        if(nspdsum.gt.ndec) nspdsum=ndec
        if(nspdsum.lt.0) nspdsum=0
        nspdsum=max(ndec-ntestdm,nspdsum,nsdrho)
        psum=psum*dnegjf*termpq/fajo(l-m+1)
        iterm=0
        if(psum.ne.0.0e0_knd) iterm=int(log10(abs(psum)))
        psum=psum*(10.0e0_knd**(-iterm))
        ipsum=idnegjf+itermpq-ifajo(l-m+1)+iterm
        psum=psum*(10.0e0_knd**(ipsum-iqsum))
        pdsum=pdsum*dnegjf*termpq/fajo(l-m+1)
        if(m.ne.0) pdsum=pdsum*rm*(x1+1.0e0_knd)/(x1*(x1+2.0e0_knd))
        iterm=0
        if(pdsum.ne.0.0e0_knd) iterm=int(log10(abs(pdsum)))
        pdsum=pdsum*(10.0e0_knd**(-iterm))
        ipdsum=idnegjf+itermpq-ifajo(l-m+1)+iterm
        pdsum=pdsum*(10.0e0_knd**(ipdsum-iqsum))
160     continue
        r2c=qsum+qnsum+psum
        r2dc=qdsum+qndsum+pdsum
        nqs=0
        if(qsum/r2c.ne.0.0e0_knd) nqs=int(log10(abs(qsum/r2c)))
        nqns=0
        if(qnsum/r2c.ne.0.0e0_knd)
     1                nqns=int(log10(abs(qnsum/r2c)))
        nps=0
        if(psum/r2c.ne.0.0e0_knd)
     1                nps=int(log10(abs(psum/r2c)))
        nsqsum=nsqsum+nqs
        if(nsqsum.lt.0) nsqsum=0
        if(nsqsum.gt.ndec) nsqsum=ndec
        if(qsum/r2c.eq.0.0e0_knd) nsqsum=0
        nsqnsum=nsqnsum+nqns
        if(nsqnsum.lt.0) nsqnsum=0
        if(nsqnsum.gt.ndec) nsqnsum=ndec
        if(qnsum/r2c.eq.0.0e0_knd) nsqnsum=0
        nspsum=nspsum+nps
        if(nspsum.lt.0) nspsum=0
        if(nspsum.gt.ndec) nspsum=ndec
        if(psum/r2c.eq.0.0e0_knd) nspsum=0
        nsub=max(nsqsum,nsqnsum,nspsum)
        nqds=0
        if(qdsum/r2dc.ne.0.0e0_knd) nqds=int(log10(abs(qdsum/r2dc)))
        nqnds=0
        if(qndsum/r2dc.ne.0.0e0_knd)
     1                nqnds=int(log10(abs(qndsum/r2dc)))
        if(qnsum.eq.0.0e0_knd.and.qndsum.eq.0.0e0_knd) iopqnsum=0
        npds=0
        if(pdsum/r2dc.ne.0.0e0_knd)
     1                npds=int(log10(abs(pdsum/r2dc)))
        if(psum.eq.0.0e0_knd.and.pdsum.eq.0.0e0_knd) ioppsum=0
        nsqdsum=nsqdsum+nqds
        if(nsqdsum.lt.0) nsqdsum=0
        if(nsqdsum.gt.ndec) nsqdsum=ndec
        if(qdsum/r2dc.eq.0.0e0_knd) nsqdsum=0
        nsqndsum=nsqndsum+nqnds
        if(nsqndsum.lt.0) nsqndsum=0
        if(nsqndsum.gt.ndec) nsqndsum=ndec
        if(qndsum/r2dc.eq.0.0e0_knd) nsqndsum=0
        nspdsum=nspdsum+npds
        if(nspdsum.lt.0) nspdsum=0
        if(nspdsum.gt.ndec) nspdsum=ndec
        if(pdsum/r2dc.eq.0.0e0_knd) nspdsum=0
        nsubd=max(nsqdsum,nsqndsum,nspdsum)
        if(qnsum.eq.0.0e0_knd.and.qndsum.eq.0.0e0_knd) iopqnsum=0
        if(psum.eq.0.0e0_knd.and.pdsum.eq.0.0e0_knd) ioppsum=0
        wronca=r1c*r2dc*10.0e0_knd**(ir1e+iqsum)
        wroncb=r2c*r1dc*10.0e0_knd**(iqsum+ir1de)
        wronc=wronca-wroncb
        naccleg=-int(log10(abs((wronc-wront)/wront)+dec))
        if(naccleg.lt.0) naccleg=0
        if(naccleg.gt.ndec) naccleg=ndec
        nsubw=-int(log10(abs(wronc/wronca)+dec))
        if(nsubw.lt.0) nsubw=0
        if(naccleg.gt.1) naccleg=naccleg+nsubw
        itest=ndec-2-nsubw-max(nsub,nsubd)
        if(itest.lt.0) itest=0
        if(naccleg.lt.minacc.and.naccleg.lt.itest.and.x1.le.0.01e0_knd)
     1       then
          fac=wront/wronc
          qsum=qsum*fac
          qdsum=qdsum*fac
          qnsum=qnsum*fac
          qndsum=qndsum*fac
          psum=psum*fac
          pdsum=pdsum*fac
          r2c=qsum+qnsum+psum
          r2dc=qdsum+qndsum+pdsum
          jflagl=1
          naccleg=itest
          end if
        if(naccleg.gt.3.and.nps.lt.(-ndec-1).and.npds.lt.(-ndec-1))
     1       ioppsum=0
        if(naccleg.gt.3.and.nqns.lt.(-ndec-1).and.nqnds.lt.(-ndec-1))
     1      iopqnsum=0
        if(naccleg.lt.0) naccleg=0
          if(jflagl.eq.0) then
          nsub=max(nsub,jsub,nsdneg)
          nsubd=max(nsubd,jsub,nsdneg)
          end if
        iterm=int(log10(abs(r2c)))
        r2c=r2c*(10.0e0_knd**(-iterm))
        ir2e=iqsum+iterm
        if(abs(r2c).ge.1.0e0_knd) go to 170
        r2c=r2c*10.0e0_knd
        ir2e=ir2e-1
170     continue
        iterm=int(log10(abs(r2dc)))
        r2dc=r2dc*(10.0e0_knd**(-iterm))
        ir2de=iqsum+iterm
        if(abs(r2dc).ge.1.0e0_knd) go to 180
        r2dc=r2dc*10.0e0_knd
        ir2de=ir2de-1
180     continue
        if(ioppsum.eq.1.and.iopqnsum.eq.1) write(40,190) jleg,jlegp,
     1                                      m,lim1,lim2,m,nsub,nsubd
190     format(8x,'r2leg: qsum, psum and qnsum series converged in ',i6,
     1        ',' i6,' and ',i4,' terms; ',i6,',' i6,' and ' i4,
     2        ' terms avail.',/,15x,i2,' and ',i2,' digits of sub.',
     3        ' error in r2 and r2d.')
        if(ioppsum.eq.1.and.iopqnsum.eq.0) write(40,200) jleg,jlegp,
     1                                     lim1,lim2,nsub,nsubd
200     format(8x,'r2leg: qsum and psum series converged in ',i6,
     1        ' and ',i6,' terms; ',i6,' and ',i6,' terms avail.',/,
     2        15x,i2,' and ',i2,' digits of sub. error in r2 and r2d;',
     3        ' qnsum is negligible.')
        if(ioppsum.eq.0.and.iopqnsum.eq.1) write(40,210) jleg,m,
     1                                     lim1,m,nsub,nsubd
210     format(8x,'r2leg: qsum and qnsum series converged in ',i6,
     1        ' and ',i4,' terms; ',i6,' and ',i4,' terms avail.',/,
     2         15x,i2,' and ',i2,' digits of sub. error in r2 and r2d;'
     3         ' psum is negligible.')
        if(ioppsum.eq.0.and.iopqnsum.eq.0) write(40,220) jleg,lim1,
     1                                            nsub,nsubd
220     format(8x,'r2leg: qsum series converged in ',i6,' terms with ',
     1         i6,' terms avail.; 'i2,' and ',i2,' digits of',/,15x,
     2         'sub. error in r2 and r2d; psum and qnsum are ',
     3         'negligible.')
        if(jflagl.eq.1) write(40,230)
230     format(15x,'Wronskian used to improve accuracy of the',
     1          ' joining factor including dmfnorm and dneg.')
        return
        end
c
c
        subroutine r2neu (l,m,c,x1,limneu,ndec,nex,maxd,maxlp,maxn,
     1                    maxp,minacc,enr,sneuf,sneun,ineue,sneudf,
     2                    sneudr,prat1,pcoefn,ipcoefn,dmfnorm,idmfe,
     3                    r1dc,ir1de,r2c,ir2e,r2dc,ir2de,jneu)
c
c  purpose:     To calculate the prolate radial function of the
c               second kind and its first derivative with respect
c               to x, using the traditional expansions in terms of
c               spherical Neumann functions.
c
c  parameters:
c
c     input:    l      : l
c               m      : m
c               c      : c
c               x1     : x-1
c               limneu : maximum number of terms to be taken in the
c                        series summations for r2 and r2d
c               ndec   : number of decimal digits available in
c                        real(knd)
c               nex    : maximum exponent in real(knd) arithmetic
c               maxd   : dimension of enr array
c               maxlp  : maximum  l value desired; dimension
c                        of the sneun, sneudn, ineue, and ineude arrays
c               maxn   : dimension of sneuf and sneudf arrays
c               maxp   : dimension of prat1 array
c               minacc : number of decimal digits of desired accuracy
c                        of the resulting radial functions
c               enr    : array of ratios of successive d coefficients
c               sneuf  : array of ratios of successive spherical Neumann
c                        functions of the same parity
c               sneun  : array of characteristics for Neumann functions
c               ineue  : array of exponents for Neumann functions
c               sneudf : array of ratios of successive first derivatives
c                        of spherical Neumann functions of same parity
c               sneudr : array of ratios of first derivatives of Neumann
c                        functions to the corresponding functions
c               prat1  : array of ratios of successive coefficients in
c                        r2 and r2d sum
c               pcoefn : characteristic of coefficient for term in both
c                        r2 and r2d sums that contains Neumann function
c                        of order l
c               ipcoefn: exponent (to the base 10) corresponding to
c                        pcoefn
c               dmfnorm: characteristic of the Morse-Feshbach
c                        normalization factor of the d coefficients.
c                        equal to the reciprocal of the value of the d
c                        coefficient d(n = l - m) using this
c                        normalization for the angular functions
c               idmfe  : exponent associated with dmfnorm
c               r1dc   : charcteristic of corresponding first
c                        derivative of the radial function of the first
c                        kind
c               ir1de  : exponent of corresponding first derivative of
c                        the radial function of the first kind
c
c     output:   r2c    : characteristic of prolate radial function
c                        of the second kind
c               ir2e   : exponent of prolate radial function of the
c                        second kind
c               r2dc   : characteristic of derivative with respect
c                        to x of prolate radial function of the second
c                        kind
c               ir2de  : exponent of derivative with respect to x of
c                        prolate radial function of the second kind
c               jneu   : index of term where best convergence is
c                        achieved for r2 or for r2d, whichever term is
c                        larger
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) c,dconb,dconf,dconi,dmfnorm,dnew,dnewd,dold,doldd,
     1            pcoefn,rm,rm2,r1dc,r2c,r2dc,r2dcoef,r2dtemp,r2est,
     2            r2temp,r2test,sr2temp,sr2dtemp,sumcoef,sump,sumdp,
     3            ten,test,testd,testdm,testm,teste,testeo,tx,txd,x1
        real(knd) enr(maxd),sneudr(maxlp),sneun(maxlp),
     1            prat1(maxp),sneuf(maxn),sneudf(maxn)
c
c  integer arrays
        dimension ineue(maxlp)
c
        rm=m
        ten=10.0e0_knd
        nfac=nex/2
        if(2*(nfac/2).ne.nfac) nfac=nfac-1
        teste=ten**(nfac)
        testeo=1.0e0_knd/teste
        iscale=0
        dconf=ten**(-ndec-1)
        dconi=ten**(ndec+2)
        sumcoef=(ten**(-ir1de-ineue(l+1)-ipcoefn+idmfe))/
     1          (c*x1*(x1+2.0e0_knd)*r1dc*sneun(l+1)*pcoefn)
        r2est=abs(sumcoef*dmfnorm)
        dconb=r2est/dconi
        r2test=r2est*dconi
        r2dcoef=rm/((x1+1.0e0_knd)*(x1+2.0e0_knd)*x1)
        rm2=rm*2.0e0_knd
        lm2=(l-m)/2
c
c  ix = 0 for l-m even; ix = 1 for l-m odd
        ix=l-m-2*lm2
        lim=limneu/2-ix
c
c  compute radial function of the second kind
c
c  backward series
        r2temp=1.0e0_knd
        r2dtemp=1.0e0_knd
        sump=1.0e0_knd
        sumdp=1.0e0_knd
        if(r2est.gt.dconi) go to 20
        if (lm2.lt.1) go to 20
        dold=1.0e0_knd
        doldd=1.0e0_knd
          do 10 j=lm2,1,-1
          jj=j+j+ix
          dnew=-dold/(sneuf(jj+m)*prat1(jj+1)*enr(j))
          dnewd=-doldd/(sneudf(jj+m)*prat1(jj+1)*enr(j))
          r2temp=r2temp+dnew
          r2dtemp=r2dtemp+dnewd
          if(dnew.gt.0.0e0_knd) sump=sump+dnew
          if(dnewd.gt.0.0e0_knd) sumdp=sumdp+dnewd
          if(abs(dnew/r2temp)+abs(dnewd/r2dtemp).lt.dconb) go to 20
          dold=dnew
          doldd=dnewd
10      continue
20      continue
c
c  forward series
        dold=1.0e0_knd
        doldd=1.0e0_knd
        testm=1.0e0_knd
        testdm=1.0e0_knd
        sr2temp=r2temp
        sr2dtemp=r2dtemp
        tx=sump
        txd=sumdp
        js=lim
        jds=lim
          do 70 j=lm2+1,lim
          jj=j+j+ix
          dnew=-dold*enr(j)*sneuf(jj+m)*prat1(jj+1)
          dnewd=-doldd*enr(j)*sneudf(jj+m)*prat1(jj+1)
          r2temp=r2temp+dnew
          r2dtemp=r2dtemp+dnewd
          if(dnew.gt.0.0e0_knd) sump=sump+dnew
          if(dnewd.gt.0.0e0_knd) sumdp=sumdp+dnewd
          test=abs(dnew/r2temp)
          testd=abs(dnewd/r2dtemp)
          if(abs(r2temp).gt.r2test) go to 80
          if(test.lt.testm) go to 30
          go to 40
30        testm=test
          sr2temp=r2temp
          tx=sump
          js=j
40        continue
          if(testd.lt.testdm) go to 50
          go to 60
50        testdm=testd
          sr2dtemp=r2dtemp
          txd=sumdp
          jds=j
60        continue
          if(test+testd.lt.dconf) go to 90
            if(abs(r2temp).gt.teste) then
            r2temp=r2temp*testeo
            r2dtemp=r2dtemp*testeo
            sr2temp=sr2temp*testeo
            sr2dtemp=sr2dtemp*testeo
            sump=sump*testeo
            sumdp=sumdp*testeo
            dnew=dnew*testeo
            dnewd=dnewd*testeo
            iscale=iscale+nfac
            r2test=r2test*testeo
            tx=tx*testeo
            txd=txd*testeo
            end if
          dold=dnew
          doldd=dnewd
70        continue
80      r2temp=sr2temp
        r2dtemp=sr2dtemp
        sump=tx
        sumdp=txd
90      continue
        jneu=max(js,jds)
        naccs1=int(log10(abs(tx/r2temp)+dconf))
        if(naccs1.lt.0) naccs1=0
        if(naccs1.gt.ndec) naccs1=ndec
        naccs2=int(log10(abs(txd/r2dtemp)+dconf))
        if(naccs2.lt.0) naccs2=0
        if(naccs2.gt.ndec) naccs2=ndec
        jtestm=-int(log10(testm+dconf))
        if(jtestm.lt.0) jtestm=0
        if(jtestm.gt.ndec) jtestm=ndec
        jtestdm=-int(log10(testdm+dconf))
        if(jtestdm.lt.0) jtestdm=0
        if(jtestdm.gt.ndec) jtestdm=ndec
        write(40,100) j,lim,js,jtestm,naccs1,jds,jtestdm,naccs2
100     format(8x,'r2neu: numerator converged in ',i6,' terms; ',
     1         i6,' terms available.',/,15x,'best r2 at ',i6,
     2         ' terms with convergence to ',i3,' digits and sub error',
     3         ' of',i3,' digits.',/,15x,'best r2d at ',i6,' terms',
     4         ' with convergence to ',i3,' digits and sub. error of',
     5         i3,' digits.')
c
c  combining results to form the radial function characteristics
c  r2c and r2dc and corresponding exponents ir2e and ir2de
        r2c=r2temp*sneun(l+1)*pcoefn/dmfnorm
        iterm=int(log10(abs(r2c)))
        ir2e=ineue(l+1)+ipcoefn-idmfe+iterm+iscale
        r2c=r2c*10.0e0_knd**(-iterm)
        if(abs(r2c).ge.1.0e0_knd) go to 110
        r2c=r2c*10.0e0_knd
        ir2e=ir2e-1
110        continue
        r2dc=r2dcoef*r2c+(c*r2dtemp*sneun(l+1)*sneudr(l+1)*pcoefn/
     1       dmfnorm)*10.0e0_knd**(ineue(l+1)+ipcoefn-idmfe+iscale-ir2e)
        iterm=int(log10(abs(r2dc)))
        ir2de=ir2e+iterm
         r2dc=r2dc*10.0e0_knd**(-iterm)
        if(abs(r2dc).ge.1.0e0_knd) go to 120
        r2dc=r2dc*10.0e0_knd
        ir2de=ir2de-1
120        continue
        return
        end
c
c
        subroutine r2eta (l,m,c,x1,eta,nee,incnee,limeta,ndec,nex,maxd,
     1                    maxlp,maxn,maxp,minacc,wm,enr,sneuf,sneun,
     2                    ineue,sneudf,sneudr,pdratt,pratb,pratt,
     3                    pcoefn,ipcoefn,pdcoefn,ipdcoefn,r1c,ir1e,r1dc,
     4                    ir1de,naccmax,naccr,r2c,ir2e,r2dc,ir2de,
     5                    nacceta,nacciop,jeta,iopnee,neemark,naccd,
     6                    naccn,naccnmax)
c
c  purpose:     To calculate the prolate radial function of the
c               second kind and its first derivative with respect
c               to x, using an expansion of spherical Neumann
c               functions.
c
c  parameters:
c
c     input:    l       : l
c               m       : m
c               c       : c
c               x1      : x-1
c               eta     : value for eta used in calculation
c               nee     : index in the array of eta values in the main
c                         program that corresponds to the value of eta
c                         used in r2eta calculations
c               incnee  : increment in nee
c               limeta  : maximum number of terms available in the sums
c                         for r2 and r2d
c               ndec    : number of decimal digits for real(knd)
c               nex     : maximum exponent in real(knd) arithmetic
c               maxd    : dimension of enr array
c               maxlp   : maximum  l value desired; dimension
c                         of the sneun, sneudr, and ineue arrays
c               maxn    : dimension of sneuf and sneudf arrays
c               maxp    : dimension of pdratt, pratb, and pratt arrays
c               minacc  : minimum number of accurate decimal digits
c                         that are requested
c               wm      : 1 - eta*eta = sin(nee)*sin(nee)
c               enr     : array of ratios of successive d coefficients
c               sneuf   : array of ratios of successive spherical
c                         Neumann functions of the same parity
c               sneun   : array of characteristics for Neumann functions
c               ineue   : array of exponents corresponding to sneun
c               sneudf  : array of ratios of successive first
c                         derivatives of spherical Neumann functions of
c                         the same parity
c               sneudr  : array of ratios of first derivatives of the
c                         spherical Neumann functions to the
c                         corresponding functions
c               pdratt  : array of ratios of successive first
c                         derivatives of the associated Legendre
c                         functions of the first kind of the same parity
c                         (used in numerator series)
c               pratb   : array of ratios of successive associated
c                         Legendre functions of the first kind of the
c                         same parity (used in denominator series)
c               pratt   : array of ratios of successive associated
c                         Legendre functions of the first kind of the
c                         same parity (used in numerator series)
c               pcoefn  : characteristic of the ratio of the numerator
c                         and denominator associated Legendre functions
c                         of the first kind of order m and degree l
c               ipcoefn : exponent corresponding to pcoefn
c               pdcoefn : characteristic of the ratio of the first
c                         derivative of the associated Legendre function
c                         of the first kind in the numerator and the
c                         associated Legendre function of the first kind
c                         in the denominator, both of order m and
c                         degree l
c               ipdcoefn: exponent corresponding to pdcoefn
c               r1c     : characteristic of the radial function of the
c                         first kind (calculated in r1bes)
c               irie    : exponent corresponding to r1c
c               r1dc    : characteristic of the first derivative with
c                         respect to x of the radial function of the
c                         first kind (calculated in r1bes)
c               ir1de   : exponent corresponding to r1dc
c               naccmax : maximum accuracy (in decimal digits) obtained
c                         for the current value of l from previous
c                         r2eta calculations
c               naccr   : accuracy of radial functions calculated for
c                         this value of l earlier using other methods.
c                         if this is the only method used the default
c                         value of minacc is used for naccr
c
c     output:   r2c     : characteristic of the prolate radial function
c                         of the second kind
c               ir2e    : exponent of the prolate radial function of the
c                         second kind
c               r2dc    : characteristic of the first derivative with
c                         respect to x of the prolate radial function
c                         of the second kind
c               ir2de   : exponent corresponding to r2dc
c               nacceta : estimated number of accurate decimal digits in
c                         r2 and r2d. computed from the wronskian if
c                         nacciop = 0; estimated from the series if
c                         nacciop = 1
c               nacciop : integer flag = 1 if the denominator in the
c                         expressions for r2 and r2d is computed using
c                         the theoretical wronskian and known values for
c                         r1 and r1d. nacciop = 0 otherwise
c               jeta    : maximum number of terms taken in the numerator
c                         sums for r2 and r2d
c               iopnee  : integer flag = 0 if none of the values used
c                         for eta for the present l has led to an
c                         estimated accuracy nacceta of at least 3
c                         decimal digits (else iopnee = 1) and the
c                         subtraction error in the numerator series
c                         for both r2 and r2d is not greater than
c                         ndec-minacc digits (else iopnee =2)
c               neemark : index for the eta value in the array storing
c                         eta values in the main program that
c                         corresponds to the last eta value used for
c                         which iopnee = 0
c               naccd   : estimated accuracy of denominator sum
c               naccn   : estimated accuracy of numerator sums
c
c     input/
c     output:   naccnmax: maximum accuracy (in decimal digits) obtained
c                         for the numerator series for the current value
c                         of l from all previous r2eta calculations
c                         (input) and including the curent r2eta
c                         calculation (output)
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) c,dcon,dconi,dec,denom,dnew,dnewd1,dnewd2,dnewsum,
     1            dnewdsum1,dnewdsum2,dold,doldd1,doldd2,eta,etas,
     2            factor,pcoefn,pdcoefn,reld12,rm,rm2,r1c,r1dc,r2c,r2dc,
     3            r2dcoef1,r2dcoef2,r2dtemp,r2dtemp1,r2dtemp2,r2est,
     4            r2temp,r2test,sr2temp,sr2dtemp1,sr2dtemp2,sumcoef,
     5            sumdnp1,sumdnp2,sumdp,sumnp,ten,test,testdm1,testdm2,
     6            testd1,testd2,testm,teste,testeo,tx,txd1,txd2,wm,
     7            wronc,wronca,wroncb,wroncm,wront,xet,xets,x1
        real(knd) enr(maxd),sneudr(maxlp),sneun(maxlp),pratb(maxp),
     1            pratt(maxp),pdratt(maxp),sneuf(maxn),sneudf(maxn)
c
c  integer arrays
        dimension ineue(maxlp)
c
        ten=10.0e0_knd
        nfac=nex/2
        if(2*(nfac/2).ne.nfac) nfac=nfac-1
        teste=ten**(nfac)
        testeo=1.0e0_knd/teste
        iscale=0
        dec=ten**(-ndec-2)
        rm=m
        dcon=ten**(-ndec-2)
        dconi=ten**(ndec+2)
        etas=eta*eta
        xet=sqrt(x1*(x1+2.0e0_knd)+etas)
        xets=xet*xet
        factor=1.0e0_knd
          if(-ir1de-ineue(l+1)-ipcoefn.gt.nex-ndec-30) then
          iscale=nex-ndec-30
          factor=ten**(-iscale)
          sumcoef=ten**(-ir1de-ineue(l+1)-ipcoefn-iscale)/
     1            (c*x1*(x1+2.0e0_knd)*r1dc*sneun(l+1)*pcoefn)
          else
          sumcoef=(ten**(-ir1de-ineue(l+1)-ipcoefn))/
     1          (c*x1*(x1+2.0e0_knd)*r1dc*sneun(l+1)*pcoefn)
          end if
        r2dcoef1=-eta*wm/(xets*xet)
        r2dcoef2=c*(x1+1.0e0_knd)/xet
        reld12=(r2dcoef2/r2dcoef1)*sneudr(l+1)*(pcoefn/
     1         pdcoefn)*ten**(ipcoefn-ipdcoefn)
        rm2=m+m
        lm2=(l-m)/2
c
c  ix = 0 for l-m even; ix = 1 for l-m odd
        ix=l-m-2*lm2
        lim=limeta/2-ix
c
c  compute radial function of the second kind and its first derivative
c
c  backward series for denominator
        denom=1.0e0_knd
        sumdp=1.0e0_knd
        if (lm2.lt.1) go to 20
        dold=1.0e0_knd
          do 10 j=lm2,1,-1
          jj=j+j+ix
          dnew=dold/(pratb(jj+1)*enr(j))
          denom=denom+dnew
          if(dnew.gt.00.0e0_knd) sumdp=sumdp+dnew
          if(abs(dnew/denom).lt.dec) go to 20
          dold=dnew
10        continue
20      continue
c
c  forward series for denominator
        dold=1.0e0_knd
          do 30 j=lm2+1,lim
          jj=j+j+ix
          dnew=dold*enr(j)*pratb(jj+1)
          denom=denom+dnew
          if(dnew.gt.00.0e0_knd) sumdp=sumdp+dnew
          if(abs(dnew/denom).lt.dec) go to 40
          dold=dnew
30        continue
40      continue
        jden=j
        numsub=0
        if(sumdp.ne.0.0e0_knd) numsub=int(log10(abs(sumdp/denom)))
        if(numsub.lt.0) numsub=0
        if(numsub.gt.ndec) numsub=ndec
        naccd=ndec-max(2,int(log10(c)))-numsub
        if(naccd.lt.0) naccd=0
        r2est=abs(sumcoef*denom)
        r2test=r2est*dconi
c
c  backward series for numerator
        dold=factor
        doldd1=factor
        doldd2=factor*reld12
        r2temp=dold
        sumnp=r2temp
        r2dtemp1=0.0e0_knd
        sumdnp1=0.0e0_knd
        r2dtemp2=doldd2
        sumdnp2=0.0e0_knd
        if(doldd2.gt.0.0e0_knd) sumdnp2=doldd2
        if(l.ne.0) r2dtemp1=factor
        if(l.ne.0) sumdnp1=r2dtemp1
        if(lm2.eq.0) go to 60
          do 50 j=lm2,1,-1
          jj=j+j+ix
          dnew=-dold/(sneuf(jj+m)*pratt(jj+1)*enr(j))
          dnewd1=-doldd1/(sneuf(jj+m)*pdratt(jj+1)*enr(j))
          dnewd2=-doldd2/(sneudf(jj+m)*pratt(jj+1)*enr(j))
          r2temp=r2temp+dnew
          r2dtemp1=r2dtemp1+dnewd1
          r2dtemp2=r2dtemp2+dnewd2
          if(dnew.gt.0.0e0_knd) sumnp=sumnp+dnew
          if(dnewd1.gt.0.0e0_knd) sumdnp1=sumdnp1+dnewd1
          if(dnewd2.gt.0.0e0_knd) sumdnp2=sumdnp2+dnewd2
          if(abs(dnew/r2temp)+abs(dnewd1/r2dtemp1)+abs(dnewd2/r2dtemp2)
     1         .lt.dcon) go to 60
          dold=dnew
          doldd1=dnewd1
          doldd2=dnewd2
50        continue
60      continue
          if(m.eq.0.and.jj.eq.2) then
          r2dtemp1=r2dtemp1-dnewd1
          if(dnewd1.gt.0.0e0_knd) sumdnp1=sumdnp1-dnewd1
          end if
c
c  forward series for numerator
        dold=factor
        doldd1=factor
        doldd2=factor*reld12
        test=1.0e0_knd
        testd1=1.0e0_knd
        testd2=1.0e0_knd
        testm=1.0e0_knd
        testdm1=1.0e0_knd
        testdm2=1.0e0_knd
        js=lm2
        jds1=lm2
        jds2=lm2
        sr2temp=r2temp
        sr2dtemp1=r2dtemp1
        sr2dtemp2=r2dtemp2
        tx=sumnp
        txd1=sumdnp1
        txd2=sumdnp2
        kount=0
        kountd1=0
        kountd2=0
        dnewsum=0.0e0_knd
        dnewdsum1=0.0e0_knd
        dnewdsum2=0.0e0_knd
          do 110 j=lm2+1,lim-1
          jj=j+j+ix
          kount=kount+1
          kountd1=kountd1+1
          kountd2=kountd2+1
          dnew=-dold*enr(j)*sneuf(jj+m)*pratt(jj+1)
          dnewd1=-doldd1*enr(j)*sneuf(jj+m)*pdratt(jj+1)
          dnewd2=-doldd2*enr(j)*sneudf(jj+m)*pratt(jj+1)
          if((dnew/dold).le.0.0e0_knd.or.kount.eq.100) go to 70
          dnewsum=dnewsum+dnew
          go to 80
70        r2temp=r2temp+dnewsum
          kount=0
          if(abs(r2temp).gt.r2test) go to 120
          if(dnewsum.gt.0.0e0_knd) sumnp=sumnp+dnewsum
          if(dnewsum.ne.0.0e0_knd) test=abs(dnewsum/r2temp)
          dnewsum=dnew
          if(test.ge.testm) go to 80
          testm=test
          sr2temp=r2temp
          js=j
          tx=sumnp
80        if((dnewd1/doldd1.le.0.0e0_knd).or.(kountd1.eq.100))
     1            go to 85
          dnewdsum1=dnewdsum1+dnewd1
          go to 90
85        r2dtemp1=r2dtemp1+dnewdsum1
          kountd1=0
          if(dnewdsum1.gt.0.0e0_knd) sumdnp1=sumdnp1+dnewdsum1
          if(dnewdsum1.ne.0.0e0_knd) testd1=abs(dnewdsum1/r2dtemp1)
          dnewdsum1=dnewd1
          if(testd1.ge.testdm1) go to 90
          testdm1=testd1
          sr2dtemp1=r2dtemp1
          jds1=j
          txd1=sumdnp1
90        if(((dnewd2/doldd2).le.0.0e0_knd).or.(kountd2.eq.100))
     1            go to 95
          dnewdsum2=dnewdsum2+dnewd2
          go to 100
95        r2dtemp2=r2dtemp2+dnewdsum2
          kountd2=0
          if(dnewdsum2.gt.0.0e0_knd) sumdnp2=sumdnp2+dnewdsum2
          if(dnewdsum2.ne.0.0e0_knd) testd2=abs(dnewdsum2/r2dtemp2)
          dnewdsum2=dnewd2
          if(testd2.ge.testdm2) go to 100
          testdm2=testd2
          sr2dtemp2=r2dtemp2
          jds2=j
          txd2=sumdnp2
100       if(test+testd1+testd2.lt.dcon) go to 130
            if(abs(r2temp).gt.teste) then
            r2temp=r2temp*testeo
            r2dtemp1=r2dtemp1*testeo
            r2dtemp2=r2dtemp2*testeo
            sr2temp=sr2temp*testeo
            sr2dtemp1=sr2dtemp1*testeo
            sr2dtemp2=sr2dtemp2*testeo
            tx=tx*testeo
            txd1=txd1*testeo
            txd2=txd2*testeo
            sumnp=sumnp*testeo
            sumdnp1=sumdnp1*testeo
            sumdnp2=sumdnp2*testeo
            dnew=dnew*testeo
            dnewd1=dnewd1*testeo
            dnewd2=dnewd2*testeo
            dnewsum=dnewsum*testeo
            dnewdsum1=dnewdsum1*testeo
            dnewdsum2=dnewdsum2*testeo
            iscale=iscale+nfac
            r2test=r2test*testeo
            end if
          dold=dnew
          doldd1=dnewd1
          doldd2=dnewd2
110       continue
120     r2temp=sr2temp
        r2dtemp1=sr2dtemp1
        r2dtemp2=sr2dtemp2
        go to 135
130     tx=sumnp
        txd1=sumdnp1
        txd2=sumdnp2
        js=j
        jds1=j
        jds2=j
135     continue
        jmax=j
        jeta=max(js,jds1,jds2,jden)
        jds=max(jds1,jds2)
        jtestm=ndec
        if(testm.ne.0.0e0_knd) jtestm=-int(log10(testm))
        if(jtestm.lt.0) jtestm=0
        if(jtestm.gt.ndec) jtestm=ndec
        jtestdm1=ndec
          if(testdm1.ne.0.0e0_knd) then
          jtestdm1=-int(log10(testdm1))
          end if
        if(jtestdm1.lt.0) jtestdm1=0
        if(jtestdm1.gt.ndec) jtestdm1=ndec
        jtestdm2=ndec
          if(testdm2.ne.0.0e0_knd) then
          jtestdm2=-int(log10(testdm2))
          end if
        if(jtestdm2.lt.0) jtestdm2=0
        if(jtestdm2.gt.ndec) jtestdm2=ndec
        naccns1=0
          if(abs(tx*r2temp).ne.0.0e0_knd) then
          naccns1=int(log10(abs(tx/r2temp)))
          end if
        if(naccns1.lt.0) naccns1=0
        if(naccns1.gt.ndec) naccns1=ndec
        r2dtemp=r2dtemp1+r2dtemp2
        naccns2=0
          if(abs((txd1+txd2)*r2dtemp).ne.0.0e0_knd) then
          naccns2=int(log10(abs((txd1+txd2)/r2dtemp)))
          end if
        if(naccns2.lt.0) naccns2=0
        if(naccns2.gt.ndec) naccns2=ndec
          if(abs(r2dtemp1*r2dtemp2).ne.0.0e0_knd) then
          icord=int(log10(abs(r2dtemp1/r2dtemp2)))
          if(icord.gt.0) jtestdm2=jtestdm2+icord
          if(icord.lt.0) jtestdm1=jtestdm1-icord
          jtestdm=min(jtestdm1,jtestdm2)
          end if
        if(abs(r2dtemp1).eq.0.0e0_knd) jtestdm=jtestdm2
        if(abs(r2dtemp2).eq.0.0e0_knd) jtestdm=jtestdm1 
        ncorr=max(0,-int(log10(x1)-0.001e0_knd))+1
        if(x1.ge.0.05e0_knd) ncorr=1
          if(x1.le.0.02e0_knd) then
          naccn1=min(jtestm-1,ndec-ncorr,ndec-naccns1)
          naccn2=min(jtestdm-1,ndec-ncorr,ndec-naccns2)
          else
          naccn1=min(jtestm-1,ndec-ncorr,ndec-naccns1-1)
          naccn2=min(jtestdm-1,ndec-ncorr,ndec-naccns2-1)
          end if
        naccn=min(naccn1,naccn2)
        if(naccn.gt.ndec-2) naccn=ndec-2
        if(naccn.lt.0) naccn=0
        nsubeta=max(naccns1,naccns2)
c
c  combining results to form the radial function characteristics
c  r2c and r2dc and corresponding exponents ir2e and ir2de
        r2c=r2temp*sneun(l+1)*pcoefn/denom
        iterm=0
        if(r2c.ne.0.0e0_knd) iterm=int(log10(abs(r2c)))
        ir2e=ineue(l+1)+ipcoefn+iterm+iscale
        r2c=r2c*10.0e0_knd**(-iterm)
        r2dc=r2dcoef1*r2dtemp*sneun(l+1)*pdcoefn/denom
        iterm=0
        if(r2dc.ne.0.0e0_knd) iterm=int(log10(abs(r2dc)))
        ir2de=ineue(l+1)+ipdcoefn+iterm+iscale
         r2dc=r2dc*10.0e0_knd**(-iterm)
        write(40,140) jmax,jden,lim,js,jtestm,naccns1,jds,jtestdm,
     1               naccns2,naccn,naccd
140     format(8x,'r2eta: numerator, denominator converged in ',
     1         i6,' ,',i6,' terms; ',i6,' terms available.',/,
     2         15x,'best r2 at ',i6,' terms with convergence to',i3,
     3         ' digits;',i3,' digits subtr. error.',/,15x,
     4         'best r2d at ',i6,' terms with convergence to ',i3,
     5         ' digits;',i3,' digits subtr. error.',/,15x,
     6         'estimated numerator and denominator accuracy is ',i4,
     7         ' and',i4,' digits.')
        wronca=r1c*r2dc*(10.0e0_knd**(ir1e+ir2de))
        wroncb=r2c*r1dc*(10.0e0_knd**(ir2e+ir1de))
        wronc=wronca-wroncb
        wront=1.0e0_knd/(c*x1*(x1+2.0e0_knd))
        wroncm=max(abs(wronca),abs(wroncb))
        nsubw =-int(log10(abs(wronc/wroncm)+dec))
        nacceta=-int(log10(abs((wronc-wront)/wront)+dec))
        if(nacceta.lt.0) nacceta=0
        nacciop=0
        if(nacceta.lt.minacc.and.naccn-nsubw.gt.naccd.and.naccn.gt.
     1      nacceta.and.(naccn.ge.min(naccr,4).or.(jtestm.ge.ndec-1.and.
     2      jtestdm.ge.ndec-1))) nacciop=1
        if(jtestm.lt.5.or.jtestdm.lt.5) nacciop=0     
        if(naccn-nsubw.le.naccr) nacciop=0
        if(nacciop.eq.0) go to 160
        write(40,150)
150     format(15x,'denominator calculated using wronskian.')
        nacceta=naccn-nsubw
        r2c=r2c*wront/wronc
        iterm=0
        if(r2c.ne.00.0e0_knd) iterm=int(log10(abs(r2c)))
        ir2e=ir2e+iterm
        r2c=r2c*10.0e0_knd**(-iterm)
        r2dc=r2dc*wront/wronc
        iterm=0
        if(r2dc.ne.00.0e0_knd) iterm=int(log10(abs(r2dc)))
        ir2de=ir2de+iterm
        r2dc=r2dc*10.0e0_knd**(-iterm)
160     if(abs(r2c).ge.1.0e0_knd) go to 170
        r2c=r2c*10.0e0_knd
        ir2e=ir2e-1
170     continue
        if(abs(r2dc).ge.1.0e0_knd) go to 180
        r2dc=r2dc*10.0e0_knd
        ir2de=ir2de-1
180     continue
        naccnmaxp=naccnmax
        if(naccnmax.gt.naccn.and.naccnmax.ge.3) iopnee=1
        if(naccn.gt.naccnmax) naccnmax=naccn
        if(ndec-nsubeta.lt.naccr+2) iopnee=2
        if(nacciop.eq.0.and.naccn.gt.nacceta) naccnmax=max(naccnmaxp,
     1      nacceta)
          if(nacceta.lt.3.and.naccmax.eq.0.and.ndec-nsubeta.ge.
     1        naccr) then
          iopnee=0
          neemark=nee
          end if
        if(ndec-nsubeta.ge.naccr+2) iopnee=0
        if(jtestm.eq.ndec.and.jtestdm.eq.ndec.and.naccn.le.max(naccr,5))
     1      iopnee=2
        if(naccns1.eq.ndec.or.naccns2.eq.ndec) iopnee=2
        return
        end
c
c
        subroutine geteig (l,m,c,eig2,eig3,eig4,eig5,eigval)
c
c  purpose:     To calculate an estimate of the eigenvalue.
c
c  parameters:
c
c     input:    l        : l
c               m        : m
c               c        : c
c               eig2-eig5: previous eigenvalues for l-4, l-3, l-2, and
c                          l-1
c
c     output:   eigval   : estimate of the eigenvalue
c
        use param
c
c  real(knd) scalars
        real(knd) c,csq,eigval,eig2,eig3,eig4,eig5,lam1,lam2,
     1            lam3,lam4,q,q2,q4,r,rl,rl2,rm,rm2,sm
c
        csq=c*c
        rl=l
        rl2=l+l
        rm=m
        rm2=m+m
        r=l-m
        q=2*(l-m)+1
        q2=q*q
        q4=q2*q2
        sm=m*m
c
c  special case for small m and large c
        if(l.eq.(m+4).and.c.gt.8.0e0_knd.and.m.lt.3) go to 60
c
c  if previous values have been computed use these to determine
c  the next value
        if(l.gt.(m+3)) go to 10
c
c  use expansion in terms of c**2 for low c, and
c  expansion in terms of c for large c (c>8)
        if(c.gt.80.0e0_knd) go to 60
        if(c.gt.6.0e0_knd.and.m.lt.4) go to 60
        if(c.gt.3.0e0_knd.and.l.eq.0) go to 60
        if(c.gt.4.0e0_knd.and.l.eq.1) go to 60
        if(c.gt.5.0e0_knd.and.l.eq.2) go to 60
        if(c.gt.5.0e0_knd.and.l.gt.(m+1)) go to 10
c
c  compute coefficients for c**2 expansion
        lam1=rl*(rl+1.0e0_knd)
        lam2=.5e0_knd*(1.0e0_knd-(rm2-1.0e0_knd)*(rm2+1.0e0_knd)/
     1        ((rl2-1.0e0_knd)*(rl2+3.0e0_knd)))
        lam3=.5e0_knd*(rl-rm-1.0e0_knd)*(rl-rm)*(rl+rm-1.0e0_knd)*
     1       (rl+rm)/((rl2-3.0e0_knd)*(rl2-1.0e0_knd)*(rl2-1.0e0_knd)
     2       *(rl2-1.0e0_knd)*(rl2+1.0e0_knd))-0.5e0_knd*(rl-rm+
     3       1.0e0_knd)*(rl-rm+2.0e0_knd)*(rl+rm+1.0e0_knd)*(rl+
     4       rm+2.0e0_knd)/((rl2+1.0e0_knd)*(rl2+3.0e0_knd)**3*
     5       (rl2+5.0e0_knd))
        lam4=(4.0e0_knd*rm*rm-1.0e0_knd)*((rl-rm+1.0e0_knd)*
     1       (rl-rm+2.0e0_knd)*(rl+rm+1.0e0_knd)*(rl+rm+2.0e0_knd)
     2       /((rl2-1.0e0_knd)*(rl2+1.0e0_knd)*(rl2+3.0e0_knd)**5*
     3       (rl2+5.0e0_knd)*(rl2+7.0e0_knd))-(rl-rm-1.0e0_knd)*
     4       (rl-rm)*(rl+rm-1.0e0_knd)*(rl+rm)/((rl2-5.0e0_knd)*
     5       (rl2-3.0e0_knd)*(rl2-1.0e0_knd)**5*(rl2+1.0e0_knd)*
     6       (rl2+3.0e0_knd)))
        eigval=lam1+csq*(lam2+csq*(lam3+lam4*csq))
        return
10      continue
c
c  pick the correct expansion (first through third order)
        if(l.gt.(3+m)) go to 50
        if(l.gt.(2+m)) go to 30
c
c  first order
        eigval=2.0e0_knd*eig5-eig4
20      return
c
c  second order
30      eigval=3.0e0_knd*eig5-3.0e0_knd*eig4+eig3
40      return
c
c  third order
50      eigval=4.0e0_knd*eig5-6.0e0_knd*eig4+4.0e0_knd*eig3-eig2
        return
60      continue
        ic=c
c
c  if m>6 the eigenvalues are very regularly spaced
        if(m.gt.6.and.l.gt.m+1) go to 10
c
c  n*(n+1) behavior reached?
        if(m.lt.(10+ic)) go to 70
        eigval=rl*(rl+1.0e0_knd)
        return
c
c  compute eigenvalue estimate with asymptotic expansion
70      eigval=q*c+sm-0.125e0_knd*(q2+5.0e0_knd)-q*(q2-32.0e0_knd*sm+
     1         11.0e0_knd)/(64.0e0_knd*c)-(5.0e0_knd*(q4+26.0e0_knd*q2+
     2         21.0e0_knd)-384.0e0_knd*sm*(q2+1.0e0_knd))/(1024.0e0_knd*
     3         csq)-(q*((33.0e0_knd*q4+1594.0e0_knd*q2+5621.0e0_knd)/
     4         (128.0e0_knd*128.0e0_knd)-sm*(37.0e0_knd*q2+167.0e0_knd)/
     5         (128.0e0_knd)+sm*sm/8.0e0_knd))/(csq*c)-((63.0e0_knd*q2*
     6         q4+4940.0e0_knd*q4+43327.0e0_knd*q2+22470.0e0_knd)/
     7         (256.0e0_knd*256.0e0_knd)-sm*(115.0e0_knd*q4+1310.0e0_knd
     8         *q2+735.0e0_knd)/512.0e0_knd+3.0e0_knd*sm*sm*(q2+
     9         1.0e0_knd)/8.0e0_knd)/(csq*csq)
        return
        end
c
c
        subroutine conver (l,m,c,limd,blist,glist,eig1,eig3,eig4,
     1                     ndec,maxd,eigval,eig5,enr,ienr)
c
c  purpose:     To determine a converged eigenvalue using the
c               boukwamp method.
c  parameters:
c
c     input:    l     : l
c               m     : m
c               c     : c
c               limd  : number of enr values computed
c               blist : array of coefficients used in recursion relation
c               glist : array of coefficients used in recursion relation
c               eig1  : previous eigenvalue for l-4
c               eig3  : previous eigenvalue for l-2
c               eig4  : previous eigenvalue for l-1
c               ndec  : number of decimal digits for real(knd)
c               maxd  : dimension of enr,blist,glist arrays
c               eigval: estimated value of the eigenvalue
c
c     output:   eigval: converged eigenvalue
c               eig5  : set equal to eigval
c               enr   : array of scaled ratios of successive d
c                       coefficients
c               ienr  : index n of last d coefficient ratio used
c                       in computing first term in the denominator
c                       of the eigenvalue correction
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) c,cll,clu,cora,corb,csq,de,dec,dl,eig1,eig3,eig4,
     1            eig5,eigdec,eigval,enrc,fl
        real(knd) blist(maxd),enr(maxd),glist(maxd)
c
        csq=c*c
        dec=10.0e0_knd**(-ndec-1)
        eigdec=10.0e0_knd**(-ndec+1)
        if(l.eq.m.or.l.eq.m+1) imax=1
c
c  set the original eigenvalue spacing estimate. this is arbitrary
        if(eigval.lt.eig4) eigval=eig4
        if(l.gt.m) cll=eig4
        if(l.eq.(m+2).or.l.eq.(m+3)) clu=eigval+0.5e0_knd*(eigval-eig3)
        if(l.gt.(m+3)) clu=eigval+0.5e0_knd*(eig3-eig1)
        lm2=(l-m)/2
        limdb=2*ienr+50
        if(l.eq.m.or.l.eq.m+1) limdb=2*ienr
        if(limdb.gt.limd) limdb=limd
c
c  begin Bouwkamp procedure
        fl=eigval
        jnde=0
        ix=l-m-2*lm2
        ifc=1
        lim2=limdb/2-ix
        iglim=lim2+1
        irio=lm2+1
        iw1=lm2+2
40      enr(1)=eigval-glist(1)
        if(lm2.lt.1) go to 60
c
c  evaluate the continued fraction
          do 50 i=1,lm2
          enr(i+1)=-blist(i)/enr(i)-glist(i+1)+eigval
50        continue
60      enr(lim2)=-blist(lim2)/(glist(iglim)-eigval)
        iw15=lim2-1
        ip=iw1+iw15
        if(iw15.lt.iw1) go to 80
c
c  evaluate the continued fraction
          do 70 i=iw1,iw15
          ipi=ip-i
          enr(ipi)=-blist(ipi)/(glist(ipi+1)-eigval+enr(ipi+1))
70        continue
80      if(ifc.eq.50) go to 130
        enrc=-blist(irio)/(glist(irio+1)-eigval+enr(irio+1))
        de=enrc*enrc/blist(irio)
        corb=de
        if(lim2.lt.iw1) go to 100
c
c  compute first sum in the denominator of the correction
          do 90 i=iw1,lim2
          de=enr(i)*enr(i)/blist(i)*de
          corb=corb+de
          if(abs(de/corb).lt.dec.and.(l.eq.m.or.l.eq.m+1.or.
     1       i.gt.ienr-20)) go to 100
90        continue
100     if((l.eq.m.or.l.eq.m+1).and.i.gt.imax) imax=i
        if(l.ne.m.and.l.ne.m+1.and.i.gt.ienr) ienr=i
        de=1.0e0_knd
        cora=de
        if(lm2.lt.1) go to 120
c
c  compute second term in the denominator of the correction
          do 110 i=1,lm2
          de=blist(irio-i)/(enr(irio-i)*enr(irio-i))*de
          cora=cora+de
          if(abs(de/cora).lt.dec) go to 120
110       continue
c
c  compute the correction to the eigenvalue
120     dl=(enrc-enr(irio))/(cora+corb)
        eigval=dl+eigval
c
c  eigenvalue accurate enough?
        if(abs(dl/eigval).lt.eigdec) go to 130
        ifc=ifc+1
        if(ifc.lt.50) go to 40
130     continue
c
c  is the eigenvalue the correct one
c  if not then modify the original guess
c
        if(l.eq.m.or.l.eq.(m+1)) go to 180
        if(eigval.gt.cll) go to 140
c
c  converged to next lower eigenvalue of the same parity
        cll=fl
        go to 170
140     if(eigval.lt.clu) go to 180
c
c  converged to the next higher eigenvalue of the same parity
        clu=fl
c
c  eigenvalue is now somewhere within the ranges established
c  above, repeat entire Bouwkamp procedure for a value from this
c  range (the mid-point)
170     eigval=0.5e0_knd*(cll+clu)
        fl=eigval
        ifc=1
        jnde=jnde+1
c
c  too many modifications are being made
c  error in the routine
        if(jnde.eq.50) go to 210
        go to 40
180     eig5=eigval
        if(l.eq.m.or.l.eq.m+1) ienr=imax
c
c  calculate the d coefficient ratios (enr)
        enr(1)=eigval-glist(1)
          do 190 i=1,lm2-1
          enr(i+1)=-blist(i)/enr(i)-glist(i+1)+eigval
190       continue
        lim2=limd/2-ix
        enr(lim2)=-blist(lim2)/(glist(lim2+1)-eigval)
        ilow=lm2+1
        iupp=lim2-1
        ip=ilow+iupp
          do 200 i=ilow,iupp
          ipi=ip-i
          enr(ipi)=-blist(ipi)/(glist(ipi+1)-eigval+enr(ipi+1))
200       continue
        return
c
c  error printout
210     continue
        write(20,220) l,c,m
        write(60,220) l,c,m
220     format(1x,'error in eigenvalue at l= ',i5,2x,'c = ',e25.15,
     1         2x,'m= ',i5)
        return
        end
c
c
        subroutine dnorm (l,m,c,ndec,nex,limd,maxd,enr,sgn,d01,id01,
     1                    dmfnorm,idmfe,dmlmf,idmlmfe,dmsnorm,idmse,
     2                    dmlms,idmlmse,jnorm,jsub)
c
c  purpose:     To compute d coefficient ratios from n values and to
c               calculate the normalization of the d coefficients.
c
c  parameters:
c
c     input:    l       : l
c               m       : m
c               c       : c
c               ndec    : number of decimal digits for real(knd)
c               nex     : maximum exponent available in real(knd)
c               limd    : approximately twice the maximum number
c                         of terms available to be taken in the sum
c               maxd    : dimension of enr array
c               enr     : array of ratios of scaled d coefficients
c
c     output:   enr     : array of ratios of d coefficients.
c                         enr(i) = ratio of the d coefficient with
c                         subscript 2*i+ix to the d coefficient with
c                         subscript 2*(i-1)+ix. Here ix =0 when l-m is
c                         even and ix=1 when l-m is odd.
c                         If the user needs the d coefficent ratios,
c                         they are available below right before
c                         statement 20.
c               sgn     : sign of the d coefficient for n=l-m
c               d01     : characteristic of ratio of first d
c                         coefficient (either d0 or d1, depending on
c                         whether l-m is even or odd) to the d
c                         coefficient of order equal to l-m
c               id01    : exponent corresponding to d01
c               dmfnorm : characteristic of the Morse-Feshbach
c                         normalization factor of the d coefficients.
c                         equal to the reciprocal of the value of the
c                         d coefficient d(n = l - m) using this
c                         normalization for the angular functions
c               idmfe   : exponent associated with dmfnorm
c               dmlmf   : characteristic of the d coefficient with
c                         subscript l - m for the Morse and Feshbach
c                         normalization
c               idmlmfe : exponent associated with dmlmf
c               dmsnorm : characteristic of Meixner-Schafke normalization
c                         sum of the d coefficients. Equal to the
c                         reciprocal of the square of the d coefficient
c                         d(n = l - m) using this normalization for the
c                         angular functions
c               idmse   : exponent associated with dmsnorm
c               dmlms   : characteristic of the d coefficient with index
c                         l-m in the Meixner-Schafke normalization
c               idmlmse : exponent associated with dmlms
c               jnorm   : maximum index of enr required for convergence
c                         of dmfnorm and dmsnorm
c               jsub    : number of decimal digits of subtraction error
c                         incurred in calculating dmfnorm
c
        use param
c
c  real(knd) scalars and array
        real(knd) aj,arr,c,coef,csq,dec,dmfnorm,dmsnorm,dmlmf,
     1            dmlms,d01,ea,rm2,sgn,sump,ten,term,teste,testeo
        real(knd) enr(maxd)
c
        ten=10.0e0_knd
        rm2=m+m
        rm2m1=m+m-1
        rm2p1=m+m+1
        rm2m3=m+m-3
        dec=ten**(-ndec-1)
        nfac=nex/3
        if(2*(nfac/2).ne.nfac) nfac=nfac-1
        teste=ten**nfac
        testeo=1.0e0_knd/teste
        csq=c*c
        lm2=(l-m)/2
        ix=l-m-2*lm2
        lim2=limd/2-ix
        sgn=1.0e0_knd
          do 20 i=1,lim2
          arr=ix+i+i
          ea=arr+arr+rm2
          if(i.gt.lm2) go to 10
          if(enr(i).lt.(0.0e0_knd))  sgn=sgn*(-1.0e0_knd)
10        enr(i)=(ea-1.0e0_knd)*(ea+1.0e0_knd)*enr(i)/((arr+rm2)*
     1             (arr+rm2-1.0e0_knd)*csq)
20        continue
c
c  compute the Morse-Feshbach normalizing factor
        dmfnorm=1.0e0_knd
        sump=1.0e0_knd
        term=1.0e0_knd
        jlow=l-m+2
        jterm=lm2
        iflag=0
        idmfe=0
          do 30 j=jlow,limd,2
          aj=j
          jterm=jterm+1
          term=term*(aj+rm2)*enr(jterm)*(aj+rm2-1.0e0_knd)/
     1         (aj*(aj-1.0e0_knd))
          if(term.gt.0.0e0_knd) sump=sump+term
          dmfnorm=dmfnorm+term
          if(abs(term/dmfnorm).lt.dec) go to 40
          if(abs(dmfnorm).lt.teste) go to 30
          dmfnorm=dmfnorm*testeo
          term=term*testeo
          sump=sump*testeo
          idmfe=idmfe+nfac
          iflag=1
30        continue
40      jlow=l-m
        jmf=jterm
        if(jlow.lt.2.or.iflag.eq.1) go to 60
        term=1.0e0_knd
        jterm=lm2
          do 50 j=jlow,2,-2
          aj=j
          term=term*aj*(aj-1.0e0_knd)/((aj+rm2
     1         -1.0e0_knd)*(aj+rm2)*enr(jterm))
          if(term.gt.0.0e0_knd) sump=sump+term
          jterm=jterm-1
          dmfnorm=dmfnorm+term
          if(abs(term/dmfnorm).lt.dec) go to 60
50        continue
60      continue
        if(dmfnorm.eq.0.0e0_knd) dmfnorm=dec
        jsub=int(log10(abs(sump/dmfnorm)+dec))
        if(jsub.lt.0) jsub=0
        if(jsub.gt.ndec) jsub=ndec
        iterm=0
        if(dmfnorm.ne.0.0e0_knd) iterm=int(log10(abs(dmfnorm)))
        idmfe=idmfe+iterm
        dmfnorm=dmfnorm*(10.0e0_knd**(-iterm))
        dmlmf=1.0e0_knd/dmfnorm
        idmlmfe=-idmfe
        write(40,70) lim2,jmf,jsub
70      format(8x,i6,' "d" coefficients; mf norm. converged in ',
     1         i6,' terms with ',i3,' digit subtr. error.')
c
c  compute the d0(c|ml) or d1(c|ml)
        id01=0
           d01=1.0e0_knd
        if(lm2.eq.0) go to 90
          do 80 kjl=1,lm2
          kkjl=lm2-kjl+1
              d01=d01/enr(kkjl)
                if(abs(d01).gt.teste) then
            d01=d01*testeo
            id01=id01+nfac
            end if
            if(abs(d01).lt.testeo) then
            d01=d01*teste
            id01=id01-nfac
            end if 
80        continue
        iterm=int(log10(abs(d01)))
        d01=d01*(10.0e0_knd**(-iterm))
        id01=id01+iterm
90      continue
c
c  compute the Meixner-Schafke normalizing factor
        jflag=0
        idmse=0
        dmsnorm=1.0e0_knd
        coef=1.0e0_knd
        jlow=l-m+2
        jterm=lm2
          do 150 j=jlow,limd,2
          aj=j
          aj2=aj+aj
          jterm=jterm+1
          coef=coef*(aj+rm2)*enr(jterm)*(aj+rm2m1)*enr(jterm)
     1         *(aj2+rm2m3)/(aj*(aj-1.0e0_knd)*(aj2+rm2p1))
          dmsnorm=dmsnorm+coef
          if(abs(coef/dmsnorm).lt.dec) go to 160
            if(abs(dmsnorm).gt.teste) then
            dmsnorm=dmsnorm*testeo
            coef=coef*testeo
            idmse=idmse+nfac
            jflag=1
            end if
150       continue
160     jlow=l-m
        jn=jterm
        jnorm=max(jmf,jn)
        if(jlow.lt.2.or.jflag.eq.1) go to 180
        coef=1.0e0_knd
        jterm=lm2
        j=jlow
          do 170 jj=2,jlow,2
          aj=j
          aj2=aj+aj
          coef=coef*aj*(aj-1.0e0_knd)*(aj2+rm2p1)/((aj2+rm2m3)*
     1            enr(jterm)*enr(jterm)*(aj+rm2)*(aj+rm2m1))
          jterm=jterm-1
          j=j-2
          dmsnorm=dmsnorm+coef
          if(abs(coef/dmsnorm).lt.dec) go to 180
170       continue
180     iterm=int(log10(dmsnorm))
        dmsnorm=dmsnorm*ten**(-iterm)
        idmse=idmse+iterm
          if(2*(idmse/2).ne.idmse) then
          idmse=idmse-1
          dmsnorm=ten*dmsnorm
          end if
        dmlms=sgn/sqrt(dmsnorm)
        idmlmse=-idmse/2
        write(50,190) jn,lim2
190     format(5x,' Meixner-Schafke normalization converged in ',
     1         i6,' terms; ',i6,' terms available.')
200     continue
        return
        end
c
c
        subroutine dalt (l,m,c,limdr,maxdr,maxmp,ndec,nex,ioppsum,
     1                   eigval,enrneg,drhor,dneg,idneg,nsdneg,nsdrho)
c
c  purpose:     To calculate d ratios with negative subscripts
c               and d-rho ratios.
c  parameters:
c
c     input:    l       : l
c               m       : m
c               c       : c
c               limdr   : number of ratios of successive d-rho
c                         coefficients calculated
c               maxdr   : dimension of drhor array
c               maxmp   : dimension of enrneg array
c               ndec    : number of decimal digits for real(knd)
c               nex     : maximum exponent for real(knd)
c               ioppsum : integer index = 0 if no d rho coefficients
c                         are calculated (psum not needed for r2leg)
c               eigval  : eigenvalue
c
c     output:   enrneg  : array of d coefficient ratios with
c                         negative subscripts
c               drhor   : array of d rho coefficient ratios
c               dneg    : characteristic of the ratio of the d
c                         coefficient with index -2m+ix to the
c                         d coefficient with index ix, where
c                         ix = 0 if l-m is even and ix = 1 if
c                         l-m is odd
c               idneg   : exponent (base 10) of dneg
c               nsdneg  : subtaction error in calculating dneg and
c                         maximum error in calculating the enrneg
c                         array
c               nsdrho  : maximum subtraction error in calculating
c                         the drhor array
c
        use param
c
c  real(knd) scalars
        real(knd) c,dneg,eigval,r,rm,rn,t,ten,teste,testeo,uterm,
     1            vterm,wterm
        real(knd) amnsdrho,ansdneg,ansdrho,asub,bsub
c
c  real(knd) arrays
        real(knd) enrneg(maxmp),drhor(maxdr)
c
        ten=10.0e0_knd
        nfac=nex/3
        if(2*(nfac/2).ne.nfac) nfac=nfac-1
        teste=ten**(nfac)
        testeo=1.0e0_knd/teste
c  if l-m is even, ix=0; if l-m is odd, ix=1
        ix=(l-m)-2*((l-m)/2)
        t=m+m-ix-ix
c
c  calculate ratios of d coefficients with negative subscripts
c
c       enrneg(k) = { d(-2m+2k-2)/d(-2m+2k), l-m even    }
c                   { d(-2m+1+2k-2)/d(-2m+1+2k), l-m odd }
c
        rm=m
          if(m.eq.0) then
          dneg=1.0e0_knd
          idneg=0
          go to 30
          end if
          do 10 i=1,m+1
          enrneg(i)=0.0e0_knd
10        continue
c
c  first calculate enrneg(1)
        n=2-2*m+ix
        rn=n
        r=n+m+m
        uterm=r*(r-1.0e0_knd)/((rn+r+1.0e0_knd)*(rn+r-1.0e0_knd))
        r=n+m-2
        vterm=(2.0e0_knd*r*(r+1.0e0_knd)-2.0e0_knd*rm*rm-
     1        1.0e0_knd)/((2.0e0_knd*r+3.0e0_knd)*(2.0e0_knd*r-
     2        1.0e0_knd))+(r*(r+1.0e0_knd)-eigval)/(c*c)
c
c       calculations continue up to and including
c       enrneg(k=m) = { d(-2)/d(0), l-m even }
c                     { d(-1)/d(1), l-m odd  }
c
        enrneg(1)=-uterm/vterm
        dneg=enrneg(1)
        idneg=0
        ansdneg=0.0e0_knd
        nsdneg=0
c
c  backward recursion beginning with enrneg(1) and
c  ending with enrneg(m)
c
          do i=2,2*m-2,2
          ii=i-2*m+ix
          n=ii+2
          j=i/2
          rn=n
          r=n+m+m
          uterm=r*(r-1.0e0_knd)/((rn+r+1.0e0_knd)*(rn+r-
     1               1.0e0_knd))
          r=n+m-2
          vterm=(2.0e0_knd*r*(r+1.0e0_knd)-2.0e0_knd*rm*rm-
     1          1.0e0_knd)/((2.0e0_knd*r+3.0e0_knd)*(2.0e0_knd*r-
     2          1.0e0_knd))+(r*(r+1.0e0_knd)-eigval)/(c*c)
          r=n-4
          wterm=(r+2.0e0_knd)*(r+1.0e0_knd)/((r+r+rm+rm+
     1                3.0e0_knd)*(r+r+rm+rm+1.0e0_knd))
          enrneg(j+1)=-uterm/(wterm*enrneg(j)+vterm)
          dneg=dneg*enrneg(j+1)
            if(wterm*enrneg(j)*vterm.ne.0.0e0_knd)
     1         then
            asub=log10(abs(vterm/(vterm+
     1                       wterm*enrneg(j))))
            if(asub.gt.0.0e0_knd) ansdneg=ansdneg+asub
            bsub=log10(abs(vterm/(wterm*enrneg(j))))
            if(bsub.gt.0.0e0_knd) ansdneg=max(0.0e0_knd,ansdneg-bsub)
            if(int(ansdneg).gt.nsdneg) nsdneg=ansdneg
            end if
            if(abs(dneg).gt.teste) then
            dneg=dneg*testeo
            idneg=idneg+nfac
            end if
            if(abs(dneg).lt.testeo) then
            dneg=dneg*teste
            idneg=idneg-nfac
            end if
          end do
          if(nsdneg.gt.ndec) nsdneg=ndec
        iterm=int(log10(abs(dneg)))
        dneg=dneg*(ten**(-iterm))
        idneg=idneg+iterm
c
c  calculate ratios of d rho coefficients
c
c       drhor(k-m) = { d(rho|2k)/d(rh0|2k-2), l-m even  }
c                    { d(rho|2k-1)/d(rho|2k-3), l-m odd }
c
30      if(ioppsum.eq.0) go to 60
          ansdrho=0.0e0_knd
          amnsdrho=0.0e0_knd
          nsdrho=0
          mnsdrho=0
          do 40 i=1,limdr
          drhor(i)=(0.0e0_knd,0.0e0_knd)
40        continue
          do 50 i=2*limdr,6,-2
          n=4-i+ix-m-m
          ii=(i-2)/2
          rn=n
          r=n+m+m
          uterm=r*(r-1.0e0_knd)/((rn+r+1.0e0_knd)*(rn+r-1.0e0_knd))
          r=n+m-2
          vterm=(2.0e0_knd*r*(r+1.0e0_knd)-2.0e0_knd*rm*rm-
     1           1.0e0_knd)/((2.0e0_knd*r+3.0e0_knd)*(2.0e0_knd*r-
     2           1.0e0_knd))+(r*(r+1.0e0_knd)-eigval)/(c*c)
          r=n-4
          wterm=(r+2.0e0_knd)*(r+1.0e0_knd)/((r+r+rm+rm+3.0e0_knd)*
     1          (r+r+rm+rm+1.0e0_knd))
          drhor(ii)=-uterm/(wterm*drhor(ii+1)+vterm)
            if(wterm*drhor(ii+1)*vterm.ne.0.0e0_knd)
     1         then
            asub=log10(abs(vterm/(vterm+wterm*drhor(ii+1))))
            if(asub.gt.0.0e0_knd) ansdrho=ansdrho+asub
            bsub=log10(abs(vterm/(wterm*drhor(ii+1))))
            if(bsub.gt.0.0e0_knd) ansdrho=max(0.0e0_knd,ansdrho-bsub)
            if(ansdrho.gt.amnsdrho) amnsdrho=ansdrho
            end if
50        continue
        n=-2*m+ix
        r=n+m-2
        vterm=(2.0e0_knd*r*(r+1.0e0_knd)-2.0e0_knd*rm*rm-1.0e0_knd)/
     1        ((2.0e0_knd*r+3.0e0_knd)*(2.0e0_knd*r-1.0e0_knd))+
     2        (r*(r+1.0e0_knd)-eigval)/(c*c)
        r=n-4
        wterm=(r+2.0e0_knd)*(r+1.0e0_knd)/((r+r+rm+rm+3.0e0_knd)*
     1        (r+r+rm+rm+1.0e0_knd))
c
c       the final value of ii is 1;
c       drhor(1) has a special value:
c       drhor(1) = { d(rho|2m+2)/d(-2m), l-m even  }
c                  { d(rho|2m+1)/d(-2m+1), l-m odd }
c
        drhor(1)=1.0e0_knd/((t-1.0e0_knd)*(t+1.0e0_knd)*
     1           (wterm*drhor(2)+vterm))
          if(wterm*drhor(2)*vterm.ne.(0.0e0_knd,0.0e0))
     1         then
          asub=log10(abs(vterm/(vterm+wterm*drhor(2))))
          if(asub.gt.0.0e0_knd) ansdrho=ansdrho+asub
          bsub=log10(abs(vterm/(wterm*drhor(2))))
          if(bsub.gt.0.0e0_knd) ansdrho=max(0.0e0_knd,ansdrho-bsub)
          if(ansdrho.gt.amnsdrho) amnsdrho=ansdrho
          end if
        nsdrho=int(amnsdrho)+1
        if(ix.eq.1) drhor(1)=-drhor(1)
60      continue
        return
        end
c
c
        subroutine gauss (ndec,n,x,w)
c
c  purpose:     To evaluate the coordinates and weighting factors
c               for an nth order Gaussian quadrature
c
c  parameters:
c
c     input:    ndec: number of decimal digits in real(knd) arithmetic;
c                     usually equal to 33
c               n   : order of quadrature
c
c     output:   x   : coordinate values for quadrature
c               w   : weighting factors
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) delta,der,pi,s,t,test,u,v,z
        real(knd) x(n),w(n)
c
        test=10.0e0_knd**(-ndec)
        imax=(n+1)/2
        pi=acos(-1.0e0_knd)
          do 40 i=1,imax
          z=cos(pi*(i-0.25e0_knd)/(n+0.5e0_knd))
            do 20 j=1,30
            u=0.0e0_knd
            v=1.0e0_knd
              do 10 k=1,n
              t=u
              u=v
              v=((k+k-1)*z*u-(k-1)*t)/k
10                 continue
            s=z*z-1.0e0_knd
            der=n*(z*v-u)/s
            delta=-v/der-0.5e0_knd*v*v*((n*n*s-n*z*z-n)*v+
     1             2.0e0_knd*n*z*u)/(der*der*der*s*s)
            z=z+delta
            if(abs(delta/z).lt.test) go to 30
20          continue
30        continue
          x(i)=-z
          x(n+1-i)=z
          w(i)=2.0e0_knd/((1.0e0_knd-z*z)*der*der)
          w(n+1-i)=w(i)
40          continue
        return
        end
c
c
        subroutine pleg (m,lim,maxp,limcsav,iopd,ndec,nex,barg,narg,
     1                   maxt,pr,pdr,pdnorm,ipdnorm,pnorm,ipnorm,alpha,
     2                   beta,gamma,coefa,coefb,coefc,coefd,coefe)
c
c  purpose:     To calculate ratios of successive associated Legendre
c               functions of the first kind for given arguments barg.
c               to calculate corresponding ratios of their first
c               derivatives. To calculate the characteristics and
c               exponents of both the Legendre functions of the first
c               kind and their first derivatives.
c
c  parameters:
c
c     input:    m      : m
c               lim    : two less than the number of associated Legendre
c                        function ratios calculated for given arguments
c               maxp   : dimension of alpha, beta, gamma, coefa, coefb,
c                        coefc, coefd, and coefe arrays and second
c                        dimension of pr and pdr arrays
c               limcsav: integer equal to the number of coefficients in
c                        each of the arrays alpha, beta, gamma, coefa,
c                        coefb, coefc, coefd, and coefe arrays that
c                        have already been calculated in earlier calls
c                        to pleg for this value of m and will not be
c                        calculated again. [Note that the minimum
c                        array index for the coefficients is 3 and
c                        the maximum array index is limcsav+2]
c               iopd   : integer that is set = 0 if derivatives of
c                        Legendre functions (i.e., their ratios)
c                        are not required when iopang = 1 and the
c                        first derivatives of the angular functions
c                        are not requested.
c                        iopd is set = 1 when iopang = 2 and pleg is
c                        also being used to obtain ratios of first
c                        derivatives of Legendre functions for use in
c                        computing the first derivatives of the angular
c                        functions.
c                        iopd is set = 2 when pleg is being used to
c                        compute ratios of Legendre functions for use in
c                        the calculation of the denominator term used in
c                        calculating the radial functions of the second
c                        kind and their first derivatives in r2eta.
c                        iopd is set = 3 when pleg is being used to
c                        compute ratios of both the Legendre functions
c                        and their first derivatives for use in the
c                        calculation of the numerator terms used
c                        in r2eta and in r2leg to calculate the radial
c                        functions of the second kind and their first
c                        deriatives.
c               ndec   : number of decimal digits for real(knd)
c               nex    : maximum exponent for real(knd)
c               barg   : array of narg values of eta for which Legendre
c                        functions are to be calculated
c               narg   : number of specified values of eta in barg array
c               maxt   : dimension of barg array
c
c     output:   pr     : array of ratios of successive first kind
c                        associated Legendre functions of the same
c                        parity
c               pdr    : array of ratios of successive derivatives of
c                        first kind associated Legendre functions of
c                        the same parity
c               pdnorm : array of characteristics of the first
c                        derivatives of associated Legendre functions
c                        of the first kind of order m and degree m
c               ipdnorm: array of exponents of the first derivatives
c                        of associated Legendre functions of the first
c                        kind of order m and degree m
c               pnorm  : array of characteristics of the associated
c                        Legendre functions of the first kind of order m
c                        and degree m
c               ipnorm : array of exponents of the associated Legendre
c                        functions of the first kind of order m and
c                        degree m
c
c     input/output:
c               alpha  : array of coefficients in the recursion
c                        formula for the associated Legendre functions
c               beta   : array of coefficients in the recursion
c                        formula for the associated Legendre functions
c               gamma  : array of coefficients in the recursion
c                        formula for the associated Legendre functions
c               coefa  : array of coefficients in the expression
c                        relating the derivative ratios pdr to the
c                        function ratios pr
c               coefb  : array of coefficients in the expression
c                        relating the derivative ratios pdr to the
c                        function ratios pr
c               coefc  : array of coefficients in the expression
c                        relating the derivative ratios pdr to the
c                        function ratios pr
c               coefd  : array of coefficients in the expression
c                        relating the derivative ratios pdr to the
c                        function ratios pr
c               coefe  : array of coefficients in the expression
c                        relating the derivative ratios pdr to the
c                        function ratios pr
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) adec,ajterm,am2p1,anden1,anden2,an2tnp1,bargs,den,rm,
     1            rm2,temp1,temp2,temp3,ten,term,teste,testeo,ta,tb,tc,
     2            t1,t2
        real(knd) alpha(maxp),barg(maxt),beta(maxp),coefa(maxp),
     1            coefb(maxp),coefc(maxp),coefd(maxp),coefe(maxp),
     2            gamma(maxp),pdnorm(maxt),pdr(maxt,maxp),pdr1(maxp),
     3            pr(maxt,maxp),pnorm(maxt)
c
c  integer array
        dimension ipdnorm(maxt),ipnorm(maxt)
c
        ten=10.0e0_knd
        adec=ten**(-ndec+2)
        nfac=nex/2
        if(2*(nfac/2).ne.nfac) nfac=nfac-1
        teste=ten**nfac
        testeo=1.0e0_knd/teste
        rm=m
        rm2=m*m
        am2p1=m+m+1
        m2=m+m
        m2m1=m2-1
        mm1=m-1
        mm2=m-2
        msqp1=2*m*m+1
c
c  calculate the coefficients alpha(j), beta(j), and gamma(j) for
c  the three term recursion relating the Legendre function ratios
c
c              m                m
c   pr(k,j) = p    (barg(k)) / p  (barg(k))
c              m+j-1            m+j-3
c
c  and calculate the coefficients coefa(j), coefb(j), coefc(j),
c  coefd(j), and coefe(j) in the expression used to calculate
c  ratios of Legendre function derivatives
c
c               m                 m
c   pdr(k,j) = p'    (barg(k)) / p'  (barg(k))
c               m+j-1             m+j-3
c
          if(limcsav.ge.lim) go to 30
          do 10 j=limcsav+3,lim+2
                n=m+j-3
                n2=n+n
                n2p3=n2+3
                n2p1=n2+1
                n2m1=n2-1
                nmmm2=n-mm2
                nmmm1=n-mm1
                npmm1=n+mm1
                npm=n+m
                npmm1=n+mm1
          npmp1=n+m+1
          npmp2=npmp1+1
                an2tnp1=real(2*n,knd)*real((n+1),knd)
                anden1=real(nmmm2,knd)*real(nmmm1,knd)
                anden2=real(n2m1,knd)*anden1
                alpha(j)=real(n2p3,knd)*real(n2p1,knd)/anden1
                beta(j)=real(n2p1,knd)*(real(msqp1,knd)-an2tnp1)/anden2
                gamma(j)=-real(n2p3,knd)*real(npm,knd)*real(npmm1,knd)/anden2
          coefa(j)=-real(npmp2,knd)/real(nmmm1,knd)
          coefb(j)=real(n2p3,knd)*real((n+2),knd)/anden1
          coefc(j)=-real(npmp1,knd)*real(npmp2,knd)/anden1
          coefd(j)=real(npmp1,knd)/real(nmmm2,knd)
          coefe(j)=-real((n+1),knd)*real(n2p3,knd)/anden1
10        continue
        gamma(3)=0.0e0_knd
        gamma(4)=0.0e0_knd
        term=1.0e0_knd
        iterm=0
        if(m.lt.2) go to 30
          do jm=2,m
                term=(jm+jm-1)*term
            if(term.gt.teste) then
                  term=term*testeo
                  iterm=iterm+nfac
            end if
          end do
        jterm=int(log10(term))
        term=term*(ten**(-jterm))
        iterm=iterm+jterm
30      continue
c
c   calculate the ratios of successive Legendre functions of the same
c   parity using the three term recursion relationship
c
c   pr(k,j) = alpha(j)*barg(k)*barg(k) + beta(j) + gamma(j)/pr(k,j-2)
c
          do 140 k=1,narg
                pnorm(k)=term
          ipnorm(k)=iterm
          pdnorm(k)=term
          ipdnorm(k)=iterm
c
c   define the first two ratios equal to unity and (2m+1)*barg(k)
          pr(k,1)=1.0e0_knd
          pr(k,2)=am2p1*barg(k)
          jdelta=1
          if(abs(barg(k)).lt.adec) jdelta=2
          bargs=barg(k)*barg(k)
            do 40 j=3,lim+2,jdelta
            pr(k,j)=alpha(j)*bargs+beta(j)+(gamma(j)/pr(k,j-2))
40          continue
c
c   calculate the corresponding ratios of first derviatives of
c   successive Legendre functions of the same parity using the
c   following relationship (except for eta equal to zero or unity,
c   where special expressions are used, and except for when the
c   magnitude of the argument barg is less than or equal to 0.1,
c   where recursion on the ratios of successive first derivatives
c   of the same parity is used instead)
c
c              (coefa(j)+coefb(j)*barg(k)*barg(k))*pr(k,j)+coefc(j)
c   pdr(k,j) = ----------------------------------------------------
c                  pr(k,j)+coefd(j)+coefe(j)*barg(k)*barg(k)
c
          if(iopd.eq.0.or.iopd.eq.2) go to 120
          pdr(k,1)=1.0e0_knd
          pdr(k,2)=1.0e0_knd
          if(abs(barg(k)).ge.adec) go to 50
          pdr(k,2)=am2p1
            do j=4,lim+2,2
            pdr(k,j)=-real(m2m1+j,knd)/real(j-2,knd)
            end do
          go to 140
50        if(abs(abs(barg(k))-1.0e0_knd).ge.adec) go to 70
          if(m.eq.0) go to 60
          if(m.ne.2) go to 130
          pdr(k,1)=-2.0e0_knd*barg(k)
          go to 80
60        temp1=1.0e0_knd
          temp2=3.0e0_knd
          pdr(k,2)=1.0e0_knd
          pdr(k,3)=3.0e0_knd*barg(k)
            do j=4,lim+2
            temp3=temp2+j-1
            pdr(k,j)=temp3/temp1
            temp1=temp2
            temp2=temp3
            end do
          go to 140
70        if(m.ne.0) go to 80
          pdr(k,1)=1.0e0_knd
          pdr(k,2)=1.0e0_knd
          pdr(k,3)=3.0e0_knd*barg(k)
          jlow=4
          go to 90
80        pdr(k,2)=am2p1*((rm+1.0e0_knd)*bargs-1.0e0_knd)/(rm*barg(k))
          jlow=3
90        continue
          if(abs(barg(k)).le.0.1e0_knd) go to 110
            do 100 j=jlow,lim+2
            den=(pr(k,j)+coefd(j)+coefe(j)*bargs)
            if(den.eq.0.0e0_knd) den=1.0e-50_knd
            pdr(k,j)=((coefa(j)+coefb(j)*bargs)*pr(k,j)+coefc(j))/den
100         continue
          go to 120
110       continue
          if(m.ne.0) pdr1(1)=pdr(k,2)
          if(m.eq.0) pdr1(2)=pdr(k,3)
            do j=jlow-1,lim+1
            n=j+m-1
            t1=bargs-1.0e0_knd
            t2=(n*n*t1+rm2)
            ta=j*t2
            tb=(n+n+1)*barg(k)*(t2+n*t1)
            tc=-(n+m)*(t2+(n+n+1)*t1)
            pdr1(j)=(tb+tc/pdr1(j-1))/ta
            end do
            do j=jlow,lim+2
            pdr(k,j)=pdr1(j-2)*pdr1(j-1)
            end do
120       if(m.eq.0.or.iopd.eq.2.or.iopd.eq.3) go to 140
          if(abs(abs(barg(k))-1.0e0_knd).lt.adec) go to 130
          ajterm=rm*log10(1.0e0_knd-bargs)/2.0e0_knd
          jterm=int(ajterm)
          ipnorm(k)=ipnorm(k)+jterm
          pnorm(k)=pnorm(k)*(ten**(ajterm-jterm))
          if(iopd.eq.0) go to 130
          ajterm=log10(rm*abs(barg(k)))+(rm-2.0e0_knd)*
     1           log10(1.0e0_knd-bargs)/2.0e0_knd
          jterm=int(ajterm)
          ipdnorm(k)=ipdnorm(k)+jterm
          pdnorm(k)=-pdnorm(k)*(ten**(ajterm-jterm))
          if(barg(k).lt.0.0e0_knd) pdnorm(k)=-pdnorm(k)
          go to 140
130       pnorm(k)=0.0e0_knd
          ipnorm(k)=0
          if(m.ne.2) pdnorm(k)=0.0e0_knd
          if(m.ne.2) ipdnorm(k)=0
140       continue
        return
        end
c
c
        subroutine qleg (m,lnum,limq,maxq,x1,ndec,qdr,qdml,iqdml,qdl,
     1                   iqdl,qr,qml,iqml,ql,iql,termpq,itermpq)
c
c  purpose:     To calculate ratios of successive associated Legendre
c               functions of the second kind for given c,x, and m.
c               to calculate corresponding ratios of their first
c               derivatives. To calculate the characteristics and
c               exponents of both the Legendre functions of the second
c               kind and their first derivatives.
c
c  parameters:
c
c     input:    m      : m
c               lnum   : number of l values desired (=lmax+1);
c                        also equal to the dimension of the arrays
c                        ql, iql, qdl, and iqdl
c               limq   : the number of associated Legendre function
c                        ratios calculated for given m,lnum,c,ndec,
c                        and x1
c               maxq   : dimension of qr and qdr arrays
c               x1     : x - 10.0e0_knd
c               ndec   : number of decimal digits for real(knd)
c
c     output:   qdr    : ratios of derivatives of successive
c                        associated Legendre functions of the second
c                        kind
c               qdml   : characteristic of the first derivative of
c                        the associated Legendre function of the second
c                        kind with order m and degree m-1
c               iqdml  : exponent corresponding to qdml
c               qdl    : characteristic of the first derivative of
c                        the associated Legendre function of the second
c                        kind with order m and degree m
c               iqdl   : exponent corresponding to qdl
c               qr     : ratios of successive associated Legendre
c                        functions of the second kind
c               qml    : characteristic of the associated Legendre
c                        function of the second kind with order m
c                        and degree m-1
c               iqml   : exponent corresponding to qml
c               ql     : characteristic of the associated Legendre
c                        function of the second kind with order m and
c                                                           -m/2
c                        degree m, scaled by (2m-1)!!(x*x-1)
c               iql    : exponent corresponding to ql
c               termpq : characteristic of the relative size of the
c                        maximum terms in the positive degree q series
c                        and the p series used to calculate r2 and r2d
c                        in subroutine r2leg
c               itermpq: exponent corresponding to termpq
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) ajm,dec,qdml,qlow,qml,qupp,q00,q11,rin,rm,
     1            term,termpq,tjm,tm,tmr,x,x1,x1d,xsqr
        real(knd) qdl(lnum),qdr(maxq),ql(lnum),qr(maxq)
c
c  integer arrays
        dimension iqdl(lnum),iql(lnum)
c
        dec=10.0e0_knd**(-ndec)
        rm=m
        tm=rm+rm
        tmr=tm/(tm+1.0e0_knd)
        x=x1+1.0e0_knd
        x1d=(x+1.0e0_knd)*x1
        xsqr=sqrt(x1d)
        mxqrest=limq+ndec*int((1.0e0_knd-1.0e0_knd/log10(x-xsqr)))
        if(m.eq.0) mlimq=50000*ndec+limq
        if(m.eq.1) mlimq=12000*ndec+limq
        if(m.eq.2) mlimq=5000*ndec+limq
        if(m.eq.3) mlimq=600*ndec+limq
        if(m.ge.4) mlimq=100*ndec+limq
        if(m.eq.1.and.x1.lt.1.0e-9_knd) mlimq=50000*ndec+limq
        mxqr=min(mxqrest,mlimq)
        mxqrpm=mxqr+m
        write(40,5) mxqrpm
5       format(15x,'used backward recursion to calculate ratios of q',
     1         ' functions starting at order',i8)
c
c                              m    m
c  calculate ratios qr(k+m) = q  / q    ,k=m+limq to k=m+1 using
c                              k    k-1
c
c                                         m       m               1/2
c  backward recursion from qr(maxm+2m) = q     / q      =x-(x*x-1),
c                                         mxqr+m  mxqr+m-1
c
c  where the last quantity is the asymptotic limit of the ratio as
c  mxqr approaches infinity
c
        qupp=x-xsqr
          do jn=mxqr+m,limq+m+1,-1
          rin=jn
          qlow=(rin+rm-1.0e0_knd)/(x*(rin+rin-1.0e0_knd)
     1         -(rin-rm)*qupp)
          qupp=qlow
          end do
        qr(limq+m+m)=qupp
          do 10 jn=limq+m,m+2,-1
          rin=jn
          qr(jn+m-1)=(rin+rm-1.0e0_knd)/(x*(rin+rin-1.0e0_knd)
     1               -(rin-rm)*qr(jn+m))
10        continue
c
c                              m     m
c  calculate ratios qr(k+m) = q   / q   ,k=m-1 to k=-m+1 using
c                              k-1   k
c
c                                       m      m
c  backward recursion from qr(m-1+m) = q    / q     = x
c                                       m-2    m-1
c
20      if(m.eq.0) go to 100
        qr(m+m-1)=x
        if(m.eq.1) go to 40
          do 30 jn=m-1,2-m,-1
          rin=jn
          qr(jn+m-1)=(x*(rin+rin-1.0e0_knd)
     1               -((rin-rm)/qr(jn+m)))/(rin+rm-1.0e0_knd)
30        continue
40      continue
c
c                  m
c  calculation of q    , m > 0 by forward division of qr ratios
c                  m-1
c
c                 m
c  starting with q  calculated from its closed form expression,
c                 0
c
c                           -m/2
c  scaled by (2m-1)!!(x*x-1).
c
        qml=rm*log10(x+1.0e0_knd)-log10(2.0e0_knd)
        iqterm=int(rm*log10(x1/(x+1.0e0_knd)))
        if(iqterm.lt.-ndec) go to 50
        qml=qml+log10(1.0e0_knd-((x1/(x+1.0e0_knd))**m))
50      continue
        term=1.0e0_knd
        iterm=0
        if(m.lt.2) go to 70
          do jm=2,m
          ajm=jm
          term=term*(ajm-1.0e0_knd)/(ajm+ajm-1.0e0_knd)
          if(term.gt.dec) go to 60
          term=term/dec
          iterm=iterm-ndec
60        continue
          end do
70      term=log10(term)
        qml=qml+term+iterm
        iqml=int(qml)
        qml=10.0e0_knd**(qml-iqml)
        if(2*(m/2).ne.m) qml=-qml
        if(m.lt.2) go to 90
          do jm=1,m-1
          qml=qml/qr(jm+m)
          if(abs(qml).gt.dec) go to 80
          qml=qml/dec
          iqml=iqml-ndec
80        continue
          end do
90      continue
        iterm=int(log10(abs(qml)))
        qml=qml*10.0e0_knd**(-iterm)
        iqml=iqml+iterm
c
c                  m
c  calculation of q   by forward recursion in m starting with values
c                  m
c       0       1
c  for q   and q  obtained from their closed form expressions, scaled
c       0       1
c                    -m/2
c  by (2m-1)!!(x*x-1).
c
100     q00=0.5e0_knd*log((x+1.0e0_knd)/x1)
        if(m.ne.0) go to 110
        ql(1)=q00
        go to 130
110     q11=x1d*q00-x
        if(m.ne.1) go to 120
        ql(1)=q11
        go to 130
120     qlow=q00
        qupp=q11
          do jm=1,m-1
          tjm=real((jm+jm),knd)/(jm+jm+1)
          ql(1)=(x1d-tjm)*qupp+tjm*x1d*qlow
          qlow=qupp
          qupp=ql(1)
          end do
130     iql(1)=int(log10(abs(ql(1))))
        ql(1)=ql(1)*(10.0e0_knd**(-iql(1)))
c
c  calculation of ratios of the first derivatives of q with respect
c  to x, using the relationships:
c
c                  m    m      [kx]qr(k+m)-(k+m)
c     qdr(k+m) = q'  / q'   =  ----------------- , k=m+lim to k=m+1
c                  k    k-1    [(k-m)]qr(k+m)-kx
c
c                  m      m    [(k-m)]qr(k+m)-kx
c     qdr(k+m) = q'   / q'  =  ----------------- , k=m-1 to k=-m+1
c                  k-1    k    [kx]qr(k+m)-(k+m)
c
          do jm=m+1,m+limq
          ajm=jm
          qdr(jm+m)=(ajm*x*qr(jm+m)-(ajm+rm))/((ajm-rm)*qr(jm+m)-ajm*x)
          end do
c
        if(m.eq.0) go to 140
          do jm=1-m,m-1
          ajm=jm
          qdr(jm+m)=(ajm*x*qr(jm+m)-(ajm-rm))/((ajm+rm)*qr(jm+m)-ajm*x)
          end do
140     continue
c
c                   m         m                      m        m
c  calculation of q'    and q'  from the values for q    and q  .
c                   m-1       m                      m-1      m
c
        if(m.gt.0) go to 150
        qdl(1)=-1.0e0_knd/x1d
        iqdl(1)=0
        go to 160
150     qdml=-rm*x*qml/x1d
        iterm=int(log10(abs(qdml)))
        qdml=qdml*(10.0e0_knd**(-iterm))
        iqdml=iqml+iterm
        qdl(1)=rm*(x*ql(1)-2.0e0_knd*qml*(10.0e0_knd**(iqml-iql(1))))
     1         /x1d
        iqdl(1)=iql(1)
160     continue
        m2m1=m+m-1
          do jl=2,lnum
          ql(jl)=ql(jl-1)*qr(m2m1+jl)
          iql(jl)=iql(jl-1)
          if(abs(ql(jl)).gt.1.0e-10_knd) go to 170
          ql(jl)=ql(jl)*1.0e10_knd
          iql(jl)=iql(jl)-10
170       qdl(jl)=qdl(jl-1)*qdr(m2m1+jl)
          iqdl(jl)=iqdl(jl-1)
          if(abs(qdl(jl)).gt.1.0e-10_knd) go to 180
          qdl(jl)=qdl(jl)*1.0e10_knd
          iqdl(jl)=iqdl(jl)-10
180       end do
        termpq=rm*log10(xsqr)
        itermpq=int(termpq)
        termpq=10.0e0_knd**(termpq-itermpq)
        return
        end
c
c
       subroutine pint (c,m,lnum,x1,limint,maxint,maxlp,maxmp,ndec,
     1                  wg,xg,ngau,ngqs,rpint1,rpint2,pint1,pint2,
     2                  pint3,pint4,norme,pnorm,ipnorm,coefme,coefmo)
c
c  purpose:     To calculate integrals of the product of associated
c               Legendre functions and kernels containing spherical
c               Neumann functions and a window function. Four
c               different kernel functions are involved leading to
c               integrals of four different types. The integrals are
c               calculated using gaussian quadrature.
c
c  parameters:
c
c     input:    c      : c
c               m      : m
c               lnum   : number of l values desired
c               x1     : x - 10.0e0_knd
c               limint : number of integrals of each of the four types
c                        required
c               maxint : dimension of the integral arrays
c               maxlp  : dimension of characteristic and exponent
c                        arrays of integrals
c               maxmp  : dimension of the spherical Neumann function
c                        array
c               ndec   : number of decimal digits for real(knd)
c               wg     : gaussian quadrature weighting factors
c               xg     : gaussian quadrature arguments
c               ngau   : order of gaussian quadrature
c               ngqs   : number of gaussian quadrature steps the
c                        integrand is divided into
c
c     output:   rpint1 : array of ratios of successive integrals of
c                        the same parity of the first type (l-m even)
c                        or of the third type (l-m odd)
c               rpint2 : array of ratios of successive integrals of
c                        the same parity of the second type (l-m even)
c                        or of the fourth type (l-m odd)
c               pint1  : array of scaled values for the integrals of
c                        the first type
c               pint2  : array of scaled values for the integrals of
c                        the second type
c               pint3  : array of scaled values for the integrals of
c                        the third type
c               pint4  : array of scaled values for the integrals of
c                        the fourth type
c               norme  : array of scaling exponents for the spherical
c                        Neumann functions
c               pnorm  : array of characteristics for the scaling
c                        factors used for the associated Legendre
c                        functions
c               ipnorm : array of exponents for the scaling factors
c                        used for the associated Legendre functions
c               coefme : coefficient for the expression for r2 and r2d
c                        using the integration method (l-m even)
c               coefmo : coefficient for the expression for r2 and r2d
c                        using the integration method (l-m odd)
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) ak,amo2,an,arg,argb,arn,bn,c,coef,coefme,
     1            coefmo,coefo,coef1,coef2,coef4,darg,dec,etai,
     2            etaim1,etais,etal,etau,etcoef1,etcoef2,rm,rn,sargb,
     3            scal,sneu1,sneu2,sneu3,stemp0,step,step0,step1,step2,
     4            twom,twomi,x1,x1sm1
        real(knd) alpha(maxint),beta(maxint),p(maxint),pint1(maxint),
     1            pint2(maxint),pint3(maxint),pint4(maxint),
     2            pnorm(maxlp),rpint1(maxint),rpint2(maxint),
     3            sneun(maxmp),wg(ngau),xg(ngau),ynormn(maxmp)
c
c  integer arrays
        dimension norme(maxmp),ipnorm(maxlp)
c
        x1sm1=x1*(x1+2.0e0_knd)
        dec=10.0e0_knd**(-ndec-1)
        amo2=0.5e0_knd*m
        lim=limint
        step0=1.0e0_knd/ngqs
        coefme=(m*(x1+1.0e0_knd))/x1sm1
        coefmo=(m*(x1+1.0e0_knd)**2+x1sm1)/((x1+1.0e0_knd)*x1sm1)
        rm=m
        if(x1.ge.0.1e0_knd) go to 10
        step1=1.0e0_knd/(ngqs*(1.0e0_knd-3.0e0_knd*log10(x1)*
     1        (1.0e0_knd+3.0e0_knd*log10(rm+1.0e0_knd))))
        if(step1.gt.step0) step1=step0
        go to 20
10      step1=step0
20      step2=(1.0e0_knd-step1)/(ngqs-1)
        write(40,30) ngau,step1
30      format(11x,'order of gauss quadrature =',i4,'. first step'
     1         ' size = 'f10.6,'.')
c
c  calculation of scaling factors for the associated Legendre functions
        pnorm(1)=1.0e0_knd
        pnorm(2)=1.0e0_knd
        ipnorm(1)=0
        ipnorm(2)=0
        if(m.eq.0) go to 50
          do 40 n=1,m
          an=n+n
          bn=n+n+1
          pnorm(1)=pnorm(1)*an
          pnorm(2)=pnorm(2)*bn
          iterm1=int(log10(pnorm(1)))
          iterm2=int(log10(pnorm(2)))
          pnorm(1)=pnorm(1)*10.0e0_knd**(-iterm1)
          pnorm(2)=pnorm(2)*10.0e0_knd**(-iterm2)
          ipnorm(1)=ipnorm(1)+iterm1
          ipnorm(2)=ipnorm(2)+iterm2
40          continue
50        twom=m+m
        pnorm(3)=pnorm(1)*(twom+2)/2
        iterm3=int(log10(pnorm(3)))
        pnorm(3)=pnorm(3)*10.0e0_knd**(-iterm3)
        ipnorm(3)=iterm3+ipnorm(1)
        if(lnum.lt.4) go to 70
          do 60 il=4,lnum,2
          pnorm(il)=pnorm(il-2)*(twom+il-1)/(il-1)
          pnorm(il+1)=pnorm(il-1)*(twom+il)/(il)
          iterm1=int(log10(pnorm(il)))
          iterm2=int(log10(pnorm(il+1)))
          ipnorm(il)=ipnorm(il-2)+iterm1
          ipnorm(il+1)=ipnorm(il-1)+iterm2
          pnorm(il)=pnorm(il)*10.0e0_knd**(-iterm1)
          pnorm(il+1)=pnorm(il+1)*10.0e0_knd**(-iterm2)
60          continue
70        continue
c
c  calculation of the coefficients in the recursion relation used
c  for the scaled associated Legendre functions
        alpha(1)=(twom+1.0e0_knd)*pnorm(1)/pnorm(2)
        alpha(1)=alpha(1)*10.0e0_knd**(ipnorm(1)-ipnorm(2))
        beta(1)=0.0e0_knd
        alpha(2)=(twom+3.0e0_knd)*pnorm(2)/(pnorm(3)*2.0e0_knd)
        alpha(2)=alpha(2)*10.0e0_knd**(ipnorm(2)-ipnorm(3))
        beta(2)=-(twom+1.0e0_knd)/(twom+2.0e0_knd)
          do 80 il=3,lim+2
          alpha(il)=alpha(il-2)*(twom+il-1)*(twom+il+il-1)*
     1    (il-2)/((il-1)*(twom+il)*(twom+il+il-5))
          beta(il)=-(twom+il-1)/(twom+il)
80          continue
c
          do 90 il=1,lim+2,2
          pint1(il)=0.0e0_knd
          pint2(il)=0.0e0_knd
          pint3(il+1)=0.0e0_knd
          pint4(il+1)=0.0e0_knd
90          continue
c
c  calculation of the scaling exponents for the spherical Bessel
c  functions required for the four types of integrals
        twomi=1.0e0_knd
        if(m.eq.0) go to 110
          do 100 n=1,m
100          twomi=twomi*(n+n-1)/(n+n)
110        continue
        arg=c*sqrt(x1*(x1+2.0e0_knd))
        darg=1.0e0_knd/arg
        stemp0=-cos(arg)*darg
        norme(1)=int(log10(abs(stemp0)))
        ynormn(1)=stemp0*10.0e0_knd**(-norme(1))
        ynormn(2)=stemp0*darg-sin(arg)*darg
        norme(2)=norme(1)
        ynormn(2)=ynormn(2)*10.0e0_knd**(-norme(1))
c
          do 120 n=3,m+3,2
          rn=n+n-3
          arn=n+n-1
          ynormn(n)=-ynormn(n-2)+darg*rn*ynormn(n-1)
          ynormn(n+1)=-ynormn(n-1)+darg*arn*ynormn(n)
          norme(n+1)=log10(abs(ynormn(n+1)))
          scal=10.0e0_knd**(-norme(n+1))
          ynormn(n+1)=ynormn(n+1)*scal
          ynormn(n)=ynormn(n)*scal
          norme(n+1)=norme(n+1)+norme(n-1)
          norme(n)=norme(n+1)
120       continue
c
c  gaussian quadrature integration loops. first dividing integrand
c  into ngqs steps
          do 180 k=1,ngqs
          ak=k
          if(k.ne.1) go to 130
          etal=0.0e0_knd
          etau=step1
          step=step1
          go to 140
130       etal=step1+(ak-2.0e0_knd)*step2
          etau=etal+step2
          step=step2
140       etcoef1=(etau+etal)/2.0e0_knd
           etcoef2=(etau-etal)/2.0e0_knd
c
c  gaussian quadrature integration over each step
            do 170 i=1,ngau
             etai=etcoef1+xg(i)*etcoef2
            etais=etai*etai
            etaim1=1.0e0_knd-etais
            argb=x1sm1+etais
            coef=step*((x1sm1*etaim1/argb)**amo2)
            if(coef.lt.dec) go to 190
            sargb=sqrt(argb)
            coefo=1.0e0_knd/sargb
            arg=c*sargb
            stemp0=-cos(arg)/arg
            sneun(1)=stemp0
            sneun(1)=sneun(1)*10.0e0_knd**(-norme(1))
            sneun(2)=stemp0/arg-sin(arg)/arg
            sneun(2)=sneun(2)*10.0e0_knd**(-norme(2))
            darg=1.0e0_knd/arg
              do 150 n=3,m+3,2
              rn=n+n-3
              arn=n+n-1
              sneun(n)=-sneun(n-2)+darg*rn*sneun(n-1)
              sneun(n+1)=-sneun(n-1)+darg*arn*sneun(n)
              sneun(n+1)=sneun(n+1)*10.0e0_knd**(-norme(n+1)+norme(n-1))
              sneun(n)=sneun(n)*10.0e0_knd**(-norme(n)+norme(n-1))
150           continue
            sneu1=sneun(m+1)
            sneu2=sneun(m+2)*10.0e0_knd**(norme(m+2)-norme(m+1))
            sneu3=sneun(m+3)*10.0e0_knd**(norme(m+3)-norme(m+1))
             p(1)=twomi*(etaim1**amo2)
            p(2)=alpha(1)*etai*p(1)
            coef1=coef*sneu1*wg(i)
            coef2=coef*coefo*sneu2*wg(i)
            coef4=coef*coefo*coefo*etai*sneu3*wg(i)
            pint1(1)=pint1(1)+coef1*p(1)
            pint2(1)=pint2(1)+coef2*p(1)
            pint4(2)=pint4(2)+coef4*p(2)
c
              do 160 il=2,lim+1,2
              p(il+1)=alpha(il)*etai*p(il)+beta(il)*p(il-1)
              p(il+2)=alpha(il+1)*etai*p(il+1)+beta(il+1)*p(il)
              pint1(il+1)=pint1(il+1)+coef1*p(il+1)
              pint2(il+1)=pint2(il+1)+coef2*p(il+1)
              pint4(il+2)=pint4(il+2)+coef4*p(il+2)
160              continue
170         continue
180          continue
190        continue
          do 200 il=1,lim,2
          pint3(il+1)=(pint2(il+2)-beta(il+1)*pint2(il))
     1                /alpha(il+1)
200          continue
c
c  calculation of ratios of integrals for ease in compution of r2 and
c  r2d in subroutine r2int
        rpint1(1)=0.0e0_knd
        rpint1(2)=0.0e0_knd
        rpint2(1)=0.0e0_knd
        rpint2(2)=0.0e0_knd
          do 210 il=3,lim,2
          rpint1(il)=pint1(il)*(twom+il-1)/(pint1(il-2)*(il-1))
          rpint2(il)=pint2(il)*(twom+il-1)/(pint2(il-2)*(il-1))
          rpint1(il+1)=pint3(il+1)*(twom+il)/(pint3(il-1)*(il))
          rpint2(il+1)=pint4(il+1)*(twom+il)/(pint4(il-1)*(il))
210          continue
        return
        end
c
c
        subroutine sphbes (c,x,limj,maxj,maxlp,sbesf,sbesdf,sbesn,
     1                     ibese,sbesdr)
c
c  purpose:     To calculate ratios of successive first kind spherical
c               Bessel functions of the same parity for given c and x.
c               to calculate corresponding ratios of their first
c               derivatives. To calculate the characteristics and
c               exponents of both the Bessel functions of the first
c               kind and their first derivatives.
c
c  parameters:
c
c     input:    c      : c
c               x      : x
c               limj   : the number of spherical Bessel function
c                        ratios calculated for given lnum,c,ndec,
c                        and maximum m desired
c               maxj   : dimension of sbesf vector
c               maxlp  : the number of scale factors
c                        that are calculated
c
c     output:   sbesf  : ratios of successive first kind spherical
c                        Bessel functions of the same parity
c               sbesdf : ratios of first derivatives of successive
c                        first kind spherical Bessel functions of the
c                        same parity
c               sbesn  : characteristics for the spherical
c                        Bessel functions
c               ibese  : exponents for the spherical
c                        Bessel functions
c               sbesdr : ratios of first derivatives of spherical Bessel
c                        functions to the corresponding spherical
c                        spherical functions
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) c,cx,rn,stemp0,stemp1,x
        real(knd) sbesdf(maxj),sbesdr(maxlp),sbesf(maxj),sbesn(maxlp)
c
c  integer array
        dimension ibese(maxlp)
c
        cx=c*x
        lim1=cx+cx+20
c
c  compute first kind Bessel function ratios
c        sbesf(k)= j(n=k,c*x)/j(n=k-1,c*x)
c        sbesn(k)= (j(n=k-1),c*x))*10.0e0_knd**(-ibese(k))
c
        if (cx.lt.limj) go to 20
c
c  for c*x >= lim, use forward recursion to
c  get fcn. ratios:
c       j(n+1,c*x)/j(n,c*x)=(2*n+1)/(c*x)-1/(j(n,c*x)/j(n-1,c*x))
c
        stemp0=sin(cx)/cx
        sbesf(1)=(stemp0/cx-cos(cx)/cx)/stemp0
          do 10 n=1,limj-1
          rn=n+n+1
          sbesf(n+1)=(rn/cx)-(1.0e0_knd/sbesf(n))
10        continue
        go to 60
20      continue
c
c  for c*x < lim, use backward recursion to
c  get fcn. ratios:
c       j(n,c*x)/j(n-1,c*x) = 1/( (2*n+1)/(c*x) - j(n+1,c*x)/j(n,c*x) )
c
        stemp0=0.0e0_knd
        if(lim1.le.limj) go to 40
          do 30 n=lim1,limj,-1
          rn=n+n+1
          stemp1=1.0e0_knd/(rn/cx-stemp0)
          stemp0=stemp1
30        continue
40      sbesf(limj)=stemp0
          do 50 n=limj-1,1,-1
          rn=n+n+1
          sbesf(n)=1.0e0_knd/(rn/cx-sbesf(n+1))
50        continue
60      continue
c
c  for all c*x, calculate the amplitude and sign scale
c  factors by forward operation on the Bessel function
c  ratios.
        stemp0=sin(cx)/cx
        stemp1=stemp0/cx-cos(cx)/cx
        ibese(1)=int(log10(abs(stemp0)))
        sbesn(1)=stemp0*10.0e0_knd**(-ibese(1))
        if(abs(sin(cx)).lt.0.5e0_knd.and.cx.gt.1.0e0_knd) go to 70
        sbesn(2)=sbesn(1)*sbesf(1)
        ibese(2)=int(log10(abs(sbesn(2))))
        sbesn(2)=sbesn(2)*10.0e0_knd**(-ibese(2))
        ibese(2)=ibese(2)+ibese(1)
        go to 80
70      ibese(2)=int(log10(abs(stemp1)))
        sbesn(2)=stemp1*10.0e0_knd**(-ibese(2))
        sbesf(1)=stemp1/stemp0
80      continue
          do 90 n=3,maxlp
          sbesn(n)=sbesn(n-1)*sbesf(n-1)
          ibese(n)=log10(abs(sbesn(n)))
          sbesn(n)=sbesn(n)*10.0e0_knd**(-ibese(n))
          ibese(n)=ibese(n)+ibese(n-1)
90        continue
c
c  calculate the ratios of the first derivatives of successive
c  Bessel functions using corresponding function ratios
          do 100 n=1,limj
          rn=n-1
          sbesdf(n)=(cx-(rn+2.0e0_knd)*sbesf(n))/(rn-cx*sbesf(n))
100       continue
c
c  calculate the ratios of the first derivative to the corresponding
c  spherical Bessel function
          do 110 n=1,maxlp
          rn=n-1
          sbesdr(n)=(rn/cx)-sbesf(n)
110       continue
c
c  calculate the ratios of successive functions and derivatives
c  of the same parity
          do 120 n=limj,2,-1
          sbesf(n)=sbesf(n-1)*sbesf(n)
          sbesdf(n)=sbesdf(n-1)*sbesdf(n)
120       continue
        return
        end
c
c
        subroutine sphneu (c,x,limn,maxn,maxlp,sneuf,sneun,ineue,sneudf,
     1                     sneudr)
c
c  purpose:     To calculate ratios of spherical Neumann functions
c               and ratios of their first derivatives for given c and x.
c               to calculate the Neumann function characteristics
c               and exponents. To calculate ratios of the first
c               derivatives to corresponding functions.
c
c  parameters:
c
c     input:    c      : c
c               x      : x
c               limn   : the number of spherical Neumann function
c                        ratios calculated for given lnum,c,ndec,
c                        and maximum m desired
c               maxn   : dimension of sneuf and sneudf arrays
c               maxlp  : the number of values of scale factors
c                        that are calculated
c
c     output:   sneuf  : ratios of successive spherical Neumann
c                        functions of the same parity
c               sneun  : characteristic for the spherical
c                        Neumann functions
c               ineue  : exponent for the spherical
c                        Neumann functions
c               sneudf : ratios of first derivatives of successive
c                        spherical Neumann functions of the same parity
c               sneudr : ratios of first derivatives of spherical
c                        Neumann functions to the corresponding
c                        function
c
        use param
c
c  real(knd) scalars and arrays
        real(knd) c,cx,rn,rnn,stemp0,stemp1,x
        real(knd) sneudf(maxn),sneudr(maxlp),sneuf(maxn),sneun(maxlp)
c
c  integer arrays
        dimension ineue(maxlp)
c
c  compute first kind ratios of Neumann functions and ratios
c  of their first derivatives
c
c        sneuf(k)=y(n=k,c*x)/y(n=k-2,c*x)
c        sneun(k)=(y(n=k-1),c*x)*10.0e0_knd**(-ineue(k))
c        sneudf(k)=y'(n=k,c*x)/y'(n=k-2,c*x)
c        sneudr(k)=(y'(n=k-1),c*x)/y(n=k-1),c*x))
c
c  use forward recursion to compute function ratios
c
c       y(n+1,c*x)/y(n,c*x)=(2*n+1)/(c*x)-1/(y(n,c*x)/y(n-1,c*x))
c
c  compute derivative ratios at same time using function ratios.
        cx=c*x
        stemp0=-cos(cx)/cx
        stemp1=stemp0/cx-sin(cx)/cx
        sneuf(1)=stemp1/stemp0
        sneudf(1)=-(cx-2.0e0_knd*sneuf(1))/(cx*sneuf(1))
          do 10 n=1,limn-1
            rn=n
            rnn=n+n+1
            sneuf(n+1)=rnn/cx-1.0e0_knd/sneuf(n)
            sneudf(n+1)=(cx-(rn+2.0e0_knd)*sneuf(n+1))/(rn-
     1                  cx*sneuf(n+1))
10          continue
          sneuf(limn+1)=0.0e0_knd
          sneuf(limn+2)=0.0e0_knd
          sneudf(limn+1)=0.0e0_knd
          sneudf(limn+2)=0.0e0_knd
c
c  calculate the characteristic and exponent
c  by forward operation on the Neumann function ratios:
        ineue(1)=int(log10(abs(stemp0)))
        sneun(1)=stemp0*10.0e0_knd**(-ineue(1))
        ineue(2)=int(log10(abs(stemp1)))
        sneun(2)=stemp1*10.0e0_knd**(-ineue(2))
        nlimit=maxlp
        if(maxlp.gt.limn+1) nlimit=limn+1
          do 20 n=3,nlimit
          sneun(n)=sneun(n-1)*sneuf(n-1)
          ineue(n)=int(log10(abs(sneun(n))))
          sneun(n)=sneun(n)*10.0e0_knd**(-ineue(n))
          ineue(n)=ineue(n)+ineue(n-1)
20        continue
c
c  calculate the ratios of the first derivatives to the corresponding
c  spherical Neumann functions
          do 30 n=1,nlimit
          rn=n-1
          sneudr(n)=(rn/cx)-sneuf(n)
30        continue
c
c  calculate the ratios of successive functions and derivatives
c  of the same parity
          do 40 n=limn,2,-1
          sneuf(n)=sneuf(n-1)*sneuf(n)
          sneudf(n)=sneudf(n-1)*sneudf(n)
40        continue
        return
        end
