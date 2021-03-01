      program HeronsFormulaStrict
      real area, real s, real a, real b, real c
      logical Cond_1
      logical Cond_2

c ---------------------------------------------------
c Compute the are of a triangle using Heron's formula
c ----------------------------------------------------
      
      read (*,*)  a, b, c

      write (*,*) "a = ", a
      write (*,*) "b = ", b
      write (*,*) "c = ", c
      write (*,*)

      Cond_1 = (a > 0.0) .AND. (b > 0.0) .AND. (c > 0.0)
      Cond_2 = ((a + b) > c) .AND. ((a + c) > b) .AND. ((b + c) > a)
      if(Cond_1 .AND. Cond_2) then
      s = (a + b + c) / 2.0
      area = sqrt(s * (s - a) * (s - b) * (s - c))
      
      write (*,*) "Triangle are = ", area
      else
      write (*,*) "ERROR: this is not a triangle"
      end if 

      end



