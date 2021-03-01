program matMulProduct
  implicit none
  integer, parameter :: n = 3
  integer, dimension(3,3) :: a, b, c
  integer :: i, j, k, trace, partial_trace

  print *, 'Matrix Multiplication: A Matrix'

  a = reshape([1, 2, 3, 1, 2, 3, 1, 2, 3], shape(a))
  write( * , "(*(g0))" ) ( (a(i,j), " ", j=1, n), new_line("A"), i=1, n )

  print *, 'Matrix Multiplication: B Matrix'

  b = reshape([1, 2, 3, 1, 2, 3, 1, 2, 3], shape(b))
  write( * , "(*(g0))" ) ( (b(i,j), " ", j=1, n), new_line("A"), i=1, n )
   
  !$OMP PARALLEL SHARED(a, b, c, trace) PRIVATE(partial_trace)
  partial_trace = 0

  !$OMP DO
  DO i=1,n
    DO j=1,n
      c(i,j) = 0
      DO k=1,n
        c(i,j) = c(i,j) + a(i,k) * b(k,j)
      END DO
      IF (i .EQ. j) THEN
        partial_trace = partial_trace + c(i,j)
      END IF
    END DO
  END DO
  !$OMP END DO

  !$OMP CRITICAL
      print *, 'Add ', partial_trace, ' to ', trace
      trace = trace + partial_trace
  !$OMP END CRITICAL

  !$OMP END PARALLEL

  print *, new_line("A"), 'Matrix Multiplication: Result Matrix'
  write( * , "(*(g0))" ) ( (c(i,j), " ", j=1, n), new_line("A"), i=1, n )

  print *, 'Result Matrix Trace'
  print *, trace
   
end program matMulProduct