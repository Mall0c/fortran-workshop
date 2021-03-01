program ModulesAndArrays

    use LinearAlgebra

    real, dimension(3, 4) :: points
    real, dimension(4, 4) :: homogCoords
    real, dimension(4, 4) :: matrix
    integer :: i

    points(:, 1) = (/ 0, 0, 0 /)
    points(:, 2) = (/ 1, 0, 0 /)
    points(:, 3) = (/ 0, 1, 0 /)
    points(:, 4) = (/ 0, 0, 1 /)

    write(*, *) "Original points:"
    write(*, *) transpose(points)

    matrix = makeIdentityMatrix(4)
    matrix(1:2, 1) = (/ cos(pi), sin(pi) /)
    matrix(1:2, 2) = (/ -sin(pi), cos(pi) /)
    matrix(1:3, 4) = (/ 2, 2, 1 /)

    do i = 1, 4
        homogCoords(:, i) = vecToHomogCoords(points(:, i))
        homogCoords(:, i) = matmul(matrix, homogCoords(:, i))
        points(:, i) = homogCoordsToVec(homogCoords(:, i))
    end do

    write(*, *) "Transformed points:"
    write(*, *) transpose(points)

contains

end program