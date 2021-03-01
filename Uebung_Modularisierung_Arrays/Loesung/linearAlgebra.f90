module LinearAlgebra

    implicit none
    real, parameter :: pi = 3.14159

contains

    function makeIdentityMatrix(size) result(matrix)

        implicit none
        integer, intent(in) :: size
        real, dimension(size, size) :: matrix
        integer :: i

        matrix = 0

        do i = 1, size
            matrix(i, i) = 1
        end do

    end function makeIdentityMatrix

    function vecToHomogCoords(vec) result(coords)

        implicit none
        real, dimension(:), intent(in) :: vec
        real, dimension(size(vec) + 1) :: coords
        
        coords = 1
        coords(1:size(vec)) = vec

    end function vecToHomogCoords

    function homogCoordsToVec(coords) result(vec)

        implicit none
        real, dimension(:), intent(in) :: coords
        real, dimension(size(coords) - 1) :: vec
        integer :: i

        do i = 1, size(vec)
            vec(i) = coords(i) / coords(size(coords))
        end do

    end function homogCoordsToVec

end module