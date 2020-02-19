module matrixMulMod
contains

  ! ====================================================== !
  ! === Matrix - Matrix Multiply                       === !
  ! ====================================================== !
  subroutine MatrixMultiply( Amat, Bmat, Cmat, LI, LJ, LK )
    implicit none
    integer         , intent(in)  :: LI, LJ, LK
    double precision, intent(in)  :: Amat(LI,LJ), Bmat(LJ,LK)
    double precision, intent(out) :: Cmat(LI,LK)
    integer                       :: i, j, k
    double precision              :: mul

    do i=1, LI
       do k=1, LK
          mul = 0.d0
          do j=1, LJ
             mul = mul + Amat(i,j)*Bmat(j,k)
          enddo
          Cmat(i,k) = mul
       enddo
    enddo

    return
  end subroutine MatrixMultiply
  
end module matrixMulMod
