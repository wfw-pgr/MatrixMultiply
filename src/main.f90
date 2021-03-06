! ====================================================== !
! === Matrix Multiply Example                        === !
! ====================================================== !
program main
  use matrixMulMod
  implicit none
  character(100)  , parameter   :: AmatFile = 'dat/Amat.dat'
  character(100)  , parameter   :: BmatFile = 'dat/Bmat.dat'
  character(100)  , parameter   :: CmatFile = 'dat/Cmat.dat'
  double precision, allocatable :: Amat(:,:), Bmat(:,:), Cmat(:,:)
  integer                       :: i, j, k, LI, LJ, LK, LM
  double precision              :: avg, std
  integer         , parameter   :: lun = 50

  ! ------------------------------------- !
  ! --- [1] Data Load                 --- !
  ! ------------------------------------- !
  !  -- [1-1] Load Amatrix            --  !
  open(lun,file=trim(AmatFile),status='old',form='formatted')
  read(lun,*) LI, LJ
  allocate( Amat(LI,LJ) )
  do i=1, LI
     read(lun,*) Amat(i,:)
  enddo
  close(lun)
  write(6,*) Amat
  !  -- [1-2] Load Bmatrix            --  !
  open(lun,file=trim(BmatFile),status='old',form='formatted')
  read(lun,*) LK, LM
  allocate( Bmat(LK,LM) )
  if ( LJ.ne.LK ) then
     write(6,*) '[main] incompatible size ??', LJ, LK
     stop
  endif
  do i=1, LK
     read(lun,*) Bmat(i,:)
  enddo
  close(lun)
  write(6,*) Bmat


  ! ------------------------------------- !
  ! --- [2] Gauss Elimination         --- !
  ! ------------------------------------- !
  allocate( Cmat(LI,LM) )
  Cmat = 0.d0
  call MatrixMultiply( Amat, Bmat, Cmat, LI, LJ, LM )
  
  ! ------------------------------------- !
  ! --- [3] Answer Check              --- !
  ! ------------------------------------- !
  open(lun,file=trim(CmatFile),status='replace',form='formatted')
  write(lun,*) LI, LM
  do i=1, LI
     write(lun,*) Cmat(i,:)
  enddo
  close(lun)
  
  return
end program main

