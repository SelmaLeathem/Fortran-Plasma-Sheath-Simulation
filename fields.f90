MODULE fields
   USE constants
   USE parameters
   IMPLICIT NONE

   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::rho
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::phi
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::E_field
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE::a,b,c,gam

   CONTAINS

   SUBROUTINE allocate_rho()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(rho(return_no_cells()+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_rho

   SUBROUTINE allocate_phi()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(phi(return_no_cells()+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_phi

   SUBROUTINE allocate_E_field()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(E_field(return_no_cells()+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_E_field
   
   SUBROUTINE allocate_abcgam()
      IMPLICIT NONE
      INTEGER::AllocateStatus,no_cells
      no_cells = return_no_cells()

      ALLOCATE(a(no_cells+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"

      ALLOCATE(b(no_cells+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"

      ALLOCATE(c(no_cells+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"

      ALLOCATE(gam(no_cells+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_abcgam

   SUBROUTINE deallocate_abcgam()
      IMPLICIT NONE
      INTEGER::DeallocateStatus,no_cells
      no_cells = return_no_cells()

     DEALLOCATE(a,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"

     DEALLOCATE(b,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"

     DEALLOCATE(c,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"

     DEALLOCATE(gam,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"

   END SUBROUTINE deallocate_abcgam

   SUBROUTINE deallocate_rho()
     IMPLICIT NONE
     INTEGER::DeallocateStatus
     DEALLOCATE(rho,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
   END SUBROUTINE deallocate_rho

   SUBROUTINE deallocate_phi()
     IMPLICIT NONE
     INTEGER::DeallocateStatus
     DEALLOCATE(phi,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
   END SUBROUTINE deallocate_phi

   SUBROUTINE deallocate_E_field()
     IMPLICIT NONE
     INTEGER::DeallocateStatus
     DEALLOCATE(E_field,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
   END SUBROUTINE deallocate_E_field

   SUBROUTINE grid_rho(phi0,phiL,qi,qe)
      IMPLICIT NONE
      INTEGER::no_cells
      REAL(KIND=DOUBLE)::debye_length,dx
      REAL(KIND=DOUBLE),INTENT(IN)::phi0,phiL
      REAL(KIND=DOUBLE),DIMENSION (:),INTENT(IN)::qe,qi 
      dx=return_dx()
      debye_length=return_debye_L()
      no_cells=return_no_cells()
      rho = 0.0D0
      rho = -(dx/debye_length)*(dx/debye_length)*(qi + qe)
      rho(1) = 0.0D0
      rho(no_cells) = 0.0D0
      rho(2) = rho(2) - phi0
      rho(no_cells-1) = rho(no_cells-1) - phiL

   END SUBROUTINE grid_rho

   SUBROUTINE grid_phi(phi0,phiL)   
      IMPLICIT NONE
      INTEGER::j,no_cells
      REAL(KIND = DOUBLE)::bet 
      REAL(KIND = DOUBLE),INTENT(IN)::phi0,phiL
      no_cells=return_no_cells()
      
      a=1.0D0
      b=-2.0D0
      c=1.0D0
      gam=0.0D0
      phi=0.0D0

      a(1) = 0.0
      b(1) = 0.0
      c(1) = 0.0

      a(2) = 0.0
      b(2) = -2.0
      c(2) = 1.0

      a(no_cells-1) = 1.0
      b(no_cells-1) = -2.0
      c(no_cells-1) = 0.0

      bet = b(2)
      phi(2)=rho(2)/bet

      DO j=3,no_cells-1,1
         gam(j) = c(j-1)/bet
         bet = b(j) - a(j)*gam(j)
         phi(j) = (rho(j) - a(j)*phi(j-1))/bet
      END DO
 
      DO j=no_cells-1,2,-1
         phi(j) = phi(j) - gam(j+1)*phi(j+1)
      END DO 

      phi(1) = phi0
      phi(no_cells)=phiL

   END SUBROUTINE grid_phi  

   SUBROUTINE grid_E_field()
      IMPLICIT NONE
      INTEGER::j,no_cells 
      REAL(KIND=DOUBLE)::debye_length,dx
      debye_length = return_debye_L()
      dx = return_dx()
      no_cells = return_no_cells()
      E_field = 0.0D0

      DO j=2,no_cells-1,1
         E_field(j)=(phi(j-1) - phi(j+1))/(2.0D0*dx/debye_length)
      END DO 

      E_field(1) = 2.0D0*E_field(2)-E_field(3)
      E_field(no_cells)=2.0D0*E_field(no_cells-1) - E_field(no_cells-2)
   END SUBROUTINE grid_E_field

END MODULE fields
