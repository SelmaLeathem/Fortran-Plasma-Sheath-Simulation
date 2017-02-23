MODULE particle
USE constants
USE parameters
IMPLICIT NONE
 
   INTEGER::reached_left_wall,reached_right_wall 
   INTEGER, PARAMETER:: type_electron=1, type_ion=0 
   INTEGER,DIMENSION (:), ALLOCATABLE:: in_out_e 
   INTEGER,DIMENSION (:), ALLOCATABLE:: in_out_i 
   INTEGER(KIND=LONG_INT),DIMENSION (:), ALLOCATABLE:: je
   INTEGER(KIND=LONG_INT),DIMENSION (:), ALLOCATABLE:: ji

   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::we
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::wi
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::qe
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::qi
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::xe
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::xi
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::ve
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::vi
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::Epe
   REAL(KIND = DOUBLE),DIMENSION (:), ALLOCATABLE ::Epi



   CONTAINS

   SUBROUTINE allocate_qe()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(qe(return_no_cells()+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_qe
  
   SUBROUTINE allocate_qi()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(qi(return_no_cells()+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_qi
  
   SUBROUTINE allocate_je()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(je(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_je
  
   SUBROUTINE allocate_ji()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(ji(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_ji
  
   SUBROUTINE allocate_we()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(we(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_we
  
   SUBROUTINE allocate_wi()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(wi(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_wi
  
   SUBROUTINE allocate_in_out_e()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(in_out_e(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_in_out_e
  
   SUBROUTINE allocate_in_out_i()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(in_out_i(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_in_out_i
  
   SUBROUTINE allocate_xe()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(xe(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_xe
  
   SUBROUTINE allocate_xi()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(xi(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_xi
  
   SUBROUTINE allocate_ve()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(ve(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_ve
  
   SUBROUTINE allocate_vi()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(vi(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_vi
  
   SUBROUTINE allocate_Epe()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(Epe(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_Epe
  
   SUBROUTINE allocate_Epi()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(Epi(return_no_particles()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
   END SUBROUTINE allocate_Epi
  
   SUBROUTINE deallocate_qe()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(qe,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_qe
  
   SUBROUTINE deallocate_qi()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(qi,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_qi
  
   SUBROUTINE deallocate_in_out_e()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(in_out_e,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_in_out_e
  
   SUBROUTINE deallocate_in_out_i()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(in_out_i,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_in_out_i
  
   SUBROUTINE deallocate_je()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(je,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_je
  
   SUBROUTINE deallocate_ji()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(ji,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_ji
  
   SUBROUTINE deallocate_we()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(we,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_we
  
   SUBROUTINE deallocate_wi()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(wi,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_wi
  
   SUBROUTINE deallocate_xe()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(xe,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_xe
  
   SUBROUTINE deallocate_xi()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(xi,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_xi
  
   SUBROUTINE deallocate_ve()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(ve,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_ve
  
   SUBROUTINE deallocate_vi()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(vi,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_vi
  
   SUBROUTINE deallocate_Epe()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(Epe,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_Epe
  
   SUBROUTINE deallocate_Epi()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(Epi,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful" 
   END SUBROUTINE deallocate_Epi
  
   SUBROUTINE initiate_positions(x)
     IMPLICIT NONE
     INTEGER(KIND = LONG_INT)::i,no_particles
     REAL(KIND=DOUBLE)::origin
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(INOUT)::X
     origin = 0.5D0*return_dx()
     no_particles = return_no_particles()
 
     DO i=1,no_particles,1
        x(i)=x(i)*DBLE(i)+origin
     END DO
   END SUBROUTINE initiate_positions

   FUNCTION return_v(vt)
      IMPLICIT NONE
      REAL(KIND=DOUBLE)::v,return_v, r1, r2
      REAL(KIND=DOUBLE),INTENT(IN)::vt
      v= 11000.0D0*vt

      DO WHILE(DABS(v)>10000.0D0*vt)
          r1 = 2.0D0*(1.0D0-DBLE(RAND()))
          r2 = 1.0D0-DBLE(RAND())
          v =  vt*(2.0D0*DCOS(PI*r1)*DSQRT(-DLOG(r2)))
      END DO
      return_v=v
   END FUNCTION return_v 
  
   SUBROUTINE initiate_velocities(vt,v)
     IMPLICIT NONE
     INTEGER(KIND = LONG_INT)::i,no_particles
     REAL(KIND=DOUBLE),INTENT(IN)::vt
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(INOUT)::v
     no_particles = return_no_particles()
     DO i=1,no_particles,1
        v(i)=v(i)*return_v(vt)
     END DO
   END SUBROUTINE initiate_velocities

   SUBROUTINE set_left_counter(negative_charge,reached_left_wall)
     IMPLICIT NONE
     INTEGER, INTENT(INOUT)::reached_left_wall
     INTEGER, INTENT(IN)::negative_charge
     IF (negative_charge == 1) THEN  
         reached_left_wall = reached_left_wall - 1
     ELSE
         reached_left_wall = reached_left_wall + 1
     END IF 
   END SUBROUTINE set_left_counter
  
   SUBROUTINE set_right_counter(negative_charge,reached_right_wall)
     IMPLICIT NONE
     INTEGER, INTENT(INOUT)::reached_right_wall
     INTEGER, INTENT(IN)::negative_charge
     IF (negative_charge == 1) THEN  
         reached_right_wall = reached_right_wall - 1
     ELSE
         reached_right_wall = reached_right_wall + 1
     END IF 
   END SUBROUTINE set_right_counter

   SUBROUTINE collect_charge(qc,q,x,in_out,j,w)
     IMPLICIT NONE
     INTEGER::q_index,k,cell_no
     INTEGER(KIND = LONG_INT)::i,no_particles
     INTEGER,DIMENSION(:),INTENT(IN)::in_out
     INTEGER(KIND=LONG_INT),DIMENSION(:),INTENT(INOUT)::j
     REAL(KIND=DOUBLE)::dx,qo
     REAL(KIND=DOUBLE),INTENT(IN)::qc
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(INOUT)::q,w
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(IN)::x
     cell_no = return_no_cells()
     no_particles= return_no_particles()
     dx = return_dx()

     q = 0.0D0

     DO i=1,no_particles,1
        j(i) = NINT((x(i)/dx),KIND = LONG_INT)
        w(i) = x(i)/dx - DBLE(j(i))
        q_index = in_out(i)*j(i)
        IF (q_index >= 1 .AND. q_index <= cell_no) THEN
          q(q_index) = q(q_index) + DBLE(in_out(i))*(1.0D0-w(i))
          q(q_index + 1) = q(q_index+1) + DBLE(in_out(i))*w(i)
        END IF
     END DO
     
     qo=return_qo()
     q = (qc/qo)*q 

   END SUBROUTINE collect_charge  

   SUBROUTINE particle_E_field(qc,Egrid,in_out,x,Ep,j,w)
     IMPLICIT NONE
     INTEGER::q_index
     INTEGER(KIND=LONG_INT)::i,no_particles
     INTEGER,DIMENSION(:),INTENT(IN)::in_out
     INTEGER(KIND=LONG_INT),DIMENSION(:),INTENT(IN)::j
     REAL(KIND=DOUBLE)::dx,qc,q_sign
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(IN)::Egrid
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(IN)::x,w
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(INOUT)::Ep
     no_particles=return_no_particles()
     Ep = 0.0D0
     q_sign = qc/DABS(qc)
         
     DO i=1,no_particles,1
        q_index = in_out(i)*j(i)
        Ep(i)= (Egrid(q_index)*(1.0D0-w(i))+Egrid(q_index+1)*w(i)) 
     END DO
         Ep=q_sign*Ep*DBLE(in_out)
   END SUBROUTINE particle_E_field

   SUBROUTINE move_particles(negative_charge,vt,in_out,x,v,Ep)
     IMPLICIT NONE
     INTEGER::midway,no_cells
     INTEGER(KIND=LONG_INT)::i,no_particles
     INTEGER,INTENT(IN)::negative_charge
     INTEGER,DIMENSION(:),INTENT(INOUT)::in_out
     REAL(KIND=DOUBLE)::dtvte,dtwpe,dx,vte,length,s_start,s_end
     REAL(KIND=DOUBLE),INTENT(IN)::vt
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(INOUT)::x
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(INOUT)::v
     REAL(KIND=DOUBLE),DIMENSION(:),INTENT(IN)::Ep

     vte=return_vte()
     dtvte=return_dt()*vte
     dtwpe=return_dt()*return_plasma_freq()
     dx = return_dx()
     midway=0.5D0*DBLE(return_no_cells())*dx
     no_cells=return_no_cells()
     no_particles=return_no_particles()
     length=return_length()
     s_start=0.5D0*dx
     s_end=length+0.5*dx 

     v = v + dtwpe*Ep*DBLE(in_out)
     x = x + dtvte*v*DBLE(in_out)

     DO i=1,no_particles,1
       IF( x(i)< s_start .OR. x(i)> s_end ) THEN
          IF (negative_charge .EQ. 0) THEN
              IF (x(i)< s_start) THEN
                  reached_left_wall=reached_left_wall+1
              ELSE IF (x(i)> s_end) THEN
                  reached_right_wall=reached_right_wall+1
              END IF
              x(i) = midway
              v(i) = return_v(vt)/vte
         ELSE IF (negative_charge .EQ. 1) THEN
              IF (x(i)< s_start) THEN
                 IF (reached_left_wall > 0) THEN
                   x(i) = midway
                   v(i) = return_v(vt)/vte
                   reached_left_wall=reached_left_wall-1
                   in_out(i)=1
                 ELSE IF (reached_left_wall == 0) THEN
                   in_out(i)=0
                 END IF
              ELSE IF (x(i)> s_end) THEN
                 IF (reached_right_wall > 0) THEN
                   x(i) = midway
                   v(i) = return_v(vt)/vte
                   reached_right_wall=reached_right_wall-1
                   in_out(i)=1
                 ELSE IF (reached_right_wall == 0) THEN
                   in_out(i)=0
                 END IF
              END IF
         END IF
       END IF
     END DO
   END SUBROUTINE move_particles           


END MODULE particle
