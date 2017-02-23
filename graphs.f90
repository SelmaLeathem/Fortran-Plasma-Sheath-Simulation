MODULE graphs
   USE constants
   USE parameters
   IMPLICIT NONE

   INTEGER,PARAMETER:: DIST_NO=10001
   INTEGER, DIMENSION (:), ALLOCATABLE ::counter
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::rho_plot
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::phi_plot
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::E_field_plot
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::de_plot
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::di_plot
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::ve_plot
   REAL(KIND = DOUBLE), DIMENSION (:), ALLOCATABLE ::vi_plot
   REAL(KIND=DOUBLE),DIMENSION (:), ALLOCATABLE::v_grid_temp

   CONTAINS

      SUBROUTINE allocate_rho_plot()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(rho_plot(return_no_cells()+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_rho_plot

      SUBROUTINE allocate_phi_plot()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(phi_plot(return_no_cells()+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_phi_plot

      SUBROUTINE allocate_E_field_plot()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(E_field_plot(return_no_cells()+1),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_E_field_plot


      SUBROUTINE allocate_de_plot()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(de_plot(DIST_NO),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_de_plot


      SUBROUTINE allocate_di_plot()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(di_plot(DIST_NO),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_di_plot


      SUBROUTINE allocate_ve_plot()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(ve_plot(return_no_cells()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_ve_plot

      SUBROUTINE allocate_vi_plot()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(vi_plot(return_no_cells()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_vi_plot

      SUBROUTINE allocate_v_grid_temp()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(v_grid_temp(return_no_cells()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_v_grid_temp

      SUBROUTINE allocate_counter()
      IMPLICIT NONE
      INTEGER::AllocateStatus
      ALLOCATE(counter(return_no_cells()),STAT=AllocateStatus)
      IF (AllocateStatus /= 0) STOP "Not enough memory for array"
      END SUBROUTINE allocate_counter

      SUBROUTINE deallocate_rho_plot()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(rho_plot,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
      END SUBROUTINE deallocate_rho_plot

     SUBROUTINE deallocate_phi_plot()
     IMPLICIT NONE
     INTEGER::DeallocateStatus
     DEALLOCATE(phi_plot,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
     END SUBROUTINE deallocate_phi_plot

     SUBROUTINE deallocate_E_field_plot()
     IMPLICIT NONE
     INTEGER::DeallocateStatus
     DEALLOCATE(E_field_plot,STAT=DeallocateStatus)
     IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
     END SUBROUTINE deallocate_E_field_plot

      SUBROUTINE deallocate_de_plot()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(de_plot,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
      END SUBROUTINE deallocate_de_plot

      SUBROUTINE deallocate_di_plot()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(di_plot,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
      END SUBROUTINE deallocate_di_plot

      SUBROUTINE deallocate_ve_plot()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(ve_plot,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
      END SUBROUTINE deallocate_ve_plot


      SUBROUTINE deallocate_vi_plot()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(vi_plot,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
      END SUBROUTINE deallocate_vi_plot

      SUBROUTINE deallocate_v_grid_temp()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(v_grid_temp,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
      END SUBROUTINE deallocate_v_grid_temp

      SUBROUTINE deallocate_counter()
      IMPLICIT NONE
      INTEGER::DeallocateStatus
      DEALLOCATE(counter,STAT=DeallocateStatus)
      IF (DeallocateStatus /= 0) STOP "deallocation unsuccessful"
      END SUBROUTINE deallocate_counter

      SUBROUTINE initialize_plots()
         IMPLICIT NONE
         rho_plot = 0.0D0
         phi_plot = 0.0D0
         E_field_plot = 0.0D0
         de_plot =0.0D0
         di_plot = 0.0D0
         ve_plot = 0.0D0
         vi_plot = 0.0D0
      END SUBROUTINE initialize_plots 

      SUBROUTINE sum_fields(rho,phi,E_field)
         IMPLICIT NONE
         REAL(KIND=DOUBLE),DIMENSION (:),INTENT(IN)::rho,phi,E_field
         rho_plot = rho_plot + rho
         phi_plot = phi_plot + phi
         E_field_plot = E_field_plot + E_field
      END SUBROUTINE sum_fields

      SUBROUTINE sum_av_velocities(x,v,v_plot,j)
         IMPLICIT NONE
         INTEGER::k
         INTEGER(KIND = LONG_INT)::i,no_cells,no_particles
         REAL(KIND=DOUBLE)::dx
         REAL(KIND=DOUBLE),DIMENSION (:),INTENT(IN)::x,v
         INTEGER(KIND=LONG_INT),DIMENSION (:),INTENT(IN)::j
         REAL(KIND=DOUBLE),DIMENSION (:),INTENT(INOUT)::v_plot
         no_cells=return_no_cells() 
         no_particles=return_no_cells()
         dx=return_dx()
         counter = 0
         v_grid_temp = 0.0D0       
         DO i=1,no_particles,1
            IF (j(i)>=1 .AND. j(i) <=no_cells) THEN
              v_grid_temp(j(i)) = v_grid_temp(j(i)) + v(i)
              counter(j(i)) = counter(j(i)) + 1 
            END IF
         END DO

         DO k=1,no_cells,1
            IF(counter(k) .NE. 0) THEN
               v_plot(k)=v_plot(k) + v_grid_temp(k)/DBLE(counter(k))
            END IF
         END DO
      END SUBROUTINE sum_av_velocities

      SUBROUTINE sum_av_dist(dist_location,spread,x,v,d_plot,j)
        IMPLICIT NONE
        INTEGER::vbin,divide_dist_by
        INTEGER,INTENT(IN)::dist_location,spread
        INTEGER(KIND = LONG_INT)::no_particles,i 
        INTEGER(KIND=LONG_INT),DIMENSION (:),INTENT(IN)::j
        REAL,DIMENSION (:)::dist(DIST_NO)
        REAL(KIND=DOUBLE)::dx
        REAL(KIND=DOUBLE),DIMENSION (:),INTENT(IN)::x,v
        REAL(KIND=DOUBLE),DIMENSION (:),INTENT(INOUT)::d_plot

        no_particles = return_no_particles()
        divide_dist_by = (DIST_NO - 1)/10
        dist = 0.0D0
      
        DO i=1,no_particles,1
           IF (j(i)>=(dist_location-spread).AND.j(i)<=(dist_location+spread)) THEN
              vbin = NINT(v(i)*DBLE(divide_dist_by))
              IF (ABS(vbin) <= (DIST_NO-1)/2) THEN
                 dist(vbin + 1 + (DIST_NO-1)/2) = &
                 dist(vbin + 1 + (DIST_NO-1)/2) + 1.0
              END IF
           END IF
        END DO
            d_plot = d_plot + dist
      END SUBROUTINE sum_av_dist

     SUBROUTINE average_fields(no_steps)
        IMPLICIT NONE
        REAL(KIND=DOUBLE),INTENT(IN)::no_steps
        rho_plot = rho_plot/no_steps
        phi_plot = phi_plot/no_steps
        E_field_plot = E_field_plot/no_steps
     END SUBROUTINE average_fields

     SUBROUTINE average_velocities(no_steps,v_plot)
        IMPLICIT NONE
        REAL(KIND=DOUBLE),INTENT(IN)::no_steps
        REAL(KIND=DOUBLE),DIMENSION (:),INTENT(INOUT)::v_plot
        v_plot = v_plot/no_steps
     END SUBROUTINE average_velocities

     SUBROUTINE average_dist(no_steps,d_plot)
        IMPLICIT NONE
        REAL(KIND=DOUBLE),INTENT(IN)::no_steps
        REAL(KIND=DOUBLE),DIMENSION (:),INTENT(INOUT)::d_plot
        d_plot = d_plot/no_steps
     END SUBROUTINE average_dist

     SUBROUTINE write_fields_file()
        IMPLICIT NONE
        INTEGER::j,no_cells     
        no_cells=return_no_cells()
        open(1,file='av_fields.txt',status='replace')  
        DO j=1,no_cells,1
         WRITE(1,*) DBLE(j),rho_plot(j),phi_plot(j),E_field_plot(j)
        END DO
        close(1)
     END SUBROUTINE write_fields_file

     SUBROUTINE write_v_plot_file(which_name,v_plot)
        IMPLICIT NONE
        CHARACTER(20)::filename
        INTEGER::j,no_cells     
        INTEGER,INTENT(IN)::which_name
        REAL(KIND=DOUBLE),DIMENSION (:),INTENT(IN)::v_plot
        IF (which_name .EQ. 1) THEN
           filename = "av_ve_plot.txt"
        ELSE IF (which_name .EQ. 0) THEN
           filename = "av_vi_plot.txt"
        END IF
        no_cells=return_no_cells()
        open(1,file=filename,status='replace')  
        DO j=1,no_cells,1
         WRITE(1,*) DBLE(j),v_plot(j)
        END DO
        close(1)
     END SUBROUTINE write_v_plot_file

     SUBROUTINE write_d_plot_file(which_name,d_plot)
        IMPLICIT NONE
        CHARACTER(20)::filename
        INTEGER::j,no_bins     
        INTEGER,INTENT(IN)::which_name
        REAL(KIND=DOUBLE)::midway
        REAL(KIND=DOUBLE),DIMENSION (:),INTENT(IN)::d_plot
        IF (which_name .EQ. 1) THEN
           filename = "av_de_plot.txt"
        ELSE IF (which_name .EQ. 0) THEN
           filename = "av_di_plot.txt"
        END IF
        midway = DBLE((DIST_NO-1)/2)
        open(1,file=filename,status='replace')  
        DO j=1,DIST_NO,1
         WRITE(1,*) DBLE(j)-midway,d_plot(j)
        END DO
        close(1)
     END SUBROUTINE write_d_plot_file

END MODULE graphs

