      PROGRAM fortranSim
      USE constants
      USE parameters
      USE particle
      USE fields
      USE graphs

      IMPLICIT NONE
      INTEGER,PARAMETER::IS_ELECTRON=1,IS_ION=0
      INTEGER::vbin,divide_dist_by,no_graph_steps,start_graphs
      INTEGER::end_graphs,graph_duration,dist_location,spread,no_cells
      INTEGER(KIND = LONG_INT):: i,no_particles,time_step,run_time
      INTEGER,DIMENSION (:)::dist(DIST_NO)
      REAL (KIND = DOUBLE) :: dxpe,dxpi,vte,vti,qce,qci,phi0,phiL 
      REAL (KIND = DOUBLE) ::no_graphing_steps


      CALL SRAND(SEED) 
      no_graph_steps = 100
      no_graphing_steps = DBLE(no_graph_steps)
      run_time = 1002
      start_graphs = 900
      end_graphs = start_graphs + no_graph_steps

      no_cells=return_no_cells()
      dist_location = NINT(0.75*DBLE(no_cells)) 
      spread = NINT(0.1*DBLE(no_cells)) 

      !make particle and field arrays, this step will eventually
      !be in a separate module as will have more than one species
      !of ion as an option

      Call allocate_qe()
      Call allocate_qi()
      Call allocate_in_out_e()
      Call allocate_in_out_i()
      Call allocate_xe()
      Call allocate_xi()
      Call allocate_je()
      Call allocate_ji()
      Call allocate_we()
      Call allocate_wi()
      Call allocate_ve()
      Call allocate_vi()
      Call allocate_Epe()
      Call allocate_Epi()
      Call allocate_rho()
      Call allocate_phi()
      Call allocate_E_field()
      Call allocate_abcgam()

      !***initialize variables***

      dxpe=return_dxpe()
      dxpi=return_dxpi()
      xe=dxpe
      Call initiate_positions(xe)
      xi=dxpi
      Call initiate_positions(xi)
      vte=return_vte()
      ve=1.0D0/vte
      vi=1.0D0/vte
      CALL initiate_velocities(vte,ve)
      vti=return_vti()
      CALL initiate_velocities(vti,vi)
      in_out_e = 1
      in_out_i = 1
      Epe = 0.0D0
      Epi = 0.0D0
      qe = 0.0D0
      qi = 0.0D0
      rho = 0.0D0
      phi = 0.0D0
      E_field = 0.0D0
      je=0.0D0
      ji=0.0D0
      we=0.0D0
      wi=0.0D0

      no_particles = return_no_particles()
      divide_dist_by = (DIST_NO - 1)/10
      dist = 0.0D0 
      DO i=1,no_particles,1
        vbin = NINT(ve(i)*DBLE(divide_dist_by))
        IF (ABS(vbin) <= (DIST_NO-1)/2) THEN
            dist(vbin + 1 + (DIST_NO-1)/2) = &
            dist(vbin + 1 + (DIST_NO-1)/2) + 1.0
        END IF
      END DO  
      open(1,file='loading_dist.txt',status= 'replace')
      DO i=1,DIST_NO,1
         write(1,*) i,dist(i) 
      END DO
      close(1)

      qce = return_qce()
      qci = return_qci()

      !will eventually vary sinusoidally
      phi0 = 0.0D0
      phiL = 0.0D0


      DO time_step = 1,run_time,1
      PRINT *,"time = ",time_step

      CALL move_particles(IS_ION,vti,in_out_i,xi,vi,Epi)
      CALL move_particles(IS_ELECTRON,vte,in_out_e,xe,ve,Epe)
      
      CALL collect_charge(qce,qe,xe,in_out_e,je,we)
      CALL collect_charge(qci,qi,xi,in_out_i,ji,wi)

      CALL grid_rho(phi0,phiL,qi,qe) 

      CALL grid_phi(phi0,phiL)

      CALL grid_E_field()

      CALL particle_E_field(qce,E_field,in_out_e,xe,Epe,je,we)
      CALL particle_E_field(qci,E_field,in_out_i,xi,Epi,ji,wi)
      
      IF (time_step .EQ. start_graphs) THEN
          CALL allocate_rho_plot()
          CALL allocate_phi_plot()
          CALL allocate_E_field_plot()
          CALL allocate_de_plot()
          CALL allocate_di_plot()
          CALL allocate_ve_plot()
          CALL allocate_vi_plot()
          CALL allocate_v_grid_temp()
          CALL allocate_counter()

          CALL initialize_plots()
      END IF
      
      IF (time_step >= start_graphs .AND. time_step < end_graphs) THEN
          CALL sum_fields(rho,phi,E_field)

          CALL sum_av_velocities(xe,ve,ve_plot,je)
          CALL sum_av_velocities(xi,vi,vi_plot,ji)

          CALL sum_av_dist(dist_location,spread,xe,ve,de_plot,je)
          CALL sum_av_dist(dist_location,spread,xi,vi,di_plot,ji)
      END IF

      IF (time_step .EQ. end_graphs) THEN
          CALL average_fields(no_graphing_steps)

          CALL average_velocities(no_graphing_steps,ve_plot)
          CALL average_velocities(no_graphing_steps,vi_plot)

          
          CALL average_dist(no_graphing_steps,de_plot)
          CALL average_dist(no_graphing_steps,di_plot)

          CALL write_fields_file()
          CALL write_v_plot_file(IS_ELECTRON,ve_plot)
          CALL write_v_plot_file(IS_ION,vi_plot)
          
          CALL write_d_plot_file(IS_ELECTRON,de_plot)
          CALL write_d_plot_file(IS_ION,di_plot)
          
          CALL deallocate_rho_plot()
          CALL deallocate_phi_plot()
          CALL deallocate_E_field_plot()
          CALL deallocate_de_plot()
          CALL deallocate_di_plot()
          CALL deallocate_ve_plot()
          CALL deallocate_vi_plot()
          CALL deallocate_v_grid_temp()
          CALL deallocate_counter()

      END IF

      END DO 

      
        
      open(1,file='e_vel.txt',status= 'replace')
      DO i=1,no_particles,1
         IF(MOD(i,10000) .EQ. 0) THEN
            write(1,*) xe(i),ve(i) 
         END IF
      END DO
      close(1)
        
      open(1,file='i_vel.txt',status= 'replace')
      DO i=1,no_particles,1
         IF(MOD(i,10000) .EQ. 0) THEN
            write(1,*) xi(i),vi(i) 
         END IF
      END DO
      close(1)
        
 
      !deallocate arrays

      Call deallocate_qe()
      Call deallocate_qi()
      Call deallocate_in_out_e()
      Call deallocate_in_out_i()
      Call deallocate_xe()
      Call deallocate_xi()
      Call deallocate_je()
      Call deallocate_ji()
      Call deallocate_we()
      Call deallocate_wi()
      Call deallocate_ve()
      Call deallocate_vi()
      Call deallocate_Epe()
      Call deallocate_Epi()
      Call deallocate_rho()
      Call deallocate_phi()
      Call deallocate_E_field()
      Call deallocate_abcgam()
      
 

      END PROGRAM fortranSim
     
